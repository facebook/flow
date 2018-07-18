/**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/callback.h>
#include <caml/unixsupport.h>

#include <stdio.h>
#include <pthread.h>

#if (OCAML_VERSION_MAJOR == 4) && (OCAML_VERSION_MINOR < 3)
// Basically the ocaml headers and the mac headers are redefining the same
// types. AFAIK, this is a longstanding bug in the ocaml headers
// (http://caml.inria.fr/mantis/print_bug_page.php?bug_id=4877). Looking at the
// OS X headers, the conflicting typehints are gated with these definitions, so
// this was the easiest workaround for me.
//
// This bug was apparently fixed in 4.03, causing the opposite problem, hence
// the above gating.
#define _UINT64
#define _UINT32
#endif

#include <CoreFoundation/CoreFoundation.h>
#include <CoreServices/CoreServices.h>
#include <libkern/OSAtomic.h>

/**
 * Commands and events are passed back and forth between the main thread and
 * the run loop thread using two lockless linked lists. Each linked list is
 * only prepended to by one thread and only cleared by the other thread. Here's
 * a quick explanation of how they work:
 *
 * So lets say the linked list of events looks like
 *
 * events = C -> B -> A
 *
 * and we have a new event D to put on this list. We point D at C and try to
 * atomically set events to point at D if and only if events still points at C.
 * If this works we have
 *
 * events = D -> C -> B -> A
 *
 * If this exchange fails, that means events was changed by the other thread.
 * The other thread can clear the list, so we know events can only be NULL
 * until we change it. So the state is now
 *
 * events = NULL.
 *
 * In theory we could just set events to D. I still use the compareAndSwap to
 * assert this invariant though. Now the state is
 *
 * events = D
 *
 * From the reading side, it looks like this: Let's say the state is
 *
 * events = C -> B -> A
 *
 * So we'll set to_do = C and try to swap NULL into events atomically if and
 * only if events still points to C. If it works, the state will be
 *
 * events = NULL
 *
 * If it fails, then the writing thread must have added something to the list.
 *
 * So the state might look like
 *
 * events = D -> C -> B -> A
 *
 * So we just try again until it works.
 */
typedef enum { ADD_WATCH, RM_WATCH } command_type;
struct command {
  struct command *next;
  char *arg;
  command_type type;
};

struct event {
  struct event *next;
  char *wpath;
  char *path;
};

/**
 * This is a structure that the thread holds on to. It has all the context it
 * needs
 */
struct thread_env {
  // This fd will be ready to be read when there is a command ready to be run
  int read_command_fd;
  // The thread writes to this fd when there are events ready to be returned
  int write_event_fd;
  // A pointer to the linked list of commands to run. The thread will only ever
  // remove all the commands...it will never append a command
  struct command **commands;
  // A pointer to the linked list of events to process. The thread will append
  // events to this linked list
  struct event **events;
};

/**
 * This is a structure that holds all the context the main thread needs. A
 * pointer to this structure will be returned to ocaml and passed in whenever
 * something needs to be done
 */
struct env {
  // The main thread writes to this fd when there are events ready to be run
  int write_command_fd;
  // This fd will be ready to be read when there are events ready to be
  // processed
  int read_event_fd;
  // A pointer to the linked list of commands to run. The main thread will
  // append commands to this list
  struct command **commands;
  // A pointer to the linked list of events to process. The main thread will
  // only ever remove all the events...it will never append an event
  struct event **events;
};

/**
 * The pipes are only used signal the other thread that something is ready. So
 * writing '.' to the pipe is sufficient. In theory, you could get away with
 * only having pipes and not using the linked lists at all. pthreads can
 * automically write to and read from pipes up to N bytes (which I think is
 * usually 512). In the case where we have large paths, though, it gets a
 * little tricker, and I like writing lockless data structures :P
 */
static void signal_on_pipe(int fd) {
  char dot = '.';
  write(fd, &dot, 1);
}

/**
 * Cleans out all the data on a pipe. The other thread has signaled us on this
 * pipe and we've received the signal, now we need to read all the data so we
 * can select on this fd again.
 */
static void clear_pipe(int fd) {
  fd_set rfds;
  struct timeval tv;
  int retval;
  char c;
  FD_ZERO(&rfds);
  FD_SET(fd, &rfds);
  tv.tv_sec = 0;
  tv.tv_usec = 0;
  while (true) {
    // I really hate select(). The first argument is
    // 1 + max(fd1, fd2, ..., fdn) because reasons
    retval = select(fd + 1, &rfds, NULL, NULL, &tv);
    if (retval > 0) {
      read(fd, &c, 1);
    } else {
      break;
    }
  }
}


/**
 * This callback is called whenver an FSEvents watch is triggered
 */
static void watch_callback(
    ConstFSEventStreamRef streamRef,
    void *info,
    size_t numEvents,
    void *eventPaths,
    const FSEventStreamEventFlags eventFlags[],
    const FSEventStreamEventId eventIds[]) {
  struct thread_env *thread_env = (struct thread_env *)info;
  int i;
  char **paths = eventPaths;
  CFStringRef cf_wpath;
  CFArrayRef paths_watched;
  CFIndex wpath_len;
  int path_len;
  struct event *ev;

  for (i = 0; i < numEvents; i++) {
    paths_watched = FSEventStreamCopyPathsBeingWatched(streamRef);
    if (CFArrayGetCount(paths_watched) < 1) {
      continue;
    }
    // Create an event with the info from FSEvents
    ev = malloc(sizeof(struct event));
    cf_wpath = CFArrayGetValueAtIndex(paths_watched, 0);
    wpath_len = CFStringGetLength(cf_wpath);
    ev->wpath = malloc(wpath_len + 1);
    CFStringGetCString(
        cf_wpath,
        ev->wpath,
        wpath_len + 1,
        kCFStringEncodingMacRoman
    );
    path_len = strlen(paths[i]);
    ev->path = malloc(path_len + 1);
    strcpy(ev->path, paths[i]);

    // Update the lockless linked list of events
    ev->next = *(thread_env->events);
    if (OSAtomicCompareAndSwapPtr(ev->next, ev, (void *)(thread_env->events)) == false) {
      // The only thing the main thread can write to events is NULL, so we can just
      // try one more time
      ev->next = NULL;
      if (OSAtomicCompareAndSwapPtr(ev->next, ev, (void *)(thread_env->events)) == false) {
        // This should never happen
        fprintf(stderr, "Unexpected error with fsevents lockless events list\n");
      }
    }
    signal_on_pipe(thread_env->write_event_fd);
  }
}

/**
 * Create a new watch for a given path
 */
static void add_watch(char *wpath, struct thread_env *env) {
  // Copied latency from Watchman
  // https://github.com/facebook/watchman/blob/5161cb6eeca4405bcaea820f377cd6d2f51ce1f4/root.c#L1838
  CFAbsoluteTime latency = 0.0001;
  FSEventStreamRef stream;
  CFStringRef path = CFStringCreateWithCString(NULL, wpath, kCFStringEncodingMacRoman);
  CFArrayRef pathsToWatch = CFArrayCreate(NULL, (const void**)&path, 1, NULL);

  // Docs say FSEventStreamCreate copies the fields out of this context, so it
  // doesn't need to be long lived
  FSEventStreamContext context = {};
  memset(&context, 0, sizeof(FSEventStreamContext));
  context.info = env;
  stream = FSEventStreamCreate(
    NULL,
    &watch_callback,
    &context,
    pathsToWatch,
    kFSEventStreamEventIdSinceNow,
    latency,
    kFSEventStreamCreateFlagFileEvents
  );
  FSEventStreamScheduleWithRunLoop(
    stream,
    CFRunLoopGetCurrent(),
    kCFRunLoopDefaultMode
  );
  FSEventStreamStart(stream);
}

/**
 * The callback for when the main thread has issued a command to the run loop
 * thread. Reads all the pending commands and executes them
 */
static void command_callback(
    CFFileDescriptorRef fdref,
    CFOptionFlags callBackTypes,
    void *context) {
  int fd = CFFileDescriptorGetNativeDescriptor(fdref);
  struct thread_env *thread_env = (struct thread_env *)context;
  struct command *to_do = NULL;
  struct command *to_free = NULL;
  clear_pipe(fd);
  do {
    to_do = *(thread_env->commands);
  } while (OSAtomicCompareAndSwapPtr(to_do, NULL, (void *)(thread_env->commands)) == false);
  while (to_do != NULL) {
    switch (to_do->type) {
      case ADD_WATCH:
        add_watch(to_do->arg, thread_env);
        break;
      case RM_WATCH:
        // hh_server doesn't need this at the moment. Shouldn't be too hard to
        // do...just need to map of paths to FSEvent streams.
        fprintf(stderr, "fsevents impl doesn't support removing watches yet\n");
        break;
    }
    to_free = to_do;
    to_do = to_do->next;
    free(to_free->arg);
    free(to_free);
  }
  // For some reason you have to re-enable this callback every bloody time
  CFFileDescriptorEnableCallBacks(fdref, kCFFileDescriptorReadCallBack);
}

/**
 * The main thread tells the run loop thread that a new command is ready by
 * writing to a pipe. This function tells the run loop to call a callback
 * whenever that fd is ready to be read
 */
static void listen_for_command_fd(struct thread_env *thread_env) {
  int fd = thread_env->read_command_fd;
  CFFileDescriptorContext fd_context = {};
  fd_context.info = thread_env;
  CFFileDescriptorRef fdref = CFFileDescriptorCreate(
    kCFAllocatorDefault,
    fd,
    true,
    command_callback,
    &fd_context
  );
  CFFileDescriptorEnableCallBacks(fdref, kCFFileDescriptorReadCallBack);
  CFRunLoopSourceRef source = CFFileDescriptorCreateRunLoopSource(
    kCFAllocatorDefault,
    fdref,
    0
  );
  CFRunLoopAddSource(CFRunLoopGetCurrent(), source, kCFRunLoopDefaultMode);
  CFRelease(source);
}

/**
 * The run loop thread is responsible for running the loop and then executing
 * the callbacks when they are triggered
 */
static void run_loop_thread(struct thread_env *thread_env) {
  listen_for_command_fd(thread_env);
  CFRunLoopRun();
}

/**
 * Starts up a thread running a run loop, creates the env, and then returns it
 */
static struct env *fsevents_init(void) {
  pthread_t loop_thread;
  struct thread_env *thread_env;
  struct env *env;

  int command_pipe[2];
  int event_pipe[2];

  struct command **commands = malloc(sizeof(struct command*));
  struct event **events = malloc(sizeof(struct event*));
  *commands = NULL;
  *events = NULL;

  pipe(command_pipe);
  pipe(event_pipe);

  thread_env = malloc(sizeof(struct thread_env));
  thread_env->read_command_fd = command_pipe[0];
  thread_env->write_event_fd = event_pipe[1];
  thread_env->commands = commands;
  thread_env->events = events;

  pthread_create(&loop_thread, NULL, (void *)&run_loop_thread, thread_env);

  env = malloc(sizeof(struct env));
  env->write_command_fd = command_pipe[1];
  env->read_event_fd = event_pipe[0];
  env->commands = commands;
  env->events = events;

  return env;
}

/**
 * Sends a command to the run loop thread by adding the command to the linked
 * list of commands and then signaling the run loop thread through a pipe
 */
static void send_command(struct env *env, command_type type, char const *arg) {
  struct command *c = malloc(sizeof(struct command));

  c->type = type;
  c->arg = malloc(strlen(arg) + 1);
  strcpy(c->arg, arg);
  c->next = *(env->commands);
  if (OSAtomicCompareAndSwapPtr(c->next, c, (void *)(env->commands)) == false) {
    // The only thing the thread can write to commands is NULL, so we can just
    // try one more time
    c->next = NULL;
    if (OSAtomicCompareAndSwapPtr(c->next, c, (void *)(env->commands)) == false) {
      // This should never happen
      fprintf(stderr, "Unexpected error with fsevents lockless commands list\n");
    }
  }
  signal_on_pipe(env->write_command_fd);
}

/**
 * Grabs the events that are ready to be processed from the linked list and
 * clears the list
 */
static struct event *read_events(struct env *env) {
  struct event *to_do = NULL;
  clear_pipe(env->read_event_fd);
  do {
    to_do = *(env->events);
  } while (OSAtomicCompareAndSwapPtr(to_do, NULL, (void *)(env->events)) == false);
  return to_do;
}

CAMLprim value stub_fsevents_init(value unit)
{
  CAMLparam1(unit);
  // We're returning a pointer to ocaml land. This type will be opaque to them
  // and only be useful when passed back to c. This is a safe way to pass
  // pointers back to ocaml. See
  // http://caml.inria.fr/pub/docs/manual-ocaml/intfc.html#sec412
  CAMLreturn((value)fsevents_init());
}

CAMLprim value stub_fsevents_add_watch(value env, value path)
{
  CAMLparam2(env, path);
  CAMLlocal1(ret);
  struct env *c_env = (struct env*)env;
  char *rpath = realpath(String_val(path), NULL);
  if (rpath == NULL) {
    uerror("realpath", path);
  }
  send_command(c_env, ADD_WATCH, rpath);
  ret = caml_copy_string(rpath);
  free(rpath);
  CAMLreturn(ret);
}

/**
 * This functionality is not yet implemented
 */
CAMLprim value stub_fsevents_rm_watch(value env, value path)
{
  CAMLparam2(env, path);
  CAMLlocal1(ret);
  struct env *c_env = (struct env*)env;
  char *rpath = realpath(String_val(path), NULL);
  if (rpath == NULL) {
    uerror("realpath", path);
  }
  send_command(c_env, RM_WATCH, String_val(path));
  ret = caml_copy_string(rpath);
  free(rpath);
  CAMLreturn(ret);
}

CAMLprim value stub_fsevents_get_event_fd(value env)
{
  CAMLparam1(env);
  int fd = ((struct env *)env)->read_event_fd;
  CAMLreturn(Val_int(fd));
}

CAMLprim value stub_fsevents_read_events(value env)
{
  CAMLparam1(env);
  CAMLlocal3(event_list, event, cons);
  struct event *to_free;
  struct event *events = read_events((struct env*)env);
  event_list = Val_emptylist;
  while (events != NULL) {
    // A tuple to store the filed
    event = caml_alloc(2, 0);
    Store_field(event, 0, caml_copy_string(events->path));
    Store_field(event, 1, caml_copy_string(events->wpath));
    to_free = events;
    events = events->next;

    // This is how you do event::event_list in c
    cons = caml_alloc(2, 0);
    Store_field(cons, 0, event);
    Store_field(cons, 1, event_list);
    event_list = cons;

    // Free the processed event
    free(to_free->path);
    free(to_free->wpath);
    free(to_free);
  }

  CAMLreturn(event_list);
}
