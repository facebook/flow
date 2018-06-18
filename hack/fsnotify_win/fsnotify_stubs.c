/**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 */

/* See `fsnotify_win/fsnotify.ml` for a general description. */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/callback.h>
#include <caml/unixsupport.h>

#include <stdio.h>
#include <stdlib.h>
#include <Windows.h>
#include <assert.h>

// Terrible guesstimate of ~1k files, highly unlikely,
// but it's ~16kb buffer.
#define FILE_NOTIFY_BUFFER_LENGTH ((sizeof(DWORD) * 4) * 1000)
#define FILE_NOTIFY_CONDITIONS (\
FILE_NOTIFY_CHANGE_FILE_NAME | \
FILE_NOTIFY_CHANGE_DIR_NAME | \
FILE_NOTIFY_CHANGE_SIZE | \
FILE_NOTIFY_CHANGE_LAST_WRITE | \
FILE_NOTIFY_CHANGE_CREATION)

// list of event
struct events {
  struct events *next;
  char *wpath;                   // the root directory
  FILE_NOTIFY_INFORMATION *info; // the list of changed files
};

// OCaml: Fsnotify.fsenv
struct fsenv {
  struct events *events; // the lock-less list of events
  HANDLE pipe;           // the pipe to wakeup the OCaml program
};

// Parameter to the watching thread
struct wenv {
  char *wpath;         // the directory to be watched
  struct fsenv* fsenv; // where to store the events.
};

// The pipes are only used signal the other thread that something is ready. So
// writing '.' to the pipe is sufficient.
static void signal_on_pipe(struct fsenv *fsenv)
{
  char dot = '.';
  DWORD bytesWritten;
  WriteFile(fsenv->pipe, &dot, sizeof(dot), &bytesWritten, NULL);
  // If `WriteFail` fails, that's probably because the pipe is full,
  // or closed. Then we don't care.
}

// Push a new event in the shared list and wakeup the OCaml thread
static void push_info(struct wenv *wenv, FILE_NOTIFY_INFORMATION *info)
{
  struct events *ev = (struct events*)malloc(sizeof(struct events));
  struct fsenv *fsenv = wenv->fsenv;
  ev->info = info;
  ev->wpath = wenv->wpath;
  do {
    ev->next = fsenv->events;
  } while (InterlockedCompareExchangePointer((void**)&(fsenv->events),
                                             ev, ev->next) != ev->next);
  signal_on_pipe(fsenv);
}

// Pop all events from the shared list
static struct events *pop_events(struct fsenv *fsenv)
{
  struct events *res;
  do {
    res = fsenv->events;
  } while (InterlockedCompareExchangePointer((void**)&(fsenv->events),
                                             NULL, res) != res);
  return res;
}

// Converts a `struct events *` in an OCaml value of type `Fsnotify.event list`
static value parse_events(struct events *events)
{
  CAMLparam0();
  CAMLlocal4(res, cell, ev, wpath);
  res = Val_unit;
  for (;;) {
    if (events == NULL) break;
    FILE_NOTIFY_INFORMATION *fileInfo = events->info;
    wpath = caml_copy_string(events->wpath);
    for (;;) {
      // Forcefully null terminate the filename.
      char* modifiedFilename =
        (char*)malloc(fileInfo->FileNameLength);
      wcstombs_s(NULL, modifiedFilename,
                 fileInfo->FileNameLength,
                 fileInfo->FileName,
                 fileInfo->FileNameLength / sizeof (wchar_t));
      // Allocate 'Fsnotify.events'
      ev = caml_alloc_tuple(2);
      Store_field(ev, 0, caml_copy_string(modifiedFilename));
      Store_field(ev, 1, wpath);
      // Allocate list cell
      cell = caml_alloc_tuple(2);
      Store_field(cell, 0, ev);
      Store_field(cell, 1, res);
      res = cell;
      // Next info
      if (fileInfo->NextEntryOffset == 0)
        break;
      FILE_NOTIFY_INFORMATION *old = fileInfo;
      fileInfo =
        (FILE_NOTIFY_INFORMATION*)
        ((char*)fileInfo + fileInfo->NextEntryOffset);
      free(old);
    }
    // Next event
    struct events *old = events;
    events = events->next;
    free(old);
  }
  CAMLreturn(res);
}

// Main function for the watching thread
static DWORD watcher_thread_main(LPVOID param) {

  char *buf;
  struct wenv *wenv = (struct wenv *)param;
  HANDLE dir_handle = CreateFile(wenv->wpath,
    FILE_LIST_DIRECTORY, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
    NULL,
    OPEN_EXISTING,
    FILE_FLAG_BACKUP_SEMANTICS,
    NULL);

  if (dir_handle == INVALID_HANDLE_VALUE) {
    win32_maperr(GetLastError());
    uerror("CreateFile", Nothing);
  }

  while(TRUE) {
    DWORD size;
    buf = malloc(FILE_NOTIFY_BUFFER_LENGTH);
    if(!ReadDirectoryChangesW(dir_handle,
                              buf, FILE_NOTIFY_BUFFER_LENGTH,
                              TRUE,
                              FILE_NOTIFY_CONDITIONS,
                              &size, NULL, NULL)) {
      fprintf(stderr, "FATAL ERROR\n");
      fflush(stderr);
      break;
    };
    push_info(wenv, (FILE_NOTIFY_INFORMATION*) buf);
  }

  free(buf);
  CloseHandle(dir_handle);
  return 0;
}

// Stub: create a watching thread for a given directory
value caml_fsnotify_add_watch(value vfsenv, value path)
{
  CAMLparam2(vfsenv, path);
  char output[_MAX_PATH];
  struct fsenv *fsenv = (struct fsenv*)vfsenv;
  struct wenv *wenv = malloc(sizeof(struct wenv));
  wenv->wpath = _strdup(_fullpath(output, String_val(path), _MAX_PATH));
  wenv->fsenv = fsenv;
  HANDLE th = CreateThread(NULL, 0, watcher_thread_main, wenv, 0, NULL);
  if (th == INVALID_HANDLE_VALUE) {
    win32_maperr(GetLastError());
    uerror("CreateThread", Nothing);
  }
  CAMLreturn(Val_long(th));
}

// Stub: read all events from the shared list
value caml_fsnotify_read_events(value vfsenv)
{
  CAMLparam1(vfsenv);
  struct fsenv *fsenv = (struct fsenv*)vfsenv;
  struct events *events = pop_events(fsenv);
  CAMLreturn(parse_events(events));
}

// Stub: initialize the environment
value caml_fsnotify_init(value pipe)
{
  CAMLparam1(pipe);
  struct fsenv *fsenv = malloc(sizeof(fsenv));
  fsenv->pipe = Handle_val(pipe);
  fsenv->events = NULL;
  CAMLreturn((value)fsenv);
}
