/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* See `fsnotify_win/fsnotify.ml` for a general description. */

// expose win_wide_char_to_multi_byte
#define CAML_INTERNALS

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/osdeps.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

#include <Windows.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

// Terrible guesstimate of ~1k files, highly unlikely,
// but it's ~16kb buffer.
#define FILE_NOTIFY_BUFFER_LENGTH ((sizeof(DWORD) * 4) * 1000)
#define FILE_NOTIFY_CONDITIONS                                  \
  (FILE_NOTIFY_CHANGE_FILE_NAME | FILE_NOTIFY_CHANGE_DIR_NAME | \
   FILE_NOTIFY_CHANGE_SIZE | FILE_NOTIFY_CHANGE_LAST_WRITE |    \
   FILE_NOTIFY_CHANGE_CREATION)

// list of event
struct events {
  struct events* next;
  char* wpath; // the root directory
  FILE_NOTIFY_INFORMATION* info; // the list of changed files
};

// OCaml: Fsnotify.fsenv
struct fsenv {
  struct events* events; // the lock-less list of events
  HANDLE pipe; // the pipe to wakeup the OCaml program
};

// Parameter to the watching thread
struct wenv {
  char* wpath; // path of the directory to be watched
  HANDLE dir_handle; // handle of the directory to be watched
  struct fsenv* fsenv; // where to store the events.
};

// The pipes are only used signal the other thread that something is ready. So
// writing '.' to the pipe is sufficient.
static void signal_on_pipe(struct fsenv* fsenv) {
  char dot = '.';
  DWORD bytesWritten;
  WriteFile(fsenv->pipe, &dot, sizeof(dot), &bytesWritten, NULL);
  // If `WriteFail` fails, that's probably because the pipe is full,
  // or closed. Then we don't care.
}

// Push a new event in the shared list and wakeup the OCaml thread
static void push_info(struct wenv* wenv, FILE_NOTIFY_INFORMATION* info) {
  struct events* ev = (struct events*)malloc(sizeof(struct events));
  struct fsenv* fsenv = wenv->fsenv;
  ev->info = info;
  ev->wpath = wenv->wpath;
  do {
    ev->next = fsenv->events;
  } while (InterlockedCompareExchangePointer(
               (void**)&(fsenv->events), ev, ev->next) != ev->next);
  signal_on_pipe(fsenv);
}

// Pop all events from the shared list
static struct events* pop_events(struct fsenv* fsenv) {
  struct events* res;
  do {
    res = fsenv->events;
  } while (InterlockedCompareExchangePointer(
               (void**)&(fsenv->events), NULL, res) != res);
  return res;
}

// Converts a `struct events *` in an OCaml value of type `Fsnotify.event list`
static value parse_events(struct events* events) {
  CAMLparam0();
  CAMLlocal5(res, cell, ev, wpath_value, path_value);
  res = Val_unit;
  for (;;) {
    if (events == NULL)
      break;
    FILE_NOTIFY_INFORMATION* fileInfo = events->info;
    wpath_value = caml_copy_string(events->wpath);
    size_t wpath_len = strlen(events->wpath);
    for (;;) {
      int i;

      // Relative path in UTF16. Not null-terminated!
      const wchar_t* filename = fileInfo->FileName;
      // Length of relative path in UTF16. Does not include final NULL.
      size_t wlen = fileInfo->FileNameLength;
      // Length of the relative path in UTF8.
      int slen = win_wide_char_to_multi_byte(filename, wlen, NULL, 0);

      // Allocate the absolute path, including a slash
      path_value = caml_alloc_string(wpath_len + 1 + slen);
      char* path = (char*)String_val(path_value);
      strncpy(path, events->wpath, wpath_len);
      path[wpath_len] = '\\';
      win_wide_char_to_multi_byte(filename, wlen, path + wpath_len + 1, slen);

      // normalize slashes
      //
      // this SHOULD be a no-op because the wpath should already be normalized,
      // and ReadDirectoryChangesW should give us paths using native slashes,
      // but it's cheap to double check.
      //
      // ideally, we'd normalize to forward slashes here and consistently
      // use forward slashes everywhere.
      for (i = 0; i < slen; i++) {
        if (path[i] == '/') {
          path[i] = '\\';
        }
      }

      // Allocate 'Fsnotify.events'
      ev = caml_alloc_tuple(2);
      Store_field(ev, 0, path_value);
      Store_field(ev, 1, wpath_value);
      // Allocate list cell
      cell = caml_alloc_tuple(2);
      Store_field(cell, 0, ev);
      Store_field(cell, 1, res);
      res = cell;
      // Next info
      if (fileInfo->NextEntryOffset == 0)
        break;
      FILE_NOTIFY_INFORMATION* old = fileInfo;
      fileInfo =
          (FILE_NOTIFY_INFORMATION*)((char*)fileInfo + fileInfo->NextEntryOffset);
      free(old);
    }
    // Next event
    struct events* old = events;
    events = events->next;
    free(old);
  }
  CAMLreturn(res);
}

// Main function for the watching thread
static DWORD watcher_thread_main(LPVOID param) {
  char* buf;
  struct wenv* wenv = (struct wenv*)param;
  while (TRUE) {
    DWORD size;
    buf = malloc(FILE_NOTIFY_BUFFER_LENGTH);
    if (!ReadDirectoryChangesW(
            wenv->dir_handle,
            buf,
            FILE_NOTIFY_BUFFER_LENGTH,
            TRUE,
            FILE_NOTIFY_CONDITIONS,
            &size,
            NULL,
            NULL)) {
      fprintf(stderr, "FATAL ERROR\n");
      fflush(stderr);
      break;
    };
    push_info(wenv, (FILE_NOTIFY_INFORMATION*)buf);
  }

  free(buf);
  CloseHandle(wenv->dir_handle);
  free(wenv);
  return 0;
}

// Stub: create a watching thread for a given directory
value caml_fsnotify_add_watch(value vfsenv, value path) {
  CAMLparam2(vfsenv, path);
  char output[_MAX_PATH];
  struct fsenv* fsenv = (struct fsenv*)vfsenv;
  struct wenv* wenv = malloc(sizeof(struct wenv));
  wenv->wpath = _strdup(_fullpath(output, String_val(path), _MAX_PATH));
  wenv->fsenv = fsenv;

  HANDLE dir_handle = CreateFile(
      wenv->wpath,
      FILE_LIST_DIRECTORY,
      FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
      NULL,
      OPEN_EXISTING,
      FILE_FLAG_BACKUP_SEMANTICS,
      NULL);

  if (dir_handle == INVALID_HANDLE_VALUE) {
    win32_maperr(GetLastError());
    uerror("CreateFile", Nothing);
  }
  wenv->dir_handle = dir_handle;

  HANDLE th = CreateThread(NULL, 0, watcher_thread_main, wenv, 0, NULL);
  if (th == INVALID_HANDLE_VALUE) {
    win32_maperr(GetLastError());
    uerror("CreateThread", Nothing);
  }
  CAMLreturn(Val_long(th));
}

// Stub: read all events from the shared list
value caml_fsnotify_read_events(value vfsenv) {
  CAMLparam1(vfsenv);
  struct fsenv* fsenv = (struct fsenv*)vfsenv;
  struct events* events = pop_events(fsenv);
  CAMLreturn(parse_events(events));
}

// Stub: initialize the environment
value caml_fsnotify_init(value pipe) {
  CAMLparam1(pipe);
  struct fsenv* fsenv = malloc(sizeof(fsenv));
  fsenv->pipe = Handle_val(pipe);
  fsenv->events = NULL;
  CAMLreturn((value)fsenv);
}
