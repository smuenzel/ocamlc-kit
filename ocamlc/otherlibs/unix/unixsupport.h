/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_UNIXSUPPORT_H
#define CAML_UNIXSUPPORT_H

#include <caml/misc.h>

#ifdef _WIN32 /* Windows */
#define WIN32_LEAN_AND_MEAN
#include <wtypes.h>
#include <winbase.h>
#include <stdlib.h>
#include <direct.h>
#include <process.h>
#include <sys/types.h>
#include <winsock2.h>
#include <ws2tcpip.h>
#include <wspiapi.h>
#else /* Unix */
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _WIN32
struct filedescr {
  union {
    HANDLE handle;
    SOCKET socket;
  } fd;                   /* Real windows handle */
  enum { KIND_HANDLE, KIND_SOCKET } kind;
  int crt_fd;             /* C runtime descriptor */
  unsigned int flags_fd;  /* See FLAGS_FD_* */
};

/* Information stored in flags_fd, describing more precisely the socket
 * and its status. The whole flags_fd is initialized to 0.
 */

/* Blocking or nonblocking.  By default a filedescr is in blocking state */
#define FLAGS_FD_IS_BLOCKING (1<<0)


#define Handle_val(v) (((struct filedescr *) Data_custom_val(v))->fd.handle)
#define Socket_val(v) (((struct filedescr *) Data_custom_val(v))->fd.socket)
#define Descr_kind_val(v) (((struct filedescr *) Data_custom_val(v))->kind)
#define CRT_fd_val(v) (((struct filedescr *) Data_custom_val(v))->crt_fd)
#define Flags_fd_val(v) (((struct filedescr *) Data_custom_val(v))->flags_fd)

extern value win_alloc_handle(HANDLE);
extern value win_alloc_socket(SOCKET);
extern int win_CRT_fd_of_filedescr(value handle);

extern SOCKET win32_socket(int domain, int type, int protocol,
                           LPWSAPROTOCOL_INFO info,
                           BOOL inherit);

#define NO_CRT_FD (-1)

extern void win32_maperr(DWORD errcode);
#endif /* _WIN32 */

#define Nothing ((value) 0)

extern value unix_error_of_code (int errcode);
extern int code_of_unix_error (value error);

CAMLnoreturn_start
extern void unix_error (int errcode, const char * cmdname, value arg)
CAMLnoreturn_end;

CAMLnoreturn_start
extern void uerror (const char * cmdname, value arg)
CAMLnoreturn_end;

extern void caml_unix_check_path(value path, const char * cmdname);

#define UNIX_BUFFER_SIZE 65536

#define DIR_Val(v) *((DIR **) &Field(v, 0))

extern char_os ** cstringvect(value arg, char * cmdname);
extern void cstringvect_free(char_os **);

extern int unix_cloexec_default;
extern int unix_cloexec_p(value cloexec);

#ifdef _WIN32
extern int win_set_inherit(HANDLE fd, BOOL inherit);
/* This is a best effort, not guaranteed to work, so don't fail on error */
#define win_set_cloexec(fd, cloexec) \
  win_set_inherit((fd), ! unix_cloexec_p((cloexec)))
#else
extern void unix_set_cloexec(int fd, char * cmdname, value arg);
extern void unix_clear_cloexec(int fd, char * cmdname, value arg);
#endif

#ifdef __cplusplus
}
#endif

#ifdef _WIN32
#define EXECV_CAST (const char_os * const *)
#else
#define EXECV_CAST
#endif

#endif /* CAML_UNIXSUPPORT_H */
