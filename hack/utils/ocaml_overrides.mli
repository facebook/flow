(*
 * Copyright (c) 2019, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

(* This `.mli` file was generated automatically. It may include extra
definitions that should not actually be exposed to the caller. If you notice
that this interface file is a poor interface, please take a few minutes to
clean it up manually, and then delete this comment once the interface is in
shape. *)

module Ocaml_unix = Unix
module Ocaml_Sys = Sys

module Unix : sig
  type error = Unix.error =
    | E2BIG
    | EACCES
    | EAGAIN
    | EBADF
    | EBUSY
    | ECHILD
    | EDEADLK
    | EDOM
    | EEXIST
    | EFAULT
    | EFBIG
    | EINTR
    | EINVAL
    | EIO
    | EISDIR
    | EMFILE
    | EMLINK
    | ENAMETOOLONG
    | ENFILE
    | ENODEV
    | ENOENT
    | ENOEXEC
    | ENOLCK
    | ENOMEM
    | ENOSPC
    | ENOSYS
    | ENOTDIR
    | ENOTEMPTY
    | ENOTTY
    | ENXIO
    | EPERM
    | EPIPE
    | ERANGE
    | EROFS
    | ESPIPE
    | ESRCH
    | EXDEV
    | EWOULDBLOCK
    | EINPROGRESS
    | EALREADY
    | ENOTSOCK
    | EDESTADDRREQ
    | EMSGSIZE
    | EPROTOTYPE
    | ENOPROTOOPT
    | EPROTONOSUPPORT
    | ESOCKTNOSUPPORT
    | EOPNOTSUPP
    | EPFNOSUPPORT
    | EAFNOSUPPORT
    | EADDRINUSE
    | EADDRNOTAVAIL
    | ENETDOWN
    | ENETUNREACH
    | ENETRESET
    | ECONNABORTED
    | ECONNRESET
    | ENOBUFS
    | EISCONN
    | ENOTCONN
    | ESHUTDOWN
    | ETOOMANYREFS
    | ETIMEDOUT
    | ECONNREFUSED
    | EHOSTDOWN
    | EHOSTUNREACH
    | ELOOP
    | EOVERFLOW
    | EUNKNOWNERR of int

  exception Unix_error of error * string * string

  val error_message : error -> string

  val handle_unix_error : ('a -> 'b) -> 'a -> 'b

  val environment : unit -> string array

  val getenv : string -> string

  val putenv : string -> string -> unit

  type process_status = Unix.process_status =
    | WEXITED of int
    | WSIGNALED of int
    | WSTOPPED of int

  type wait_flag = Unix.wait_flag =
    | WNOHANG
    | WUNTRACED

  val execv : string -> string array -> 'a

  val execve : string -> string array -> string array -> 'a

  val execvp : string -> string array -> 'a

  val execvpe : string -> string array -> string array -> 'a

  val fork : unit -> int

  val wait : unit -> int * process_status

  val waitpid : wait_flag list -> int -> int * process_status

  val system : string -> process_status

  val getpid : unit -> int

  val getppid : unit -> int

  val nice : int -> int

  type file_descr = Unix.file_descr

  val stdin : file_descr

  val stdout : file_descr

  val stderr : file_descr

  type open_flag = Unix.open_flag =
    | O_RDONLY
    | O_WRONLY
    | O_RDWR
    | O_NONBLOCK
    | O_APPEND
    | O_CREAT
    | O_TRUNC
    | O_EXCL
    | O_NOCTTY
    | O_DSYNC
    | O_SYNC
    | O_RSYNC
    | O_SHARE_DELETE
    | O_CLOEXEC
    | O_KEEPEXEC

  type file_perm = int

  val openfile : string -> open_flag list -> file_perm -> file_descr

  val close : file_descr -> unit

  val read : file_descr -> bytes -> int -> int -> int

  val write : file_descr -> bytes -> int -> int -> int

  val single_write : file_descr -> bytes -> int -> int -> int

  val write_substring : file_descr -> string -> int -> int -> int

  val single_write_substring : file_descr -> string -> int -> int -> int

  val in_channel_of_descr : file_descr -> in_channel

  val out_channel_of_descr : file_descr -> out_channel

  val descr_of_in_channel : in_channel -> file_descr

  val descr_of_out_channel : out_channel -> file_descr

  type seek_command = Unix.seek_command =
    | SEEK_SET
    | SEEK_CUR
    | SEEK_END

  val lseek : file_descr -> int -> seek_command -> int

  val truncate : string -> int -> unit

  val ftruncate : file_descr -> int -> unit

  type file_kind = Unix.file_kind =
    | S_REG
    | S_DIR
    | S_CHR
    | S_BLK
    | S_LNK
    | S_FIFO
    | S_SOCK

  type stats = Unix.stats = {
    st_dev: int;
    st_ino: int;
    st_kind: file_kind;
    st_perm: file_perm;
    st_nlink: int;
    st_uid: int;
    st_gid: int;
    st_rdev: int;
    st_size: int;
    st_atime: float;
    st_mtime: float;
    st_ctime: float;
  }

  val stat : string -> stats

  val lstat : string -> stats

  val fstat : file_descr -> stats

  val isatty : file_descr -> bool

  module LargeFile = Unix.LargeFile

  val unlink : string -> unit

  val link : string -> string -> unit

  type access_permission = Unix.access_permission =
    | R_OK
    | W_OK
    | X_OK
    | F_OK

  val chmod : string -> file_perm -> unit

  val fchmod : file_descr -> file_perm -> unit

  val chown : string -> int -> int -> unit

  val fchown : file_descr -> int -> int -> unit

  val umask : int -> int

  val access : string -> access_permission list -> unit

  val dup : ?cloexec:bool -> file_descr -> file_descr

  val dup2 : ?cloexec:bool -> file_descr -> file_descr -> unit

  val set_nonblock : file_descr -> unit

  val clear_nonblock : file_descr -> unit

  val set_close_on_exec : file_descr -> unit

  val clear_close_on_exec : file_descr -> unit

  val rmdir : string -> unit

  val chroot : string -> unit

  type dir_handle = Unix.dir_handle

  val opendir : string -> dir_handle

  val readdir : dir_handle -> string

  val rewinddir : dir_handle -> unit

  val closedir : dir_handle -> unit

  val pipe : ?cloexec:bool -> unit -> file_descr * file_descr

  val mkfifo : string -> file_perm -> unit

  val create_process :
    string -> string array -> file_descr -> file_descr -> file_descr -> int

  val create_process_env :
    string ->
    string array ->
    string array ->
    file_descr ->
    file_descr ->
    file_descr ->
    int

  val open_process_in : string -> in_channel

  val open_process_out : string -> out_channel

  val open_process : string -> in_channel * out_channel

  val open_process_full :
    string -> string array -> in_channel * out_channel * in_channel

  val close_process_in : in_channel -> process_status

  val close_process_out : out_channel -> process_status

  val close_process : in_channel * out_channel -> process_status

  val close_process_full :
    in_channel * out_channel * in_channel -> process_status

  val symlink : ?to_dir:bool -> string -> string -> unit

  val has_symlink : unit -> bool

  val readlink : string -> string

  val select :
    file_descr list ->
    file_descr list ->
    file_descr list ->
    float ->
    file_descr list * file_descr list * file_descr list

  type lock_command = Unix.lock_command =
    | F_ULOCK
    | F_LOCK
    | F_TLOCK
    | F_TEST
    | F_RLOCK
    | F_TRLOCK

  val lockf : file_descr -> lock_command -> int -> unit

  val kill : int -> int -> unit

  type sigprocmask_command = Unix.sigprocmask_command =
    | SIG_SETMASK
    | SIG_BLOCK
    | SIG_UNBLOCK

  val sigprocmask : sigprocmask_command -> int list -> int list

  val sigpending : unit -> int list

  val sigsuspend : int list -> unit

  val pause : unit -> unit

  type process_times = Unix.process_times = {
    tms_utime: float;
    tms_stime: float;
    tms_cutime: float;
    tms_cstime: float;
  }

  type tm = Unix.tm = {
    tm_sec: int;
    tm_min: int;
    tm_hour: int;
    tm_mday: int;
    tm_mon: int;
    tm_year: int;
    tm_wday: int;
    tm_yday: int;
    tm_isdst: bool;
  }

  val time : unit -> float

  val gettimeofday : unit -> float

  val gmtime : float -> tm

  val localtime : float -> tm

  val mktime : tm -> float * tm

  val alarm : int -> int

  val sleep : int -> unit

  val sleepf : float -> unit

  val times : unit -> process_times

  val utimes : string -> float -> float -> unit

  type interval_timer = Unix.interval_timer =
    | ITIMER_REAL
    | ITIMER_VIRTUAL
    | ITIMER_PROF

  type interval_timer_status = Unix.interval_timer_status = {
    it_interval: float;
    it_value: float;
  }

  val getitimer : interval_timer -> interval_timer_status

  val setitimer :
    interval_timer -> interval_timer_status -> interval_timer_status

  val getuid : unit -> int

  val geteuid : unit -> int

  val setuid : int -> unit

  val getgid : unit -> int

  val getegid : unit -> int

  val setgid : int -> unit

  val getgroups : unit -> int array

  val setgroups : int array -> unit

  val initgroups : string -> int -> unit

  type passwd_entry = Unix.passwd_entry = {
    pw_name: string;
    pw_passwd: string;
    pw_uid: int;
    pw_gid: int;
    pw_gecos: string;
    pw_dir: string;
    pw_shell: string;
  }

  type group_entry = Unix.group_entry = {
    gr_name: string;
    gr_passwd: string;
    gr_gid: int;
    gr_mem: string array;
  }

  val getlogin : unit -> string

  val getpwnam : string -> passwd_entry

  val getgrnam : string -> group_entry

  val getpwuid : int -> passwd_entry

  val getgrgid : int -> group_entry

  type inet_addr = Unix.inet_addr

  val inet_addr_of_string : string -> inet_addr

  val string_of_inet_addr : inet_addr -> string

  val inet_addr_any : inet_addr

  val inet_addr_loopback : inet_addr

  val inet6_addr_any : inet_addr

  val inet6_addr_loopback : inet_addr

  type socket_domain = Unix.socket_domain =
    | PF_UNIX
    | PF_INET
    | PF_INET6

  type socket_type = Unix.socket_type =
    | SOCK_STREAM
    | SOCK_DGRAM
    | SOCK_RAW
    | SOCK_SEQPACKET

  type sockaddr = Unix.sockaddr =
    | ADDR_UNIX of string
    | ADDR_INET of inet_addr * int

  val socket :
    ?cloexec:bool -> socket_domain -> socket_type -> int -> file_descr

  val domain_of_sockaddr : sockaddr -> socket_domain

  val socketpair :
    ?cloexec:bool ->
    socket_domain ->
    socket_type ->
    int ->
    file_descr * file_descr

  val accept : ?cloexec:bool -> file_descr -> file_descr * sockaddr

  val bind : file_descr -> sockaddr -> unit

  val connect : file_descr -> sockaddr -> unit

  val listen : file_descr -> int -> unit

  type shutdown_command = Unix.shutdown_command =
    | SHUTDOWN_RECEIVE
    | SHUTDOWN_SEND
    | SHUTDOWN_ALL

  val shutdown : file_descr -> shutdown_command -> unit

  val getsockname : file_descr -> sockaddr

  val getpeername : file_descr -> sockaddr

  type msg_flag = Unix.msg_flag =
    | MSG_OOB
    | MSG_DONTROUTE
    | MSG_PEEK

  val recv : file_descr -> bytes -> int -> int -> msg_flag list -> int

  val recvfrom :
    file_descr -> bytes -> int -> int -> msg_flag list -> int * sockaddr

  val send : file_descr -> bytes -> int -> int -> msg_flag list -> int

  val send_substring :
    file_descr -> string -> int -> int -> msg_flag list -> int

  val sendto :
    file_descr -> bytes -> int -> int -> msg_flag list -> sockaddr -> int

  val sendto_substring :
    file_descr -> string -> int -> int -> msg_flag list -> sockaddr -> int

  type socket_bool_option = Unix.socket_bool_option =
    | SO_DEBUG
    | SO_BROADCAST
    | SO_REUSEADDR
    | SO_KEEPALIVE
    | SO_DONTROUTE
    | SO_OOBINLINE
    | SO_ACCEPTCONN
    | TCP_NODELAY
    | IPV6_ONLY

  type socket_int_option = Unix.socket_int_option =
    | SO_SNDBUF
    | SO_RCVBUF
    | SO_ERROR
    | SO_TYPE
    | SO_RCVLOWAT
    | SO_SNDLOWAT

  type socket_optint_option = Unix.socket_optint_option = SO_LINGER

  type socket_float_option = Unix.socket_float_option =
    | SO_RCVTIMEO
    | SO_SNDTIMEO

  val getsockopt : file_descr -> socket_bool_option -> bool

  val setsockopt : file_descr -> socket_bool_option -> bool -> unit

  val getsockopt_int : file_descr -> socket_int_option -> int

  val setsockopt_int : file_descr -> socket_int_option -> int -> unit

  val getsockopt_optint : file_descr -> socket_optint_option -> int option

  val setsockopt_optint :
    file_descr -> socket_optint_option -> int option -> unit

  val getsockopt_float : file_descr -> socket_float_option -> float

  val setsockopt_float : file_descr -> socket_float_option -> float -> unit

  val getsockopt_error : file_descr -> error option

  val open_connection : sockaddr -> in_channel * out_channel

  val shutdown_connection : in_channel -> unit

  val establish_server :
    (in_channel -> out_channel -> unit) -> sockaddr -> unit

  type host_entry = Unix.host_entry = {
    h_name: string;
    h_aliases: string array;
    h_addrtype: socket_domain;
    h_addr_list: inet_addr array;
  }

  type protocol_entry = Unix.protocol_entry = {
    p_name: string;
    p_aliases: string array;
    p_proto: int;
  }

  type service_entry = Unix.service_entry = {
    s_name: string;
    s_aliases: string array;
    s_port: int;
    s_proto: string;
  }

  val gethostname : unit -> string

  val gethostbyname : string -> host_entry

  val gethostbyaddr : inet_addr -> host_entry

  val getprotobyname : string -> protocol_entry

  val getprotobynumber : int -> protocol_entry

  val getservbyname : string -> string -> service_entry

  val getservbyport : int -> string -> service_entry

  type addr_info = Unix.addr_info = {
    ai_family: socket_domain;
    ai_socktype: socket_type;
    ai_protocol: int;
    ai_addr: sockaddr;
    ai_canonname: string;
  }

  type getaddrinfo_option = Unix.getaddrinfo_option =
    | AI_FAMILY of socket_domain
    | AI_SOCKTYPE of socket_type
    | AI_PROTOCOL of int
    | AI_NUMERICHOST
    | AI_CANONNAME
    | AI_PASSIVE

  val getaddrinfo :
    string -> string -> getaddrinfo_option list -> addr_info list

  type name_info = Unix.name_info = {
    ni_hostname: string;
    ni_service: string;
  }

  type getnameinfo_option = Unix.getnameinfo_option =
    | NI_NOFQDN
    | NI_NUMERICHOST
    | NI_NAMEREQD
    | NI_NUMERICSERV
    | NI_DGRAM

  val getnameinfo : sockaddr -> getnameinfo_option list -> name_info

  type terminal_io = Unix.terminal_io = {
    mutable c_ignbrk: bool;
    mutable c_brkint: bool;
    mutable c_ignpar: bool;
    mutable c_parmrk: bool;
    mutable c_inpck: bool;
    mutable c_istrip: bool;
    mutable c_inlcr: bool;
    mutable c_igncr: bool;
    mutable c_icrnl: bool;
    mutable c_ixon: bool;
    mutable c_ixoff: bool;
    mutable c_opost: bool;
    mutable c_obaud: int;
    mutable c_ibaud: int;
    mutable c_csize: int;
    mutable c_cstopb: int;
    mutable c_cread: bool;
    mutable c_parenb: bool;
    mutable c_parodd: bool;
    mutable c_hupcl: bool;
    mutable c_clocal: bool;
    mutable c_isig: bool;
    mutable c_icanon: bool;
    mutable c_noflsh: bool;
    mutable c_echo: bool;
    mutable c_echoe: bool;
    mutable c_echok: bool;
    mutable c_echonl: bool;
    mutable c_vintr: char;
    mutable c_vquit: char;
    mutable c_verase: char;
    mutable c_vkill: char;
    mutable c_veof: char;
    mutable c_veol: char;
    mutable c_vmin: int;
    mutable c_vtime: int;
    mutable c_vstart: char;
    mutable c_vstop: char;
  }

  val tcgetattr : file_descr -> terminal_io

  type setattr_when = Unix.setattr_when =
    | TCSANOW
    | TCSADRAIN
    | TCSAFLUSH

  val tcsetattr : file_descr -> setattr_when -> terminal_io -> unit

  val tcsendbreak : file_descr -> int -> unit

  val tcdrain : file_descr -> unit

  type flush_queue = Unix.flush_queue =
    | TCIFLUSH
    | TCOFLUSH
    | TCIOFLUSH

  val tcflush : file_descr -> flush_queue -> unit

  type flow_action = Unix.flow_action =
    | TCOOFF
    | TCOON
    | TCIOFF
    | TCION

  val tcflow : file_descr -> flow_action -> unit

  val setsid : unit -> int

  val getcwd : unit -> string

  val chdir : string -> unit

  val mkdir : string -> int -> unit

  val rename : string -> string -> unit
end

module Sys : sig
  val argv : string array

  val executable_name : string

  external remove : string -> unit = "caml_sys_remove"

  external getenv : string -> string = "caml_sys_getenv"

  val getenv_opt : string -> string option

  external command : string -> int = "caml_sys_system_command"

  external time : unit -> (float[@unboxed])
    = "caml_sys_time" "caml_sys_time_unboxed"
    [@@noalloc]

  val interactive : bool ref

  val os_type : string

  type backend_type = Sys.backend_type =
    | Native
    | Bytecode
    | Other of string

  val backend_type : backend_type

  val unix : bool

  val win32 : bool

  val cygwin : bool

  val word_size : int

  val int_size : int

  val big_endian : bool

  val max_string_length : int

  val max_array_length : int

  external runtime_variant : unit -> string = "caml_runtime_variant"

  external runtime_parameters : unit -> string = "caml_runtime_parameters"

  type signal_behavior = Sys.signal_behavior =
    | Signal_default
    | Signal_ignore
    | Signal_handle of (int -> unit)

  external signal : int -> signal_behavior -> signal_behavior
    = "caml_install_signal_handler"

  val set_signal : int -> signal_behavior -> unit

  val sigabrt : int

  val sigalrm : int

  val sigfpe : int

  val sighup : int

  val sigill : int

  val sigint : int

  val sigkill : int

  val sigpipe : int

  val sigquit : int

  val sigsegv : int

  val sigterm : int

  val sigusr1 : int

  val sigusr2 : int

  val sigchld : int

  val sigcont : int

  val sigstop : int

  val sigtstp : int

  val sigttin : int

  val sigttou : int

  val sigvtalrm : int

  val sigprof : int

  val sigbus : int

  val sigpoll : int

  val sigsys : int

  val sigtrap : int

  val sigurg : int

  val sigxcpu : int

  val sigxfsz : int

  exception Break

  val catch_break : bool -> unit

  val ocaml_version : string

  val enable_runtime_warnings : bool -> unit

  val runtime_warnings_enabled : unit -> bool

  external opaque_identity : 'a -> 'a = "%opaque"

  val getcwd : unit -> string

  val chdir : string -> unit

  val is_directory : string -> bool

  val rename : string -> string -> unit

  val file_exists : string -> bool

  val readdir : string -> string array
end
