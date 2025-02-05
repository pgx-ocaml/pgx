(* PG'OCaml is a set of OCaml bindings for the PostgreSQL database.
 *
 * PG'OCaml - type safe interface to PostgreSQL.
 * Copyright (C) 2005-2009 Richard Jones and other authors.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this library; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *)

module type S = Pgx.S with type 'a Io.t = 'a

external reraise : exn -> _ = "%reraise"

module Io = struct
  type 'a t = 'a
  type ssl_config
  let return x = x
  let (>>=) v f = f v
  let catch f fexn =
    try f () with
    | e -> fexn e
  let protect f ~(finally : unit -> unit) =
    let result = ref None in
    try
      result := Some (f ());
      raise Exit
    with
    | Exit as e ->
      finally ();
      (match !result with
       | Some x -> x
       | None -> reraise e)
    | e ->
      finally ();
      reraise e
end

let make ~net ~sw =
  let module Thread = struct
    include Io
    type sockaddr =
      | Unix of string
      | Inet of string * int
    type close_flow = unit -> unit
    type in_channel = Eio.Buf_read.t * close_flow
    type out_channel = Eio.Buf_write.t
    let random_list_element lst =
      List.nth lst (Random.int (List.length lst))
    let open_connection sockaddr =
      let addr : Eio.Net.Sockaddr.stream =
        match sockaddr with
        | Unix path -> `Unix path
        | Inet (hostname, port) ->
          let service = string_of_int port in
          let addrs = Eio.Net.getaddrinfo_stream net hostname ~service in
          random_list_element addrs
      in
      let flow = Eio.Net.connect ~sw net addr in
      let r = Eio.Buf_read.of_flow ~max_size:(1 * 1024 * 1024) flow in
      let close_flow_triggered, resolve_close_flow_triggered = Eio.Promise.create () in
      let w, resolve_w = Eio.Promise.create () in
      Eio.Fiber.fork ~sw begin fun () ->
        Eio.Buf_write.with_flow ~initial_size:(4 * 1024) flow begin fun w ->
          Eio.Promise.resolve resolve_w w;
          Eio.Promise.await close_flow_triggered
        end;
        Eio.Net.close flow
      end;
      let w = Eio.Promise.await w in
      let close_flow () =
        Eio.Promise.resolve resolve_close_flow_triggered ()
      in
      (r, close_flow), w
    let upgrade_ssl = `Not_supported
    let output_char w c =
      Eio.Buf_write.char w c
    let output_binary_int w n =
      Eio.Buf_write.BE.uint32 w (Int32.of_int n)
    let output_string w s =
      Eio.Buf_write.string w s
    let flush w =
      Eio.Buf_write.flush w
    let input_char (r, _) =
      Eio.Buf_read.any_char r
    let input_binary_int (r, _) =
      Eio.Buf_read.BE.uint32 r |> Int32.to_int
    let really_input (r, _) buf pos len =
      let s = Eio.Buf_read.take len r in
      Bytes.blit_string s 0 buf pos len
    let close_in (_r, close_flow) =
      close_flow ()
    let getlogin () =
      Eio_unix.run_in_systhread @@ fun () ->
      (* The unix getlogin syscall can fail *)
      let uid = Unix.getuid () in
      let pwuid = Unix.getpwuid uid in
      pwuid.pw_name
    let debug = prerr_endline
    module Sequencer = struct
      type 'a monad = 'a t
      type 'a t = 'a * Eio.Mutex.t
      let create t =
        t, Eio.Mutex.create ()
      let enqueue (t, mutex) f =
        Eio.Mutex.use_rw ~protect:true mutex (fun () -> f t)
    end
  end in
  let module M = Pgx.Make (Thread) in
  (module M : S)

module type S_with_t = sig
  module Impl : S
  val t : Impl.t
end

module type S_with_Prepared_s = sig
  module Impl : S
  val s : Impl.Prepared.s
end

type t = (module S_with_t)

let connect
    ~net
    ~sw
    (* ?(ssl:[ `Always of ssl_config | `Auto | `No ] option) *)
    ?host
    ?port
    ?user
    ?password
    ?database
    ?unix_domain_socket_dir
    ?verbose
    ?max_message_length
    ()
  =
  let (module Impl) = make ~net ~sw in
  let t = Impl.connect ?ssl:None ?host ?port ?user ?password ?database ?unix_domain_socket_dir ?verbose ?max_message_length () in
  let module M = struct
    module Impl = Impl
    let t = t
  end in
  ((module M) : t)

let with_conn
    ~net
    (* ?(ssl:[ `Always of ssl_config | `Auto | `No ] option) *)
    ?host
    ?port
    ?user
    ?password
    ?database
    ?unix_domain_socket_dir
    ?verbose
    ?max_message_length
    f
  =
  Eio.Switch.run begin fun sw ->
    let (module Impl) = make ~net ~sw in
    Impl.with_conn ?ssl:None ?host ?port ?user ?password ?database ?unix_domain_socket_dir ?verbose ?max_message_length begin fun t ->
      let module M = struct
        module Impl = Impl
        let t = t
      end in
      f ((module M) : t)
    end
  end

let close ((module M) : t) =
  M.Impl.close M.t

let ping ((module M) : t) =
  M.Impl.ping M.t

let alive ((module M) : t) =
  M.Impl.alive M.t

let begin_work ?isolation ?access ?deferrable ((module M) : t) =
  let t = M.Impl.begin_work ?isolation ?access ?deferrable M.t in
  let module M = struct
    module Impl = M.Impl
    let t = t
  end in
  ((module M) : t)

let commit ((module M) : t) =
  M.Impl.commit M.t

let rollback ((module M) : t) =
  M.Impl.rollback M.t

let with_transaction ?isolation ?access ?deferrable ((module M) : t) f =
  M.Impl.with_transaction ?isolation ?access ?deferrable M.t begin fun t ->
    let module M = struct
      module Impl = M.Impl
      let t = t
    end in
    f ((module M) : t)
  end

module Prepared = struct
  type s = (module S_with_Prepared_s)
  let sexp_of_s ((module M) : s) =
    M.Impl.Prepared.sexp_of_s M.s
  let prepare ?name ?types ((module M) : t) ~query =
    let s = M.Impl.Prepared.prepare ?name ?types M.t ~query in
    let module M = struct
      module Impl = M.Impl
      let s = s
    end in
    ((module M) : s)
  let close ((module M) : s) =
    M.Impl.Prepared.close M.s
  let with_prepare ?name ?types ((module M) : t) ~query ~f =
    M.Impl.Prepared.with_prepare ?name ?types M.t ~query ~f:begin fun s ->
      let module M = struct
        module Impl = M.Impl
        let s = s
      end in
      f ((module M) : s)
    end
  let execute ?portal ((module M) : s) =
    M.Impl.Prepared.execute ?portal M.s
  let execute_unit ?portal ((module M) : s) ~params =
    M.Impl.Prepared.execute_unit ?portal M.s ~params
  let execute_fold ?portal ((module M) : s) ~params ~init ~f =
    M.Impl.Prepared.execute_fold ?portal M.s ~params ~init ~f
  let execute_iter ?portal ((module M) : s) ~params ~f =
    M.Impl.Prepared.execute_iter ?portal M.s ~params ~f
  let execute_map ?portal ((module M) : s) ~params ~f =
    M.Impl.Prepared.execute_map ?portal M.s ~params ~f
  let execute_many ((module M) : s) ~params =
    M.Impl.Prepared.execute_many M.s ~params
  let describe ((module M) : s) =
    M.Impl.Prepared.describe M.s
  let close_portal ?portal ((module M) : s) =
    M.Impl.Prepared.close_portal ?portal M.s
  let describe_portal ?portal ((module M) : s) =
    M.Impl.Prepared.describe_portal ?portal M.s
end

let execute ?params ((module M) : t) query =
  M.Impl.execute ?params M.t query

let execute_unit ?params ((module M) : t) query =
  M.Impl.execute_unit ?params M.t query

let execute_fold ?params ((module M) : t) query ~init ~f =
  M.Impl.execute_fold ?params M.t query ~init ~f

let execute_map ?params ((module M) : t) query ~f =
  M.Impl.execute_map ?params M.t query ~f

let execute_iter ?params ((module M) : t) query ~f =
  M.Impl.execute_iter ?params M.t query ~f

let execute_many ((module M) : t) ~query ~params =
  M.Impl.execute_many M.t ~query ~params

let simple_query ((module M) : t) query =
  M.Impl.simple_query M.t query
