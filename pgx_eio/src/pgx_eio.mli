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

val make : net:_ Eio.Net.t -> sw:Eio.Switch.t -> (module S)

include S

val connect :
  net:[> [> `Generic ] Eio.Net.ty ] Eio.Resource.t ->
  sw:Eio.Switch.t ->
  ?host:string ->
  ?port:int ->
  ?user:string ->
  ?password:string ->
  ?database:string ->
  ?unix_domain_socket_dir:string ->
  ?verbose:int ->
  ?max_message_length:int ->
  unit ->
  t

val with_conn :
  net:[> [> `Generic ] Eio.Net.ty ] Eio.Resource.t ->
  ?host:string ->
  ?port:int ->
  ?user:string ->
  ?password:string ->
  ?database:string ->
  ?unix_domain_socket_dir:string ->
  ?verbose:int ->
  ?max_message_length:int ->
  (t -> 'a) ->
  'a
