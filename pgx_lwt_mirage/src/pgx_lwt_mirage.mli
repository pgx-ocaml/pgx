(* Copyright (C) 2020 Petter A. Urkedal
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version,
 * with the OCaml static compilation exception.
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

module Make
    (STACK : Tcpip.Stack.V4V6)
    (H : Happy_eyeballs_mirage.S with type stack = STACK.t
                                  and type flow = STACK.TCP.flow) : sig
  val connect : STACK.t * H.t -> (module Pgx_lwt.S)
end
