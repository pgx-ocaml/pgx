(** Pgx_value types using Core's Date and Time_float modules *)
open Core

type v = Pgx.Value.v [@@deriving compare, sexp_of]
type t = Pgx.Value.t [@@deriving compare, sexp_of]

include module type of Pgx.Value with type v := v and type t := t

val of_date : Date.t -> t
val to_date_exn : t -> Date.t
val to_date : t -> Date.t option
val of_time : Time_float.t -> t
val to_time_exn : t -> Time_float.t
val to_time : t -> Time_float.t option
