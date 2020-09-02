(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Lesser General Public            *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(** Maps over integers implemented as Patricia trees.  The following
   signature is a subset of [Map.S with type key = int], with the same
   specifications (not repeated here).

   Warning: [min_binding] and [max_binding] are linear w.r.t. the
   size of the map. They are barely more efficient than a
   straightforward implementation using [fold].
*)

type key = int

type (+'a) t

val empty: 'a t

val is_empty: 'a t -> bool

val mem: key -> 'a t -> bool

val add: key -> 'a -> 'a t -> 'a t

val update: key -> ('a option -> 'a option) -> 'a t -> 'a t

val singleton: key -> 'a -> 'a t

val remove: key -> 'a t -> 'a t

val merge:
         (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t

val union: (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t

val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int

val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val iter: (key -> 'a -> unit) -> 'a t -> unit

val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

val for_all: (key -> 'a -> bool) -> 'a t -> bool

val exists: (key -> 'a -> bool) -> 'a t -> bool

val filter: (key -> 'a -> bool) -> 'a t -> 'a t

val filter_map: (key -> 'a -> 'b option) -> 'a t -> 'b t

val partition: (key -> 'a -> bool) -> 'a t -> 'a t * 'a t

val cardinal: 'a t -> int

val bindings: 'a t -> (key * 'a) list

val min_binding: 'a t -> (key * 'a)

val min_binding_opt: 'a t -> (key * 'a) option

val max_binding: 'a t -> (key * 'a)

val max_binding_opt: 'a t -> (key * 'a) option

val choose: 'a t -> (key * 'a)

val choose_opt: 'a t -> (key * 'a) option

val split: key -> 'a t -> 'a t * 'a option * 'a t

val find: key -> 'a t -> 'a

val find_opt: key -> 'a t -> 'a option

val find_first: (key -> bool) -> 'a t -> key * 'a

val find_first_opt: (key -> bool) -> 'a t -> (key * 'a) option

val find_last: (key -> bool) -> 'a t -> key * 'a

val find_last_opt: (key -> bool) -> 'a t -> (key * 'a) option

val map: ('a -> 'b) -> 'a t -> 'b t

val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t

val to_seq : 'a t -> (key * 'a) Seq.t

val to_seq_from : key -> 'a t -> (key * 'a) Seq.t

val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t

val of_seq : (key * 'a) Seq.t -> 'a t
