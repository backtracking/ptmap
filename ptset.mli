(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(*i $Id$ i*)

(*s Sets of integers implemented as Patricia trees.  The following
    signature is exactly [Set.S with type elt = int], with the same
    specifications. This is a purely functional data-structure. The
    performances are similar to those of the standard library's module
    [Set]. The representation is unique and thus structural comparison
    can be performed on Patricia trees. *)

include Set.S with type elt = int

(*s Warning: [min_elt] and [max_elt] are linear w.r.t. the size of the
    set. In other words, [min_elt t] is barely more efficient than [fold
    min t (choose t)]. *)

(*s Additional functions not appearing in the signature [Set.S] from ocaml
    standard library. *)

(* [intersect u v] determines if sets [u] and [v] have a non-empty
   intersection. *)

val intersect : t -> t -> bool

(* [of_list l] constructs a set from the elements of the list *)

val of_list : elt list -> t

(* [to_list s] alias for [elements s] *)

val to_list : t -> elt list

(* [map f s] is the set whose elements are [f a0],[f a1]... [f
   aN], where [a0],[a1]...[aN] are the elements of [s].
   The elements are passed to [f] in unspecified order. *)

val map: (elt -> elt) -> t -> t

(*s Big-endian Patricia trees *)

module Big : sig
  include Set.S with type elt = int
  val intersect : t -> t -> bool
  val of_list : elt list -> t
  val to_list : t -> elt list
  val map: (elt -> elt) -> t -> t
end

(*s Big-endian Patricia trees with non-negative elements. Changes:
    - [add] and [singleton] raise [Invalid_arg] if a negative element is given
    - [mem] is slightly faster (the Patricia tree is now a search tree)
    - [min_elt] and [max_elt] are now O(log(N))
    - [elements] returns a list with elements in ascending order
 *)

module BigPos : sig
  include Set.S with type elt = int
  val intersect : t -> t -> bool
  val of_list : elt list -> t
  val to_list : t -> elt list
  val map: (elt -> elt) -> t -> t
end
