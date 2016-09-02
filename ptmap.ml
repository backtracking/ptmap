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

(*s Maps of integers implemented as Patricia trees, following Chris
    Okasaki and Andrew Gill's paper {\em Fast Mergeable Integer Maps}
    ({\tt\small http://www.cs.columbia.edu/\~{}cdo/papers.html\#ml98maps}).
    See the documentation of module [Ptset] which is also based on the
    same data-structure. *)

type key = int

type 'a t =
  | Empty
  | Leaf of int * 'a
  | Branch of int * int * 'a t * 'a t

let empty = Empty

let is_empty t = t = Empty

let zero_bit k m = (k land m) == 0

let rec mem k = function
  | Empty -> false
  | Leaf (j,_) -> k == j
  | Branch (_, m, l, r) -> mem k (if zero_bit k m then l else r)

let rec find k = function
  | Empty -> raise Not_found
  | Leaf (j,x) -> if k == j then x else raise Not_found
  | Branch (_, m, l, r) -> find k (if zero_bit k m then l else r)

let lowest_bit x = x land (-x)

let branching_bit p0 p1 = lowest_bit (p0 lxor p1)

let mask p m = p land (m-1)

let join (p0,t0,p1,t1) =
  let m = branching_bit p0 p1 in
  if zero_bit p0 m then
    Branch (mask p0 m, m, t0, t1)
  else
    Branch (mask p0 m, m, t1, t0)

let match_prefix k p m = (mask k m) == p

let add k x t =
  let rec ins = function
    | Empty -> Leaf (k,x)
    | Leaf (j,_) as t ->
	if j == k then Leaf (k,x) else join (k, Leaf (k,x), j, t)
    | Branch (p,m,t0,t1) as t ->
	if match_prefix k p m then
	  if zero_bit k m then
	    Branch (p, m, ins t0, t1)
	  else
	    Branch (p, m, t0, ins t1)
	else
	  join (k, Leaf (k,x), p, t)
  in
  ins t

let branch = function
  | (_,_,Empty,t) -> t
  | (_,_,t,Empty) -> t
  | (p,m,t0,t1)   -> Branch (p,m,t0,t1)

let remove k t =
  let rec rmv = function
    | Empty -> Empty
    | Leaf (j,_) as t -> if k == j then Empty else t
    | Branch (p,m,t0,t1) as t ->
	if match_prefix k p m then
	  if zero_bit k m then
	    branch (p, m, rmv t0, t1)
	  else
	    branch (p, m, t0, rmv t1)
	else
	  t
  in
  rmv t

let rec iter f = function
  | Empty -> ()
  | Leaf (k,x) -> f k x
  | Branch (_,_,t0,t1) -> iter f t0; iter f t1

let rec choose = function
  | Empty -> raise Not_found
  | Leaf (k,v) -> (k,v)
  | Branch (_,_,t0,t1) ->
    try choose t0
    with Not_found ->
      choose t1
(*$T choose
  try let _ = choose empty in false with Not_found -> true
  choose (add 1 true empty) = (1, true)
*)

let rec map f = function
  | Empty -> Empty
  | Leaf (k,x) -> Leaf (k, f x)
  | Branch (p,m,t0,t1) -> Branch (p, m, map f t0, map f t1)

let rec mapi f = function
  | Empty -> Empty
  | Leaf (k,x) -> Leaf (k, f k x)
  | Branch (p,m,t0,t1) -> Branch (p, m, mapi f t0, mapi f t1)

let rec fold f s accu = match s with
  | Empty -> accu
  | Leaf (k,x) -> f k x accu
  | Branch (_,_,t0,t1) -> fold f t0 (fold f t1 accu)

(* utility fun for unit tests *)
(*$inject
  let of_list l =
    List.fold_left (fun acc (k, v) ->
      add k v acc
    ) empty l
*)

let max_binding m =
  let init = choose m in
  fold (fun k' v' ((k, v) as acc) ->
      if k' > k then
        (k', v')
      else
        acc
    ) m init
(*$T max_binding
  (try let _ = max_binding empty in false with Not_found -> true) = true
  max_binding (of_list [(-1,false); (5,true); (0,false)]) = (5,true)
*)

let min_binding m =
  let init = choose m in
  fold (fun k' v' ((k, v) as acc) ->
      if k' < k then
        (k', v')
      else
        acc
    ) m init
(*$T min_binding
  (try let _ = min_binding empty in false with Not_found -> true) = true
  min_binding (of_list [(-1,false); (5,true); (0,false)]) = (-1,false)
*)

let bindings m =
  fold (fun k v acc ->
      (k, v) :: acc
    ) m []
(*$T bindings
  bindings empty = []
  List.sort Pervasives.compare (bindings (of_list [(-1,false); (5,true); (0,false)])) = \
    [(-1,false); (0,false); (5,true)]
*)

let cardinal m =
  fold (fun _k _v acc ->
      acc + 1
    ) m 0
(*$T cardinal
  cardinal empty = 0
  cardinal (of_list [(-1,false); (5,true); (0,false)]) = 3
*)

let singleton k v =
  add k v empty

let find_opt k m =
  try Some (find k m)
  with Not_found -> None

(* FBR: probably far from optimal algorithm *)
let merge f mx my =
  let mx_keys = List.rev_map fst (bindings mx) in
  let my_keys = List.rev_map fst (bindings my) in
  let unique_keys = List.sort_uniq Pervasives.compare (List.rev_append mx_keys my_keys) in
  List.fold_left (fun acc k ->
      let maybe_vx = find_opt k mx in
      let maybe_vy = find_opt k my in
      match f k maybe_vx maybe_vy with
      | None -> acc
      | Some z -> add k z acc
    ) empty unique_keys
(*$T merge
  let l1 = [(-1,-1); (0,0); (5,4)] in \
  let l2 = [(5,5)] in \
  let l3 = [(-1,-1); (0,0); (5,5)] in \
  equal (=) (of_list l3) \
    (merge (fun _k x y -> max x y) (of_list l1) (of_list l2))
*)

(* FBR: probably far from optimal algorithm *)
let union f mx my =
  let mx_keys = List.rev_map fst (bindings mx) in
  let my_keys = List.rev_map fst (bindings my) in
  let unique_keys = List.sort_uniq Pervasives.compare (List.rev_append mx_keys my_keys) in
  List.fold_left (fun acc k ->
      let maybe_vx = find_opt k mx in
      let maybe_vy = find_opt k my in
      match maybe_vx, maybe_vy with
      | None, None -> assert(false)
      | None, Some vy -> add k vy acc
      | Some vx, None -> add k vx acc
      | Some vx, Some vy ->
        match f k vx vy with
        | None -> acc
        | Some vz -> add k vz acc
    ) empty unique_keys
(*$T union
  let l1 = [(-1,false); (0,false); (5,true)] in \
  let l2 = [(1,true)] in \
  let l3 = l2 @ l1 in \
  let m1 = of_list l1 in \
  let m2 = of_list l2 in \
  let m3 = of_list l3 in \
  equal (=) m3 (union (fun _k x _y -> Some x) m1 m2)
*)

let partition p m =
  fold (fun k v (acc_yes, acc_no) ->
      if p k v then
        (add k v acc_yes, acc_no)
      else
        (acc_yes, add k v acc_no)
    ) m (empty, empty)
(*$T partition
  let l1 = [(-1,false); (0,false); (5,true)] in \
  let yes, no = partition (fun k _v -> k < 0) (of_list l1) in \
  equal (=) yes (of_list [(-1,false)]) && \
    equal (=) no (of_list (List.tl l1))
*)

let filter p m =
  fold (fun k v acc ->
      if p k v then
        add k v acc
      else
        acc
    ) m empty
(*$T filter
  let l1 = [(-1,false); (0,false); (5,true)] in \
  filter (fun k _v -> k = 0) (of_list l1) = of_list [(0,false)]
*)

exception Found

let exists p m =
  try
    iter (fun k v ->
        if p k v then raise Found
        else ()
      ) m;
    false
  with Found -> true
(*$T exists
  let l1 = [(-1,false); (0,false); (5,true)] in \
  let m1 = of_list l1 in \
  exists (fun k _v -> k = 0) m1 && \
    not (exists (fun k _v -> k = 6) m1)
*)

let for_all p m =
  try
    iter (fun k v ->
        if not (p k v) then raise Found
        else ()
      ) m;
    true
  with Found -> false
(*$T for_all
  let l1 = [(-1,false); (0,false); (5,true)] in \
  let m1 = of_list l1 in \
  for_all (fun k _v -> k = -1 || k = 0 || k = 5) m1
*)

(* we order constructors as Empty < Leaf < Branch *)
let compare cmp t1 t2 =
  let rec compare_aux t1 t2 = match t1,t2 with
    | Empty, Empty -> 0
    | Empty, _ -> -1
    | _, Empty -> 1
    | Leaf (k1,x1), Leaf (k2,x2) ->
	let c = compare k1 k2 in
	if c <> 0 then c else cmp x1 x2
    | Leaf _, Branch _ -> -1
    | Branch _, Leaf _ -> 1
    | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
	let c = compare p1 p2 in
	if c <> 0 then c else
	let c = compare m1 m2 in
	if c <> 0 then c else
        let c = compare_aux l1 l2 in
        if c <> 0 then c else
        compare_aux r1 r2
  in
  compare_aux t1 t2

let equal eq t1 t2 =
  let rec equal_aux t1 t2 = match t1, t2 with
    | Empty, Empty -> true
    | Leaf (k1,x1), Leaf (k2,x2) -> k1 = k2 && eq x1 x2
    | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
	p1 = p2 && m1 = m2 && equal_aux l1 l2 && equal_aux r1 r2
    | _ -> false
  in
  equal_aux t1 t2
