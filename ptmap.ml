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

let find_opt k m = try Some (find k m) with Not_found -> None

let rec find_first_opt f = function
  | Empty -> None
  | Leaf (j,x) -> if f j then Some (j,x) else None
  | Branch (_, _, l, r) ->
    match find_first_opt f l, find_first_opt f r with
    | Some (lk,lv) , Some (rk,rv) -> if lk < rk then Some (lk,lv) else Some (rk,rv)
    | Some v, None | None, Some v -> Some v
    | None, None -> None

let find_first f = function
  | Empty -> raise Not_found
  | Leaf (j,x) -> if f j then (j,x) else raise Not_found
  | Branch (_, _, l, r) ->
    match find_first_opt f l, find_first_opt f r with
    | Some (lk,lv) , Some (rk,rv) -> if lk < rk then (lk,lv) else (rk,rv)
    | Some v, None | None, Some v -> v
    | None, None -> raise Not_found

let rec find_last_opt f = function
  | Empty -> None
  | Leaf (j,x) -> if f j then Some (j,x) else None
  | Branch (_, _, l, r) ->
    match find_last_opt f l, find_last_opt f r with
    | Some (lk,lv) , Some (rk,rv) -> if lk > rk then Some (lk,lv) else Some (rk,rv)
    | Some v, None | None, Some v -> Some v
    | None, None -> None

let find_last f = function
  | Empty -> raise Not_found
  | Leaf (j,x) -> if f j then (j,x) else raise Not_found
  | Branch (_, _, l, r) ->
    match find_last_opt f l, find_last_opt f r with
    | Some (lk,lv) , Some (rk,rv) -> if lk > rk then (lk,lv) else (rk,rv)
    | Some v, None | None, Some v -> v
    | None, None -> raise Not_found

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

let singleton k v =
  add k v empty

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

(* utility fun for unit tests *)
(*$inject
  let of_list l =
    List.fold_left (fun acc (k, v) ->
      add k v acc
    ) empty l
*)

let rec cardinal = function
  | Empty -> 0
  | Leaf _ -> 1
  | Branch (_,_,t0,t1) -> cardinal t0 + cardinal t1
(*$T cardinal
  cardinal empty = 0
  cardinal (of_list [(-1,false); (5,true); (0,false)]) = 3
*)

let rec iter f = function
  | Empty -> ()
  | Leaf (k,x) -> f k x
  | Branch (_,_,t0,t1) -> iter f t0; iter f t1

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

let rec for_all p = function
  | Empty -> true
  | Leaf (k, v)  -> p k v
  | Branch (_,_,t0,t1) -> for_all p t0 && for_all p t1

let rec exists p = function
  | Empty -> false
  | Leaf (k, v) -> p k v
  | Branch (_,_,t0,t1) -> exists p t0 || exists p t1

let rec filter pr = function
  | Empty -> Empty
  | Leaf (k, v) as t -> if pr k v then t else Empty
  | Branch (p,m,t0,t1) -> branch (p, m, filter pr t0, filter pr t1)

let partition p s =
  let rec part (t,f as acc) = function
    | Empty -> acc
    | Leaf (k, v) -> if p k v then (add k v t, f) else (t, add k v f)
    | Branch (_,_,t0,t1) -> part (part acc t0) t1
  in
  part (Empty, Empty) s

let rec choose = function
  | Empty -> raise Not_found
  | Leaf (k, v) -> (k, v)
  | Branch (_, _, t0, _) -> choose t0   (* we know that [t0] is non-empty *)
(*$T choose
  try let _ = choose empty in false with Not_found -> true
  choose (add 1 true empty) = (1, true)
*)

let rec choose_opt = function
  | Empty -> None
  | Leaf (k, v) -> Some (k, v)
  | Branch (_, _, t0, _) -> choose_opt t0   (* we know that [t0] is non-empty *)

let split x m =
  let coll k v (l, b, r) =
    if k < x then add k v l, b, r
    else if k > x then l, b, add k v r
    else l, Some v, r
  in
  fold coll m (empty, None, empty)

let rec min_binding = function
  | Empty -> raise Not_found
  | Leaf (k, v) -> (k, v)
  | Branch (_,_,s,t) ->
    let (ks, _) as bs = min_binding s in
    let (kt, _) as bt = min_binding t in
    if ks < kt then bs else bt
(*$T min_binding
  (try let _ = min_binding empty in false with Not_found -> true) = true
  min_binding (of_list [(-1,false); (5,true); (0,false)]) = (-1,false)
*)

let rec min_binding_opt = function
  | Empty -> None
  | Leaf (k, v) -> Some (k, v)
  | Branch (_,_,s,t) ->
    match (min_binding_opt s, min_binding_opt t) with
    | (None, None) -> None
    | (None, bt) -> bt
    | (bs, None) -> bs
    | ((Some (ks, _) as bs), (Some (kt, _) as bt)) ->
      if ks < kt then bs else bt

let rec max_binding = function
  | Empty -> raise Not_found
  | Leaf (k, v) -> (k, v)
  | Branch (_,_,s,t) ->
    let (ks, _) as bs = max_binding s in
    let (kt, _) as bt = max_binding t in
    if ks > kt then bs else bt
(*$T max_binding
  (try let _ = max_binding empty in false with Not_found -> true) = true
  max_binding (of_list [(-1,false); (5,true); (0,false)]) = (5,true)
*)

let rec max_binding_opt = function
  | Empty -> None
  | Leaf (k, v) -> Some (k, v)
  | Branch (_,_,s,t) ->
    match (max_binding_opt s, max_binding_opt t) with
    | (None, None) -> None
    | (None, bt) -> bt
    | (bs, None) -> bs
    | ((Some (ks, _) as bs), (Some (kt, _) as bt)) ->
      if ks > kt then bs else bt

let bindings m =
  fold (fun k v acc -> (k, v) :: acc) m []
(*$T bindings
  bindings empty = []
  List.sort Pervasives.compare (bindings (of_list [(-1,false); (5,true); (0,false)])) = \
    [(-1,false); (0,false); (5,true)]
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

let merge f m1 m2 =
  let add m k = function None -> m | Some v -> add k v m in
  (* first consider all bindings in m1 *)
  let m = fold
      (fun k1 v1 m -> add m k1 (f k1 (Some v1) (find_opt k1 m2))) m1 empty in
  (* then bindings in m2 that are not in m1 *)
  fold (fun k2 v2 m -> if mem k2 m1 then m else add m k2 (f k2 None (Some v2)))
    m2 m
(*$T merge
  let l1 = [(-1,-1); (0,0); (5,4)] in \
  let l2 = [(5,5)] in \
  let l3 = [(-1,-1); (0,0); (5,5)] in \
  equal (=) (of_list l3) \
    (merge (fun _k x y -> max x y) (of_list l1) (of_list l2))
*)

let union f m1 m2 =
  (* first, consider all bindings in m1 or in (m1 inter m2) *)
  let m =
    fold (fun k v m ->
        match find_opt k m2 with
        | None -> add k v m (* only in m1 *)
        | Some w ->
          begin (* in (m1 inter m2) *)
            match f k v w with
            | None -> m (* key is dropped *)
            | Some z -> add k z m
          end
      ) m1 empty
  in
  (* last, consider all bindings only in m2 *)
  fold (fun k v m ->
      match find_opt k m1 with
      | None -> add k v m (* only in m2 *)
      | Some _ -> m (* already processed before *)
    ) m2 m
(*$T union
  let l1 = [(-1,false); (0,false); (5,true)] in \
  let l2 = [(0,true); (6,true)] in \
  let l3 = [(-1,false); (5,true); (6,true)] in \
  let m1 = of_list l1 in \
  let m2 = of_list l2 in \
  let m3 = of_list l3 in \
  equal (=) m3 (union (fun _ _ _ -> None) m1 m2)
*)

let update x f m =
  match f (find_opt x m) with
  | None -> remove x m
  | Some z -> add x z m

let to_seq m =
  let rec prepend_seq m s = match m with
    | Empty -> s
    | Leaf (k, v) -> fun () -> Seq.Cons((k,v), s)
    | Branch (_, _, l, r) -> prepend_seq l (prepend_seq r s)
  in
  prepend_seq m Seq.empty

let to_seq_from k m =
  let rec prepend_seq m s = match m with
    | Empty -> s
    | Leaf (key, v) -> if key >= k then fun () -> Seq.Cons((key,v), s) else s
    | Branch (_, _, l, r) -> prepend_seq l (prepend_seq r s)
  in
  prepend_seq m Seq.empty

let add_seq s m =
  Seq.fold_left (fun m (k, v) -> add k v m) m s

let of_seq s =
  Seq.fold_left (fun m (k, v) -> add k v m) empty s
