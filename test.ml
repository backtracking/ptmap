let rec int_pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = int_pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)

let max_int = (int_pow 2 30) -1

let test empty add mem =
  let seed = Random.int max_int in
  Random.init seed;
  let s =
    let rec loop s i =
      if i = 1000 then s else loop (add (Random.int max_int) true s) (succ i)
    in
    loop empty 0
  in
  Random.init seed;
  for i = 0 to 999 do assert (mem (Random.int max_int) s) done

let list_to_map lst =
  let rec loop l map = match l with
  | [] -> map
  | item :: ls -> loop ls (Ptmap.add item true map)
  in
  loop lst Ptmap.empty

let test_find_first =
  let k,v = Ptmap.find_first (fun i -> i > 3) (list_to_map [3; 1; 2; 4; 6; 5]) in
  assert (k = 4)

let test_find_first_opt =
  match Ptmap.find_first_opt (fun i -> i > 3) (list_to_map [3; 1; 2; 4; 6; 5]) with
  | Some (4, v) -> assert true
  | _ -> assert false

let test_find_last =
  let k,v = Ptmap.find_last (fun i -> i < 4) (list_to_map [3; 1; 2; 4; 6; 5]) in
  assert (k = 3)

let test_find_last_opt =
  match Ptmap.find_last_opt (fun i -> i < 4) (list_to_map [3; 1; 2; 4; 6; 5]) with
  | Some (3, v) -> assert true
  | _ -> assert false

let test_update_remove =
  let m = Ptmap.update 2 (fun _ -> None) (list_to_map [3; 1; 2; 4; 6; 5]) in
  match Ptmap.find_opt 2 m with
  | None -> assert true
  | _ -> assert false

let test_update_add =
  let m = Ptmap.update 2 (fun _ -> Some true) (list_to_map [3; 1; 4; 6; 5]) in
  match Ptmap.find_opt 2 m with
  | Some true -> assert true
  | _ -> assert true

let test_update_update =
  let m = Ptmap.update 2 (fun _ -> Some false) (list_to_map [3; 1; 2; 4; 6; 5]) in
  match Ptmap.find_opt 2 m with
  | Some false -> assert true
  | _ -> assert true

let main () =
  test Ptmap.empty Ptmap.add Ptmap.mem;
  test_find_first;
  test_find_first_opt;
  test_find_last;
  test_find_last_opt;
  test_update_remove;
  test_update_add;
  test_update_update

let () = main ()
