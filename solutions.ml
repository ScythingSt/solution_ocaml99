let () = print_endline "Hellow!";;

(* Problem 01 *)
let rec last (lst: 'a list): 'a option =
  match lst with
  | [] -> None
  | [x] -> Some x
  | _ :: xs -> last xs

(* Problem 02 *)
let rec last_two (lst: 'a list): ('a * 'a) option =
  match lst with
  | [] | [_] -> None
  | [x; y] -> Some (x,y)
  | _ :: xs -> last_two xs

(* Problem 03 *)
let rec at (idx: int) (lst: 'a list): 'a option =
  match idx, lst with
  | _, [] -> None
  | 1, x :: _ -> Some x
  | i, x :: xs -> at (i-1) xs
(** Alternative solution with pattern matching guards:
let rec at (idx: int) (lst: 'a list): 'a option =
  match lst with
  | [] -> None
  | x :: _ when idx = 1 -> Some x
  | _ :: xs -> at (idx-1) xs 
*)

(* Problem 04 *)
let rec length (lst: 'a list): int =
  let rec len_aux (lst: 'a list) (acc: int): int =
  match lst with
  | [] -> acc
  | _ :: xs -> len_aux xs (acc + 1)
  in len_aux lst 0

(* Problem 05 *)
let rec rev (lst: 'a list): 'a list =
  let rec rev_aux (lst: 'a list) (acc: 'a list): 'a list =
    match lst with
    | [] -> acc
    | x :: xs -> rev_aux xs (x :: acc) in
    rev_aux lst []

(* Problem 06 *)
let rec rev (lst: 'a list): 'a list =
  let rec rev_aux (lst: 'a list) (acc: 'a list): 'a list =
    match lst with
    | [] -> acc
    | x :: xs -> rev_aux xs (x :: acc) in
    rev_aux lst []

let rec is_palindrome (lst: 'a list): bool =
  lst = rev lst

(* Problem 07 *)
type 'a node =
  | One of 'a
  | Many of 'a node list

let rec flatten (lst: 'a node list): 'a list =
  match lst with
  | [] -> []
  | h :: t -> 
    match h with
    | One x -> x :: flatten t
    | Many xs -> flatten xs @ flatten t

(* Problem 08 *)
let rec compress (lst: 'a list): 'a list =
  let rec compress_aux (lst: 'a list) (acc: 'a list): 'a list =
    match lst with
    | [] -> acc
    | [x] -> x :: acc
    | x :: y :: tl -> 
      if (x=y) then compress_aux (y :: tl) acc else compress_aux (y :: tl) (x :: acc)
     in
  List.rev @@ compress_aux lst []

(* Problem 09 *)
let rec rev (lst: 'a list): 'a list =
  let rec rev_aux (lst: 'a list) (acc: 'a list): 'a list =
    match lst with
    | [] -> acc
    | x :: xs -> rev_aux xs (x :: acc) in
    rev_aux lst []

let rec pack (lst: 'a list): 'a list list =
  let rec pack_aux (lst: 'a list) (acc: 'a list list): 'a list list =
    match lst, acc with
    | [], acc -> acc
    | x :: xs, [] -> pack_aux xs [[x]]
    | x :: xs, h :: t ->
      match x = List.hd h with
      | true  -> pack_aux xs ((x :: h) :: t)
      | false -> pack_aux xs ([x] :: acc) in
  rev @@ pack_aux lst []

(* Problem 10 *)
let encode (lst: 'a list): (int * 'a) list =
  let rec encode_aux (lst: 'a list) (acc: (int * 'a) list): (int * 'a) list = 
    match lst, acc with
    | [],_ -> List.rev acc
    | h :: tl, [] -> encode_aux tl [(1,h)]
    | h :: tl, (n,x) :: xs -> 
      if (h=x) then encode_aux tl ((n+1,x) :: xs) else encode_aux tl ((1,h) :: acc)
    in
    encode_aux lst []

(* Problem 11 *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode (lst: 'a list): 'a rle list =
  let rec encode_aux (lst: 'a list) (acc: 'a rle list): 'a rle list = 
    match lst, acc with
    | [],_ -> List.rev acc
    | h :: tl, [] -> encode_aux tl [One h]
    | h :: tl, x :: xs -> 
      match x with
      | One o -> if (h=o) then encode_aux tl (Many (2,o) :: xs) else encode_aux tl (One h :: acc)
      | Many (n,o) -> if (h=o) then encode_aux tl (Many (n+1,o) :: xs) else encode_aux tl (One h :: acc)
    in
    encode_aux lst []

(* Problem 12 *)
let decode (lst: 'a rle list): 'a list =
  let rec decode_aux (lst: 'a rle list) (acc: 'a list): 'a list =
    match lst, acc with
    | [], _ -> List.rev acc
    | h :: tl, _ -> 
      match h with
      | One o -> decode_aux tl (o :: acc)
      | Many (2,o) -> decode_aux (One o :: tl) (o :: acc)
      | Many (n,o) -> decode_aux (Many (n-1,o) :: tl) (o :: acc)
    in
    decode_aux lst []

(* Problem 13 *)
(* Implement the so-called run-length encoding data compression method directly. But I did it directly... *)
(* I will do an indirect way later. *)

(* Problem 14 *)
let duplicate (lst: 'a list): 'a list =
  let rec duplicate_aux (lst: 'a list) (acc: 'a list): 'a list =
  match lst with
  | []-> List.rev acc
  | h :: tl -> duplicate_aux tl (h :: h :: acc)
  in
  duplicate_aux lst []

(* Problem 15 *)
let replicate (lst: 'a list) (num: int): 'a list =
  let times = num in
  let rec replicate_aux (lst: 'a list) (num: int) (acc: 'a list): 'a list =
  match lst, num with
  | [], _ -> List.rev acc
  | _ :: tl, 0 -> replicate_aux tl times acc
  | h :: tl, num -> replicate_aux lst (num-1) (h :: acc)
  in
  replicate_aux lst num []

(* Problem 16 *)
(* definition of n-th here is very unconventional. But we respect it for once here. *)
let drop (lst: 'a list) (num: int): 'a list =
  let rec drop_aux (lst: 'a list) (num: int) (acc: 'a list): 'a list =
  match lst, num with
  | [],_ -> List.rev acc
  | _ :: tl, 1 -> (List.rev acc) @ tl
  | h :: tl, _ -> drop_aux tl (num-1) (h :: acc)
  in
  drop_aux lst num []
  
(* Problem 17 *)
let split (lst: 'a list) (num: int): ('a list * 'a list) =
  let tup = ([], lst) in
  let rec split_aux (tup: 'a list * 'a list) (num: int): 'a list * 'a list =
  match tup, num with
  | (l, []), _ -> (List.rev l, [])
  | (l, r), 0 -> (List.rev l, r)
  | (l, y :: ys), num -> split_aux (y :: l, ys) (num - 1)
  in
  split_aux tup num

(* Problem 18 *)
let slice (lst: 'a list) (low: int) (high: int): 'a list =
  (* List.filteri (fun i -> fun _ -> (low <= i && i <= high)) lst *)
  let filter (pre: int -> bool) (lst: 'a list): 'a list =
    let rec filter_aux (pre: int -> bool) (lst: 'a list) (idx: int) (acc: 'a list): 'a list =
    match lst with
    | [] -> acc
    | x :: xs -> if (pre idx) 
        then filter_aux pre xs (idx + 1) (x :: acc)
        else  filter_aux pre xs (idx + 1) acc
  in
  List.rev (filter_aux pre lst 0 []) in
  filter (fun i -> (low <= i && i <= high)) lst

(* Problem 19 *)
let rotate (lst: 'a list) (idx: int): 'a list =
  let len = List.length lst in
  let rec modulo (a: int) (b: int): int =
    if (a >= 0) then a mod b else
      modulo (a+b) b in
  let rec rotate_aux (lst: 'a list) (idx: int) (l: 'a list) (r: 'a list): 'a list =
  match lst, (modulo idx len) <> 0 with
  | [], _ ->  l @ r
  | x :: xs, true  -> rotate_aux xs (idx-1) l (r @ [x;])
  | x :: xs, false -> rotate_aux xs 0 (l @ [x;]) r
  in
  rotate_aux lst idx [] []

(* Problem 20 *)
let remove_at (idx: int) (lst: 'a list): 'a list =
  let rec remove_aux (idx: int) (lst: 'a list) (acc: 'a list): 'a list =
  match idx, lst with
  | _, [] -> List.rev acc
  | 0, x :: xs -> (List.rev acc) @ xs
  | idx, x :: xs -> remove_aux (idx-1) xs (x :: acc) 
  in 
  remove_aux idx lst []

(* Problem 21 *)
let insert_at (ele: 'a) (idx: int) (lst: 'a list): 'a list =
  let rec insert_aux (ele: 'a)(idx: int) (lst: 'a list) (acc: 'a list): 'a list =
  match idx, lst with
  | _, [] -> (List.rev acc) @ [ele;]
  | 0, lst -> (List.rev acc) @ [ele;] @ lst
  | idx, x :: xs -> insert_aux ele (idx-1) xs (x :: acc)
  in
  insert_aux ele idx lst []

(* Problem 22 *)
let range (l: int) (h: int): int list =
  let rec range_aux (l: int) (h: int) (acc: int list): int list =
  if (l < h) then
  (if (l = h) then (h :: acc) else range_aux (l+1) (h) (l :: acc))
  else (if (l = h) then List.rev (* Why rev here? *) (h :: acc) else range_aux (l-1) (h) (l :: acc)) 
  in
  range_aux l h []

(* Problem 23 *)
let rand_select (lst: 'a list) (num: int): 'a list =
  if (num < 0) then [] else if (num >= List.length lst) then lst else
    let rec isolate (idx: int) (lst: 'a list): ('a * 'a list) =
      let rec isolate_aux (idx: int) (lst: 'a list) (lef: 'a list): ('a * 'a list) =
      match idx, lst with
      | _, [] -> failwith "UNREACHABLE"
      | 0, x :: xs -> x, lef @ xs
      | idx, x :: xs -> isolate_aux (idx-1) xs (x :: lef)
    in
    isolate_aux idx lst []
    in
  let rec rand_select_aux (lst: 'a list) (num: int) (acc: 'a list): 'a list =
  match lst, num with
  | [], _ | _, 0 -> acc
  | lst, num -> 
    let idx = Random.int_in_range ~min:0 ~max:(List.length lst - 1) in
    let x, xs = isolate idx lst in
    rand_select_aux xs (num-1) (x :: acc)
  in
  rand_select_aux lst num []

(* Problem 24 *)
let lotto_select (num: int) (max: int): int list =
  let rec lotto_select_aux (num: int) (max: int) (acc: int list): int list =
    let lot: int = Random.int_in_range ~min:1 ~max:max in
    if (num <= 0) then acc else lotto_select_aux (num-1) max (lot :: acc)
  in
  lotto_select_aux num max []

(* Problem 25 *)
let permutation (lst: 'a list): 'a list =
  if (lst = []) then lst else
  let len = List.length lst in
  let rec permutation_aux (lst: 'a list) (acc: 'a list) (len: int): 'a list =
    let rec isolate (idx: int) (lst: 'a list): ('a * 'a list) =
      let rec isolate_aux (idx: int) (lst: 'a list) (lef: 'a list): ('a * 'a list) =
      match idx, lst with
      | _, [] -> failwith "UNREACHABLE"
      | 0, x :: xs -> x, lef @ xs
      | idx, x :: xs -> isolate_aux (idx-1) xs (x :: lef)
    in
    isolate_aux idx lst []
    in
    
    if (lst = []) then acc else 
    let idx = Random.int_in_range ~min:0 ~max:(len-1) in
    let a_i, rest = isolate idx lst in
    permutation_aux rest (a_i :: acc) (len-1)
    in
    permutation_aux lst [] len

(* Problem 26 *)
let extract (k: int) (lst: 'a list): 'a list list =
  (*B Boundary check *)
  let len, msg = List.length lst, "invalid int of k." in
  if (k > len || k < 0) then failwith msg else if (k = 0) then [[]] else if (k = len) then [lst] else
  (*E Boundary check *)
  let rec extract_aux (k: int) (lst: 'a list) (ele: 'a list) (acc: 'a list list): 'a list list =
    let len = List.length lst in
    match lst with
    | [] -> acc
    | _ when k >= len -> (lst @ ele) :: acc
    | _ when k = 0    -> ele :: acc
    | x :: xs -> 
      (extract_aux (k-1) xs (x :: ele) acc) @ (extract_aux k xs ele acc)
    in
    extract_aux k lst [] []

(* Problem 27 *)
(* let group (lst: 'a list) (ptn: int list): ('a list list list, string) result =
  let validate (lst: 'a list) (ptn: int list): bool =
  begin
    let sum (ptn: int list): int =
    let rec sum_aux (ptn: int list) (acc: int): int =
      match ptn with
      | [] -> acc
      | x :: xs -> sum_aux xs (x + acc)
    in sum_aux ptn 0 in
    let is_nonnegtive (ptn: int list): bool =
    let rec is_nonnegtive_aux (ptn: int list) (acc: bool): bool =
      match ptn with
      | [] -> acc
      | x :: xs -> is_nonnegtive_aux xs (if x >= 0 then acc else false)
    in is_nonnegtive_aux ptn true in
    let len = List.length lst in
    
    if (sum ptn < len && is_nonnegtive ptn) then true else false
  end
  in
    if (not (validate lst ptn)) then Error "Cannot be grouped as requested." else 
  
  let group_aux (lst: 'a list) (ptn: int list): 'a list list list = *)
    (* # group ["a";"b";"c";"d"]  [2;1];; *)
    (* [[["a"; "b"]; ["c"]]; [["a"; "c"]; ["b"]]; [["b"; "c"]; ["a"]];[["a"; "b"]; ["d"]]; [["a"; "c"]; ["d"]]; [["b"; "c"]; ["d"]];[["a"; "d"]; ["b"]]; [["b"; "d"]; ["a"]]; [["a"; "d"]; ["c"]];[["b"; "d"]; ["c"]]; [["c"; "d"]; ["a"]]; [["c"; "d"]; ["b"]]] *)



(* Problem 31 *)
(* let is_prime (num: int): bool =
  if (num <= 1) then false else if (num < 4) then true else
    let i = ref 2 in
    let result = ref false in
    let terminate (res: bool ref) (idx: int ref): unit =
      res := true;
      idx := num; in
  while ((!i)*(!i) <= num) do
    if (num mod !i = 0) then terminate result i else 
    i := !i + 1;
  done;
  not !result

let is_prime (num: int): bool =
  if (num <= 1) then false else if (num <= 3) then true else
  let rec is_prime_aux (num: int) (tst: int): bool =
  if (tst * tst > num) then true else
  if (num mod tst = 0) then false else is_prime_aux num (tst+1) in
  is_prime_aux num 2 *)

(* Problem 32 *)
(* let gcd (a: int) (b: int): int =
  let rec gcdq (a: int) (b: int) (q: int): int =    
    if a >= b*q then gcdq a b (q+1) else 
    let q = q-1 in
    let r = a - b*q in 
    if r = 0 then b else gcdq b r 1

    in 
    let a, b = abs a, abs b in
    if (a < b) then gcdq b a 1 else gcdq a b 1 *)