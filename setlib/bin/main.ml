let not f = if f = false then true else false

let boolprint a = print_endline (string_of_bool a)

(*
let debug_list a =
  let () = print_endline "["  in
  let () = List.iter (Printf.printf "%i\n") a in
  let () = print_endline "]" in ()
*)

let rec is_in a b = 
  match b with
  | [] -> false
  | x :: y -> if x = a then true else is_in a y

let rec a_subset_of_b a b =
  match a with
  | [] -> true
  | x :: y -> if not (is_in x b) then false else a_subset_of_b y b



let rec union a b =
  match a with
  | [] -> b 
  | x :: y -> union y (if not (is_in x b) then x :: b else b)

let rec comb l u g = 
  match l with
  | [] -> g 
  | x :: y -> comb y u ((union x u) :: g)

let rec comb2 l u g =
  match l with
  | [] -> g
  | x :: y -> comb2 y u (union (comb u x []) g)



let set_equal a b = if (a_subset_of_b a b) = true && (a_subset_of_b b a) then true else false 

let rec set_in_b a b = 
  match b with
  | [] -> false
  | x :: y -> if set_equal x a then true else set_in_b a y

let rec unionchecker a b =
  match a with
  | [] -> true
  | x :: y -> if not (set_in_b x b) then false else unionchecker y b

let unionclosed a = unionchecker (comb2 a a []) a



let ucexample = [[1;2;3]; [2;4]; [1;2;3;4]; [1;]; [1;7;3]; [1;2;4;3;7]]
let uoexample = [[1;2;3];[2;5;9];[4;7;8]]

let () = boolprint (unionclosed ucexample)
let () = boolprint (unionclosed uoexample)