let () = Printf.printf "%s\n" "Ocaml Exercises. https://ocaml.org/exercises";;



(* Tail of a List *)
(* Write a function last : 'a list -> 'a option that returns the last element of a list *)
let rec last lst = 
  match lst with
  | [] -> None
  | [el] -> Some el
  | _ :: tail -> last tail
;;

(* Test with valid list *)
(* Should print "Last element: 8" *)
let last_list_1 = [1; 2; 3; 4; 5; 6; 7; 8];;
Printf.printf "Last element: %s\n" (
  match (last last_list_1) with 
  | None -> "Empty list"
  | Some el -> string_of_int el
);;

(* Test with invalid list *)
(* Should print "Last element: Empty list" *)
let last_list_2 = [];;
Printf.printf "Last element: %s\n" (
  match (last last_list_2) with 
  | None -> "Empty list"
  | Some el -> string_of_int el
);;



(* Last Two Elements of a List  *)
(* Find the last but one (last and penultimate) elements of a list. *)
let rec last_two lst = 
  match lst with
  | [] | [_] -> None
  | [x; y] -> Some [x; y]
  | _ :: tail -> last_two tail
;;

(* Test with a valid list *)
(* Should print "Last 2 elements: 4 5" *)
let last_two_list_1 = ["1"; "2"; "3"; "4"; "5"];;
Printf.printf "Last 2 elements: %s\n" (
  match (last_two last_two_list_1) with 
  | None -> "Length is less than 2"
  | Some lst -> (String.concat " " lst)
);;

(* Test with a invalid list (empty) *)
(* Should print "Last 2 elements: Length is less than 2" *)
let last_two_list_2 = [];;
Printf.printf "Last 2 elements: %s\n" (
  match (last_two last_two_list_2) with 
  | None -> "Length is less than 2"
  | Some lst -> (String.concat " " lst)
);;

(* Test with a invalid list (one element) *)
(* Should print "Last 2 elements: Length is less than 2" *)
let last_two_list_3 = ["1"];;
Printf.printf "Last 2 elements: %s\n" (
  match (last_two last_two_list_3) with 
  | None -> "Length is less than 2"
  | Some lst -> (String.concat " " lst)
);;



(* N'th Element of a List  *)
(* Find the N'th element of a list. *)
let rec nth lst n = 
  match lst with 
  | [] -> None
  | head :: tail -> match n with 
    | 0 -> Some head
    | _ -> nth tail (n - 1)
;;

(* Test with valid list *)
(* Should print "Element at index 4 is 5" *)
let nth_list_1 = ["1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"];;
Printf.printf "Element at index 4 is %s\n" (
  match (nth nth_list_1 4) with
  | None -> "does not exist"
  | Some el -> el
);;

(* Test with invalid list *)
(* Should print "Element at index 4 does not exist" *)
let nth_list_2 = [];;
Printf.printf "Element at index 4 is %s\n" (
  match (nth nth_list_2 4) with
  | None -> "does not exist"
  | Some el -> el
);;




(* Length of a List  *)
(* Find the number of elements of a list. *)
let length lst = 
  let rec loop n = function
    | [] -> n
    | _ :: tail -> loop (n + 1) tail
  in
  loop 0 lst
;;

(* Test with valid list *)
(* Should print "Length: 5" *)
let length_list_1 = [1; 2; 3; 4; 5];;
Printf.printf "Length: %d\n" (length length_list_1);;

(* Test with invalid list (empty) *)
(* Should print "Length: 0" *)
let length_list_2 = [];;
Printf.printf "Length: %d\n" (length length_list_2);;




(* Reverse a List  *)
(* Reverse a list. *)
let reverse lst = 
  let rec prepend lst rev = 
    match lst with
    | [] -> rev
    | head :: tail -> prepend tail (head :: rev)
  in 
  prepend lst []
;;

(* Test with valid list *)
(* Should print "Reverse is: " *)
let reverse_list = [1; 2; 3; 4; 5];;
Printf.printf "Reverse is: [%s]\n" (
  String.concat "; " (List.map string_of_int (reverse reverse_list))
)
