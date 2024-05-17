let string_list_pp = [%show: string list];;

let string_of_string_list: string list -> string = Format.asprintf "@[%a@]" string_list_pp;;
(* let string_of_string_list: string list -> string = Format.asprintf "@[%a@]" [%show: string list];; *)

let v: string list = String.split_on_char ' ' "Hello using an opam library";;
