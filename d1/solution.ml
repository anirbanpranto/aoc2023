(* utils *)
let value_of (c) : int option =
    match c with
    | '0' -> Some 0
    | '1' -> Some 1
    | '2' -> Some 2
    | '3' -> Some 3
    | '4' -> Some 4
    | '5' -> Some 5
    | '6' -> Some 6
    | '7' -> Some 7
    | '8' -> Some 8
    | '9' -> Some 9
    | _ -> None

let read_file filename = 
    let lines = ref [] in
    let chan = open_in filename in
    try
        while true; do
        lines := input_line chan :: !lines
    done; !lines
    with End_of_file ->
        close_in chan;
        List.rev !lines ;;

let file = "input.txt"

let string_to_char_list s = let x = String.to_seq s in List.of_seq x;;

(* solution *)
let l_s list =
    let f = List.nth list 0 in f;;
let l_e len list =
    let f = List.nth list (len-1) in f;;
let add str c = str ^ Char.escaped c

let concat len list = add(add "" (l_s list)) (l_e len list)

let digit_list line =
    let is_digit c = 
    (
        match value_of c with
        | Some x -> true
        | None -> false
    )
    in List.filter is_digit (string_to_char_list line);;

let length line = let x = digit_list line in List.length x;;
let num_line line = let len = length line in let list = digit_list line in int_of_string (concat len list);;

let rec calibrate l =
    match l with
    []-> 0
    |h::t-> num_line h + (calibrate t);;

let () =
    let sum = let list_of_string = read_file file in calibrate list_of_string in print_int sum;;