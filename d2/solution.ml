open Printf
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

let get_game_id str =
  let id_str =
    let game_id = 
      let game_id_str = 
        let before_colon = 
         String.split_on_char(':') str in (List.nth before_colon 0) in String.split_on_char(' ') game_id_str in (List.nth game_id 1) in int_of_string id_str 

let get_iterations str =
  let after_colon =
    let split =
        String.split_on_char(':') str in (List.tl split) in String.split_on_char(';') (List.nth after_colon 0)

let is_this_ok color value =
  match String.trim color with
  | "blue" -> (value <= 14)
  | "green" -> (value <= 13)
  | "red" -> (value <= 12)
  | _ -> false

let is_valid str =
    let items = String.split_on_char(' ') str in (is_this_ok (List.nth items 2) (int_of_string (List.nth items 1)));;

let get_value line id =
  let valid_item (line_u : string) =
  let x = String.split_on_char(',') line_u in List.for_all is_valid x
  in match valid_item line with
  | false -> 0
  | true -> id

let rec sum_list list =
  match list with
    | [] -> 0
    | h::t -> List.nth h 0 + sum_list t

let solve lines = 
    let f_i = let items = List.map(fun line -> let iter = get_iterations line in List.map (fun x -> get_value x (get_game_id line)) iter) lines in
    List.filter (fun x -> not (List.exists (fun x -> x == 0) x)) items
    in sum_list f_i; 
;;

(* debug *)
let () = let sum = let lines = read_file file in solve lines in print_int sum;;
