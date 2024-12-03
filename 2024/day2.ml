let read_file file = In_channel.with_open_bin file In_channel.input_lines;;
let read_lines file = List.filter ((<>) "") (read_file file);;

let lines_to_intlist lines =
    let split_lines = List.map (String.split_on_char ' ') lines in
    List.map (List.map (int_of_string)) split_lines;;

let list_diff list =
    let rec aux acc = function
        | [] | [ _ ] -> List.rev acc
        | x :: ((y :: t) as xs) -> aux ((y - x) :: acc) xs
    in
    aux [] list;;

let count elm l = List.length @@ List.filter (fun x -> x = elm) l;;

let is_same_sign x y = x * y > 0;;

let list_is_stable list =
    let diff = list_diff list in
    let rec aux = function
    | [] -> true
    | [ x ] -> x <> 0 && (abs x) <= 3
    | x :: ((y :: t) as xs) ->
        (is_same_sign x y && (abs x) <= 3 && x <> 0) && aux xs
    in
    aux diff;;

let list_stability_list list =
    let diff = list_diff list in
    let aux sign prev acc = function
        | [] -> true
        | [x] -> 

let input_text = read_lines "inputs/day2.txt";;
let input_data = lines_to_intlist input_text;;

let () =
    let results = List.map list_is_stable input_data in
    Printf.printf "%d\n" (count true results);;
