let read_file file = In_channel.with_open_bin file In_channel.input_lines;;
let read_lines file = List.filter ((<>) "") (read_file file);;

let lines_to_intlist lines =
    let split_lines = List.map (String.split_on_char ' ') lines in
    List.map (List.map (int_of_string)) split_lines;;

let list_diff list =
    let rec aux acc = function
        | [] | [ _ ] -> acc
        | x :: ((y :: t) as xs) -> aux ((y - x) :: acc) xs
    in
    aux [] list;;

let count elm l = List.length @@ List.filter (fun x -> x = elm) l;;

let is_same_sign x y = x * y > 0;;
let rec list_is_same_sign = function
    | [] | [ _ ] -> true
    | x :: ((y :: t) as xs) ->
        if is_same_sign x y then
            list_is_same_sign xs
        else
            false;;

let list_stability list =
    let diff = list_diff list in
    list_is_same_sign diff && not (List.mem 0 diff);;

let input_text = read_lines "inputs/test.txt";;
let input_data = lines_to_intlist input_text;;

let () =
    let results = List.map list_stability input_data in
    Printf.printf "%d\n" (count true results);;
