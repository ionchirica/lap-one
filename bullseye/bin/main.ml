open Bullseye
open Types

let () =
  if Array.length Sys.argv < 3 then
    (Format.printf "Missing arguments: dune exec bullseye <task1|task2> <score>\n";
    exit 1;)
  else 
  (* Read arguments from the command line *)
  let arg1 = Sys.argv.(1) in
  let arg2 = Sys.argv.(2) in

  (* Convert arg2 to an integer *)
  let target =
    try int_of_string arg2
    with Failure _ ->
      print_endline "Error: arg2 must be an integer.";
      exit 1
  in

  (* Decide which checkout function to use based on arg1 *)
  let call_checkouts =
    if arg1 = "task2" then Task2.compute_checkouts target
    else Task1.compute_checkouts target
  in

  (* Print the number of sequences *)
  (* Printf.printf "%d\n" (List.length call_checkouts); *)

  (* Print the sequences *)
  List.iter
    (fun seq ->
      let string_seq =
        List.map
          (function
            | S x -> "S" ^ string_of_int x
            | D x -> "D" ^ string_of_int x
            | T x -> "T" ^ string_of_int x)
          seq
      in
      print_endline (String.concat " " string_seq))
    call_checkouts
