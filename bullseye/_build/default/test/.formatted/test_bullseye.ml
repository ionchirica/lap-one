open OUnit2
open Bullseye
open Sys

(* Converts throws (S, D, T) to their string representation *)
let string_of_throw = function
  | Types.S n -> Printf.sprintf "S%d" n
  | Types.D n -> Printf.sprintf "D%d" n
  | Types.T n -> Printf.sprintf "T%d" n

(* Reads the expected result from a given file *)
let get_expected_result filename =
  try
    let ic = open_in filename in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    let result = String.split_on_char '\n' content in
    result
  with
  | Sys_error _ -> []
  | e ->
      print_endline ("Error reading file: " ^ filename);
      raise e

(* Sort a sequence by the length of each checkout and then by the elements within *)
let sort_checkouts checkouts =
  List.sort
    (fun a b ->
      let len_cmp = compare (List.length a) (List.length b) in
      if len_cmp <> 0 then len_cmp
      else compare (List.map string_of_throw a) (List.map string_of_throw b))
    checkouts

(* Loops through the test cases and runs the tests *)
let run_task_tests task =
  let i = 1 in
  let j = 171 in
  let tests = ref [] in

  for x = i to j do
    let filenames =
      match task with
      | "task1" ->
          [ Printf.sprintf "../../../test/expected/task1_%d.expected" x ]
      | "task2" ->
          [ Printf.sprintf "../../../test/expected/task2_%d.expected" x ]
      | _ -> []
    in

    List.iter
      (fun filename ->
        if file_exists filename then
          let expected_result = get_expected_result filename in
          let test_case =
            task >:: fun _ ->
            let result =
              match task with
              | "task1" -> Task1.compute_checkouts x
              | "task2" -> Task2.compute_checkouts x
              | _ -> []
            in

            (* check if same size *)
            if
              List.length result = 0
              && (List.length expected_result = 0
                 || (List.length expected_result = 1 && expected_result = [ "" ])
                 )
            then assert true
            else if List.length result <> List.length expected_result then
              assert false
            else
              (* sort the obtained result *)
              let flattened_result =
                List.map
                  (fun seq -> List.map string_of_throw seq |> String.concat " ")
                  (sort_checkouts result)
              in
              (* compare obtained with expected results *)
              if String.equal task "task1" then
                assert_equal expected_result flattened_result
          in
          tests := test_case :: !tests)
      filenames
  done;

  Printf.printf "Number of tests: %d\n" (List.length !tests);
  run_test_tt_main ("test suite for " ^ task >::: !tests)

let () =
  let task = ref "both" in
  let spec =
    [
      ( "-task",
        Arg.Set_string task,
        "Specify which task to run (task1, task2, or both)" );
    ]
  in
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [-task task1|task2|both]" in
  Arg.parse spec (fun _ -> ()) usage;

  (* Print argument details *)
  Printf.printf "Task argument: %s\n" !task;

  (* Run the tasks based on the argument *)
  match !task with
  | "task1" -> run_task_tests "task1"
  | "task2" -> run_task_tests "task2"
  | "both" ->
      run_task_tests "task1";
      run_task_tests "task2"
  | _ -> Printf.printf "Invalid argument! Use 'task1', 'task2', or 'both'.\n"
