open Types

let singles = S 25 :: List.init 20 (fun i -> S (i + 1))
let doubles = D 25 :: List.init 20 (fun i -> D (i + 1))
let triples = List.init 20 (fun i -> T (i + 1))

(*
  Primeira implementação.

  Neste caso estamos a ignorar que as combinações:
    S1 T1 D1 e
    T1 S1 D1 são a mesma coisa.

  optimizações possíveis:
    filtrar os pontos que irão dar overflow nos pontos.
    ignorar as falhas para evitar o sort_uniq no fim
 *)

let unfold = function S x -> x | D x -> x * 2 | T x -> x * 3

let checkout points =
  let rec checkout_aux point round history =
    if round = 0 then if point = points then [ history ] else []
    else
      List.flatten
        (List.map
           (fun x -> checkout_aux (point + unfold x) (round - 1) (x :: history))
           singles
        @ List.map
            (fun x ->
              checkout_aux (point + unfold x) (round - 1) (x :: history))
            triples
        @ List.map
            (fun x ->
              checkout_aux (point + unfold x) (round - 1) (x :: history))
            doubles
        @ [ checkout_aux point (round - 1) history ])
  in
  let res =
    List.flatten
      (List.map
         (fun double_point ->
           checkout_aux (unfold double_point) 2 [ double_point ])
         doubles)
  in
  List.sort_uniq compare res

(*
  Segunda implementação.

  Neste caso o resultado é ligeiramente diferente, consideramos portanto as permutações:
    S1 T1 D1 e
    T1 S1 D1 como a mesma coisa.
 *)
let same_elements_except_last l1 l2 =
  let l1_without_last = List.tl (List.rev l1) in
  let l2_without_last = List.tl (List.rev l2) in
  List.sort compare l1_without_last = List.sort compare l2_without_last

let remove_permutations lists =
  let rec aux removed remaining =
    match remaining with
    | [] -> List.rev removed
    | head :: tail ->
        let is_permutation =
          List.exists (fun x -> same_elements_except_last x head) removed
        in
        if is_permutation then aux removed tail else aux (head :: removed) tail
  in
  aux [] lists

(* Converts throws (S, D, T) to their string representation *)
let string_of_throw = function
  | Types.S n -> Printf.sprintf "S%d" n
  | Types.D n -> Printf.sprintf "D%d" n
  | Types.T n -> Printf.sprintf "T%d" n

let sort_checkouts checkouts =
  List.sort
    (fun a b ->
      let len_cmp = compare (List.length a) (List.length b) in
      if len_cmp <> 0 then len_cmp
      else compare (List.map string_of_throw a) (List.map string_of_throw b))
    checkouts

let compute_checkouts (target : int) : checkouts =
  let res = checkout target in
  let res = remove_permutations res in
  Printf.printf "%d\n" (List.length res);
  res
