open Core.Std

let reqd_weight = 50

(* Receives array in decreasing order *)
let rec num_trips' (a : int array) (lo : int) (items_left : int) : int =
  if items_left <= 0 then 0 else
    let lub_weight = Int.round_up reqd_weight ~to_multiple_of:a.(lo) in
    let num_items = lub_weight / a.(lo) in
    if num_items > items_left then
      0 (* not enough for another trip *)
    else
      1 + num_trips' a (lo + 1) (items_left - num_items)

let num_trips (a : int array) : int =
  Array.sort a ~cmp:Int.descending;
  num_trips' a 0 (Array.length a)

let () =
  let t = read_int () in
  for day = 1 to t do
    let n = read_int () in
    let a = Array.create ~len:n 0 in
    for i = 0 to n - 1 do
      a.(i) <- read_int ()
    done;
    Printf.printf "Case #%d: %d\n" day (num_trips a)
  done
