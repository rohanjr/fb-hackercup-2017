open Core.Std

let square (x : int) : int = Int.pow x 2

let in_circle (a : int) (b : int) (r : int) (x : int) (y : int) : bool =
  square (x - a) + square (y - b) <= square r

let pi : float = 4.0 *. atan 1.0

let angle_from (a : int) (b : int) (x : int) (y : int) : float option =
  let dx, dy = x - a, y - b in
  if dx = 0 then
    if dy = 0 then
      None
    else if dy > 0 then
      Some 0.0
    else
      Some pi
  else
    let base_angle = atan2 (Float.of_int (Int.abs dy)) (Float.of_int (Int.abs dx)) in
    Some (match dx > 0, dy >= 0 with
    | true, true -> pi /. 2.0 -. base_angle
    | true, false -> pi /. 2.0 +. base_angle
    | false, true -> 1.5 *. pi +. base_angle
    | false, false -> 1.5 *. pi -. base_angle)

let in_pie (p : int) (x : int) (y : int) : bool =
  let a, b, r = 50, 50, 50 in
  let pie_angle = (Float.of_int p /. 100.0) *. 2.0 *. pi in
  p > 0 && in_circle a b r x y && (match angle_from a b x y with
    | None -> true
    | Some angle -> angle <= pie_angle)

let () =
  let n = read_int () in
  for i = 1 to n do
    read_line ()
      |> String.split ~on:' '
      |> List.map ~f:Int.of_string
      |> begin function
         | [p; x; y] ->
             let colour = if in_pie p x y then "black" else "white" in
             Printf.printf "Case #%d: %s\n" i colour
         | _ -> prerr_endline "Invalid input"; exit 1
         end
  done
