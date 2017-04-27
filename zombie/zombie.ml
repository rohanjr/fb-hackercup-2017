open Core.Std

let max_rolls = 20
let die_faces = [| 4; 6; 8; 10; 12; 20 |]

let get_probs () : float array array array * float array array array =
  let num_die = Array.length die_faces in
  let prob_eq, prob_ge =
    Array.make_matrix ~dimx:num_die ~dimy:(max_rolls + 1) (Array.create ~len:0 0.0),
    Array.make_matrix ~dimx:num_die ~dimy:(max_rolls + 1) (Array.create ~len:0 0.0) in
  for die = 0 to num_die - 1 do
    let num_faces = die_faces.(die) in
    for num_rolls = 1 to max_rolls do
      let min_damage = num_rolls in
      let max_damage = num_rolls * num_faces in
      (* Reallocate arrays for individual and cumulative damage probabilies *)
      prob_eq.(die).(num_rolls) <- Array.create ~len:(max_damage + 1) 0.0;
      prob_ge.(die).(num_rolls) <- Array.create ~len:(max_damage + 1) 0.0;
      let prob_less = ref 0.0 in
      for d = min_damage to max_damage do
        let prob =
          if num_rolls = 1 then
            1 // num_faces
          else
            let min_damage_per_roll = Int.max 1 (d - num_faces * (num_rolls - 1)) in
            let max_damage_per_roll = Int.min num_faces (d - (num_rolls - 1)) in
            let p = ref 0.0 in
            for k = min_damage_per_roll to max_damage_per_roll do
              p := !p +. prob_eq.(die).(1).(k) *. prob_eq.(die).(num_rolls - 1).(d - k)
            done;
            !p
          in
        prob_eq.(die).(num_rolls).(d) <- prob;
        prob_ge.(die).(num_rolls).(d) <- 1.0 -. !prob_less;
        prob_less := !prob_less +. prob
      done
    done
  done;
  (prob_eq, prob_ge)

let parse_spell (spell : string) : int * int * int =
  let s1, rest = String.lsplit2_exn spell ~on:'d' in
  let rolls = Int.of_string s1 in
  match String.lsplit2 rest ~on:'+' with
  | Some (s2, s3) -> (rolls, Int.of_string s2, Int.of_string s3)
  | None -> begin match String.lsplit2 rest ~on:'-' with
      | Some (s2, s3) -> (rolls, Int.of_string s2, -(Int.of_string s3))
      | None -> (rolls, Int.of_string rest, 0) end

let kill_prob (prob_ge : float array array array) (health : int) (spell : string) : float =
  let (rolls, faces, offset) = parse_spell spell in
  let goal = health - offset in
  if goal <= rolls then
    1.0
  else if goal > rolls * faces then
    0.0
  else
    let die, _ = Array.findi_exn die_faces ~f:(fun _ -> (=) faces) in
    prob_ge.(die).(rolls).(goal)

let () =
  let prob_ge = snd (get_probs ()) in
  let t = read_int () in
  for i = 1 to t do
    let health = read_line () |> String.split ~on:' ' |> List.hd_exn |> Int.of_string in
    let spells = read_line () |> String.split ~on:' ' in
    let kill_probs = List.map spells ~f:(kill_prob prob_ge health) in
    let max_prob = List.fold kill_probs ~init:0.0 ~f:Float.max in
    Printf.printf "Case #%d: %f\n" i max_prob
  done
