let dump_coord (x,y) =
  Printf.printf "(%f,%f) " x y

let dump l =
  let _ = Printf.printf "{ " in
  let _ = List.iter dump_coord l in
  Printf.printf "}\n%!"

let read_coords =
  let rec aux acc n =
      if n = 0
      then List.rev acc
      else let c = Scanf.scanf "%f %f\n" (fun x y -> (x,y))
      in aux (c::acc) (n-1)
  in
  fun n -> aux [] n

  
let read () =
  Scanf.scanf "%d\n" read_coords