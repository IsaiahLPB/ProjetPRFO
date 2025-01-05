(* ----------------- IMPLEMENTATION DE GRAPHICS ----------------- *)

module IntSet = Set.Make(struct type t = int let compare = (-) end)


let draw_edge ((x1,y1),(x2,y2)) =
    let _ = Graphics.moveto x1 y1 in
    Graphics.lineto x2 y2

(*
	The function prints the regular points in black and the relay points in blue
*)
let draw_pb pts relay =
    let c = Graphics.foreground in
    let _ = Graphics.set_color Graphics.black in
    let _ = List.iter (fun (x,y) -> Graphics.fill_circle x y 5) pts in
    let _ = Graphics.set_color Graphics.blue in
    let _ = List.iter (fun (x,y) -> Graphics.fill_circle x y 5) relay in
    Graphics.set_color c

let draw_sol sol =
    let c = Graphics.foreground in
    let _ = Graphics.set_color Graphics.red in
    let _ = Graphics.set_line_width 3 in
    let _ = List.iter draw_edge sol in
    let _ = Graphics.set_line_width 1 in
    Graphics.set_color c

let draw (sx,sy) pts relay sol =
    let args = Printf.sprintf " %dx%d" sx sy in
    let _ = Graphics.open_graph args in
    let _ = draw_pb pts relay in
    let _ = draw_sol sol in
    let _ = Graphics.wait_next_event [Graphics.Key_pressed] in
    Graphics.close_graph ()

(*
	Modified the function to add a relay argument representing the relay points separately from the regular points
*)
let draw_steiner (sx,sy) pts relay sol =
    let ps0 = pts in
    let ps  = List.fold_left (fun acc (c1,c2) -> c1::c2::acc) ps0 sol in
    let xm  = List.fold_left (fun acc (x,_) -> min acc x) infinity ps in
    let xM  = List.fold_left (fun acc (x,_) -> max acc x) neg_infinity ps in
    let ym  = List.fold_left (fun acc (_,y) -> min acc y) infinity ps in
    let yM  = List.fold_left (fun acc (_,y) -> max acc y) neg_infinity ps in
    let margin  = 10.0 in
    let sxf = float_of_int sx in
    let syf = float_of_int sy in
    let _ = if sxf <= 2.0 *. margin
            then failwith "[draw_steiner] width must be more than 20px."
            else ()
    in
    let _ = if syf <= 2.0 *. margin
            then failwith "[draw_steiner] height must be more than 20px."
            else ()
    in
    let eps     = 1e-6 in
    let zoom_x  = max eps ((sxf -. 2.0 *. margin) /. (max eps (xM -. xm))) in
    let zoom_y  = max eps ((syf -. 2.0 *. margin) /. (max eps (yM -. ym))) in
    let adjust (x,y) =
        ( int_of_float ( margin +. zoom_x *. (x-.xm))
        , int_of_float ( margin +. zoom_y *. (y-.ym))
        )
    in
    let pts' = List.map adjust ps0 in
    let relay = List.map adjust relay in
    let sol' = List.map (fun (c1,c2) -> (adjust c1, adjust c2)) sol in
    draw (sx,sy) pts' relay sol'