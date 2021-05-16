open Graphics
open Images
open Png

;;
Graphics.open_graph " 800x680"

let height = Graphics.size_y

let width = Graphics.size_x

let px_size = 16

let scaling = ref 2

let open_window =
  try
    while true do
      ()
    done
  with _ -> Graphics.clear_graph ()

(* most of this is very similar to pokemon, must change *)
let img_arr img_t =
  let graph_arr =
    match img_t with
    | Rgba32 bits ->
        let h = bits.Rgba32.height in
        let w = bits.Rgba32.width in
        Array.init h (fun heights ->
            Array.init w (fun widths ->
                let { color = { r; g; b }; alpha = tr } =
                  Rgba32.unsafe_get bits widths heights
                in
                rgb r g b))
    | _ -> failwith "never"
  in
  let list_img = Array.(map Array.to_list graph_arr |> to_list) in
  let rec nappend a b n = nappend (Array.append a [| b |]) b (n - 1) in
  List.fold_left
    (fun acc row ->
      nappend acc
        (List.fold_left
           (fun racc color -> nappend racc color 1)
           [||] row)
        1)
    [||] list_img

let upload_img folder file x y =
  let img = Png.load (folder ^ file ^ ".png") [] in
  let draw = img |> img_arr |> Graphics.make_image in
  Graphics.draw_image draw x y

;;
open_window;
upload_img "card" "Green"
