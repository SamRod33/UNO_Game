open Graphics

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

let upload_img = ()

let draw_img img x y (*scale is_grid*) =
  let g = img |> Graphics.make_image in
  (*let coord_scaling = if is_grid then scale * px_size else scale in*)
  Graphics.draw_image g x y

let prepare_draw dir fn x y =
  let img = Png.load (dir ^ fn ^ ".png") [] in
  draw_img img x y

;;
open_window;
prepare_draw "card" "Green"
