open Graphics

;;
Graphics.open_graph " 800x680"

let height = Graphics.size_y

let width = Graphics.size_x

let open_window =
  try
    while true do
      ()
    done
  with _ -> Graphics.clear_graph ()

let upload_img = ()

;;
open_window
