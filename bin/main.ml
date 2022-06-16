open Raytracer

let image_width = 256
let image_height = 256

let () =
  Printf.printf "P3\n%d %d\n255\n" image_width image_height;

  for i = 0 to image_height - 1 do
    Printf.eprintf "Number of lines left: %d\n" (image_height - i);

    for j = 0 to image_width - 1 do 
      Util.write_color stdout { x = float i /. (float image_width -. 1.); y = float j /. (float image_height -. 1.); z = 0.25; }
    done
  done
