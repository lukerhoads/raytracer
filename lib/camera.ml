open Vec3

type t = { origin: Vec3.t; lower_left_corner: Vec3.t; horizontal: Vec3.t; vertical: Vec3.t; }

let aspect_ratio = 16.0 /. 9.0
let image_width = 400
let image_height = Float.to_int(Float.of_int(image_width) /. aspect_ratio)
let viewport_height = 2.0
let viewport_width = aspect_ratio *. viewport_height
let focal_length = 1.0 
let origin = Vec3.zero 
let horizontal = Vec3.create viewport_width 0. 0.
let vertical = Vec3.create 0. viewport_height 0.

let create = 
  { 
    origin; 
    horizontal;
    vertical;
    lower_left_corner = origin -| horizontal /| 2. -| vertical /| 2. -| (Vec3.create 0. 0. focal_length);
  }

let get_ray (cam: t) u v : Ray.t = 
  Ray.create cam.origin (cam.lower_left_corner +| u *| cam.horizontal +| v *| cam.vertical -| cam.origin)