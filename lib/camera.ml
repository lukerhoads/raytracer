open Base
open Vec3

type t = {
  lower_left_corner: Vec3.t;
  horizontal: Vec3.t;
  vertical: Vec3.t;
  origin: Vec3.t;
  w: Vec3.t;
  u: Vec3.t;
  v: Vec3.t;
  lens_radius: float;
}

let degrees_to_radians degrees = 
  let open Float in 
  degrees * Float.pi / 180.

let rec random_in_unit_disc () = 
  let open Float in 
  let p = Vec3.create (Random.float_range (-1.) 1.) (Random.float_range (-1.) 1.) 0. in 
  if Vec3.length_squared p >= 1. then 
    random_in_unit_disc ()
  else 
    p 

let create 
  ~lookfrom 
  ~lookat 
  ~vup 
  ~vfov 
  ~aspect_ratio 
  ~aperture 
  ~focus_dist = 
  let open Float in 
  let theta = degrees_to_radians vfov in
  let h = tan (theta / 2.) in 
  let viewport_height = 2. * h in
  let viewport_width = aspect_ratio * viewport_height in
  let w = Vec3.unit (lookfrom -| lookat) in 
  let u = Vec3.unit (Vec3.cross vup w) in 
  let v = Vec3.cross w u in
  let origin = lookfrom in 
  let horizontal = focus_dist * viewport_width *| u 
  and vertical = focus_dist * viewport_height *| v in
  let lower_left_corner = 
    origin -| 
    (horizontal /| 2.) -| 
    (vertical /| 2.) -| 
    (focus_dist *| w) in 
  let lens_radius = aperture / 2. in 
  { lower_left_corner; horizontal; vertical; origin; w; u; v; lens_radius }

let get_ray u v camera = 
  let rd = camera.lens_radius *| random_in_unit_disc() in 
  let offset = rd.x *| camera.u +| rd.y *| camera.v in 
  Ray.create 
    (camera.origin +| offset) 
    (camera.lower_left_corner +| 
    (u *| camera.horizontal) +| 
    (v *| camera.vertical) -| 
    camera.origin -| offset)