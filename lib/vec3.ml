type t = { x: float; y: float; z: float; }

let create x y z = { x; y; z; }


let zero = create 0. 0. 0.

let (+|) v1 v2 =
  { x = v1.x +. v2.x; y = v1.y +. v2.y; z = v1.z +. v2.z }

let (-|) v1 v2 =
  { x = v1.x -. v2.x; y = v1.y -. v2.y; z = v1.z -. v2.z }

let ( *| ) t v = 
  { x = v.x *. t; y = v.y *. t; z = v.z *. t }

let (/|) v t =  
  let t_inv = 1. /. t in 
  t_inv *| v

let dot v1 v2 = 
  v1.x *. v2.x +. v1.y *. v2.y +. v1.z *. v2.z

let length_squared v =
  dot v v

let random_unit_cube = { x = Random.float 1.; y = Random.float 1.; z = Random.float 1. }

let in_unit_sphere = 
  let random = random_unit_cube in 
  let rec loop random = 
    if length_squared random <= 1. then random
    else loop random_unit_cube in 
  loop random

let length v = 
  Float.sqrt (length_squared v)

let cross u v =
  {
    x = u.y *. v.z -. u.z *. v.y;
    y = u.z *. v.x -. u.x *. v.z;
    z = u.x *. v.y -. u.y *. v.x;
  }

let unit v = 
  v /| (length v)

let random_unit_vector = 
  unit in_unit_sphere

let lerp v1 v2 t = 
  ((1. -. t) *| v1) +| (t *| v2)

let format chan v = 
  Printf.fprintf chan "%f %f %f" v.x v.y v.z

let random_in_hemisphere normal =
  let unit_sphere = in_unit_sphere in 
  if dot unit_sphere normal > 0. then unit_sphere
  else (-1.) *| unit_sphere