type t = { x: float; y: float; z: float; }

let create x y z = { x; y; z; }

let zero = create 0. 0. 0.

let (+|) v1 v2 =
  { x = v1.x +. v2.x; y = v1.y +. v2.y; z = v1.z +. v2.z }

let (-|) v1 v2 =
  { x = v1.x -. v2.x; y = v1.y -. v2.y; z = v1.z -. v2.z }

let ( *| ) t v = 
  { x = v.x *. t; y = v.y *. t; z = v.z *. t }

let mult_vec v1 v2 =
  { x = v1.x *. v2.x; y = v1.y *. v2.y; z = v1.z *. v2.z }

let mult v1 s = {x= v1.x *. s; y= v1.y *. s; z= v1.z *. s}

let (/|) v t =  
  let t_inv = 1. /. t in 
  t_inv *| v

let dot v1 v2 = 
  v1.x *. v2.x +. v1.y *. v2.y +. v1.z *. v2.z

let length_squared v =
  dot v v

let cross u v =
  {
    x = u.y *. v.z -. u.z *. v.y;
    y = u.z *. v.x -. u.x *. v.z;
    z = u.x *. v.y -. u.y *. v.x;
  }

let lerp v1 v2 t = 
  ((1. -. t) *| v1) +| (t *| v2)

let format chan v = 
  Printf.fprintf chan "%f %f %f" v.x v.y v.z

let length v = 
  Float.sqrt (length_squared v)

let unit v = 
  v /| (length v)

let near_zero v =
  let open Float in 
  let s = 1e-8 in 
  (abs v.x < s) && (abs v.y < s) && (abs v.z < s)

let reflect v n = 
  v -| (2. *| ((dot v n) *| n))