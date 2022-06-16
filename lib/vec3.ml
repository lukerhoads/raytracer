type vec3 = { x: float; y: float; z: float; }

let zero_vec = { x = 0.0; y = 0.0; z = 0.0; }

let ( + ) v1 v2 =
  { x = v1.x +. v2.x; y = v1.y +. v2.y; z = v1.z +. v2.z }

let ( * ) v t = 
  { x = v.x *. t; y = v.y *. t; z = v.z *. t }

let ( / ) v t =  v * ( 1.0 /. t )

let length_squared v =
  v.x *. v.x +. v.y *. v.y +. v.z *. v.z

let length v = 
  Float.sqrt (length_squared v)

let format chan v = 
  Printf.fprintf chan "%f %f %f" v.x v.y v.z

let dot v1 v2 = 
  v1.x *. v2.x +. v1.y *. v1.y +. v1.z *. v1.z

let cross v1 v2 =
  { x = v1.y *. v2.z -. v1.z *. v2.y; y = v1.z *. v2.x -. v1.x *. v2.z; z = v1.x *. v2.y -. v1.y *. v2.x }

let unit v = 
  v / (length v)
