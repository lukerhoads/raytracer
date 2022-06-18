open Vec3

let t_min = 0. 
and t_max = max_float

type hit_record = { t: float; p: Vec3.t; normal: Vec3.t; front_face: bool; }

let get_face_normal (ray: Ray.t) (normal: Vec3.t) : bool =
  (Vec3.dot ray.direction normal) < 0.

type t = { center: Vec3.t; radius: float }

let create center radius = { center; radius; }

let hit (r: Ray.t) ({ center; radius }: t) : hit_record option =
  let oc = r.origin -| center in 
  let a = Vec3.length_squared r.direction
  and half_b = Vec3.dot oc r.direction
  and c = (Vec3.length_squared oc) -. (radius *. radius) in

  let hit_record_from_t t: hit_record option =  
    if (t < t_max && t > t_min) then 
      let p = Ray.at r t in 
      let outward_normal = (p -| center) /| radius in 
      match get_face_normal r outward_normal with 
      | true -> Some { t; p; normal = ((p -| center) /| radius); front_face = true; }
      | false -> Some { t; p; normal = ((-1.) *| ((p -| center) /| radius)); front_face = false; }
    else 
      None
    in

  let discriminant = (half_b *. half_b) -. (a *. c) in 
  if discriminant > 0. then 
    let root = Float.sqrt discriminant in
    let t1 = (-.half_b -. root) /. a in 
    match hit_record_from_t t1 with
    | Some hit_record -> Some hit_record
    | None -> 
      let t2 = (-.half_b +. root) /. a in 
      hit_record_from_t t2
  else 
    None