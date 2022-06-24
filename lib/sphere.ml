open Base
open Vec3

type t = { center: Vec3.t; radius: float; material: Material.t }

let create center radius material = { center; radius; material }

let hit (r: Ray.t) ({ center; radius; material }: t) : Material.hit_record option =
  let open Float in
  let t_min = 0.00001 in
  let oc = r.origin -| center in 
  let a = Vec3.length_squared r.direction
  and half_b = Vec3.dot oc r.direction
  and c = (Vec3.length_squared oc) -. (radius *. radius) in

  let hit_record_from_t t front_face: Material.hit_record option =  
    if (t > t_min) then 
      let p = Ray.at r t in 
      let outward_normal = (p -| center) /| radius in 
      let normal = if front_face then outward_normal else (negate outward_normal) in 
      Some { t; p; normal; front_face; material }
    else 
      None
    in

  let discriminant = (half_b *. half_b) -. (a *. c) in 
  if discriminant > 0. then 
    let root = Float.sqrt discriminant in
    let t1 = (-.half_b -. root) /. a in 
    match hit_record_from_t t1 true with
    | Some hit_record -> Some hit_record
    | None -> 
      let t2 = (-.half_b +. root) /. a in 
      hit_record_from_t t2 false
  else 
    None
    

    