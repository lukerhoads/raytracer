open Base 
open Vec3

type mat_type = Lambertian | Metal

type t = { albedo: Vec3.t; material: mat_type }

type hit_record = { t: float; p: Vec3.t; normal: Vec3.t; front_face: bool; mat_ptr: t }

let create albedo material = { albedo; material }

let random_unit_vector () =
  let open Float in
  let a = Random.float_range 0. (2. * pi)
  and z = Random.float_range (-1.) 1. in
  let r = sqrt(1. - z * z) in
  Vec3.create (r * cos(a)) (r * sin(a)) z

let scatter (lamb: t) (ray: Ray.t) (hit_rec: hit_record): (Ray.t * Vec3.t * bool) = 
  let open Float in
  match lamb.material with 
  | Lambertian -> 
    let scatter = hit_rec.normal +| random_unit_vector() in 
    if Vec3.near_zero scatter then (Ray.create hit_rec.p hit_rec.normal, lamb.albedo, true)
    else (Ray.create hit_rec.p scatter, lamb.albedo, true)
  | Metal -> 
    let reflected = Vec3.reflect ray.direction hit_rec.normal in 
    let scattered = Ray.create hit_rec.p reflected in 
    (scattered, lamb.albedo, ((Vec3.dot scattered.direction hit_rec.normal) > 0.))