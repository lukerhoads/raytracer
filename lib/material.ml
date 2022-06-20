open Base 
open Vec3

type t = 
  | Lambertian of { albedo: Vec3.t }
  | Metal of { albedo: Vec3.t; fuzz: float }
  | Dielectric of { ir: float }

type face_direction =
  | FrontFace 
  | BackFace

type hit_record = { t: float; p: Vec3.t; normal: Vec3.t; front_face: bool; material: t }

let random_unit_vector () =
  let open Float in
  let a = Random.float_range 0. (2. * pi)
  and z = Random.float_range (-1.) 1. in
  let r = sqrt(1. - z * z) in
  Vec3.create (r * cos(a)) (r * sin(a)) z

let reflectance cosine ref_idx = 
  let open Float in 
  let r0 = (1. - ref_idx) / (1. + ref_idx) in 
  let r0 = r0 * r0 in 
  r0 +. (1. - r0) * (int_pow (1. -. cosine) 5)

type scatter_result = { scattered: Ray.t; attenuation: Vec3.t }

let scatter (ray: Ray.t) (hit_rec: hit_record) = 
  let open Float in
  function 
  | Lambertian { albedo } -> 
    let scatter = hit_rec.normal +| random_unit_vector() in 
    let scattered = Ray.create hit_rec.p scatter in
    if Vec3.near_zero scatter then Some { scattered = Ray.create hit_rec.p hit_rec.normal; attenuation = albedo }
    else Some { scattered; attenuation = albedo }
  | Metal { albedo; fuzz } -> 
    let reflected = Vec3.reflect ray.direction hit_rec.normal in 
    let scattered = Ray.create hit_rec.p (reflected +| fuzz *| random_unit_vector()) in 
    Option.some_if ((Vec3.dot scattered.direction hit_rec.normal) > 0.) { scattered; attenuation = albedo }
  | Dielectric { ir } -> 
    let attenuation = Vec3.create 1. 1. 1. in
    let refraction_ratio = if hit_rec.front_face then (1. /. ir) else ir in 
    let unit_direction = Vec3.unit ray.direction in 
    let cos_theta = min (dot (Vec3.negate unit_direction) hit_rec.normal) 1. in 
    let sin_theta = sqrt (1. - cos_theta * cos_theta) in 
    let cannot_refract = refraction_ratio * sin_theta > 1. || reflectance cos_theta refraction_ratio > Random.float 1. in 
    let direction = if cannot_refract 
      then Vec3.reflect unit_direction hit_rec.normal 
      else Vec3.refract unit_direction hit_rec.normal refraction_ratio in 
    let scattered = Ray.create hit_rec.p direction in 
    Some { scattered; attenuation }