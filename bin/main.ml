open Base
open Stdio

open Raytracer
open Vec3

let sphere1 = Sphere.create (Vec3.create 0. 0. (-1.)) 0.5
let sphere2 = Sphere.create (Vec3.create 0. (-100.5) (-1.)) 100.
let world = Scene.create |> Scene.add sphere1 |> Scene.add sphere2

let random_unit_vector () =
  let open Float in
  let a = Random.float_range 0. (2. * pi)
  and z = Random.float_range (-1.) 1. in
  let r = sqrt(1. - z * z) in
  Vec3.create (r * cos(a)) (r * sin(a)) z

let ray_color (ray: Ray.t) =
  let max_depth = 50 in
  let rec helper r dep = 
    match dep with 
    | 0 -> Vec3.zero
    | _ -> 
      match Scene.hit r world with 
      | Some hit_record -> 
        let target = hit_record.p +| hit_record.normal +| random_unit_vector() in
        mult (helper (Ray.create hit_record.p (target -| hit_record.p)) (dep - 1)) 0.5
      | None ->
        let unit_direction = ray.direction in
        let t = 0.5 *. (unit_direction.y +. 1.0) in 
        lerp (Vec3.create 1. 1. 1.) (Vec3.create 0.5 0.7 1.0) t in 
  helper ray max_depth

let write_color file color samples_per_pixel = 
  let scale = 1. /. Float.of_int samples_per_pixel in 
  let r = color.x *. scale |> Float.sqrt
  and g = color.y *. scale |> Float.sqrt
  and b = color.z *. scale |> Float.sqrt in 
  let ir = 256. *. Float.clamp_exn r ~min:0. ~max:1. |> Float.to_int
  and ig = 256. *. Float.clamp_exn g ~min:0. ~max:1. |> Float.to_int
  and ib = 256. *. Float.clamp_exn b ~min:0. ~max:1. |> Float.to_int in
  Out_channel.fprintf file "%d %d %d\n" ir ig ib 
    
let () =
  let width = 200
  and height = 100
  and samples_per_pixel = 100 in
  let file = Out_channel.create "image.ppm" in
  let _ = Out_channel.fprintf file "P3\n%d %d\n255\n" width height in
  
  (* let _ = 
  (Sequence.cartesian_product 
    (Sequence.range ~stride:(-1) ~stop:`inclusive (height - 1) 0)
    (Sequence.range 0 width)
  )
  |> Sequence.iter 
    ~f:(fun (j, i) ->
      let color = (Sequence.range 0 samples_per_pixel) |> Sequence.fold ~init:Vec3.zero ~f:(fun acc _ -> 
        let u = (Float.of_int(i) +. Random.float(1.)) /. Float.of_int(width)
        and v = (Float.of_int(j) +. Random.float(1.)) /. Float.of_int(height) in
        let r = Camera.get_ray u v in
        acc +| ray_color r
      ) in write_color file color samples_per_pixel) in *)

  let _ = 
    (Sequence.range ~stride:(-1) ~stop:`inclusive (height - 1) 0) |> Sequence.iter ~f:(fun (j) ->
      (* printf "%d lines left\n" j; *)
      (Sequence.range 0 width) |> Sequence.iter ~f:(fun (i) ->
          let color = (Sequence.range 0 samples_per_pixel) |> Sequence.fold ~init:Vec3.zero ~f:(fun acc _ -> 
            let u = (Float.of_int(i) +. Random.float (1.)) /. Float.of_int(width)
            and v = (Float.of_int(j) +. Random.float (1.)) /. Float.of_int(height) in
            let r = Camera.get_ray u v in
            acc +| ray_color r
          ) in write_color file color samples_per_pixel)) in
    printf "Done\n"