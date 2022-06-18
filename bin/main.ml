open Base
open Stdio

open Raytracer
open Raytracer.Vec3
open Raytracer.Ray

let aspect_ratio = 16.0 /. 9.0
let image_width = 400
let image_height = Float.to_int(Float.of_int(image_width) /. aspect_ratio)
let samples_per_pixel = 100
let max_depth = 50

let sphere1 = Sphere.create (Vec3.create 0. 0. (-1.)) 0.5
let sphere2 = Sphere.create (Vec3.create 0. (-100.5) (-1.)) 100.
let world = Scene.create |> Scene.add sphere1 |> Scene.add sphere2

let camera = Camera.create

let rec ray_color ray depth =
  if depth > 0 then
  match Scene.hit ray world with 
  | Some hit_record -> 
    let target = hit_record.p +| Vec3.random_in_hemisphere hit_record.normal in
    (ray_color (Ray.create hit_record.p (target -| hit_record.p)) (depth - 1)) /| 2. 
  | None ->
    let unit_direction = ray.direction in
    let t = 0.5 *. (unit_direction.y +. 1.0) in 
    lerp (Vec3.create 1. 1. 1.) (Vec3.create 0.5 0.7 1.0) t    
  else Vec3.zero

let write_color file color samples_per_pixel = 
  let scale = 1. /. Float.of_int samples_per_pixel in 
  let r = color.x *. scale
  and g = color.y *. scale 
  and b = color.z *. scale in
  let ir = 256. *. Float.clamp_exn r ~min:0. ~max:1.0
  and ig = 256. *. Float.clamp_exn g ~min:0. ~max:1.0
  and ib = 256. *. Float.clamp_exn b ~min:0. ~max:1.0 in
  Out_channel.fprintf file "%d %d %d\n" (Float.to_int(ir)) (Float.to_int(ig)) (Float.to_int(ib)) 
    
let () =
  let file = Out_channel.create "image.ppm" in
  let _ = Out_channel.fprintf file "P3\n%d %d\n255\n" image_width image_height in

  let _ = 
  (Sequence.cartesian_product 
    (Sequence.range ~stride:(-1) ~stop:`inclusive (image_height - 1) 0)
    (Sequence.range 0 image_width)
  )
  |> Sequence.iter 
    ~f:(fun (j, i) ->
      let color = (Sequence.range 0 samples_per_pixel) |> Sequence.fold ~init:Vec3.zero ~f:(fun acc _ -> 
        let u = (Float.of_int(i) +. Random.float (1.)) /. Float.of_int(image_width)
        and v = (Float.of_int(j) +. Random.float (1.)) /. Float.of_int(image_height) in
        let r = Camera.get_ray camera u v in
        acc +| ray_color r max_depth
      ) in
      write_color file color samples_per_pixel) in

  printf "Done\n"