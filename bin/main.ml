open Base
open Stdio

open Raytracer
open Vec3

(* let material_ground = Material.Lambertian { albedo = Vec3.create 0.8 0.8 0. } 
let material_center = Material.Lambertian { albedo = Vec3.create 0.1 0.2 0.5 }

let material_left = Material.Dielectric { ir = 1.5 }
let material_right = Material.Metal { albedo = Vec3.create 0.8 0.6 0.2; fuzz = 0. }
let ground_sphere = Sphere.create (Vec3.create 0. (-100.5) (-1.)) 100. material_ground 
let sphere1 = Sphere.create (Vec3.create 0. 0. (-1.)) 0.5 material_center

let sphere2 = Sphere.create (Vec3.create (-1.) 0. (-1.)) 0.5 material_left
let sphere3 = Sphere.create (Vec3.create (-1.) 0. (-1.)) (-0.4) material_left
let sphere4 = Sphere.create (Vec3.create 1. 0. (-1.)) 0.5 material_right *)

let ground_sphere = Sphere.create (Vec3.create 0. (-100.5) (-1.)) 100. (Material.Lambertian { albedo = Vec3.create 0.8 0.8 0. })
let sphere1 = Sphere.create (Vec3.create 0. 0. (-1.)) 0.5
  (Material.Lambertian { albedo = (Vec3.create 0.1 0.2 0.5) })
let sphere2 = Sphere.create (Vec3.create (-1.) 0. (-1.)) 0.5
  (Material.Dielectric { ir = 1.5 })
let sphere3 = Sphere.create (Vec3.create (-1.) 0. (-1.)) (-0.4)
  (Material.Dielectric { ir = 1.5 })
and sphere4 = Sphere.create (Vec3.create 1. 0. (-1.)) 0.5
  (Material.Metal { albedo = (Vec3.create 0.8 0.6 0.2); fuzz = 0. })
let world = 
  let open Scene in 
  create 
  |> add ground_sphere |> add sphere1 |> add sphere2 |> add sphere3 |> add sphere4

let ray_color (ray: Ray.t) =
  let max_depth = 50 in
  let rec helper r dep = 
    match dep with 
    | 0 -> Vec3.zero
    | _ -> 
      match Scene.hit r world with 
      | Some hit_record -> 
        begin match Material.scatter ray hit_record hit_record.material with
          | Some { scattered; attenuation } -> 
            Vec3.mult_vec (helper scattered (dep - 1)) attenuation 
          | None -> Vec3.zero
        end
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
  let width = 400
  and height = 200
  and samples_per_pixel = 100 in
  let file = Out_channel.create "image.ppm" in
  let _ = Out_channel.fprintf file "P3\n%d %d\n255\n" width height in
  
  let _ = 
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
      ) in write_color file color samples_per_pixel) in

  (* let _ = 
    (Sequence.range ~stride:(-1) ~stop:`inclusive (height - 1) 0) |> Sequence.iter ~f:(fun (j) ->
      (* printf "%d lines left\n" j; *)
      (Sequence.range 0 width) |> Sequence.iter ~f:(fun (i) ->
          let color = (Sequence.range 0 samples_per_pixel) |> Sequence.fold ~init:Vec3.zero ~f:(fun acc _ -> 
            let u = (Float.of_int(i) +. Random.float (1.)) /. Float.of_int(width)
            and v = (Float.of_int(j) +. Random.float (1.)) /. Float.of_int(height) in
            let r = Camera.get_ray u v in
            acc +| ray_color r
          ) in write_color file color samples_per_pixel)) in *)
    printf "Done\n"