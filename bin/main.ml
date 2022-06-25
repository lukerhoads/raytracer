open Base
open Stdio

open Raytracer
open Vec3

let random_vec () = 
  Vec3.create (Random.float 1.) (Random.float 1.) (Random.float 1.)

let world = 
  let open Float in 
  let open Scene in
  let sphere1 = Sphere.create (Vec3.create 0. (-1000.) 0.) 1000.
    (Material.Lambertian { albedo = (Vec3.create 0.5 0.5 0.5) })
  and sphere2 = Sphere.create (Vec3.create 0. 1. 0.) 1.
    (Material.Dielectric { ir = 1.5 })
  and sphere3 = Sphere.create (Vec3.create (-4.) 1. 0.) 1.0
    (Material.Lambertian { albedo = (Vec3.create 0.4 0.2 0.1) })
  and sphere4 = Sphere.create (Vec3.create 4. 1. 0.) 1.0
    (Material.Metal { albedo = (Vec3.create 0.7 0.6 0.5); fuzz = 0.0 }) in
  (Sequence.cartesian_product
    (Sequence.range (-11) 11)
    (Sequence.range (-11) 11)
  ) |> Sequence.fold  
    ~init:(create 
      |> add sphere1 
      |> add sphere2 
      |> add sphere3 
      |> add sphere4
    )
    ~f:(fun acc (b, a) ->
      let choose_mat = Random.float 1. in 
      let center = Vec3.create (Int.to_float(a) +. 0.9 *. (Random.float 1.)) 0.2 (Int.to_float(b) +. 0.9 *. (Random.float 1.)) in 
      if ((Vec3.length (center -| (Vec3.create 4. 0.2 0.))) > 0.9) then 
        if choose_mat < 0.8 then 
          let albedo = Vec3.mult_vec (random_vec()) (random_vec()) in 
          acc |> add (Sphere.create center 0.2 (Material.Lambertian { albedo }))
        else if choose_mat < 0.95 then 
          let albedo = Vec3.create (Random.float_range 0.5 1.) (Random.float_range 0.5 1.) (Random.float_range 0.5 1.) in 
          let fuzz = Random.float_range 0. 0.5 in 
          acc |> add (Sphere.create center 0.2 (Material.Metal { albedo; fuzz }))
        else 
          acc |> add (Sphere.create center 0.2 (Material.Dielectric { ir = 1.5 }))
      else 
        acc
    )

let ray_color (r: Ray.t) =
  let max_depth = 50 in
  let rec helper r dep = 
    match Scene.hit r world with 
    | Some hit_record -> 
      if dep <= 0 then 
        Vec3.zero 
      else
        begin match Material.scatter r hit_record hit_record.material with
        | Some { scattered; attenuation } -> 
          Vec3.mult_vec (helper scattered (dep - 1)) attenuation 
        | None -> Vec3.zero
        end
    | None ->
      let unit_direction = Vec3.unit r.direction in
      let t = 0.5 *. (unit_direction.y +. 1.0) in 
      Vec3.lerp (Vec3.create 1. 1. 1.) (Vec3.create 0.5 0.7 1.0) t in
  helper r max_depth

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
  printf "Hello";
  let aspect_ratio = 16. /. 9. in 
  let width = 1920 in 
  let height = Float.to_int (Float.of_int width /. aspect_ratio) in
  let samples_per_pixel = 100
  and lookfrom = Vec3.create 13. 2. 3. 
  and lookat = Vec3.create 0. 0. 0. in
  let camera = Camera.create
    ~lookfrom:lookfrom 
    ~lookat:lookat 
    ~vup:(Vec3.create 0. 1. 0.)
    ~vfov:20.
    ~aspect_ratio:aspect_ratio
    ~aperture:0.1
    ~focus_dist:10.
  in
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
        let r = camera |> Camera.get_ray u v in
        ray_color r +| acc 
      ) in write_color file color samples_per_pixel) in

  (* let _ = 
    (Sequence.range ~stride:(-1) ~stop:`inclusive (height - 1) 0) |> Sequence.iter ~f:(fun (j) ->
      printf "%d lines left\n" j;
      (Sequence.range 0 width) |> Sequence.iter ~f:(fun (i) ->
          let color = (Sequence.range 0 samples_per_pixel) |> Sequence.fold ~init:Vec3.zero ~f:(fun acc _ -> 
            let u = (Float.of_int(i) +. Random.float (1.)) /. Float.of_int(width)
            and v = (Float.of_int(j) +. Random.float (1.)) /. Float.of_int(height) in
            let r = camera |> Camera.get_ray u v in
            acc +| ray_color r
          ) in write_color file color samples_per_pixel)) in *)
    printf "Done\n"