let write_color chan (color: Vec3.vec3) = 
  Printf.fprintf chan "%d %d %d\n" (int_of_float (255.999 *. color.x)) (int_of_float (255.999 *. color.y)) (int_of_float (255.999 *. color.z))