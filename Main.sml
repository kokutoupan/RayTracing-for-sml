structure Main = struct 
  fun many_spheres_stage () = 
  let 
    fun create_random_scene () : Type.shape list = 
    let 
      fun range (low : int, high : int) : int list =
        List.tabulate (high - low + 1, fn i => low + i)

      val a_indices = range (~11, 10)
      val b_indices = range (~11, 10)

      val small_spheres_nested_list =
        List.map (fn a_int =>
          List.mapPartial (fn b_int =>
          let
            val real_a = Real.fromInt a_int
            val real_b = Real.fromInt b_int
            val choose_mat = randReal ()
            val center_offset_x = real_a + 0.9 * (randReal ())
            val center_offset_z = real_b + 0.9 * (randReal ())
            val center = Vec3.create (center_offset_x, 0.2, center_offset_z)
          in
            (*TDOO*)
            if Vec3.length_sq (Vec3.sub center (Vec3.create(4.0, 0.2,0.0))) < 0.89
            then 
              NONE
            else
              let 
                val sphere_material =
                  if choose_mat < 0.8 then 
                    let 
                      val albedo = Vec3.random_vector ()
                    in
                      Lambertian.create albedo NONE
                    end
                  else if choose_mat < 0.95 then
                    let 
                      val albedo = Vec3.random_vector ()
                      val fuzz = (randReal ()) / 2.0
                    in
                      Metal.create albedo fuzz
                    end
                  else
                      Dielectric.create (1.5)

                val new_sphere = Sphere.create center 0.2 sphere_material
              in
                SOME new_sphere
              end
          end
        ) b_indices
        ) a_indices

        val small_spheres = List.concat small_spheres_nested_list

    in
      small_spheres
    end

    val mini_spheres = create_random_scene ();

    val mini_bvh = Hittables.hbvh_build mini_spheres;

    val mat_ground = Lambertian.create (Vec3.create(0.5, 0.5, 0.5))
    (SOME (Texture.checker_texture 0.5));
    val sphere_g = Sphere.create (Vec3.create(0.0, ~1000.0, 0.0))  1000.0 mat_ground;

    val material1 = Dielectric.create 1.5;
    val sphere1 = Sphere.create (Vec3.create(0.0, 1.0, 0.0))  1.0 material1;

    val material2 = Lambertian.create (Vec3.create(0.4, 0.2, 0.1)) NONE;
    val sphere2 = Sphere.create (Vec3.create(~4.0, 1.0, 0.0))  1.0 material2;

    val material3 = Metal.create (Vec3.create(0.7, 0.6, 0.5)) 0.0;
    val sphere3 = Sphere.create (Vec3.create(4.0, 1.0, 0.0))  1.0 material3;

    val world_spheres = sphere_g::sphere1::sphere2::sphere3::mini_bvh::[];

    val wd_obj = Type.Hittable_listT (Hittables.hlst_create_list
    world_spheres);
  in
    wd_obj
  end;

  fun quads () =
  let
    val left_red = Lambertian.create (Vec3.create(1.0, 0.0, 0.0)) NONE;
    val back_green = Lambertian.create (Vec3.create(0.0, 1.0, 0.0)) NONE;
    val right_blue = Lambertian.create (Vec3.create(0.0, 0.0, 1.0)) NONE;
    val uppder_orange = Lambertian.create (Vec3.create(1.0, 0.6, 0.0)) NONE;
    val lowwer_teal = Lambertian.create (Vec3.create(0.0, 0.7, 0.9)) NONE;

    val left = Quad.create (Vec3.create(~3.0,~2.0,5.0))
    (Vec3.create(0.0,0.0,~4.0)) (Vec3.create(0.0,4.0,0.0)) left_red;
    
    val back = Quad.create (Vec3.create(~2.0,~2.0,0.0)) 
    (Vec3.create(4.0,0.0,0.0)) (Vec3.create(0.0,4.0,0.0)) back_green;

    val right = Quad.create (Vec3.create(3.0,~2.0,1.0))
    (Vec3.create(0.0,0.0,4.0)) (Vec3.create(0.0,4.0,0.0)) right_blue;

    val uppder = Quad.create (Vec3.create(~2.0,3.0,1.0))
    (Vec3.create(4.0,0.0,0.0)) (Vec3.create(0.0,0.0,4.0)) uppder_orange;

    val lowwer = Quad.create (Vec3.create(~2.0,~3.0,5.0))
    (Vec3.create(4.0,0.0,0.0)) (Vec3.create(0.0,0.0,~4.0)) lowwer_teal;

  in
    Type.Hittable_listT (Hittables.hlst_create_list [left, back, right, uppder,
    lowwer])
  end;



  val output = "out.ppm";

  (*val wd_obj = many_spheres_stage ();*)
  val wd_obj = quads ();

  fun render output = 
  let
    val start = Time.now ();
    val _ = Camera.render wd_obj output
    val finish = Time.now ();
    val duration = Time.- (finish,start)
  in
    print ("Time taken: " ^ Time.toString duration ^ "\n")

  end
    
end;

val _ = print("Start\n")
val _ = Main.render Main.output
