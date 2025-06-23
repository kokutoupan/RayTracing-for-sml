structure Main =
struct
  fun many_spheres_stage () =
    let
      fun create_random_scene () : Type.shape list =
        let
          fun range (low: int, high: int) : int list =
            List.tabulate (high - low + 1, fn i => low + i)

          val a_indices = range (~11, 10)
          val b_indices = range (~11, 10)

          val small_spheres_nested_list =
            List.map
              (fn a_int =>
                 List.mapPartial
                   (fn b_int =>
                      let
                        val real_a = Real.fromInt a_int
                        val real_b = Real.fromInt b_int
                        val choose_mat = randReal ()
                        val center_offset_x = real_a + 0.9 * (randReal ())
                        val center_offset_z = real_b + 0.9 * (randReal ())
                        val center =
                          Vec3.create (center_offset_x, 0.2, center_offset_z)
                      in
                        (*TDOO*)
                        if
                          Vec3.length_sq (Vec3.sub center
                            (Vec3.create (4.0, 0.2, 0.0))) < 0.89
                        then
                          NONE
                        else
                          let
                            val sphere_material =
                              if choose_mat < 0.8 then
                                let val albedo = Vec3.random_vector ()
                                in Lambertian.fromColor albedo
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

                            val new_sphere =
                              Sphere.create center 0.2 sphere_material
                          in
                            SOME new_sphere
                          end
                      end) b_indices) a_indices

          val small_spheres = List.concat small_spheres_nested_list

        in
          small_spheres
        end

      val mini_spheres = create_random_scene ();

      val mini_bvh = Hittables.hbvh_build mini_spheres;

      val mat_ground = Lambertian.fromTexture (Texture.checker_texture 0.5);
      val sphere_g =
        Sphere.create (Vec3.create (0.0, ~1000.0, 0.0)) 1000.0 mat_ground;

      val material1 = Dielectric.create 1.5;
      val sphere1 = Sphere.create (Vec3.create (0.0, 1.0, 0.0)) 1.0 material1;

      val material2 = Lambertian.fromColor (Vec3.create (0.4, 0.2, 0.1));
      val sphere2 = Sphere.create (Vec3.create (~4.0, 1.0, 0.0)) 1.0 material2;

      val material3 = Metal.create (Vec3.create (0.7, 0.6, 0.5)) 0.0;
      val sphere3 = Sphere.create (Vec3.create (4.0, 1.0, 0.0)) 1.0 material3;

      val world_spheres =
        sphere_g :: sphere1 :: sphere2 :: sphere3 :: mini_bvh :: [];

      val wd_obj =
        Type.Hittable_listT (Hittables.hlst_create_list world_spheres);

      val cam_settings =
        { aspect_ratio = 16.0 / 9.0
        , image_width = 600
        , samples_per_pixel = 300
        , max_depth = 50
        , vfov = 20.0
        , lookfrom = Vec3.create (13.0, 2.0, 3.0)
        , lookat = Vec3.create (0.0, 0.0, 0.0)
        , vup = Vec3.create (0.0, 1.0, 0.0)
        , defocus_angle = 0.6
        , focus_dist = 10.0
        , background_color = Color.white
        }

    in
      (cam_settings, wd_obj)
    end;

  fun quads () =
    let
      val left_red = Lambertian.fromColor (Vec3.create (1.0, 0.0, 0.0));
      val back_green = Lambertian.fromColor (Vec3.create (0.0, 1.0, 0.0));
      val right_blue = Lambertian.fromColor (Vec3.create (0.0, 0.0, 1.0));
      (*val uppder_orange = Lambertian.fromColor (Vec3.create(1.0, 0.6, 0.0));*)
      val uppder_orange = DiffuseLight.fromColor (Vec3.create (1.0, 1.0, 1.0));
      val lowwer_teal = Lambertian.fromColor (Vec3.create (0.0, 0.7, 0.9));

      val left =
        Quad.create (Vec3.create (~3.0, ~2.0, 5.0))
          (Vec3.create (0.0, 0.0, ~4.0)) (Vec3.create (0.0, 4.0, 0.0)) left_red;

      val back =
        Quad.create (Vec3.create (~2.0, ~2.0, 0.0))
          (Vec3.create (4.0, 0.0, 0.0)) (Vec3.create (0.0, 4.0, 0.0)) back_green;

      val right =
        Quad.create (Vec3.create (3.0, ~2.0, 1.0)) (Vec3.create (0.0, 0.0, 4.0))
          (Vec3.create (0.0, 4.0, 0.0)) right_blue;

      val uppder =
        Quad.create (Vec3.create (~2.0, 3.0, 1.0)) (Vec3.create (4.0, 0.0, 0.0))
          (Vec3.create (0.0, 0.0, 4.0)) uppder_orange;

      val lowwer =
        Quad.create (Vec3.create (~2.0, ~3.0, 5.0))
          (Vec3.create (4.0, 0.0, 0.0)) (Vec3.create (0.0, 0.0, ~4.0))
          lowwer_teal;

      val world = Type.Hittable_listT (Hittables.hlst_create_list
        [left, back, right, uppder, lowwer])

      val cam_settings =
        { aspect_ratio = 1.0
        , image_width = 400
        , samples_per_pixel = 100
        , max_depth = 30
        , vfov = 80.0
        , lookfrom = Vec3.create (0.0, 0.0, 9.0)
        , lookat = Vec3.create (0.0, 0.0, 0.0)
        , vup = Vec3.create (0.0, 1.0, 0.0)
        , defocus_angle = 0.0
        , focus_dist = 10.0
        , background_color = Color.create (0.7, 0.8, 1.0)
        }
    in
      (cam_settings, world)
    end;

  fun cornell_box () =
    let
      val red = Lambertian.fromColor (Vec3.create (0.65, 0.05, 0.05));
      val white = Lambertian.fromColor (Vec3.create (0.73, 0.73, 0.73));
      val green = Lambertian.fromColor (Vec3.create (0.12, 0.45, 0.15));
      val light = DiffuseLight.fromColor (Vec3.create (15.0, 15.0, 15.0));

      (* 左の壁 (x=555) *)
      val left =
        Quad.create (Vec3.create (555.0, 0.0, 0.0))
          (Vec3.create (0.0, 0.0, 555.0)) (* vが先に *)
          (Vec3.create (0.0, 555.0, 0.0)) (* uが後に *) green;

      (* 右の壁 (x=0) *)
      val right =
        Quad.create (Vec3.create (0.0, 0.0, 0.0))
          (Vec3.create (0.0, 555.0, 0.0)) (Vec3.create (0.0, 0.0, 555.0)) red;

      (* 光源 *)
      val light_source =
        Quad.create (Vec3.create (343.0, 554.0, 332.0))
          (Vec3.create (~130.0, 0.0, 0.0)) (Vec3.create (0.0, 0.0, ~105.0))
          light;
      (* 床 (y=0) *)
      val floor =
        Quad.create (Vec3.create (0.0, 0.0, 0.0))
          (Vec3.create (0.0, 0.0, 555.0)) (* vが先に *)
          (Vec3.create (555.0, 0.0, 0.0)) (* uが後に *) white;

      val ceiling =
        Quad.create (Vec3.create (555.0, 555.0, 555.0))
          (Vec3.create (~555.0, 0.0, 0.0)) (Vec3.create (0.0, 0.0, ~555.0))
          white;

      val back_wall =
        Quad.create (Vec3.create (0.0, 0.0, 555.0))
          (Vec3.create (0.0, 555.0, 0.0)) (* vが先に *)
          (Vec3.create (555.0, 0.0, 0.0)) (* uが後に *) white;

      val small_box =
        Hittables.create_translate
          (Hittables.create_rotate
             (Hittables.create_box (Vec3.create (0.0, 0.0, 0.0))
                (Vec3.create (165.0, 165.0, 165.0)) white) Vec3.Yaxis
             (Common.deg2rad 15.0)) (Vec3.create (130.0, 0.0, 65.0));


      val big_box =
        Hittables.create_translate
          (Hittables.create_rotate
             (Hittables.create_box (Vec3.create (0.0, 0.0, 0.0))
                (Vec3.create (165.0, 330.0, 165.0)) white) Vec3.Yaxis
             (Common.deg2rad ~18.0)) (Vec3.create (265.0, 0.0, 295.0))


      val cam_settings =
        { aspect_ratio = 1.0
        , image_width = 200
        , samples_per_pixel = 50
        , max_depth = 30
        , vfov = 40.0
        , lookfrom = Vec3.create (278.0, 278.0, ~800.0)
        , lookat = Vec3.create (278.0, 278.0, 0.0)
        , vup = Vec3.create (0.0, 1.0, 0.0)
        , defocus_angle = 0.0
        , focus_dist = 10.0
        , background_color = Color.black
        }

    in
      ( cam_settings
      , Type.Hittable_listT (Hittables.hlst_create_list
          [ left
          , right
          , light_source
          , floor
          , ceiling
          , back_wall
          , small_box
          , big_box
          ])
      )

    end

  fun cornell_smoke () =
    let
      val red = Lambertian.fromColor (Vec3.create (0.65, 0.05, 0.05));
      val white = Lambertian.fromColor (Vec3.create (0.73, 0.73, 0.73));
      val green = Lambertian.fromColor (Vec3.create (0.12, 0.45, 0.15));
      val light = DiffuseLight.fromColor (Vec3.create (7.0, 7.0, 7.0));
      val black_iso = Isotropic.fromColor (Color.black);
      val white_iso = Isotropic.fromColor (Color.white);

      (* 左の壁 (x=555) *)
      val left =
        Quad.create (Vec3.create (555.0, 0.0, 0.0))
          (Vec3.create (0.0, 0.0, 555.0)) (* vが先に *)
          (Vec3.create (0.0, 555.0, 0.0)) (* uが後に *) green;

      (* 右の壁 (x=0) *)
      val right =
        Quad.create (Vec3.create (0.0, 0.0, 0.0))
          (Vec3.create (0.0, 555.0, 0.0)) (Vec3.create (0.0, 0.0, 555.0)) red;

      (* 光源 *)
      val light_source =
        Quad.create (Vec3.create (443.0, 554.0, 432.0))
          (Vec3.create (~330.0, 0.0, 0.0)) (Vec3.create (0.0, 0.0, ~305.0))
          light;
      (* 床 (y=0) *)
      val floor =
        Quad.create (Vec3.create (0.0, 0.0, 0.0))
          (Vec3.create (0.0, 0.0, 555.0)) (* vが先に *)
          (Vec3.create (555.0, 0.0, 0.0)) (* uが後に *) white;

      val ceiling =
        Quad.create (Vec3.create (555.0, 555.0, 555.0))
          (Vec3.create (~555.0, 0.0, 0.0)) (Vec3.create (0.0, 0.0, ~555.0))
          white;

      val back_wall =
        Quad.create (Vec3.create (0.0, 0.0, 555.0))
          (Vec3.create (0.0, 555.0, 0.0)) (* vが先に *)
          (Vec3.create (555.0, 0.0, 0.0)) (* uが後に *) white;

      val small_box =
        Hittables.create_constantMedium
          (Hittables.create_translate
             (Hittables.create_rotate
                (Hittables.create_box (Vec3.create (0.0, 0.0, 0.0))
                   (Vec3.create (165.0, 165.0, 165.0)) white) Vec3.Yaxis
                (Common.deg2rad 15.0)) (Vec3.create (130.0, 0.0, 65.0))) 0.01
          white_iso;


      val big_box =
        Hittables.create_constantMedium
          (Hittables.create_translate
             (Hittables.create_rotate
                (Hittables.create_box (Vec3.create (0.0, 0.0, 0.0))
                   (Vec3.create (165.0, 330.0, 165.0)) white) Vec3.Yaxis
                (Common.deg2rad ~18.0)) (Vec3.create (265.0, 0.0, 295.0))) 0.01
          black_iso


      val cam_settings =
        { aspect_ratio = 1.0
        , image_width = 300
        , samples_per_pixel = 100
        , max_depth = 50
        , vfov = 40.0
        , lookfrom = Vec3.create (278.0, 278.0, ~800.0)
        , lookat = Vec3.create (278.0, 278.0, 0.0)
        , vup = Vec3.create (0.0, 1.0, 0.0)
        , defocus_angle = 0.0
        , focus_dist = 10.0
        , background_color = Color.black
        }

    in
      ( cam_settings
      , Type.Hittable_listT (Hittables.hlst_create_list
          [ left
          , right
          , light_source
          , floor
          , ceiling
          , back_wall
          , small_box
          , big_box
          ])
      )

    end


  val output = "out.ppm";

  (*val wd_obj = many_spheres_stage ();*)
  val (cam_settings, wd_obj) = cornell_smoke ();

  fun render output =
    let
      val start = Time.now ();
      val _ = Camera.render cam_settings wd_obj output
      val finish = Time.now ();
      val duration = Time.- (finish, start)
    in
      print ("Time taken: " ^ Time.toString duration ^ "\n")

    end

end;

val _ = print ("Start\n")
val _ = Main.render Main.output
