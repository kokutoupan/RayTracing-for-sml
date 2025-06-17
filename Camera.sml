structure Camera = struct
  val aspect_ratio = 1.0;
  val image_width = 200;
  val image_height = Real.toInt IEEEReal.TO_NEAREST (Real.fromInt image_width / aspect_ratio);

  val samples_per_pixel = 30;
  val max_depth = 10;

  val vfov = 80.0;

  val lookfrom = Vec3.create(0.0,0.0,9.0);
  val lookat = Vec3.create(0.0,0.0,0.0);
  val vup = Vec3.create(0.0,1.0,0.0);

  val defocus_angle = 0.0;
  val focus_dist = 10.0;

  (*setting end*)

  val camera_center = lookfrom;
  val h = Math.tan( (Common.degrees_to_radians vfov) / 2.0 ) ;

  val viewport_height = 2.0 * h * focus_dist;
  val viewport_width = (Real.fromInt image_width) / (Real.fromInt image_height) * viewport_height;

  val w = Vec3.unit_vector (Vec3.sub lookfrom lookat);
  val u = Vec3.unit_vector (Vec3.cross vup w);
  val v = Vec3.cross w u;

  val viewport_u = Vec3.scale u viewport_width
  val viewport_v = Vec3.scale (Vec3.neg v) viewport_height

  val pixel_delta_u = Vec3.divide viewport_u (Real.fromInt image_width);
  val pixel_delta_v = Vec3.divide viewport_v (Real.fromInt image_height);

  val viewport_upper_left =
    Vec3.sub
      (Vec3.sub
        (Vec3.sub camera_center (Vec3.scale w focus_dist))
        (Vec3.divide viewport_u 2.0))
      (Vec3.divide viewport_v 2.0)
  

  val pixel00_loc = Vec3.add viewport_upper_left  (Vec3.scale (Vec3.add
  pixel_delta_u pixel_delta_v) 0.5);

  val defocus_radius = focus_dist * (Math.tan 
  (Common.degrees_to_radians (defocus_angle/2.0)));
  val defocus_disk_u = Vec3.scale u defocus_radius;
  val defocus_disk_v = Vec3.scale v defocus_radius;
  
    
  fun defocus_disk_sample center ()=
      let 
        val p = Vec3.random_unit_vector ()
      in
        Vec3.add center 
        (Vec3.add 
        (Vec3.scale defocus_disk_u (#x p)) 
        (Vec3.scale defocus_disk_v (#y p)))
      end


  fun sample_square () =
  let
    val x = randReal ();
    val y = randReal ();
  in
    Vec3.create (x - 0.5,y - 0.5, 0.0)
  end;


  fun ray_color _ _ 0 = Color.create(0.0,0.0,0.0)
    | ray_color (ray:Ray.t) (world:Type.shape) (depth)= 
  let 
    
    val recode = Hittables.hit_shape world ray (0.001,1000.0);

    fun recode2col (recode: Type.hit_record) =
      case recode of 
          Type.NoHit => 
          let
            val ray_dir = (#dir ray);
            val a = 0.5 * (#y ray_dir + 1.0)

            val col = Vec3.add (Vec3.scale (Color.create(1.0,1.0,1.0)) (1.0 - a))  (Vec3.scale
            (Color.create(0.5,0.7,1.0)) a)
          in
            col
          end
        | Type.Hit hit =>
            let
              val mat = #mat hit
              val hit = Type.Hit hit
              val (ray_r,col) = 
                case mat of
                     Type.LambertianT m => Lambertian.scatter m ray hit
                   | Type.MetalT m => Metal.scatter m ray hit
                   | Type.DielectricT m => Dielectric.scatter m ray hit

            in
              Vec3.scaleV col (ray_color ray_r world (depth-1))
            end
  in
      recode2col recode
  end;


  fun get_ray (i,j)=
  let
    val offset =  sample_square ()

    val u = Real.fromInt i + (#x offset);
    val v = Real.fromInt j + (#y offset);
    val pixel_sample =
          Vec3.add pixel00_loc
            (Vec3.add
              (Vec3.scale pixel_delta_u u)
              (Vec3.scale pixel_delta_v v))
    
    val ray_orig = if defocus_angle > 0.0 
                   then defocus_disk_sample camera_center () 
                   else camera_center

    val ray_dir = Vec3.sub pixel_sample ray_orig

    val ray = Ray.create ray_orig (Vec3.unit_vector ray_dir)
  in 
    (ray)
  end;

    fun render (world:Type.shape)  filename=
    let
      

      val out = TextIO.openOut filename

      (* ヘッダーの出力 *)
      val _ = TextIO.output (out, "P3\n")
      val _ = TextIO.output (out, Int.toString image_width ^ " " ^ Int.toString image_height ^ "\n255\n")

      (* メインループ：ピクセル毎にRGB値を計算して出力 *)
      val _ =
        List.app (fn j =>
          let 
            val _ = print("\rScanlines remaining:" ^ Int.toString (image_height
            - j) ^ "   ")
          in
            List.app (fn i =>
              let
                val col = foldr (fn (_, acc) => 
                                  Vec3.add (ray_color (get_ray (i, j)) world
                                  max_depth) acc)
                                Vec3.zero
                                (List.tabulate (samples_per_pixel, fn _ => ()))
                (*val col = get_ray  (i,j);*)
                val _ = Color.write_color out (Vec3.divide col (Real.fromInt
                samples_per_pixel))
              in
                ()
              end
            ) (List.tabulate (image_width, fn x => x))
          end
        ) (List.tabulate (image_height, fn y => y))

      val _ = TextIO.closeOut out
      val _ = print("\nDone.\n")
    in
      ()
    end

end;
