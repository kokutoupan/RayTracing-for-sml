structure Camera = struct
  val aspect_ratio = 16.0 / 9.0;
  val image_width = 400;
  val image_height = Real.toInt IEEEReal.TO_NEAREST (Real.fromInt image_width / aspect_ratio);
  
  val viewport_height = 2.0;
  val viewport_width = (Real.fromInt image_width) / (Real.fromInt image_height) * viewport_height;

  (*camera*)

  val focal_length = 1.0;
  val camera_center = Vec3.create(0.0,0.0,0.0);

  val viewport_u = Vec3.create(viewport_width,0.0,0.0);
  val viewport_v = Vec3.create(0.0,~viewport_height,0.0);

  val pixel_delta_u = Vec3.divide viewport_u (Real.fromInt image_width);
  val pixel_delta_v = Vec3.divide viewport_v (Real.fromInt image_height);

  val viewport_upper_left = Vec3.sub ( 
    Vec3.sub (Vec3.sub 
    camera_center (Vec3.create(0.0,0.0,focal_length))
    )
      (Vec3.divide  viewport_u 2.0)) 
      (Vec3.divide viewport_v 2.0);

  val pixel00_loc = Vec3.add viewport_upper_left  (Vec3.scale (Vec3.add
  pixel_delta_u pixel_delta_v) 0.5);
  

  val samples_per_pixel = 10;
  val max_depth = 10;

  val rng = Common.rng;

  fun sample_square rng =
  let
    val x = Random.randReal rng;
    val y = Random.randReal rng;
  in
    Vec3.create (x - 0.5,y - 0.5, 0.0)
  end;


  fun ray_color _ _ 0 = Color.create(0.0,0.0,0.0)
    | ray_color (ray:Ray.t) (world:Type.shape) (depth)= 
  let 
    
    val recode = Hittable_list.hit_shape world ray (0.001,1000.0);

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
                     Type.LambertianT m => Lambertian.scatter (Type.LambertianT m) ray hit
                   | Type.MetalT m => Metal.scatter (Type.MetalT m) ray hit

            in
              Vec3.scaleV col (ray_color ray_r world (depth-1))
            end
  in
      recode2col recode
  end;


  fun get_ray (i,j)=
  let
    val offset =  sample_square rng

    val x_base = Real.fromInt i + (#x offset);
    val y_base = Real.fromInt j + (#y offset);
    val sV = Vec3.create(x_base, y_base, 0.0);

    val pixel_sample = Vec3.add pixel00_loc (Vec3.scaleV (Vec3.add
    pixel_delta_u pixel_delta_v) sV);

  

    val ray_dir = Vec3.sub pixel_sample camera_center;

    val ray = Ray.create camera_center (Vec3.unit_vector ray_dir)
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
