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


  fun ray_color (ray:Ray.t) (world:Type.shape) = 
  let 
    
    val recode = Hittable_list.hit_shape world ray (0.0,100000.0);

    fun recode2col (recode: Type.hit_record) =
      case recode of 
          Type.NoHit => Color.create(0.0,0.0,0.0)
        | Type.Hit hit =>
            let
              val n = #normal hit
            in
              Vec3.scale (Color.create(((#x n)+1.0), ((#y n)+1.0), ((#z
              n)+1.0))) 0.5
            end
  in
      recode2col recode
  end;


  fun get_ray world (i,j)=
  let
    val pixel_center = Vec3.add pixel00_loc (Vec3.scaleV (Vec3.add
    pixel_delta_u pixel_delta_v) (Vec3.create(Real.fromInt i, Real.fromInt j,
    0.0)));

    val ray_dir = Vec3.sub pixel_center camera_center;

    val ray = Ray.create camera_center (Vec3.unit_vector ray_dir);

    (*val col = ray_color ray;*)
    val col = ray_color ray world
  in 
    col
  end;

    fun render filename (world:Type.shape)=
    let
      
      val get_ray = get_ray world

      val out = TextIO.openOut filename

      (* ヘッダーの出力 *)
      val _ = TextIO.output (out, "P3\n")
      val _ = TextIO.output (out, Int.toString image_width ^ " " ^ Int.toString image_height ^ "\n255\n")

      (* メインループ：ピクセル毎にRGB値を計算して出力 *)
      val _ =
        List.app (fn j =>
          let 
            val _ = print("\rScanlines remaining:" ^ Int.toString (image_height - j) )
          in
            List.app (fn i =>
              let

                val col = foldr (fn (_, acc) => Vec3.add (get_ray(i, j))  acc)
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
