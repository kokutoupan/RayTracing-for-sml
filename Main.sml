use "Vec3.sml";
use "Type.sml";
use "Ray.sml";
use "Hittable.sml";
use "Sphere.sml";
use "Hittable_list.sml";

structure Main = struct 
  val aspect_ratio = 16.0 / 9.0;
  val image_width = 400;
  val image_height = Real.toInt IEEEReal.TO_NEAREST (Real.fromInt image_width / aspect_ratio);
  
  val viewport_height = 2.0;
  val viewport_width = aspect_ratio * viewport_height;

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

  fun ray_color (ray:Ray.t) = 
    let 
      val ray_dir = (#dir ray);
      val a = 0.5 * (#y ray_dir + 1.0)

      val col = Vec3.add (Vec3.scale (Color.create(1.0,1.0,1.0)) (1.0 - a))  (Vec3.scale
      (Color.create(0.5,0.7,1.0)) a)
    in
      col
    end;

  fun recode2col (recode: Type.hit_record) =
    case recode of 
         Type.NoHit => Color.create(0.0,0.0,0.0)
       | Type.Hit hit => Color.create((#t hit),0.0,1.0)

  fun render filename =
    let
      val out = TextIO.openOut filename

      (* ヘッダーの出力 *)
      val _ = TextIO.output (out, "P3\n")
      val _ = TextIO.output (out, Int.toString image_width ^ " " ^ Int.toString image_height ^ "\n255\n")

      val sq = Sphere.create (Vec3.create(0.0, 0.0, ~1.0))  0.5;
      (* メインループ：ピクセル毎にRGB値を計算して出力 *)
      val _ =



        List.app (fn j =>
          let 
            val _ = print("\rScanlines remaining:" ^ Int.toString (image_height - j) )
          in
            List.app (fn i =>
              let
                val pixel_center = Vec3.add pixel00_loc (Vec3.scaleV (Vec3.add
                pixel_delta_u pixel_delta_v) (Vec3.create(Real.fromInt i, Real.fromInt j,
                0.0)));

                val ray_dir = Vec3.sub pixel_center camera_center;

                val ray = Ray.create camera_center (Vec3.unit_vector ray_dir);

                val recode = Sphere.hit sq ray 0.0 100000.0;

                (*val col = ray_color ray;*)
                val col = recode2col recode;

                val _ = Color.write_color out col
              in
                ()
              end
            ) (List.tabulate (image_width, fn x => x))
          end
        ) (List.tabulate (image_height, fn y => y))

      val _ = TextIO.closeOut out
    in
      ()
    end
    
end;
