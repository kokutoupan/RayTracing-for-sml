structure Camera =
struct
  (* 1. 設定値をまとめるためのレコード型を定義 *)
  type settings =
    { aspect_ratio: real
    , image_width: int
    , samples_per_pixel: int
    , max_depth: int
    , vfov: real
    , (* Vertical view angle (field of view) *)
      lookfrom: Vec3.t
    , (* Point camera is looking from *)
      lookat: Vec3.t
    , (* Point camera is looking at *)
      vup: Vec3.t
    , (* Camera-relative "up" direction *)
      defocus_angle: real
    , (* Variation angle of rays through each pixel *)
      focus_dist: real
    , (* Distance from camera lookfrom point to plane of perfect focus *)
      background_color: Color.t
    }

  (* 2. 設定から計算される内部状態を保持するレコード型を定義 *)
  type camera_params =
    { image_height: int
    , camera_center: Vec3.t
    , pixel00_loc: Vec3.t
    , pixel_delta_u: Vec3.t
    , pixel_delta_v: Vec3.t
    , defocus_disk_u: Vec3.t
    , defocus_disk_v: Vec3.t
    , background_color: Color.t
    }

  (* 3. 設定レコードを受け取り、計算済みのカメラパラメータを返す関数 *)
  fun create (s: settings) : camera_params =
    let
      val image_height = Real.toInt IEEEReal.TO_NEAREST
        (Real.fromInt (#image_width s) / #aspect_ratio s)

      val camera_center = #lookfrom s
      val h = Math.tan ((Common.degrees_to_radians (#vfov s)) / 2.0)

      val viewport_height = 2.0 * h * #focus_dist s
      val viewport_width =
        (Real.fromInt (#image_width s)) / (Real.fromInt image_height)
        * viewport_height

      val w = Vec3.unit_vector (Vec3.sub (#lookfrom s) (#lookat s))
      val u = Vec3.unit_vector (Vec3.cross (#vup s) w)
      val v = Vec3.cross w u

      val viewport_u = Vec3.scale u viewport_width
      val viewport_v = Vec3.scale (Vec3.neg v) viewport_height

      val pixel_delta_u = Vec3.divide viewport_u (Real.fromInt (#image_width s))
      val pixel_delta_v = Vec3.divide viewport_v (Real.fromInt image_height)

      val viewport_upper_left =
        Vec3.sub
          (Vec3.sub (Vec3.sub camera_center (Vec3.scale w (#focus_dist s)))
             (Vec3.divide viewport_u 2.0)) (Vec3.divide viewport_v 2.0)

      val pixel00_loc = Vec3.add viewport_upper_left
        (Vec3.scale (Vec3.add pixel_delta_u pixel_delta_v) 0.5)

      val defocus_radius =
        (#focus_dist s)
        * (Math.tan (Common.degrees_to_radians ((#defocus_angle s) / 2.0)))
      val defocus_disk_u = Vec3.scale u defocus_radius
      val defocus_disk_v = Vec3.scale v defocus_radius
    in
      { image_height = image_height
      , camera_center = camera_center
      , pixel00_loc = pixel00_loc
      , pixel_delta_u = pixel_delta_u
      , pixel_delta_v = pixel_delta_v
      , defocus_disk_u = defocus_disk_u
      , defocus_disk_v = defocus_disk_v
      , background_color = #background_color s
      }
    end


  (* 4. render関数を修正し、settingsを引数に取るように変更 *)
  fun render (settings: settings) (world: Type.shape) (filename: string) =
    let
      (* 設定からカメラの内部パラメータを計算 *)
      val cam_params = create settings
      
      val samples_per_pixel = #samples_per_pixel settings
      val max_depth = #max_depth settings
      val image_height = #image_height cam_params
      val image_width = #image_width settings

      (* ヘルパー関数をrender関数の内部で定義 *)
      fun sample_square () =
        let
          val x = randReal ()
          val y = randReal ()
        in
          Vec3.create (x - 0.5, y - 0.5, 0.0)
        end

      fun defocus_disk_sample center () =
        let
          val p = Vec3.random_unit_vector ()
        in
          Vec3.add center
            (Vec3.add (Vec3.scale (#defocus_disk_u cam_params) (#x p))
               (Vec3.scale (#defocus_disk_v cam_params) (#y p)))
        end

      fun get_ray (i, j) =
        let
          val offset = sample_square ()

          val u = Real.fromInt i + (#x offset)
          val v = Real.fromInt j + (#y offset)
          val pixel_sample =
            Vec3.add (#pixel00_loc cam_params)
              (Vec3.add (Vec3.scale (#pixel_delta_u cam_params) u)
                 (Vec3.scale (#pixel_delta_v cam_params) v))

          val ray_orig =
            if #defocus_angle settings > 0.0 then
              defocus_disk_sample (#camera_center cam_params) ()
            else
              #camera_center cam_params

          val ray_dir = Vec3.sub pixel_sample ray_orig
          val ray = Ray.create ray_orig (Vec3.unit_vector ray_dir)
        in
          ray
        end

      fun ray_color _ _ 0 = Color.create (0.0, 0.0, 0.0)
        | ray_color (ray: Ray.t) (world: Type.shape) (depth) =
            let
              val recode = Hittables.hit world ray
                (Interval.create 0.001 Real.maxFinite)

              fun recode2col (recode: Type.hit_record) =
                case recode of
                  Type.NoHit => (#background_color cam_params)
                | Type.Hit hit =>
                    let
                      val mat = #mat hit
                      val hit_record = Type.Hit hit

                      val emit_color =
                        case mat of Type.Material m => (#emit m) ray hit_record

                      val scatter_res =
                        case mat of
                          Type.Material m => (#scatter m) ray hit_record
                    in
                      case scatter_res of
                        NONE => emit_color
                      | SOME (ray_r, col) =>
                          Vec3.add emit_color (Vec3.scaleV col
                            (ray_color ray_r world (depth - 1)))
                    end
            in
              recode2col recode
            end

      fun compute_pixel_color (i,j) =
      let
        fun sample_once _ =
          let
            val ray = get_ray (i, j)
          in
            ray_color ray world max_depth
          end

        (* 指定回数サンプリングして色のリストを取得 *)
        val color_samples = List.tabulate (samples_per_pixel, sample_once)

        (* 全サンプルの色を合計 *)
        val total_color = List.foldl (fn (color, sum) => Vec3.add color sum) Vec3.zero color_samples

        (* 平均色を計算するためのスケール *)
        val scale = Real.fromInt samples_per_pixel
      in
        (Vec3.divide total_color scale)
      end

      (* === レンダリング本体 === *)

      val header = "P3\n"
                 ^ Int.toString image_width ^ " "
                 ^ Int.toString image_height ^ "\n"
                 ^ "255\n"


      val out = TextIO.openOut filename

      val _ = TextIO.output (out, header)

      val _ =
        List.app
          (fn j =>
             let
               val _ = print
                 ("\rScanlines remaining:"
                  ^ Int.toString (image_height - j) ^ "   ")
             in
               List.app
                 (fn i =>
                    let
                      val pixel_color = compute_pixel_color (i, j)
                    in
                      Color.write_color out pixel_color
                    end) (List.tabulate (image_width, fn x => x))
             end) (List.tabulate (image_height, fn y => y))

      val _ = TextIO.closeOut out
    in
      print("\nDone.\n")
    end
end
