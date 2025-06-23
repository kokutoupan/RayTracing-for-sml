structure Hittables =
struct
  type hlst_t = Type.hittable_list;
  type hbvh_t = Type.h_bvh;
  type translate_t = Type.translate_t;
  type rotate_t = Type.rotate_t;
  type constantMedium_t = Type.constantMedium_t;

  val Min_Leaf_size = 15

  fun hlst_empty () =
    { lst = []
    , bbox = AABB.createV (Vec3.create (~0.0, ~0.0, ~0.0))
        (Vec3.create (~0.0, ~0.0, ~0.0))
    }

  fun hlst_add (hittable_list: hlst_t) (shape: Type.shape) =
    { lst = shape :: (#lst hittable_list)
    , bbox = AABB.createBB (#bbox hittable_list) (Type.bbox_of_shape shape)
    }

  fun hlst_add_list (hittable_list: hlst_t) (shapes: Type.shape list) =
    let
      val lst =
        foldl (fn (shape, hlst) => hlst_add hlst shape) hittable_list shapes
    in
      lst
    end

  fun hlst_create_list (shapes: Type.shape list) =
    hlst_add_list (hlst_empty ()) shapes


  fun create_constantMedium (boundary: Type.shape) (density: real)
    (mat: Type.material) : Type.shape =
    let
      val negInvDensity = ~1.0 / density
    in
      Type.ConstantMediumT
        {boundary = boundary, negInvDensity = negInvDensity, mat = mat}
    end;

  fun create_box (a: Vec3.t) (b: Vec3.t) (mat: Type.material) : Type.shape =
    let
      val min =
        Vec3.create
          ( (Real.min ((#x a), (#x b)))
          , (Real.min ((#y a), (#y b)))
          , (Real.min ((#z a), (#z b)))
          );
      val max =
        Vec3.create
          ( (Real.max ((#x a), (#x b)))
          , (Real.max ((#y a), (#y b)))
          , (Real.max ((#z a), (#z b)))
          );


      val dx = Vec3.create ((#x max) - (#x min), 0.0, 0.0);
      val dy = Vec3.create (0.0, (#y max) - (#y min), 0.0);
      val dz = Vec3.create (0.0, 0.0, (#z max) - (#z min));

      val front =
        Quad.create (Vec3.create ((#x min), (#y min), (#z max))) dx dy mat;
      val right =
        Quad.create (Vec3.create ((#x max), (#y min), (#z max))) (Vec3.neg dz)
          dy mat;
      val back =
        Quad.create (Vec3.create ((#x max), (#y min), (#z min))) (Vec3.neg dx)
          dy mat;
      val left =
        Quad.create (Vec3.create ((#x min), (#y min), (#z min))) dz dy mat;
      val top =
        Quad.create (Vec3.create ((#x min), (#y max), (#z max))) dx
          (Vec3.neg dz) mat;
      val bottom =
        Quad.create (Vec3.create ((#x min), (#y min), (#z min))) dx dz mat;

      val objs = hlst_create_list [front, back, right, left, top, bottom]

    in
      Type.Hittable_listT objs
    end

  fun create_translate (obj: Type.shape) (offset: Vec3.t) =
    Type.TranslateT
      { obj = obj
      , offset = offset
      , bbox = AABB.translate (Type.bbox_of_shape obj) offset
      };

  fun create_rotate (obj: Type.shape) (axis: Vec3.t) (angle: real) =
    Type.RotateT
      { obj = obj
      , axis = axis
      , angle = angle
      , bbox = AABB.rotate (Type.bbox_of_shape obj) axis angle
      };

  local
    (* レイの方向ベクトルの特定の軸成分を取得するヘルパー関数 *)
    fun get_ray_dir_component (ray_dir: Vec3.t, axis: Type.split_axis) : real =
      case axis of
        Type.X_Axis => #x ray_dir
      | Type.Y_Axis => #y ray_dir
      | Type.Z_Axis => #z ray_dir
  in
    fun hlst_hit ({lst = [], ...}: hlst_t) (_: Ray.t) (_: Interval.t) :
      Type.hit_record = Type.NoHit
      | hlst_hit ({lst = lst, ...}: hlst_t) (ray: Ray.t)
          ((ray_t_min, ray_t_max): Interval.t) : (Type.hit_record) =
          let
            fun hit_list ([]: Type.shape list) (_: Ray.t) (_: Interval.t) :
              Type.hit_record = Type.NoHit
              | hit_list ((h :: hittable_list): Type.shape list) (ray: Ray.t)
                  ((ray_t_min, ray_t_max): Interval.t) : Type.hit_record =
                  let
                    val hitrec = hit_shape h ray (ray_t_min, ray_t_max)
                    val next_hitrec =
                      case hitrec of
                        Type.NoHit =>
                          hit_list hittable_list ray (ray_t_min, ray_t_max)
                      | Type.Hit r =>
                          hit_list hittable_list ray (ray_t_min, (#t r))
                  in
                    case next_hitrec of
                      Type.NoHit => hitrec
                    | _ => next_hitrec
                  end
          in
            hit_list lst ray (ray_t_min, ray_t_max)
          end

    and hbvh_hit (hbvh: hbvh_t) ray (t_min, t_max) : Type.hit_record =
      if AABB.is_hit (#bbox hbvh) ray (t_min, t_max) then
        let
          val lhs = #lhs hbvh
          val rhs = #rhs hbvh


          val (first_child_to_test, second_child_to_test) =
            case #axis_opt hbvh of
              NONE => (lhs, rhs)
            | SOME axis =>
                let val ray_dir_comp = get_ray_dir_component ((#dir ray), axis)
                in if ray_dir_comp > 0.0 then (lhs, rhs) else (rhs, lhs)
                end

          val hit_rec1 =
            hit_shape first_child_to_test ray
              (t_min, t_max) (* hit_shapeはshapeを扱う関数 *)
          val t_max_updated =
            case hit_rec1 of
              Type.NoHit => t_max
            | Type.Hit r => #t r
          val hit_rec2 =
            hit_shape second_child_to_test ray (t_min, t_max_updated)

        in
          case hit_rec2 of
            Type.NoHit => hit_rec1
          | _ => hit_rec2
        end
      else
        Type.NoHit

    and trans_hit (trans: translate_t) (ray: Ray.t) (t_ren: Interval.t) :
      Type.hit_record =
      let
        val trans_ray =
          Ray.create (Vec3.sub (#orig ray) (#offset trans)) (#dir ray)
        val record = hit_shape (#obj trans) trans_ray t_ren
      in
        case record of
          Type.NoHit => record
        | Type.Hit r =>
            Type.Hit
              { p = Vec3.add (#p r) (#offset trans)
              , normal = #normal r
              , t = #t r
              , u = #u r
              , v = #v r
              , front_face = #front_face r
              , mat = #mat r
              }
      end

    and rotate_hit (rotate: rotate_t) (ray: Ray.t) (t_ren: Interval.t) :
      Type.hit_record =
      let
        val rotate_ray =
          Ray.create (Vec3.rotate (#orig ray) (#axis rotate) (#angle rotate))
            (Vec3.rotate (#dir ray) (#axis rotate) (#angle rotate))
        val record = hit_shape (#obj rotate) rotate_ray t_ren
      in
        case record of
          Type.NoHit => Type.NoHit
        | Type.Hit r =>
            Type.Hit
              { p = Vec3.rotate (#p r) (#axis rotate) (~(#angle rotate))
              , normal = Vec3.rotate (#normal r) (#axis rotate)
                  (~(#angle rotate))
              , t = #t r
              , u = #u r
              , v = #v r
              , front_face = #front_face r
              , mat = #mat r
              }
      end

    and constantMedium_hit (cmed: constantMedium_t) ray (t_min, t_max) :
      Type.hit_record =
      let
        fun clamp_time (t1, t2) : real * real =
          let
            val tftmp = if t1 < t_min then t_min else t1

            val tf = if tftmp < 0.0 then 0.0 else tftmp
            val ta = if t2 > t_max then t_max else t2
          in
            (tf, ta)
          end


        fun distance_inside (t1, tf, ta) : real =
          if t1 >= ta then
            0.0
          else
            let
              val ray_length = Vec3.length (#dir ray)
              val dist_inside_boundary = (ta - tf) * ray_length
            in
              dist_inside_boundary
            end

        fun hit_distance () : real =
          (#negInvDensity cmed) * Math.ln (randReal ())

        fun hit_time (t1, t2) : real =
          let
            val (tf, ta) = clamp_time (t1, t2)
            val distance = distance_inside (t1, tf, ta)
            val hit_dis = hit_distance ()
          in
            if distance > hit_dis then tf + hit_dis / (Vec3.length (#dir ray))
            else ~1.0
          end

        val rec1 = hit_shape (#boundary cmed) ray Interval.universe
      in
        case rec1 of
          Type.NoHit => Type.NoHit
        | Type.Hit r1 =>
            let
              val t1 = #t r1
              val rec2 =
                hit_shape (#boundary cmed) ray (t1 + 0.001, Real.maxFinite)
            in
              case rec2 of
                Type.NoHit => Type.NoHit
              | Type.Hit r =>
                  let
                    val t = hit_time (t1, (#t r))
                  in
                    if t < ~0.5 then
                      Type.NoHit
                    else
                      Type.Hit
                        { p = Ray.at ray t
                        , normal = Vec3.create (1.0, 0.0, 0.0)
                        , t = t
                        , u = 0.5
                        , v = 0.5
                        , front_face = true
                        , mat = #mat cmed
                        }
                  end
            end
      end

    and hit_shape (Type.NONE) _ _ = Type.NoHit
      | hit_shape (Type.SphereT s) ray (t_min, t_max) : Type.hit_record =
          Sphere.hit s ray (t_min, t_max)
      | hit_shape (Type.QuadT q) ray (t_min, t_max) : Type.hit_record =
          Quad.hit q ray (t_min, t_max)
      | hit_shape (Type.ConstantMediumT cmed) ray (t_min, t_max) :
        Type.hit_record =
          constantMedium_hit cmed ray (t_min, t_max)
      | hit_shape (Type.Hittable_listT lst) ray (t_min, t_max) : Type.hit_record =
          hlst_hit lst ray (t_min, t_max)
      | hit_shape (Type.H_bvhT bvh) ray (t_min, t_max) : Type.hit_record =
          hbvh_hit bvh ray (t_min, t_max)
      | hit_shape (Type.TranslateT trans) ray (t_min, t_max) : Type.hit_record =
          trans_hit trans ray (t_min, t_max)
      | hit_shape (Type.RotateT rot) ray (t_min, t_max) : Type.hit_record =
          rotate_hit rot ray (t_min, t_max)
  end

  fun hbvh_create (lhs: Type.shape) (rhs: Type.shape) =
    { lhs = lhs
    , rhs = rhs
    , bbox = AABB.createBB (Type.bbox_of_shape lhs) (Type.bbox_of_shape rhs)
    }

  fun hbvh_build (lst: Type.shape list) : Type.shape =
    if length lst <= Min_Leaf_size then
      let
        val lst = hlst_create_list lst
      in
        Type.H_bvhT
          { lhs = Type.Hittable_listT lst
          , rhs = Type.NONE
          , bbox = (#bbox lst)
          , axis_opt = NONE
          }
      end
    else
      let
        val hlst = hlst_create_list lst
        val select_axis = AABB.longest_axis (#bbox hlst)

        (*val comp = AABB.box_min_cmp select_axis*)
        val comp = AABB.box_center_cmp select_axis
        val bbox = (#bbox hlst)

        val _ = AABB.print_aabb (#bbox hlst)

        val sorted_lst =
          ListMergeSort.sort
            (fn (h1, h2) => comp (Type.bbox_of_shape h1, Type.bbox_of_shape h2))
            lst
      in
        Type.H_bvhT
          { lhs = hbvh_build (List.drop (sorted_lst, (length lst) div 2))
          , rhs = hbvh_build (List.take (sorted_lst, (length lst) div 2))
          , bbox = bbox
          , axis_opt = SOME
              (if select_axis = 0 then Type.X_Axis
               else if select_axis = 1 then Type.Y_Axis
               else Type.Z_Axis)
          }
      end

end;
