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

  fun hit (shape: Type.shape) (ray: Ray.t) ((t_min, t_max): Interval.t) :
    Type.hit_record =
    case shape of
      Type.NONE => Type.NoHit
    (* 基本的な図形：それぞれのモジュールの純粋なhit関数を呼び出す（変更なし）*)
    | Type.SphereT s => Sphere.hit s ray (t_min, t_max)
    | Type.QuadT q => Quad.hit q ray (t_min, t_max)

    (* ===== Hittable List のロジック (旧 hlst_hit) ===== *)
    | Type.Hittable_listT lst_t =>
        if null (#lst lst_t) then
          Type.NoHit
        else
          let
            fun find_closest_hit (h, (closest_hit, closest_so_far)) =
              let
                (* find_closest_hit内部で、このHittable.hit自身を再帰呼び出し *)
                val temp_hit = hit h ray (t_min, closest_so_far)
              in
                case temp_hit of
                  Type.NoHit => (closest_hit, closest_so_far)
                | Type.Hit r => (temp_hit, #t r)
              end
            val initialState = (Type.NoHit, t_max)
            val (final_hit, _) =
              List.foldl find_closest_hit initialState (#lst lst_t)
          in
            final_hit
          end

    (* ===== BVH のロジック (旧 hbvh_hit) ===== *)
    | Type.H_bvhT bvh =>
        if not (AABB.is_hit (#bbox bvh) ray (t_min, t_max)) then
          Type.NoHit
        else
          let
            fun get_ray_dir_component (ray_dir, axis) =
              case axis of
                Type.X_Axis => #x ray_dir
              | Type.Y_Axis => #y ray_dir
              | Type.Z_Axis => #z ray_dir

            val (first_child, second_child) =
              case #axis_opt bvh of
                NONE => (#lhs bvh, #rhs bvh)
              | SOME axis =>
                  let
                    val comp = get_ray_dir_component (#dir ray, axis)
                  in
                    if comp > 0.0 then (#lhs bvh, #rhs bvh)
                    else (#rhs bvh, #lhs bvh)
                  end

            (* BVHの子ノードに対して、このHittable.hit自身を再帰呼び出し *)
            val hit1 = hit first_child ray (t_min, t_max)
            val new_t_max =
              case hit1 of
                Type.NoHit => t_max
              | Type.Hit r => #t r

            (* 探索範囲を狭めて、もう片方の子ノードを探索 *)
            val hit2 = hit second_child ray (t_min, new_t_max)
          in
            case hit2 of
              Type.NoHit => hit1
            | _ => hit2
          end

    (* ===== Translate のロジック (旧 trans_hit) ===== *)
    | Type.TranslateT trans =>
        let
          val moved_ray =
            Ray.create (Vec3.sub (#orig ray) (#offset trans)) (#dir ray)
          (* 動かす前のオブジェクトに対して、このHittable.hit自身を再帰呼び出し *)
          val record = hit (#obj trans) moved_ray (t_min, t_max)
        in
          case record of
            Type.NoHit => Type.NoHit
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

    (* ===== Rotate のロジック (旧 rotate_hit) ===== *)
    | Type.RotateT rot =>
        let
          val rotated_orig = Vec3.rotate (#orig ray) (#axis rot) (#angle rot)
          val rotated_dir = Vec3.rotate (#dir ray) (#axis rot) (#angle rot)
          val rotated_ray = Ray.create rotated_orig rotated_dir

          (* 回転させる前のオブジェクトに対して、このHittable.hit自身を再帰呼び出し *)
          val record = hit (#obj rot) rotated_ray (t_min, t_max)
        in
          case record of
            Type.NoHit => Type.NoHit
          | Type.Hit r =>
              let
                val p = Vec3.rotate (#p r) (#axis rot) (~(#angle rot))
                val normal = Vec3.rotate (#normal r) (#axis rot) (~(#angle rot))
              in
                Type.Hit
                  { p = p
                  , normal = normal
                  , t = #t r
                  , u = #u r
                  , v = #v r
                  , front_face = #front_face r
                  , mat = #mat r
                  }
              end
        end

    (* ===== Constant Medium のロジック (旧 constantMedium_hit) ===== *)
    | Type.ConstantMediumT cmed =>
        let
          (* 境界オブジェクトとの最初の衝突点を計算 *)
          val rec1 = hit (#boundary cmed) ray Interval.universe
        in
          case rec1 of
            Type.NoHit => Type.NoHit
          | Type.Hit r1 =>
              let
                (* 境界オブジェクトとの次の衝突点を計算 *)
                val rec2 = hit (#boundary cmed) ray
                  ((#t r1) + 0.0001, Real.maxFinite)
              in
                case rec2 of
                  Type.NoHit => Type.NoHit
                | Type.Hit r2 =>
                    let
                      val t_start = Real.max (t_min, #t r1)
                      val t_end = Real.min (t_max, #t r2)

                    in
                      if t_start >= t_end then
                        Type.NoHit
                      else
                        let
                          val ray_length = Vec3.length (#dir ray)
                          val dist_inside = (t_end - t_start) * ray_length
                          val hit_dist =
                            (#negInvDensity cmed) * Math.ln (randReal ())
                        in
                          if hit_dist > dist_inside then
                            Type.NoHit
                          else
                            let
                              val t = t_start + hit_dist / ray_length
                            in
                              Type.Hit
                                { p = Ray.at ray t
                                , normal =
                                    Vec3.create (1.0, 0.0, 0.0) (*任意*)
                                , t = t
                                , u = 0.5 (*任意*)
                                , v = 0.5 (*任意*)
                                , front_face = true (*任意*)
                                , mat = #mat cmed
                                }
                            end
                        end
                    end
              end
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
