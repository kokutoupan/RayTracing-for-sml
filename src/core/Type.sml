structure Type =
struct

  type texture_t = real -> real -> Vec3.t -> Color.t

  type lambertian_t = {tex: texture_t}
  type metal_t = {albedo: Color.t, fuzz: real}
  type dielectric_t = {ref_idx: real}
  type diffuseLight_t = {tex: texture_t}
  type isotropic_t = {tex: texture_t}

  datatype hit_record =
    NoHit
  | Hit of
      { p: Vec3.t
      , normal: Vec3.t
      , t: real
      , u: real
      , v: real
      , front_face: bool
      , mat: material
      }
  and material =
    Material of {
      scatter: Ray.t -> hit_record -> (Ray.t * Color.t) option,
      emit:    Ray.t -> hit_record -> Color.t
    }

  datatype split_axis = X_Axis | Y_Axis | Z_Axis

  type sphere = {center: Vec3.t, radius: real, mat: material, bbox: AABB.t};
  type quad_t =
    { q: Vec3.t
    , u: Vec3.t
    , v: Vec3.t
    , w: Vec3.t
    , mat: material
    , bbox: AABB.t
    , normal: Vec3.t
    , d: real
    };


  datatype shape =
    Hittable_listT of {lst: shape list, bbox: AABB.t}
  | SphereT of sphere
  | QuadT of quad_t
  | ConstantMediumT of {boundary: shape, negInvDensity: real, mat: material}
  | H_bvhT of
      {lhs: shape, rhs: shape, bbox: AABB.t, axis_opt: split_axis option}
  | TranslateT of {obj: shape, offset: Vec3.t, bbox: AABB.t}
  | RotateT of {obj: shape, axis: Vec3.t, angle: real, bbox: AABB.t}
  | NONE; (*dumy*)

  type hittable_list = {lst: shape list, bbox: AABB.t};
  type h_bvh =
    {lhs: shape, rhs: shape, bbox: AABB.t, axis_opt: split_axis option};
  type translate_t = {obj: shape, offset: Vec3.t, bbox: AABB.t}
  type rotate_t = {obj: shape, axis: Vec3.t, angle: real, bbox: AABB.t}
  type constantMedium_t = {boundary: shape, negInvDensity: real, mat: material};
  (* shape から bbox を抽出するヘルパー関数 *)
  fun bbox_of_shape (s: shape) : AABB.t =
    case s of
      Hittable_listT record => #bbox record
    | SphereT sphere_record => #bbox sphere_record
    | QuadT record => #bbox record
    | ConstantMediumT record => bbox_of_shape (#boundary record)
    | H_bvhT record => #bbox record
    | TranslateT record => #bbox record
    | RotateT record => #bbox record
    | NONE => raise Fail "don't allow none bbox_of_shape";

end;
