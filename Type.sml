structure Type = struct
  datatype hit_record = NoHit|  Hit of{
    p: Vec3.t,
    normal: Vec3.t,
    t: real,
    front_face: bool,
    mat: material
  }
  and material = LambertianT of {albedo: Color.t}
               | MetalT of {albedo: Color.t,fuzz: real}
               | DielectricT of {ref_idx: real}

  type sphere = {center: Vec3.t, radius: real, mat:material, bbox:AABB.t};

  datatype shape = Hittable_listT of {lst:shape list,bbox:AABB.t}
                 | SphereT of sphere
                 | H_bvhT of {lhs:shape,rhs:shape,bbox:AABB.t}
                 | NONE; (*dumy*)

  type hittable_list = {lst:shape list,bbox:AABB.t};
  type h_bvh = {lhs:shape,rhs:shape,bbox:AABB.t};


  (* shape から bbox を抽出するヘルパー関数 *)
  fun bbox_of_shape (s: shape) : AABB.t =
    case s of
      Hittable_listT record => #bbox record  
    | SphereT sphere_record => #bbox sphere_record 
    | H_bvhT record => #bbox record
    | _ => raise Fail "don't allow none bbox_of_shape";

  fun get_hittable_list_payload (shape_value : shape) : hittable_list =
  case shape_value of
    Hittable_listT record_payload => record_payload
  | _ => raise Fail "unsafe_get_hittable_list_payload: shape was not a Hittable_listT";

end;
