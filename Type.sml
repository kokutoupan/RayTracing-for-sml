structure Type = struct
  datatype hit_record = NoHit|  Hit of{
    p: Vec3.t,
    normal: Vec3.t,
    t: real,
    front_face: bool,
    mat: material
  }
  and material = LambertianT of {albedo: Color.t}
              |  MetalT of {albedo: Color.t}

  type sphere = {center: Vec3.t, radius: real, mat:material};

  datatype shape = Hittable_listT of shape list
                 | SphereT of sphere;


  type hittable_list = shape list;
end;
