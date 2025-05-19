structure Type = struct
  type sphere = {center: Vec3.t, radius: real};
  datatype hit_record = NoHit|  Hit of{
    p: Vec3.t,
    normal: Vec3.t,
    t: real,
    front_face: bool
  };
  datatype shape = Hittable_listT of shape list
                 | SphereT of sphere;
  type hittable_list = shape list;
end;
