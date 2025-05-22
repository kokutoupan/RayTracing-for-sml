structure Common = struct

  fun realMin (a: real) (b: real) = if a < b then a else b
  fun degrees_to_radians degrees=
    degrees * 0.017453292519943295769236907684886
end;
