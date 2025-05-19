structure Interval = struct
  type t = real * real;

  fun size (i:t) =
  let 
    val (min,max) = i
  in
    max - min
  end;

  fun contains (i:t) (x:real) =
  let 
    val (min,max) = i
  in
    x >= min andalso x <= max
  end;

  fun surrounds (i:t) (x:real) =
  let 
    val (min,max) = i
  in
    min < x andalso x < max
  end;

  fun clamp ((min,max):t) (x:real) =
    if x < min then min else if x > max then max else x


end;
