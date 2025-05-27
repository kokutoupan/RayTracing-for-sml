structure Interval = struct
  type t = real * real;

  fun create (min:t) (max:t) =
    (min,max);

  fun createIN ((a0,a1):t) ((b0,b1):t) =
    (Common.min a0 b0, Common.max a1 b1);

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


  fun print_interval ((a0,a1):t) =
  let 
    val _ = print(Int.toString (Real.toInt IEEEReal.TO_NEAREST (a0*100.0)) ^","
    ^Int.toString (Real.toInt IEEEReal.TO_NEAREST (a1*100.0))^  " | ")
  in
    ()
  end

end;
