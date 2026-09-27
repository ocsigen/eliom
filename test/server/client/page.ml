let receive v =
  snd (Marshal.from_string (Marshal.to_string (Eliom.Wrap.wrap v) []) 0 : _ * _)
