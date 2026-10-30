type%shared t = A | B of int

let%server x = 1
let%server f () = [%client ~%x + 1]
let%server at_load = [%client ~%x + 2]
let%client injected = ~%x + 3
let%shared both = ~%x + 4
let%shared g y = [%client ~%y + 5]
let%shared at_load_shared = [%client ~%x + 6]

module%server M = struct
  let v = [%client 7]
end

module%shared N = struct
  let w = [%client 8]
  let i = ~%x
end
