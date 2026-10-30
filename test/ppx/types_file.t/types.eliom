let%server x = 1
let%client y = ~%x
let%server length l = [%client List.length ~%l]
let%server missing () = [%client 0]
