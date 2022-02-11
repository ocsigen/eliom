type 'a request = 'a

let from_fun f = f ()
let from_val x = x
let force x = x
