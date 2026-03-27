type 'a request = {data : unit -> 'a; mark : 'a request Wrap.wrapper}
[@@warning "-69"]

let mark = Wrap.create_wrapper (fun l -> l.data ())
let from_fun data = {data; mark}
let from_val v = {data = (fun () -> v); mark}
let force v = v.data ()
