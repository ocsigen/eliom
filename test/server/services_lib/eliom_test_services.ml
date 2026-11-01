open Eliom

let text s = Lwt.return (s, "text/plain")

let get ?priority path params =
  Service.create ?priority ~path:(Service.Path path) ~meth:(Service.Get params)
    ()

let string ?options ?code ?headers service f =
  Registration.String.register ?options ?code ?headers ~service f
