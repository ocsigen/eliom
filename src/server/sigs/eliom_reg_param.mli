type page
type options
type return
type result

val send :
  ?options:options ->
  ?charset:string ->
  ?code: int ->
  ?content_type:string ->
  ?headers: Http_headers.t ->
  page ->
  Ocsigen_http_frame.result Lwt.t

val send_appl_content : Eliom_service.send_appl_content
  (** Whether the service is capable to send application content when
      required. This field is usually [Eliom_service.XNever]. This
      value is recorded inside each service just after
      registration.  *)

val result_of_http_result : Ocsigen_http_frame.result -> result
