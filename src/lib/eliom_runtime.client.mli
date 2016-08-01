
(** Look-up of the value of an injection in the global injection table. *)
val get_injection : ?ident:string -> ?pos:Eliom_lib.pos -> string -> 'a

(** Register a function from the tuple of injected values (['args])
    to the actual code of the client value (['res]) under some
    closure ID *)
val register_client_closure : closure_id:string -> ('args -> 'res) -> unit

(** Takes the next list of {!Eliom_lib_base.client_value_datum}s
    from the queue of server section data of the compilation unit
    provided by the first argument
    (cf. {!Eliom_lib_base.compilation_unit_global_data}). It
    initializes and registers the global client values created in
    that section.

    Called in parallel with <<a_api subproject="server"|val
    Eliom_service.Syntax_helpers.close_server_section>>. *)
val close_server_section : string -> unit

(** Takes the next list of {!Eliom_lib_base.injection_datum}s from
    the queue of client section data of the compilation unit
    specfied with the argument
    (cf. {!Eliom_lib_base.compilation_unit_global_data}). It
    registers those injections for subsequent usage of
    {!Eliom_client.Syntax_helpers.get_injection}.

    Called in parallel with <<a_api subproject="server"|val
    Eliom_service.Syntax_helpers.close_client_section>>. *)
val open_client_section : string -> unit

val get_escaped_value : Eliom_lib.poly -> 'a
