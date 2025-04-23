type info = {
  i_sess_info : Eliom_common.sess_info;
  i_subpath : string list;
  i_meth : Eliom_common.meth;
  i_get_params : (string * string) list;
  i_post_params : (string * string) list;
}

module A = struct
  type site_data = unit
  type info' = info
  type info = info'

  (* the suffix is the only thing we seem to need *)
  type params = string list option
  type result = Eliom_service.result

  let site_data _ = ()
  let sess_info_of_info { i_sess_info; _ } = i_sess_info
  let subpath_of_info { i_subpath; _ } = i_subpath
  let meth_of_info { i_meth; _ } = i_meth
  let make_params _ _ suffix _ = suffix

  let get_number_of_reloads =
    let count = ref 0 in
    fun () ->
      count := !count + 1;
      !count

  module Raw_table = Map.Make (struct
    type t = Eliom_common.meth

    let compare = compare
  end)

  type table_content =
    [ `Ptc of unit option * (params, result) Eliom_common.service list ]

  type service =
    ( table ref * Eliom_common.page_table_key,
      Eliom_common.na_key_serv )
    Eliom_lib.leftright

  and node = service list
  and table = table_content Raw_table.t

  module Table = struct
    type t = table

    let add { Eliom_common.key_meth; _ } p m = Raw_table.add key_meth (`Ptc p) m

    let find { Eliom_common.key_meth; _ } m =
      let (`Ptc v) = Raw_table.find key_meth m in
      v

    let empty () = Raw_table.empty
    let remove { Eliom_common.key_meth; _ } = Raw_table.remove key_meth
  end

  (* FIXME: dummy *)
  module Node = struct
    type t = unit

    let up _ = ()
    let remove _ = ()
  end

  module Container = struct
    type t = {
      mutable t_services :
        (int * int * Table.t Eliom_common.dircontent ref) list;
      mutable t_contains_timeout : bool;
      mutable t_na_services :
        (Eliom_common.na_key_serv, bool -> params -> result Lwt.t) Hashtbl.t;
    }

    let get { t_services; _ } = t_services
    let set_contains_timeout a b = a.t_contains_timeout <- b
    let set tables l = tables.t_services <- l
    let dlist_add ?sp:_ _tables _srv = ()
  end

  let handle_directory _ = Lwt.return Eliom_service.No_contents
end

include Eliom_route_base.Make (A)

let global_tables =
  A.Container.
    {
      t_services = [];
      t_contains_timeout = false;
      t_na_services = Hashtbl.create 256;
    }

let add_naservice k f { A.Container.t_na_services; _ } =
  Hashtbl.add t_na_services k f

let call_naservice k { A.Container.t_na_services; _ } =
  try (Hashtbl.find t_na_services k) true None
  with Not_found -> Lwt.fail Eliom_common.Eliom_404

let rec na_key_of_params ~get = function
  | (k, v) :: _ when k = Eliom_common.naservice_name ->
      Some (if get then Eliom_common.SNa_get_ v else Eliom_common.SNa_post_ v)
  | (k, v) :: _ when k = Eliom_common.naservice_num ->
      Some (if get then Eliom_common.SNa_get' v else Eliom_common.SNa_post' v)
  | _ :: l -> na_key_of_params ~get l
  | [] -> None

let rec remove_site_dir p p' =
  match (p, p') with
  | h :: t, h' :: t' when h = h' -> remove_site_dir t t'
  | [], t -> Some t
  | _ -> None

let call_service ({ i_get_params; i_post_params; i_subpath; _ } as info) =
  let info =
    match remove_site_dir (Eliom_request_info.get_site_dir ()) i_subpath with
    | Some i_subpath -> { info with i_subpath }
    | None -> info
  in
  match na_key_of_params ~get:true i_get_params with
  | Some k -> call_naservice k global_tables
  | None -> (
      match na_key_of_params ~get:false i_post_params with
      | Some k -> call_naservice k global_tables
      | None -> find_service 0. global_tables None () info)
