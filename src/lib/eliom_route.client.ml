module A= struct

  type site_data = unit

  type info = {
    i_sess_info : Eliom_common.sess_info;
    i_subpath   : string list;
    i_meth      : Eliom_common.meth
  }

  type result = unit

  let site_data _ = ()

  let sess_info_of_info {i_sess_info} = i_sess_info

  let subpath_of_info {i_subpath} = i_subpath

  let meth_of_info {i_meth} = i_meth

  let make_server_params _ _ _ _ = ()

  let get_number_of_reloads () = 0

  module Raw_table = Map.Make(struct
      type t = Eliom_common.page_table_key
      let compare = compare
    end)

  type table_content =
    [`Ptc of
       node option *
       ((Eliom_common.anon_params_type * Eliom_common.anon_params_type) *
        (int ref option * (float * float ref) option *
         (bool -> Eliom_common.server_params -> result Lwt.t)))
         list
    ]

  and service = (
    table ref * Eliom_common.page_table_key,
    Eliom_common.na_key_serv
  ) Eliom_lib.leftright

  and node = service list

  and table = table_content Raw_table.t

  module Table = struct

    type t = table

    let empty () = Raw_table.empty

    let add k v m = Raw_table.add k (`Ptc v) m

    let find k m =
      let `Ptc v = Raw_table.find k m in v

    let empty () = Raw_table.empty

    let remove = Raw_table.remove

  end

  (* FIXME: dummy *)
  module Node = struct

    type t = node

    let up n = ()

    let remove n = ()

  end

  type tables = {
    mutable t_services         :
      (int * int * Table.t Eliom_common.dircontent ref) list;
    mutable t_na_services      : Node.t;
    mutable t_contains_timeout : bool
  }

  let tables_services {t_services} = t_services

  let set_contains_timeout a b =
    a.t_contains_timeout <- b

  let set_tables_services tables l =
    tables.t_services <- l

  let service_dlist_add ?sp:_ tables srv =
    let l = srv :: tables.t_na_services in
    tables.t_na_services <- l;
    l

  let handle_directory _ = Lwt.return ()

end

include Eliom_route_base.Make(A)

let global_tables = {
  A.t_services         = [];
  A.t_na_services      = [];
  A.t_contains_timeout = false
}
