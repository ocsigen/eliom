
open Ocsigen_lib_base

module Client_value = struct

  type +'a t = {
    closure_id: int64;
    instance_id: int;
  }

  let closure_id { closure_id } = closure_id
  let instance_id { instance_id } = instance_id
end
