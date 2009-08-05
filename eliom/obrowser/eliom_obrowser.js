caml_closure_table = [] ;

function caml_run_from_table (vm, id, marg) {
    if (caml_closure_table [id] == null) {
	vm.failwith ("unbound closure");
    }
    caml_closure_arg = input_val (marg);
    vm.thread_new (caml_closure_table [id]);
    vm.run ();
}

RT.caml_register_closure = function (id, clos) {
    caml_closure_table[id] = clos;
    return 0;
}

RT.caml_get_closure_arg = function (unit) {
    return caml_closure_arg;
}
