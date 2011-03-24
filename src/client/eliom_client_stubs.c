#include <stdlib.h>
#define D(f) void f () { exit(1); }
D(caml_register_closure)
D(caml_regexp_make)
D(caml_regexp_split)
D(caml_regexp_last_index)
D(caml_regexp_test)
D(caml_regexp_exec)
D(caml_regexp_index)
D(caml_regexp_replace)
D(caml_regexp_replace_fun)
