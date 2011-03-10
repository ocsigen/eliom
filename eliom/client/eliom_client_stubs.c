#include <stdlib.h>
#define D(f) void f () { exit(1); }
D(caml_register_closure)
D(caml_regexp_make)
D(caml_regexp_split)
D(caml_get_obj_table_uuid)
