#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <assert.h>

CAMLprim value caml_infix_to_closure(value v) {
  CAMLparam1(v);
  assert (Tag_val(v) == Infix_tag);
  CAMLreturn(v - Infix_offset_val(v));
}

CAMLprim value caml_infix_of_closure(value closure, value infix) {
  CAMLparam2(closure,infix);
  assert (Tag_val(infix) == Infix_tag);
  assert (Tag_val(closure) == Closure_tag);
  CAMLreturn(closure + Infix_offset_val(infix));
}

