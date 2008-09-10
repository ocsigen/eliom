#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <grp.h>

CAMLprim value disable_nagle(value fd)
{
  CAMLparam1(fd);
  int i = 1;

  setsockopt(Int_val(fd), IPPROTO_TCP, TCP_NODELAY,
             &i, sizeof(int));

  CAMLreturn(Val_int(0));
}

CAMLprim value initgroups_stub(value user, value group)
{
  CAMLparam2(user, group);

#ifdef _BSD_SOURCE
  if (initgroups(String_val(user), (gid_t)Int_val(group)) == -1) {
    caml_failwith("initgroups");
  } else {
    CAMLreturn(Val_unit);
  }
#else
  CAMLreturn(Val_unit);
#endif
}
