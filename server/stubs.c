#include <caml/mlvalues.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>


CAMLprim value disable_nagle(value fd)
{

  int i = 1;

  setsockopt(Int_val(fd), IPPROTO_TCP, TCP_NODELAY,
             &i, sizeof(int));

  return Val_int(0);

}

