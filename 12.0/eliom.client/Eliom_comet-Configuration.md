
# Module `Eliom_comet.Configuration`

Change the reactivity of channels. Multiples configurations ( of type `t` ) can be created. The resulting behaviour is the minimal ( in the meaning of maximal reactivity ) between all configurations

```ocaml
type t
```
```ocaml
val new_configuration : unit -> t
```
Creates a new configuration with default value. It modifies the current behaviour immediately

```ocaml
val drop_configuration : t -> unit
```
`drop_configuration t` restores the behaviour to the minimum of configuration without `t`. If there is no other configuration than `t`, it is restored to the defaults.

```ocaml
val set_always_active : t -> bool -> unit
```
`set_always_active c b` if b is true, tells the client to always stay active. Default value is false.

```ocaml
val set_timeout : t -> float -> unit
```
`set_timeout c t` tells the client to stay active at least `t` seconds when the application lose the focus. Default value is 180\.

```ocaml
val set_active_until_timeout : t -> bool -> unit
```
`set_active_until_timeout c v` sets the activity changing behaviour. if `v` is `true` the page is kept active even if not focused until the client receive a timeout message from the server. It implies that if the server keeps sending data to the client, the comet connection will never be closed. Default value is false.

```ocaml
val set_time_between_requests : t -> float -> unit
```
after `set_time_between_requests t v`, the main loop will wait for `v` seconds between two requests. It is taken into account immediately. Default value is 0\.

```ocaml
val set_time_between_requests_when_idle : t -> (float * float * float) -> unit
```
`set_time_between_requests_when_idle t (a, b, c)` sets the time between two requests when the the windows does not have the focus, after the timeout. This amount of time is computed using an affine function (a \* T \+ b), where T is the amount of time elapsed since the beginning of the idle phase, with a maximal time of c. If you want no request at all, do `set_always_active false`. Setting this to `(0., 0., 0.)` is equivalent to `set_always_active true`. Default value is `(0.5, 60., 600.)`.
