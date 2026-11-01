# Client-server programming

An Eliom application runs partly on the server and partly in the browser or in a mobile app. It is written as one OCaml program, and a syntax extension says where each piece of code runs.

- [Eliom applications](./applications.md): the general principles.
- [Multi-tier programming](./multitier.md): the syntax extension, with shared sections, client fragments and injections.
- [Generating HTML](./html.md): typed HTML built on the server or on the client, elements kept by reference, and reactive pages.
- [Communication](./communication.md): remote procedure calls, notifications from the server, and the other ways to exchange values.
- [Client-side services](./services.md): services whose handlers run on the client.
- [Shared reactive programming](./react.md): reactive values defined on the server and used on the client.
- [Caching data and off-line applications](./cache.md): a cache of data for the client, and applications that work off-line.
- [Custom wrapping](./wrapping.md): how server values are sent to the client; useful when working on Eliom itself.
