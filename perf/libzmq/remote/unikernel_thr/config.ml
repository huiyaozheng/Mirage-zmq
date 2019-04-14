open Mirage

let main = foreign ~packages:[package "mirage-zmq"] "Remote_thr.Main" (stackv4 @-> job)

let stack = generic_stackv4 default_network

let () =
  register "remote_thr"  [
    main $ stack
  ]
