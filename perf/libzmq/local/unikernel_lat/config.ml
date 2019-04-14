open Mirage

let main = foreign ~packages:[package "mirage-zmq"] "Local_lat.Main" (stackv4 @-> job)

let stack = generic_stackv4 default_network

let () =
  register "local_lat"  [
    main $ stack
  ]
