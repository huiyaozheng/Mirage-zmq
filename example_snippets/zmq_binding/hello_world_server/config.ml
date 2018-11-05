open Mirage


let main =
  foreign
    "Unikernel.Hello_zmq"  job

let () =  
  let packages = [
    package "Zmq"
  ]
  in
  register "hello_server" ~packages [main]
