module Hello_zmq = struct
  let start =
    let context = Zmq.Context.create () in
    let requester = Zmq.Socket.create context Zmq.Socket.req in
    let rec loop = function
      | 0 -> Zmq.Socket.close requester; Zmq.Context.terminate context
      | n -> Printf.printf "Sending request %d...\n" n;
             Zmq.Socket.send requester "Hello";
             let reply = Zmq.Socket.recv requester in
              Printf.printf "Received reply %d: [%s]\n" n reply;
              loop (n-1)
      in 
      print_endline "Connecting to hello world server...";
      Zmq.Socket.connect requester "tcp://localhost:5555";
      loop 10;
      Lwt.return_unit
end