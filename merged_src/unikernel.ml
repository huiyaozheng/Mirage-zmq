open Lwt.Infix

module Main (S: Mirage_stack_lwt.V4) = struct
    let start s =
        let context = Mirage_zmq.Context.create_context () in
        let module Socket = Mirage_zmq.Socket_tcp (S) in
        let socket = Socket.create_socket context Mirage_zmq.REP in
            Socket.bind socket 5555 s
end
