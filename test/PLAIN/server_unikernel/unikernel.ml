open Lwt.Infix
open Mirage_zmq
module Main (S: Mirage_stack_lwt.V4) = struct
    let start s =
        let context = Context.create_context () in
        let module Socket = Socket_tcp (S) in
        let socket = Socket.create_socket context ~mechanism:PLAIN REP in
            Socket.set_plain_user_list socket [("admin", "password")];
            Socket.bind socket 5555 s;
            let rec read_and_print () =
                Logs.info (fun f -> f "Started socket\n"); 
                Socket.recv socket >>= fun msg -> match msg with
                | Data(msg) -> Logs.info (fun f -> f "Received msg: %s\n" msg); 
                        Socket.send socket (Data("Authenticated")) >>= fun () ->
                        read_and_print ();
                | _ -> Logs.info (fun f -> f "Unexpected msg received\n");  Lwt.return_unit
            in  read_and_print ()
end
