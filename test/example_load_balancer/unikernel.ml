open Lwt.Infix
open Mirage_zmq

module Main (S : Mirage_stack_lwt.V4) = struct
  let start s =
    let context = Context.create_context () in
    let module Socket = Socket_tcp (S) in
    let item_queue = Queue.create () in
    (* Accepting tasks from PUSH sources *)
    let front_end () =
      let socket = Socket.create_socket context PULL in
      Socket.bind socket 5556 s ;
      let rec read_and_enqueue () =
        Socket.recv socket
        >>= function
        | Data msg ->
            Queue.push msg item_queue ;
            Lwt.pause () >>= fun () -> read_and_enqueue ()
        | _ ->
            (* The input should never match this pattern *)
            Lwt.return_unit
      in
      read_and_enqueue ()
    in
    (* Dispatching tasks to PULL workers *)
    let back_end () =
      let socket = Socket.create_socket context PUSH in
      Socket.bind socket 5557 s ;
      let rec send_task () =
        if Queue.is_empty item_queue then
          Lwt.pause () >>= fun () -> send_task ()
        else
          let task = Queue.pop item_queue in
          Socket.send socket (Data task) >>= fun () -> send_task ()
      in
      send_task ()
    in
    Lwt.join [front_end (); back_end ()]
end
