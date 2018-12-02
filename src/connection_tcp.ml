open Mirage_stack_lwt
open Lwt
module Connection_tcp (S: Mirage_stack_lwt.V4) (C: Connection.Connection) = struct
      let buffer_to_string data = 
          let content = ref [] in
            Bytes.iter (fun b -> content := Char.code b :: !content) data;
            String.concat " "  (List.map (fun x -> string_of_int x)  (List.rev !content))

      let connect s port =
      let rec read_and_print flow t = 
          S.TCPV4.read flow >>= (function
            | Ok `Eof -> Logs.info (fun f -> f "Closing connection!");  Lwt.return_unit
            | Error e -> Logs.warn (fun f -> f "Error reading data from established connection: %a" S.TCPV4.pp_error e); Lwt.return_unit
            | Ok (`Data b) -> (Logs.debug (fun f -> f "read: %d bytes:\n%s" (Cstruct.len b)  (buffer_to_string (Cstruct.to_bytes b))); 
                              let (new_t, action_list) = C.connection_fsm t (Cstruct.to_bytes b) in
                              let rec act actions = 
                                    (match actions with
                                      | [] -> read_and_print flow new_t
                                      | (hd::tl) -> 
                                      (match hd with
                                          | C.Write(b) -> (S.TCPV4.write flow (Cstruct.of_bytes b) >>= function
                                                            | Error _ -> (Logs.warn (fun f -> f "Error writing data to established connection."); Lwt.return_unit)
                                                            | Ok () -> act tl)
                                          | C.Continue -> act tl
                                          | C.Close -> Lwt.return_unit))
                              in
                                    act action_list))
      in
            S.listen_tcpv4 s ~port (
              fun flow ->
                let dst, dst_port = S.TCPV4.dst flow in
                  Logs.info (fun f -> f "new tcp connection from IP %s on port %d" (Ipaddr.V4.to_string dst) dst_port);
                  read_and_print flow (C.new_connection ())
            );
            S.listen s
end