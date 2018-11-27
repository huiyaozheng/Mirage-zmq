
let connect (S: Mirage_stack_lwt.V4) port =
let handle_connection flow t = 
let rec read_and_print flow t = 
    S.TCPV4.read flow >>= function
      | Ok `Eof -> Logs.info (fun f -> f "Closing connection!");  Lwt.return_unit
      | Error e -> Logs.warn (fun f -> f "Error reading data from established connection: %a" S.TCPV4.pp_error e); Lwt.return_unit
      | Ok (`Data b) -> Logs.debug (fun f -> f "read: %d bytes:\n%s" (Cstruct.len b)  (buffer_to_string (Cstruct.to_bytes b))); 
                        let (new_t, action_list) = connection_fsm t (Cstruct.to_bytes b) in
                        let 
                              write flow new_t
and write flow t = 
    S.TCPV4.write flow (connection_fsm t (Cstruct.to_bytes b)) >>= function
      | Error _ -> (Logs.warn (fun f -> f "Error writing data to established connection."); Lwt.return_unit)
      | Ok () -> read_and_print flow 
        
in
      S.listen_tcpv4 s ~port (
        fun flow ->
          let dst, dst_port = S.TCPV4.dst flow in
            Logs.info (fun f -> f "new tcp connection from IP %s on port %d" (Ipaddr.V4.to_string dst) dst_port);
            handle_connection flow (Connection.new_connection)
      );
      S.listen s;
