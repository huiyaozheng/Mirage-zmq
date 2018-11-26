

let connect (S: Mirage_stack_lwt.V4) connection port =
let rec read_and_print flow = 
    S.TCPV4.read flow >>= function
      | Ok `Eof -> Logs.info (fun f -> f "Closing connection!");  Lwt.return_unit
      | Error e -> Logs.warn (fun f -> f "Error reading data from established connection: %a" S.TCPV4.pp_error e); Lwt.return_unit
      | Ok (`Data b) -> Logs.debug (fun f -> f "read: %d bytes:\n%s" (Cstruct.len b)  (buffer_to_string (Cstruct.to_bytes b))); write flow
and write flow =
    S.TCPV4.write flow >>= function
        | Ok () -> read_and_print flow
        | Error _ -> (Logs.warn (fun f -> f "Error writing data to established connection."); Lwt.return_unit)
in
      S.listen_tcpv4 s ~port (
        fun flow ->
          let dst, dst_port = S.TCPV4.dst flow in
            Logs.info (fun f -> f "new tcp connection from IP %s on port %d" (Ipaddr.V4.to_string dst) dst_port);
            read_and_print flow
      );
      S.listen s;
