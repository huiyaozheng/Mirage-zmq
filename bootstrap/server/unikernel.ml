open Lwt.Infix
open Greeting

module Main (S: Mirage_stack_lwt.V4) = struct
  let buffer_to_string data = 
    let content = ref [] in
      Bytes.iter (fun b -> content := Char.code b :: !content) data;
      String.concat " "  (List.map (fun x -> string_of_int x)  (List.rev !content))
  let greeting = Greeting.new_greeting "NULL" |> Greeting.to_bytes
  let rec read_and_print flow = 
    S.TCPV4.read flow >>= function
      | Ok `Eof -> Logs.info (fun f -> f "Closing connection!");  Lwt.return_unit
      | Error e -> Logs.warn (fun f -> f "Error reading data from established connection: %a" S.TCPV4.pp_error e); Lwt.return_unit
      | Ok (`Data b) -> Logs.debug (fun f -> f "read: %d bytes:\n%s" (Cstruct.len b)  (buffer_to_string (Cstruct.to_bytes b))); read_and_print flow
  let start s =
    let port = Key_gen.port () in
      S.listen_tcpv4 s ~port (
        fun flow ->
          let dst, dst_port = S.TCPV4.dst flow in
            Logs.info (fun f -> f "new tcp connection from IP %s on port %d" (Ipaddr.V4.to_string dst) dst_port);
            Logs.info (fun f -> f "sending greeting: %s" (buffer_to_string greeting));
            S.TCPV4.write flow (Cstruct.of_bytes greeting ) >>= function 
            | Ok () -> read_and_print flow
            | Error _ -> (Logs.warn (fun f -> f "Error writing data to established connection."); Lwt.return_unit)
      );
      S.listen s
end
