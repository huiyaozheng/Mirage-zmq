type stage = 
    | GREETING
    | HANDSHAKE
    | TRAFFIC
    | ERROR

type t = {
    stage : stage;
    counter : int;
    greeting_state : Greeting.state;
    security_policy : string
}

let new_connection = {
    mutable stage = GREETING;
    mutable as_server = false;
    counter = 0;
    greeting_state = START;
    (* Set custom policy *)
    mutable security_policy = "NULL"
}

let connection_fsm t bytes = 
    match (t.stage) with
        | GREETING -> (let len = Bytes.length bytes in
                       let convert greeting_action_list =
                            match greeting_action_list with
                                | [] ->
                                | hd::tl ->
                                    match hd with 
                                        | Send_bytes(b) -> (WRITE(b)::(convert tl))
                                        | Set_server(b) -> t.as_server <- b; convert tl
                                        | Set_mechanism(s) -> t.security_policy <- s; convert tl
                                        | Continue -> convert tl
                                        | Ok -> t.stage <- HANDSHAKE; convert tl
                                        | Error(s) -> [CLOSE]
                        (* Hard code the length here. The greeting is either complete or split into 11 + 53 or 10 + 54*)
                        match len with
                            (* Full greeting *)
                            | 64 -> let (state, action_list) = Greeting.handle_list (t.greeting_state, 
                                        [Recv_sig(Bytes.sub b 0 10); 
                                         Recv_Vmajor(Bytes.sub b 10 1);
                                         Recv_Vminor(Bytes.sub b 11 1);
                                         Recv_Mechanism(Bytes.sub b 12 20);
                                         Recv_as_server(Bytes.sub b 32 1);
                                         Recv_filler(Bytes.sub b 33 31)   
                                        ]) [] in
                                    let connection_action = convert action in 
                                    ({t with greeting_state = state}, convert action)
                            (* Signature + version major *)
                            | 11 -> let (state, action_list) = Greeting.handle_list (t.greeting_state, [Recv_sig(Bytes.sub b 0 10); Recv_Vmajor(Bytes.sub b 10 1)]) [] in
                                    ({t with greeting_state = state}, convert action)
                            (* Signature *)
                            | 10 -> let (state, action_list) = Greeting.handle (t.greeting_state, Recv_sig(b)) in
                                    ({t with greeting_state = state}, convert action)
                            (* version minor + rest *)
                            | 53 -> let (state, action_list) = Greeting.handle_list (t.greeting_state, 
                                        [Recv_Vminor(Bytes.sub b 0 1);
                                         Recv_Mechanism(Bytes.sub b 1 20);
                                         Recv_as_server(Bytes.sub b 21 1);
                                         Recv_filler(Bytes.sub b 22 31)   
                                        ]) [] in
                                    let connection_action = convert action in 
                                    ({t with greeting_state = state}, convert action)
                            (* version major + rest *)
                            | 54 -> let (state, action_list) = Greeting.handle_list (t.greeting_state, 
                                        [Recv_Major(Bytes.sub 0 1)
                                         Recv_Vminor(Bytes.sub b 1 1);
                                         Recv_Mechanism(Bytes.sub b 2 20);
                                         Recv_as_server(Bytes.sub b 22 1);
                                         Recv_filler(Bytes.sub b 23 31)   
                                        ]) [] in
                                    let connection_action = convert action in 
                                    ({t with greeting_state = state}, convert action)
                            | _ ->  {ERROR, [CLOSE]}
                    )
        | HANDSHAKE -> (t, [])
        | TRAFFIC -> (t, [])
        | ERROR -> (t, [])
    
module type Connection = sig
    type t
    type stage
    type action =
    | Write of bytes
    | Continue
    | Close
    val connection_fsm : t -> Bytes.t -> t * action list
    val new_connection : unit -> t
end