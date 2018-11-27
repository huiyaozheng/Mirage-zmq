type t = {
    name : string;
    data : bytes
}

let to_frame c = 
    Frame.make_frame (Bytes.concat Bytes.empty [Bytes.of_string c.name; c.data]) false true