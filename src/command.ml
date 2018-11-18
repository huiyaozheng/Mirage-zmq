type command = {
    name : bytes;
    data : bytes
}

let to_frame c = 
    Frame.make_frame (Bytes.to_string (Bytes.concat Bytes.empty [c.name; c.data])) false true