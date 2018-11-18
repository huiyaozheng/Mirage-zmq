type message = {
    ifMore : bool;
    body : bytes
}

let to_frame m = 
    Frame.make_frame (Bytes.to_string m.body) m.ifMore true