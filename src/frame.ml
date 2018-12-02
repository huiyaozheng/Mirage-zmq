type frame = {
    flag : char;
    size : int;
    body : bytes
}

(** TODO network bytes order *)
let size_to_bytes size = 
    if size > 255 then
        Bytes.make 1 (Char.chr size)
    else
        Bytes.init 8 (fun i -> Char.chr ((size land (255 lsl (i - 1) * 8)) lsr ((i - 1) * 8)))

let make_frame body ifMore ifCommand = 
    let f = ref 0 in
    let len = Bytes.length body in
        if ifMore then f := !f + 1;
        if ifCommand then f := !f + 4;
        if len > 255 then f := !f + 2;
        {flag = (Char.chr (!f)); size = len; body}

let to_bytes f =
    Bytes.concat Bytes.empty [Bytes.make 1 f.flag; size_to_bytes f.size; f.body]

let from_bytes bytes = {
    
}
