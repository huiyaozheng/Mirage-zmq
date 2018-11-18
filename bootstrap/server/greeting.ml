type version = {
    major : bytes;
    minor : bytes
}

type greeting = { 
    signature  : bytes;
    version    : version;
    mechanism  : string;
    filler     : bytes
}

let signature = 
    let s = Bytes.make 10 (Char.chr 0) in
        Bytes.set s 0 (Char.chr 255);
        Bytes.set s 9 (Char.chr 127);
        s

let version = {major = Bytes.make 1 (Char.chr 3); minor = Bytes.make 1 (Char.chr 0)}

let filler = Bytes.make 31 (Char.chr 0)

let pad_mechanism m =
    let b = Bytes.of_string m in
        if Bytes.length b < 20 then
            Bytes.cat b (Bytes.make (20 - Bytes.length b) (Char.chr 0))
        else
            b

let to_bytes g = 
    Bytes.concat Bytes.empty [g.signature; g.version.major; g.version.minor; pad_mechanism g.mechanism; g.filler]

let new_greeting mechanism = {signature; version; mechanism; filler}

(** TODO: check validity of the greeting*)
let decode_greeting byte_sequence = 
    let mechanism = Bytes.sub_string byte_sequence 12 20 in 
        {signature; version; mechanism; filler}
