type t = {
    name : string;
    data : bytes
}

val to_frame : t -> Frame.frame