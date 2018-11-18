type command = {
    name : bytes;
    data : bytes
}

val to_frame : command -> Frame.frame