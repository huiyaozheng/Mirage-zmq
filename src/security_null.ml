type property = string * string

type metadata = List of property

let ready metadata =
    Command.({name = Bytes.concat Bytes.empty [Char.chr 213; "READY"]; data = metadata}) 

let mechanism = "NULL"