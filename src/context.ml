type socket = 
    | REQ
    | REP
    | DEALER
    | ROUTER

type t =  List of socket

let new_context = []


