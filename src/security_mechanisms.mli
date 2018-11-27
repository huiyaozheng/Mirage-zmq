module type mechanism = sig
    val name : string
    type state
    type action
    val handle : state -> Command.t -> state * action
end