module Trie : sig
  type t

  val create : unit -> t

  val insert : t -> string -> unit

  val delete : t -> string -> unit

  val find : t -> string -> bool

  val is_empty : t -> bool

  val to_list : t -> string list
end = struct
  type node = {mutable count: int; mutable children: (char * node ref) list}

  type t = node

  let rec f list c =
    match list with
    | [] ->
        None
    | (cc, n) :: tl ->
        if c = cc then Some n else f tl c

  let rec sep list c accum =
    match list with
    | [] ->
        None
    | (cc, n) :: tl ->
        if c = cc then Some (accum, n, tl) else sep tl c ((cc, n) :: accum)

  let create () = {count= 0; children= []}

  let rec insert t entry =
    if entry = "" then t.count <- t.count + 1
    else
      let c = entry.[0] in
      let rec make_branch s =
        if s = "" then {count= 1; children= []}
        else
          { count= 0
          ; children=
              [ ( s.[0]
                , ref (make_branch (String.sub s 1 (String.length s - 1))) ) ]
          }
      in
      match f t.children c with
      | None ->
          t.children
          <- ( c
             , ref (make_branch (String.sub entry 1 (String.length entry - 1)))
             )
             :: t.children
      | Some ref_node ->
          insert !ref_node (String.sub entry 1 (String.length entry - 1))

  let delete t entry =
    let rec delete_rec t entry =
      if entry = "" then (
        if t.count > 0 then t.count <- t.count - 1 ;
        t.count = 0 && t.children = [] )
      else
        let c = entry.[0] in
        match sep t.children c [] with
        | None ->
            false
        | Some (front, ref_node, back) ->
            if
              delete_rec !ref_node
                (String.sub entry 1 (String.length entry - 1))
            then (
              t.children <- front @ back ;
              t.count = 0 && t.children = [] )
            else (
              t.children <- front @ ((c, ref_node) :: back) ;
              false )
    in
    if entry = "" then ( if t.count > 0 then t.count <- t.count - 1 )
    else
      let c = entry.[0] in
      match sep t.children c [] with
      | None ->
          ()
      | Some (front, ref_node, back) ->
          if
            delete_rec !ref_node (String.sub entry 1 (String.length entry - 1))
          then t.children <- front @ back
          else t.children <- front @ ((c, ref_node) :: back)

  let find t entry =
    if t.count > 0 then true
    else
      let rec find_rec t entry =
        if entry = "" || t.children = [] then true
        else
          let c = entry.[0] in
          match f t.children c with
          | None ->
              false
          | Some ref_node ->
              find_rec !ref_node (String.sub entry 1 (String.length entry - 1))
      in
      let c = entry.[0] in
      match f t.children c with
      | None ->
          false
      | Some ref_node ->
          find_rec !ref_node (String.sub entry 1 (String.length entry - 1))

    let is_empty t = 
      t.count = 0 && t.children = []

    let rec to_list t = 
      let rec add s n accum =
        if n = 0 then accum else
        add s (n - 1) (s::accum)
      in
      (add "" t.count []) @
      List.flatten (List.map (fun (c, ref_node) -> List.map (fun s -> String.make 1 c ^ s) (to_list (!ref_node)) ) t.children)
end

let sort list = List.sort (String.compare) list

let trie = Trie.create ()

let _ = Trie.insert trie ""

let _ = assert(not (Trie.is_empty trie))

let _ = assert (Trie.find trie "ABC")

let _ = assert (Trie.find trie "A")

let _ = assert (sort (Trie.to_list trie) = [""])

let _ = Trie.delete trie ""

let _ = assert (sort (Trie.to_list trie) = [])

let _ = assert(Trie.is_empty trie)

let _ = assert (not (Trie.find trie "ABC"))

let _ = Trie.insert trie "A"

let _ = assert (sort (Trie.to_list trie) = ["A"])

let _ = Trie.insert trie "A"

let _ = assert (sort (Trie.to_list trie) = ["A"; "A"])

let _ = assert(not (Trie.is_empty trie))

let _ = assert (Trie.find trie "ABC")


let _ = assert (Trie.find trie "A")

let _ = assert (Trie.find trie "AB")

let _ = Trie.delete trie "A"

let _ = assert (sort (Trie.to_list trie) = ["A"])

let _ = assert(not (Trie.is_empty trie))

let _ = assert (Trie.find trie "ABC")

let _ = assert (Trie.find trie "AB")

let _ = Trie.delete trie "A"

let _ = assert (sort (Trie.to_list trie) = [])

let _ = assert(Trie.is_empty trie)

let _ = assert (not (Trie.find trie "ABC"))

let _ = assert (not (Trie.find trie "AB"))

let _ = assert (not (Trie.find trie "A"))

let _ = Trie.insert trie "AB"

let _ = assert (sort (Trie.to_list trie) = ["AB"])

let _ = assert(not (Trie.is_empty trie))

let _ = assert (Trie.find trie "ABC")

let _ = Trie.insert trie "AD"

let _ = assert (sort (Trie.to_list trie) = ["AB"; "AD"])

let _ = assert(not (Trie.is_empty trie))

let _ = assert (Trie.find trie "ABC")

let _ = assert (Trie.find trie "ADCC")

let _ = assert (not (Trie.find trie "AEA"))

let _ = Trie.delete trie "AB"

let _ = assert (sort (Trie.to_list trie) = ["AD"])

let _ = assert(not (Trie.is_empty trie))

let _ = assert (not (Trie.find trie "ABC"))

let _ = assert (Trie.find trie "ADCC")

let _ = Trie.insert trie "ABD"

let _ = assert (sort (Trie.to_list trie) = ["ABD"; "AD"])

let _ = Trie.delete trie "ABD"

let _ = assert (sort (Trie.to_list trie) = ["AD"])

let _ = assert (Trie.find trie "AD")

let _ = assert(not (Trie.is_empty trie))
