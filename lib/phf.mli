type elt
type t
type query
type answer

val of_seq : elt Seq.t -> t
val to_iter : t -> elt Seq.t
val search : t -> query -> answer
val combine : answer -> answer -> answer
