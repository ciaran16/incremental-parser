module Json : sig
  type spec

  type list_type = int -> ?upto:int -> spec -> spec

  val primitive : spec

  val array : list_type

  val obj : list_type

  val array_or_obj : list_type

  val randomly_balanced : ?list:list_type -> ?min_branch:int -> max_branch:int ->
    ?min_depth:int -> max_depth:int -> spec -> spec

  val perfectly_balanced : ?list:list_type -> branch:int -> depth:int -> spec -> spec

  val lock : spec -> spec

  val tokens : spec -> Json_lexer.token array

  val raw : spec -> string

  val raw_of_tokens : Json_lexer.token array -> string
end
