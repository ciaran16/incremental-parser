type 'tok lex_result

val token : 'tok -> 'tok lex_result

val error : ?token:'tok -> string -> 'tok lex_result
