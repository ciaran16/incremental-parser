type seconds = float

val read_data : string -> string

val time_of : (unit -> 'a) -> seconds * 'a

val time_of_unix : (unit -> 'a) -> seconds * 'a

val print_time : seconds -> unit

val print_time_of : (unit -> 'a) -> unit

val print_time_of_unix : (unit -> 'a) -> unit
