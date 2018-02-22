open Ppx_core

val read_expr :
  longident -> ?endianness:string -> masking:bool -> loc:location -> expression

val write_expr :
  longident -> ?endianness:string -> masking:bool -> loc:location -> expression

val byte_size_expr : longident -> loc:location -> expression
