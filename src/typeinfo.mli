open Ppx_core

val reader_name : ?endianness:string -> longident -> longident

val writer_name : ?endianness:string -> longident -> longident

val size_expr : loc:Location.t -> longident -> Parsetree.expression
