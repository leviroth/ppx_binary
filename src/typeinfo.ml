open Ppx_core
open Ast_builder.Default
open Ast_helper

type basic_type_info = {module_name: string; byte_size: int}

let basic_types =
  String_dict.of_alist_exn
    [ ("uint8", {module_name= "Uint8"; byte_size= 1})
    ; ("uint16", {module_name= "Uint16"; byte_size= 2})
    ; ("uint24", {module_name= "Uint24"; byte_size= 3})
    ; ("uint32", {module_name= "Uint32"; byte_size= 4})
    ; ("uint40", {module_name= "Uint40"; byte_size= 5})
    ; ("uint48", {module_name= "Uint48"; byte_size= 6})
    ; ("uint56", {module_name= "Uint56"; byte_size= 7})
    ; ("uint64", {module_name= "Uint64"; byte_size= 8})
    ; ("uint128", {module_name= "Uint128"; byte_size= 16})
    ; ("int8", {module_name= "Int8"; byte_size= 1})
    ; ("int16", {module_name= "Int16"; byte_size= 2})
    ; ("int24", {module_name= "Int24"; byte_size= 3})
    ; ("int32", {module_name= "Int32"; byte_size= 4})
    ; ("int40", {module_name= "Int40"; byte_size= 5})
    ; ("int48", {module_name= "Int48"; byte_size= 6})
    ; ("int56", {module_name= "Int56"; byte_size= 7})
    ; ("int64", {module_name= "Int64"; byte_size= 8})
    ; ("int128", {module_name= "Int128"; byte_size= 16}) ]

let converter_name typ ~direction_string ~if_basic ~target_type =
  match typ with
  | Lident s ->
    begin match String_dict.find basic_types s with
      | Some {module_name} -> if_basic module_name
      | None ->
        match s with
        | "t" ->
          lident @@ Printf.sprintf "%s_%s" direction_string target_type
        | s ->
          lident @@ Printf.sprintf "%s_%s_%s" s direction_string target_type end
  | Ldot (t, s) ->
    let new_s =
      match s with
      | "t" -> Printf.sprintf "%s_%s" direction_string target_type
      | s -> Printf.sprintf "%s_%s_%s" s direction_string target_type
    in
    Ldot (t, new_s)
  | _ -> Location.raise_errorf "Lapply not implemented"

let bytes_converter typ ?endianness ~direction_string =
  let if_basic module_name =
    let endianness =
      match endianness with
      | Some e -> e
      | None -> Location.raise_errorf "Endianness required"
    in
    Ldot (lident module_name,
          Printf.sprintf "%s_bytes_%s_endian"
            direction_string
            endianness)
  in
  converter_name typ ~direction_string ~if_basic ~target_type:"bytes"

let int_converter typ ~direction_string =
  let if_basic module_name =
    Ldot (lident module_name, Printf.sprintf "%s_int" direction_string)
  in
  converter_name typ ~direction_string ~if_basic ~target_type:"int"

let read_expr typ ?endianness ~masking ~loc =
  let read_buf_fn =
    Exp.mk ~loc
    @@ Pexp_ident {txt=bytes_converter ~direction_string:"of" ?endianness typ; loc}
  in
  let read_buf_expr = [%expr [%e read_buf_fn] buf offset] in
  match masking with
  | false -> read_buf_expr
  | true ->
    let converter =
      let txt =
        int_converter ~direction_string:"to" typ
      in
      Exp.mk ~loc @@ Pexp_ident {txt; loc}
    in
    [%expr ([%e read_buf_expr] |> [%e converter])]

let write_expr typ ?endianness ~masking ~loc =
  let write_buf_fn =
    Exp.mk ~loc
    @@ Pexp_ident {txt=bytes_converter ~direction_string:"to" ?endianness typ; loc}
  in
    match masking with
    | false -> [%expr fun data -> [%e write_buf_fn] data buf offset]
    | true ->
      let converter =
        let txt =
          int_converter ~direction_string:"of" typ
        in
        Exp.mk ~loc @@ Pexp_ident {txt; loc}
      in
      [%expr (fun data ->
          let data = [%e converter] data in [%e write_buf_fn] data buf offset)]

let byte_size_expr typ ~loc =
  Exp.mk ~loc @@
  match typ with
  | Lident s -> (match String_dict.find basic_types s with
      | Some {byte_size} ->
        Pexp_constant (Pconst_integer (Int.to_string byte_size, None))
      | None ->
        let s =
          match s with
          | "t" -> "byte_size"
          | s -> Printf.sprintf "byte_size_%s" s
        in
        Pexp_ident (Located.lident ~loc s))
  | Ldot (t, s) ->
    let s =
      match s with
      | "t" -> "byte_size"
      | s -> Printf.sprintf "byte_size_%s" s
    in
    Pexp_ident ({txt=Ldot (t, s); loc})
  | _ -> Location.raise_errorf "Lapply not implemented"
