open Ppx_core
open Ast_builder.Default
open Ast_helper
open Ppx_type_conv.Std

let ( @@ ) = Caml.( @@ )

let endianness =
  Attribute.declare "binary.endianness" Attribute.Context.label_declaration
    Ast_pattern.(pstr (pstr_eval (pexp_ident (lident __)) nil ^:: nil))
    (fun x -> x )


let lident_loc_of_string_loc x = {x with txt= Lident x.txt}

type typeinfo = {module_name: string; byte_size: int}

let type_to_module =
  Map.of_alist_exn
    (module String)
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


module Gen_str = struct
  let generate ~loc ~path:_ (rec_flag, tds) default_endianness =
    let get_default_endianness ~loc =
      match default_endianness with
      | Some ("little" | "big" as s) -> s
      | Some _ ->
          Location.raise_errorf ~loc "Default endianness must be little or big"
      | None -> Location.raise_errorf ~loc "Default endianness required"
    in
    let extract_module_info = function
      | {ptyp_desc= Ptyp_constr ({txt= Lident s}, [])} ->
          Map.find_exn type_to_module s
      | {ptyp_loc} -> Location.raise_errorf ~loc:ptyp_loc "Expected basic type"
    in
    let get_type_byte_width t = (extract_module_info t).byte_size in
    let read_or_write_expression ld offset direction =
      let loc = ld.pld_loc in
      let module_info = extract_module_info ld.pld_type in
      let endianness =
        match Attribute.get endianness ld with
        | Some ("little" | "big" as s) -> s
        | Some _ ->
            Location.raise_errorf ~loc
              "Default endianness must be little or big"
        | None -> get_default_endianness ~loc
      in
      match direction with
      | `Read ->
          let fn =
            evar ~loc
            @@ Printf.sprintf "%s.of_bytes_%s_endian" module_info.module_name
                 endianness
          in
          [%expr [%e fn] buf (offset + [%e offset])]
      | `Write ->
          let fn =
            evar ~loc
            @@ Printf.sprintf "%s.to_bytes_%s_endian" module_info.module_name
                 endianness
          in
          let data_field =
            pexp_field ~loc [%expr data]
            @@ lident_loc_of_string_loc ld.pld_name
          in
          [%expr [%e fn] [%e data_field] buf (offset + [%e offset])]
    in
    let build_exprs l direction =
      let rec aux l offset acc =
        match l with
        | [] -> List.rev acc
        | hd :: tl ->
            let offset_expr =
              pexp_constant ~loc:hd.pld_loc
              @@ Pconst_integer (Int.to_string offset, None)
            in
            aux tl
              (offset + get_type_byte_width hd.pld_type)
              (read_or_write_expression hd offset_expr direction :: acc)
      in
      aux l 0 []
    in
    let build_field_assignments : label_declaration list -> value_binding list =
      fun l ->
        let pats = List.map l ~f:(fun ld -> ppat_var ~loc ld.pld_name) in
        let exprs = build_exprs l `Read in
        List.map2_exn pats exprs ~f:(fun pat expr ->
            value_binding ~loc ~pat ~expr )
    in
    let writer_fn
        : label_declaration list -> string Asttypes.loc -> Location.t
          -> structure_item =
      fun lds type_name loc ->
        let fn_name =
          { type_name with
            txt=
              ( match type_name.txt with
              | "t" -> "to_bytes"
              | s -> s ^ "_to_bytes" ) }
        in
        let reads = build_exprs lds `Write in
        let expr = esequence loc reads in
        let expr = [%expr fun data buf offset -> [%e expr]] in
        let binding =
          { pvb_pat= ppat_var ~loc fn_name
          ; pvb_expr= expr
          ; pvb_attributes= []
          ; pvb_loc= loc }
        in
        {pstr_desc= Pstr_value (Nonrecursive, [binding]); pstr_loc= loc}
    in
    let stitch l final loc =
      let rec aux l acc =
        match l with
        | [] -> acc
        | hd :: tl -> aux tl @@ pexp_let ~loc Nonrecursive [hd] acc
      in
      aux (List.rev l) final
    in
    let build_fn
        : location -> expression -> label_declaration list -> expression =
      fun loc final_expression label_declarations ->
        let assignments = build_field_assignments label_declarations in
        let expr = stitch assignments final_expression loc in
        [%expr fun buf offset -> [%e expr]]
    in
    let reader_fn
        : label_declaration list -> string Asttypes.loc -> Location.t
          -> structure_item =
      fun l type_name loc ->
        let record_fields =
          List.map l ~f:(fun {pld_name; pld_loc} ->
              let lident_loc = lident_loc_of_string_loc pld_name in
              (lident_loc, pexp_ident ~loc lident_loc) )
        in
        let tail_expression = pexp_record ~loc record_fields None in
        let fn_name =
          { type_name with
            txt=
              ( match type_name.txt with
              | "t" -> "of_bytes"
              | s -> s ^ "_of_bytes" ) }
        in
        let binding =
          { pvb_pat= ppat_var ~loc fn_name
          ; pvb_expr= build_fn loc tail_expression l
          ; pvb_attributes= []
          ; pvb_loc= loc }
        in
        {pstr_desc= Pstr_value (Nonrecursive, [binding]); pstr_loc= loc}
    in
    let structure_of_td : type_declaration -> structure = function
      | {ptype_kind= Ptype_record l; ptype_name; ptype_loc} ->
          [reader_fn l ptype_name ptype_loc; writer_fn l ptype_name ptype_loc]
      | _ -> []
    in
    List.concat_map ~f:structure_of_td tds

end

module Gen_sig = struct
  let reader_type ~loc ty = [%type : Bytes.t -> int -> [%t ty]]

  let writer_type ~loc ty = [%type : [%t ty] -> Bytes.t -> int -> unit]

  let generate ~loc ~path:_ (rec_flag, tds) =
    let reader_fn_item ({ptype_name; ptype_loc= loc} as td) =
      let type_ = combinator_type_of_type_declaration td ~f:reader_type in
      let fn_name =
        match ptype_name.txt with "t" -> "of_bytes" | s -> s ^ "_of_bytes"
      in
      psig_value ~loc
      @@ value_description ~loc ~name:{ptype_name with txt= fn_name} ~type_
           ~prim:[]
    in
    let writer_fn_item ({ptype_name; ptype_loc= loc} as td) =
      let type_ = combinator_type_of_type_declaration td ~f:writer_type in
      let fn_name =
        match ptype_name.txt with "t" -> "to_bytes" | s -> s ^ "_to_bytes"
      in
      psig_value ~loc
      @@ value_description ~loc ~name:{ptype_name with txt= fn_name} ~type_
           ~prim:[]
    in
    let structure_of_td : type_declaration -> signature = function
      | {ptype_kind= Ptype_record _; ptype_name; ptype_loc} as td ->
          [reader_fn_item td; writer_fn_item td]
      | _ -> []
    in
    List.concat_map ~f:structure_of_td tds

end

let () =
  Type_conv.add "binary"
    ~str_type_decl:
      (Type_conv.Generator.make ~attributes:[Attribute.T endianness]
         Type_conv.Args.(empty +> arg "endianness" (estring __))
         Gen_str.generate)
    ~sig_type_decl:(Type_conv.Generator.make_noarg Gen_sig.generate)
  |> Type_conv.ignore
