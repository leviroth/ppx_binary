open Ppx_core
open Ast_builder.Default
open Ast_helper
open Ppx_type_conv.Std

let endianness =
  Attribute.declare "binary.endianness" Attribute.Context.label_declaration
    Ast_pattern.(pstr (pstr_eval (pexp_ident (lident __)) nil ^:: nil))
    (fun x -> x )

let masking =
  Attribute.declare "binary.masking" Attribute.Context.label_declaration
    Ast_pattern.(pstr (pstr_eval (pexp_ident (lident __)) nil ^:: nil))
    (fun x -> x )


let lident_loc_of_string_loc x = {x with txt= Lident x.txt}

module Gen_str = struct
  let size lds name loc =
    let sizes =
      List.map lds ~f:(fun ({pld_type} as ld) ->
          match pld_type with
          | {ptyp_desc= Ptyp_constr ({txt}, []); ptyp_loc=loc} ->
            let txt =
              match Attribute.get masking ld with
              | None -> txt
              | Some s -> Lident s
            in
            Typeinfo.size_expr ~loc txt
          | {ptyp_loc} ->
            Location.raise_errorf ~loc:ptyp_loc "Could not handle type")
    in
    let size_expr =
      List.reduce_exn sizes ~f:(fun x y -> [%expr [%e x] + [%e y]])
    in
    let name = match name with
      | {txt="t"} -> [%pat? byte_size]
      | {txt=s} ->
        let s = Printf.sprintf "byte_size_%s" s in
        ppat_var ~loc {txt=s; loc}
    in
    [%stri let [%p name] = [%e size_expr]]

  let generate ~loc ~path:_ (rec_flag, tds) default_endianness =
    let get_default_endianness ~loc =
      Option.map default_endianness ~f:(function
          | "little" | "big" as s -> s
          | _ ->
            Location.raise_errorf ~loc
              "Default endianness must be little or big")
    in
    let extract_type_name = function
      | {ptyp_desc= Ptyp_constr ({txt}, [])} -> txt
      | {ptyp_loc} -> Location.raise_errorf ~loc:ptyp_loc "Could not handle type"
    in
    let read_or_write_expression ld direction =
      let loc = ld.pld_loc in
      let type_name =
        match Attribute.get masking ld with
        | Some s -> Lident s
        | None -> extract_type_name ld.pld_type
      in
      let endianness =
        match Attribute.get endianness ld with
        | Some ("little" | "big" as s) -> Some s
        | Some _ ->
            Location.raise_errorf ~loc
              "Endianness must be little or big"
        | None -> get_default_endianness ~loc
      in
      match direction with
      | `Read ->
          let fn =
            Exp.mk ~loc
            @@ Pexp_ident {txt=Typeinfo.reader_name ?endianness type_name; loc}
          in
          let fn =
            match Attribute.get masking ld with
            | None -> fn
            | Some _ ->
              let converter =
                let txt =
                  Typeinfo.get_int_converter ~direction_string:"to" type_name
                in
                Exp.mk ~loc @@ Pexp_ident {txt; loc}
              in
              [%expr (fun buf offset -> [%e fn] buf offset |> [%e converter])]
          in
          (* (match type_name with Lident s | Ldot (_, s)-> print_endline s | _ -> assert false); *)
          let offset_increment = Typeinfo.size_expr ~loc type_name in
          let offset_expr = [%expr offset + [%e offset_increment]] in
          [%expr [%e fn] buf offset, [%e offset_expr]]
      | `Write ->
          let fn =
            Exp.mk ~loc
            @@ Pexp_ident {txt=Typeinfo.writer_name ?endianness type_name; loc}
          in
          let fn =
            match Attribute.get masking ld with
            | None -> fn
            | Some _ ->
              let converter =
                let txt =
                  Typeinfo.get_int_converter ~direction_string:"of" type_name
                in
                Exp.mk ~loc @@ Pexp_ident {txt; loc}
              in
              [%expr (fun data buf offset -> let data = [%e converter] data in [%e fn] data buf offset)]
          in
          let data_field =
            pexp_field ~loc [%expr data]
            @@ lident_loc_of_string_loc ld.pld_name
          in
          [%expr [%e fn] [%e data_field] buf offset]
    in
    let build_field_assignments : label_declaration list -> value_binding list =
      fun l ->
        let pats = List.map l ~f:(fun ld -> ppat_var ~loc ld.pld_name) in
        let exprs = List.map l ~f:(fun ld -> read_or_write_expression ld `Read) in
        List.map2_exn pats exprs ~f:(fun pat expr ->
            value_binding ~loc ~pat:[%pat? [%p pat], offset] ~expr )
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
    let writer_fn =
      fun lds type_name loc ->
        let fn_name =
          { type_name with
            txt=
              ( match type_name.txt with
              | "t" -> "to_bytes"
              | s -> s ^ "_to_bytes" ) }
        in
        let writes = List.map lds ~f:(fun ld ->
            let type_name = extract_type_name ld.pld_type in
            let type_name =
              match Attribute.get masking ld with
              | Some s -> Lident s
              | None -> type_name
            in
            let offset_increment = Typeinfo.size_expr ~loc type_name in
            let offset_expr = [%expr offset + [%e offset_increment]] in
            let read_expr = read_or_write_expression ld `Write in
            let expr = [%expr [%e read_expr], [%e offset_expr]] in
            value_binding ~loc ~pat:[%pat? (), offset] ~expr)
        in
        let expr = stitch writes [%expr ()] loc in
        let expr = [%expr fun data buf offset -> [%e expr]] in
        let binding =
          { pvb_pat= ppat_var ~loc fn_name
          ; pvb_expr= expr
          ; pvb_attributes= []
          ; pvb_loc= loc }
        in
        {pstr_desc= Pstr_value (Nonrecursive, [binding]); pstr_loc= loc}
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
        [size l ptype_name ptype_loc;
         reader_fn l ptype_name ptype_loc;
         writer_fn l ptype_name ptype_loc]
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
        let size_item =
          let size_name = {
            ptype_name with txt =
                              match ptype_name.txt with
                              | "t" -> "byte_size"
                              | s -> Printf.sprintf "byte_size_%s" s}
          in
          {psig_desc =
             Psig_value
               (Ast_builder.Default.value_description
                  ~loc:ptype_loc
                  ~name:size_name
                  ~type_:(ptyp_constr
                            ~loc:ptype_loc
                            {txt=lident "int"; loc=ptype_loc}
                            [])
                  ~prim:[]);
           psig_loc = ptype_loc}
        in
        [size_item;
         reader_fn_item td; writer_fn_item td]
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
