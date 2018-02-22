open Ppx_core
open Ast_builder.Default
open Ast_helper
open Ppx_type_conv.Std

type field_info =
  { name: string Asttypes.loc
  ; disk_type: longident
  ; masking: bool
  ; endianness: string option
  ; read_expr: expression
  ; write_expr: expression
  ; byte_size_expr: expression }

let endianness =
  Attribute.declare "binary.endianness" Attribute.Context.label_declaration
    Ast_pattern.(pstr (pstr_eval (pexp_ident (lident __)) nil ^:: nil))
    (fun x -> x)


let masking =
  Attribute.declare "binary.masking" Attribute.Context.label_declaration
    Ast_pattern.(pstr (pstr_eval (pexp_ident (lident __)) nil ^:: nil))
    (fun x -> x)


let lident_loc_of_string_loc x = {x with txt= Lident x.txt}

let to_info ({pld_name; pld_type; pld_loc= loc} as ld) ~default_endianness =
  let masking, disk_type =
    match pld_type with
    | {ptyp_desc= Ptyp_constr ({txt}, [])} -> (
      match Attribute.get masking ld with
      | None -> (false, txt)
      | Some s -> (true, Lident s) )
    | {ptyp_loc} -> Location.raise_errorf ~loc "Could not handle type"
  in
  let endianness =
    Option.first_some default_endianness @@ Attribute.get endianness ld
  in
  { name= pld_name
  ; disk_type
  ; masking
  ; endianness
  ; read_expr= Typeinfo.read_expr disk_type ?endianness ~masking ~loc
  ; write_expr= Typeinfo.write_expr disk_type ?endianness ~masking ~loc
  ; byte_size_expr= Typeinfo.byte_size_expr disk_type ~loc }


(** Convert a list of assignments [p1 = e1; ...; pn = en] and a final expression
   [expr] into the expression [let p1 = e1 in let ... in let pn = en in expr].
   *)
let stitch l ~final_expression ~loc =
  let rec aux l acc =
    match l with
    | [] -> acc
    | hd :: tl -> aux tl @@ pexp_let ~loc Nonrecursive [hd] acc
  in
  aux (List.rev l) final_expression


let size record_name ~info_list ~loc =
  let sizes = List.map info_list ~f:(fun {byte_size_expr} -> byte_size_expr) in
  let size_expr =
    List.reduce_exn sizes ~f:(fun x y -> [%expr [%e x] + [%e y]])
  in
  let name =
    match record_name with
    | {txt= "t"} -> [%pat? byte_size]
    | {txt= s} ->
        let s = Printf.sprintf "byte_size_%s" s in
        ppat_var ~loc {record_name with txt= s}
  in
  [%stri let [%p name] = [%e size_expr]]


let offset_expr {byte_size_expr} ~loc = [%expr offset + [%e byte_size_expr]]

let writer_fn record_name ~info_list ~loc =
  let fn_name =
    { record_name with
      txt= (match record_name.txt with "t" -> "to_bytes" | s -> s ^ "_to_bytes")
    }
  in
  let writes =
    List.map info_list ~f:(fun info ->
        let write_expr = info.write_expr in
        let data_field =
          pexp_field ~loc [%expr data]
          @@ lident_loc_of_string_loc info.name
        in
        let expr =
          [%expr ([%e write_expr] [%e data_field], [%e offset_expr info ~loc])]
        in
        value_binding ~loc ~pat:[%pat? (), offset] ~expr )
  in
  let expr = stitch writes ~final_expression:[%expr ()] ~loc in
  let expr = [%expr fun data buf offset -> [%e expr]] in
  let binding =
    { pvb_pat= ppat_var ~loc fn_name
    ; pvb_expr= expr
    ; pvb_attributes= []
    ; pvb_loc= loc }
  in
  {pstr_desc= Pstr_value (Nonrecursive, [binding]); pstr_loc= loc}


let reader_fn record_name ~info_list ~loc =
  let build_fn final_expression =
    let assignments =
      List.map info_list ~f:(fun ({name; read_expr} as info) ->
          let pattern = ppat_var ~loc name in
          let expr = [%expr ([%e read_expr], [%e offset_expr info ~loc])] in
          value_binding ~loc ~pat:[%pat? [%p pattern], offset] ~expr )
    in
    let expr = stitch assignments ~final_expression ~loc in
    [%expr fun buf offset -> [%e expr]]
  in
  let record_fields =
    List.map info_list ~f:(fun {name} ->
        let lident_loc = lident_loc_of_string_loc name in
        (lident_loc, pexp_ident ~loc:name.loc lident_loc) )
  in
  let final_expression = pexp_record ~loc record_fields None in
  let fn_name =
    { record_name with
      txt= (match record_name.txt with "t" -> "of_bytes" | s -> s ^ "_of_bytes")
    }
  in
  let binding =
    { pvb_pat= ppat_var ~loc fn_name
    ; pvb_expr= build_fn final_expression
    ; pvb_attributes= []
    ; pvb_loc= loc }
  in
  {pstr_desc= Pstr_value (Nonrecursive, [binding]); pstr_loc= loc}


let structure_of_td ~default_endianness = function
  | {ptype_kind= Ptype_record l; ptype_name; ptype_loc= loc} ->
      let info_list = List.map l ~f:(to_info ~default_endianness) in
      [ size ptype_name ~info_list ~loc
      ; reader_fn ptype_name ~info_list ~loc
      ; writer_fn ptype_name ~info_list ~loc ]
  | _ -> []


module Gen_str = struct
  let generate ~loc ~path:_ (rec_flag, tds) default_endianness =
    let default_endianness =
      Option.map default_endianness ~f:(function
        | ("little" | "big") as s -> s
        | _ ->
            Location.raise_errorf ~loc
              "Default endianness must be little or big" )
    in
    List.concat_map ~f:(structure_of_td ~default_endianness) tds
end

module Gen_sig = struct
  let reader_type ~loc ty = [%type: Bytes.t -> int -> [%t ty]]

  let writer_type ~loc ty = [%type: [%t ty] -> Bytes.t -> int -> unit]

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
            let size_name =
              { ptype_name with
                txt=
                  ( match ptype_name.txt with
                  | "t" -> "byte_size"
                  | s -> Printf.sprintf "byte_size_%s" s ) }
            in
            { psig_desc=
                Psig_value
                  (Ast_builder.Default.value_description ~loc:ptype_loc
                     ~name:size_name
                     ~type_:
                       (ptyp_constr ~loc:ptype_loc
                          {txt= lident "int"; loc= ptype_loc} [])
                     ~prim:[])
            ; psig_loc= ptype_loc }
          in
          [size_item; reader_fn_item td; writer_fn_item td]
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
