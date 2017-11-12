open Ppx_core
open Ast_builder.Default
open Ast_helper
open Ppx_type_conv.Std
let (@@) = Caml.(@@)

let extract_record pexp_desc ~loc =
  let aux = function
    | {txt ; loc}, {pexp_desc = Pexp_constraint ({pexp_desc = Pexp_ident {txt = Lident t_name}},
                                                 {ptyp_desc = Ptyp_constr ({txt = Lident t}, _)}); pexp_loc} -> (pexp_loc, (t_name, t))
    | {loc}, _ -> Location.raise_errorf ~loc "Invalid record field"
  in
  match pexp_desc with
  | Pexp_record (l, _) -> List.map ~f:aux l
  | _ -> Location.raise_errorf ~loc "Expected list of field names and types"

let extract_type_name = function
  | { ptyp_desc = Ptyp_constr ({txt = Lident s}, []) } -> s
  | { ptyp_loc } -> Location.raise_errorf ~loc:ptyp_loc "Expected basic type"

let extract_record' : label_declaration -> Location.t * (string * string) = function
  | {pld_loc ; pld_name = {txt} ; pld_type} -> (pld_loc, (txt, extract_type_name pld_type))

let extract_payload pstr loc =
  match pstr with
  | PStr [{pstr_desc = Pstr_eval ({pexp_desc; pexp_loc}, _)}] -> extract_record pexp_desc loc
  | _ -> Location.raise_errorf ~loc "Expected list of field names and types"

let build_field (loc, (name, t_name)) =
  let t = Typ.constr {txt = (Lident t_name); loc} [] in
  Type.field { txt = name ; loc} @@ t

let get_type_byte_width = fun _ -> 2

let single_field_assignment {pld_name; pld_type; pld_loc = loc} offset : value_binding =
  let fn = evar ~loc @@ Printf.sprintf "U%s.of_bytes_little_endian" @@ extract_type_name pld_type in
  let pat = ppat_var ~loc pld_name in
  let expr = [%expr [%e fn] buf (offset + [%e offset])] in
  value_binding ~loc ~pat ~expr

let build_field_assignments : label_declaration list -> value_binding list =
  let rec aux l offset acc =
    match l with
    | [] -> List.rev acc
    | hd :: tl ->
      let offset_expr = pexp_constant ~loc:hd.pld_loc @@ Pconst_integer (Int.to_string offset, None) in
      aux tl (offset + get_type_byte_width hd.pld_type) (single_field_assignment hd offset_expr :: acc)
  in fun l -> aux l 0 []

let stitch l final loc =
  let rec aux l acc =
    match l with
    | [] -> acc
    | hd :: tl -> aux tl @@ pexp_let ~loc Nonrecursive [hd] acc
  in aux (List.rev l) final

let build_fn : location -> expression -> label_declaration list -> expression =
  fun loc final_expression label_declarations ->
  let assignments = build_field_assignments label_declarations in
  let expr = stitch assignments final_expression loc in
  [%expr
    (fun buf offset ->
       [%e expr]
    )
  ]

let lident_loc_of_string_loc x = {x with txt = Lident x.txt}

let reader_fn : label_declaration list -> string Asttypes.loc -> Location.t -> structure_item =
  fun l type_name loc ->
    let record_fields = List.map l ~f:(fun {pld_name; pld_loc} ->
        let lident_loc = lident_loc_of_string_loc pld_name in
        (lident_loc, pexp_ident ~loc lident_loc)) in
    let tail_expression = pexp_record ~loc record_fields None in
    let fn_name = {type_name with txt = "read_" ^ type_name.txt } in
    let binding = {pvb_pat = ppat_var ~loc fn_name;
                   pvb_expr = build_fn loc tail_expression l;
                   pvb_attributes = [];
                   pvb_loc = loc}
    in
    {pstr_desc = Pstr_value (Nonrecursive, [binding]);
     pstr_loc = loc
    }



let writer_fn : label_declaration list -> string Asttypes.loc -> Location.t -> structure_item =
  fun _ _ -> raise @@ Invalid_argument "not implemented"

let f : type_declaration -> structure =
  function
  | { ptype_kind = Ptype_record l; ptype_name; ptype_loc } ->
    [reader_fn l ptype_name ptype_loc; ]
(* writer_fn l ptype_name ptype_loc] *)
  | _ -> []

module Gen_str = struct
  let generate ~loc ~path:_ (rec_flag, tds) =
    List.concat_map ~f tds
end

module Gen_sig = struct
  let generate ~loc ~path:_ (rec_flag, tds) =
    let kind = Ptype_record (List.map ~f:build_field [!default_loc, ("foo", "int"); !default_loc, ("bar", "string")]) in
    let new_sig = Sig.mk @@ Psig_type (Nonrecursive, [Type.mk ~kind {txt = "x"; loc = !default_loc}]) in
    [new_sig]
end

let () =
  Type_conv.add "binary"
    ~str_type_decl:(Type_conv.Generator.make_noarg Gen_str.generate)
    ~sig_type_decl:(Type_conv.Generator.make_noarg Gen_sig.generate)
  |> Type_conv.ignore
