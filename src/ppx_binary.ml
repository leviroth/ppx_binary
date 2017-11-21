open Ppx_core
open Ast_builder.Default
open Ast_helper
open Ppx_type_conv.Std
let (@@) = Caml.(@@)

type typeinfo = {
  module_name : string;
  byte_size : int;
}

let type_to_module =
  Map.of_alist_exn (module String)
    [
      "uint8", {module_name = "Uint8"; byte_size = 1};
      "uint16", {module_name = "Uint16"; byte_size = 2};
      "uint24", {module_name = "Uint24"; byte_size = 3};
      "uint32", {module_name = "Uint32"; byte_size = 4};
      "uint40", {module_name = "Uint40"; byte_size = 5};
      "uint48", {module_name = "Uint48"; byte_size = 6};
      "uint56", {module_name = "Uint56"; byte_size = 7};
      "uint64", {module_name = "Uint64"; byte_size = 8};
      "uint128", {module_name = "Uint128"; byte_size = 16};
      "int8", {module_name = "Int8"; byte_size = 1};
      "int16", {module_name = "Int16"; byte_size = 2};
      "int24", {module_name = "Int24"; byte_size = 3};
      "int32", {module_name = "Int32"; byte_size = 4};
      "int40", {module_name = "Int40"; byte_size = 5};
      "int48", {module_name = "Int48"; byte_size = 6};
      "int56", {module_name = "Int56"; byte_size = 7};
      "int64", {module_name = "Int64"; byte_size = 8};
      "int128", {module_name = "Int128"; byte_size = 16};
    ]

let extract_module_info = function
  | { ptyp_desc = Ptyp_constr ({txt = Lident s}, []) } -> Map.find_exn type_to_module s
  | { ptyp_loc } -> Location.raise_errorf ~loc:ptyp_loc "Expected basic type"

let get_type_byte_width = fun t -> (extract_module_info t).byte_size

let single_field_assignment {pld_name; pld_type; pld_loc = loc} offset : value_binding =
  let module_info = extract_module_info pld_type in
  let fn = evar ~loc @@ Printf.sprintf "%s.of_bytes_little_endian" @@ module_info.module_name in
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
    let fn_name = {type_name with txt = match type_name.txt with | "t" -> "of_bytes" | s -> s ^ "of_bytes"} in
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

let structure_of_td : type_declaration -> structure =
  function
  | { ptype_kind = Ptype_record l; ptype_name; ptype_loc } ->
    [reader_fn l ptype_name ptype_loc; ]
  | _ -> []

module Gen_str = struct
  let generate ~loc ~path:_ (rec_flag, tds) =
    List.concat_map ~f:structure_of_td tds
end

let () =
  Type_conv.add "binary"
    ~str_type_decl:(Type_conv.Generator.make_noarg Gen_str.generate)
  |> Type_conv.ignore
