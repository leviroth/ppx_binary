open Migrate_parsetree
open Ast_406
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let extract_record pexp_desc loc =
  let aux = function
    | {txt ; loc}, {pexp_desc = Pexp_constraint ({pexp_desc = Pexp_ident {txt = Lident t_name}},
                                                 {ptyp_desc = Ptyp_constr ({txt = Lident t}, _)}); pexp_loc} -> (pexp_loc, (t_name, t))
    | {loc}, _ -> raise @@ Location.Error (Location.error ~loc "Invalid record field")
  in
  match pexp_desc with
  | Pexp_record (l, _) -> List.map aux l
  | _ -> raise @@ Location.Error (Location.error ~loc "Expected list of field names and types")

let extract_payload pstr loc =
  match pstr with
  | PStr [{pstr_desc = Pstr_eval ({pexp_desc; pexp_loc}, _)}] -> extract_record pexp_desc loc
  | _ -> raise @@ Location.Error (Location.error ~loc "Expected list of field names and types")

let build_field (loc, (name, t_name)) =
  let t = Typ.constr {txt = (Lident t_name); loc} [] in
  Type.field { txt = name ; loc} @@ t

let binary_mapper _config _cookies =
  { default_mapper with
    type_declaration = fun mapper type_declaration ->
      match type_declaration with
      | ({ ptype_manifest = Some ({ptyp_desc = Ptyp_extension ({ txt = "binary"; loc }, pstr)})} as x) ->
        { x with ptype_manifest = None;
                 ptype_kind = Ptype_record (List.map build_field (extract_payload pstr loc))}
      | x -> default_mapper.type_declaration mapper x;
  }

let () =
  Driver.register ~name:"binary" Versions.ocaml_406 binary_mapper
