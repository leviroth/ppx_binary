open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

(* We need a helper to extract the payload into a simple (string * string) list.

   Then we can use that list as the basis for constructing the rest of the code.
*)

(* These functions should preserve loc *)
let extract_tuple : Parsetree.expression -> string * string =
  function
  | {pexp_desc = Pexp_tuple [{pexp_desc = Pexp_ident ({txt = Lident field_name})};
                             {pexp_desc = Pexp_ident ({txt = Lident type_name})}];} ->
    field_name, type_name
  | {pexp_loc} -> raise @@ Location.Error (Location.error ~loc:pexp_loc "Expected two identifiers")

let rec extract_list : Parsetree.expression -> (string * string) list =
  fun l ->
    let rec aux l acc =
      match l with
      | {pexp_desc = Pexp_construct ({txt = Lident "::"; loc}, Some {pexp_desc = Pexp_tuple [hd; tl]}) } ->
        aux tl @@ extract_tuple hd :: acc
      | {pexp_desc = Pexp_construct ({txt = Lident "[]"; loc}, None) } ->
        List.rev acc
      | {pexp_loc} -> raise @@ Location.Error (Location.error ~loc:pexp_loc "Expected list of field names and types")
    in
    aux l []

let extract_payload : Parsetree.payload -> (string * string) list =
  function
  | PStr [{pstr_desc = Pstr_eval (expr, _)}] -> extract_list expr
  | _ -> raise @@ invalid_arg "foo"

let build_field (name, t_name) ~loc =
  let t = Typ.constr {txt = (Lident t_name); loc} [] in
  Type.field { txt = name ; loc} @@ t

let binary_mapper argv =
  { default_mapper with
    type_declaration = fun mapper type_declaration ->
      match type_declaration with
      | ({ ptype_manifest = Some ({ptyp_desc = Ptyp_extension ({ txt = "binary"; loc }, pstr)})} as x) ->
        { x with ptype_manifest = None;
                 ptype_kind = Ptype_record (List.map (build_field ~loc) (extract_payload pstr))}
      | x -> default_mapper.type_declaration mapper x;
  }

let () = register "binary" binary_mapper
