open List
open Syntax_objects
open Macro_ast
open Scope

exception BindingError of string
let binding_error msg = raise (BindingError msg)

type binding =
  (syntax, int) Hashtbl.t

let all_bindings : binding = Hashtbl.create 10

let add_binding id binding =
  Hashtbl.add all_bindings id binding

let find_all_matching_bindings id =
  let open Hashtbl in
  fold (fun k v acc ->
    let same_id = syntax_e id = syntax_e k in
    let is_subset = ScopeSet.subset (syntax_s k) (syntax_s id) in
    acc @ if same_id && is_subset then [k] else []
  ) all_bindings []

let rec get_largest_scope ss cur acc =
  let open ScopeSet in
  match ss with
  | [] -> acc
  | SyntaxList l :: t -> get_largest_scope (l @ t) cur acc
  | SyntaxObj (e, sc) :: t ->
    match length (elements sc) with
    | len when len > cur -> get_largest_scope t len (SyntaxObj (e, sc) :: [])
    | len when len = cur -> get_largest_scope t cur (SyntaxObj (e, sc) :: acc)
    | _ -> get_largest_scope t cur acc

let rec check_unambigious max_id candidate_ids =
  let open ScopeSet in
  match candidate_ids with
  | h :: t ->
    if subset (syntax_s h) (syntax_s max_id) then
      check_unambigious max_id t
    else binding_error ("Ambigious scope")
  | [] -> ()

let resolve id =
  let candidate_ids = find_all_matching_bindings id in
  match candidate_ids with
  | [] -> (- 1)
  | _ ->
    (* compare all candidate objects' scope length & choose greatest *)
    let max_id = hd (get_largest_scope candidate_ids 0 []) in
    check_unambigious max_id candidate_ids;
    Hashtbl.find all_bindings max_id

module SExpSet = Set.Make(struct type t = s_exp let compare = compare end)

type int_set = SExpSet.t

let core_scope = scope ()

let core_form_list = ["lambda"; "let-syntax"; "quote"; "quote-syntax"]

let core_forms =
  let quote s = SId s in
  SExpSet.of_list (map quote core_form_list)

let core_primitive_list = ["datum->syntax"; "syntax->datum"; "syntax-e";
                            "list"; "cons"; "first"; "second"; "rest"; "map"]

let core_primitives =
  let quote s = SId s in
  SExpSet.of_list (map quote core_primitive_list)

let add_core =
  let quote s = SId s in
  let sym = map quote (core_primitive_list @ core_form_list) in
  iter (fun s -> add_binding (syntax s [core_scope]) core_scope) sym

let introduce s = add_scope s core_scope
