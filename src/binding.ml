open List
open Syntax_objects

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
  | SyntaxObj (e, sc) :: t ->
    match length (elements sc) with
    | len when len > cur -> get_largest_scope t len (SyntaxObj (e, sc) :: [])
    | len when len = cur -> get_largest_scope t cur (SyntaxObj (e, sc) :: acc)
    | _ -> get_largest_scope t cur acc

let resolve id =
  let candidate_ids = find_all_matching_bindings id in
  match candidate_ids with
  | [] -> (- 1)
  | _ ->
    (* compare all candidate objects' scope length & choose greatest/first *)
    let max_id = hd (get_largest_scope candidate_ids (- 1) []) in
    (* check if ambigious?? *)
    Hashtbl.find all_bindings max_id
