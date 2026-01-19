(** Type alias cycle detection with contractiveness checking.

    Implements well-foundedness checking for recursive type definitions.
    See {!module:Cycle_check.mli} for detailed documentation. *)

open Common
open Types

(** Errors detected during cycle checking. *)
type cycle_error =
  | DirectCycle of string * Location.t
  | UnguardedCycle of string list * Location.t
  | MutualCycle of string list * Location.t

exception Cycle_detected of cycle_error

(** State for tracking visited types during cycle detection. *)
type visit_state =
  | NotVisited
  | Visiting of bool  (** bool = have we seen a contractive position? *)
  | Visited

(** Check if a type constructor is contractive (guards recursion).

    A type constructor is contractive if it's a variant or record type,
    not a type alias. Passing through a contractive constructor makes
    the recursion well-founded. *)
let is_contractive_constructor env path =
  match path with
  | PathBuiltin _ ->
    (* Builtins like int are trivially contractive (no recursion) *)
    true
  | PathLocal name ->
    (* Look up the type declaration to check if it's a variant/record *)
    begin match Environment.find_type name env with
    | Some decl ->
      begin match decl.declaration_kind with
      | DeclarationVariant _ -> true  (* Variants are contractive *)
      | DeclarationRecord _ -> true   (* Records are contractive *)
      | DeclarationExtensible -> true (* Extensible variants are contractive *)
      | DeclarationAbstract ->
        (* Type aliases are NOT contractive *)
        Option.is_none decl.declaration_manifest
      end
    | None ->
      (* Unknown type: be conservative, assume NOT contractive *)
      false
    end
  | PathIdent _ | PathDot _ | PathApply _ ->
    (* Module paths: look up if possible, otherwise assume not contractive *)
    (* For now, be conservative *)
    false

(** Check a type expression for cycles.

    @param env Typing environment for type lookups
    @param recursive_set Names of types being defined (for detecting cycles)
    @param visited State of each type in the recursion
    @param contractive Whether we've passed through a contractive position
    @param loc Location for error messages
    @param ty Type expression to check *)
let rec check_type ~env ~recursive_set ~visited ~contractive ~loc ty =
  match representative ty with
  | TypeVariable _ ->
    (* Type variables don't participate in definition cycles *)
    ()

  | TypeConstructor (path, args) ->
    let name = path_to_string path in

    (* Check if this is a reference to a type being defined *)
    if List.mem name recursive_set then begin
      match Hashtbl.find_opt visited name with
      | Some (Visiting seen_contractive) ->
        (* We're in a cycle - check if it's guarded *)
        if not (contractive || seen_contractive) then
          raise (Cycle_detected (UnguardedCycle ([name], loc)))
      | Some Visited ->
        (* Already fully checked, no cycle here *)
        ()
      | Some NotVisited | None ->
        (* First visit: mark as visiting with current contractiveness *)
        Hashtbl.replace visited name (Visiting contractive)
    end;

    (* Determine if this constructor is contractive *)
    let is_contractive = is_contractive_constructor env path in
    let new_contractive = contractive || is_contractive in

    (* Recursively check arguments *)
    List.iter (check_type ~env ~recursive_set ~visited
                 ~contractive:new_contractive ~loc) args

  | TypeArrow (_, arg, result) ->
    (* Arrow types are NOT contractive *)
    check_type ~env ~recursive_set ~visited ~contractive ~loc arg;
    check_type ~env ~recursive_set ~visited ~contractive ~loc result

  | TypeTuple elements ->
    (* Tuples are NOT contractive (they're just products) *)
    List.iter (check_type ~env ~recursive_set ~visited ~contractive ~loc) elements

  | TypeRecord row ->
    (* Records ARE contractive (they're like single-constructor variants) *)
    check_row ~env ~recursive_set ~visited ~contractive:true ~loc row

  | TypePolyVariant pv_row ->
    (* Polymorphic variants ARE contractive (they're sum types) *)
    check_poly_variant_row ~env ~recursive_set ~visited ~contractive:true ~loc pv_row

  | TypeRowEmpty ->
    ()

  | TypePackage pkg ->
    (* Check all types in the package signature *)
    List.iter (fun (_, ty) ->
      check_type ~env ~recursive_set ~visited ~contractive:true ~loc ty
    ) pkg.package_signature

and check_row ~env ~recursive_set ~visited ~contractive ~loc row =
  List.iter (fun (_, field) ->
    match field with
    | RowFieldPresent ty ->
      check_type ~env ~recursive_set ~visited ~contractive ~loc ty
  ) row.row_fields;
  check_type ~env ~recursive_set ~visited ~contractive ~loc row.row_more

and check_poly_variant_row ~env ~recursive_set ~visited ~contractive ~loc pv_row =
  List.iter (fun (_, field) ->
    match field with
    | PVFieldPresent (Some ty) ->
      check_type ~env ~recursive_set ~visited ~contractive ~loc ty
    | PVFieldPresent None | PVFieldAbsent -> ()
  ) pv_row.pv_fields;
  check_type ~env ~recursive_set ~visited ~contractive ~loc pv_row.pv_more

(** Check if a type expression contains a reference to a given type name.
    Used for detecting self-references in type aliases. *)
let rec contains_type_name name ty =
  match representative ty with
  | TypeVariable _ -> false
  | TypeConstructor (path, args) ->
    path_to_string path = name || List.exists (contains_type_name name) args
  | TypeArrow (_, arg, result) ->
    contains_type_name name arg || contains_type_name name result
  | TypeTuple elements ->
    List.exists (contains_type_name name) elements
  | TypeRecord row ->
    contains_type_name_in_row name row
  | TypePolyVariant pv_row ->
    contains_type_name_in_pv_row name pv_row
  | TypeRowEmpty -> false
  | TypePackage pkg ->
    List.exists (fun (_, ty) -> contains_type_name name ty) pkg.package_signature

and contains_type_name_in_row name row =
  List.exists (fun (_, field) ->
    match field with
    | RowFieldPresent ty -> contains_type_name name ty
  ) row.row_fields
  || contains_type_name name row.row_more

and contains_type_name_in_pv_row name pv_row =
  List.exists (fun (_, field) ->
    match field with
    | PVFieldPresent (Some ty) -> contains_type_name name ty
    | PVFieldPresent None | PVFieldAbsent -> false
  ) pv_row.pv_fields
  || contains_type_name name pv_row.pv_more

(** Check a single type definition for cycles. *)
let check_type_definition ~env ~loc name _params kind manifest =
  let visited = Hashtbl.create 8 in
  let recursive_set = [name] in

  (* Mark this type as being visited *)
  Hashtbl.add visited name (Visiting false);

  begin match kind, manifest with
  | DeclarationAbstract, Some manifest_ty ->
    (* Type alias: ANY self-reference is a cycle error.
       Unlike variant/record types, type aliases cannot be recursive
       even if wrapped in a data constructor. OCaml rejects:
         type t = t list
       because t is an alias, not a data type. *)
    if contains_type_name name manifest_ty then
      raise (Cycle_detected (UnguardedCycle ([name], loc)));

    (* Also check for indirect cycles through other type aliases *)
    begin try
      check_type ~env ~recursive_set ~visited ~contractive:false ~loc manifest_ty
    with Cycle_detected (UnguardedCycle (path, _)) ->
      (* Add this type name to the cycle path *)
      if not (List.mem name path) then
        raise (Cycle_detected (UnguardedCycle (name :: path, loc)))
      else
        raise (Cycle_detected (UnguardedCycle (path, loc)))
    end

  | DeclarationVariant constructors, _ ->
    (* Variant: each constructor IS contractive *)
    List.iter (fun ctor ->
      match ctor.constructor_argument_type with
      | None -> ()
      | Some arg_ty ->
        check_type ~env ~recursive_set ~visited ~contractive:true ~loc arg_ty
    ) constructors

  | DeclarationRecord fields, _ ->
    (* Record: the record itself IS contractive *)
    List.iter (fun (_, field_ty) ->
      check_type ~env ~recursive_set ~visited ~contractive:true ~loc field_ty
    ) fields

  | DeclarationExtensible, _ ->
    (* Extensible type: no constructors defined yet, nothing to check *)
    ()

  | DeclarationAbstract, None ->
    (* Abstract type: no body to check *)
    ()
  end;

  Hashtbl.replace visited name Visited

(** Check mutually recursive type definitions for invalid cycles. *)
let check_mutual_recursion ~env ~loc definitions =
  let names = List.map (fun decl -> decl.declaration_name) definitions in
  let visited = Hashtbl.create (List.length definitions) in

  (* Initialize all as not visited *)
  List.iter (fun name -> Hashtbl.add visited name NotVisited) names;

  (* Check each definition *)
  List.iter (fun decl ->
    if Hashtbl.find visited decl.declaration_name = NotVisited then begin
      Hashtbl.replace visited decl.declaration_name (Visiting false);

      begin match decl.declaration_kind, decl.declaration_manifest with
      | DeclarationAbstract, Some manifest_ty ->
        check_type ~env ~recursive_set:names ~visited ~contractive:false
          ~loc manifest_ty
      | DeclarationVariant constructors, _ ->
        List.iter (fun ctor ->
          match ctor.constructor_argument_type with
          | None -> ()
          | Some arg_ty ->
            check_type ~env ~recursive_set:names ~visited ~contractive:true
              ~loc arg_ty
        ) constructors
      | DeclarationRecord fields, _ ->
        List.iter (fun (_, field_ty) ->
          check_type ~env ~recursive_set:names ~visited ~contractive:true
            ~loc field_ty
        ) fields
      | DeclarationExtensible, _ ->
        (* Extensible type: no constructors defined yet *)
        ()
      | DeclarationAbstract, None ->
        ()
      end;

      Hashtbl.replace visited decl.declaration_name Visited
    end
  ) definitions

(** Format cycle error for user-friendly display. *)
let format_error = function
  | DirectCycle (name, _) ->
    Printf.sprintf
      "Cyclic type alias: The type '%s' is defined in terms of itself.\n\n\
       Hint: Recursive types must be guarded by a data constructor.\n\
       Instead of:  type %s = %s\n\
       Try:         type %s = %s of %s"
      name name name name (String.capitalize_ascii name) name

  | UnguardedCycle (path, _) ->
    let path_str = String.concat " -> " path in
    Printf.sprintf
      "Unguarded recursive type: The type definition creates a cycle: %s\n\n\
       Recursive types must pass through a data constructor.\n\
       For example:\n\
         INVALID: type t = int -> t\n\
         VALID:   type t = Leaf | Node of t * t"
      path_str

  | MutualCycle (names, _) ->
    let names_str = String.concat ", " names in
    Printf.sprintf
      "Mutually recursive type cycle: The types [%s] form an unguarded cycle.\n\n\
       Each type in a mutually recursive group must be guarded by a data constructor."
      names_str
