(*
			 CS 51 Final Project
			MiniML -- Expressions
			     Spring 2017
*)

(* Abstract syntax of MiniML expressions *)

type unop =
  | Negate
;;

type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan
;;

type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
 and varid = string ;;

(* Sets of varids *)
module SS = Set.Make (struct
		       type t = varid
		       let compare = String.compare
		     end ) ;;

type varidset = SS.t ;;

(* Test to see if two sets have the same elements (for
    testing purposes) *)
let same_vars = SS.equal;;

(* Generate a set of variable names from a list of strings (for
    testing purposes) *)
let vars_of_list = SS.of_list ;;

(* Return a set of the variable names free in [exp] *)
let rec free_vars (exp : expr) : varidset =
  match exp with
  | Num _ | Bool _ | Raise | Unassigned -> SS.empty
  | Var id -> SS.singleton id
  | Unop (_, e) -> free_vars e
  | Binop (_, e1, e2) -> SS.union (free_vars e1) (free_vars e2)
  | App (e1, e2) -> SS.union (free_vars e1) (free_vars e2)
  | Fun (id, e) -> SS.remove id (free_vars e)
  | Let (id, e1, e2) -> SS.union (SS.remove id (free_vars e2)) (free_vars e1)
  | Conditional (e1, e2, e3) ->
    SS.union (free_vars e1) (SS.union (free_vars e2) (free_vars e3))
  | Letrec (id, e1, e2) ->
    SS.union (SS.remove id (free_vars e2)) (SS.remove id (free_vars e1))
;;

(* Return a fresh variable, constructed with a running counter a la
    gensym. Assumes no variable names use the prefix "var". *)

let new_varname =
  let ctr = ref 0 in
  fun () ->
    let var = !ctr in
    ctr := (!ctr + 1);
    "x" ^ (string_of_int var)
;;

(* Substitute [repl] for free occurrences of [var_name] in [exp] *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  match exp with
  | Num _ | Bool _ | Raise | Unassigned -> exp
  | Var id -> if id = var_name then repl else exp
  | Unop (op, e) -> Unop (op, subst var_name repl e)
  | Binop (op, e1, e2) ->
    Binop (op, subst var_name repl e1, subst var_name repl e2)
  | Conditional (e1, e2, e3) ->
    Conditional
      (subst var_name repl e1, subst var_name repl e2, subst var_name repl e3)
  | App (e1, e2) -> App (subst var_name repl e1, subst var_name repl e2)
  | Fun (id, e) ->
    if id = var_name
      then Fun (id, e)
    else if SS.mem id (free_vars repl)
      then Fun (new_varname (), subst var_name repl e)
    else Fun (id, subst var_name repl e)
  | Let (id, e1, e2) ->
    if id = var_name
      then Let (id, subst var_name repl e1, e2)
    else if SS.mem id (free_vars repl)
      then Let (new_varname (), subst var_name repl e1, subst var_name repl e2)
    else Let (id, subst var_name repl e1, subst var_name repl e2)
  | Letrec (id, e1, e2) ->
    if id = var_name
      then Letrec (id, subst var_name repl e1, subst var_name repl e2)
    else if SS.mem id (free_vars repl)
      then Let (new_varname (), subst var_name repl e1, subst var_name repl e2)
    else Let (id, subst var_name repl e1, subst var_name repl e2)
;;


(* exp_to_string -- Returns a string representation of the expr *)
let rec exp_to_string (exp : expr) : string =
  match exp with
  | Num n -> string_of_int n
  | Bool b -> string_of_bool b
  | Var id -> id
  | Unop (_op, e) -> "-" ^ exp_to_string e
  | Binop (op, e1, e2) ->
    exp_to_string e1 ^ (match op with
                        | Plus -> " + "
                        | Minus -> " - "
                        | Times -> " * "
                        | Equals -> " = "
                        | LessThan -> " < ") ^ exp_to_string e2
  | Conditional (e1, e2, e3) ->
    " if " ^ exp_to_string e1 ^ " then " ^ exp_to_string e2 ^ " else " ^
      exp_to_string e3
  | Fun (id, e) -> " fun " ^ id ^ " -> " ^ exp_to_string e
  | Let (id, e1, e2) ->
    " let " ^ id ^ " = " ^ exp_to_string e1 ^ " in " ^ exp_to_string e2
  | Letrec (id, e1, e2) ->
    " let rec " ^ id ^ " = " ^ exp_to_string e1 ^ " in " ^ exp_to_string e2
  | Raise -> "Raise"
  | Unassigned -> "Unassigned"
  | App (e1, e2) -> exp_to_string e1 ^ " " ^ exp_to_string e2
;;

(* exp_to_abstract_string: Returns a string representation of the abstract
   syntax of the expr *)
let rec exp_to_abstract_string (exp : expr) : string =
  match exp with
  | Var id -> "Var(" ^ id ^ ")"
  | Num i -> "Num(" ^ string_of_int i ^ ")"
  | Bool b -> "Bool(" ^ string_of_bool b ^ ")"
  | Unop (_op, e) -> "Unop(Negate, " ^ exp_to_abstract_string e ^ ")"
  | Binop (op, e1, e2) ->
    "Binop(" ^ (match op with
                | Plus -> "Plus, "
                | Minus -> "Minus, "
                | Times -> "Times, "
                | Equals -> "Equals, "
                | LessThan -> "LessThan, ")
        ^ exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2 ^ ")"
  | Conditional (e1, e2, e3) ->
    "Conditional(" ^ exp_to_abstract_string e1 ^ ", " ^
      exp_to_abstract_string e2 ^ ", " ^ exp_to_abstract_string e3 ^ ")"
  | Fun (id, e) -> "Fun(" ^ id ^ ", " ^ exp_to_abstract_string e ^ ")"
  | Let (id, e1, e2) ->
    "Let(" ^ id ^ ", " ^ exp_to_abstract_string e1 ^ ", " ^
      exp_to_abstract_string e2 ^ ")"
  | Letrec (id, e1, e2) ->
    "Letrec(" ^ id ^ ", " ^ exp_to_abstract_string e1 ^ ", " ^
      exp_to_abstract_string e2 ^ ")"
  | Raise -> "Raise"
  | Unassigned -> "Unassigned"
  | App (e1, e2) ->
    "App(" ^ exp_to_abstract_string e1 ^ ", " ^
      exp_to_abstract_string e2 ^ ")"
;;
