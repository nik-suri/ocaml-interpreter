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
  failwith "free_vars not implemented" ;;
  
(* Return a fresh variable, constructed with a running counter a la
    gensym. Assumes no variable names use the prefix "var". *)
let new_varname () : varid =
  failwith "new_varname not implemented" ;;
  
(* Substitute [repl] for free occurrences of [var_name] in [exp] *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  failwith "subst not implemented" ;;

    
(* exp_to_string -- Returns a string representation of the expr *)
let rec exp_to_string (exp : expr) : string =
  failwith "exp_to_string not implemented" ;;

(* exp_to_abstract_string: Returns a string representation of the abstract
   syntax of the expr *)
let rec exp_to_abstract_string (exp : expr) : string =
  failwith "exp_to_abstract_string not implemented" ;;
