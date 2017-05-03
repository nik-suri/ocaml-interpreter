(*
                         CS 51 Final Project
                         MiniML -- Evaluation
                             Spring 2017
*)

(* This module implements a small untyped ML-like language under
   various operational semantics.
 *)

open Expr ;;

(* Exception for evaluator runtime, generated by a runtime error *)
exception EvalError of string ;;
(*Exception for evaluator runtime, generated by an explicit "raise" construct*)
exception EvalException ;;


(* Environments and values *)

module type Env_type = sig
    type env
    type value =
      | Val of expr
      | Closure of (expr * env)
    val create : unit -> env
    val close : expr -> env -> value
    val lookup : env -> varid -> value
    val extend : env -> varid -> value ref -> env
    val env_to_string : env -> string
    val value_to_string : ?printenvp:bool -> value -> string
  end

module Env : Env_type =
  struct
    type env = (varid * value ref) list
     and value =
       | Val of expr
       | Closure of (expr * env)

    (* Creates an empty environment *)
    let create () : env = [] ;;

    (* Creates a closure from an expression and the environment it's
       defined in *)
    let close (exp : expr) (env : env) : value =
      Closure (exp, env)
    ;;

    (* Looks up the value of a variable in the environment *)
    let rec lookup (env : env) (varname : varid) : value =
      match env with
      | [] -> raise (EvalError ("unassigned variable " ^ varname))
      | h::t -> if fst h = varname then !(snd h) else lookup t varname
    ;;

    (* Returns a new environment just like env except that it maps the
       variable varid to loc *)
    let extend (env : env) (varname : varid) (loc : value ref) : env =
      (varname, loc) :: (List.remove_assoc varname env)
    ;;

    (* Returns a printable string representation of an environment *)
    let rec env_to_string (env : env) : string =
      let value_to_string ?(printenvp : bool = true) (v : value) : string =
        match v with
        | Val e -> exp_to_string e
        | Closure (e, env) ->
          if printenvp
            then "(" ^ exp_to_string e ^ ", [" ^ env_to_string env ^ "])"
          else exp_to_string e in
      match env with
      | [] -> ""
      | h::t -> (fst h) ^ " = " ^ (value_to_string !(snd h)) ^ "; " ^
                env_to_string t
    ;;

    (* Returns a printable string representation of a value; the flag
       printenvp determines whether to include the environment in the
       string representation when called on a closure *)
    let value_to_string ?(printenvp : bool = true) (v : value) : string =
      match v with
      | Val e -> exp_to_string e
      | Closure (e, env) ->
        if printenvp
          then "(" ^ exp_to_string e ^ ", [" ^ env_to_string env ^ "])"
        else exp_to_string e
    ;;

  end
;;

(* The evaluation function: Returns the result of type `value` of
   evaluating the expression `exp` in the environment `env`. In this
   initial implementation, we just convert the expression unchanged to
   a value and return it. *)


(** The external evaluator, which can be either the identity function,
    the substitution model version or the dynamic or lexical
    environment model version. *)

let eval_t exp _env : Env.value = Val exp ;;

let rec eval_s (exp : expr) _env : Env.value =
  match exp with
  | Num _ | Bool _ -> Val exp
  | Var id -> raise (EvalError ("unbound variable " ^ id))
  | Raise -> raise EvalException
  | Unassigned -> raise (EvalError "unassigned variable")
  | Unop (_op, e) ->
    (match eval_s e _env with
     | Val e' | Closure (e', _) ->
       (match e' with
        | Num n -> Val (Num (-n))
        | Var id -> raise (EvalError ("unbound variable " ^ id))
        | Bool b -> raise (EvalError ("cannot negate bool " ^ string_of_bool b))
        | _ -> raise (EvalError (exp_to_string exp ^ " cannot be negated"))))
  | Binop (op, e1, e2) ->
    (match eval_s e1 _env, eval_s e2 _env with
     | Val e1', Val e2' | Val e1', Closure (e2', _) | Closure (e1', _), Val e2'
     | Closure (e1', _), Closure (e2', _) ->
       (match e1', e2' with
        | Num n, Num m -> (match op with
                           | Plus -> Val (Num (n + m))
                           | Minus -> Val (Num (n - m))
                           | Times -> Val (Num (n * m))
                           | Equals -> Val (Bool (n = m))
                           | LessThan -> Val (Bool (n < m)))
        | Var id, _ -> raise (EvalError ("unbound variable " ^ id))
        | Bool a, Bool b -> (match op with
                             | Plus | Minus | Times -> raise (EvalError
                              (exp_to_string exp ^ " wrong type for operation"))
                             | Equals -> Val (Bool (a = b))
                             | LessThan -> Val (Bool (a < b)))
        | _ -> raise (EvalError (exp_to_string exp ^ "  error"))))
  | Conditional (e1, e2, e3) ->
    (match eval_s e1 _env with
     | Val e | Closure (e, _) ->
       (match e with
        | Bool true -> eval_s e2 _env
        | Bool false -> eval_s e3 _env
        | _ -> raise (EvalError (exp_to_string exp ^ " type error"))))
  | Fun (id, e) -> Val (Fun (id, e))
  | Let (id, e1, e2) -> eval_s (subst id e1 e2) _env
  | Letrec (id, e1, e2) ->
    eval_s (subst id (subst id (Letrec (id, e1, Var id)) e1) e2) _env
  | App (e1, e2) ->
    (match eval_s e1 _env with
     | Val e' | Closure (e', _) ->
       (match e' with
        | Fun (id, e) -> eval_s (subst id e2 e) _env
        | _ -> raise (EvalError (exp_to_string exp ^ " bad redex"))))
;;

let rec eval_d (exp : expr) (env : Env.env) : Env.value =
  match exp with
  | Num _ | Bool _ -> Val exp
  | Var id -> Env.lookup env id
  | Raise -> raise EvalException
  | Unassigned -> raise (EvalError "unassigned variable")
  | Unop (_op, e) ->
    (match eval_d e env with
     | Val e' | Closure (e', _) ->
       (match e' with
        | Num n -> Val (Num (-n))
        | Bool b -> raise (EvalError ("can't negate bool " ^ string_of_bool b))
        | Var id -> raise (EvalError ("unbound variable " ^ id))
        | _ -> raise (EvalError "cannot be negated")))
  | Binop (op, e1, e2) ->
    (match eval_d e1 env, eval_d e2 env with
      | Val e1', Val e2' ->
        (match e1', e2' with
        | Num n, Num m -> (match op with
                           | Plus -> Val (Num (n + m))
                           | Minus -> Val (Num (n - m))
                           | Times -> Val (Num (n * m))
                           | Equals -> Val (Bool (n = m))
                           | LessThan -> Val (Bool (n < m)))
        | Var id, _ -> raise (EvalError ("unbound variable " ^ id))
        | Bool a, Bool b -> (match op with
                             | Plus | Minus | Times -> raise (EvalError
                              (exp_to_string exp ^ " wrong type for operation"))
                             | Equals -> Val (Bool (a = b))
                             | LessThan -> Val (Bool (a < b)))
        | _ -> raise (EvalError (exp_to_string exp ^ " error")))
      | _ -> raise (EvalError "no closures in dynamic scoping"))
  | Conditional (e1, e2, e3) ->
    (match eval_d e1 env with
     | Val e | Closure (e, _) ->
       (match e with
        | Bool true -> eval_d e2 env
        | Bool false -> eval_d e3 env
        | _ -> raise (EvalError (exp_to_string exp ^ " type error"))))
  | Fun (id, e) -> Val (Fun (id, e))
  | Let (id, e1, e2) ->
    eval_d e2 (Env.extend env id (ref ((eval_d e1 env))))
  | Letrec (id, e1, e2) ->
    let temp_val = ref (Env.Val Unassigned) in
    let env_n = Env.extend env id temp_val in
    temp_val := eval_d e1 env_n;
    eval_d e2 env_n
  | App (e1, e2) ->
    (match eval_d e1 env with
     | Val e' | Closure (e', _) ->
       (match e' with
        | Fun (id, e) ->
          eval_d e (Env.extend env id (ref ((eval_d e2 env))))
        | _ -> raise (EvalError ("argument" ^
          exp_to_string e1 ^ "is not a function - cannot be applied"))))
;;

let rec eval_l (exp : expr) (env : Env.env) : Env.value =
  match exp with
  | Num _ | Bool _ -> Env.close exp env
  | Var id -> Env.lookup env id
  | Raise -> raise EvalException
  | Unassigned -> raise (EvalError "unassigned variable")
  | Unop (_op, e) ->
    (match eval_l e env with
     | Val e' | Closure (e', _) ->
       (match e' with
        | Num n -> Env.close (Num (-n)) env
        | Bool b -> raise (EvalError ("can't negate bool " ^ string_of_bool b))
        | Var id -> raise (EvalError ("unbound variable " ^ id))
        | _ -> raise (EvalError "cannot be negated")))
  | Binop (op, e1, e2) ->
    (match eval_l e1 env, eval_l e2 env with
      | Closure (e1', _), Closure (e2', _) ->
        (match e1', e2' with
        | Num n, Num m -> (match op with
                           | Plus -> Env.close (Num (n + m)) env
                           | Minus -> Env.close (Num (n - m)) env
                           | Times -> Env.close (Num (n * m)) env
                           | Equals -> Env.close (Bool (n = m)) env
                           | LessThan -> Env.close (Bool (n < m)) env)
        | Var id, _ -> raise (EvalError ("unbound variable " ^ id))
        | Bool a, Bool b -> (match op with
                             | Plus | Minus | Times -> raise (EvalError
                              (exp_to_string exp ^ " wrong type for operation"))
                             | Equals -> Env.close (Bool (a = b)) env
                             | LessThan -> Env.close (Bool (a < b)) env)
        | _ -> raise (EvalError (exp_to_string exp ^ " error")))
      | _ -> raise (EvalError "unclosed environment"))
  | Conditional (e1, e2, e3) ->
    (match eval_l e1 env with
     | Val _ -> raise (EvalError "unclosed environment")
     | Closure (e, env') ->
       (match e with
        | Bool true -> eval_l e2 env'
        | Bool false -> eval_l e3 env'
        | _ -> raise (EvalError (exp_to_string exp ^ " type error"))))
  | Fun (id, e) -> Env.close (Fun (id, e)) env
  | Let (id, e1, e2) ->
    eval_l e2 (Env.extend env id (ref ((eval_l e1 env))))
  | Letrec (id, e1, e2) ->
    let temp_val = ref (Env.Val Unassigned) in
    let env_n = Env.extend env id temp_val in
    temp_val := eval_l e1 env_n;
    eval_l e2 env_n
  | App (e1, e2) ->
    (match eval_l e1 env with
     | Val _ -> raise (EvalError "unclosed environment")
     | Closure (e', env') ->
       (match e' with
        | Fun (id, e) ->
          eval_l e (Env.extend env' id (ref ((eval_l e2 env))))
        | _ ->
          raise (EvalError "argument is not a function - cannot be applied")))
;;

let evaluate = eval_l ;;
