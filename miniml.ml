(* 
			 CS 51 Final Project
		    MiniML -- Read-Eval-Print Loop
			     Spring 2017
*)

module Ev = Evaluation ;;
module MP = Miniml_parse ;;
module ML = Miniml_lex ;;
module Ex = Expr ;;

open Printf ;;

(* str_to_exp: string -> expr
   Returns the expression specified by the string using the Miniml
   parser. *)
let str_to_exp (str: string) : Ex.expr =
  let lexbuf = Lexing.from_string str in
  let exp = MP.input ML.token lexbuf in
  exp
;;

(* repl: unit -> unit
   Read-eval-print loop for MiniML. *)
let repl () =
  (* lexical analyzer buffer from stdin *)
  let lexbuf = Lexing.from_channel stdin in
  (* set up the initial environment *)
  let env = Ev.Env.create () in
  printf "Entering %s...\n" Sys.argv.(0);
  flush stdout;
  while true do
    (try
	(* prompt *)
        printf "<== ";
        flush stdout;
        (* read and parse an expression from the input *)
        let exp = MP.input ML.token lexbuf in
        (* evaluate it *)
        let res = Ev.evaluate exp env in
        (* print the result *)
        printf "==> %s\n" (Ex.exp_to_abstract_string res)
      with
      | Parsing.Parse_error -> printf "xx> parse error\n"
      | Ev.EvalError msg -> printf "xx> evaluation error: %s\n" msg
      | Ev.EvalException -> printf "xx> evaluation exception\n"
      | End_of_file -> printf "Goodbye.\n"; exit 0
    );
    flush stdout
  done
;;
	
(* Run repl if called from command line *)

try
  let _ = Str.search_forward (Str.regexp "miniml\\.\\(byte\\|native\\)") (Sys.argv.(0)) 0 in
  repl ()
with Not_found -> () ;;
