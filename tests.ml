open Expr ;;
open Evaluation ;;
open Miniml ;;

let v2e (v : Env.value) : Expr.expr =
  match v with
  | Env.Val e | Env.Closure (e, _) -> e
;;

let test_to_strings () =
  (* exp_to_abstract_string tests *)
  assert (exp_to_abstract_string (Num 3) = "Num(3)");
  assert (exp_to_abstract_string (Var "x") = "Var(x)");
  assert (exp_to_abstract_string (Bool true) = "Bool(true)");
  assert (exp_to_abstract_string
    (Unop(Negate, Num(3))) = "Unop(Negate, Num(3))");
  assert (exp_to_abstract_string
    (Binop(Plus, Num(3), Var("x"))) = "Binop(Plus, Num(3), Var(x))");
  assert (exp_to_abstract_string
    (Conditional(Binop(LessThan, Num(4), Num(3)), Num(10), Var("y"))) =
    "Conditional(Binop(LessThan, Num(4), Num(3)), Num(10), Var(y))");
  assert (exp_to_abstract_string (Fun("x", Binop(Times, Var("x"), Var("x")))) =
    "Fun(x, Binop(Times, Var(x), Var(x)))");
  assert (exp_to_abstract_string (Let("y", Num(5), Binop(Minus, Var("y"),
    Num(2)))) = "Let(y, Num(5), Binop(Minus, Var(y), Num(2)))");
  assert (exp_to_abstract_string
    (Letrec("f", Fun("x", Conditional(Binop(Equals, Var("x"), Num(0)), Num(1),
    Binop(Times, Var("x"), App(Var("f"), Binop(Minus, Var("x"), Num(1)))))),
    App(Var("f"), Num(4)))) =
    "Letrec(f, Fun(x, Conditional(Binop(Equals, Var(x), Num(0)), Num(1), " ^
    "Binop(Times, Var(x), App(Var(f), Binop(Minus, Var(x), Num(1)))))), " ^
    "App(Var(f), Num(4)))");
  assert (exp_to_abstract_string
    (App(Fun("x", Binop(Plus, Var("x"), Var("x"))), Num(5))) =
    "App(Fun(x, Binop(Plus, Var(x), Var(x))), Num(5))");

  (* value_to_string/env_to_string tests (can be tested together because
     value_to_string calls env_to_string in the place of a closure) *)
  assert (Env.value_to_string (Env.Val (Num(5))) = "5");
  assert (Env.value_to_string (Env.Val (Binop(Plus,Var("x"),Num(4)))) = "x + 4");
  assert (Env.value_to_string (eval_l (str_to_exp "let f = 3 in x + 3 ;;") (Env.create ())) = "(6, [f = (3, []); ])");
  assert (Env.value_to_string (eval_l (str_to_exp "let y = 3 in let f = fun x -> x + y in f 3 ;;") (Env.create ())) = "(6, [x = (3, [f = ( fun x -> x + y, [y = (3, []); ]); y = (3, []); ]); y = (3, []); ])")
  (* assert (Env.value_to_string (eval_l (str_to_exp "") )) *)
;;

let test_free_vars () =
  assert (same_vars (free_vars (Num(5))) (vars_of_list []));
  assert (same_vars (free_vars (Var("x"))) (vars_of_list ["x"]));
  assert (same_vars (free_vars (str_to_exp "x + y ;;")) (vars_of_list ["x";"y"]));
  assert (same_vars (free_vars (str_to_exp "fun x -> x - y ;;")) (vars_of_list ["y"]));
  assert (same_vars (free_vars (str_to_exp "fun y -> x - z ;;")) (vars_of_list ["x";"z"]));
  assert (same_vars (free_vars (str_to_exp "let x = fun y -> z in x 5 ;;")) (vars_of_list ["z"]));
  assert (same_vars (free_vars (str_to_exp "let rec f = fun n -> if n = 0 then 1 else n * f (n - 1) in f 5 ;;")) (vars_of_list []))
;;

let test_subst () =
  assert (subst "x" (Num(5)) (Bool(true)) = Bool true);
  assert (subst "x" (Num(5)) (str_to_exp "x + x ;;") = (str_to_exp "5 + 5 ;;"));
  assert (subst "y" (Num(5)) (str_to_exp "x + x ;;") = (str_to_exp "x + x ;;"));
  assert (subst "x" (Num(5)) (str_to_exp "fun x -> x - 2 ;;") = (str_to_exp "fun x -> x - 2 ;;"));
  assert (subst "x" (Num(5)) (str_to_exp "fun y -> x * x ;;") = (str_to_exp "fun y -> 5 * 5 ;;"));
  assert (subst "y" (Num(5)) (str_to_exp "let f = fun x -> x * y in f 6 ;;") = (str_to_exp "let f = fun x -> x * 5 in f 6 ;;"));
  assert (subst "z" (Binop(Plus,Num(4),Var("y"))) (str_to_exp "let f = fun x -> x + z in f 7 ;;") = (str_to_exp "let f = fun x -> x + (4 + y) in f 7 ;;"));
  assert (subst "y" (Num(5)) (str_to_exp "let rec f = fun n -> if n = 0 then y else n * f (n - 1) in f 5 ;;") = (str_to_exp "let rec f = fun n -> if n = 0 then 5 else n * f (n - 1) in f 5 ;;"))
;;

let test_closure () =
  let env = Env.create () in
  assert (Env.close (Num(3)) (Env.create()) = Env.Closure(Num(3), env));
;;

(* Uses Env.extend to avoid raising errors (looking up a variable in an empty
  environment *)
let test_lookup_extend () =
  let env = Env.extend (Env.create ()) "x" (ref (Env.Val (Num(3)))) in
  assert ((Env.lookup env "x") = Env.Val (Num(3)));
  let env1 = Env.extend env "y" (ref (Env.Val (Binop(Plus,Num(3),Num(4))))) in
  assert ((Env.lookup env1 "y") = Env.Val (Binop(Plus,Num(3),Num(4))));
  let env2 = Env.extend env1 "f" (ref (Env.Val (str_to_exp "fun x -> x * 10 ;;"))) in
  assert ((Env.lookup env2 "f") = Env.Val (str_to_exp "fun x -> x * 10 ;;"))
;;

let test_eval_sl (eval : Expr.expr -> Env.env -> Env.value) () =
  assert (v2e (eval (str_to_exp "4 + 5 ;;") (Env.create ())) = Num(9));
  assert (v2e (eval (str_to_exp "~(~(~5)) ;;") (Env.create ())) = Num(-5));
  assert (v2e (eval (str_to_exp "(fun x -> x * x) 5 ;;") (Env.create ())) = Num(25));
  assert (v2e (eval (str_to_exp "let f = fun x -> x in f (f 3) ;;") (Env.create ())) = Num(3));
  assert (v2e (eval (str_to_exp "let f = fun x -> x + 1 in f ;;") (Env.create ())) = (str_to_exp "fun x -> x + 1 ;;"));
  assert (v2e (eval (str_to_exp "let f = fun y -> y + 5 in let y = 3 in let x = fun z -> z + y in f (x 5) ;;") (Env.create ())) = Num(13));
  assert (v2e (eval (str_to_exp "let f = fun x -> fun y -> x + y in (f 10) 5 ;;") (Env.create ())) = Num(15));
  assert (v2e (eval (str_to_exp "let x = 1 in let f = fun y -> x + y in let x = 2 in f 3 ;;") (Env.create ())) = Num(4));
  assert (v2e (eval (str_to_exp "let x = 0 in let f = fun y -> x in let x = 42 in f 42 ;;") (Env.create ())) = Num(0));
  assert (v2e (eval (str_to_exp "let f = 3 in let y = 4 in let z = 6 in let x = fun x -> fun y -> fun z -> x + y + z in (((x f) y) z) ;;") (Env.create())) = Num(13));
  assert (v2e (eval (str_to_exp "let rec f = fun n -> if n = 0 then 1 else n * f (n - 1) in f 5 ;;") (Env.create ())) = Num(120));
  assert (v2e (eval (str_to_exp "let rec f = fun n -> if n = 0 then 0 else n + f (n - 1) in f 5 ;;") (Env.create ())) = Num(15));
  assert (v2e (eval (str_to_exp "let rec f = fun n -> if n = 0 then 0 else (if n = 1 then 1 else f (n - 1) + f (n - 2)) in f 7 ;;") (Env.create ())) = Num(13))
;;

let test_eval_d () =
  assert (eval_d (str_to_exp "4 + 5 ;;") (Env.create ()) = Val (Num(9)));
  assert (eval_d (str_to_exp "~(~(~5)) ;;") (Env.create ()) = Val (Num(-5)));
  assert (eval_d (str_to_exp "(fun x -> x * x) 5 ;;") (Env.create ()) = Val (Num(25)));
  assert (eval_d (str_to_exp "let f = fun x -> x in f (f 3) ;;") (Env.create ()) = Val (Num(3)));
  assert (eval_d (str_to_exp "let f = fun x -> x + 1 in f ;;") (Env.create ()) = Val (str_to_exp "fun x -> x + 1 ;;"));
  assert (eval_d (str_to_exp "let f = fun y -> y + 5 in let y = 3 in let x = fun z -> z + y in f (x 5) ;;") (Env.create ()) = Val (Num(13)));
  assert (eval_d (str_to_exp "let x = 1 in let f = fun y -> x + y in let x = 2 in f 3 ;;") (Env.create ()) = (Val (Num(5))));
  assert (eval_d (str_to_exp "let x = 0 in let f = fun y -> x in let x = 42 in f 42 ;;") (Env.create ()) = (Val (Num(42))));
  assert (eval_d (str_to_exp "let rec f = fun n -> if n = 0 then 1 else n * f (n - 1) in f 5 ;;") (Env.create ()) = Val (Num(120)));
  assert (eval_d (str_to_exp "let rec f = fun n -> if n = 0 then 0 else n + f (n - 1) in f 5 ;;") (Env.create ()) = Val (Num(15)));
  assert (eval_d (str_to_exp "let rec f = fun n -> if n = 0 then 0 else (if n = 1 then 1 else f (n - 1) + f (n - 2)) in f 7 ;;") (Env.create ()) = Val (Num(13)))
;;

let run_tests =
  test_to_strings ();
  test_free_vars ();
  test_subst ();
  test_closure ();
  test_lookup_extend ();
  test_eval_sl eval_s ();
  test_eval_d ();
  test_eval_sl eval_l ();
  ()
