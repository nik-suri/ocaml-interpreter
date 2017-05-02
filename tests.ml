open Expr ;;
open Evaluation ;;
open Miniml ;;

let test_exp_to_abstract_string () =
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
    "App(Fun(x, Binop(Plus, Var(x), Var(x))), Num(5))")
;;

let test_free_vars () =
  assert (same_vars (free_vars (Num(5))) (vars_of_list []));
  assert (same_vars (free_vars (Var("x"))) (vars_of_list ["x"]));
  assert (same_vars (free_vars (str_to_exp "x + y ;;")) (vars_of_list ["x";"y"]));
  assert (same_vars (free_vars (str_to_exp "fun x -> x - y ;;")) (vars_of_list ["y"]));
  assert (same_vars (free_vars (str_to_exp "fun y -> x - z ;;")) (vars_of_list ["x";"z"]));
  assert (same_vars (free_vars (str_to_exp "let x = fun y -> z in x 5 ;;")) (vars_of_list ["z"]))
;;

let test_subst () =
  assert (subst "x" (Num(5)) (Bool(true)) = Bool true);
  assert (subst "x" (Num(5)) (str_to_exp "x + x ;;") = (str_to_exp "5 + 5 ;;"));
  assert (subst "y" (Num(5)) (str_to_exp "x + x ;;") = (str_to_exp "x + x ;;"));
  assert (subst "x" (Num(5)) (str_to_exp "fun x -> x - 2 ;;") = (str_to_exp "fun x -> x - 2 ;;"));
  assert (subst "x" (Num(5)) (str_to_exp "fun y -> x * x ;;") = (str_to_exp "fun y -> 5 * 5 ;;"));
  assert (subst "y" (Num(5)) (str_to_exp "let f = fun x -> x * y in f 6 ;;") = (str_to_exp "let f = fun x -> x * 5 in f 6 ;;"))
;;

let test_eval (eval : Expr.expr -> Env.env -> Env.value) () =
  assert (eval (str_to_exp "4 + 5 ;;") (Env.create ()) = Val (Num(9)));
  assert (eval (str_to_exp "~(~(~5)) ;;") (Env.create ()) = Val (Num(-5)));
  assert (eval (str_to_exp "(fun x -> x * x) 5 ;;") (Env.create ()) = Val (Num(25)));
  assert (eval (str_to_exp "let f = fun x -> x in f (f 3) ;;") (Env.create ()) = Val (Num(3)));
  assert (eval (str_to_exp "let f = fun x -> x + 1 in f ;;") (Env.create ()) = Val (str_to_exp "fun x -> x + 1 ;;"));
  assert (eval (str_to_exp "let f = fun y -> y + 5 in let y = 3 in let x = fun z -> z + y in f (x 5) ;;") (Env.create ()) = Val (Num(13)));
  assert (eval (str_to_exp "let x = 1 in let f = fun y -> x + y in let x = 2 in f 3 ;;") (Env.create ()) = (if eval = eval_s || eval = eval_l then Val (Num(4)) else Val (Num(5))));
  assert (eval (str_to_exp "let x = 0 in let f = fun y -> x in let x = 42 in f 42 ;;") (Env.create ()) = (if eval = eval_s || eval = eval_l then Val (Num(0)) else Val (Num(42))));
  assert (eval (str_to_exp "let rec f = fun n -> if n = 0 then 1 else n * f (n - 1) in f 5 ;;") (Env.create ()) = Val (Num(120)));
  assert (eval (str_to_exp "let rec f = fun n -> if n = 0 then 0 else n + f (n - 1) in f 5 ;;") (Env.create ()) = Val (Num(15)));
  assert (eval (str_to_exp "let rec f = fun n -> if n = 0 then 0 else (if n = 1 then 1 else f (n - 1) + f (n - 2)) in f 7 ;;") (Env.create ()) = Val (Num(13)))
;;

let run_tests =
  test_exp_to_abstract_string ();
  test_free_vars ();
  test_subst ();
  test_eval eval_s ();
  test_eval eval_d ();
  test_eval eval_l ();
  ()
