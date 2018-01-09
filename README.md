
# OCaml Interpreter
## by Nikhil Suri

A fully functioning command-line interpreter created for Harvard's CS51 course that I took in Spring 2017. It can evaluate unary and binary operations, conditional statements, functions and recursive functions, and function application statements. Throughout the course, we used OCaml to focus on topics such as recursion, types, scope, and efficiency. This project incorporates all of those elements. See below for a description of all files included. I did not write this entire application on my own. See the "initial" commit to view the initial distribution code provided by the CS51 staff.

* **evaluation.ml:** Defining different models of evaluation.
* **expr.ml:** Defining expression types, toString functions, and the substitution model.
* **expr.mli:** Basic abstract type definitions.
* **miniml.ml:** Runs the main application.
* **miniml_lex.mll:** Lexical Analyzer.
* **miniml_parse.mly:** Command-line parser.
* **tests.ml:** Tests written by me.
* **writeup.md:** Describing the lexically scoped evaluator.

