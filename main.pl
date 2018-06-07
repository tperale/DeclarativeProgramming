% Exemple of phrases
%
% The variable x lies between 0 and 10.
% Variable x varies from 1 to 20.
% A variable x is in the range 100 to 14.
% y is between -2 and 25.
% x equals a plus b.
% x is c times 2.
% The variable z contains the product of b and c.
% y is less than 5 + 2 * q.
% All these variables are greater than 5.
% Variable q is greater than or equal to the quotient of z and 2.
% Variable w holds the dividend of z and 2.
% It is greater than q.

% Exemple of input
%
% The variable x lies between 0 and 10.
% Variable y varies from 10 to -10.
% A variable z is in the range 0 to 15.
% It equals x plus y.
% All these variables are greater than -20
% y is less than 5 + 2 * x.
% x is greater than y times 2.
% Variable y is greater than or equal to the quotient of z and 4.

:- use_module(symbols).

% --
% -- MAIN PROGRAM
% --
number(X) --> [X], {integer(X)}.
number(X) --> [X], {string(X), number_string(_, X)}.

% -- Variable declaration
variable_decl_sym --> ["The", "variable"].
variable_decl_sym --> ["Variable"].
variable_decl_sym --> ["A", "variable"].
variable_decl_sym --> [].

range_start --> ["lies", "between"].
range_start --> ["varies", "from"].
range_start --> ["is", "in", "the", "range"].
range_start --> ["is", "between"].
range_connect --> ["and"].
range_connect --> ["to"].
range(X, Y) --> range_start, number(X), range_connect, number(Y), { X < Y }.
range(Y, X) --> range_start, number(X), range_connect, number(Y), { X > Y }.

variable_name(X) --> [X], {string_length(X, 1), char_type(X, alpha), char_type(X, lower)}.
variable_decl(Var, range(X, Y)) --> variable_decl_sym, variable_name(Var), range(X, Y). % TODO Create variable in tree and add the range to it

% -- Assignation
assignation_sym --> ["equals"].
assignation_sym --> ["is"].
assignation_sym --> ["contains"].
assignation_sym --> ["holds"].

comparaison(lte(X)) --> [_, "less", "than", "or", "equal", "to"], expr(X).
comparaison(lt(X)) --> [_, "less", "than"], expr(X).
comparaison(gte(X)) -->[_, "greater", "than", "or", "equal", "to"], expr(X).
comparaison(gt(X)) --> [_, "greater", "than"], expr(X).
comparaison(X) --> assignation_sym, expr(X).

assignation(Var, Value) --> variable_decl_sym, variable_name(Var), comparaison(Value). % TODO Verify existance of variable in tree and add Y to the tree
assignation(all, Value) --> ["All", "these", "variables"], comparaison(Value). % TODO add Y to every component of the tree
assignation(it, Value) --> ["It"], comparaison(Value). % TODO add Y to every component of the tree

% -- Expression declaration

term(X) --> number(X).
term(X) --> variable_name(X).

% Division
division_sym --> ["/"].
division(X, Y) --> term(X), division_sym, expr(Y).
division(X, Y) --> ["the", "quotient", "of"], term(X), ["and"], expr(Y).
division(X, Y) --> ["the", "dividend", "of"], term(X), ["and"], expr(Y).

% Multiplication
multiplication_sym --> ["*"].
multiplication_sym --> ["times"].
multiplication(X, Y) --> term(X), multiplication_sym, expr(Y).
multiplication(X, Y) --> ["the", "product", "of"], term(X), ["and"], expr(Y).

% Addition
addition_sym --> ["+"].
addition_sym --> ["plus"].
addition(X, Y) --> term(X), addition_sym, expr(Y).

% Substraction
substraction_sym --> ["-"].
substraction_sym --> ["minus"].
substraction(X, Y) --> term(X), substraction_sym, expr(Y).

expr(X) --> term(X).
expr(division(X, Y)) --> division(X, Y).
expr(multiplication(X, Y)) --> multiplication(X, Y).
expr(substraction(X, Y)) --> substraction(X, Y).
expr(addition(X, Y)) --> addition(X, Y).

% --
% -- Line parsing part
% --
line(variable_decl(X, Y), Sin, Sout) --> variable_decl(X, Y), { add_variable_to_symtable(Sin, X, SoutTmp), add_operation_to_symtable(SoutTmp, X, Y, Sout) }.
line(assignation(all, Y), Sin, Sout) --> assignation(all, Y), { add_operation_to_all(Sin, Y, Sout) }. % TODO Handle the variable in the assignation expr.
line(assignation(it, Y), Sin, Sout) --> assignation(it, Y), { add_operation_to_last(Sin, Y, Sout) }. % TODO Handle the variable in the assignation expr.
line(assignation(X, Y), Sin, Sout) --> assignation(X, Y), { variable_exist_in_symtable(Sin, X), add_operation_to_symtable(Sin, X, Y, Sout) }. % TODO Handle the variable in the assignation expr.
parse(Line, X, Sin, Sout) :-
  phrase(line(X, Sin, Sout), Line).

% --
% -- TEST
% --
:- begin_tests(parsing).

test(number) :-
  phrase(number(1), [1]),
  phrase(number("1"), ["1"]).

test(variable_name) :-
  phrase(variable_name("q"), ["q"]),
  variable_decl_sym(["Variable"], []),
  phrase(variable_decl("q", range(12, 16)), ["Variable", "q", "lies", "between", 12, "and", 16]),
  phrase(variable_decl("q", range(1, 20)), ["A", "variable", "q", "varies", "from", 1, "to", 20]),
  phrase(variable_decl("q", range(-12, 16)), ["q", "is", "between", -12, "and", 16]),
  phrase(variable_decl("q", range(14, 100)), ["The", "variable", "q", "is", "in", "the", "range", 100, "to", 14]).

test(expr) :-
  phrase(expr(1), [1]),
  phrase(expr(division(1, 2)), [1, "/", 2]),
  phrase(expr(addition(1, 2)), [1, "+", 2]),
  phrase(expr(substraction(1, 2)), [1, "minus", 2]),
  phrase(expr(addition("q", 2)), ["q", "plus", 2]),
  phrase(expr(addition("q", multiplication(2, 4) )), ["q", "plus", 2, "*", 4]).

test(assignation) :-
  phrase(assignation("z", multiplication("b", "c")), ["The", "variable", "z", "contains", "the", "product", "of", "b", "and", "c"]),
  phrase(assignation("z", multiplication("b", "c")), ["z", "contains", "the", "product", "of", "b", "and", "c"]),
  phrase(assignation("x", addition("a", "b")), ["x", "equals", "a", "plus", "b"]),
  phrase(assignation("x", multiplication("c", 2)), ["x", "is", "c", "times", 2]),
  phrase(assignation("q", gte(division("z", 2))), ["q", "is", "greater", "than", "or", "equal", "to", "the", "quotient", "of", "z", "and", 2]),
  phrase(assignation("y", lt(addition(5, multiplication(2, "q")))), ["y", "is", "less", "than", 5, "+", 2, "*", "q"]).

test(parsing) :-
  parse(["Variable", "q", "lies", "between", 12, "and", 16], variable_decl("q", range(12, 16)), [], X1),
  parse(["A", "variable", "z", "is", "in", "the",  "range", 0, "to", -15], variable_decl("z", range(-15, 0)), X1, X2),
  parse(["q", "equals", 1, "plus", "z"], assignation("q", addition(1, "z")), X2, X3),
  parse(["The", "variable", "z", "is", "greater", "than", 5, "*", "q", "+", 12], assignation("z", gt(multiplication(5, addition("q", 12)))), X3, [("z", [gt(multiplication(5, addition("q", 12))), range(-15, 0)]),  ("q", [addition(1, "z"), range(12, 16)])]).
