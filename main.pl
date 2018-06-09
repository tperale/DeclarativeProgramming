:- use_module(library(clpfd)).
:- use_module(symbols).

% --
% -- MAIN PROGRAM
% --
number(X) --> [X], {integer(X), !}.
number(Int) --> [X], {string(X), number_string(Int, X), !}.

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
range(Var, Min, Max) --> range_start, number(X), range_connect, number(Y), { Min is min(X,Y), Max is max(X, Y), Var in Min..Max }.

% Validation of the variable name
variable_name(X) --> [X], {string_length(X, 1), char_type(X, alpha), char_type(X, lower)}.

variable_decl(Var, range(X, Y), Sin, Sout) --> variable_decl_sym, variable_name(VarName), range(Var, X, Y), { add_variable_to_symtable(Sin, VarName, Var, Sout) }.

% -- Expression declaration
term(X, _) --> number(X). % TODO return real numbers
term(Var, Sin) --> variable_name(X), { variable_exist_in_symtable(Sin, X, Var) }. % Retrieve prolog variable from variable name

% Division
division_sym --> ["/"].
division(X, Y, Sin) --> term(X, Sin), division_sym, expr(Y, Sin).
division(X, Y, Sin) --> ["the", "quotient", "of"], term(X, Sin), ["and"], expr(Y, Sin).
division(X, Y, Sin) --> ["the", "dividend", "of"], term(X, Sin), ["and"], expr(Y, Sin).

% Multiplication
multiplication_sym --> ["*"].
multiplication_sym --> ["times"].
multiplication(X, Y, Sin) --> term(X, Sin), multiplication_sym, expr(Y, Sin).
multiplication(X, Y, Sin) --> ["the", "product", "of"], term(X, Sin), ["and"], expr(Y, Sin).

% Addition
addition_sym --> ["+"].
addition_sym --> ["plus"].
addition(X, Y, Sin) --> term(X, Sin), addition_sym, expr(Y, Sin).

% Substraction
substraction_sym --> ["-"].
substraction_sym --> ["minus"].
substraction(X, Y, Sin) --> term(X, Sin), substraction_sym, expr(Y, Sin).

expr(X, Sin) --> term(X, Sin).
expr(X // Y, Sin) --> division(X, Y, Sin).
expr(X * Y, Sin) --> multiplication(X, Y, Sin).
expr(X - Y, Sin) --> substraction(X, Y, Sin).
expr(X + Y, Sin) --> addition(X, Y, Sin).

% -- Assignation
assignation_sym --> ["equals"].
assignation_sym --> ["is"].
assignation_sym --> ["contains"].
assignation_sym --> ["holds"].

comparaison(Var, Sin) --> [_, "less", "than", "or", "equal", "to"], expr(X, Sin), { Var #=< X }.
comparaison(Var, Sin) --> [_, "less", "than"], expr(X, Sin), { Var #< X }.
comparaison(Var, Sin) -->[_, "greater", "than", "or", "equal", "to"], expr(X, Sin), { Var #=< X }.
comparaison(Var, Sin) --> [_, "greater", "than"], expr(X, Sin), { Var #> X }.
comparaison(Var, Sin) --> assignation_sym, expr(X, Sin), { Var #= X }.

assignation(VarName, Var, Sin) --> variable_decl_sym, variable_name(VarName), comparaison(Var, Sin), { variable_exist_in_symtable(Sin, VarName, Var) }.
% assignation(all, Value, Sin) --> ["All", "these", "variables"], comparaison(Value), {}.
assignation(VarName, Var, Sin) --> ["It"], comparaison(Var, Sin), { [(VarName, Var) | _] = Sin }.

% --
% -- Line parsing part
% --
line(variable_decl(X, Y), Sin, Sout) --> variable_decl(X, Y, Sin, Sout).
% line(assignation(all, Y), Sin, Sout) --> assignation(all, Y), { add_operation_to_all(Sin, Y, Sout) }. % TODO Handle the variable in the assignation expr.
line(assignation(VarName, Var), Sin, Sout) --> assignation(VarName, Var, Sin), { reorder_first_in_symtable(Sin, VarName, Sout) }.
parse(Line, X, Sin, Sout) :-
  phrase(line(X, Sin, Sout), Line).

parse_line(Line, Sin, Sout) :-
  split_string(Line, " ", "", WordList),
  parse(WordList, _, Sin, Sout).

parse_text(Text, Out) :-
  split_string(Text, "\n", " ", LineList),
  empty_symtable(X),
  foldl(parse_line, LineList, X, Out).

% --
% -- TEST
% --
:- begin_tests(parsing).

test(number) :-
  phrase(number(1), [1]),
  phrase(number(1), ["1"]).

test(variable_name) :-
  phrase(variable_name("q"), ["q"]).

test(variable_decl) :-
  phrase(variable_decl(X, range(12, 16), [], [("q", X)]), ["Variable", "q", "lies", "between", 12, "and", 16]),
  phrase(variable_decl(X, range(1, 20), [], [("q", X)]), ["A", "variable", "q", "varies", "from", 1, "to", 20]),
  phrase(variable_decl(X, range(-12, 16), [], [("q", X)]), ["q", "is", "between", -12, "and", 16]),
  phrase(variable_decl(X, range(14, 100), [], [("q", X)]), ["The", "variable", "q", "is", "in", "the", "range", 100, "to", 14]).

test(expr) :-
  phrase(expr(1, []), [1]),
  phrase(expr(1 // 2, []), [1, "/", 2]),
  phrase(expr(1 + 2, []), [1, "+", 2]),
  phrase(expr(1 - 2, []), [1, "minus", 2]),
  phrase(expr(X, [("q", X)]), ["q", "plus", 2]),
  phrase(expr(Y, [("q", Y)]), ["q", "plus", 2, "*", 4]),
  phrase(expr(Z, [("q", Z)]), ["q", "/", 2]).

test(assignation) :-
  phrase(assignation("z", Z, [("z", Z), ("b", _), ("c", _)]), ["The", "variable", "z", "contains", "the", "product", "of", "b", "and", "c"]),
  phrase(assignation("z", ZZ, [("z", ZZ), ("b", _), ("c", _)]), ["z", "contains", "the", "product", "of", "b", "and", "c"]),
  phrase(assignation("x", X, [("x", X), ("a", _), ("b", _)]), ["x", "equals", "a", "plus", "b"]),
  phrase(assignation("x", XX, [("x", XX), ("c", _)]), ["x", "is", "c", "times", 2]).

test(parsing_base) :-
  parse(["Variable", "q", "lies", "between", 12, "and", 16], variable_decl(_, range(12, 16)), [], X1),
  parse(["A", "variable", "z", "is", "in", "the",  "range", 14, "to", -15], variable_decl(_, range(-15, 14)), X1, X2),
  parse(["q", "equals", 1, "plus", "z"], _, X2, X3),
  parse(["The", "variable", "z", "is", "greater", "than", 2, "*", "q", "-", 12], _, X3, [("z", _), ("q", _)]).

test(parsing_it) :-
  parse(["Variable", "q", "lies", "between", 12, "and", 16], variable_decl(_, range(12, 16)), [], X1),
  parse(["A", "variable", "z", "is", "in", "the",  "range", 14, "to", -15], variable_decl(_, range(-15, 14)), X1, X2),
  parse(["It", "is", "greater", "than", 2, "*", "q", "-", 12], _, X2, X3),
  parse(["q", "equals", 1, "plus", "z"], _, X3, [("q", _), ("z", _)]).

test(parsing_base_2) :-
  parse(["Variable", "q", "lies", "between", 12, "and", 16], variable_decl(_, range(12, 16)), [], X1),
  parse(["A", "variable", "z", "is", "in", "the",  "range", 14, "to", -15], variable_decl(_, range(-15, 14)), X1, X2),
  parse(["Variable", "q", "is", "greater", "than", "or", "equal", "to", "z", "/", "1"], _, X2, _).

test(parsing_text_1) :-
  parse_text("Variable q lies between 12 and 16
A variable z is in the range 14 to -15
It is greater than 2 * q - 12
q equals 1 plus z", [("q", _), ("z", _)]).

test(parsing_text_2) :-
  parse_text("The variable x lies between 0 and 10
Variable y varies from 10 to -10
A variable z is in the range 0 to 15
It equals x plus y
y is less than 5 + 2 * x
x is greater than y times 2
Variable y is greater than or equal to the quotient of z and 4", _).

% test(parsing_text_3_with_all) :-
%   parse_text("The variable x lies between 0 and 10
% Variable y varies from 10 to -10
% A variable z is in the range 0 to 15
% It equals x plus y
% All these variables are greater than -20
% y is less than 5 + 2 * x
% x is greater than y times 2
% Variable y is greater than or equal to the quotient of z and 4", Out).
