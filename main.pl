:- use_module(library(clpfd)).
:- use_module(symbols).
:- use_module(report).

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
range(Var) --> range_start, number(X), range_connect, number(Y), { Min is min(X,Y), Max is max(X, Y), Var in Min..Max }.

% Validation of the variable name
variable_name(X) --> [X], {string_length(X, 1), char_type(X, alpha), char_type(X, lower)}.

variable_decl(Sin, Sout) --> variable_decl_sym, variable_name(VarName), range(Var), { add_variable_to_symtable(Sin, VarName, Var, Sout) }.

% -- Assignation
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
comparaison(#=<) --> [_, "less", "than", "or", "equal", "to"].
comparaison(#<) --> [_, "less", "than"].
comparaison(#>=) -->[_, "greater", "than", "or", "equal", "to"].
comparaison(#>) --> [_, "greater", "than"].
comparaison(#=) --> ["equals"].
comparaison(#=) --> ["is"].
comparaison(#=) --> ["contains"].
comparaison(#=) --> ["holds"].

assignation(Sin, Sout) --> variable_decl_sym, variable_name(VarName), comparaison(CompFun), expr(X, Sin), { variable_exist_in_symtable(Sin, VarName, Var), call(CompFun, Var, X), reorder_first_in_symtable(Sin, VarName, Sout) }.
assignation(Sin, Sout) --> ["All", "these", "variables"], comparaison(CompFun), expr(X, Sin), { apply_to_all_symboles(Sin, CompFun, X), mark_operation_all(Sin, Sout) }.
assignation(Sin, Sin) --> ["It"], comparaison(CompFun), expr(X, Sin), { take_last_symbole(Sin, First), (_, Var) = First, call(CompFun, Var, X) }.

% --
% -- Line parsing part
% --
line_end --> ["."].
line_end --> [].

line(Sin, Sout) --> variable_decl(Sin, Sout), line_end.
line(Sin, Sout) --> assignation(Sin, Sout), line_end.

%% parse(+Line:[string], +Sin, -Sout)
%
% parse/3
%
% @param Line A single sentence from a linear programming description with each words separeted in a list.
% @param Sin The current symbol table to use to parse this line.
% @param Sout The symbol table at the end of the parsing procedure.
parse(Line, Sin, Sout) :-
  phrase(line(Sin, Sout), Line).

%% parse_line(+Line:string, +Sin, -Sout)
%
% parse_line/3 Parse a single line of a whole linear programming description.
%
% @param Line A single sentence from a linear programming description.
% @param Sin The current symbol table to use to parse this line.
% @param Sout The symbol table at the end of the parsing procedure.
parse_line(Line, Sin, Sout) :-
  split_string(Line, " ", "", WordList),
  parse(WordList, Sin, Sout).

%% parse_text(+Text:string, -Out)
%
% parse_text/2 parse a text describing a linear program as stated in the
% assignment .pdf. Each sentence are separeted by a "\n".
%
% @param Text The whole linear program sentences.
% @param Out The symbol table grouping the variables found during the parsing.
parse_text(Text, Out) :-
  split_string(Text, "\n", " ", LineList),
  empty_symtable(X),
  foldl(parse_line, LineList, X, Out).

solve(Text) :-
  parse_text(Text, Out),
  print_report(Out).

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
  empty_symtable(X),
  phrase(variable_decl(X, [[("q", _)], _]), ["Variable", "q", "lies", "between", 12, "and", 16]),
  phrase(variable_decl(X, [[("q", _)], _]), ["A", "variable", "q", "varies", "from", 1, "to", 20]),
  phrase(variable_decl(X, [[("q", _)], _]), ["q", "is", "between", -12, "and", 16]),
  phrase(variable_decl(X, [[("q", _)], _]), ["The", "variable", "q", "is", "in", "the", "range", 100, "to", 14]).

test(expr) :-
  empty_symtable(E),
  phrase(expr(1, E), [1]),
  phrase(expr(1 // 2, E), [1, "/", 2]),
  phrase(expr(1 + 2, E), [1, "+", 2]),
  phrase(expr(1 - 2, E), [1, "minus", 2]),
  phrase(expr(X, [[("q", X)], _]), ["q", "plus", 2]),
  phrase(expr(Y, [[("q", Y)], _]), ["q", "plus", 2, "*", 4]),
  phrase(expr(Z, [[("q", Z)], _]), ["q", "/", 2]).

test(assignation) :-
  phrase(assignation([[("z", Z), ("b", _), ("c", _)], _], [[("z", Z), ("b", _), ("c", _)], _]), ["The", "variable", "z", "contains", "the", "product", "of", "b", "and", "c"]),
  phrase(assignation([[("z", ZZ), ("b", _), ("c", _)], _], [[("z", ZZ), ("b", _), ("c", _)], _]), ["z", "contains", "the", "product", "of", "b", "and", "c"]),
  phrase(assignation([[("x", X), ("a", _), ("b", _)], _], [[("x", X), ("a", _), ("b", _)], _]), ["x", "equals", "a", "plus", "b"]),
  phrase(assignation([[("x", XX), ("c", _)], _], [[("x", XX), ("c", _)], _]), ["x", "is", "c", "times", 2]).

test(parsing_base) :-
  empty_symtable(X),
  parse(["Variable", "q", "lies", "between", 12, "and", 16], X, X1),
  parse(["A", "variable", "z", "is", "in", "the",  "range", 14, "to", -15], X1, X2),
  parse(["q", "equals", 1, "plus", "z"], X2, X3),
  parse(["The", "variable", "z", "is", "greater", "than", 2, "*", "q", "-", 12], X3, [[("z", Z), ("q", Q)], 0]),
  fd_inf(Q, 12),
  fd_sup(Q, 15),
  fd_inf(Z, 11),
  fd_sup(Z, 14).

test(parsing_it) :-
  empty_symtable(X),
  parse(["Variable", "q", "lies", "between", 12, "and", 16], X, X1),
  parse(["A", "variable", "z", "is", "in", "the",  "range", 14, "to", -15], X1, X2),
  parse(["It", "is", "greater", "than", 2, "*", "q", "-", 12], X2, X3),
  parse(["q", "equals", 1, "plus", "z"], X3, [[("q", Q), ("z", Z)], 0]),
  fd_inf(Q, 12),
  fd_sup(Q, 15),
  fd_inf(Z, 11),
  fd_sup(Z, 14).

test(parsing_all) :-
  empty_symtable(X),
  parse(["Variable", "q", "lies", "between", 12, "and", 16], X, X1),
  parse(["A", "variable", "z", "is", "in", "the",  "range", 14, "to", -15], X1, X2),
  parse(["All", "these", "variables","are", "greater", "than", "or", "equal", "to", "z", "/", "1"], X2, [[("z", Z),  ("q", Q)], 1]),
  fd_inf(Q, 12),
  fd_sup(Q, 16),
  fd_inf(Z, -15),
  fd_sup(Z, 14).

test(parsing_text_1) :-
  parse_text("Variable q lies between 12 and 16
A variable z is in the range 14 to -15
It is greater than 2 * q - 12
q equals 1 plus z
All these variables are greater than 12", [[("q", Q), ("z", Z)], 1]),
  fd_inf(Q, 14),
  fd_sup(Q, 15),
  fd_inf(Z, 13),
  fd_sup(Z, 14).

test(parsing_text_2) :-
  parse_text("The variable x lies between 0 and 10
Variable y varies from 10 to -10
A variable z is in the range 0 to 15
It equals x plus y
y is less than 5 + 2 * x
x is greater than y times 2
Variable y is greater than or equal to the quotient of z and 4", [[("y", Y),  ("x", X),  ("z", Z)], 0]),
  fd_inf(Y, 0),
  fd_sup(Y, 4),
  fd_inf(X, 1),
  fd_sup(X, 10),
  fd_inf(Z, 1),
  fd_sup(Z, 14).

test(parsing_text_3_with_all) :-
  parse_text("The variable x lies between 0 and 10
Variable y varies from 10 to -10
A variable z is in the range 0 to 15
It equals x plus y
All these variables are greater than -20
y is less than 5 + 2 * x
x is greater than y times 2
Variable y is greater than or equal to the quotient of z and 4",  [[("y", Y),  ("x", X),  ("z", Z)], 0]),
  fd_inf(Y, 0),
  fd_sup(Y, 4),
  fd_inf(X, 1),
  fd_sup(X, 10),
  fd_inf(Z, 1),
  fd_sup(Z, 14).
