:- module(symbols, [empty_symtable/1, add_variable_to_symtable/3, add_operation_to_symtable/4, variable_exist_in_symtable/2]).

replace(_, _, [], []).
replace(O, R, [O|T], [R|T2]) :- replace(O, R, T, T2).
replace(O, R, [H|T], [H|T2]) :- H \= O, replace(O, R, T, T2).

empty_symtable(SymbolesOut) :- SymbolesOut = [].

add_variable_to_symtable(Symboles, Var, SymbolesOut) :-
  % TODO Should refer to real prolog variable ?
  ( variable_exist_in_symtable(Symboles, Var)
    -> throw(variable_already_exist)
    ; SymbolesOut = [(Var, []) | Symboles]
  ).

add_operation_to_symtable(Symboles, Var, Operation, SymbolesOut) :-
  select((Var, FormerOperations), Symboles, Rest),
  SymbolesOut = [(Var, [Operation | FormerOperations]) | Rest].

variable_exist_in_symtable(Symboles, Var) :- member((Var, _), Symboles).

:- begin_tests(symbols).

test(creation) :-
  empty_symtable([]).

test(add) :-
  add_variable_to_symtable([], "a", [("a", [])]).

test(operation) :-
  add_operation_to_symtable([("a", [])], "a", "foo", [("a", ["foo"])]).

test(member) :-
  variable_exist_in_symtable([("a", [])], "a").