:- module(symbols, [empty_symtable/1, add_variable_to_symtable/4, variable_exist_in_symtable/3, reorder_first_in_symtable/3]).

empty_symtable(X) :- X = [].

add_variable_to_symtable(Symboles, VarName, Var, SymbolesOut) :-
  ( variable_exist_in_symtable(Symboles, VarName, _)
    -> throw(variable_already_exist)
    ; SymbolesOut = [(VarName, Var) | Symboles]
  ).

reorder_first_in_symtable(Symboles, VarName, SymbolesOut) :-
 select((VarName, Var), Symboles, Rest),
 SymbolesOut = [(VarName, Var) | Rest].

variable_exist_in_symtable(Symboles, VarName, Var) :- member((VarName, Var), Symboles).

:- begin_tests(symbols).

test(creation) :-
  empty_symtable([]).

test(add) :-
  add_variable_to_symtable([], "a", Var, [("a", Var)]).

test(existance) :-
  variable_exist_in_symtable([("a", Var)], "a", Var).

test(reorder) :-
  reorder_first_in_symtable([("a", _), ("b", _)], "b", [("b", _), ("a", _)]).
