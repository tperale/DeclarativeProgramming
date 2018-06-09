:- module(symbols, [empty_symtable/1, add_variable_to_symtable/4, variable_exist_in_symtable/3, reorder_first_in_symtable/3, apply_to_all_symboles/3]).

empty_symtable(X) :- X = [].

add_variable_to_symtable(Symboles, VarName, Var, SymbolesOut) :-
  ( variable_exist_in_symtable(Symboles, VarName, _)
    -> throw(variable_already_exist)
    ; SymbolesOut = [(VarName, Var) | Symboles]
  ).

reorder_first_in_symtable(X, none, X).
reorder_first_in_symtable(Symboles, VarName, SymbolesOut) :-
 select((VarName, Var), Symboles, Rest),
 SymbolesOut = [(VarName, Var) | Rest].

variable_exist_in_symtable(Symboles, VarName, Var) :- member((VarName, Var), Symboles).

apply_to_all_symboles([], _, _).
apply_to_all_symboles([(_, Var) | Symboles], Comp, With) :-
  call(Comp, Var, With),
  apply_to_all_symboles(Symboles, Comp, With).

:- begin_tests(symbols).

test(creation) :-
  empty_symtable([]).

test(add) :-
  add_variable_to_symtable([], "a", Var, [("a", Var)]).

test(existance) :-
  variable_exist_in_symtable([("a", Var)], "a", Var).

test(reorder) :-
  reorder_first_in_symtable([("a", _), ("b", _)], none, [("a", _), ("b", _)]),
  reorder_first_in_symtable([("a", _), ("b", _)], "b", [("b", _), ("a", _)]).

test(all) :-
  apply_to_all_symboles([("b", 1), ("a", 1)], <, 2).
