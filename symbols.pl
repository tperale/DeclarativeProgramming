:- module(symbols, [empty_symtable/1, add_variable_to_symtable/4, variable_exist_in_symtable/3, reorder_first_in_symtable/3, apply_to_all_symboles/3, take_first_symbole/2]).

%% empty_symtable(-X:[])
%
% empty_symtable/1 Create a new symbole table.
%
% @param X The newly created symbole table.
empty_symtable(X) :- X = [].

%% add_variable_to_symtable(+Symboles:[(string, _)], +VarName:string, -Var, -SymbolesOut:[(string, _)])
%
% add_variable_to_symtable/4 In a provided symtable add a new variable entry to the symbol table.
%
% @param Symboles A symbol table from which we will add new variable.
% @param VarName The new variable name found during the parsing.
% @param Var The variable associated to {VarName}.
% @param SymbolesOut The symbol table after adding the new variable.
add_variable_to_symtable(Symboles, VarName, Var, SymbolesOut) :-
  ( variable_exist_in_symtable(Symboles, VarName, _)
    -> throw(variable_already_exist)
    ; SymbolesOut = [(VarName, Var) | Symboles]
  ).

%% reorder_first_in_symtable(+Symboles:[(string, _)], +VarName:string, -SymbolesOut:[(string, _)])
%
% reorder_first_in_symtable/3 Put the @{VarName} associated entry in the symbol table in the first place of the symbol table.
%
% @param Symboles A symbol table from which we will add new variable.
% @param VarName The new variable name found during the parsing.
% @param SymbolesOut The symbol table after adding the new variable.
reorder_first_in_symtable(X, none, X).
reorder_first_in_symtable(Symboles, VarName, SymbolesOut) :-
 select((VarName, Var), Symboles, Rest),
 SymbolesOut = [(VarName, Var) | Rest].

%% variable_exist_in_symtable(+Symboles:[(string, _)], +VarName:string, -SymbolesOut:[(string, _)])
%
% variable_exist_in_symtable/3 Verify @{VarName} exist in the symbol table.
%
% @param Symboles A symbol table from which we will add new variable.
% @param VarName The new variable name found during the parsing.
% @param SymbolesOut The symbol table after adding the new variable.
variable_exist_in_symtable(Symboles, VarName, Var) :- member((VarName, Var), Symboles).

%% apply_to_all_symboles(+Symboles:[(string, _)], +Comp, +With)
%
% apply_to_all_symboles/3 Apply a comparaison @{Comp} with @{With} to every entry of @{Symboles}.
%
% @param Symboles A symbol table from which we will add new variable.
% @param Comp The function used to apply on member of the symbol table.
% @param With Parameter to pass to the @{Comp}.
apply_to_all_symboles([], _, _).
apply_to_all_symboles([(_, Var) | Symboles], Comp, With) :-
  call(Comp, Var, With),
  apply_to_all_symboles(Symboles, Comp, With).

take_first_symbole(Symboles, Symbole) :-
  [Symbole | _ ] = Symboles.

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
