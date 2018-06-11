:- module(symbols, [empty_symtable/1, add_variable_to_symtable/4, variable_exist_in_symtable/3, reorder_first_in_symtable/3, apply_to_all_symboles/3, take_last_symbole/2, mark_operation_all/2]).

%% empty_symtable(-X:[[], 0])
%
% empty_symtable/1 Create a new symbole table.
%
% @param X The newly created symbole table.
empty_symtable(X) :- X = [[], 0].

%% add_variable_to_symtable(+Symboles:[[(string, _)], Int], +VarName:string, -Var, -SymbolesOut:[(string, _)])
%
% add_variable_to_symtable/4 In a provided symtable add a new variable entry to the symbol table.
%
% @param Symboles A symbol table from which we will add new variable.
% @param VarName The new variable name found during the parsing.
% @param Var The variable associated to {VarName}.
% @param SymbolesOut The symbol table after adding the new variable.
add_variable_to_symtable([Symboles, _], VarName, Var, SymbolesOut) :-
  ( variable_exist_in_symtable(Symboles, VarName, _)
    -> throw(variable_already_exist)
    ; SymbolesOut = [[(VarName, Var) | Symboles], 0]
  ).

%% reorder_first_in_symtable(+Symboles:[(string, _)], +VarName:string, -SymbolesOut:[(string, _)])
%
% reorder_first_in_symtable/3 Put the @{VarName} associated entry in the symbol table in the first place of the symbol table.
%
% @param Symboles A symbol table from which we will add new variable.
% @param VarName The new variable name found during the parsing.
% @param SymbolesOut The symbol table after adding the new variable.
reorder_first_in_symtable([Symboles, _], VarName, SymbolesOut) :-
  select((VarName, Var), Symboles, Rest),
  SymbolesOut = [[(VarName, Var) | Rest], 0],
  !.

%% variable_exist_in_symtable(+Symboles:[[(string, _)], Int], +VarName:string, -SymbolesOut:[(string, _)])
%
% variable_exist_in_symtable/3 Verify @{VarName} exist in the symbol table.
%
% @param Symboles A symbol table from which we will add new variable.
% @param VarName The new variable name found during the parsing.
% @param SymbolesOut The symbol table after adding the new variable.
variable_exist_in_symtable([Symboles, _], VarName, Var) :- member((VarName, Var), Symboles).

%% apply_to_all_symboles(+Symboles:[[(string, _)], Int], +Comp, +With)
%
% apply_to_all_symboles/3 Apply a comparaison @{Comp} with @{With} to every entry of @{Symboles}.
%
% @param Symboles A symbol table from which we will add new variable.
% @param Comp The function used to apply on member of the symbol table.
% @param With Parameter to pass to the @{Comp}.
apply_to_all_symboles_([], _, _).
apply_to_all_symboles_([(_, Var) | Symboles], Comp, With) :-
  call(Comp, Var, With),
  apply_to_all_symboles_(Symboles, Comp, With).

apply_to_all_symboles([Sym, _], Comp, With) :-
  apply_to_all_symboles_(Sym, Comp, With).

%% was_last_operation_all(+Symboles:[[(string, _)], Int])
%
% was_last_operation_all/1 Verify if in the symtable recorder the last operation as a "all".
%
% @param Symboles A symbol table from which we will add new variable.
was_last_operation_all([_, 1]) :- true.
was_last_operation_all([_, 0]) :- false.

%% mark_operation_all(+Symboles:[[(string, _)], Int], -SymbolesOut)
%
% mark_operation_all/2 Mark in the symtable we used a "all" operation.
%
% @param Symboles A symbol table from which we will add new variable.
% @param SymbolesOut The symbol table after adding the new variable.
mark_operation_all([Symboles, _], SymbolesOut) :-
  SymbolesOut = [Symboles, 1].

%% take_last_symbole(+Symboles:[[(string, _)], Int], -Symbole)
%
% take_last_symbole/2 Retrieve the last used symbol if the last operation was not a "all".
%
% @param Symboles A symbol table from which we will add new variable.
% @param Symbole The symbol table after adding the new variable.
take_last_symbole(Symboles, Symbole) :-
  ( was_last_operation_all(Symboles)
    -> throw(last_operation_was_all)
    ; [[Symbole | _], _] = Symboles
  ).

:- begin_tests(symbols).

test(creation) :-
  empty_symtable([[], 0]).

test(add) :-
  empty_symtable(X),
  add_variable_to_symtable(X, "a", Var, [[("a", Var)], 0]).

test(existance) :-
  variable_exist_in_symtable([[("a", Var)], _], "a", Var).

test(reorder) :-
  reorder_first_in_symtable([[("a", _), ("b", _)], _], "b", [[("b", _), ("a", _)], _]).

test(all) :-
  apply_to_all_symboles([[("b", 1), ("a", 1)], _], <, 2).
