:- module(report, [print_report/1]).
% :- use_module(library(io)).

print_varname([(VarName, _) | []]) :-
  write_term(VarName, []).
print_varname([(VarName, _) | Rest]) :-
  write_term(VarName, []),
  write(","),
  print_varname(Rest).

print_var([]).
print_var([(VarName, Var) | Rest]) :-
  fd_inf(Var, Min),
  fd_sup(Var, Max),
  write_term(Min, []),
  write(" <= "),
  write_term(VarName, []),
  write(" <= "),
  write_term(Max, []),
  writeln(""),
  print_var(Rest).

print_report([Report, _]) :-
  writeln("Report: Solved the problem"),
  writeln("-----------------------------"),
  write("The program found the following variables ["),
  print_varname(Report),
  writeln("]."),
  writeln("With:"),
  print_var(Report).
