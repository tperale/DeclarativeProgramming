:- module(report, [print_report/1]).
% :- use_module(library(io)).

print_var([(VarName, _) | []]) :-
  write_term(VarName, []).
print_var([(VarName, _) | Rest]) :-
  write_term(VarName, []),
  write(","),
  print_var(Rest).

print_report([Report, _]) :-
  writeln("Report: Solved the problem"),
  writeln("-----------------------------"),
  write("The program found the following variables ["),
  print_var(Report),
  writeln("].").
