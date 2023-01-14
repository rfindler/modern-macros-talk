#lang datalog

ancestor(A, B) :- parent(A, B).
ancestor(A, B) :-
  parent(A, C),
  ancestor(C, B).

parent(john, douglas).
parent(bob, john).
parent(ebbon, bob).

ancestor(A,B)?