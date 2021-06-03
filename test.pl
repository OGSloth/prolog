map([], [], _).
map([A|As], [B|Bs], F) :-
   call(F, A, B),
   map(As, Bs, F).

fold([], Acc, Acc, _).
fold([A|As], B, Acc1, F) :-
   call(F, Acc1, A, Acc2),
   fold(As, B, Acc2, F).

reduce([A|As], Bs, F) :-   
   fold(As, Bs, A, F).	



someFunction(A, B, C) :- C is A / B. 

myFold([], Acc, Acc).
myFold([A|As], Wynik, Acc1) :-
   someFunction(Acc1, A, Acc2),
   myFold(As, Wynik, Acc2).



someFunction2(A, B, C) :- 
    Bsq is B*B,
    C = [Bsq|A]. 

myFold2([], Acc, Acc).
myFold2([A|As], Wynik, Acc1) :-
   someFunction2(Acc1, A, Acc2),
   myFold2(As, Wynik, Acc2).
