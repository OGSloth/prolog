% Marcin Gadomski - mg370790
ensure_loaded(library(lists)).

verify() :- 
    read(N),
    read(Program),
    verify(N, Program).

checkN(N) :- (
    number(N), N >= 1
    -> true
    ;  write("Error: parametr 0 powinien byc liczba > 0"),
       false
    ).

checkProgram(Program) :- 
    %atom_string(Program, ProgName),
    (string(Program)
    -> true
    ; write("Error: parametr 1 powinnen byc napisem"),
      false
    ),
    (exists_file(Program)
    -> true
    ; format("Error: brak pliku o nazwie - ~w", [Program]),
      false
    ).

verify(N, Program) :-
    checkN(N),
    write(N),
    nl,
    checkProgram(Program),
    read_file_to_terms(Program, [vars(V), arrays(A), P], []),
    write(P), nl, 
    initState([V, A, P], N, StanPoczatkowy),
    write(StanPoczatkowy).

% Initial State
% state(Variables, Arrays, Steps)
% Variables - List of variables
% Arrays - List of Arrays
% Steps - Key: Program Pid, Value: Line to be executed by program of this pid
%              0 means that program did not start yet

initState(Program, N, StanPoczatkowy) :-
    Program = [V, A, P],
    write(V),
    buildVars(V, Vars),
    nl,write(Vars),nl,
    buildSteps(N, Steps),
    nl,write(Steps),nl,
    buildArrays(A, Arrays, Steps),
    nl,write(Arrays),nl,
    nl,write(P),nl,
    StanPoczatkowy = state(Vars, Arrays, Steps).

buildArrays(A, Arrays, S) :- buildArrays(A, [], Arrays, S).
buildArrays([], L, L, _).
buildArrays([H|T], L, L0, S) :-
    put([H|[S]], L, L1),
    buildArrays(T, L1, L0, S).

buildVars(V, Vars) :- buildVars(V, [], Vars).
buildVars([], L, L).
buildVars([H|T], L, L0) :-
    put([H|0], L, L1),
    buildVars(T, L1, L0).

buildSteps(N, Steps) :- buildSteps(0, N, Steps).
buildSteps(N, N, []).
buildSteps(N0, N, [[N0|0]| L]) :-
    N0 < N,
    N1 is N0 + 1,
    buildSteps(N1, N, L).

put(KV, AL, AL0) :-
    KV = [K|V],
    get(K, AL, V),
    remove(KV, AL, AL_KV),
    put(KV, AL_KV, AL0).

put(KV, AL, [KV | AL]).

get(K, AL, V):-
    member([K|V], AL).

step([_, _, program(P)], state(V, A, S), PrId, state(V, A, Sw)) :-
    get(PrId, S, Step),
    write(P),
    write(V),
    write(A),
    put([PrId|Step + 1], S, Sw),
    write(Step).

dupli(L1,N,L2) :- dupli(L1,N,L2,N).
dupli([],_,[],_).
dupli([_|Xs],N,Ys,0) :- dupli(Xs,N,Ys,N).
dupli([X|Xs],N,[X|Ys],K) :- K > 0, K1 is K - 1, dupli([X|Xs],N,Ys,K1).


%Parser - Parsing instructions, arithmetic and logic
%parse(assign(Zmienna, WyrArytm), state(V, A, S), PrId, state(Vw, Aw, SW)).
%parse(goto(Liczba), state(V, A, S), PrId, state(Vw, Aw, SW)).
%parse(condGoto(WyrLogiczne, Liczba), state(V, A, S), PrId, state(Vw, Aw, SW)).
%parse(sekcja, state(V, A, S), PrId, state(Vw, Aw, SW)).


zmienna(Ident, state(V, _, _), PrId, Output) :-
    atom(Ident),
    (Ident == pid
        -> Output is PrId
        ; get(Ident, V, Output)
    ).

zmienna(arr(Ident, WyrArytm), state(V, A, _), PrId, Output) :-
    wyrArytm(WyrArytm, state(V, A, _), PrId, E),
    get(Ident, A, Array),
    get(E, Array, Output).

wyrArytm(WyrProste1+WyrProste2, state(V, A, _), PrId, Output) :-
    wyrArytm(WyrProste1, state(V, A, _), PrId, E1),
    wyrArytm(WyrProste2, state(V, A, _), PrId, E2),
    Output is E1 + E2.

wyrArytm(WyrProste1-WyrProste2, state(V, A, _), PrId, Output) :-
    wyrArytm(WyrProste1, state(V, A, _), PrId, E1),
    wyrArytm(WyrProste2, state(V, A, _), PrId, E2),
    Output is E1 - E2.

wyrArytm(WyrProste1*WyrProste2, state(V, A, _), PrId, Output) :-
    wyrArytm(WyrProste1, state(V, A, _), PrId, E1),
    wyrArytm(WyrProste2, state(V, A, _), PrId, E2),
    Output is E1 * E2.

wyrArytm(WyrProste1/WyrProste2, state(V, A, _), PrId, Output) :-
    wyrArytm(WyrProste1, state(V, A, _), PrId, E1),
    wyrArytm(WyrProste2, state(V, A, _), PrId, E2),
    Output is E1 / E2.

wyrArytm(WyrProste, state(V, A, _), PrId, Output) :- (
    number(WyrProste)
    -> Output is WyrProste
    ; zmienna(WyrProste, state(V, A, _), PrId, Output)
    ).

wyrLogiczne(WyrProste1<WyrProste2, state(V, A, _), PrId, Output) :-
    wyrArytm(WyrProste1, state(V, A, _), PrId, E1),
    wyrArytm(WyrProste2, state(V, A, _), PrId, E2),
    (E1 < E2
        -> Output = true
        ; Output = false
    ).

wyrLogiczne(WyrProste1=WyrProste2, state(V, A, _), PrId,  Output) :-
    wyrArytm(WyrProste1, state(V, A, _), PrId, E1),
    wyrArytm(WyrProste2, state(V, A, _), PrId, E2),
    (E1 == E2
        -> Output = true
        ; Output = false
    ).

wyrLogiczne(WyrProste1=\=WyrProste2, state(V, A, _), PrId, Output) :-
    wyrArytm(WyrProste1, state(V, A, _), PrId, E1),
    wyrArytm(WyrProste2, state(V, A, _), PrId, E2),
    (E1 =\= E2
        -> Output = true
        ; Output = false
    ).

