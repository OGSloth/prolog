% Marcin Gadomski - mg370790
ensure_loaded(library(lists)).

verify() :-                    % verify /0
    read(N),                   % Read input from IO
    read(Program),
    verify(N, Program).        % Run verification

checkN(N) :- (                 % Checks if N is valid
    number(N), N >= 1          % N must be a number >= 1
    -> true                    % If so continue
    ;  write("Error: parametr 0 powinien byc liczba > 0"),
       false                   % If not, fail
    ).

checkProgram(Program) :-       % Checks if Program is valid
    (string(Program)           % Must be a string
    -> true
    ; write("Error: parametr 1 powinnen byc napisem"),
      false
    ),
    (exists_file(Program)      % File needs to exist
    -> true                    % If exists continue
    ; format("Error: brak pliku o nazwie - ~w", [Program]),
      false                    % Otherwise - fail
    ).

verify(N, Program) :-
    checkN(N),
    checkProgram(Program),     % Input validation
    read_file_to_terms(Program, [vars(V), arrays(A), P], []), % File to terms
    initState([V, A, P], N, StanPoczatkowy), % Initialise state
    write(StanPoczatkowy).

% Initial State
% state(Variables, Arrays, Steps)
% Variables - List of variables
% Arrays - List of Arrays (From 0 to N)
% Steps - Key: Program Pid, Value: Line to be executed by program of this pid
%              0 means that program did not start yet

initState(Program, N, StanPoczatkowy) :-
    Program = [V, A, P],
    write(V),
    buildVars(V, Vars),
    nl,write(Vars),nl,
    buildSteps(N, Steps),
    buildArray(N, Array),
    nl,write("ESSA - "), write(Array), nl,
    buildArrays(A, Arrays, Array),
    nl,write(Arrays),nl,
    nl,write(P),nl,nl,nl,
    StanPoczatkowy = state(Vars, Arrays, Steps),
    step(Program, StanPoczatkowy, 1, Wyj),
    nl,write(Wyj),nl.


step([_, _, program(P)], state(V, A, S), PrId, Wyj) :-
    get(PrId, S, Step),
    GoTo is Step-1,
    nth0(GoTo, P, X),
    write(X),nl,
    parse(X, state(V, A, S), PrId, Wyj).

%Parser - Parsing instructions, arithmetic and logic
parse(assign(Zmienna, WyrArytm), state(V, A, S), PrId, state(Vw, A, Sw)) :-
    atom(Zmienna),
    wyrArytm(WyrArytm, state(V, A, _), PrId, E),
    update([Zmienna|E], V, Vw),
    moveStep(state(_, _, S), PrId, state(_, _, Sw)).

parse(assign(arr(Zmienna, WyrArytm1), WyrArytm2), state(V, A, S), PrId, state(V, Aw, Sw)) :-
    atom(Zmienna),
    wyrArytm(WyrArytm2, state(V, A, _), PrId, E),
    wyrArytm(WyrArytm1, state(V, A, _), PrId, P),
    get(Zmienna, A, [Arr|_]),
    update([P|E], Arr, Arrw),
    update([Zmienna|Arrw], A, Aw),
    moveStep(state(_, _, S), PrId, state(_, _, Sw)).

parse(goto(Liczba), state(V, A, S), PrId, state(V, A, Sw)) :-
    update([PrId|Liczba], S, Sw).

%ToDo ---- Check if it should be Number or get be wyrArytm
parse(condGoto(WyrLogiczne, Liczba), state(V, A, S), PrId, state(V, A, Sw)) :-
    wyrLogiczne(WyrLogiczne, state(V, A, _), PrId, Output),
    (Output == true
        -> update([PrId|Liczba], S, Sw)
        ; moveStep(state(V, A, S), PrId, state(V, A, Sw))
    ).

parse(sekcja, state(V, A, S), PrId, state(V, A, Sw)) :-
    moveStep(state(V, A, S), PrId, state(V, A, Sw)).

% Increases value of the current state of the value PrId by one
moveStep(state(V, A, S), PrId, state(V, A, Sw)) :-
    get(PrId, S, Step),
    Step2 is Step + 1,
    update([PrId|Step2], S, Sw).


buildArrays(A, Arrays, Arr) :- buildArrays(A, [], Arrays, Arr).
buildArrays([], L, L, _).
buildArrays([H|T], L, L0, Arr) :-
    put([H|[Arr]], L, L1),
    buildArrays(T, L1, L0, Arr).

buildVars(V, Vars) :- buildVars(V, [], Vars).
buildVars([], L, L).
buildVars([H|T], L, L0) :-
    put([H|0], L, L1),
    buildVars(T, L1, L0).

buildArray(N, Array) :- buildArray(0, N, Array).
buildArray(N, N, []).
buildArray(N0, N, [[N0|0]| L]) :-
    N0 < N,
    N1 is N0 + 1,
    buildArray(N1, N, L).

buildSteps(N, Steps) :- buildSteps(0, N, Steps).
buildSteps(N, N, []).
buildSteps(N0, N, [[N0|1]| L]) :-
    N0 < N,
    N1 is N0 + 1,
    buildSteps(N1, N, L).

update(KV, AL, AL0) :-
    KV = [K|_],
    delete(AL, [K|_], AL_KV),
    put(KV, AL_KV, AL0).

put(KV, AL, AL0) :-
    KV = [K|V],
    get(K, AL, V),
    remove(KV, AL, AL_KV),
    put(KV, AL_KV, AL0).

put(KV, AL, [KV | AL]).

get(K, AL, V):-
    member([K|V], AL).

dupli(L1,N,L2) :- dupli(L1,N,L2,N).
dupli([],_,[],_).
dupli([_|Xs],N,Ys,0) :- dupli(Xs,N,Ys,N).
dupli([X|Xs],N,[X|Ys],K) :- K > 0, K1 is K - 1, dupli([X|Xs],N,Ys,K1).


% zmienna() ....
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

% wyrArytm()....

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


% wyrLogiczne(+Wyr, +State, +Pid, -Output)
% wyrLogiczne evals if Wyr condition holds with state = State
% and current pid = Pid
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

