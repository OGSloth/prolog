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
    read_file_to_terms(Program, [variables(V), arrays(A), P], []), % File to terms
    initState([V, A, P], N, StanPoczatkowy), % Initialise state
    write(StanPoczatkowy), nl,
    runDFA(StanPoczatkowy, P, N).
    %step([_, _, P], StanPoczatkowy, 0, Wyj),
    %verifyCritSection(P, Wyj, X),


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PlayGround Functions
fold([], Acc, Acc, _).
fold([A|As], B, Acc1, F) :-
   call(F, Acc1, A, Acc2),
   fold(As, B, Acc2, F).



runDFA2(CurrentState, Program, N, Paths, StateAcc, PrId, Wynik) :-
    step([_, _, Program], CurrentState, PrId, NewState),
    getCurrentStep(PrId, CurrentState, Step),
    runDFA2(NewState, Program, N, [[PrId|Step]|Paths], StateAcc, Wynik).

runDFA2(CurrentState, Program, N, Paths, StateAcc, Wynik) :-(
    member(CurrentState, StateAcc)
    -> Wynik = StateAcc % Idk, what should be there tbh
    ; verifyCritSection(Program, CurrentState, X),
    (X >= 2
    -> write("ahojjjj"), nl, write(CurrentState), nl, write(StateAcc), nl, printSteps(Paths), nl, false %Or false?!
    ; pidsList(N, NewPidsList),
    fold(NewPidsList, Wynik, [CurrentState|StateAcc], runDFA2(CurrentState, Program, N, Paths))
    )
).

%%%%%%%%%%%

runDFA(InitialState, Program, N) :- runDFA2(InitialState, Program, N, [], [], _).
%    pidsList(N, Pids),
%    iteratePids(Pids, Wynik, [], [], InitialState, Program, N),
%    write("Skonczylo sie? xd"), write(Wynik).

iteratePids([], Acc, Acc, _, _, _) :- write("Jumperoooo"), nl, write(Acc).
iteratePids([], _, _, _,_,_) :- nl, write("Wtf xd"),nl.
iteratePids([Pid|Pids], Wynik, Acc1, Paths, CurrentState, Program, N) :-
    nl,nl,write(Pid), nl,
    write("Essa"), nl,
    write(Pids), nl,
    write(CurrentState), nl,
    write(Acc1), nl,
    step([_, _, Program], CurrentState, Pid, NewState),
    (
        member(NewState, Acc1)
        -> Wynik = Acc1
        ; verifyCritSection(Program, NewState, X),
        (
          X >= 2
          -> write("Ahojjjj"), nl, write(Paths), nl, printSteps(Paths), write("AUUUU!"), nl, false %ToDo - Check if ! would work
          ; pidsList(N, NewPidsList),
            getCurrentStep(Pid, CurrentState, Step),
            write(Pids),
            iteratePids(Pids, Wynik1, [CurrentState|Acc1], Paths, NewState, Program, N),
            write("Wynik1 = "), write(Wynik1), nl,
            iteratePids(NewPidsList, Wynik, Wynik1, [[Pid|Step]|Paths], NewState, Program, N),
            write("Global? = "), write(Wynik), nl
        )
    ).

getCurrentStep(PrId, state(_, _, S), Step) :- get(PrId, S, Step).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5%%%%%%%%%%%%%%%%%%%%%%%%%

verifyCritSection(P, state(_, _, S), X) :- verifyCritSection(P, S, X).
verifyCritSection(program(P), [[_|S]|T], X1) :-
    verifyCritSection(program(P), T, X),
    S1 is S-1,
    (
        nth0(S1, P, sekcja)
        ->  X1 is X+1
        ; X1 is X
    ).

verifyCritSection(_, [], 0).

%run(P, CurrState, [States|Steps]):

printSteps([]).
printSteps([[PrId|Step]|T]) :-
    printSteps(T),
    format('    Proces ~d: ~d', [PrId, Step]),nl.


% Initial State
% state(Variables, Arrays, Steps)
% Variables - List of variables
% Arrays - List of Arrays (From 0 to N)
% Steps - Key: Program Pid, Value: Line to be executed by program of this pid

initState(Program, N, StanPoczatkowy) :-
    Program = [V, A, _],
    buildVars(V, Vars),
    buildSteps(N, Steps),
    buildArray(N, Array),
    buildArrays(A, Arrays, Array),
    StanPoczatkowy = state(Vars, Arrays, Steps).


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

parse(assign(array(Zmienna, WyrArytm1), WyrArytm2), state(V, A, S), PrId, state(V, Aw, Sw)) :-
    atom(Zmienna),
    wyrArytm(WyrArytm2, state(V, A, _), PrId, E),
    wyrArytm(WyrArytm1, state(V, A, _), PrId, P),
    get(Zmienna, A, [Arr|_]),
    update([P|E], [Arr], Arrw),
    write(Arrw), nl,
    [XD] = Arrw,
    write(XD), nl,
    update([Zmienna|Arrw], A, Aw),
    write(A), nl,
    write([P|E]), nl,
    write(Aw), nl,
    moveStep(state(_, _, S), PrId, state(_, _, Sw)).

parse(goto(Liczba), state(V, A, S), PrId, state(V, A, Sw)) :-
    update([PrId|Liczba], S, Sw).

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

pidsList(N, List) :-
    N1 is N-1,
    pidsList(0, N1, List).

pidsList(N, N, [N]).
pidsList(N0, N, [N0| List]) :-
    N0 < N,
    N1 is N0 + 1,
    pidsList(N1, N, List).



%% Arrays, Variables and Steps list managment
% Update List under the Key
update([K|V], AL, AL0) :- replace([K|_], [K|V], AL, AL0).

replace(_, _, [], []).
replace(O, R, [O|T], [R|T2]) :- replace(O, R, T, T2).
replace(O, R, [H|T], [H|T2]) :- H \= O, replace(O, R, T, T2).

put(KV, AL, AL0) :-
    KV = [K|V],
    get(K, AL, V),
    remove(KV, AL, AL_KV),
    put(KV, AL_KV, AL0).

put(KV, AL, [KV | AL]).

get(K, AL, V):-
    member([K|V], AL).

% zmienna() ....
zmienna(Ident, state(V, _, _), PrId, Output) :-
    atom(Ident),
    (Ident == pid
        -> Output is PrId
        ; get(Ident, V, Output)
    ).

zmienna(array(Ident, WyrArytm), state(V, A, _), PrId, Output) :-
    wyrArytm(WyrArytm, state(V, A, _), PrId, E),
    get(Ident, A, [Array]),
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

