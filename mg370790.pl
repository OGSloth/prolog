% Marcin Gadomski - mg370790
ensure_loaded(library(lists)).

verify() :-            % verify /0
  read(N),             % Read input from IO
  read(Program),
  verify(N, Program).  % Run verification

checkN(N) :- (         % Checks if N is valid
  number(N), N >= 1    % N must be a number >= 1
  -> true              % If so continue
  ;  write("Error: parametr 0 powinien byc liczba > 0"),
     false             % If not, fail
  ).

checkProgram(Program) :-     % Checks if Program is valid
  (string(Program)           % Must be a string
  -> true
  ; write("Error: parametr 1 powinnen byc napisem"),
    false
  ),
  (exists_file(Program)    % File needs to exist
  -> true                  % If exists continue
  ; format("Error: brak pliku o nazwie - ~w", [Program]),
    false                  % Otherwise - fail
  ).

% The program main function
% Gets input, parses, interprates and searches for the result
verify(N, Program) :-
  checkN(N),
  checkProgram(Program),                        % Input validation
  read_file_to_terms(Program, [variables(V), arrays(A), P], []),% File to terms
  initState([V, A, P], N, StanPoczatkowy),      % Initialise state
  startDFA(StanPoczatkowy, P, N),               % Start main verificaiton part
  write("Program jest poprawny (bezpieczny).").

% Utility function to propery go to next states from current state
fold([], Acc, Acc, _).       % Folding finished, Result = Accumulator
fold([A|As], B, Acc1, F) :-
   call(F, Acc1, A, Acc2),   % Call passed function, new accumulator
   fold(As, B, Acc2, F).     % Continue folding with new accumulator

% Function searching for the result
% "DFA" is in current state with pointed transition
% Goes there, saves path and goes into the node
runDFA(CurrSt, Program, N, Paths, StAcc, PrId, Wynik) :-
  step([_, _, Program], CurrSt, PrId, NewState),
  getCurrentStep(PrId, CurrSt, Step),
  runDFA(NewState, Program, N, [[PrId|Step]|Paths], StAcc, Wynik).

% Check if node is valid, go to the neighbour nodes
runDFA(CurrSt, Program, N, Paths, StAcc, Wynik) :-(
  member(CurrSt, StAcc)                     % If node is checked, finish
  -> Wynik = StAcc                          % (This branch returns accumulator)
  ; verifyCritSection(Program, CurrSt, X),  % Otherwise check critical sections
  (X >= 2                                   % X >= 2 -> 2 or more pids in crit
  -> write("Program jest niepoprawny."), nl,
     write("Niepoprawny przeplot:"), nl,
     printSteps(Paths), write(CurrSt), false % Failure handling
  ; pidsList(N, NPL),                       % Initiate next transtitons
  fold(NPL, Wynik, [CurrSt|StAcc], runDFA(CurrSt, Program, N, Paths))
  ) % Go further, from node to transitioned nodes
).

% startDFA \3 starts startDFA \6 with empty accumulators
startDFA(InitSt, Program, N) :- runDFA(InitSt, Program, N, [], [], _).

% Gets program line to be executed by program with pid = PrId
getCurrentStep(PrId, state(_, _, S), Step) :- get(PrId, S, Step).

% Checks how many programs are currently in critical section
verifyCritSection(P, state(_, _, S), X) :- verifyCritSection(P, S, X).
verifyCritSection(program(P), [[_|S]|T], X1) :-
  verifyCritSection(program(P), T, X), % Check recursivly
  S1 is S-1,                           % Align step and list iterations diffs
  (
    nth0(S1, P, sekcja)                % Check if program is in section
    ->  X1 is X+1                      % If so, add one to the result
    ; X1 is X                          % Otherwise continue
  ).

verifyCritSection(_, [], 0).

% Simple function to print path from initial state to 
% critical section overlapse
printSteps([]).
printSteps([[PrId|Step]|T]) :-
  printSteps(T),
  format('  Proces ~d: ~d', [PrId, Step]),nl.


% Initial State
% state(Variables, Arrays, Steps)
% Variables - List of variables
% Arrays - List of Arrays (From 0 to N)
% Steps - Key: Program Pid, Value: Line to be executed by program of this pid

initState(Program, N, StanPoczatkowy) :-
  Program = [V, A, _],                          % Take input
  buildVars(V, Vars),                           % Create Variables list
  buildSteps(N, Steps),                         % Create Steps list
  buildArray(N, Array),                         % Create an empty array
  buildArrays(A, Arrays, Array),                % Create Arrays list
  StanPoczatkowy = state(Vars, Arrays, Steps).  % Put all together as state

% Crucial program function
% Takes Program (As initialiy parsed tokens)
% Current state, id and output nwe state
step([_, _, program(P)], state(V, A, S), PrId, Wyj) :-
  get(PrId, S, Step),                   % Take program line to be executed
  GoTo is Step-1,                       % Align iterations differences
  nth0(GoTo, P, X),                     % Take program's line
  parse(X, state(V, A, S), PrId, Wyj).  % Parse and execute the line

% Following lines of code are focused on parsing and executing lines of code

% Parses assigning evalved value of WyrAryrm to variable Zmienna
parse(
  assign(Zmienna, WyrArytm),
  state(V, A, S),
  PrId,
  state(Vw, A, Sw)
) :-
  atom(Zmienna),                                  % Check if can be assigned
  wyrArytm(WyrArytm, state(V, A, _), PrId, E),    % Eval WyrArytm value
  update([Zmienna|E], V, Vw),                     % Update current values list
  moveStep(state(_, _, S), PrId, state(_, _, Sw)).% Finish codeline, go durther

% Assign value to the array
parse(
	assign(array(Zmienna, WyrArytm1), WyrArytm2),
	state(V, A, S),
	PrId,
	state(V, Aw, Sw)
) :-
  atom(Zmienna),
  wyrArytm(WyrArytm2, state(V, A, _), PrId, E),  % Eval value to be stored
  wyrArytm(WyrArytm1, state(V, A, _), PrId, P),  % Eval array index for value
  get(Zmienna, A, [Arr|_]),                      % Get an array named Zmienna
  update([P|E], Arr, Arrw),                      % Update array with values
  update([Zmienna|[Arrw]], A, Aw),               % Update arrays with an array
  moveStep(state(_, _, S), PrId, state(_, _, Sw)). % Finish, go further

% goto function parsing
parse(goto(Liczba), state(V, A, S), PrId, state(V, A, Sw)) :-
  update([PrId|Liczba], S, Sw).                 % Just go to Liczba code line

% confitional goto function parsing
parse(condGoto(WyrLogiczne, Liczba), state(V, A, S), PrId, state(V, A, Sw)) :-
  wyrLogiczne(WyrLogiczne, state(V, A, _), PrId, Output), % Eval
  (Output == true
    -> update([PrId|Liczba], S, Sw)                       % If true go to line
    ; moveStep(state(V, A, S), PrId, state(V, A, Sw))     % Otherwise- further
  ).

% parsing section
parse(sekcja, state(V, A, S), PrId, state(V, A, Sw)) :-  % Section does nothing
  moveStep(state(V, A, S), PrId, state(V, A, Sw)).       % Just go firther

% Increases value of the current state of the value PrId by one
moveStep(state(V, A, S), PrId, state(V, A, Sw)) :-
  get(PrId, S, Step),
  Step2 is Step + 1,
  update([PrId|Step2], S, Sw).

% Mentioned before functions co create arrays, vatiables and list of pids
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

% Replaces values
replace(_, _, [], []).
replace(O, R, [O|T], [R|T2]) :- replace(O, R, T, T2).
replace(O, R, [H|T], [H|T2]) :- H \= O, replace(O, R, T, T2).

% Puts new value
put(KV, AL, AL0) :-
  KV = [K|V],
  get(K, AL, V),
  remove(KV, AL, AL_KV),
  put(KV, AL_KV, AL0).

put(KV, AL, [KV | AL]).

% Gets value from the list
get(K, AL, V):-
  member([K|V], AL).

% zmienna(+Ident, +State, +Pid, -Output)
% Returns value of the variable named Ident in state State
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

% wyrArytm(+Wyr, +State, +Pid, -Output)
% wyrArytm evals value of the arithmetical equation within state
% Puts value in the output

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

