:- use_module(library(clpfd)).

% apply_operations([_,1,_,2,_,3,_,4,_,5,_,6,_,7,_,8,_,9], X).
% calculate([A,1,B,2,C,3,D,4,E,5,F,6,G,7,H,8,I,9], 100).

:- initialization(main, main).

main(_Argv) :-
    insert_ops([1,2,3,4,5,6,7,8,9], Input),
    findall(Input=X, calculate(Input, X), Sols),

    writeln('Solutions for 100:'),
    findall(Input, member(Input=100, Sols), Filtered),
    forall(member(Sol, Filtered), (collapse_digits(Sol, Out), writeln(Out))),

    findall(Out, member(_=Out, Sols), NumsUnsorted),
    msort(NumsUnsorted, Nums),
    freq(Nums, Freq),
    keysort(Freq, Sorted),
    reverse(Sorted, [N-X|_]),
    format('~w appears ~w times~n', [X, N]),

    lowest_no_solution(1, Nums),

    n_largest(10, Val, member(_=Val, Sols), TopSols),
    format('Largest 10 expressable numbers: ~w~n', [TopSols]).

find_max(Comp, Goal, Out) :- n_largest(1, Comp, Goal, [Out]).
n_largest(N, Comp, Goal, Out) :-
    findnsols(N, Comp, limit(N, order_by([desc(Comp)], Goal)), Out).
find_min(Comp, Goal, Out) :- n_smallest(1, Comp, Goal, [Out]).
n_smallest(N, Comp, Goal, Out) :-
    findnsols(N, Comp, limit(N, order_by([asc(Comp)], Goal)), Out).


lowest_no_solution(N, Xs) :-
    (
        member(N, Xs) ->
            N1 #= N + 1,
            lowest_no_solution(N1, Xs);
        format('Largest number not present: ~w~n', [N])
    ).

freq(Xs, Out) :-
    msort(Xs, [X|Sorted]),
    freq_sorted(1, X, Sorted, Out).

freq_sorted(N, X, [], [N-X]).
freq_sorted(N, X, [X|Xs], Out) :-
    N1 #= N + 1,
    freq_sorted(N1, X, Xs, Out).
freq_sorted(N, X, [Y|Xs], [N-X|Out]) :-
    X \= Y,
    freq_sorted(1, Y, Xs, Out).

is_op(+).
is_op(-).

insert_ops([], []).
insert_ops([X|Xs], [_Op,X|Rest]) :- insert_ops(Xs, Rest).

calculate(Xs, Res) :-
    collapse_digits(Xs, ToCalc),
    apply_operations(0, ToCalc, Res).

collapse_digits([FirstOp,X|Xs], [FirstOp|Res]) :-
    collapse_digits(X, Xs, Res).

collapse_digits(Cur, [], [Cur]).
collapse_digits(Cur, [^,X|Xs], Rest) :-
    X in 0..9,
    New #= Cur*10 + X,
    collapse_digits(New, Xs, Rest).
collapse_digits(Cur, [Op,X|Xs], [Cur,Op|Rest]) :-
    is_op(Op),
    X in 0..9,
    collapse_digits(X, Xs, Rest).

apply_operations(Cur, [], Cur).
apply_operations(Cur, [+,X|Xs], Res) :-
    apply_operations(Cur, Xs, Rest),
    Res #= Rest + X.
apply_operations(Cur, [-,X|Xs], Res) :-
    apply_operations(Cur, Xs, Rest),
    Res #= -X + Rest.

