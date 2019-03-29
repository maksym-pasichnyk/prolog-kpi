fib(0, 1).
fib(1, 1).
fib(X, R) :-
    X1 is X - 1,
    X2 is X - 2,
    fib(X1, R1),
    fib(X2, R2),
    R is R1 + R2.

gcd(X, X, X).
gcd(X, Y, D):- X < Y -> Y1 is Y - X, gcd(X, Y1, D).
gcd(X, Y, D):- Y < X -> X1 is X - Y, gcd(X1, Y, D).

coprime(A, B, R):- gcd(A, B, R1), (R1 == 1 -> R is 1; R is 0).

phi(I, I, 0).
phi(I, M, R):- I < M ->
    coprime(I, M, R1),
    I1 is I + 1,
    phi(I1, M, R2),
    R is R1 + R2.

goldbachaux(1, 1, 1, 1).
goldbachaux(A, B, _, _):- A >= B.
goldbachaux(A, B, R1, R2):- A < B -> 
    A1 is A + 1,
    B1 is B - 1,
    goldbachaux(A1, B1, R3, R4),
    coprime(R3, R4, R6),
    (R6 == 1 -> 
        R1 is R3, R2 is R4; 
        R1 is  A, R2 is  B
    ).
    
golbach(1, 0, 1).
golbach(N, X1, X2):-
    A is 1,
    B is N - 1,
    goldbachaux(A, B, X1, X2).

phi(M, R) :- phi(1, M, R).

head([H], H).
head([H, _|_], H).

tail([_], []).
tail([_, H|T], [H|T]).

get(L, 0, R):- head(L, R).
get(L, I, R):-
    I1 is I - 1,
    tail(L, L1),
    get(L1, I1, R).

join(A, [], [A]).
join(A, [H|T], [A, H|T]).

append(A, [], [A]).
append(A, [T], [A, T]).

dubsaux(_, A, 0, A).
dubsaux(H, A, C, R):- 
    C1 is C - 1,
    join(H, A, R1),
    dubsaux(H, R1, C1, R).

dubs(_, 0, []).
dubs([], _, []).
dubs(L, C, R):-
    head(L, HEAD),
    tail(L, TAIL),
    dubs(TAIL, C, R2),
    dubsaux(HEAD, R2, C, R).

splitaux(_, [], L1, L2, L1, L2).
splitaux(M, L, L1, L2, R1, R2):-
    head(L, HEAD),
    tail(L, TAIL),
    (HEAD == M -> splitaux(M, TAIL, L1, L2, R1, R2);
        HEAD < M -> 
            splitaux(M, TAIL, L1, L2, X1, R2),
            join(HEAD, X1, R1);
            splitaux(M, TAIL, L1, L2, R1, X2),
            join(HEAD, X2, R2)
    ).

split(M, L, L1, L2):- splitaux(M, L, [], [], L1, L2).

adjacent(_, _, [], 0).
adjacent(X, Y, [X, Y, _|_], 1).
adjacent(X, Y, [Y, X, _|_], 1).
adjacent(_, _, [_], 0).
adjacent(X, Y, [_, H|T], R):- adjacent(X, Y, [H|T], R).

member(_, [], 0).
member(X, [Y], 0).
member(X, [X], 1).
member(X, [X|_], 1).
member(X, [_, X], 1).
member(X, [_, H|T], R):- member(X, [H|T], R).

setaux(L1, L2, L, R):-
    write("set is not implemented").

set(L1, L2, R):- setaux(L1, L2, [], R).

run_a(X):-
    fib(X, R),
    format("fib(~D) = ~D\n", [X, R]).

run_b(A, B):-
    gcd(A, B, R),
    (R = 1 -> 
        format("~D and ~D is a coprime integers\n", [A, B]);
        format("~D and ~D is not a coprime integers\n", [A, B])
    ).

run_c(M):-
    phi(M, R),
    format("phi(~D) = ~D\n", [M, R]).

run_d(N):-
    golbach(N, X1, X2),
    format("~D + ~D = ~D\n", [X1, X2, N]).

run_e(L, I):-
    get(L, I, R),
    write(L), format("[~D] = ", I), write(R), nl.

run_f(L, C):-
    dubs(L, C, R5),
    write(L), format(" * ~D = ", C), write(R5), nl.

run_g(L, M):-
    split(M, L, R1, R2),
    write(L), format(" // ~D -> ", M), write(R1), write(", "), write(R2), nl.

run_h():- !.

run_i(L1, L2):- 
    set(L1, L2, R),
    write(R), nl.

run_j(X, Y, Z):-
    adjacent(X, Y, Z, R),
    (R == 1 -> 
        write(X), write(" and "), write(Y), write(" is adjacent\n");
        write(X), write(" and "), write(Y), write(" is not adjacent\n")
    ).

:-
% a
    run_a(10),
% b
    run_b(122, 11),
% c
    run_c(10),
% d
    run_d(28),
% e
    run_e([a, b, c, d], 1),
% f
    run_f([a, b, c], 3),
% g
    run_g([1, 2, 3, 4, 5, 6, 7], 4),
% h
    run_h(),
% i
    run_i([a, b, c, d], [c, d, e, f]),
% j
    run_j(b, c, [a, b, c, d, e]),
    halt.
