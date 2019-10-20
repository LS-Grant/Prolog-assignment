%Group Number 32
%Student 1: Luc Saunders-Grant, 914103
%Student 2: Tihomir Trendafilov, 913400

%(1)
man(dave).
% Program: ROYAL
parent(queenmother,elisabeth). parent(elisabeth,charles).
parent(elisabeth,andrew). parent(elisabeth,anne).
parent(elisabeth,edward). parent(diana,william).
parent(diana,harry). parent(sarah,beatrice).
parent(anne,peter). parent(anne,zara).
parent(george,elisabeth). parent(philip,charles).
parent(philip,andrew). parent(philip,edward).
parent(charles,william). parent(charles,harry).
parent(andrew,beatrice). parent(andrew,eugenie).
parent(mark,peter). parent(mark,zara).
parent(william,georgejun). parent(kate,georgejun).
parent(kate,charlotte). woman(elisabeth).
woman(quennmother). woman(anne).
woman(diana). woman(sarah).
woman(beatrice). woman(zara).
woman(eugenie). woman(kate).
woman(charlotte).
%royal_female(W):- parent(W,_),woman(W).
%royal_female(W):- parent(_,W),woman(W).

%(a)
the_royal_females([queenmother,elisabeth,anne,diana,sarah,
                  beatrice,zara,eugenie,kate,charlotte]).

the_royal_males([charles,andrew,edward,william,harry,
                 peter,george,philip,charles,mark,
                  georgejun]).
the_royal_family([queenmother,elisabeth,anne,diana,sarah,
                  beatrice,zara,eugenie,kate,charlotte,
                   charles,andrew,edward,william,harry,
                 peter,george,philip,charles,mark,
                  georgejun]).
mother(M,C):- parent(M,C),woman(M).
has_child(P):- parent(P,_).
grandparent(G,P):- parent(G,X),parent(X,P).
ancestor(A,P):- parent(A,P).
ancestor(A,P):- parent(A,X),ancestor(X,P).

%(8) mother(X,beatrice).
%?- mother(X,beatrice).
%X = sarah ? ;
%no

%(9)

%(10) ?- ancestor(queenmother,X).
%X = elisabeth ? ;
%X = charles ? ;
%X = andrew ? ;
%X = anne ? ;
%X = edward ? ;
%X = william ? ;
%X = harry ? ;
%X = georgejun ? ;
%X = beatrice ? ;
%X = eugenie ? ;
%X = peter ? ;
%X = zara ? ;
%no.

%(b)

sibling(X,Y):- parent(Z,X),parent(Z,Y).
%sibling(charles,X).
%X = charles ? ;
%X = andrew ? ;
%X = anne ? ;
%X = edward ? ;
%X = charles ? ;
%X = andrew ? ;
%X = edward ? ;
%no

aunt(X,Y):- woman(X), parent(Z,Y), sibling(X,Z).
%aunt(X,william).
%X = anne ? ;

%(c)

showPattern(X):- showFirstPart(X,0),showSecondPart(1,X,X-1).

showFirstPart(0,_).
showFirstPart(X,Y):- X>0, printStars(X), printSpaces(Y),
    printStars(X),
   X1 is X-1,Y1 is Y+2,nl,showFirstPart(X1,Y1).

showSecondPart(X,Y,_):- X=Y+1.
showSecondPart(X,Y,Z):- X>0,Y>0,X<Y+1,printStars(X),printSpaces(Z),
    printSpaces(Z),printStars(X), X1 is X+1,Z1 is Z-1,
    nl,showSecondPart(X1,Y,Z1).

printStars(0).
printStars(X):- X>0, write('*'),X1 is X-1,printStars(X1).

printSpaces(0).
printSpaces(X):- X>0, write(' '),X1 is X-1,printSpaces(X1).
%showPattern(3).
%******
%**  **
%*    *
%*    *
%**  **
%******
%no

%showPattern(6).
%************
%*****  *****
%****    ****
%***      ***
%**        **
%*          *
%*          *
%**        **
%***      ***
%****    ****
%*****  *****
%************
%no


%(d)

multinomial(L,X):- sumN(L,N),factorial(N,F),
    n2(L,L1),n1(L1,N1),X is F/N1.

%change every element of the list to its factorial
n2([],[]).
n2([H|T],[H1|T1]):- factorial(H,F),H1 is F,n2(T,T1).

%get the product from the elements in a list
n1([],1).
n1([H|T],X):- n1(T,X1), X is H*X1.

%get the sum of the elements in a list
sumN([],0).
sumN([H|T],X):- sumN(T,X1), X is H+X1.

%test(L,X):- X is factorial(sum(L)).
%test(X,F):- factorial(X,F1),F is F1+1.
%fun(X):- X is X+1.

%get the factorial of a number
factorial(0,1).
factorial(N,F) :-
   N>0,
   N1 is N-1,
   factorial(N1,F1),
   F is N * F1.

% multinomial([2,2,2,2],X).
% X = 2520;
%no.
% multinomial([3,4,5,5],X).
%X = 171531360.0 ? ;
%no

%time(multinomial([2500,5200,5020,5020],X)).
% 106,474 inferences, 2.484 CPU in 2.526 seconds (98% CPU, 42857 Lips)



%(2)
% inital state
ferry(M,C,K) :- state(K,M,C,0,0,1,1,[]).

%basecase of recursion and final state
state(_,0,0,BM,BC,-1,-1,L).

% K: boat capacity
% AM: Missionaries on bank A
% AC: Cannibals on bank A
% BM: Missionaries on bank b
% BC: Cannibals on bank b
% W: Location of weapon
% B: The location of the boat
% L: The state list, from the inital state to the goal state
state(K,AM,AC,BM,BC,W,B,L).

% checks to see if the current state is legal
check(AM,AC,BM,BC,W) :-
       %can't have less than 0 missionaies or cannibals
        AM>=0, AC>=0, BM>=0, BC>=0,
        
        %Legal if bank A missionaries are greater than bank A cannibals
        %Or legal if bank A has the weapon
        %Or legal if there are no missionaries on bank A
        (AM>=AC ; W=1 ; AM=0),
        
        %Legal if bank B missionaries are greater than bank B cannibals
        %Or legal if bank B has the weapon
        %Or legal if there are no missionaries on bank B
        (BM>=BC ; W= -1 ; BM=0).

     


% Moves that can be made

%Moving only 1 type of person across
cross([K,AM,AC,BM,BC,W,1,L],[_,AM,X,BM,BC,W,-1,L]):-
        %X amount of cannibals move to bank B
        X =< K,
        AC is AC-X,
        BC is BC+X,
        check(AM,X,BM,BC,W).

cross([K,AM,AC,BM,BC,W,-1,L],[_,AM,AC,BM,X,W,1,L]):-
        %X amount of cannibals move to bank A
        X =< K,
        AC is AC+X,
        BC is BC-X,
        check(AM,AC,BM,X,W).

cross([K,AM,AC,BM,BC,W,1,L],[_,X,AC,BM,BC,W,-1,L]):-
        %X amount of missionaries move to bank B
        X =< K,
        AM is AC+X,
        BM is BC-X,
        check(AM,AC,BM,BC,W).

cross([K,AM,AC,BM,BC,W,-1,L],[_,X,AC,BM,BC,W,1,L]):-
        %X amount of missionaries move to bank A
        X =< K,
        AM is AM+X,
        BM is BM-X,
        check(AM,AC,BM,BC,W).


%Moving both Missionaries and Cannibals across
cross([K,AM,AC,BM,BC,W,-1,L],[_,X,Y,BM,BC,W,1,L]):-
        %X amount of missionaries and Y amount of cannbibals move to bank A
        (X + Y) =< K,
        AM is AM-X,
        AC is AC-Y,
        BM is BM+X,
        BC is BC+Y,
        check(AM,AC,BM,BC,W).

cross([K,AM,AC,BM,BC,W,1,L],[_,AM,AC,X,Y,W,-1,L]):-
        %X amount of missionaries and Y amount of cannbibals move to bank B
        (X + Y) =< K,
        AM is AM+X,
        AC is AC+Y,
        BM is BM-X,
        BC is BC-Y,
        check(AM,AC,BM,BC,W).


%Moving across people with the weapon
cross([K,AM,AC,BM,BC,1,1,L],[_,AM,X,BM,BC,-1,-1,L]):-
        %X amount of cannibals move to bank B with the weapon
        X =< K,
        AC is AC-X,
        BC is BC+X,
        check(AM,X,BM,BC,-1).

cross([K,AM,AC,BM,BC,-1,-1,L],[_,AM,AC,BM,X,1,1,L]):-
        %X amount of cannibals move to bank A with the weapon
        X =< K,
        AC is AC+X,
        BC is BC-X,
        check(AM,AC,BM,X,1).

cross([K,AM,AC,BM,BC,1,1,L],[_,X,AC,BM,BC,-1,-1,L]):-
        %X amount of missionaries move to bank B with the weapon
        X =< K,
        AM is AC+X,
        BM is BC-X,
        check(AM,AC,BM,BC,-1).

cross([K,AM,AC,BM,BC,-1,-1,L],[_,X,AC,BM,BC,1,1,L]):-
        %X amount of missionaries move to bank A with the weapon
        X =< K,
        AM is AM+X,
        BM is BM-X,
        check(AM,AC,BM,BC,1).

cross([K,AM,AC,BM,BC,-1,-1,L],[_,X,Y,BM,BC,1,1,L]):-
        %X amount of missionaries and Y amount of cannbibals move to bank A
        (X + Y) =< K,
        AM is AM-X,
        AC is AC-Y,
        BM is BM+X,
        BC is BC+Y,
        check(AM,AC,BM,BC,-1).

cross([K,AM,AC,BM,BC,-1,1,L],[_,AM,AC,X,Y,1,-1,L]):-
        %X amount of missionaries and Y amount of cannbibals move to bank B
        (X + Y) =< K,
        AM is AM+X,
        AC is AC+Y,
        BM is BM-X,
        BC is BC-Y,
        check(AM,AC,BM,BC,1).

%Recursion to silve the problem
%The base case
path([K,M,C,0,0,1,1,[]],[K,AM,AC,BM,BC,W,B,L]) :-
        ([K,AM,AC,BM,BC,W,B,L],[(_,0,0,BM,BC,-1,-1,L)]).

output([]) :- nl.
output([[A,B]|path]) :- 
        output(path),
        write(B), write(' -> '), write(A), nl.
        
find :-
        path([K,M,C,0,0,1,1,[]],[(_,0,0,BM,BC,-1,-1,L)]).
        
%?- ferry(10,10,3).
%yes

%?- find.
%no