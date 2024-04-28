:- use_module(gmgraph).
laby(X,Y,V) :- lab(L),nth0(Y,L,L1),nth0(X,L1,V).


colorier(N,0) :- gr_rect_couleur(N,white).
colorier(N,1) :- gr_rect_couleur(N,black).
colorier(N,2) :- gr_rect_couleur(N,green).
colorier(N,3) :- gr_rect_couleur(N,blue).



dessine_ligne_laby(Y):-
    A is Y * 19,
    B is A + 18,
    Ypos is Y*30,
    between(A,B,N),
    Xpos is (N *30) mod (19 * 30),
    gr_rect_pos(N,Xpos,Ypos,30,30),
    X is N mod 19,
    laby(X,Y,V),
    colorier(N,V).

dessine_laby :- between(0,18,N),dessine_ligne_laby(N),fail.
dessine_laby.



noirXY(X,Y) :- N is (Y*19) + X,
    gr_rect_couleur(N,black).


rougeXY(X,Y) :- N is (Y*19) + X,
    gr_rect_couleur(N,red).

blancXY(X,Y) :- N is (Y*19) + X,
    gr_rect_couleur(N,white).


solutions(X,Y) :-
    laby(X,Y,R),
    R \== 1,
    R \== 3,
    avanceDe(X,Y,[[X,Y]]).

avanceDe(X,Y,_) :- sleep(0.01), rougeXY(X,Y),fail.

avanceDe( X, Y, Listeariane) :-laby(X,Y, 3),sleep(0.5),fail.

avanceDe(X,Y, ListeAriane) :-
    laby(X,Y,R),
    R \== 1,
    R \== 3,
    Y1 is Y - 1,
    \+ member([X,Y1],ListeAriane),
    avanceDe(X,Y1,[[X,Y1] |ListeAriane]).


avanceDe(X,Y, ListeAriane) :-
    laby(X,Y,R),
    R \== 1,
    R \== 3,
    X1 is X - 1,
    \+ member([X1,Y],ListeAriane),
    avanceDe(X1,Y, [[X1,Y] |ListeAriane]).

avanceDe(X,Y, ListeAriane) :-
    laby(X,Y,R),
    R \== 1,
    R \== 3,

    X1 is X + 1,
    \+ member([X1,Y],ListeAriane),
    avanceDe(X1,Y, [[X1,Y] |ListeAriane]).

avanceDe(X,Y, ListeAriane) :-
    laby(X,Y,R),
    R \== 1,
    R \== 3,
    Y1 is Y + 1,
    \+ member([X,Y1],ListeAriane),
    avanceDe(X,Y1, [[X,Y1] |ListeAriane]).

avanceDe(X,Y,_) :-laby(X,Y,0), blancXY(X,Y), fail.
avanceDe(X,Y,_) :-laby(X,Y,1), noirXY(X,Y), fail.









