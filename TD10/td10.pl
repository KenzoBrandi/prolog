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
retourneUtile([T|Q],X,L) :- retourneUtile(Q,X,[T|L]).
retourneUtile([],L,L).

retourne(L,R) :- retourneUtile(L,R,[]).

dessine_chemin([], _).
dessine_chemin([ case(X,Y) | Q ], C) :-
    laby(X,Y, V), V \== 3,
    N is 19*Y+X,
    gr_rect_couleur(N, C),
    dessine_chemin(Q, C)
.
dessine_chemin([ case(X,Y) | Q], C) :-
laby(X,Y, 3),
dessine_chemin(Q,C).

avanceDe(X,Y,_) :- sleep(0.01), rougeXY(X,Y),fail.

avanceDe( X, Y, L) :-laby(X,Y,3),
	retourne([case(X,Y)|L],Chemin),
	dessine_chemin(Chemin,yellow),
	sleep(0.5),fail.

avanceDe(X,Y, ListeAriane) :-
    laby(X,Y,R),
    R \== 1,
    Y1 is Y - 1,
    laby(X,Y1,R1),
    R1 \== 1,
    \+ member(case(X,Y1),ListeAriane),
    avanceDe(X,Y1,[case(X,Y) |ListeAriane]).


avanceDe(X,Y, ListeAriane) :-
    laby(X,Y,R),
    R \== 1,
    X1 is X - 1,
    laby(X1,Y,R1),
    R1 \==1,
    \+ member(case(X1,Y),ListeAriane),
    avanceDe(X1,Y, [case(X,Y) |ListeAriane]).

avanceDe(X,Y, ListeAriane) :-
    laby(X,Y,R),
    R \== 1,
    X1 is X + 1,
    laby(X1,Y,R1),
    R1 \== 1,
    \+ member(case(X1,Y),ListeAriane),
    avanceDe(X1,Y, [case(X,Y) |ListeAriane]).

avanceDe(X,Y, ListeAriane) :-
    laby(X,Y,R),
    R \== 1,
    Y1 is Y + 1,
    laby(X,Y1,R1),
    R1 \== 1,
    \+ member(case(X,Y1),ListeAriane),
    avanceDe(X,Y1, [case(X,Y) |ListeAriane]).

avanceDe(X,Y,_) :- blancXY(X,Y), fail.



avanceDeCollect( X, Y, L,C) :-laby(X,Y, 3),retourne([case(X,Y)|L],C).

avanceDeCollect(X,Y, ListeAriane,C) :-
    laby(X,Y,R),
    R \== 1,
    Y1 is Y - 1,
    laby(X,Y1,R1),
    R1 \== 1,
    \+ member(case(X,Y1),ListeAriane),
    avanceDeCollect(X,Y1,[case(X,Y) |ListeAriane],C).


avanceDeCollect(X,Y, ListeAriane,C) :-
    laby(X,Y,R),
    R \== 1,
    X1 is X - 1,
    laby(X1,Y,R1),
    R1 \==1,
    \+ member(case(X1,Y),ListeAriane),
    avanceDeCollect(X1,Y, [case(X,Y) |ListeAriane],C).

avanceDeCollect(X,Y, ListeAriane,C) :-
    laby(X,Y,R),
    R \== 1,
    X1 is X + 1,
    laby(X1,Y,R1),
    R1 \== 1,
    \+ member(case(X1,Y),ListeAriane),
    avanceDeCollect(X1,Y, [case(X,Y) |ListeAriane],C).

avanceDeCollect(X,Y, ListeAriane,C) :-
    laby(X,Y,R),
    R \== 1,
    Y1 is Y + 1,
    laby(X,Y1,R1),
    R1 \== 1,
    \+ member(case(X,Y1),ListeAriane),
    avanceDeCollect(X,Y1, [case(X,Y) |ListeAriane],C).


eviteMinotaure(X,Y, Chemin) :- avanceDeCollect(1,1,[],Chemin),
    \+ member(case(X,Y),Chemin).

















