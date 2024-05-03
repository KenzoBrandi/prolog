:- use_module(gmgraph).


ligne([_|Q],N,X,Y,R):- gr_rect_pos(N,X,Y,30,30),
                  N1 is N+1,
                  X1 is X + 30,
                  ligne(Q,N1,X1,Y,R) .
ligne([],N,_,_,N).

matrice([T|Q],N,X,Y):- ligne(T,N,X,Y,R),
                   N1 is R,
                   Y1 is Y +30,
                   matrice(Q,N1,30,Y1).

grille(L):-L \== [], lab(L), matrice(L,0,30,10).



laby(X,Y,V) :- lab(L),nth0(Y,L,L1),nth0(X,L1,V).


colorier(N,0) :-  gr_rect_couleur(N,white).
colorier(N,1) :-  gr_rect_couleur(N,black).
colorier(N,2) :-  gr_rect_couleur(N,green).
colorier(N,3) :-  gr_rect_couleur(N,blue).

dessine_ligne_laby(Y):-
    A is Y * 19,
    B is A + 18,
    Ypos is Y *30,
    between(A,B,N),
    Xpos is (N*30) mod (19*30),
    gr_rect_pos(N,Xpos,Ypos,30,30),
    X is N mod 19,
    laby(X,Y,V),
    colorier(N,V),fail.

dessine_laby :- between(0,18,N),dessine_ligne_laby(N).
dessine_laby.

