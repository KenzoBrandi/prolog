

% Module d'affichage XPCE / SWI-PROLOG
% 	G.Ménier  UBS gildas.menier@univ-ubs.fr
% 	cours Prog logique & fonctionnelle Licence 1 UBS
%   etendu rect (10 rectangles possibles)


:- module(gmgraph,[

        gr_init/0,            % création fenêtre
        gr_end/0,             % fin
        
        gr_flush/0,           % flush
        gr_f/0,               % f (lush)

        % tortue
        gr_dessine/1,        % trace un  trait dans la direction courante
      
        gr_deplace/1,        % deplace sans tracer
        gr_deplaceTexture/1,% none, dotted, dashed,  dashdot, dashdotted, longdash
        
        gr_tourne/1,        % rotation relative
           gr_gauche/0,        % rotation 90 deg à gauche
           gr_droite/0,        % rotation 90 deg à droite          
        gr_couleur/1,        % couleur courante
        gr_couleur_hsv/3,    % H(0,360) 100, 100
        gr_couleur_nom/1,    % red, blue etc..
        gr_fleche/1,        % none, first, second, both
        gr_pos/0,           % marqueur pour voir la position
          gps/0,

        gr_push/0,          % pile de contexte
        gr_pop/0,            % pile de contexte

        gr_rect_pos/5, 
        gr_rect_couleur/2

    ]).

 



:- use_module(library(pce)).


gr_init :- 
   gr_end,
     gr_couleur_nom(blue),
     gr_fleche(none),
     gr_deplaceTexture(none), 
   B is 450, H is 620,
   B1 is B /2, H1 is H / 2,
   W1 is 0, assert(turtle(B1, H1, W1)),
   new(@win, picture('UBS - Licence 1', size(B, H))),
   send(@win, open) .%
 
  

% 10 boites numérotées et colorées   
 
gr_rect_pos(1, X,Y, Width, Height) :-  free(@bb1), free(@tt1), send(@win, display, new(@bb1, box(Width,Height)), point(X, Y)),
                                       send(@win, display, new(@tt1, text(1)), point(X+3, Y+3)), gr_flush.  
gr_rect_pos(2, X,Y, Width, Height) :-  free(@bb2), free(@tt2), send(@win, display, new(@bb2, box(Width,Height)), point(X, Y)),
                                       send(@win, display, new(@tt2, text(2)), point(X+3, Y+3)), gr_flush.  
gr_rect_pos(3, X,Y, Width, Height) :-  free(@bb3), free(@tt3), send(@win, display, new(@bb3, box(Width,Height)), point(X, Y)),
                                       send(@win, display, new(@tt3, text(3)), point(X+3, Y+3)), gr_flush.  
gr_rect_pos(4, X,Y, Width, Height) :-  free(@bb4), free(@tt4), send(@win, display, new(@bb4, box(Width,Height)), point(X, Y)),
                                       send(@win, display, new(@tt4, text(4)), point(X+3, Y+3)), gr_flush.  
gr_rect_pos(5, X,Y, Width, Height) :-  free(@bb5), free(@tt5), send(@win, display, new(@bb5, box(Width,Height)), point(X, Y)),
                                       send(@win, display, new(@tt5, text(5)), point(X+3, Y+3)), gr_flush.  
gr_rect_pos(6, X,Y, Width, Height) :-  free(@bb6), free(@tt6), send(@win, display, new(@bb6, box(Width,Height)), point(X, Y)),
                                       send(@win, display, new(@tt6, text(6)), point(X+3, Y+3)), gr_flush.  
gr_rect_pos(7, X,Y, Width, Height) :-  free(@bb7), free(@tt7), send(@win, display, new(@bb7, box(Width,Height)), point(X, Y)),
                                       send(@win, display, new(@tt7, text(7)), point(X+3, Y+3)), gr_flush.  
gr_rect_pos(8, X,Y, Width, Height) :-  free(@bb8), free(@tt8), send(@win, display, new(@bb8, box(Width,Height)), point(X, Y)),
                                       send(@win, display, new(@tt8, text(8)), point(X+3, Y+3)), gr_flush.  
gr_rect_pos(9, X,Y, Width, Height) :-  free(@bb9), free(@tt9), send(@win, display, new(@bb9, box(Width,Height)), point(X, Y)),
                                       send(@win, display, new(@tt9, text(9)), point(X+3, Y+3)), gr_flush.  
gr_rect_pos(10, X,Y, Width, Height) :-  free(@bb10), free(@tt10), send(@win, display, new(@bb10, box(Width,Height)), point(X, Y)),
                                       send(@win, display, new(@tt10, text(10)), point(X+3, Y+3)), gr_flush.  

gr_rect_couleur(1,  C) :- send(@bb1,  fill_pattern, colour(C)), gr_flush.%
gr_rect_couleur(2,  C) :- send(@bb2,  fill_pattern, colour(C)), gr_flush.%
gr_rect_couleur(3,  C) :- send(@bb3,  fill_pattern, colour(C)), gr_flush.%
gr_rect_couleur(4,  C) :- send(@bb4,  fill_pattern, colour(C)), gr_flush.%
gr_rect_couleur(5,  C) :- send(@bb5,  fill_pattern, colour(C)), gr_flush.%
gr_rect_couleur(6,  C) :- send(@bb6,  fill_pattern, colour(C)), gr_flush.%
gr_rect_couleur(7,  C) :- send(@bb7,  fill_pattern, colour(C)), gr_flush.%
gr_rect_couleur(8,  C) :- send(@bb8,  fill_pattern, colour(C)), gr_flush.%
gr_rect_couleur(9,  C) :- send(@bb9,  fill_pattern, colour(C)), gr_flush.%
gr_rect_couleur(10, C) :- send(@bb10, fill_pattern, colour(C)), gr_flush.%



gr_end :- retractall(turtle(_,_,_)),
          retractall(tcouleur),
          retractall(tfleche),
          retractall(tpile),
          retractall(dtexture),
          retractall(tbox),
          free(@win).




gr_flush:- send(@win, flush).

   gr_f :- gr_flush.




turtle_draw(L):-
    retract(turtle(X1, Y1, W)),
    X2 is X1 + L*cos(W*pi/180), Y2 is Y1 - L*sin(W*pi/180),
    tcouleur(C), tfleche(F), !,
    send(@win, display, new(E, line(X1, Y1, X2, Y2,  F))),
    send(E, colour, C),
    assert(turtle(X2, Y2, W)).


turtle_turn(W):-
    retract(turtle(X, Y, W1)),
    W2 is W1 + W, assert(turtle(X, Y, W2)).


turtle_move(L):-
    retract(turtle(X1, Y1, W)),
    X2 is X1 + L*cos(W*pi/180),
    Y2 is Y1 - L*sin(W*pi/180),
      assert(turtle(X2, Y2, W)).


gr_dessine(T)   :- turtle_draw(T), gr_flush.

gr_deplace(T)   :- dtexture(none), turtle_move(T), !.

gr_deplace(L)   :- dtexture(TK),
                    retract(turtle(X1, Y1, W)),
                    X2 is X1 + L*cos(W*pi/180), Y2 is Y1 - L*sin(W*pi/180),
                    send(@win, display, new(E, line(X1, Y1, X2, Y2))),
                    send(E, colour, colour(antiquewhite3)),
                    send(E, texture, TK),
                    assert(turtle(X2, Y2, W)),
                    gr_flush
                   . %


gr_deplaceTexture(TK) :-
                    retractall(dtexture(_)),
                    assert(dtexture(TK))
                   .%


gr_tourne(R)    :- turtle_turn(R).
gr_gauche       :- gr_tourne(90).
gr_droite       :- gr_droite(-90).



gr_couleur_hsv(H,S,V) :- gr_couleur(colour(@default, H, S, V, hsv)).
gr_couleur_nom(V)     :- gr_couleur(colour(V)).
gr_couleur(C)         :- retractall(tcouleur(_)), assert(tcouleur(C)).


gr_fleche(C)    :- retractall(tfleche(_)),   assert(tfleche(C)).


gr_pos  :- turtle(X,Y,_),
           X1 is X-8, Y1 is Y-8,
           send(@win, display,  new(CC, circle(16)), point(X1,Y1)),
           send(CC, colour, colour(magenta)),
           tcouleur(C), gr_couleur(colour(magenta)),
           tfleche(F), gr_fleche(second),
           gr_dessine(10),
           gr_tourne(180), gr_deplace(10), gr_tourne(180),
           gr_couleur(C), gr_fleche(F),
           gr_flush().


gps :- gr_pos.


% pile de position


gr_push :-  turtle(X,Y,W), tcouleur(C), tfleche(F), asserta(pile(X,Y,W,C,F)).

gr_pop  :-  retract(pile(X,Y,W,C,F)),
            retractall(turtle(_,_,_)), assert(turtle(X,Y,W)),
            gr_couleur(C), gr_fleche(F), !.

gr_pop.






