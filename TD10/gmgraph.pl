

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
   B is 850, H is 820,
   B1 is B /2, H1 is H / 2,
   W1 is 0, assert(turtle(B1, H1, W1)),
   new(@win, picture('UBS - Licence 1', size(B, H))),
   send(@win, open) .%
 
  



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




%  Script Scala (400) / GM 2023 ----------------

gr_rect_pos(0, X,Y, Width, Height) :-  free(@bs0), send(@win, display, new(@bs0, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(0)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(1, X,Y, Width, Height) :-  free(@bs1), send(@win, display, new(@bs1, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(1)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(2, X,Y, Width, Height) :-  free(@bs2), send(@win, display, new(@bs2, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(2)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(3, X,Y, Width, Height) :-  free(@bs3), send(@win, display, new(@bs3, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(3)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(4, X,Y, Width, Height) :-  free(@bs4), send(@win, display, new(@bs4, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(4)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(5, X,Y, Width, Height) :-  free(@bs5), send(@win, display, new(@bs5, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(5)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(6, X,Y, Width, Height) :-  free(@bs6), send(@win, display, new(@bs6, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(6)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(7, X,Y, Width, Height) :-  free(@bs7), send(@win, display, new(@bs7, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(7)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(8, X,Y, Width, Height) :-  free(@bs8), send(@win, display, new(@bs8, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(8)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(9, X,Y, Width, Height) :-  free(@bs9), send(@win, display, new(@bs9, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(9)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(10, X,Y, Width, Height) :-  free(@bs10), send(@win, display, new(@bs10, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(10)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(11, X,Y, Width, Height) :-  free(@bs11), send(@win, display, new(@bs11, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(11)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(12, X,Y, Width, Height) :-  free(@bs12), send(@win, display, new(@bs12, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(12)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(13, X,Y, Width, Height) :-  free(@bs13), send(@win, display, new(@bs13, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(13)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(14, X,Y, Width, Height) :-  free(@bs14), send(@win, display, new(@bs14, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(14)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(15, X,Y, Width, Height) :-  free(@bs15), send(@win, display, new(@bs15, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(15)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(16, X,Y, Width, Height) :-  free(@bs16), send(@win, display, new(@bs16, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(16)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(17, X,Y, Width, Height) :-  free(@bs17), send(@win, display, new(@bs17, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(17)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(18, X,Y, Width, Height) :-  free(@bs18), send(@win, display, new(@bs18, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(18)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(19, X,Y, Width, Height) :-  free(@bs19), send(@win, display, new(@bs19, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(19)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(20, X,Y, Width, Height) :-  free(@bs20), send(@win, display, new(@bs20, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(20)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(21, X,Y, Width, Height) :-  free(@bs21), send(@win, display, new(@bs21, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(21)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(22, X,Y, Width, Height) :-  free(@bs22), send(@win, display, new(@bs22, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(22)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(23, X,Y, Width, Height) :-  free(@bs23), send(@win, display, new(@bs23, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(23)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(24, X,Y, Width, Height) :-  free(@bs24), send(@win, display, new(@bs24, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(24)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(25, X,Y, Width, Height) :-  free(@bs25), send(@win, display, new(@bs25, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(25)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(26, X,Y, Width, Height) :-  free(@bs26), send(@win, display, new(@bs26, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(26)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(27, X,Y, Width, Height) :-  free(@bs27), send(@win, display, new(@bs27, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(27)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(28, X,Y, Width, Height) :-  free(@bs28), send(@win, display, new(@bs28, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(28)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(29, X,Y, Width, Height) :-  free(@bs29), send(@win, display, new(@bs29, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(29)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(30, X,Y, Width, Height) :-  free(@bs30), send(@win, display, new(@bs30, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(30)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(31, X,Y, Width, Height) :-  free(@bs31), send(@win, display, new(@bs31, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(31)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(32, X,Y, Width, Height) :-  free(@bs32), send(@win, display, new(@bs32, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(32)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(33, X,Y, Width, Height) :-  free(@bs33), send(@win, display, new(@bs33, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(33)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(34, X,Y, Width, Height) :-  free(@bs34), send(@win, display, new(@bs34, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(34)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(35, X,Y, Width, Height) :-  free(@bs35), send(@win, display, new(@bs35, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(35)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(36, X,Y, Width, Height) :-  free(@bs36), send(@win, display, new(@bs36, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(36)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(37, X,Y, Width, Height) :-  free(@bs37), send(@win, display, new(@bs37, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(37)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(38, X,Y, Width, Height) :-  free(@bs38), send(@win, display, new(@bs38, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(38)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(39, X,Y, Width, Height) :-  free(@bs39), send(@win, display, new(@bs39, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(39)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(40, X,Y, Width, Height) :-  free(@bs40), send(@win, display, new(@bs40, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(40)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(41, X,Y, Width, Height) :-  free(@bs41), send(@win, display, new(@bs41, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(41)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(42, X,Y, Width, Height) :-  free(@bs42), send(@win, display, new(@bs42, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(42)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(43, X,Y, Width, Height) :-  free(@bs43), send(@win, display, new(@bs43, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(43)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(44, X,Y, Width, Height) :-  free(@bs44), send(@win, display, new(@bs44, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(44)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(45, X,Y, Width, Height) :-  free(@bs45), send(@win, display, new(@bs45, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(45)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(46, X,Y, Width, Height) :-  free(@bs46), send(@win, display, new(@bs46, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(46)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(47, X,Y, Width, Height) :-  free(@bs47), send(@win, display, new(@bs47, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(47)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(48, X,Y, Width, Height) :-  free(@bs48), send(@win, display, new(@bs48, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(48)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(49, X,Y, Width, Height) :-  free(@bs49), send(@win, display, new(@bs49, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(49)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(50, X,Y, Width, Height) :-  free(@bs50), send(@win, display, new(@bs50, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(50)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(51, X,Y, Width, Height) :-  free(@bs51), send(@win, display, new(@bs51, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(51)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(52, X,Y, Width, Height) :-  free(@bs52), send(@win, display, new(@bs52, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(52)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(53, X,Y, Width, Height) :-  free(@bs53), send(@win, display, new(@bs53, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(53)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(54, X,Y, Width, Height) :-  free(@bs54), send(@win, display, new(@bs54, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(54)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(55, X,Y, Width, Height) :-  free(@bs55), send(@win, display, new(@bs55, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(55)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(56, X,Y, Width, Height) :-  free(@bs56), send(@win, display, new(@bs56, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(56)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(57, X,Y, Width, Height) :-  free(@bs57), send(@win, display, new(@bs57, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(57)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(58, X,Y, Width, Height) :-  free(@bs58), send(@win, display, new(@bs58, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(58)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(59, X,Y, Width, Height) :-  free(@bs59), send(@win, display, new(@bs59, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(59)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(60, X,Y, Width, Height) :-  free(@bs60), send(@win, display, new(@bs60, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(60)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(61, X,Y, Width, Height) :-  free(@bs61), send(@win, display, new(@bs61, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(61)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(62, X,Y, Width, Height) :-  free(@bs62), send(@win, display, new(@bs62, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(62)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(63, X,Y, Width, Height) :-  free(@bs63), send(@win, display, new(@bs63, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(63)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(64, X,Y, Width, Height) :-  free(@bs64), send(@win, display, new(@bs64, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(64)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(65, X,Y, Width, Height) :-  free(@bs65), send(@win, display, new(@bs65, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(65)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(66, X,Y, Width, Height) :-  free(@bs66), send(@win, display, new(@bs66, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(66)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(67, X,Y, Width, Height) :-  free(@bs67), send(@win, display, new(@bs67, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(67)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(68, X,Y, Width, Height) :-  free(@bs68), send(@win, display, new(@bs68, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(68)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(69, X,Y, Width, Height) :-  free(@bs69), send(@win, display, new(@bs69, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(69)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(70, X,Y, Width, Height) :-  free(@bs70), send(@win, display, new(@bs70, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(70)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(71, X,Y, Width, Height) :-  free(@bs71), send(@win, display, new(@bs71, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(71)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(72, X,Y, Width, Height) :-  free(@bs72), send(@win, display, new(@bs72, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(72)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(73, X,Y, Width, Height) :-  free(@bs73), send(@win, display, new(@bs73, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(73)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(74, X,Y, Width, Height) :-  free(@bs74), send(@win, display, new(@bs74, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(74)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(75, X,Y, Width, Height) :-  free(@bs75), send(@win, display, new(@bs75, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(75)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(76, X,Y, Width, Height) :-  free(@bs76), send(@win, display, new(@bs76, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(76)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(77, X,Y, Width, Height) :-  free(@bs77), send(@win, display, new(@bs77, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(77)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(78, X,Y, Width, Height) :-  free(@bs78), send(@win, display, new(@bs78, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(78)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(79, X,Y, Width, Height) :-  free(@bs79), send(@win, display, new(@bs79, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(79)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(80, X,Y, Width, Height) :-  free(@bs80), send(@win, display, new(@bs80, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(80)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(81, X,Y, Width, Height) :-  free(@bs81), send(@win, display, new(@bs81, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(81)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(82, X,Y, Width, Height) :-  free(@bs82), send(@win, display, new(@bs82, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(82)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(83, X,Y, Width, Height) :-  free(@bs83), send(@win, display, new(@bs83, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(83)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(84, X,Y, Width, Height) :-  free(@bs84), send(@win, display, new(@bs84, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(84)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(85, X,Y, Width, Height) :-  free(@bs85), send(@win, display, new(@bs85, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(85)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(86, X,Y, Width, Height) :-  free(@bs86), send(@win, display, new(@bs86, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(86)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(87, X,Y, Width, Height) :-  free(@bs87), send(@win, display, new(@bs87, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(87)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(88, X,Y, Width, Height) :-  free(@bs88), send(@win, display, new(@bs88, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(88)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(89, X,Y, Width, Height) :-  free(@bs89), send(@win, display, new(@bs89, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(89)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(90, X,Y, Width, Height) :-  free(@bs90), send(@win, display, new(@bs90, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(90)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(91, X,Y, Width, Height) :-  free(@bs91), send(@win, display, new(@bs91, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(91)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(92, X,Y, Width, Height) :-  free(@bs92), send(@win, display, new(@bs92, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(92)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(93, X,Y, Width, Height) :-  free(@bs93), send(@win, display, new(@bs93, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(93)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(94, X,Y, Width, Height) :-  free(@bs94), send(@win, display, new(@bs94, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(94)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(95, X,Y, Width, Height) :-  free(@bs95), send(@win, display, new(@bs95, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(95)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(96, X,Y, Width, Height) :-  free(@bs96), send(@win, display, new(@bs96, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(96)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(97, X,Y, Width, Height) :-  free(@bs97), send(@win, display, new(@bs97, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(97)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(98, X,Y, Width, Height) :-  free(@bs98), send(@win, display, new(@bs98, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(98)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(99, X,Y, Width, Height) :-  free(@bs99), send(@win, display, new(@bs99, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(99)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(100, X,Y, Width, Height) :-  free(@bs100), send(@win, display, new(@bs100, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(100)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(101, X,Y, Width, Height) :-  free(@bs101), send(@win, display, new(@bs101, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(101)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(102, X,Y, Width, Height) :-  free(@bs102), send(@win, display, new(@bs102, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(102)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(103, X,Y, Width, Height) :-  free(@bs103), send(@win, display, new(@bs103, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(103)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(104, X,Y, Width, Height) :-  free(@bs104), send(@win, display, new(@bs104, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(104)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(105, X,Y, Width, Height) :-  free(@bs105), send(@win, display, new(@bs105, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(105)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(106, X,Y, Width, Height) :-  free(@bs106), send(@win, display, new(@bs106, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(106)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(107, X,Y, Width, Height) :-  free(@bs107), send(@win, display, new(@bs107, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(107)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(108, X,Y, Width, Height) :-  free(@bs108), send(@win, display, new(@bs108, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(108)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(109, X,Y, Width, Height) :-  free(@bs109), send(@win, display, new(@bs109, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(109)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(110, X,Y, Width, Height) :-  free(@bs110), send(@win, display, new(@bs110, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(110)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(111, X,Y, Width, Height) :-  free(@bs111), send(@win, display, new(@bs111, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(111)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(112, X,Y, Width, Height) :-  free(@bs112), send(@win, display, new(@bs112, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(112)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(113, X,Y, Width, Height) :-  free(@bs113), send(@win, display, new(@bs113, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(113)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(114, X,Y, Width, Height) :-  free(@bs114), send(@win, display, new(@bs114, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(114)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(115, X,Y, Width, Height) :-  free(@bs115), send(@win, display, new(@bs115, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(115)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(116, X,Y, Width, Height) :-  free(@bs116), send(@win, display, new(@bs116, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(116)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(117, X,Y, Width, Height) :-  free(@bs117), send(@win, display, new(@bs117, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(117)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(118, X,Y, Width, Height) :-  free(@bs118), send(@win, display, new(@bs118, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(118)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(119, X,Y, Width, Height) :-  free(@bs119), send(@win, display, new(@bs119, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(119)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(120, X,Y, Width, Height) :-  free(@bs120), send(@win, display, new(@bs120, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(120)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(121, X,Y, Width, Height) :-  free(@bs121), send(@win, display, new(@bs121, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(121)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(122, X,Y, Width, Height) :-  free(@bs122), send(@win, display, new(@bs122, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(122)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(123, X,Y, Width, Height) :-  free(@bs123), send(@win, display, new(@bs123, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(123)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(124, X,Y, Width, Height) :-  free(@bs124), send(@win, display, new(@bs124, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(124)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(125, X,Y, Width, Height) :-  free(@bs125), send(@win, display, new(@bs125, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(125)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(126, X,Y, Width, Height) :-  free(@bs126), send(@win, display, new(@bs126, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(126)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(127, X,Y, Width, Height) :-  free(@bs127), send(@win, display, new(@bs127, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(127)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(128, X,Y, Width, Height) :-  free(@bs128), send(@win, display, new(@bs128, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(128)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(129, X,Y, Width, Height) :-  free(@bs129), send(@win, display, new(@bs129, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(129)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(130, X,Y, Width, Height) :-  free(@bs130), send(@win, display, new(@bs130, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(130)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(131, X,Y, Width, Height) :-  free(@bs131), send(@win, display, new(@bs131, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(131)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(132, X,Y, Width, Height) :-  free(@bs132), send(@win, display, new(@bs132, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(132)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(133, X,Y, Width, Height) :-  free(@bs133), send(@win, display, new(@bs133, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(133)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(134, X,Y, Width, Height) :-  free(@bs134), send(@win, display, new(@bs134, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(134)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(135, X,Y, Width, Height) :-  free(@bs135), send(@win, display, new(@bs135, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(135)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(136, X,Y, Width, Height) :-  free(@bs136), send(@win, display, new(@bs136, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(136)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(137, X,Y, Width, Height) :-  free(@bs137), send(@win, display, new(@bs137, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(137)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(138, X,Y, Width, Height) :-  free(@bs138), send(@win, display, new(@bs138, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(138)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(139, X,Y, Width, Height) :-  free(@bs139), send(@win, display, new(@bs139, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(139)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(140, X,Y, Width, Height) :-  free(@bs140), send(@win, display, new(@bs140, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(140)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(141, X,Y, Width, Height) :-  free(@bs141), send(@win, display, new(@bs141, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(141)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(142, X,Y, Width, Height) :-  free(@bs142), send(@win, display, new(@bs142, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(142)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(143, X,Y, Width, Height) :-  free(@bs143), send(@win, display, new(@bs143, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(143)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(144, X,Y, Width, Height) :-  free(@bs144), send(@win, display, new(@bs144, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(144)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(145, X,Y, Width, Height) :-  free(@bs145), send(@win, display, new(@bs145, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(145)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(146, X,Y, Width, Height) :-  free(@bs146), send(@win, display, new(@bs146, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(146)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(147, X,Y, Width, Height) :-  free(@bs147), send(@win, display, new(@bs147, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(147)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(148, X,Y, Width, Height) :-  free(@bs148), send(@win, display, new(@bs148, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(148)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(149, X,Y, Width, Height) :-  free(@bs149), send(@win, display, new(@bs149, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(149)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(150, X,Y, Width, Height) :-  free(@bs150), send(@win, display, new(@bs150, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(150)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(151, X,Y, Width, Height) :-  free(@bs151), send(@win, display, new(@bs151, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(151)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(152, X,Y, Width, Height) :-  free(@bs152), send(@win, display, new(@bs152, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(152)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(153, X,Y, Width, Height) :-  free(@bs153), send(@win, display, new(@bs153, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(153)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(154, X,Y, Width, Height) :-  free(@bs154), send(@win, display, new(@bs154, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(154)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(155, X,Y, Width, Height) :-  free(@bs155), send(@win, display, new(@bs155, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(155)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(156, X,Y, Width, Height) :-  free(@bs156), send(@win, display, new(@bs156, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(156)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(157, X,Y, Width, Height) :-  free(@bs157), send(@win, display, new(@bs157, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(157)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(158, X,Y, Width, Height) :-  free(@bs158), send(@win, display, new(@bs158, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(158)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(159, X,Y, Width, Height) :-  free(@bs159), send(@win, display, new(@bs159, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(159)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(160, X,Y, Width, Height) :-  free(@bs160), send(@win, display, new(@bs160, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(160)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(161, X,Y, Width, Height) :-  free(@bs161), send(@win, display, new(@bs161, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(161)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(162, X,Y, Width, Height) :-  free(@bs162), send(@win, display, new(@bs162, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(162)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(163, X,Y, Width, Height) :-  free(@bs163), send(@win, display, new(@bs163, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(163)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(164, X,Y, Width, Height) :-  free(@bs164), send(@win, display, new(@bs164, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(164)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(165, X,Y, Width, Height) :-  free(@bs165), send(@win, display, new(@bs165, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(165)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(166, X,Y, Width, Height) :-  free(@bs166), send(@win, display, new(@bs166, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(166)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(167, X,Y, Width, Height) :-  free(@bs167), send(@win, display, new(@bs167, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(167)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(168, X,Y, Width, Height) :-  free(@bs168), send(@win, display, new(@bs168, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(168)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(169, X,Y, Width, Height) :-  free(@bs169), send(@win, display, new(@bs169, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(169)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(170, X,Y, Width, Height) :-  free(@bs170), send(@win, display, new(@bs170, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(170)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(171, X,Y, Width, Height) :-  free(@bs171), send(@win, display, new(@bs171, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(171)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(172, X,Y, Width, Height) :-  free(@bs172), send(@win, display, new(@bs172, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(172)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(173, X,Y, Width, Height) :-  free(@bs173), send(@win, display, new(@bs173, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(173)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(174, X,Y, Width, Height) :-  free(@bs174), send(@win, display, new(@bs174, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(174)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(175, X,Y, Width, Height) :-  free(@bs175), send(@win, display, new(@bs175, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(175)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(176, X,Y, Width, Height) :-  free(@bs176), send(@win, display, new(@bs176, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(176)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(177, X,Y, Width, Height) :-  free(@bs177), send(@win, display, new(@bs177, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(177)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(178, X,Y, Width, Height) :-  free(@bs178), send(@win, display, new(@bs178, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(178)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(179, X,Y, Width, Height) :-  free(@bs179), send(@win, display, new(@bs179, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(179)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(180, X,Y, Width, Height) :-  free(@bs180), send(@win, display, new(@bs180, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(180)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(181, X,Y, Width, Height) :-  free(@bs181), send(@win, display, new(@bs181, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(181)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(182, X,Y, Width, Height) :-  free(@bs182), send(@win, display, new(@bs182, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(182)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(183, X,Y, Width, Height) :-  free(@bs183), send(@win, display, new(@bs183, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(183)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(184, X,Y, Width, Height) :-  free(@bs184), send(@win, display, new(@bs184, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(184)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(185, X,Y, Width, Height) :-  free(@bs185), send(@win, display, new(@bs185, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(185)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(186, X,Y, Width, Height) :-  free(@bs186), send(@win, display, new(@bs186, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(186)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(187, X,Y, Width, Height) :-  free(@bs187), send(@win, display, new(@bs187, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(187)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(188, X,Y, Width, Height) :-  free(@bs188), send(@win, display, new(@bs188, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(188)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(189, X,Y, Width, Height) :-  free(@bs189), send(@win, display, new(@bs189, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(189)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(190, X,Y, Width, Height) :-  free(@bs190), send(@win, display, new(@bs190, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(190)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(191, X,Y, Width, Height) :-  free(@bs191), send(@win, display, new(@bs191, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(191)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(192, X,Y, Width, Height) :-  free(@bs192), send(@win, display, new(@bs192, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(192)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(193, X,Y, Width, Height) :-  free(@bs193), send(@win, display, new(@bs193, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(193)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(194, X,Y, Width, Height) :-  free(@bs194), send(@win, display, new(@bs194, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(194)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(195, X,Y, Width, Height) :-  free(@bs195), send(@win, display, new(@bs195, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(195)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(196, X,Y, Width, Height) :-  free(@bs196), send(@win, display, new(@bs196, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(196)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(197, X,Y, Width, Height) :-  free(@bs197), send(@win, display, new(@bs197, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(197)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(198, X,Y, Width, Height) :-  free(@bs198), send(@win, display, new(@bs198, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(198)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(199, X,Y, Width, Height) :-  free(@bs199), send(@win, display, new(@bs199, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(199)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(200, X,Y, Width, Height) :-  free(@bs200), send(@win, display, new(@bs200, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(200)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(201, X,Y, Width, Height) :-  free(@bs201), send(@win, display, new(@bs201, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(201)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(202, X,Y, Width, Height) :-  free(@bs202), send(@win, display, new(@bs202, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(202)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(203, X,Y, Width, Height) :-  free(@bs203), send(@win, display, new(@bs203, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(203)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(204, X,Y, Width, Height) :-  free(@bs204), send(@win, display, new(@bs204, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(204)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(205, X,Y, Width, Height) :-  free(@bs205), send(@win, display, new(@bs205, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(205)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(206, X,Y, Width, Height) :-  free(@bs206), send(@win, display, new(@bs206, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(206)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(207, X,Y, Width, Height) :-  free(@bs207), send(@win, display, new(@bs207, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(207)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(208, X,Y, Width, Height) :-  free(@bs208), send(@win, display, new(@bs208, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(208)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(209, X,Y, Width, Height) :-  free(@bs209), send(@win, display, new(@bs209, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(209)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(210, X,Y, Width, Height) :-  free(@bs210), send(@win, display, new(@bs210, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(210)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(211, X,Y, Width, Height) :-  free(@bs211), send(@win, display, new(@bs211, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(211)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(212, X,Y, Width, Height) :-  free(@bs212), send(@win, display, new(@bs212, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(212)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(213, X,Y, Width, Height) :-  free(@bs213), send(@win, display, new(@bs213, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(213)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(214, X,Y, Width, Height) :-  free(@bs214), send(@win, display, new(@bs214, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(214)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(215, X,Y, Width, Height) :-  free(@bs215), send(@win, display, new(@bs215, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(215)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(216, X,Y, Width, Height) :-  free(@bs216), send(@win, display, new(@bs216, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(216)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(217, X,Y, Width, Height) :-  free(@bs217), send(@win, display, new(@bs217, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(217)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(218, X,Y, Width, Height) :-  free(@bs218), send(@win, display, new(@bs218, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(218)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(219, X,Y, Width, Height) :-  free(@bs219), send(@win, display, new(@bs219, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(219)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(220, X,Y, Width, Height) :-  free(@bs220), send(@win, display, new(@bs220, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(220)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(221, X,Y, Width, Height) :-  free(@bs221), send(@win, display, new(@bs221, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(221)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(222, X,Y, Width, Height) :-  free(@bs222), send(@win, display, new(@bs222, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(222)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(223, X,Y, Width, Height) :-  free(@bs223), send(@win, display, new(@bs223, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(223)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(224, X,Y, Width, Height) :-  free(@bs224), send(@win, display, new(@bs224, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(224)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(225, X,Y, Width, Height) :-  free(@bs225), send(@win, display, new(@bs225, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(225)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(226, X,Y, Width, Height) :-  free(@bs226), send(@win, display, new(@bs226, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(226)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(227, X,Y, Width, Height) :-  free(@bs227), send(@win, display, new(@bs227, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(227)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(228, X,Y, Width, Height) :-  free(@bs228), send(@win, display, new(@bs228, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(228)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(229, X,Y, Width, Height) :-  free(@bs229), send(@win, display, new(@bs229, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(229)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(230, X,Y, Width, Height) :-  free(@bs230), send(@win, display, new(@bs230, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(230)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(231, X,Y, Width, Height) :-  free(@bs231), send(@win, display, new(@bs231, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(231)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(232, X,Y, Width, Height) :-  free(@bs232), send(@win, display, new(@bs232, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(232)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(233, X,Y, Width, Height) :-  free(@bs233), send(@win, display, new(@bs233, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(233)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(234, X,Y, Width, Height) :-  free(@bs234), send(@win, display, new(@bs234, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(234)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(235, X,Y, Width, Height) :-  free(@bs235), send(@win, display, new(@bs235, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(235)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(236, X,Y, Width, Height) :-  free(@bs236), send(@win, display, new(@bs236, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(236)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(237, X,Y, Width, Height) :-  free(@bs237), send(@win, display, new(@bs237, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(237)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(238, X,Y, Width, Height) :-  free(@bs238), send(@win, display, new(@bs238, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(238)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(239, X,Y, Width, Height) :-  free(@bs239), send(@win, display, new(@bs239, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(239)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(240, X,Y, Width, Height) :-  free(@bs240), send(@win, display, new(@bs240, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(240)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(241, X,Y, Width, Height) :-  free(@bs241), send(@win, display, new(@bs241, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(241)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(242, X,Y, Width, Height) :-  free(@bs242), send(@win, display, new(@bs242, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(242)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(243, X,Y, Width, Height) :-  free(@bs243), send(@win, display, new(@bs243, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(243)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(244, X,Y, Width, Height) :-  free(@bs244), send(@win, display, new(@bs244, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(244)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(245, X,Y, Width, Height) :-  free(@bs245), send(@win, display, new(@bs245, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(245)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(246, X,Y, Width, Height) :-  free(@bs246), send(@win, display, new(@bs246, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(246)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(247, X,Y, Width, Height) :-  free(@bs247), send(@win, display, new(@bs247, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(247)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(248, X,Y, Width, Height) :-  free(@bs248), send(@win, display, new(@bs248, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(248)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(249, X,Y, Width, Height) :-  free(@bs249), send(@win, display, new(@bs249, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(249)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(250, X,Y, Width, Height) :-  free(@bs250), send(@win, display, new(@bs250, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(250)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(251, X,Y, Width, Height) :-  free(@bs251), send(@win, display, new(@bs251, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(251)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(252, X,Y, Width, Height) :-  free(@bs252), send(@win, display, new(@bs252, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(252)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(253, X,Y, Width, Height) :-  free(@bs253), send(@win, display, new(@bs253, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(253)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(254, X,Y, Width, Height) :-  free(@bs254), send(@win, display, new(@bs254, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(254)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(255, X,Y, Width, Height) :-  free(@bs255), send(@win, display, new(@bs255, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(255)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(256, X,Y, Width, Height) :-  free(@bs256), send(@win, display, new(@bs256, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(256)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(257, X,Y, Width, Height) :-  free(@bs257), send(@win, display, new(@bs257, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(257)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(258, X,Y, Width, Height) :-  free(@bs258), send(@win, display, new(@bs258, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(258)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(259, X,Y, Width, Height) :-  free(@bs259), send(@win, display, new(@bs259, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(259)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(260, X,Y, Width, Height) :-  free(@bs260), send(@win, display, new(@bs260, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(260)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(261, X,Y, Width, Height) :-  free(@bs261), send(@win, display, new(@bs261, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(261)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(262, X,Y, Width, Height) :-  free(@bs262), send(@win, display, new(@bs262, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(262)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(263, X,Y, Width, Height) :-  free(@bs263), send(@win, display, new(@bs263, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(263)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(264, X,Y, Width, Height) :-  free(@bs264), send(@win, display, new(@bs264, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(264)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(265, X,Y, Width, Height) :-  free(@bs265), send(@win, display, new(@bs265, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(265)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(266, X,Y, Width, Height) :-  free(@bs266), send(@win, display, new(@bs266, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(266)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(267, X,Y, Width, Height) :-  free(@bs267), send(@win, display, new(@bs267, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(267)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(268, X,Y, Width, Height) :-  free(@bs268), send(@win, display, new(@bs268, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(268)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(269, X,Y, Width, Height) :-  free(@bs269), send(@win, display, new(@bs269, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(269)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(270, X,Y, Width, Height) :-  free(@bs270), send(@win, display, new(@bs270, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(270)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(271, X,Y, Width, Height) :-  free(@bs271), send(@win, display, new(@bs271, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(271)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(272, X,Y, Width, Height) :-  free(@bs272), send(@win, display, new(@bs272, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(272)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(273, X,Y, Width, Height) :-  free(@bs273), send(@win, display, new(@bs273, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(273)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(274, X,Y, Width, Height) :-  free(@bs274), send(@win, display, new(@bs274, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(274)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(275, X,Y, Width, Height) :-  free(@bs275), send(@win, display, new(@bs275, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(275)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(276, X,Y, Width, Height) :-  free(@bs276), send(@win, display, new(@bs276, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(276)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(277, X,Y, Width, Height) :-  free(@bs277), send(@win, display, new(@bs277, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(277)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(278, X,Y, Width, Height) :-  free(@bs278), send(@win, display, new(@bs278, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(278)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(279, X,Y, Width, Height) :-  free(@bs279), send(@win, display, new(@bs279, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(279)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(280, X,Y, Width, Height) :-  free(@bs280), send(@win, display, new(@bs280, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(280)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(281, X,Y, Width, Height) :-  free(@bs281), send(@win, display, new(@bs281, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(281)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(282, X,Y, Width, Height) :-  free(@bs282), send(@win, display, new(@bs282, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(282)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(283, X,Y, Width, Height) :-  free(@bs283), send(@win, display, new(@bs283, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(283)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(284, X,Y, Width, Height) :-  free(@bs284), send(@win, display, new(@bs284, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(284)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(285, X,Y, Width, Height) :-  free(@bs285), send(@win, display, new(@bs285, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(285)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(286, X,Y, Width, Height) :-  free(@bs286), send(@win, display, new(@bs286, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(286)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(287, X,Y, Width, Height) :-  free(@bs287), send(@win, display, new(@bs287, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(287)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(288, X,Y, Width, Height) :-  free(@bs288), send(@win, display, new(@bs288, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(288)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(289, X,Y, Width, Height) :-  free(@bs289), send(@win, display, new(@bs289, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(289)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(290, X,Y, Width, Height) :-  free(@bs290), send(@win, display, new(@bs290, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(290)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(291, X,Y, Width, Height) :-  free(@bs291), send(@win, display, new(@bs291, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(291)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(292, X,Y, Width, Height) :-  free(@bs292), send(@win, display, new(@bs292, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(292)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(293, X,Y, Width, Height) :-  free(@bs293), send(@win, display, new(@bs293, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(293)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(294, X,Y, Width, Height) :-  free(@bs294), send(@win, display, new(@bs294, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(294)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(295, X,Y, Width, Height) :-  free(@bs295), send(@win, display, new(@bs295, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(295)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(296, X,Y, Width, Height) :-  free(@bs296), send(@win, display, new(@bs296, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(296)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(297, X,Y, Width, Height) :-  free(@bs297), send(@win, display, new(@bs297, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(297)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(298, X,Y, Width, Height) :-  free(@bs298), send(@win, display, new(@bs298, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(298)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(299, X,Y, Width, Height) :-  free(@bs299), send(@win, display, new(@bs299, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(299)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(300, X,Y, Width, Height) :-  free(@bs300), send(@win, display, new(@bs300, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(300)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(301, X,Y, Width, Height) :-  free(@bs301), send(@win, display, new(@bs301, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(301)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(302, X,Y, Width, Height) :-  free(@bs302), send(@win, display, new(@bs302, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(302)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(303, X,Y, Width, Height) :-  free(@bs303), send(@win, display, new(@bs303, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(303)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(304, X,Y, Width, Height) :-  free(@bs304), send(@win, display, new(@bs304, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(304)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(305, X,Y, Width, Height) :-  free(@bs305), send(@win, display, new(@bs305, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(305)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(306, X,Y, Width, Height) :-  free(@bs306), send(@win, display, new(@bs306, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(306)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(307, X,Y, Width, Height) :-  free(@bs307), send(@win, display, new(@bs307, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(307)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(308, X,Y, Width, Height) :-  free(@bs308), send(@win, display, new(@bs308, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(308)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(309, X,Y, Width, Height) :-  free(@bs309), send(@win, display, new(@bs309, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(309)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(310, X,Y, Width, Height) :-  free(@bs310), send(@win, display, new(@bs310, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(310)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(311, X,Y, Width, Height) :-  free(@bs311), send(@win, display, new(@bs311, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(311)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(312, X,Y, Width, Height) :-  free(@bs312), send(@win, display, new(@bs312, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(312)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(313, X,Y, Width, Height) :-  free(@bs313), send(@win, display, new(@bs313, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(313)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(314, X,Y, Width, Height) :-  free(@bs314), send(@win, display, new(@bs314, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(314)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(315, X,Y, Width, Height) :-  free(@bs315), send(@win, display, new(@bs315, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(315)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(316, X,Y, Width, Height) :-  free(@bs316), send(@win, display, new(@bs316, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(316)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(317, X,Y, Width, Height) :-  free(@bs317), send(@win, display, new(@bs317, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(317)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(318, X,Y, Width, Height) :-  free(@bs318), send(@win, display, new(@bs318, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(318)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(319, X,Y, Width, Height) :-  free(@bs319), send(@win, display, new(@bs319, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(319)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(320, X,Y, Width, Height) :-  free(@bs320), send(@win, display, new(@bs320, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(320)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(321, X,Y, Width, Height) :-  free(@bs321), send(@win, display, new(@bs321, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(321)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(322, X,Y, Width, Height) :-  free(@bs322), send(@win, display, new(@bs322, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(322)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(323, X,Y, Width, Height) :-  free(@bs323), send(@win, display, new(@bs323, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(323)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(324, X,Y, Width, Height) :-  free(@bs324), send(@win, display, new(@bs324, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(324)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(325, X,Y, Width, Height) :-  free(@bs325), send(@win, display, new(@bs325, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(325)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(326, X,Y, Width, Height) :-  free(@bs326), send(@win, display, new(@bs326, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(326)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(327, X,Y, Width, Height) :-  free(@bs327), send(@win, display, new(@bs327, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(327)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(328, X,Y, Width, Height) :-  free(@bs328), send(@win, display, new(@bs328, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(328)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(329, X,Y, Width, Height) :-  free(@bs329), send(@win, display, new(@bs329, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(329)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(330, X,Y, Width, Height) :-  free(@bs330), send(@win, display, new(@bs330, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(330)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(331, X,Y, Width, Height) :-  free(@bs331), send(@win, display, new(@bs331, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(331)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(332, X,Y, Width, Height) :-  free(@bs332), send(@win, display, new(@bs332, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(332)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(333, X,Y, Width, Height) :-  free(@bs333), send(@win, display, new(@bs333, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(333)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(334, X,Y, Width, Height) :-  free(@bs334), send(@win, display, new(@bs334, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(334)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(335, X,Y, Width, Height) :-  free(@bs335), send(@win, display, new(@bs335, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(335)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(336, X,Y, Width, Height) :-  free(@bs336), send(@win, display, new(@bs336, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(336)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(337, X,Y, Width, Height) :-  free(@bs337), send(@win, display, new(@bs337, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(337)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(338, X,Y, Width, Height) :-  free(@bs338), send(@win, display, new(@bs338, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(338)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(339, X,Y, Width, Height) :-  free(@bs339), send(@win, display, new(@bs339, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(339)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(340, X,Y, Width, Height) :-  free(@bs340), send(@win, display, new(@bs340, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(340)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(341, X,Y, Width, Height) :-  free(@bs341), send(@win, display, new(@bs341, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(341)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(342, X,Y, Width, Height) :-  free(@bs342), send(@win, display, new(@bs342, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(342)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(343, X,Y, Width, Height) :-  free(@bs343), send(@win, display, new(@bs343, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(343)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(344, X,Y, Width, Height) :-  free(@bs344), send(@win, display, new(@bs344, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(344)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(345, X,Y, Width, Height) :-  free(@bs345), send(@win, display, new(@bs345, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(345)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(346, X,Y, Width, Height) :-  free(@bs346), send(@win, display, new(@bs346, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(346)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(347, X,Y, Width, Height) :-  free(@bs347), send(@win, display, new(@bs347, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(347)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(348, X,Y, Width, Height) :-  free(@bs348), send(@win, display, new(@bs348, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(348)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(349, X,Y, Width, Height) :-  free(@bs349), send(@win, display, new(@bs349, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(349)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(350, X,Y, Width, Height) :-  free(@bs350), send(@win, display, new(@bs350, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(350)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(351, X,Y, Width, Height) :-  free(@bs351), send(@win, display, new(@bs351, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(351)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(352, X,Y, Width, Height) :-  free(@bs352), send(@win, display, new(@bs352, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(352)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(353, X,Y, Width, Height) :-  free(@bs353), send(@win, display, new(@bs353, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(353)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(354, X,Y, Width, Height) :-  free(@bs354), send(@win, display, new(@bs354, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(354)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(355, X,Y, Width, Height) :-  free(@bs355), send(@win, display, new(@bs355, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(355)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(356, X,Y, Width, Height) :-  free(@bs356), send(@win, display, new(@bs356, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(356)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(357, X,Y, Width, Height) :-  free(@bs357), send(@win, display, new(@bs357, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(357)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(358, X,Y, Width, Height) :-  free(@bs358), send(@win, display, new(@bs358, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(358)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(359, X,Y, Width, Height) :-  free(@bs359), send(@win, display, new(@bs359, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(359)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(360, X,Y, Width, Height) :-  free(@bs360), send(@win, display, new(@bs360, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(360)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(361, X,Y, Width, Height) :-  free(@bs361), send(@win, display, new(@bs361, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(361)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(362, X,Y, Width, Height) :-  free(@bs362), send(@win, display, new(@bs362, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(362)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(363, X,Y, Width, Height) :-  free(@bs363), send(@win, display, new(@bs363, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(363)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(364, X,Y, Width, Height) :-  free(@bs364), send(@win, display, new(@bs364, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(364)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(365, X,Y, Width, Height) :-  free(@bs365), send(@win, display, new(@bs365, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(365)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(366, X,Y, Width, Height) :-  free(@bs366), send(@win, display, new(@bs366, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(366)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(367, X,Y, Width, Height) :-  free(@bs367), send(@win, display, new(@bs367, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(367)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(368, X,Y, Width, Height) :-  free(@bs368), send(@win, display, new(@bs368, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(368)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(369, X,Y, Width, Height) :-  free(@bs369), send(@win, display, new(@bs369, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(369)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(370, X,Y, Width, Height) :-  free(@bs370), send(@win, display, new(@bs370, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(370)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(371, X,Y, Width, Height) :-  free(@bs371), send(@win, display, new(@bs371, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(371)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(372, X,Y, Width, Height) :-  free(@bs372), send(@win, display, new(@bs372, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(372)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(373, X,Y, Width, Height) :-  free(@bs373), send(@win, display, new(@bs373, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(373)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(374, X,Y, Width, Height) :-  free(@bs374), send(@win, display, new(@bs374, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(374)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(375, X,Y, Width, Height) :-  free(@bs375), send(@win, display, new(@bs375, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(375)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(376, X,Y, Width, Height) :-  free(@bs376), send(@win, display, new(@bs376, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(376)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(377, X,Y, Width, Height) :-  free(@bs377), send(@win, display, new(@bs377, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(377)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(378, X,Y, Width, Height) :-  free(@bs378), send(@win, display, new(@bs378, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(378)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(379, X,Y, Width, Height) :-  free(@bs379), send(@win, display, new(@bs379, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(379)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(380, X,Y, Width, Height) :-  free(@bs380), send(@win, display, new(@bs380, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(380)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(381, X,Y, Width, Height) :-  free(@bs381), send(@win, display, new(@bs381, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(381)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(382, X,Y, Width, Height) :-  free(@bs382), send(@win, display, new(@bs382, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(382)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(383, X,Y, Width, Height) :-  free(@bs383), send(@win, display, new(@bs383, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(383)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(384, X,Y, Width, Height) :-  free(@bs384), send(@win, display, new(@bs384, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(384)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(385, X,Y, Width, Height) :-  free(@bs385), send(@win, display, new(@bs385, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(385)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(386, X,Y, Width, Height) :-  free(@bs386), send(@win, display, new(@bs386, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(386)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(387, X,Y, Width, Height) :-  free(@bs387), send(@win, display, new(@bs387, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(387)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(388, X,Y, Width, Height) :-  free(@bs388), send(@win, display, new(@bs388, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(388)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(389, X,Y, Width, Height) :-  free(@bs389), send(@win, display, new(@bs389, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(389)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(390, X,Y, Width, Height) :-  free(@bs390), send(@win, display, new(@bs390, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(390)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(391, X,Y, Width, Height) :-  free(@bs391), send(@win, display, new(@bs391, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(391)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(392, X,Y, Width, Height) :-  free(@bs392), send(@win, display, new(@bs392, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(392)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(393, X,Y, Width, Height) :-  free(@bs393), send(@win, display, new(@bs393, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(393)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(394, X,Y, Width, Height) :-  free(@bs394), send(@win, display, new(@bs394, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(394)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(395, X,Y, Width, Height) :-  free(@bs395), send(@win, display, new(@bs395, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(395)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(396, X,Y, Width, Height) :-  free(@bs396), send(@win, display, new(@bs396, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(396)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(397, X,Y, Width, Height) :-  free(@bs397), send(@win, display, new(@bs397, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(397)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(398, X,Y, Width, Height) :-  free(@bs398), send(@win, display, new(@bs398, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(398)), point(X+3, Y+3)), gr_flush.
gr_rect_pos(399, X,Y, Width, Height) :-  free(@bs399), send(@win, display, new(@bs399, box(Width,Height)), point(X, Y)),send(@win, display, new(_, text(399)), point(X+3, Y+3)), gr_flush.

gr_rect_couleur(0,  C) :- send(@bs0,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(1,  C) :- send(@bs1,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(2,  C) :- send(@bs2,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(3,  C) :- send(@bs3,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(4,  C) :- send(@bs4,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(5,  C) :- send(@bs5,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(6,  C) :- send(@bs6,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(7,  C) :- send(@bs7,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(8,  C) :- send(@bs8,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(9,  C) :- send(@bs9,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(10,  C) :- send(@bs10,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(11,  C) :- send(@bs11,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(12,  C) :- send(@bs12,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(13,  C) :- send(@bs13,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(14,  C) :- send(@bs14,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(15,  C) :- send(@bs15,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(16,  C) :- send(@bs16,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(17,  C) :- send(@bs17,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(18,  C) :- send(@bs18,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(19,  C) :- send(@bs19,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(20,  C) :- send(@bs20,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(21,  C) :- send(@bs21,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(22,  C) :- send(@bs22,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(23,  C) :- send(@bs23,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(24,  C) :- send(@bs24,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(25,  C) :- send(@bs25,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(26,  C) :- send(@bs26,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(27,  C) :- send(@bs27,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(28,  C) :- send(@bs28,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(29,  C) :- send(@bs29,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(30,  C) :- send(@bs30,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(31,  C) :- send(@bs31,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(32,  C) :- send(@bs32,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(33,  C) :- send(@bs33,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(34,  C) :- send(@bs34,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(35,  C) :- send(@bs35,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(36,  C) :- send(@bs36,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(37,  C) :- send(@bs37,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(38,  C) :- send(@bs38,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(39,  C) :- send(@bs39,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(40,  C) :- send(@bs40,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(41,  C) :- send(@bs41,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(42,  C) :- send(@bs42,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(43,  C) :- send(@bs43,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(44,  C) :- send(@bs44,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(45,  C) :- send(@bs45,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(46,  C) :- send(@bs46,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(47,  C) :- send(@bs47,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(48,  C) :- send(@bs48,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(49,  C) :- send(@bs49,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(50,  C) :- send(@bs50,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(51,  C) :- send(@bs51,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(52,  C) :- send(@bs52,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(53,  C) :- send(@bs53,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(54,  C) :- send(@bs54,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(55,  C) :- send(@bs55,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(56,  C) :- send(@bs56,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(57,  C) :- send(@bs57,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(58,  C) :- send(@bs58,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(59,  C) :- send(@bs59,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(60,  C) :- send(@bs60,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(61,  C) :- send(@bs61,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(62,  C) :- send(@bs62,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(63,  C) :- send(@bs63,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(64,  C) :- send(@bs64,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(65,  C) :- send(@bs65,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(66,  C) :- send(@bs66,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(67,  C) :- send(@bs67,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(68,  C) :- send(@bs68,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(69,  C) :- send(@bs69,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(70,  C) :- send(@bs70,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(71,  C) :- send(@bs71,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(72,  C) :- send(@bs72,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(73,  C) :- send(@bs73,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(74,  C) :- send(@bs74,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(75,  C) :- send(@bs75,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(76,  C) :- send(@bs76,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(77,  C) :- send(@bs77,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(78,  C) :- send(@bs78,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(79,  C) :- send(@bs79,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(80,  C) :- send(@bs80,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(81,  C) :- send(@bs81,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(82,  C) :- send(@bs82,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(83,  C) :- send(@bs83,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(84,  C) :- send(@bs84,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(85,  C) :- send(@bs85,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(86,  C) :- send(@bs86,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(87,  C) :- send(@bs87,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(88,  C) :- send(@bs88,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(89,  C) :- send(@bs89,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(90,  C) :- send(@bs90,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(91,  C) :- send(@bs91,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(92,  C) :- send(@bs92,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(93,  C) :- send(@bs93,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(94,  C) :- send(@bs94,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(95,  C) :- send(@bs95,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(96,  C) :- send(@bs96,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(97,  C) :- send(@bs97,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(98,  C) :- send(@bs98,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(99,  C) :- send(@bs99,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(100,  C) :- send(@bs100,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(101,  C) :- send(@bs101,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(102,  C) :- send(@bs102,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(103,  C) :- send(@bs103,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(104,  C) :- send(@bs104,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(105,  C) :- send(@bs105,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(106,  C) :- send(@bs106,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(107,  C) :- send(@bs107,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(108,  C) :- send(@bs108,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(109,  C) :- send(@bs109,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(110,  C) :- send(@bs110,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(111,  C) :- send(@bs111,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(112,  C) :- send(@bs112,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(113,  C) :- send(@bs113,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(114,  C) :- send(@bs114,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(115,  C) :- send(@bs115,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(116,  C) :- send(@bs116,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(117,  C) :- send(@bs117,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(118,  C) :- send(@bs118,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(119,  C) :- send(@bs119,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(120,  C) :- send(@bs120,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(121,  C) :- send(@bs121,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(122,  C) :- send(@bs122,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(123,  C) :- send(@bs123,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(124,  C) :- send(@bs124,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(125,  C) :- send(@bs125,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(126,  C) :- send(@bs126,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(127,  C) :- send(@bs127,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(128,  C) :- send(@bs128,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(129,  C) :- send(@bs129,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(130,  C) :- send(@bs130,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(131,  C) :- send(@bs131,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(132,  C) :- send(@bs132,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(133,  C) :- send(@bs133,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(134,  C) :- send(@bs134,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(135,  C) :- send(@bs135,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(136,  C) :- send(@bs136,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(137,  C) :- send(@bs137,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(138,  C) :- send(@bs138,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(139,  C) :- send(@bs139,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(140,  C) :- send(@bs140,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(141,  C) :- send(@bs141,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(142,  C) :- send(@bs142,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(143,  C) :- send(@bs143,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(144,  C) :- send(@bs144,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(145,  C) :- send(@bs145,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(146,  C) :- send(@bs146,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(147,  C) :- send(@bs147,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(148,  C) :- send(@bs148,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(149,  C) :- send(@bs149,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(150,  C) :- send(@bs150,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(151,  C) :- send(@bs151,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(152,  C) :- send(@bs152,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(153,  C) :- send(@bs153,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(154,  C) :- send(@bs154,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(155,  C) :- send(@bs155,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(156,  C) :- send(@bs156,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(157,  C) :- send(@bs157,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(158,  C) :- send(@bs158,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(159,  C) :- send(@bs159,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(160,  C) :- send(@bs160,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(161,  C) :- send(@bs161,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(162,  C) :- send(@bs162,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(163,  C) :- send(@bs163,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(164,  C) :- send(@bs164,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(165,  C) :- send(@bs165,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(166,  C) :- send(@bs166,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(167,  C) :- send(@bs167,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(168,  C) :- send(@bs168,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(169,  C) :- send(@bs169,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(170,  C) :- send(@bs170,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(171,  C) :- send(@bs171,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(172,  C) :- send(@bs172,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(173,  C) :- send(@bs173,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(174,  C) :- send(@bs174,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(175,  C) :- send(@bs175,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(176,  C) :- send(@bs176,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(177,  C) :- send(@bs177,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(178,  C) :- send(@bs178,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(179,  C) :- send(@bs179,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(180,  C) :- send(@bs180,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(181,  C) :- send(@bs181,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(182,  C) :- send(@bs182,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(183,  C) :- send(@bs183,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(184,  C) :- send(@bs184,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(185,  C) :- send(@bs185,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(186,  C) :- send(@bs186,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(187,  C) :- send(@bs187,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(188,  C) :- send(@bs188,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(189,  C) :- send(@bs189,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(190,  C) :- send(@bs190,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(191,  C) :- send(@bs191,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(192,  C) :- send(@bs192,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(193,  C) :- send(@bs193,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(194,  C) :- send(@bs194,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(195,  C) :- send(@bs195,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(196,  C) :- send(@bs196,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(197,  C) :- send(@bs197,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(198,  C) :- send(@bs198,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(199,  C) :- send(@bs199,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(200,  C) :- send(@bs200,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(201,  C) :- send(@bs201,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(202,  C) :- send(@bs202,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(203,  C) :- send(@bs203,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(204,  C) :- send(@bs204,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(205,  C) :- send(@bs205,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(206,  C) :- send(@bs206,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(207,  C) :- send(@bs207,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(208,  C) :- send(@bs208,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(209,  C) :- send(@bs209,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(210,  C) :- send(@bs210,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(211,  C) :- send(@bs211,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(212,  C) :- send(@bs212,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(213,  C) :- send(@bs213,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(214,  C) :- send(@bs214,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(215,  C) :- send(@bs215,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(216,  C) :- send(@bs216,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(217,  C) :- send(@bs217,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(218,  C) :- send(@bs218,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(219,  C) :- send(@bs219,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(220,  C) :- send(@bs220,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(221,  C) :- send(@bs221,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(222,  C) :- send(@bs222,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(223,  C) :- send(@bs223,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(224,  C) :- send(@bs224,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(225,  C) :- send(@bs225,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(226,  C) :- send(@bs226,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(227,  C) :- send(@bs227,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(228,  C) :- send(@bs228,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(229,  C) :- send(@bs229,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(230,  C) :- send(@bs230,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(231,  C) :- send(@bs231,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(232,  C) :- send(@bs232,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(233,  C) :- send(@bs233,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(234,  C) :- send(@bs234,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(235,  C) :- send(@bs235,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(236,  C) :- send(@bs236,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(237,  C) :- send(@bs237,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(238,  C) :- send(@bs238,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(239,  C) :- send(@bs239,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(240,  C) :- send(@bs240,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(241,  C) :- send(@bs241,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(242,  C) :- send(@bs242,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(243,  C) :- send(@bs243,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(244,  C) :- send(@bs244,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(245,  C) :- send(@bs245,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(246,  C) :- send(@bs246,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(247,  C) :- send(@bs247,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(248,  C) :- send(@bs248,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(249,  C) :- send(@bs249,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(250,  C) :- send(@bs250,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(251,  C) :- send(@bs251,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(252,  C) :- send(@bs252,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(253,  C) :- send(@bs253,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(254,  C) :- send(@bs254,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(255,  C) :- send(@bs255,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(256,  C) :- send(@bs256,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(257,  C) :- send(@bs257,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(258,  C) :- send(@bs258,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(259,  C) :- send(@bs259,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(260,  C) :- send(@bs260,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(261,  C) :- send(@bs261,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(262,  C) :- send(@bs262,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(263,  C) :- send(@bs263,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(264,  C) :- send(@bs264,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(265,  C) :- send(@bs265,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(266,  C) :- send(@bs266,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(267,  C) :- send(@bs267,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(268,  C) :- send(@bs268,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(269,  C) :- send(@bs269,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(270,  C) :- send(@bs270,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(271,  C) :- send(@bs271,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(272,  C) :- send(@bs272,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(273,  C) :- send(@bs273,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(274,  C) :- send(@bs274,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(275,  C) :- send(@bs275,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(276,  C) :- send(@bs276,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(277,  C) :- send(@bs277,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(278,  C) :- send(@bs278,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(279,  C) :- send(@bs279,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(280,  C) :- send(@bs280,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(281,  C) :- send(@bs281,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(282,  C) :- send(@bs282,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(283,  C) :- send(@bs283,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(284,  C) :- send(@bs284,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(285,  C) :- send(@bs285,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(286,  C) :- send(@bs286,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(287,  C) :- send(@bs287,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(288,  C) :- send(@bs288,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(289,  C) :- send(@bs289,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(290,  C) :- send(@bs290,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(291,  C) :- send(@bs291,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(292,  C) :- send(@bs292,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(293,  C) :- send(@bs293,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(294,  C) :- send(@bs294,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(295,  C) :- send(@bs295,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(296,  C) :- send(@bs296,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(297,  C) :- send(@bs297,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(298,  C) :- send(@bs298,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(299,  C) :- send(@bs299,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(300,  C) :- send(@bs300,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(301,  C) :- send(@bs301,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(302,  C) :- send(@bs302,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(303,  C) :- send(@bs303,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(304,  C) :- send(@bs304,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(305,  C) :- send(@bs305,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(306,  C) :- send(@bs306,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(307,  C) :- send(@bs307,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(308,  C) :- send(@bs308,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(309,  C) :- send(@bs309,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(310,  C) :- send(@bs310,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(311,  C) :- send(@bs311,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(312,  C) :- send(@bs312,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(313,  C) :- send(@bs313,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(314,  C) :- send(@bs314,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(315,  C) :- send(@bs315,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(316,  C) :- send(@bs316,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(317,  C) :- send(@bs317,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(318,  C) :- send(@bs318,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(319,  C) :- send(@bs319,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(320,  C) :- send(@bs320,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(321,  C) :- send(@bs321,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(322,  C) :- send(@bs322,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(323,  C) :- send(@bs323,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(324,  C) :- send(@bs324,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(325,  C) :- send(@bs325,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(326,  C) :- send(@bs326,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(327,  C) :- send(@bs327,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(328,  C) :- send(@bs328,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(329,  C) :- send(@bs329,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(330,  C) :- send(@bs330,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(331,  C) :- send(@bs331,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(332,  C) :- send(@bs332,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(333,  C) :- send(@bs333,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(334,  C) :- send(@bs334,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(335,  C) :- send(@bs335,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(336,  C) :- send(@bs336,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(337,  C) :- send(@bs337,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(338,  C) :- send(@bs338,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(339,  C) :- send(@bs339,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(340,  C) :- send(@bs340,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(341,  C) :- send(@bs341,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(342,  C) :- send(@bs342,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(343,  C) :- send(@bs343,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(344,  C) :- send(@bs344,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(345,  C) :- send(@bs345,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(346,  C) :- send(@bs346,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(347,  C) :- send(@bs347,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(348,  C) :- send(@bs348,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(349,  C) :- send(@bs349,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(350,  C) :- send(@bs350,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(351,  C) :- send(@bs351,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(352,  C) :- send(@bs352,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(353,  C) :- send(@bs353,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(354,  C) :- send(@bs354,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(355,  C) :- send(@bs355,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(356,  C) :- send(@bs356,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(357,  C) :- send(@bs357,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(358,  C) :- send(@bs358,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(359,  C) :- send(@bs359,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(360,  C) :- send(@bs360,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(361,  C) :- send(@bs361,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(362,  C) :- send(@bs362,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(363,  C) :- send(@bs363,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(364,  C) :- send(@bs364,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(365,  C) :- send(@bs365,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(366,  C) :- send(@bs366,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(367,  C) :- send(@bs367,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(368,  C) :- send(@bs368,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(369,  C) :- send(@bs369,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(370,  C) :- send(@bs370,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(371,  C) :- send(@bs371,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(372,  C) :- send(@bs372,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(373,  C) :- send(@bs373,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(374,  C) :- send(@bs374,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(375,  C) :- send(@bs375,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(376,  C) :- send(@bs376,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(377,  C) :- send(@bs377,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(378,  C) :- send(@bs378,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(379,  C) :- send(@bs379,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(380,  C) :- send(@bs380,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(381,  C) :- send(@bs381,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(382,  C) :- send(@bs382,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(383,  C) :- send(@bs383,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(384,  C) :- send(@bs384,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(385,  C) :- send(@bs385,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(386,  C) :- send(@bs386,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(387,  C) :- send(@bs387,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(388,  C) :- send(@bs388,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(389,  C) :- send(@bs389,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(390,  C) :- send(@bs390,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(391,  C) :- send(@bs391,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(392,  C) :- send(@bs392,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(393,  C) :- send(@bs393,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(394,  C) :- send(@bs394,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(395,  C) :- send(@bs395,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(396,  C) :- send(@bs396,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(397,  C) :- send(@bs397,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(398,  C) :- send(@bs398,  fill_pattern, colour(C)), gr_flush.
gr_rect_couleur(399,  C) :- send(@bs399,  fill_pattern, colour(C)), gr_flush.
% -----------------




