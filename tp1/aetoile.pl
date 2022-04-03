%*******************************************************************************
%                                    AETOILE
%*******************************************************************************

/*
Rappels sur l'algorithme
 
- structures de donnees principales = 2 ensembles : P (etat pendants) et Q (etats clos)
- P est dedouble en 2 arbres binaires de recherche equilibres (AVL) : Pf et Pu
 
   Pf est l'ensemble des etats pendants (pending states), ordonnes selon
   f croissante (h croissante en cas d'egalite de f). Il permet de trouver
   rapidement le prochain etat a developper (celui qui a f(U) minimum).
   
   Pu est le meme ensemble mais ordonne lexicographiquement (selon la donnee de
   l'etat). Il permet de retrouver facilement n'importe quel etat pendant

   On gere les 2 ensembles de fa�on synchronisee : chaque fois qu'on modifie
   (ajout ou retrait d'un etat dans Pf) on fait la meme chose dans Pu.

   Q est l'ensemble des etats deja developpes. Comme Pu, il permet de retrouver
   facilement un etat par la donnee de sa situation.
   Q est modelise par un seul arbre binaire de recherche equilibre.

Predicat principal de l'algorithme :

   aetoile(Pf,Pu,Q)

   - reussit si Pf est vide ou bien contient un etat minimum terminal
   - sinon on prend un etat minimum U, on genere chaque successeur S et les valeurs g(S) et h(S)
	 et pour chacun
		si S appartient a Q, on l'oublie
		si S appartient a Ps (etat deja rencontre), on compare
			g(S)+h(S) avec la valeur deja calculee pour f(S)
			si g(S)+h(S) < f(S) on reclasse S dans Pf avec les nouvelles valeurs
				g et f 
			sinon on ne touche pas a Pf
		si S est entierement nouveau on l'insere dans Pf et dans Ps
	- appelle recursivement etoile avec les nouvelles valeurs NewPF, NewPs, NewQs

*/

%*******************************************************************************

:- ['avl.pl'].       % predicats pour gerer des arbres bin. de recherche   
:- ['taquin.pl'].    % predicats definissant le systeme a etudier

%*******************************************************************************

main :-
	% situation de départ
	initial_state(S0),
	% calcul des valeurs F0, H0, G0
	heuristique(S0, H0),
	G0 is 0,
	F0 is G0 + H0,
	% initialisations Pf, Pu et Q 
	empty(Pf),
	empty(Pu),
	empty(Q),
	% insertion of initial nodes dan Pf, Pu
	insert([[F0,H0,G0], S0], Pf, Pf1),
	insert([S0, [F0,H0,G0], nil, nil], Pu, Pu1),
	put_flat(Pf1),
	put_flat(Pu1),
	put_flat(Q),
	% lancement de Aetoile
	aetoile(Pf1, Pu1, Q).

%*******************************************************************************

%Affiche solution pfait un print de chaque action de la solution
affiche_solution([_,[_,_,G],Pere, A], Qs) :-
	belongs([Pere,V,P,A], Qs), 
	affiche_actions([Pere,V,P,A], Qs),
	write_state(Pere),
	writeln('--'),
	writeln('Action :'),
	writeln(A),
	writeln('-->').
	
affiche_solution([_,_,nil, nil]) :- writeln('SOLUTION OBTENUE').



%Traite un noeud et produit une liste de toutes les possibilitées %
expand(Pu,L):-
	final_state(Fini),
	Pu = [U,[F,H,G],Pere,A],
	heuristique2(U,Fini,Hs),
	Gs is RC + G,
	Fs is Hs +Gs,
	findall([S,[Fs,Hs,Gs],U,ACT],rule(ACT,RC,U,S), L).


%Traite les états successeurs

% En fonction des successeur rencontrés on traite le sucesseur différements
%Condition d'arrêt
loop_successors([],Pf,Pu,Q,Pf1,Pu1).

%Si S est connu dans Q alors on oubliés cet état
loop_successors([Head|Tail],Pf,Pu,Q,Pf1,Pu1):-
	belongs([S,_,_,_], Q), 
	nl,writeln('S a déjà été développé :'),
	write_state(S),
	loop_successors(Tail, Pf, Pfn, Pu, Pun, Q).

%si S est connu dans Pu alors garder le terme associé à la meilleure évaluation
%S'il y a une evaluation superiur a le heuristique déjà dévélopé
loop_successors([[S,Val,_,_]|Tail], Pf, Pfn, Pu, Pun, Q) :-
	\+belongs([S,_,_,_], Q),
	belongs([S,Val1,_,_], Pu),
	nl,writeln("S  est connu dans Pu State:"),
	write_state(S),
	nl,writeln("Val: y a une evaluation inferieur a le heuristique déjà dévélopé"),
	write_state(Val),
	nl,writeln("Val1:"),
	write_state(Val1),
	( compare_values(Val, Val1) ->
	suppress_min([Val1, S], Pf, Pf1),
	suppress([S, Val1, _, _], Pu, Pu1), 
	insert([Val, S], Pf1, Pf2),
	insert([S, Val, _, _], Pu1, Pu2),	
	loop_successor(Tail, Pf2, Pfn, Pu2, Pun, Q1)
	;
	loop_successor(Tail, Pf, Pfn, Pu, Pun, Q1)
	).

%S est une situation nouvelle
loop_successors([[S, [F, H, G], Pere, Action]|Tail], Pf, Pfn, Pu, Pun, Q) :-
	\+belongs([S,_,_,_], Q),
	\+belongs([S,_,_,_], Pu),
	insert([[F,H,G], S], Pf, Pf1),
	insert([S, [F, H, G], Pere, Action], Pu, Pu1),
	writeln("insertion of new S"),
	writeln("State:"),
	write_state(S),
	nl, writeln("Pf1"),
	put_flat(Pf1),
	nl, writeln("Pu1"),
	put_flat(Pu1),
	nl, writeln("Q"),
	put_flat(Q),
	loop_successors(Tail, Pf1, Pfn, Pu1, Pun, Q).

compare_values(Val, Val1) :- 
	Val @< Val1.

affiche_solution([U,[F,H,G],nil, nil]) :- writeln("solution trouve: ").

aetoile(nil,nil,_):-
	write('Pas de solution').

aetoile(Pf, Pu, Q) :- 
	suppress_min([Val, Fin], Pf, _),
	suppress([Fin, Val, Pere, Action], Pu, _), 
	final_state(Fin),
	writeln("Fin state trouve: "),
	!,
	affiche_solution([Fin, Val, Pere, Action], Q).

aetoile(Pf, Pu, Q) :-
	suppress_min([Val, U], Pf, Pf1),
	suppress([U, Val, Parent, Move], Pu, Pu1), 
	expand([U, Val, _, _],L),
	nl,writeln("List Succeseurs"),
	write_state(L),
	loop_successors(L, Pf1, Pfn, Pu1, Pun, Q),
	insert([U, Val, Parent, Move], Q, Qn),
	nl,writeln("insertion of State en Q"),
	nl,writeln("Pfn"),
	put_flat(Pfn),
	nl,writeln("Pun"),
	put_flat(Pun),
	put_flat(Qn),
	aetoile(Pfn, Pun, Qn).


%--------------------------------------------------------------------------------
%----------------------ZONE DE TEST----------------------------------------------
%--------------------------------------------------------------------------------

%
test1 :-
	% situation de départ
	initial_state(S0),
	% calcul des valeurs F0, H0, G0
	heuristique(S0, H0),
	G0 is 0,
	F0 is G0 + H0,
	% initialisations Pf, Pu et Q 
	empty(Pf),
	empty(Pu),
	empty(Q),
	% insertion some states
	insert([[F0,H0,G0], S0], Pf, Pf1),
	insert([S0, [F0,H0,G0], nil, nil], Pu, Pu1),
	suppress_min([Val, U], Pf1, Pf2),
	suppress([U, Val, _, _], Pu1, Pu2), 
	expand([U, Val, _, _],L),
	writeln('List Succeseurs'),
	write_state(L),
	insert([U, Val, Pere, Action], Q, Qs),
	loop_successors(L, Pf2, Pfn, Pu2, Pun, Qs),	
	writeln("Pfn"),
	put_flat(Pfn),
	writeln("Pun"),
	put_flat(Pun),
	writeln("Qs"),
	put_flat(Qs).
	
%	
test2 :-
	empty(Pf),
	empty(Pu),
	empty(Q),
	initial_state(S0),
	insert([_, S0], Pf, Pf1),
	insert([S0, _, nil, nil], Pu, Pu1),
	insert([S0, _, _, _], Q, Qs),
	loop_successors([[S0, _, nil, nil]], Pf2, Pfn, Pu2, Pun, Qs).


	
   