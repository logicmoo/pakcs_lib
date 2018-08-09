
:- dynamic([hnf/4,makeShare/2,domain/3,constructortype/7,functiontype/6,nf/4]).

% dereference a function''s argument, i.e., remove all top-level sharing structures:
derefRoot(R,V) :- var(R), !, V=R.
derefRoot(share(M),V) :- !,
	get_mutable(E,M), (E='$eval'(R) -> V=R ; derefRoot(E,V)).
derefRoot(R,R).

:- export(derefRoot/2).

% completely dereference a function''s argument, i.e., remove all sharing structures
% also inside subterms:
derefAll(R,V) :- var(R), !, V=R.
derefAll(share(M),V) :- !,
	get_mutable(E,M), (E='$eval'(R) -> derefAll(R,V) ; derefAll(E,V)).
derefAll(R,V) :- functor(R,F,N), functor(V,F,N), derefArgs(N,R,V).
derefArgs(0,_,_) :- !.
derefArgs(I,R,V) :-
	arg(I,R,RI), derefAll(RI,VI), arg(I,V,VI),
	I1 is I-1, derefArgs(I1,R,V).

:- export(derefAll/2).


