%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Curry2Prolog evaluator of main expressions.
%
% required: environment variable PAKCSHOME

:- module(evaluator,
	  [currentprogram/1, numberOfCalls/1, numberOfExits/1,
	   singlestep/0, tracemode/0, spymode/0, spypoints/1,
	   printDepth/1, printAllFailures/0,
	   profiling/1, suspendmode/1, timemode/1,
           profileCall/1, profileFail/1, profileExit/1, profileRedo/1,
	   firstCmds/1, storeFirstCmds/1,
	   evaluateGoalAndExit/1, evaluateMainExpression/3,
	   writeFailSource/1,
	   writeCurry/1, writeVar/2, writeCurryTermWithFreeVarNames/2]).

:- use_module(prologbasics).
:- use_module(basics).
:- prolog(sicstus)
   -> use_module('lib_src/prim_readshowterm'), % for showing characters
      use_module('lib_src/prim_standard')
    ; use_module('libswi/prim_readshowterm'),
      ensure_loaded(user:'libswi/prim_standard').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic numberOfCalls/1, numberOfExits/1, singlestep/0, tracemode/0,
	   spypoints/1, spymode/0, spyFail/0, printDepth/1,
	   profiling/1, profile_data/3, currentprogram/1,
	   suspendmode/1, allsolutionmode/1, timemode/1, nextIOproof/0,
	   printAllFailures/0, errorAbort/0, firstCmds/1.

currentprogram("Prelude").
numberOfCalls(0). % number of function calls
numberOfExits(0). % number of function exits (including suspensions)
%nextIOproof. % will be asserted in an alternative proof to check IO non-det.
%errorAbort. % will be asserted in case of an abort causes by error/user
singlestep. % single step mode initially on in debug mode
tracemode.  % trace mode initially on in debug mode
spypoints([]). % list of spy points
%spymode. % initially no spy points
%spyFail. % show fail ports in spy mode
printDepth(11). % maximal print depth of terms +1 (or 0 for infinity)
profiling(no). % show profiling statistics in debug mode
suspendmode(no). % yes if suspended goals should be shown
allsolutionmode(no). % yes if all solutions should be shown without asking
timemode(no).	 % yes if execution times should be shown
firstCmds([]). % first commands to be executed in main interaction loop


% store a command for processing:
storeFirstCmds(Cmd) :-
	(retract(firstCmds(_)) -> true ; true), % just to be sure
	asserta(firstCmds(Cmd)).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% evaluate a goal and exit (used in saved states):
evaluateGoalAndExit(Goal) :-
	(call(Goal) -> true ; true),
	exitCode(EC),
	halt(EC).

% evaluate an expression with a given type and a given list of free variables:
evaluateMainExpression(Exp,Type,Vs) :-
	setExitCode(0),
	prompt(_,''),		% redefine prompt for trace mode
	retract(allsolutionmode(_)), asserta(allsolutionmode(no)),
	retract(numberOfCalls(_)), retract(numberOfExits(_)),
	asserta(numberOfCalls(0)), asserta(numberOfExits(0)),
	retractAllFacts(profile_data/3),
	(retract(nextIOproof) -> true ; true), % clean fact for nextIOproof/0
	clearDynamicPreds,
	worldToken(World),
	(isIoType(Type) -> E='Prelude.apply'(Exp,World) ; E=Exp),
	(retract(hasPrintedFailure) -> true ; true),
	getRunTime(RTime1),
	getElapsedTime(ETime1),
	evalToken(Eval),
	extractMakeShareInTerm(E,MSE),
	on_exception(ErrorMsg,
	             %(suspendmode(no)
		     % -> normalizeAndCheck(MSE,V,Eval,Done), Suspended=[]
		     %  ; callAndReturnSuspensions(
		     %      user:normalizeAndCheck(MSE,V,Eval,Done),Suspended)),
	             callAndReturnSuspensions(
			  user:normalizeAndCheck(MSE,V,Eval,Done),Suspended),
		     (asserta(errorAbort),
		      ErrorMsg=debugger_abort
		       -> write('Execution aborted.'), nl, fail
		        ; setExitCode(1), printError(ErrorMsg)) ),
	getRunTime(RTime2),
	getElapsedTime(ETime2),
	% don't print failures after backtracking:
	((hasPrintedFailure ; printAllFailures) -> true
	 ; asserta(hasPrintedFailure)),
	(timemode(yes)
           -> write('Runtime: '), RTime is RTime2-RTime1, write(RTime),
	      write(' msec.'), nl,
	      write('Elapsed time: '), ETime is ETime2-ETime1, write(ETime),
	      write(' msec.'), nl
           ; true),
	numberOfCalls(NC), numberOfExits(NE),
	(NC>0 -> write('Number of function calls: '), write(NC), nl,
	         write('Number of function exits: '), write(NE), nl
               ; true),
	bindingsForNewVariables(Vs,V,NewVs),
	writeMainResult(Done,Suspended,NewVs,V),
	(Vs=[] -> true ; writeBindingsWithFreeVarNames(Suspended,Vs,NewVs)),
	(Suspended=[] -> true ; writeSuspendedGoals(Suspended)),
	flush_output,
	(var(Done)
         -> showProfileData, !, fail
         ; ((nonvar(V), V='$io'(_))
	    -> (nextIOproof
                -> retract(nextIOproof),
                   writeErr('ERROR: non-determinism in I/O actions occurred!'),
                   nlErr,
	           showProfileData,
	           !, fail
	         ; (profiling(yes) % no IO ND checking during profiling
		    -> showProfileData, !, fail
		     ; (hasPrintedFailure -> true
		        ; asserta(hasPrintedFailure)  % don't print failures
		       ),                             % during IO ND checking
		       asserta(nextIOproof), fail))
             ; allsolutionmode(no), % backtrack in allsolutionmode
	       writeMoreSolutions,
	       readMore(More),
	       \+ More = "y", % backtrack if user types wants to see more
	       showProfileData,
	       !,
	       % store command (beginning with ":") for processing:
	       (More=[58|_] -> storeFirstCmds([More]) ; true),
	       More = -1)).   % do not fail if user types end-of-file
evaluateMainExpression(_,_,_) :-
	% no further message ("no more sols") in case of abort:
	retract(errorAbort),
	!, fail.
evaluateMainExpression(_,_,_) :- % ignore proof attempt for IO ND
	retract(nextIOproof),
	showProfileData,
	!, fail.
evaluateMainExpression(_,_,_) :-
	write('No more solutions.'), nl,
	setExitCode(2),
	showProfileData,
	!, fail.

writeMainResult(Done,_,_,_) :- var(Done), !, % goal suspended
	writeErr('*** Goal suspended!'), nlErr.
writeMainResult(_,Suspended,Vs,Value) :- var(Value), !,
	write('Result: '),
	writeCurryTermWithFreeVarNames(Suspended,Vs,Value), nl.
writeMainResult(_,Suspended,Vs,'$io'(Value)) :- !,
	((nonvar(Value), Value='Prelude.()')
	 -> true
	  ; write('IO: '),
	    writeCurryTermWithFreeVarNames(Suspended,Vs,Value), nl).
writeMainResult(_,Suspended,Vs,Value) :- !,
	write('Result: '),
	writeCurryTermWithFreeVarNames(Suspended,Vs,Value), nl.
	
writeMoreSolutions :-
	pakcsrc(moresolutions,MS),
	write('More solutions? ['),
	(MS=yes -> write('Y') ; write('y')),
	write('(es)/'),
	(MS=no  -> write('N') ; write('n')),
	write('(o)/'),
	(MS=all -> write('A') ; write('a')),
	write('(ll)] '),
	flush_output.

readMore(More) :-
	readLine(Line),
	(Line = -1 -> More=Line
	            ; removeBlanks(Line,Input), processReadMore(Input,More)).

processReadMore([58|Cs],[58|Cs]) :- !. % 58=':'
processReadMore("y","y") :- !.
processReadMore("n","n") :- !.
processReadMore("a","y") :-
 	retract(allsolutionmode(_)), asserta(allsolutionmode(yes)), !.
processReadMore("","y") :- pakcsrc(moresolutions,yes), !.
processReadMore("","n") :- pakcsrc(moresolutions,no), !.
processReadMore("",More) :- pakcsrc(moresolutions,all), !,
	                    processReadMore("a",More).
processReadMore(_,More) :-
	writeMoreSolutions,
	readMore(More).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% write source of constructor matching failure:
writeFailSource(FailSrc) :-
	length(FailSrc,FTLen2),
	FTLen is FTLen2-2,
	nlErr, writeErr('FAILURE OCCURRED:'),
	(printConsFailure(file(File))
	 -> writeErr(' failure trace written to file: '), writeErr(File), nl,
	    open(File,write,Stream),
	    writeAllFailureList(Stream,FTLen,FailSrc),
	    close(Stream)
	  ; nlErr,
	    (printConsFailure(all)
	     -> writeAllFailureList(user_error,FTLen,FailSrc), nlErr
	      ; writeFailureList(user_error,FailSrc), nlErr)),
	!,
	(printConsFailure(int) -> failureInteraction(FTLen,FailSrc) ; fail).

% write last element of failure source:
writeFailureList(Stream,[FName,Args]) :- !,
	writeFailedCall(Stream,FName,Args).
writeFailureList(Stream,[_|FailSrc]) :- !,
	writeFailureList(Stream,FailSrc).
writeFailureList(Stream,Term) :- !,
	write(Stream,'ERROR: Illegal argument in writeFailureList:'), nl(Stream),
	writeCurryOnStream(Stream,Term), nl(Stream).

writeFailedCall(Stream,FName,Args) :-
	writeCurryOnStream(Stream,FName),
	write(Stream,': failed'),
	(Args=[] -> nl(Stream)
	 ; write(Stream,' for argument'),
	   (Args=[_] -> write(Stream,':') ; write(Stream,'s:')), nl(Stream),
	   map1partialM(evaluator:writeFailureArg(Stream),Args)).

writeFailureArg(Stream,Arg) :-
	write(Stream,'  '), writeCurryOnStream(Stream,Arg), nl(Stream).

writeFailCallNumber(Stream,N) :- write(Stream,N), write(Stream,': ').

failureInteraction(FTLen,FailSrc) :-
        write('(l)ist ('), write(FTLen),
	write(' calls) (s)how (f)unctions (p)rintdepth (h)elp (q)uit >'),
        get_code(C),nl,
        failureIntOption(FTLen,FailSrc,C).

failureIntOption(FTLen,FailSrc,104) :- !, % help
	skip(10),
	write('Commands in interactive mode for failure tracing:'), nl,
	write('l     - list complete trace from root to failed call'), nl,
	write('l <n> - show last <n> elements of trace'), nl,
	write('s <n> - show element with number <n> of trace'), nl,
	write('f     - show name of functions from root to failed call'), nl,
	write('p <n> - set print depth to <n> (0 = unlimited)'), nl,
	write('h     - show this message'), nl,
	write('q     - quit current failure tracing'), nl, nl,
	!, failureInteraction(FTLen,FailSrc).
failureIntOption(FTLen,FailSrc,108) :- !, % list
	readLine(LastLine),
	removeBlanks(LastLine,LastL),
	(LastL=[] -> LastArg=FTLen
	 ; (codes2number(LastL,LastArg) -> true
	    ; write('Illegal number'), nl, LastArg=0)),
	Drop is FTLen-LastArg, drop(Drop,FailSrc,LastFails),
	writeAllFailureList(user_output,LastArg,LastFails),
	!, failureInteraction(FTLen,FailSrc).
failureIntOption(FTLen,FailSrc,115) :- !, % show
	readLine(LastLine),
	removeBlanks(LastLine,LastL),
	(codes2number(LastL,GotoArg) -> true
	  ; write('Illegal number'), nl, GotoArg=1),
	Drop is FTLen-GotoArg, drop(Drop,FailSrc,[FCall|_]),
	writeFailCallNumber(user_output,GotoArg),
	writeCurry(FCall), nl,
	!, failureInteraction(FTLen,FailSrc).
failureIntOption(FTLen,FailSrc,102) :- !, % functions
	skip(10),
	writeFunctionFailureList(user_output,FTLen,FailSrc),
	failureInteraction(FTLen,FailSrc).
failureIntOption(FTLen,FailSrc,112) :- !, % printdepth
	readLine(PDLine),
	removeBlanks(PDLine,PDL),
	(codes2number(PDL,D)
	 -> retract(printDepth(_)),	
	    (D=0 -> D1=D ; D1 is D+1),
	    asserta(printDepth(D1))
	  ; write('Illegal print depth'), nl),
	write('Current printdepth: '),
	printDepth(PD), (PD=0 -> write(0) ; PD1 is PD-1, write(PD1)),
	nl,
	!, failureInteraction(FTLen,FailSrc).
failureIntOption(_,_,113) :- !, % quit
	skip(10), fail.
failureIntOption(FTLen,FailSrc,C) :-
	write('ERROR: wrong option!'), nl,
	(C=10 -> true ; skip(10)),
	failureInteraction(FTLen,FailSrc).
	
writeAllFailureList(Stream,_,[FName,Args]) :- !,
	writeFailedCall(Stream,FName,Args).
writeAllFailureList(Stream,FTLen,[FCall|FailSrc]) :- !,
	writeFailCallNumber(Stream,FTLen),
	writeCurryOnStream(Stream,FCall), nl(Stream),
	FTLen1 is FTLen-1,
	writeAllFailureList(Stream,FTLen1,FailSrc).

writeFunctionFailureList(Stream,_,[partcall(_,FName,_),Args]) :- !,
	writeFailedCall(Stream,FName,Args).
writeFunctionFailureList(Stream,FTLen,[FCall|FailSrc]) :- !,
	FCall =.. [Fun|_],
	writeFailCallNumber(Stream,FTLen),
	writeCurryOnStream(Stream,Fun), nl(Stream),
	FTLen1 is FTLen-1,
	writeFunctionFailureList(Stream,FTLen1,FailSrc).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Write suspended goals if necessary:
writeSuspendedGoals(Suspended) :-
	suspendmode(no)
	-> writeErr('*** Warning: there are suspended constraints (for details: ":set +suspend")'),
	   nlErr
	 ; write('Suspended goals (in internal representation):'), nl,
	   map1M(evaluator:tryWriteSuspGoal,Suspended).

% try to format suspended goals more nicely:
tryWriteSuspGoal(_:G) :-
	G =.. [Pred|Args],
	rev(Args,[_,_,Result|RArgs]),
	rev(RArgs,FunArgs),
	FunCall =.. [Pred|FunArgs],
	write('let '), writeCurry(Result), write(' = '),
	writeCurry(FunCall), nl,
	!.
tryWriteSuspGoal(_:G) :- !, 
	writeCurry(G), nl.
tryWriteSuspGoal(G) :- write(G), nl.


% extract all makeShare terms and replace them by the result of execute makeShare:
extractMakeShareInTerm(V,V) :- var(V), !.
extractMakeShareInTerm(makeShare(T,V),V) :- !,
	extractMakeShareInTerm(T,MST),
	makeShare(MST,V).
extractMakeShareInTerm(T,NT) :-
	T =.. [F|Args],
	map2M(evaluator:extractMakeShareInTerm,Args,NArgs),
	NT =.. [F|NArgs].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% show profile data, if desired:
showProfileData :-
	%(plprofiling(yes) -> view([user:_]) ; true),
	profiling(yes),
	currentprogram(Prog),
	append(Prog,".profile",ProgP),
	atom_codes(ProgPName,ProgP),
	tryWriteFile(ProgPName),
	!,
	tell(ProgPName),
	write('Profile data:'), nl,
	findall(pdata(F,N,Info),profile_data(F,N,Info),PD),
	sort(PD,SortedPD),
	map1M(evaluator:format_profile_entry,SortedPD),
	nl,
	told,
	append("more ",ProgP,MP),
	atom_codes(Cmd,MP),
        shellCmd(Cmd).
showProfileData.

% printing formatted profile data entries:
format_profile_entry(pdata(Func,_,port(C,F,E,R))) :-
	write(Func), write(': '),
	write('Calls='), write(C), write(' / '),
	write('Fails='), write(F), write(' / '),
	write('Exits='), write(E), write(' / '),
	write('Redos='), write(R), nl.

% predicates for storing profile information during debugging:
profileCall(P) :-
	functor(P,PF,PN),
	( retract(profile_data(PF,PN,port(C,F,E,R))) -> true
	; port(C,F,E,R)=port(0,0,0,0)),
	C1 is C+1,
	asserta(profile_data(PF,PN,port(C1,F,E,R))).

profileFail(P) :-
	functor(P,PF,PN),
	( retract(profile_data(PF,PN,port(C,F,E,R))) -> true
	; port(C,F,E,R)=port(0,0,0,0)),
	F1 is F+1,
	asserta(profile_data(PF,PN,port(C,F1,E,R))).

profileExit(P) :-
	functor(P,PF,PN),
	( retract(profile_data(PF,PN,port(C,F,E,R))) -> true
	; port(C,F,E,R)=port(0,0,0,0)),
	E1 is E+1,
	asserta(profile_data(PF,PN,port(C,F,E1,R))).

profileRedo(P) :-
	functor(P,PF,PN),
	( retract(profile_data(PF,PN,port(C,F,E,R))) -> true
	; port(C,F,E,R)=port(0,0,0,0)),
	R1 is R+1,
	asserta(profile_data(PF,PN,port(C,F,E,R1))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% write a (Prolog) term in Curry syntax:
% (used in curry2prolog for printing results or during debugging)

writeCurry(T) :- writeCurryOnStream(user_output,T).

% same as writeCurry, but write on a given stream:
writeCurryOnStream(Stream,T) :- printDepth(D), writeCurryD(Stream,D,top,T).

writeCurryD(S,D,_,_) :- D=1, !, write(S,'...').
writeCurryD(S,_,_,T) :- var(T), !, write(S,T).
writeCurryD(S,D,Nested,makeShare(T,_)) :- !, writeCurryD(S,D,Nested,T).
writeCurryD(S,D,Nested,share(M)) :- !, get_mutable(V,M),
	(V='$eval'(Exp) -> true ; Exp=V),
	writeCurryD(S,D,Nested,Exp).
writeCurryD(S,_,_,T) :- number(T), !, writeCurryLiteral(S,T).
writeCurryD(S,_,_,T) :- isCharCons(T), !, writeCurryLiteral(S,T).
writeCurryD(S,_,_,[]) :- write(S,[]), !.
writeCurryD(S,_,_,T) :- atom(T), !,
	(atom_codes(T,[95|_]) -> writeVar(user_output,T) % 95 = '_'
	 ; revTransFunctor(T,ExtName),
	   (isId(ExtName) -> write(S,ExtName)
                           ; write(S,'('), write(S,ExtName), write(S,')'))).
writeCurryD(S,D,_,T) :-
	isCompleteList(T,TL),
	!,
	(isString(TL)
	    -> % use ReadShowTerm.showTerm for showing strings:
	       show_term(TL,_,TS,[]), string2Atom(TS,TA), write(S,TA)
	     ; write(S,'['), writeCurryList(S,D,TL)).
writeCurryD(S,D,Nested,[T|Ts]) :- !,
	(Nested=nested -> write(S,'(') ; true),
	writeCurryConsList(S,D,[T|Ts]),
	(Nested=nested -> write(S,')') ; true).
writeCurryD(S,D,Nested,partcall(_,F,Args)) :- !,
	rev(Args,RArgs), Term =.. [F|RArgs], writeCurryD(S,D,Nested,Term).
writeCurryD(S,D,Nested,'Prelude.apply'(F,X)) :- !,
	D1 is D-1,
	(Nested=nested -> write(S,'(') ; true),
	writeCurryD(S,D1,nested,F), write(S,' '),
	writeCurryD(S,D1,nested,X),
	(Nested=nested -> write(S,')') ; true).
writeCurryD(S,D,Nested,'Prelude.if_then_else'(C,T,E)) :- !,
	D1 is D-1,
	(Nested=nested -> write(S,'(') ; true),
	write(S,'if '), writeCurryD(S,D1,top,C),
	write(S,' then '), writeCurryD(S,D1,top,T),
	write(S,' else '), writeCurryD(S,D1,top,E),
	(Nested=nested -> write(S,')') ; true).
writeCurryD(S,D,_,T) :-
	D1 is D-1,
	T =.. [Cons,Arg1|Args],
	isTupleCons(Cons), % tuple constructor
	!,
	write(S,'('),
	writeCurryD(S,D1,top,Arg1), writeCurryTuple(S,D1,Args),
	write(S,')').
writeCurryD(S,D,Nested,T) :-
	D1 is D-1,
	T =.. [IntCons,Arg1,Arg2],
	revTransFunctor(IntCons,Cons),
	\+ isId(Cons),		% write as an infix operator:
	!,
	(Nested=nested -> write(S,'(') ; true),
	writeCurryD(S,D1,nested,Arg1), write(S,' '),
	write(S,Cons), write(S,' '),
	writeCurryD(S,D1,nested,Arg2),
	(Nested=nested -> write(S,')') ; true).
writeCurryD(S,D,Nested,T) :-
	D1 is D-1,
	T =.. [IntCons|Args],
	revTransFunctor(IntCons,Cons),
	(Nested=nested -> write(S,'(') ; true),
	write(S,Cons),
	writeCurryArgs(S,D1,Nested,Args).

writeCurryArgs(S,_,Nested,[]) :- (Nested=nested -> write(S,')') ; true).
writeCurryArgs(S,D,Nested,[A|As]) :-
	write(S,' '),
	writeCurryD(S,D,nested,A),
	writeCurryArgs(S,D,Nested,As).

writeCurryLiteral(S,L) :- % use ReadShowTerm.showTerm for showing number/char literals
	show_term(L,_,CS,[]),
	string2Atom(CS,CA),
	write(S,CA).

writeCurryTuple(_,_,[]).
writeCurryTuple(S,D,[A|As]) :-
	write(S,','),
	writeCurryD(S,D,top,A),
	writeCurryTuple(S,D,As).

writeCurryList(_,_,[]).
writeCurryList(S,D,[_|_]) :- D=1, !, write(S,'...]').
writeCurryList(S,D,[A|As]) :-
	D1 is D-1,
	writeCurryD(S,D,top,A),
	(As=[] -> write(S,']') ; write(S,',')),
	writeCurryList(S,D1,As).

writeCurryConsList(S,D,_) :- D=1, !, write(S,'...').
writeCurryConsList(S,D,T) :-
	nonvar(T), T = [A|As], !,
	D1 is D-1,
	writeCurryD(S,D,nested,A),
	write(S,':'),
	writeCurryConsList(S,D1,As).
writeCurryConsList(S,D,T) :- writeCurryD(S,D,nested,T).


% translate a functor (operation or constructor symbol) from
% internal into external representation:
revTransFunctor(IntName,Name) :-
	constructorOrFunctionType(IntName,Name,_,_), !.
revTransFunctor(Name,Name).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% write a variable (i.e., omit first char which is always '_'):
writeVar(Str,V) :-
	atom_codes(V,[_|Cs]), atom_codes(V1,Cs),
	write(Str,V1).

% show an output substitution in a readable way:
writeBindings([]).
writeBindings([(V=B)|Bs]) :-
	nl, writeVar(user_output,V), write('='),
	% output not very helpful, should be improved
	writeCurry(B),
	writeBindings(Bs).

% instantiate unbound goal variables by the name of their variable
% in order to print the results in a nicer way:
bindFreeVars(_,[]).
bindFreeVars(Susp,[(V=B)|Bs]) :-
	((var(B), \+ occursVarInTerm(B,Susp)) -> B=V ; true),
	bindFreeVars(Susp,Bs).

% does a variable occurs in a term?
occursVarInTerm(X,T) :- var(T), !, X==T.
occursVarInTerm(X,T) :- T =.. [_|Ts], occursVarInTerms(X,Ts).
occursVarInTerms(X,[T|_]) :- occursVarInTerm(X,T), !.
occursVarInTerms(X,[_|Ts]) :- occursVarInTerms(X,Ts).

% write a Curry term where unbound variables are instantiated by
% their name given in the list of bindings (if they do not occur in any
% of the suspensions):
writeCurryTermWithFreeVarNames(Bindings,Term) :-
	writeCurryTermWithFreeVarNames([],Bindings,Term).
writeCurryTermWithFreeVarNames(Suspensions,Bindings,Term) :-
	\+ \+ (bindFreeVars(Suspensions,Bindings), writeCurry(Term)).

% write a list of variable bindings where unbound variables are instantiated by
% their name given in the list of bindings (if they do not occur in any
% of the suspensions):
writeBindingsWithFreeVarNames(Suspensions,Bindings,AllBindings) :-
	write('Bindings: '),
	\+ \+ (bindFreeVars(Suspensions,AllBindings), writeBindings(Bindings)),
	nl, !.


% Extend a list of bindings with bindings (of the form "__a")
% for all new variables occurring in a term (used to compute
% readable bindings for new variables in the computed result):
bindingsForNewVariables(Vs,Term,NewVs) :-
	bindingsForNewVariablesInTerm(Vs,(Vs,Term),NewVs),
	instantiateAllBindings(0,NewVs).

bindingsForNewVariablesInTerm(Vs,Term,NewVs) :-
	var(Term), !, addBindingForNewVariable(Vs,Term,NewVs).
bindingsForNewVariablesInTerm(Vs,Term,NewVs) :-
	Term =.. [_|Args], bindingsForNewVariablesInTerms(Vs,Args,NewVs).

bindingsForNewVariablesInTerms(Vs,[],Vs).
bindingsForNewVariablesInTerms(Vs,[T|Ts],NewVs) :-
	bindingsForNewVariablesInTerm(Vs,T,Vs1),
	bindingsForNewVariablesInTerms(Vs1,Ts,NewVs).

% add a fresh binding if the variable is not already bound:
addBindingForNewVariable([],Var,[(_=Var)]).
addBindingForNewVariable([(V=B)|Bs],Var,[(V=B)|Bs]) :-
	B==Var, !.
addBindingForNewVariable([B|Bs],Var,[B|NewBs]) :-
	addBindingForNewVariable(Bs,Var,NewBs).

% instantiate all fresh bindings:
instantiateAllBindings(_,[]).
instantiateAllBindings(N,[(B=_)|Bs]) :-
	var(B), !,
	(N<26 -> NC is 97+N, atom_codes(NA,[95,95,NC])
	       ; N0 is N-25, number_codes(N0,NS), atom_codes(NA,[95,95,97|NS])),
	B=NA,
	N1 is N+1, instantiateAllBindings(N1,Bs).
instantiateAllBindings(N,[_|Bs]) :- instantiateAllBindings(N,Bs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
