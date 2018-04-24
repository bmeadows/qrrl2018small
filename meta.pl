/*
 * Name:        meta.pl
 * Author:      Ben Meadows
 * Date:        2018-02-19
 * Description: This file contains functions that reduce a search space to the relevant space for q-RRL and provides other supporting functionality.
 */

:- dynamic goalState/1, precalculate/0.

relevantTestLiteral(Lit) :-
	relevantTestLitAction(Lit).
relevantTestLiteral(Lit) :-
	relevantTestLitAttr(Lit).
relevantTestLitAction(Lit) :-
	Lit = action(T),
	valid(Lit),
	relevantToTarg(T).
relevantTestLitAttr(Lit) :-
	Lit = attr(L),
	valid(Lit),
	L =.. [_Predicate|Args],
	Args \= [_SingleElement],
	Args \= [],
	relevantToTarg(L).
relevantToTarg(L) :-
	L =.. [_Predicate|Args],
	targetActionArgs(List),
	member(A, Args),
	member(A, List).
/*
relevantToTargetAction(L) :-
	L =.. [_Predicate, Object, _Value],
	targetActionArgs(List),
	member(Object, List).
*/

	
relevantDomainTestAlternatives(OptionList) :- 
	valid(attr(L)),
	L =.. [Predicate, Object, _Value],
	targetActionArgs(List),
	member(Object, List),
	findall(	[Predicate,Object,Val],
				(valid(attr(L1)), L1 =.. [Predicate,Object,Val]),
				ValueTriples),
	sort(ValueTriples,OptionList).
	% Note this results in doubling up, so need to reduce to a true set elsewhere
	
reduceOpsToNums(ListOfLists, Return) :-
	reduceOpsToNumbers(ListOfLists, [], Return).
reduceOpsToNumbers([], Return, Return) :- !.
reduceOpsToNumbers([H|T], Current, Return) :- 
	length(H,X),
	append([X], Current, Next),
	reduceOpsToNumbers(T, Next, Return).

multiplyOut([], Result, Result) :- !.
multiplyOut([H|T], N, Result) :-
	Next is H * N,
	multiplyOut(T, Next, Result).

getCurrentRelevantConfigurationOfAttributes(RelevantConfigList) :-
	allValidTests(TSTS), % Includes actions and fluents, too
	findall(attr(Attr),
		(currentState(attr(Attr)), member(attr(Attr), TSTS)), % Note that TSTS *already* has the attr() wrappers (amongst others)
		AttrList),
	sort(AttrList,RelevantConfigList).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 7 February 2017

applyNoiseWhereAppropriate :-
	noiseChancePercent(0),
	!.
applyNoiseWhereAppropriate :-
	useNoiseRelevantToAction(true),
	noiseChancePercent(N),
	random_between(1,100,Rand1),
	% N% chance to change fluent relevant to action
	((Rand1 =< N) -> switchActionOutcome ; true),
	!.
applyNoiseWhereAppropriate :-
	useNoiseRelevantToAction(false),
	noiseChancePercent(N),
	random_between(1,100,Rand1),
	% N% chance to change some fluent in domain
	((Rand1 =< N) -> switchAFluent ; true),
	!.
applyNoiseWhereAppropriate :- trace.

switchAFluent :-
	findall(fluent(Fl), currentState(fluent(Fl)), FList),
	random_member(F, FList),
	get_all_alternative_domain_tests(F, AltList),
	retract(currentState(F)),
	random_member(NewFluent, AltList),
	assert(currentState(NewFluent)),
	(stateConstraintsViolated -> switchAFluent ; true). % Very important - just replacing with something from get_all_alternative_domain_tests can still result in causal violation.
	% That in turn causes things like learning a constraint that incorporates a physical constraint violation, which makes filter checking hang forever!
	
switchActionOutcome :-
	allValidTests(Tests),
	findall( X, (member(X, Tests), X = fluent(_), not(currentState(X))), List), % Get all relevant fluents not currently true
	(
	(List == [])
	->
	true
	; 
	(random_member(Fluent, List), % Take one at random
	assert(currentState(Fluent)),
	get_all_alternative_domain_tests(Fluent, AltList),
	retracteachfromstate(AltList), % First attempt to obey state constraints
	% If still inconsistent, fall back on changing a RANDOM fluent - this should not be a recursive call to switchActionOutcome
	(stateConstraintsViolated -> switchAFluent ; true) % Very important - just replacing with something from get_all_alternative_domain_tests can still result in causal violation.
	% That in turn causes things like learning a constraint that incorporates a physical constraint violation, which makes filter checking hang forever!
	)),
	!.
	
retracteachfromstate([]).
retracteachfromstate([A|B]) :- retract_facts_only(currentState(A)), retracteachfromstate(B).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resetStateInPrincipledWay :-
	setObservedUnexpectedState,
	random(R1),
	random(R2),
	(R1<0.5 -> makeSingleFluentChange ; true),
	(R2<0.1 -> makeSingleFluentChange ; true),
	!,
	(stateConstraintsViolated -> resetStateInPrincipledWay ; true).
	
makeSingleFluentChange :- 
	% Select a random fluent currentState(fluent(f)), then call swapOut(fluent(f))
	findall(F, (currentState_overt_belief_only(F), F=fluent(_)), List), % Just currentState() will go wrong because of inferred beliefs not actively believed
	random_member(Fluent,List),
	swapOut(Fluent),
	!.

assertFluents([]).
assertFluents([A|B]) :-
	assert(currentState(fluent(A))),
	assertFluents(B).

assertAtts([]).
assertAtts([A|B]) :-
	assert(currentState(attr(A))),
	assertAtts(B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

precalculate :-
	statistics(process_cputime, StartCPU),
	writef('1/7. Precalculating.\n'),
	retractall(objrel(_)),
	domainGoalAction(Action),
	Action =.. [_Predicate|ArgList],
	assert(targetActionArgs(ArgList)),
	writef('2/7. Target action.\n'),
	setRandomInitialObjectConfig, % Source of the current error with this component. This randomly sets static attributes!
	writef('3/7. Randomised.\n'),
	findRelevantFluentSuperset(Action, Lits),
	writef('4/7. Superset found.\n'),
	sort(Lits,L111), print(L111), nl, nl,
	adjustForRelevance(Lits, ReturnedTests, ReturnedObjectSet), % At this point the inconsistent behaviour is already observable.
	writef('5/7. Adjusted for relevance.\n'),
	assert(allValidTests(ReturnedTests)),
	writef('6/7. Target action.\n'),
	calculateConfigSpaceSize(ReturnedObjectSet),
	writef('7/7. Calculated config size.\n'),
	retractall(objrel(_)),
	statistics(process_cputime, EndCPU),
	CPUDIFF is EndCPU - StartCPU,
	writef('CPU: '), print(CPUDIFF), nl.
	
% For each valid physical state from which the target ground action is permissible, find the relevant literals to that <physical state, action> pair.
findRelevantFluentSuperset(Action, Lits) :-
	findall(	Fluents, 
				(someDomainPhysicalStateWithApplicableAction(Action, Fluents)),
				Lits).

someDomainPhysicalStateWithApplicableAction(Action, Fluents) :-
	getTheoreticalStatePermutation(ListRepresentation), % Fluents only
	retract_facts_only(currentState(fluent(_))),
	assertFluents(ListRepresentation),
	not(stateConstraintsViolated),
	currentlyApplicableWithRelevantLits(Action, Fluents).
	
currentlyApplicableWithRelevantLits(Action, Fluents) :-
	% 1. Check it's applicable - part of the current error is that this also checks statics, so is dependent on the original randomisation of the static attributes
	validAction(Action),
	% 2. Return all fluent literals relevant to the physical config / action combo
	getRelevantFluentLits(Fluents).

adjustForRelevance(Lits, ReturnedTests, ReturnedObjectSet) :-
	sort(Lits,LitsA),
	flatten(LitsA,Lits2),
	%% Note the list_to_set/2 predicate would be more expensive than sort/2 because it involves two sorts and a linear scan.
	sort(Lits2,Lits3),
	addAllObjectAttributesAndActions(Lits3, ReturnedTests, ReturnedObjectSet).
	
% Returns fluents relevant to the action, based on current physical state
% Also asserts names of relevant objects in "objrel(X)".
getRelevantFluentLits(Set2) :-
	targetActionArgs(Set1), % 1. For the set1 of objects named in the action predicate;
	findall( 	F,
				(currentState(F), F=fluent(Content), Content =.. [_Pred|ArgList], length(ArgList, N), N>0, last(ArgList, Last), select(Last, ArgList, Remnant), allAreIn(Remnant, Set1) ),
				Set2
			), % 2. Find the set2 of all currently-true-in-state fluents such that the fluent's arguments are all in set1;
	findall( 	LastConstant,
				(member(fluent(Content),Set2), Content =.. [_Predicate|ArgList], last(ArgList, LastConstant) ),
				Set3
			), % 3. Let set3 be the set of constants mapped onto by set2;
	assertEachAsObjRel(Set3).
	
assertEachAsObjRel([]) :- !.
assertEachAsObjRel([A|B]) :-
	objrel(A),
	!,
	assertEachAsObjRel(B).
assertEachAsObjRel([A|B]) :-
	assert(objrel(A)),
	!,
	assertEachAsObjRel(B).

allAreIn(SetA, SetB) :-
	not( (member(X, SetA), not(member(X, SetB))) ).

addAllObjectAttributesAndActions(Initial, Return, ReturnedObjectSet) :-	
	findall(X, objrel(X), ListObs),
	targetActionArgs(AAs),
	append(ListObs, AAs, NewList), % Add the arguments of the target action
	sort(NewList, ReturnedObjectSet),
	addAllRelevantActions(ReturnedObjectSet, Initial, NewSet),
	% At this point, just need to find valid BDT tests, so find all relevant instantiated object properties
	% (While allValidTests(Tests) is also used to find meaningfully different obj configs, no changes are necessary for that)
	% Props1 should contain all valid instantiated multi-argument object properties whose n-1 arguments are all in ReturnedObjectSet
	findall(	attr(ObjectProperty),
				( valid(attr(ObjectProperty)), ObjectProperty =.. [_Pred|Args], length(Args, N), N>1, last(Args, Last), select(Last, Args, Remnant), allAreIn(Remnant, ReturnedObjectSet) ),
				Props1),
	sort(Props1,Props2),
	append(Props2, NewSet, Return).

addAllRelevantActions(ObjectSet, Initial, Final) :-
	findall( action(Act),
			(	
				valid(action(Act)),
				Act =.. [_Pred|Args],
				not( (member(Arg,Args), not(member(Arg,ObjectSet))) ) % No arg is outside the set of relevant domain objects
			),
			RelActions ),
	% This list kept separately, so it can be used not only to determine tests in the BDT, but also prevent the learner ever trying actions outside it
	assert(usableActionList(RelActions)),
	append(Initial,RelActions,Final).


% 1. Have determined which objects are relevant, 'ObjectSet'.
% 2. All properties of a relevant object are relevant.
% 3. For each property that CAN pertain to an object, it is guaranteed that the function will give SOME value.
% 4. Can call valid(attr(X)) and check number of arguments is >1.
% 5. So, for each predicate, count predicate([unique instantiated arguments all in ObjectSet], final argument) and then multiply out for all predicates

singleMapping(ObjectSet, Pred, Remnant, Number) :-
	valid(attr(ObjectProperty)),
	ObjectProperty =.. [Pred|Args],
	length(Args, N), N>1,
	last(Args, Last), select(Last, Args, Remnant),
	allAreIn(Remnant, ObjectSet),
	findall(	Last2,
				(valid(attr(ObjectProperty2)), ObjectProperty2 =.. [Pred|Args2], length(Args2, N2), N2>1, last(Args2, Last2), select(Last2, Args2, Remnant)),
				SetOfThingsMappedOnto),
	sort(SetOfThingsMappedOnto,SetOfThingsMappedOnto2),
	length(SetOfThingsMappedOnto2, Number).

% Speculation on alternative approach:
% Start with all the attr()s from the found set. Partition them into new lists where the whole literal except the last argument are the same in each.
% If necessary (check), remove the ones where some arg in [all but the last argument] isn't in the ObjectSet.
	
calculateConfigSpaceSize(ObjectSet) :-
	findall([Pred, Remn, Number], singleMapping(ObjectSet, Pred, Remn, Number), Collects),
	sort(Collects, Numbers),
	findall(Number, member([_,_,Number],Numbers), Final),
	list_multiply(Final, ResultTotal),
	assert(num_possible_attribute_configs(ResultTotal)).
	
list_multiply([], 0) :- !.
list_multiply(List, ResultTotal) :-
	multiply_out_list(List, 1, ResultTotal).
multiply_out_list([], Final, Final).
multiply_out_list([A|B], Working, Final) :-
	New is A * Working,
	multiply_out_list(B, New, Final).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


establishGoalState :-
	learning_type(positive_affordance_learning),
	!,
	executabilityConditionViolated(ID),
	domainGoalAction(ACTION),
	clause(impossible_if(ACTION, ID),_),
	Element = requiredToCheckBeforeTransition(ACTION, ID),
	establishGS([ Element ]).
establishGoalState :-
	% Not positive_affordance_learning
	establishGS([]).
	
establishGS(List) :-
	domainGoalAction(ACT),
	% Unexpected observations
	unexpectedResult(X),
	append(X, [lastActionWas(ACT)], List1),
	append(List1, List, List2),
	assert(goalState(List2)).

:- establishGoalState.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

validAction(A) :- actionDescription(A, Vars, Sorts), sortsHoldTrue(Sorts, Vars), not(impossible_if(A,_ID)).

validOrPotentialAction(A) :- validAction(A).
% May not be valid, but currently doing positive affordance learning and only the target executability condition is preventing the action being applicable.
validOrPotentialAction(A) :-
	learning_type(positive_affordance_learning),
	actionDescription(A, Vars, Sorts),
	sortsHoldTrue(Sorts, Vars),
	executabilityConditionViolated(TargetID),
	not(( impossible_if(A,OtherID), OtherID \= TargetID )).

sortsHoldTrue([], []).
sortsHoldTrue([Sort|Tail1], [Var|Tail2]) :-
	functor(Term, Sort, 1),
	arg(1, Term, Var),
	domain(sort(Term)),
	sortsHoldTrue(Tail1, Tail2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Removes only fluents that match the pattern and are not clauses.
% This is necessary because e.g. retractall(currentState(foo(1,2))) will retract RULES like 'currentState(foo(A,B)) :- currentState(bar(A,B)), valid(sort(A)).'
% These are needed if a domain has derived fluents, e.g., location dynamically inferred from being inside a container that is at a location.
retract_facts_only(Pattern) :-
	clause(Pattern, true), % Establishes we only match against facts
	retract(Pattern), % Only retract exactly that fact
	fail. % Failure-driven loop means 'Pattern' term doesn't get variables set, unlike in recursive loop
retract_facts_only(_) :- !.

currentState_overt_belief_only(X) :-
	clause(currentState(X), true), % There is a 'belief that X' in the fact store.
	not(( clause(currentState(X), Tail), Tail \= true, Tail )).  % 'belief that X' cannot be derived from other terms.
	% The reason BOTH checks are made is to help in cases where the system inappropriately findalls currentState beliefs and then later 'reinstates' them, turning a derived belief into a reified belief.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	
:- domain_relevance(X), call(X).


