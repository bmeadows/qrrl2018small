/*
 * Name:        qRRL.pl
 * Author:      Ben Meadows
 * Date:        2018-02-19
 * Description: This file implements learning and support functionality for q-RRL.
 */

:- dynamic register/3, struc/3, currentState/1, goalState/1, goalreached/0, episode_count/1, initial_num_of_episodes/1, currentNodeId/1, root/1, leaf/1, parent/2, child_y/2, 
child_n/2, test/2, predicted_q_value/3, leaf_stored_example/6, split_count/1, no_split_count/1, num_possible_attribute_configs/1, end_by_goal_counter/1, end_by_domain_counter/1, clause1count/1, 
clause2count/1, clause3count/1, finaloutputfile/1, output_file_counter/1, output_file_error/1, output_file_time/1, domain_specified_end/0, qValueLearned/5, leaf_generalised/2, data_output_file/1, 
total_config_count/1, learned_config/1, episode_high_val/3, examplepathrecord/5, semifinalexample/6, finalexample/5, candidate_axiom/7, final_axiom/7, lastActionWas/1, last_split_at/1, 
sumQCollector/1, countCollector/1, random_sampling_count/1, number_of_random_sample_draws_to_make/1, affectedLeavesThisConfig/1, number_of_configs_to_search/1, noiseChancePercent/1.

:- discontiguous permitted_domain_test_alternatives/2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Contents
% 1. Dependencies
% 2. Counters
% 3. Structural predicates
% 4. Running the system
% 5. Output functions
% 6. Debug registers
% 7. Q-RRL functionality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% 1. Dependencies %%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- [parameters,rrl_domain,meta,data_collector].
% The learning methods implemented in this file make use of four additional components.
% 'parameters.pl' establishes the parameters for q-RRL used by the architecture.
% 'rrl_domain.pl' encodes a domain intended for learning with q-RRL.
% 'meta.pl' initialises some domain information using functions that reduce a search space to the relevant space for q-RRL.
% 'data_collector.pl' collects and collates information on false positives from batch testing trials.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% 2. Counters %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

split_count(0).
no_split_count(0).
end_by_domain_counter(0).
end_by_goal_counter(0).
currentNodeId(0).
clause1count(0).
clause2count(0).
clause3count(0).
random_sampling_count(0).
total_config_count(0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% 3. Structural predicates %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%% BDT predicates:
% root(ID).
% child_y(ID, ID1).
% child_n(ID, ID2).
% parent(Child, Parent).
% leaf(ID).
% test(ID, Test). % : Test in {action(Action), fluent(Contents), attr(Value)}
% predicted_q_value(Leaf_ID, Value, Actioncount).
% leaf_stored_example(Leaf_ID, StateActionPairRemainder, Count, SumOfQ, SumOfSquaredQ, ConfigurationID).

%%%%%% Configuration predicates:
% learned_config(SortedAttributeList).
% num_possible_attribute_configs(N).
% total_config_count(N).
% percent_of_object_config_space_to_search(N).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% 4. Running the system %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Run a set of trials using default output files.
runbatchdefault :-
	runbatch('count.txt', 'error.txt', 'time.txt').

% Run a set of trials, specifying output files.
runbatch(CounterFile, ErrorFile, TimeFile) :-
	open('batch_out.txt', write, OStream), % Default
	set_output(OStream),
	assert(finaloutputfile('default_batch_final_out.txt')),
	assert(output_file_counter(CounterFile)),
	assert(output_file_error(ErrorFile)),
	assert(output_file_time(TimeFile)),
	enter_register(overall),
	begin_rl,
	batch_collect_data,
	exit_register(overall),
	printRegisters(OStream),
	close(OStream).

% Perform a single run of Q-RRL, reporting the final results in File specified.
run(File) :-
	(send_terminal_output_to(FileName)
	->
	(open(FileName, write, OStream), % Default
	set_output(OStream))
	; true),
	assert(finaloutputfile(File)),
	enter_register(overall),
	begin_rl,
	exit_register(overall),
	printRegisters(OStream),
	(send_terminal_output_to(FileName)
	->
	close(OStream)
	; true).

% Begin one run of Q-RRL.
begin_rl :-
	init_random,
	makeNewRoot,
	setSpaceSize,
	set_learning_rate,
	enter_register(b1), % Register used for batch tests. Do not change.
	learnAllObjectConfigurations.

% Setting the random seed, if applicable.
init_random :- random_seed(false), !.
init_random :- random_seed(N), set_random(seed(N)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% 5. Output functions %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Main functions for printing text and numbers to output. All text is printed in verbose mode, some in quiet mode, and only critical information in silent mode.
print_major(Text) :- permit_write(Text). % silent / quiet / verbose
print_medium(Text) :- not(feedback(silent)), !, permit_write(Text).
print_medium(_) :- !. % silent
print_minor(Text) :- feedback(verbose), !, permit_write(Text).
print_minor(_) :- !. % silent / quiet
%
println_major(Text) :- print_major(Text), nl_major.
println_medium(Text) :- print_medium(Text), nl_medium.
println_minor(Text) :- print_minor(Text), nl_minor.
%
nl_major :- print_major('\n').
nl_medium :- print_medium('\n').
nl_minor :- print_minor('\n').
%
permit_write(Text) :- number(Text), !, print(Text).
permit_write(Text) :- writef(Text).

% Print list of (up to 100) axioms by rank
printAxiomListRecursive(Rank) :-
	Rank > 99,
	!.
printAxiomListRecursive(Rank) :-
	not(final_axiom(Rank, _Act, _YN, _QMean, _Worst, _Count, _Configs)),
	!.
printAxiomListRecursive(Rank) :-
	final_axiom(Rank, Act, [Yes,No], QMean, Worst, Count, Configs),
	print(Rank), writef('. '),
	((Rank < 10) -> writef(' ') ; true), % Zero pad
	pretty_print_axiom(Act, Yes, No),
	writef('  [mean '), print(QMean), writef('], [worst '), print(Worst), writef('], [support: '), print(Count), writef(' examples, '), print(Configs), writef(' configs]'), nl,
	NewRank is Rank + 1,
	printAxiomListRecursive(NewRank).

printLiftedAxiomListRecursive(Rank) :-
	Rank > 99,
	!.
printLiftedAxiomListRecursive(Rank) :-
	not(final_lifted_axiom(Rank, _String)),
	!.
printLiftedAxiomListRecursive(Rank) :-
	final_lifted_axiom(Rank, String),
	print_major(Rank), print_major('.     '), println_major(String),
	NewRank is Rank + 1,
	printLiftedAxiomListRecursive(NewRank).

% Print a single axiom without a newline character
pretty_print_axiom(Act, Yes, No) :-
	print(Act),
	writef(' :- '),
	writeAllInList(Yes),
	writeAllInListNegated(No),
	writef('end.').

% Print contents of a Prolog list
writeAllInList([]) :- !.
writeAllInList([A|B]) :-
	!,
	print(A), writef(', '), writeAllInList(B).
writeAllInListNegated([]) :- !.
writeAllInListNegated([A|B]) :-
	!,
	writef('not('), print(A), writef('), '), writeAllInListNegated(B).

% Print entire content of current state being examined. Mainly for debugging.
printState :-
	currentState(X),
	print_minor(' - '), println_minor(X),
	fail.
printState.

% Print the first episode of the first configuration, and every thousandth episode of every fifth configuration thereafter.
printTableAtEndOfEpisode :-
	episode_count(N),
	 (0 is N mod 1000),
	total_config_count(Y),
	(0 is Y mod 5),
	!,
	println_minor('End of an episode, learned values:\n'),
	printLearnedBDT.
printTableAtEndOfEpisode :- !.

% Print the BDT (structure, tests, predicted QValues, and supporting information).
printLearnedBDT :-
	root(ID),
	printTree(ID, '*', 0),
	println_minor('Final counts:'),
	split_count(S),
	no_split_count(NS),
	print_minor('Number of times a leaf node split: '), println_minor(S),
	print_minor('Number of times no node was split: '), println_minor(NS),
	end_by_domain_counter(ED),
	end_by_goal_counter(EG),
	print_minor('Number of episode-sequences ended by domain: '), println_minor(ED),
	print_minor('Number of episode-sequences ended by goal success: '), println_minor(EG),
	clause1count(C1), clause2count(C2), clause3count(C3),
	print_minor('Node splits by clauses: #1: '), print_minor(C1), 
	print_minor('; #2: '), print_minor(C2),
	print_minor('; #3: '), println_minor(C3),
	total_config_count(NumStats), print_minor('* Total obj attr configs tried so far: '), print_minor(NumStats), println_minor(' !'),
	data_output_file(DOF),
	atom_concat(DOF, '.txt', Dat),
	println_minor(Dat).

% Print the BDT structure.
printTree(ID, Symbol, Count) :-
	leaf(ID),
	!,
	printTreeSpaces(Count),
	print_minor(Symbol), print_minor(' #'), print_minor(ID), print_minor('  '),
	( test(ID, T) -> (print_minor(T), print_minor('?')) ; print_minor('[no test]') ),
	!,
	print_minor(' : [value '),
	predicted_q_value(ID, Val, CountOfActions),
	print_minor(Val),
	print_minor('], [count '),
	print_minor(CountOfActions),
	print_minor('], [variance '),
	calculateVarianceAtLeafNode(ID, Variance),
	print_minor(Variance),
	print_minor('], configs '),
	findall(CID, leaf_stored_example(ID, _, _, _, _, CID), CIDLIST),
	sort(CIDLIST, Configs),
	println_minor(Configs).

printTree(ID, Symbol, Count) :-
	child_y(ID, IDY),
	child_n(ID, IDN),
	printTreeSpaces(Count),
	print_minor(Symbol), print_minor(' #'), print_minor(ID), print_minor('  '),
	( test(ID, T) -> (print_minor(T), print_minor('?')) ; print_minor('[no test]') ),
	!,
	nl_minor,
	New is Count + 1,
	printTree(IDY, 'Y', New),
	printTree(IDN, 'N', New).
	
printLeafExamples(ID, Count) :-
	printTreeSpaces(Count),
	printTreeSpaces(1),
	leaf_stored_example(ID, Remainder, ExCount, _SumOfQ, _SumOfSquaredQ, Confs),
	print_minor('{'),
	print_minor(Remainder),
	print_minor('} : '),
	print_minor(ExCount),
	print_minor(' examples. '),
	print_minor('<'),
	print_minor(Confs),
	print_minor('>\n'),
	fail.
printLeafExamples(_, _) :- !.

printTreeSpaces(0) :- !.
printTreeSpaces(Count) :-
	!,
	print_minor('  '),
	N is Count - 1,
	printTreeSpaces(N).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% 6. Debug registers %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% registers(true) implies CPU time data will be collected for major components of the Q-RRL process.

enter_register(_) :- registers(false).

enter_register(FunctionName) :-
	registers(true),
	statistics(process_cputime, Current),
	not(register(FunctionName, _, _)),
	assert(register(FunctionName, 0, Current)),
	!.

enter_register(FunctionName) :-
	registers(true),
	statistics(process_cputime, Current),
	register(FunctionName, TotalStored, X),
	retract(register(FunctionName, TotalStored, X)),
	assert(register(FunctionName, TotalStored, Current)).

exit_register(_) :- registers(false).

exit_register(FunctionName) :-
	registers(true),
	statistics(process_cputime, Current),
	register(FunctionName, TotalStored, Start),
	Diff is Current - Start,
	NewStored is TotalStored + Diff,
	retract(register(FunctionName, TotalStored, Start)),
	assert(register(FunctionName, NewStored, -1)).

printRegisters(_) :- registers(false).

printRegisters(Stream) :-
	registers(true),
	format(Stream, '~t ~w ~t~70|~n', ['Time reporting (registers):']),
	reportOnRegisters(Stream).

reportOnRegisters(_Stream) :-
	registers(true),
	not(register(_, _, _)),
	!.
reportOnRegisters(Stream) :-
	registers(true),
	register(FName, Stored, _),
	retractall(register(FName, Stored, _)),
	format(Stream, '~w ~t~40|: ~5f CPU seconds ~n', [FName, Stored]),
	!,
	reportOnRegisters(Stream).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% 7. Q-RRL functionality %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Explore a percentage of the space of relevant configurations, including at least some specified minimum and up to the total number of configurations
setSpaceSize :-
	num_possible_attribute_configs(A),
	percent_of_object_config_space_to_search(P),
	minimum_number_of_searches(Min),
	B is A-1, % Necessary
	N is (B*P)/100,
	(N > Min -> X = N ; X = Min),
	(B < X -> Y = B ; Y = X),
	assert(number_of_configs_to_search(Y)).

set_learning_rate :-
	initial_learning_rate_parameter(N),
	assert(current_learning_rate_parameter(N)).

learnAllObjectConfigurations :-
	number_of_configs_to_search(N),
	total_config_count(C),
	C >= N,
	!,
	println_medium('Necessary number of attribute configurations learned. Beginning generalisation step.\n'),

	exit_register(b1), % Register used for batch tests. Do not change.

	enter_register(g1), % Register used for batch tests. Do not change.
	(doGeneralisation -> true ; true),
	exit_register(g1), % Register used for batch tests. Do not change.
	batchReportingTime.

learnAllObjectConfigurations :-
	Str = 'data-', get_time(TimeStamp), atom_concat(Str, TimeStamp, Stream), 
	retractall(data_output_file(_)),
	assert(data_output_file(Stream)),
	setRandomInitialObjectConfig,
	makePrincipledChangeToObjectConfig,
	doAllEpisodes,
	!,
	learnAllObjectConfigurations.
	
doAllEpisodes :-
	retractall(episode_count(_)),
	assert(episode_count(0)),
	retractall(episode_high_val(_,_,_)),
	retractall(affectedLeavesThisConfig(_List)),
	assert(affectedLeavesThisConfig([])),
	performEpisodesForOneConfig.

% Q-values have converged for this configuration
performEpisodesForOneConfig :-
	episode_count(N),
	convergence_buffer(B),
	N > B,
	enter_register(check_q_value_convergence),
	q_values_have_converged,
	exit_register(check_q_value_convergence),
	!,
	print_minor('q converged, episode #'),
	println_minor(N),
	checkForObjectConfigChangeCycle.
	
% Q-values have failed to converge
performEpisodesForOneConfig :-
	episode_count(N),
	maximum_episode_limit(M),
	N > M,
	!,
	print_minor('q did not converge: over episode limit of '), println_minor(N),
	checkForObjectConfigChangeCycle.

% Q-values have not yet converged (continue)
performEpisodesForOneConfig :-
	episode_count(X),
	!,
	retractall(goalreached),
	retractall(lastActionWas(_)),
	resetStateInPrincipledWay,
	enter_register(ep_steps),
	do_steps_in_episode, % Main function
	exit_register(ep_steps),
	
	enter_register(tree_split_checking),

	total_config_count(NConfig),
	number_of_configs_to_search(CTSe),
	Frac is NConfig/CTSe,
	exploration_before_splitting(Exp),
	(Frac >= Exp -> checkForTreeSplits ; true), % Tree splitting is delayed if exploration_before_splitting is set higher than zero
	
	exit_register(tree_split_checking),
	
	printTableAtEndOfEpisode,
	!,
	Y is X+1,
	retractall(episode_count(X)),
	assert(episode_count(Y)),
	updateLearningValue,
	storeEpisodicValueMaxes,
	performEpisodesForOneConfig.
	
checkForObjectConfigChangeCycle :-
	markCurrentConfigurationLearned,
	checkObjectConfigChange.

checkObjectConfigChange :-
	number_of_configs_to_search(N),
	total_config_count(C),
	C >= N,
	!. % Finished episodes; return to recursive call
checkObjectConfigChange :-
	makePrincipledChangeToObjAttConfiguration,
	!,
	total_config_count(A), B is A +1,
	retractall(total_config_count(_)),
	assert(total_config_count(B)),
	doAllEpisodes.

% Check for duplicate configurations
makePrincipledChangeToObjectConfig :-
	checkForFurtherChangeToConfig.
	
makePrincipledChangeToObjAttConfiguration :-
	enter_register(change_obj_att_configs),
	setRandomInitialObjectConfig,
	randomWalkCurrentObjectConfig,
	exit_register(change_obj_att_configs),
	!,
	checkForFurtherChangeToConfig.

checkForFurtherChangeToConfig :-
	getCurrentRelevantConfigurationOfAttributes(X),
	learned_config(X),
	!,
	makePrincipledChangeToObjAttConfiguration.
checkForFurtherChangeToConfig :-
	getCurrentRelevantConfigurationOfAttributes(X),
	not(learned_config(X)),
	!.

% Catch case
randomWalkCurrentObjectConfig :-
	num_possible_attribute_configs(Limit),
	total_config_count(N),
	N >= Limit,
	trace,
	println_major('Error in randomWalkCurrentObjectConfig'), nl,
	!.

randomWalkCurrentObjectConfig :-
	getCurrentRelevantConfigurationOfAttributes(List),
	domainChangeObjectAtts(List).
	
markCurrentConfigurationLearned :- 
	getCurrentRelevantConfigurationOfAttributes(List),
	assert(learned_config(List)).
	
% Assume "convergence" means the best learned value in the MDP doesn't change by more than 1% for a set number of episodes in a row.
q_values_have_converged :-
	episode_count(EC),
	last_split_at(Episode),
	convergence_buffer(Buffer),
	Diff is EC - Episode,
	Diff > Buffer, % Splitting any node effectively resets the convergence counter
	findall( LeafID,
				(episode_high_val(EC, LeafID, HighVal),
				not(( episode_high_val(EC, _, HigherVal), HigherVal > HighVal ))),
				LeafIDs),
	!,
	check_last_X_differences(EC, LeafIDs, Buffer).

check_last_X_differences(_, _, 0) :- !.
check_last_X_differences(N, LeafIDs, M) :-
	C1 is N-M-1,
	C2 is N-M,
	checkDiffsForAllLeaves(C1, C2, LeafIDs),	
	O is M-1,
	check_last_X_differences(N, LeafIDs, O).

checkDiffsForAllLeaves(_,_,[]) :- !.
checkDiffsForAllLeaves(C1, C2, [ID|Tail]) :-
	% These will fail safely if there aren't records for this particular ID.
	% The overall q_values_have_converged rule will fail.
	episode_high_val(C1, ID, OlderVal),
	episode_high_val(C2, ID, NewerVal),
	% Discard when the highest Q-value is zero, to prevent divide-by-zero
	( ((OlderVal == 0.0) ; (OlderVal == 0)) -> fail ; true),
	( ((NewerVal == 0.0) ; (NewerVal == 0)) -> fail ; true),
	Diff1 is (NewerVal/OlderVal),
	Diff2 is (OlderVal/NewerVal),
	Diff1 < 1.01, % Less than 1% positive change required, for a set number of generations (convergence_buffer)
	Diff2 < 1.01, % Less than 1% negative change required, for a set number of generations (convergence_buffer)
	!,
	checkDiffsForAllLeaves(C1, C2, Tail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Case 1: e.g. positive affordance learning. An executability condition must be checked before the action's transition.
% If (a) the action is the target action, (b) the particular executability condition holds, and (c) the action's outcome matches the goal state, get positive reward.
applyActionAndReportOracleOutcome(Action, RewardValue) :-
	goalState(Goal),
	member(requiredToCheckBeforeTransition(TargAction, ID), Goal),
	Action = TargAction,
	impossible_if(TargAction, ID),
	% Check here for debugging positive affordance learning - the impossible_if statement may not hold and this point may not be reached
	!,
	applyActionToStateAndUpdateHistory(Action),
	(goalAchieved(Goal) -> (assert(goalreached), reward_pos(X), RewardValue = X) ; (reward_neg(Y), RewardValue = Y)).
% Case 2: nothing extra to check before the action's transition.
% If the action is the target action, and the action's outcome matches the goal state, get positive reward.
applyActionAndReportOracleOutcome(Action, RewardValue) :-
	goalState(Goal),
	not(member(requiredToCheckBeforeTransition(_, _), Goal)),
	!,
	applyActionToStateAndUpdateHistory(Action),
	(goalAchieved(Goal) -> (assert(goalreached), reward_pos(X), RewardValue = X) ; (reward_neg(Y), RewardValue = Y)).
% Case 3, catch case: e.g. where an executability condition must be checked before the action's transition, but it does not hold or the action under consideration is not the target action.
applyActionAndReportOracleOutcome(Action, RewardValue) :-
	applyActionToStateAndUpdateHistory(Action),
	(reward_neg(Y), RewardValue = Y).

goalAchieved(List) :-
	select(requiredToCheckBeforeTransition(_,_), List, List2),
	!,
	achieved(List2).
goalAchieved(List) :-
	achieved(List).
	
achieved([]) :-
	!.
achieved([A|B]) :-
	(currentState(A) ; 
		(A = not(fluent(Z)), not(currentState(fluent(Z))))  ;
		(A = lastActionWas(X), lastActionWas(X))  ),
	!,
	achieved(B).

applyActionToStateAndUpdateHistory(Action) :-
	retractall(lastActionWas(_)),
	assert(lastActionWas(Action)),
	applyActionToState(Action),
	!.

applyActionToStateAndUpdateHistory(Action) :-
	print_major('Action application has failed. Problem: '),
	println_major(Action),
	nl_major,
	trace.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
doGeneralisation :-
	enter_register('generalisation:1makeexamples'),
	
	findall([A1], leaf(A1), List0), length(List0,Size0),
	print_medium('===> Number of leaves in the BDT: '), println_medium(Size0),
		
	println_medium('Extracting examples...'), 
	makeGeneralisedExamples,
	
	findall([ID, ConfigID, Literals, S, C], examplepathrecord(ID, ConfigID, Literals, S, C), List1), length(List1,Size1),
	print_medium('===> Number of \'examplepathrecord\' records including duplicates; nonactions removed: '), println_medium(Size1), 
	
	exit_register('generalisation:1makeexamples'),
	enter_register('generalisation:2constructcandidates'),
	
	println_medium('Constructing full set of generalised examples...'), 
	constructCandidateAxioms,
	exit_register('generalisation:2constructcandidates'),
	enter_register('generalisation:3findallsemi'),
	findall([CC,DD,EE,FF,GG,HH], semifinalexample(CC, DD, EE, FF, GG, HH), List2), length(List2,Size2),
	print_medium('===> Number of \'semifinalexample\' records produced by finding subsets of each example: '), println_medium(Size2), 
		
	exit_register('generalisation:3findallsemi'),
	enter_register('generalisation:4maketree'),
	
	% Measure how many examples are strictly more specific than others, and report on that number.
	println_medium('Making candidate tree'),
	makeCandidateTree,
	exit_register('generalisation:4maketree'),
	enter_register('generalisation:5findallcand'),
	findall([A2A,B2B,C2C,D2D,E2E,F2F,LL], candidate_axiom(A2A, B2B, C2C, D2D, E2E, F2F, LL), List3), length(List3,Size3),
	print_medium('===> Number of candidate axioms: '), println_medium(Size3),
	println_medium('\nCandidate axioms, pre parsimony:'),
	exit_register('generalisation:5findallcand'),
	enter_register('generalisation:6adjust'),
	println_medium('Extracting top candidate axioms. FINAL CHOICES...'),
	adjustAxiomsWithMean,
	exit_register('generalisation:6adjust'),
	enter_register('generalisation:7sort'),
	println_medium('Sorting top candidate axioms. FINAL CHOICES...'),
	sortFinalAxioms,
	exit_register('generalisation:7sort'),
	enter_register('generalisation:8final'),
	println_medium('Finalising top candidate axioms, considering generality (universal filter). Final choices...'),
	finalAxiomQualityImprovement,
	extractAndReportTopAxioms(0),
	println_medium('Checking top (max ten) candidates with oracle. Final choices...'),
	checkTopCandidatesWithOracle,
	finaloutputfile(OO),
	open(OO,write,OStr),
	set_output(OStr),
	extractAndReportTopAxioms('final'),
	close(OStr),
	exit_register('generalisation:8final'),
	enter_register('generalisation:9lift'),
	println_medium('Lifting final candidates'),
	
	liftCandidates,
	
	extractAndReportTopAxioms('lifted'),
	
	exit_register('generalisation:9lift'),
	
	!.

% Transform all leaves into "path records" of tests from leaf to root, keeping only attributes and actions.
makeGeneralisedExamples :-
	% Pick any leaf L
	leaf(ID),
	not(leaf_generalised(ID,_)),
	% Collect the partial state/action described by the path from L to root, looking only at internal nodes
	!,
	getStateActionInfoOnPath(ID, StateNeg, StatePos),
	!,
	makeExamplePathRecords(ID, StateNeg, StatePos),
	% Remove leaves
	!,
	makeGeneralisedExamples.
makeGeneralisedExamples :-
	not( (leaf(ID), not(leaf_generalised(ID,_))) ),
	!.
makeGeneralisedExamples :-
	not( leaf(_) ),
	!.

makeExamplePathRecords(ID, StateNeg, StatePos) :-
	findall(X1, (member(X1, StatePos), X1 \= action(_) ), Pos1),
	findall(X2, (member(X2, StateNeg), X2 \= action(_) ), Neg1),
	sort(Pos1, Pos2),
	sort(Neg1, Neg2),
	Literals = [Pos2, Neg2],
	domainGoalAction(Target),
	(
		% Case 1: A positive action is in StatePos and it matches the target action
		% Case 2: Target action is specified within the actual examples at the leaf
		member(action(Target), StatePos)
	->
		addMatchingSubsetAsPathRecord(ID, Literals)
	;
		addMatchingSubsetAsPathRecordConditionally(ID, Literals, Target)
	).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Key here is to go through and do it separately for each different configuration ID

addMatchingSubsetAsPathRecord(ID, AttsAndFluentsPosAndNeg) :-
	leaf_stored_example(ID, _, _, _, _, ConfigID),
	not(leaf_generalised(ID,ConfigID)),
	findall(LeafExample, (LeafExample = leaf_stored_example(ID, _, _, _, _, ConfigID), LeafExample), LeafExamples),
	addANewPathRecord(ID, ConfigID, AttsAndFluentsPosAndNeg, LeafExamples),
	assert(leaf_generalised(ID,ConfigID)),
	!,
	addMatchingSubsetAsPathRecord(ID, AttsAndFluentsPosAndNeg).
addMatchingSubsetAsPathRecord(ID, _) :-
	not(leaf_generalised(ID,_))
	->
	assert(leaf_generalised(ID,-1))
	;
	true.

addMatchingSubsetAsPathRecordConditionally(ID, AttsAndFluentsPosAndNeg, DGA) :-
	leaf_stored_example(ID, _, _, _, _, ConfigID),
	not(leaf_generalised(ID,ConfigID)),
	findall(LeafExample, (LeafExample = leaf_stored_example(ID, ExRemainder, _, _, _, ConfigID), LeafExample, member(action(DGA), ExRemainder)), LeafExamples),
	addANewPathRecord(ID, ConfigID, AttsAndFluentsPosAndNeg, LeafExamples),
	assert(leaf_generalised(ID,ConfigID)),
	!,
	addMatchingSubsetAsPathRecordConditionally(ID, AttsAndFluentsPosAndNeg, DGA).
addMatchingSubsetAsPathRecordConditionally(ID, _, _) :-
	not(leaf_generalised(ID,_))
	->
	assert(leaf_generalised(ID,-1))
	;
	true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
addANewPathRecord(_, _, _, []) :- !.
addANewPathRecord(_, _, [[],[]], _) :- !.
addANewPathRecord(ID, ConfigID, [AttsAndFluentsYes,AttsAndFluentsNo], LeafExamples) :-
	retractall(sumQCollector(_)),
	retractall(countCollector(_)),
	assert(sumQCollector(0.0)),
	assert(countCollector(0)),
	amassPathRecord(ID, ConfigID, LeafExamples),
	sumQCollector(SumQ),
	countCollector(Count),
	assert(examplepathrecord(ID, ConfigID, [AttsAndFluentsYes, AttsAndFluentsNo], SumQ, Count)),
	!.
addANewPathRecord(ID, ConfigID, [AttsAndFluentsYes,AttsAndFluentsNo], LeafExamples) :-
	println_major('Error...'),
	println_major([ID, ConfigID, [AttsAndFluentsYes,AttsAndFluentsNo], LeafExamples]),
	trace.

amassPathRecord(ID, ConfigID, LeafExamples) :-
	member(leaf_stored_example(ID, _ExRemainder, InternalCount, InternalSum, _, ConfigID), LeafExamples),
	sumQCollector(SumQ), SumQNew is SumQ + (InternalSum / InternalCount),
	countCollector(Count), CountNew is Count + 1,
	retractall(sumQCollector(_)),
	retractall(countCollector(_)),
	assert(sumQCollector(SumQNew)),
	assert(countCollector(CountNew)),
	fail.
amassPathRecord(_,_,_) :- !.

subsetp([], []).
subsetp([E|Tail], [E|NTail]):- subsetp(Tail, NTail).
subsetp([_|Tail], NTail):- subsetp(Tail, NTail).
  
% Combine path records, transforming the generalised forms into 'semifinalexample', and delete path records.
% Note the examples used have no fluent information (which would have been all that distinguished some of them).
% Hence we now expect to have duplicates in our semifinalexample([S1,S2], Q, Worst, Count) entries.
% Hence "training samples".
constructCandidateAxioms :-
	not(examplepathrecord(_, _, _, _, _)),
	!.

constructCandidateAxioms :-
	% 1. Pick any path record
	examplepathrecord(ID, ConfigID, [Pos,Neg], Sum, Count),
	% 2. Infer its candidate-subsets
	pos_or_neg_clause_limit_per_axiom(Lim),
	findall(SubPos, (subsetp(Pos, SubPos), length(SubPos, SP), SP =< Lim), ListOfPosSubsets),
	findall(SubNeg, (subsetp(Neg, SubNeg), length(SubNeg, SN), SN =< Lim), ListOfNegSubsets),
	findall([A,B], (member(A,ListOfPosSubsets), member(B,ListOfNegSubsets), [A,B] \== [[],[]]), List),
	!,
	% 3. For each, create or find the candidate and increment its two values appropriately
	updateCandidateAxioms(List, Sum, Count, ID, ConfigID),
	% 4. Delete the path record
	retractall(examplepathrecord(ID, ConfigID, [Pos,Neg], Sum, Count)),
	% 5. Repeat
	!,
	constructCandidateAxioms.
	
% Base case: Done
updateCandidateAxioms([], _S, _C, _ID, _CID) :- !.

% Case 1: Candidate already exists
updateCandidateAxioms([A|B], Sum, Count, ID, ConfigID) :-
	% Check candidate exists
	A = [PosList,NegList],
	semifinalexample([PosList,NegList], QV, Worst, OldCount, OldIDList, OldConfigList),
	!,
	% Update the candidate,
	Q2 is QV + Sum,
	Count2 is OldCount + Count,
	TempMean is Sum/Count,
	((TempMean < Worst) -> Worst2 = TempMean ; Worst2 = Worst),
	(member(ID, OldIDList) -> NewIDList = OldIDList ; append(OldIDList, [ID], NewIDList)),
	(member(ConfigID, OldConfigList) -> NewConfigList = OldConfigList ; append(OldConfigList, [ConfigID], NewConfigList)),
	% Replace the old candidate
	retractall(semifinalexample([PosList,NegList], QV, Worst, OldCount, OldIDList, OldConfigList)),
	assert(semifinalexample([PosList,NegList], Q2, Worst2, Count2, NewIDList, NewConfigList)),
	% Recurse
	updateCandidateAxioms(B, Sum, Count, ID, ConfigID).

% Case 2: Make new candidate	
updateCandidateAxioms([A|B], Sum, Count, ID, ConfigID) :-
	% Assert new candidate
	A = [PosList,NegList],
	not(semifinalexample([PosList,NegList], _, _, _, _, _)),
	!,
	TempMean is Sum/Count,
	assert(semifinalexample([PosList,NegList], Sum, TempMean, Count, [ID], [ConfigID])),
	% Recurse
	updateCandidateAxioms(B, Sum, Count, ID, ConfigID).

% Catch case, only reached if an error occurs
updateCandidateAxioms(_, _, _, _, _) :- trace.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getStateActionInfoOnPath(LeafID, SortedStateNeg, SortedStatePos) :-
	recursiveGetStateActionInfoOnPath(LeafID, [], SortedStateNeg, [], SortedStatePos).

% Base case
recursiveGetStateActionInfoOnPath(Node, CurrentStateNeg, ReturnSortedStateNeg, CurrentStatePos, ReturnSortedStatePos) :-
	root(Node),
	sort(CurrentStateNeg, ReturnSortedStateNeg),
	sort(CurrentStatePos, ReturnSortedStatePos),
	!.

% Non-root: leaf or intermediate node (note the leaf=root combination is covered above)
% This is a positive or negative example of parent 'action', 'fluent', or 'attr' test.
recursiveGetStateActionInfoOnPath(Node, CurrentStateNeg, ReturnSortedStateNeg, CurrentStatePos, ReturnSortedStatePos) :-
	not(root(Node)),
	parent(Node, Node2),
	(test(Node2, Test) ; (println_major('recursiveGetStateActionInfoOnPath failed: no test\n'), trace, fail)),
	!,
	(
		child_y(Node2, Node)
		->
		(append(CurrentStatePos,[Test],CurrentStatePos2), CurrentStateNeg2 = CurrentStateNeg)
		;
		(append(CurrentStateNeg,[Test],CurrentStateNeg2), CurrentStatePos2 = CurrentStatePos) % assume child_n
	),
	!,
	recursiveGetStateActionInfoOnPath(Node2, CurrentStateNeg2, ReturnSortedStateNeg, CurrentStatePos2, ReturnSortedStatePos).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

makeCandidateTree :-
	println_medium('Finalising all semifinalexample candidates\n'),
	reduceToHighQualityCandidates,
	findall([A,B,C,D,E], finalexample(A, B, C, D, E), List3), length(List3,Size3),
	print_medium('===> Number of reduced \'finalexample\' records remaining: '), println_medium(Size3),
	establishSupportForCandidates,
	println_medium('===> Now supported with random examples.'),
	finaliseCandidates.

reduceToHighQualityCandidates :-
	not(semifinalexample(_,_,_,_,_,_)),
	!.

reduceToHighQualityCandidates :-
	semifinalexample([Yes,No], QValueSum, WorstQValue, ExampleCount, SupportingLeavesList, SupportingConfigsList),
	% Arithmetic mean
	TempMean is QValueSum/ExampleCount,
	retractall(semifinalexample([Yes,No], QValueSum, WorstQValue, ExampleCount, SupportingLeavesList, SupportingConfigsList)),
	reward_pos(N),
	Min is N * 0.99, % Take only examples that were assigned Q on average not less than 1% below max_value
	( (TempMean > Min) 
		->
		assert(finalexample([Yes,No], QValueSum, WorstQValue, ExampleCount, SupportingConfigsList)) ; true),
	!,
	reduceToHighQualityCandidates.

%%%%%%%%%%%%%%%%%

% Next cycle through all, adding the values of random examples to the appropriate candidates

establishSupportForCandidates :-
	allValidTests(DomainTestsList),
	length(DomainTestsList, DTL),
	findall(	Test,
				test(_SomeNode,Test),
				TestsUsed
			),
	sort(TestsUsed, UniqueTestsUsed),
	length(UniqueTestsUsed, UIN),
	N is (DTL - UIN) +1,
	findall(	ALength,
				( leaf(ID), getAncestorTests(ID, Ancestors), length(Ancestors, ALength) ),
				PathLengths
			),
	sum_list(PathLengths, SL),
	length(PathLengths, LL),
	P is SL / LL,
	% Take average
	Average is (N + P) / 2,
	sample_multiplier(SM),
	Total is Average * SM,
	assert(number_of_random_sample_draws_to_make(Total)),
	print_medium('Sample multiplier: '), println_medium(SM),
	print_medium('Path length: '), print_medium(P), 
	print_medium('; unused attribute (or literal) tests: '), print_medium(N), 
	print_medium('; average: '), println_medium(Average),
	print_medium('Number of random samples to draw: '), println_medium(Total),
	% Now perform the support step
	!,
	findall(X, (X = leaf_stored_example(_, _, _, _, _, _CID), X), AllExamplesList),
	length(AllExamplesList, AELSize),
	print_medium('Total set of samples to draw from: '), println_medium(AELSize),
	supportWithRandomExamples(AllExamplesList).

% Base case: Done
supportWithRandomExamples(_) :-
	number_of_random_sample_draws_to_make(X),
	random_sampling_count(Y),
	Y >= X, % (Not an integer)
	print_medium('\nFinished support step: Drew '),
	print_medium(Y),
	println_medium(' random examples.'),
	!.
% Base case: Ran out of possible samples early
supportWithRandomExamples([]) :-
	println_medium('\n*****\nFinished support step: Ran out of possible samples early!\n*****'),
	!.
% Recursive case: Support with a new sample
supportWithRandomExamples(ExampleList) :-
	drawNewRandomSampleFromBDT(ExampleList, NewExampleList),
	addToSampleCount(1),
	!,
	supportWithRandomExamples(NewExampleList).

addToSampleCount(Integer) :-
	random_sampling_count(RSC),
	RSC2 is RSC + Integer,
	retractall(random_sampling_count(RSC)),
	assert(random_sampling_count(RSC2)).

drawNewRandomSampleFromBDT(ExampleList, ElidedExampleList) :-
	% 1. Get a random example from a random leaf.
	% 2. Find the full example by adding (a) its locally stored object attributes (that match domain-defined testable ones) to (b) its path to root.
	% 3. Find each subset of the full example that matches a candidate.
	% 4. Add the example's value to that candidate's, and increment the candidate's count.
	% 5. Delete the example so it can't be drawn again.
	random_member(RandomExample, ExampleList),
	select(RandomExample, ExampleList, ElidedExampleList), % Can't be used again.
	%
	RandomExample = leaf_stored_example(LeafID, RemainderList, Count, SumOfQ, _SumOfSquaredQ, ConfigID),
	!,
	AddedValue is SumOfQ / Count,
	getStateActionInfoOnPath(LeafID, NegativeAncestors, PositiveAncestors),
	%
	append(RemainderList, PositiveAncestors, PositiveExampleLiterals1),
	NegativeExampleLiterals1 = NegativeAncestors,
	
	domainGoalAction(DGA),
	(not(member(action(DGA), PositiveExampleLiterals1))
	->
	( addToSampleCount(-1) )
	;
	(
		reduceToLits(PositiveExampleLiterals1, PositiveExampleLits),
		reduceToLits(NegativeExampleLiterals1, NegativeExampleLits),
		%
		findall(	Candidate,
					someSubsetMatchesSomeCandidate(PositiveExampleLits, NegativeExampleLits, Candidate),
					CandidatesToAdjust0
				),
		sort(CandidatesToAdjust0, CandidatesToAdjust), % Needed?
		addToAllCandidates(CandidatesToAdjust, AddedValue, ConfigID)
	)).

reduceToLits(List1, List2) :-
	findall(X, (member(X, List1), X \= action(_)), List2).

someSubsetMatchesSomeCandidate(PositiveExampleLits, NegativeExampleLits, Candidate) :-
	closed_world(false),
	!,
	% Each in the 'negative' section of the candidate axiom must also be in the 'negative' section of the example
	finalexample([AnyPositiveSubset,AnyNegativeSubset], QValueSum, WorstQValue, ExampleCount, SupportingConfigsList),
	subsetp(PositiveExampleLits, AnyPositiveSubset),
	subsetp(NegativeExampleLits, AnyNegativeSubset),
	Candidate = finalexample([AnyPositiveSubset,AnyNegativeSubset], QValueSum, WorstQValue, ExampleCount, SupportingConfigsList).
	
someSubsetMatchesSomeCandidate(PositiveExampleLits, _NegExampleLits, Candidate) :-
	closed_world(true),
	!,
	% It is enough that each in the 'negative' section of the candidate axiom is NOT in the positive section of the example
	% This is important because otherwise we can only find things that have appeared as negative tests
	Candidate = finalexample([AnyPositiveSubset,CandidateNegativeLiterals], QValueSum, WorstQValue, ExampleCount, SupportingConfigsList),
	% It exists
	finalexample(            [AnyPositiveSubset,CandidateNegativeLiterals], QValueSum, WorstQValue, ExampleCount, SupportingConfigsList),
	subsetp(PositiveExampleLits, AnyPositiveSubset),
	not( (member(N,CandidateNegativeLiterals), member(N,PositiveExampleLits) ) ).
	
addToAllCandidates([], _, _) :- !.
addToAllCandidates([A|B], AddedValue, ConfigID) :-
	A = finalexample(Content, QValueSum, WorstQValue, ExampleCount, OldConfigList),
	NewQ is QValueSum + AddedValue,
	((AddedValue < WorstQValue) -> NewWorst = AddedValue ; NewWorst = WorstQValue),
	NewCount is ExampleCount + 1,
	(member(ConfigID, OldConfigList) -> NewConfigList = OldConfigList ; append(OldConfigList, [ConfigID], NewConfigList)),
	retractall(finalexample(Content, QValueSum, WorstQValue, ExampleCount, OldConfigList)),
	assert(finalexample(Content, NewQ, NewWorst, NewCount, NewConfigList)),
	!,
	addToAllCandidates(B, AddedValue, ConfigID).

%%%%%%%%%%%%%%%%%

% Finally, remove cases that are still supported by only one example.
finaliseCandidates :-
	retractall(finalexample(_, _, _, 1, _)), % One example
	
	% Retract cases which would only be supported by one or two configurations.
	% This function removed as of 2017-06-20; not thematically compatible with relevance.
	% Relevance could have reduced configs in such a way that every attribute is important, and thus only one config exists in which the phenomenon of interest can occur.
	%%% retractall(finalexample(_, _, _, _, [_SingleElementList])),
	%%% retractall(finalexample(_, _, _, _, [_One, _Two])),
	
	retractall(finalexample(_, _, _, _, [])), % No support... This should never occur.
	!,
	changeExamplesToCandidates.

changeExamplesToCandidates :-
	finalexample([Yes,No], Q, Worst, Count, SupportingConfigsList),
	retractall(finalexample([Yes,No], _, _, _, _)),
	domainGoalAction(Act),
	length(SupportingConfigsList, L),
	assert(candidate_axiom(raw, unexpected(Act), [Yes,No], Q, Worst, Count, L)),
	!,
	% 3. Recursive call
	changeExamplesToCandidates.
changeExamplesToCandidates.


%%%%%%%%%%%%%%%%%


% 'Causal law: [Effects] :- Action, [Conditions].'
liftCandidates :-
	learning_type(causal_law_learning),
	final_axiom(Rank, unexpected(Action), [Yes,No], AdjustedMean, Worst, Count, Configs),
	!,
	unexpectedResult(Effects),
	lift_lists_to_strings([[Action],Yes,No,Effects], [[Action2],Yes2,No2,Effects2]),
	create_negation_strings(No2,Neg),
	create_comma_separated_strings(Neg,Negs),
	create_comma_separated_strings(Yes2,Pos),
	create_comma_separated_strings(Effects2,EffectsString),
	concat('Causal law:   ', EffectsString, Part1),
	concat(Part1, ' :- ', Part2),
	concat(Part2, Action2, Part3),
	concat(Part3, ', ', Part4),
	concat_with_two_possibly_empty_strings(Part4, Negs, Pos, Part5),
	concat(Part5, '.', String),
	retractall(final_axiom(Rank, unexpected(Action), [Yes,No], AdjustedMean, Worst, Count, Configs)),
	assert(final_lifted_axiom(Rank, String)),
	liftCandidates.

% 'Executability condition: impossible(Action) :- [Conditions].'
liftCandidates :-
	learning_type(executability_condition_learning),
	final_axiom(Rank, unexpected(Action), [Yes,No], AdjustedMean, Worst, Count, Configs),
	!,
	lift_lists_to_strings([[Action],Yes,No], [[Action2],Yes2,No2]),
	create_negation_strings(No2,Neg),
	create_comma_separated_strings(Neg,Negs),
	create_comma_separated_strings(Yes2,Pos),
	concat('Executability condition:   impossible(', Action2, Part1),
	concat(Part1, ') :- ', Part2),
	concat_with_two_possibly_empty_strings(Part2, Negs, Pos, Part3),
	concat(Part3, '.', String),
	retractall(final_axiom(Rank, unexpected(Action), [Yes,No], AdjustedMean, Worst, Count, Configs)),
	assert(final_lifted_axiom(Rank, String)),
	liftCandidates.

% 'Negative affordance: fails(Action) :- [Conditions].'
liftCandidates :-
	learning_type(negative_affordance_learning),
	final_axiom(Rank, unexpected(Action), [Yes,No], AdjustedMean, Worst, Count, Configs),
	!,
	lift_lists_to_strings([[Action],Yes,No], [[Action2],Yes2,No2]),
	create_negation_strings(No2,Neg),
	create_comma_separated_strings(Neg,Negs),
	create_comma_separated_strings(Yes2,Pos),
	concat('Negative affordance:   fails(', Action2, Part1),
	concat(Part1, ') :- ', Part2),
	concat_with_two_possibly_empty_strings(Part2, Negs, Pos, Part3),
	concat(Part3, '.', String),
	retractall(final_axiom(Rank, unexpected(Action), [Yes,No], AdjustedMean, Worst, Count, Configs)),
	assert(final_lifted_axiom(Rank, String)),
	liftCandidates.

% 'Positive affordance: succeeds(Action) despite (ID) [Ex Conditions] :- [Conditions].'
liftCandidates :-
	learning_type(positive_affordance_learning),
	final_axiom(Rank, unexpected(Action), [Yes,No], AdjustedMean, Worst, Count, Configs),
	!,
	executabilityConditionViolated(ID),
	clause(impossible_if(Action, ID), TailTerm),
	round_bracketed_term_to_list(TailTerm, ExCondList),
	lift_lists_to_strings([[Action],Yes,No,ExCondList], [[Action2],Yes2,No2,ExCondList2]),
	create_negation_strings(No2,Neg),
	create_comma_separated_strings(Neg,Negs),
	create_comma_separated_strings(Yes2,Pos),
	create_comma_separated_strings(ExCondList2,ExecConditionString),
	concat('Positive affordance:   succeeds(', Action2, Part1),
	concat(Part1, ') despite (', Part2),
	concat(Part2, ID, Part3),
	concat(Part3, ') ', Part4),
	concat(Part4, ExecConditionString, Part5),
	concat(Part5, ' :- ', Part6),
	concat_with_two_possibly_empty_strings(Part6, Negs, Pos, Part7),
	concat(Part7, '.', String),
	retractall(final_axiom(Rank, unexpected(Action), [Yes,No], AdjustedMean, Worst, Count, Configs)),
	assert(final_lifted_axiom(Rank, String)),
	liftCandidates.

liftCandidates.


% % % Helpers for lifting

concat_with_two_possibly_empty_strings(In, '', '', In) :- !.
concat_with_two_possibly_empty_strings(In, '', String2, Out) :- !, concat(In, String2, Out).
concat_with_two_possibly_empty_strings(In, String1, '', Out) :- !, concat(In, String1, Out).
concat_with_two_possibly_empty_strings(In, String1, String2, Out) :- !, concat(In, String1, Y), concat(Y, ', ', Z), concat(Z, String2, Out).

% Use for e.g. complex terms found as tails of clauses, to turn an arbitrary length list (...) into the same length list [...]
round_bracketed_term_to_list(RoundedTerm, OutputList) :-
	term_string(RoundedTerm, String1),
	round_to_list(String1, OutputList).
round_to_list(true, []) :- !. % If called from finding a clause that is actually a fact.
round_to_list(String, OutList) :-
	sub_string(String, 0, 1, CharsAfter, "("), % First character is open round bracket
	sub_string(String, CharsAfter, 1, 0, ")"), % Last character is close round bracket
	!,
	sub_string(String, 1, _Length, 1, Middle),
	concat('[', Middle, Middle2),
	concat(Middle2, Middle, Middle3),
	concat(Middle3, ']', FinalString),
	term_string(OutList, FinalString).
round_to_list(String, [String]). % If called without round brackets.

create_negation_strings(ListOfStrings,ListOfNegatedStrings) :-
	create_negation_strings_recursive(ListOfStrings,[],ListOfNegatedStrings).
create_negation_strings_recursive([],ListOfNegatedStrings,ListOfNegatedStrings).
create_negation_strings_recursive([NextString|Tail],CurrentNegations,Final) :-
	concat('-', NextString, NewNegation),
	create_negation_strings_recursive(Tail,[NewNegation|CurrentNegations],Final).

create_comma_separated_strings([],'').
create_comma_separated_strings([A|B],Final) :-
	create_comma_separated_strings_recursive(B,A,Final).
create_comma_separated_strings_recursive([],Final,Final).
create_comma_separated_strings_recursive([A|B],CurrentString,Final) :-
	concat(CurrentString, ', ', Next),
	concat(Next, A, Next2),
	create_comma_separated_strings_recursive(B,Next2,Final).
	
% InputListOfNListsOfTerms    : an arbitrary list of sublists, each sublist containing an arbitrary number of terms
% OutputListOfNListsOfStrings : a list of sublists following the same structure, each sublist containing terms translated into strings
lift_lists_to_strings(InputListOfNListsOfTerms, OutputListOfNListsOfStrings) :-
	recurse_lift_lists(InputListOfNListsOfTerms, [], OutputListOfNListsOfStrings).

recurse_lift_lists([], OutputListOfNListsOfStrings, OutputListOfNListsOfStrings).
recurse_lift_lists([A|B], Current, OutputListOfNListsOfStrings) :-
	recurse_lift_strings_in_list(A,[],A2),
	recurse_lift_lists(B, [A2|Current], OutputListOfNListsOfStrings).

recurse_lift_strings_in_list([],Return,Return).
recurse_lift_strings_in_list([A|B],Current,Return) :-
	term_string(A, AString),
	lift_term_to_string(AString, A2),
	recurse_lift_strings_in_list(B,[A2|Current],Return).

% Find last '('; apply string_upper to substring following that.
% If no '(', return.
lift_term_to_string(InString, OutString) :-
	sub_string(InString, _, 1, _, "("), % Has at least one open round bracket
	!,
	findall(CharsBefore,
		sub_string(InString, CharsBefore, 1, _CharsAfter, "("),
		ListOfSubStrings),
	member(Before, ListOfSubStrings),
	not(( member(Bef2, ListOfSubStrings), Bef2 > Before)), % No opening bracket comes later than this one (necessary in case of nested brackets)
	sub_string(InString, Before, _Length, 0, SubString),
	string_upper(SubString, SUBSTR),
	sub_string(InString, 0, Before, _After, Start),
	concat(Start, SUBSTR, OutString).
lift_term_to_string(InString, OutString) :-
	concat(InString, "", OutString).

adjustAxiomsWithMean :-
	not(candidate_axiom(raw,_,[_,_],_,_,_,_)),
	!.
adjustAxiomsWithMean :-
	candidate_axiom(raw,Act,[Yes,No],Q,Worst,Count,Configs),
	Mean is Q/Count,
	retractall(candidate_axiom(raw,Act,[Yes,No],Q,Worst,Count,Configs)),
	assert(candidate_axiom(Mean,Act,[Yes,No],Q,Worst,Count,Configs)),
	!,
	adjustAxiomsWithMean.

% (a) Only take absolute top tier
% (b) Rank by total number of supporting examples
sortFinalAxioms :-
	enter_register('generalisation:7sorta'),
	reward_pos(N),
	Min is N * 0.99, % Keep candidates within 1% of maximum possible value
	exit_register('generalisation:7sorta'),
	!,
	enter_register('generalisation:7sortb'),
	rankFinalAxioms(Min, 1),
	exit_register('generalisation:7sortb').
% Previously, would take the top-valued axiom and compare to that (instead of reward value) --
% but then if no objectively highly-valued axiom is found, a set of low quality ones will be returned,
% when we would prefer an empty set to be returned.

rankFinalAxioms(Min, _) :-
	not((
		candidate_axiom(Mean,_,[_,_],_,_,_,_),
		Mean > Min
	)),
	!.
rankFinalAxioms(Min, Rank) :-
	enter_register('generalisation:7sortb1'),
	candidate_axiom(Mean,Act,[Yes,No],Q,Worst,Count,Configs), % (Determined that this is the most expensive operation)
	Mean > Min,
	% Change the rank each time, starting from best (highest mean), i.e., smallest number rank
	enter_register('generalisation:7sortb2'),
	not( (candidate_axiom(M,_,_,_Q2,_,Count2,_), M > Min, Count2 > Count) ),
	exit_register('generalisation:7sortb2'),
	exit_register('generalisation:7sortb1'),
	!,
	enter_register('generalisation:7sortb3'),
	retract(candidate_axiom(Mean,Act,[Yes,No],Q,Worst,Count,Configs)),
	asserta(final_axiom(Rank,Act,[Yes,No],Mean,Worst,Count,Configs)),
	NewRank is Rank+1,
	exit_register('generalisation:7sortb3'),
	!,
	rankFinalAxioms(Min, NewRank).
	
% % % % % % % % % %

finalAxiomQualityImprovement :-
	checkCandidateAxiom(1),
	reRankAxioms(0).

% 1. Run out of axioms to check.
checkCandidateAxiom(N) :-
	not( final_axiom(N,_Act,_,_Mean,_Worst,_Count,_Configs) ),
	!.
% 2. N is a more specific form of an already-confirmed axiom.
checkCandidateAxiom(N) :-
	final_axiom(N,_,[Yes1,No1],_,_,_,_),
	final_axiom(M,_,[Yes2,No2],_,_,_,_),
	M < N,
	subset(Yes2, Yes1), % Yes2 is subset of Yes1
	subset(No2, No1),
	!,
	retractall(final_axiom(N,_,[Yes1,No1],_,_,_,_)),
	Next is N+1,
	checkCandidateAxiom(Next).
% 3. N is a more general form of an already-confirmed axiom. (unlikely, but they could have same number, so technically possible)
checkCandidateAxiom(N) :-
	final_axiom(N,_,[Yes1,No1],_,_,_,_),
	final_axiom(M,_,[Yes2,No2],_,_,_,_),
	M < N,
	subset(Yes1, Yes2), % Yes1 is subset of Yes2
	subset(No1, No2),
	!,
	retractall(final_axiom(M,_,[Yes2,No2],_,_,_,_)),
	checkCandidateAxiom(N).
% 4. Base case
checkCandidateAxiom(N) :-
	!,
	Next is N+1,
	checkCandidateAxiom(Next).
	
reRankAxioms(CurrentLargestRank) :-
	% 1. Find the candidate with the rank > CurrentLargestRank but < all other extant ranks
	final_axiom(N,Act,B,Mean,Worst,C,Configs),
	N > CurrentLargestRank,
	not(( 
		final_axiom(M,_,_,_,_,_,_),
		M > CurrentLargestRank,
		M < N
	)),
	% 2. Retract it and set it to rank CurrentLargestRank+1
	retract(final_axiom(N,Act,B,Mean,Worst,C,Configs)),
	NewRank is CurrentLargestRank+1,
	% Discard any over the top ten!
	((NewRank < 11) -> assert(final_axiom(NewRank,Act,B,Mean,Worst,C,Configs)) ; true),
	% 3. Recurse
	!,
	reRankAxioms(NewRank).
% Base case
reRankAxioms(_) :- !.

removeArtificialNoiseGeneratorForChecking :-
	retract(noiseChancePercent(_)),
	assert(noiseChancePercent(0)).
	
checkTopCandidatesWithOracle :-
	% For each remaining candidate C in turn:
	% 1. Start with domain-defined physical state
	% 2. Test random object attribute configurations until you get an attribute configuration S where the action succeeds
	% 3. Now:
	%		(a) Modulate S to S' by the minimum necessary changes such that the partial configuration described by C holds in S'
	% 		(b) Poll the oracle as to whether the action still succeeds in S'
	% 		(c) If it does, discard the candidate axiom C
	removeArtificialNoiseGeneratorForChecking,
	batchReportingOutputs(0),
	extractAndReportTopAxioms(0),
	number_of_filters(Num),
	recursiveCheckTopCandidates(1, Num).
		

recursiveCheckTopCandidates(Current, Finish) :-
	Current > Finish,
	!.
recursiveCheckTopCandidates(Current, Finish) :-
	oracleFilterCandidates(1),
	reRankAxioms(0),
	batchReportingOutputs(Current),
	extractAndReportTopAxioms(Current),
	Next is Current +1,
	recursiveCheckTopCandidates(Next, Finish).

oracleFilterCandidates(N) :-
	not(final_axiom(N,_Act,_B,_Mean,_Worst,_C,_Configs)),
	!.
oracleFilterCandidates(N) :-
	print_medium('Oracle filter:\n  Checking candidate #'), println_medium(N),
	!,
	final_axiom(N,Act,[True,False],Mean,Worst,C,Configs),
	!,
	oracleFilterCandidates_by_rule_type(N,Act,True,False,Mean,Worst,C,Configs),
	M is N +1,
	!,
	oracleFilterCandidates(M).
	
% Learning positive affordances requires a special variation of filtering.
% Procedure:
% 1. Generate a random state S where the target impossible_if condition applies, and the other impossible_if conditions don't.
% 2. Make the necessary changes to S such that the candidate's literals are true, producing S'.
% 3. Only continue if the target impossible_if condition still applies and the other impossible_if clauses still don't.
% 4. Apply the action.
% 5. If the goal state is reached (literals + last-action-applied-was + ignore the pre-check for a particular impossible-if), retain the candidate; else, discard it.
% As always, it could now succeed through coincidence, in which case the oracle filter passes it through.
oracleFilterCandidates_by_rule_type(N,Act,True,False,Mean,Worst,C,Configs) :-
	learning_type(positive_affordance_learning),
	!,
	goalState(Goal),
	Act = unexpected(TargAction),
	member(requiredToCheckBeforeTransition(TargAction, ID), Goal),
	setAndTryNonvirtuousState(TargAction,ID,True,False),
	(not(actionActuallyHasUnexpectedOutcomeInDomain)
		->
		(print_low('  Pruning candidate <positive affordance> #'), println_low(N),
		retractall(final_axiom(N,Act,[True,False],Mean,Worst,C,Configs)))
		;
		true
	).
% Else, default
oracleFilterCandidates_by_rule_type(N,Act,True,False,Mean,Worst,C,Configs) :-
	% Learning type is not positive_affordance_learning
	!,
	setAndTryVirtuousState(True,False),	
	(not((systemThinksActionWillFailInDomain ; actionActuallyHasUnexpectedOutcomeInDomain))
		->
		(print_low('  Pruning candidate #'), println_low(N),
		retractall(final_axiom(N,Act,[True,False],Mean,Worst,C,Configs)))
		;
		true
	).


systemThinksActionWillFailInDomain :-
	domainGoalAction(Action),
	not(validAction(Action)), % System already believes it will fail in the current circumstances
	!.
actionActuallyHasUnexpectedOutcomeInDomain :-
	domainGoalAction(Action),
	applyActionToStateAndUpdateHistory(Action),
	goalState(GOAL),    % 1. This defines an unexpected failure/success;
	goalAchieved(GOAL), % 2. Therefore this is positive
	!.

storeCurrentStateAndConfig(List) :-
	findall(N, currentState(N), List).

restoreStateAndConfig(List) :-
	retract_facts_only(currentState(_)),
	restoreFromList(List).

restoreFromList([]) :- !.
restoreFromList([A|B]) :- 
	assert(currentState(A)),
	restoreFromList(B).


% % % % % % % % % % % % % % % % % % % % % % % % % 
% % % % % % % % % % % % % % % % % % % % % % % % % 
% % % % % % % % % % % % % % % % % % % % % % % % % 

% Each time this is called, it repeatedly
% 1. Generates a random new state
% 2. Makes the necessary swapins/swapouts to make it fulfil the learned axiom
% 3. Checks for consistency of the new physical state; continues if consistent; else starts over
% 4. Returns if the target impossible_if condition applies and the other impossible_if clauses don't; else starts over
setAndTryNonvirtuousState(Act,ID,True,False) :-
	resetStateAtRandom,
	setRandomInitialObjectConfig,
	!,
	setAndTryNonvirtuousState_sub_function(Act,ID,True,False).

setAndTryNonvirtuousState_sub_function(Act,ID,True,False) :-
	unfixable_virtuous_state_deviation(False),
	!,
	setAndTryNonvirtuousState(Act,ID,True,False).

setAndTryNonvirtuousState_sub_function(Act,ID,True,False) :-
	% Get list of non-target impossible_if conditions
	findall(	OtherID,
				(clause(impossible_if(Act, OtherID), _), OtherID \= ID),
				OtherConditions
			),
	adjustVirtuousState(True,False,Outcome),
	( ((Outcome = false) ; stateConstraintsViolated) -> setAndTryNonvirtuousState(Act,ID,True,False) ; true),
	!,
	(((impossible_if(Act,ID), not((member(XCond, OtherConditions), impossible_if(Act,XCond))))) % The target impossible_if condition applies; no others apply
	-> 
		true
		;
		setAndTryNonvirtuousState(Act,ID,True,False)
	).


	
% % % % % % % % % % % % % % % % % % % % % % % % % 
% % % % % % % % % % % % % % % % % % % % % % % % % 
% % % % % % % % % % % % % % % % % % % % % % % % % 

% Each time this is called, it repeatedly
% 1. Generates a random new state until it finds one where the target action succeeds
% 2. Makes the necessary swapins/swapouts to make it fulfil the learned axiom
% 3. Checks for consistency of the new physical state; returns if consistent, else starts over
setAndTryVirtuousState(True,False) :-
	resetStateAtRandom,
	setRandomInitialObjectConfig,
	storeCurrentStateAndConfig(List),
	!,
	((unfixable_virtuous_state_deviation(False) ; systemThinksActionWillFailInDomain ; actionActuallyHasUnexpectedOutcomeInDomain) -> setAndTryVirtuousState(True,False) ; 
		(
		% Action succeeds under these circumstances
		restoreStateAndConfig(List),
		adjustVirtuousState(True,False,Outcome),
		!,
		% If the adjusted state violates state constraints, start over. Else done.
		( ((Outcome = false) ; stateConstraintsViolated) -> setAndTryVirtuousState(True,False) ; true)
		)
	).

% Without this check before adjustVirtuousState(True,False), it will loop infinitely as it 'retracts' a derived belief and then the belief remains true in the state because it's derived
unfixable_virtuous_state_deviation(False) :-
	member(F,False),
	currentState(F),
	not(currentState_overt_belief_only(F)).
	/*
	Very important: There are interactions between overt beliefs stored as currentState(X) and beliefs that can be derived from others, in the form currentState(Y).
	This was added later, and much of the program is agnostic to it, and may e.g. assume that finding all currentState(X), retracting everything, and then resetting to the found Xs will set things
	as they were, even though in fact this process will accidentally reified derived beliefs into overt beliefs!
	Without the level of caution in unfixable_virtuous_state_deviation and currentState_overt_belief_only, things quickly devolve into broken assumptions and infinite loops.
	See also retract_facts_only.
	*/

adjustVirtuousState(_,False,false) :-
	unfixable_virtuous_state_deviation(False),
	!.
adjustVirtuousState(True,False,true) :-
	not( (member(F,False), currentState(F)) ),
	not( (member(T,True), not(currentState(T))) ),
	!.
adjustVirtuousState(True,False,Return) :-
	swapAllOutFromState(False), % Replaces with some attribute
	swapAllInToState(True), % Replaces existing attribute
	!,
	adjustVirtuousState(True,False,Return).
	
swapAllOutFromState([]) :- !.
swapAllOutFromState([A|B]) :-
	swapOut(A),
	swapAllOutFromState(B).

swapAllInToState([]) :- !.
swapAllInToState([A|B]) :-
	swapIn(A),
	swapAllInToState(B).	

swapOut(Literal) :-
	not( currentState(Literal) ),
	!.
swapOut(Literal) :-
	currentState(Literal),
	retract_facts_only(currentState(Literal)),
	get_all_alternative_domain_tests(Literal, AltList),
	random_member(Alt, AltList),
	assert(currentState(Alt)).
swapIn(Literal) :-
	currentState(Literal),
	!.
swapIn(Literal) :-
	get_all_alternative_domain_tests(Literal, AltList),
	deleteAllLits(AltList),
	assert(currentState(Literal)).
	
deleteAllLits([]) :- !.
deleteAllLits([A|B]) :-
	retract_facts_only(currentState(A)),
	deleteAllLits(B).


% % % % % % % % % % % % % % % % % % % % % % % % % 
% % % % % % % % % % % % % % % % % % % % % % % % % 
% % % % % % % % % % % % % % % % % % % % % % % % % 


batchReportingOutputs(Index) :-
	% If the batch support files exist (have been asserted), then output to them appropriately.
	output_file_counter(CounterFile),
	output_file_error(ErrorFile),
	!,
	open(CounterFile,append,CounterStream),
	open(ErrorFile,append,ErrorStream),
	reportTrueAndFalsePositives(CounterStream,ErrorStream,Index),
	close(CounterStream),
	close(ErrorStream),
	!.
% If no batch reporting started, do nothing.
batchReportingOutputs(_).

batchReportingTime :-
	% If the batch support files exist (have been asserted), then output to them appropriately.
	output_file_time(TFile),
	% Only if registers are being kept in the first place
	registers(true),
	!,
	open(TFile,append,TimeStream),
	key_registers_report(TimeStream),
	close(TimeStream),
	!.
% If no batch reporting started, do nothing.
batchReportingTime.

key_registers_report(Stream) :-
	register(b1, Stored1, -1), % BDT building
	write(Stream, '\n(1) '),
	write(Stream, Stored1),
	% (precalculation step shouldn't change, and might be omitted from batch runs)
	register(g1, Stored3, -1), % Generalisation
	write(Stream, '\n(3) '),
	write(Stream, Stored3),
	!.
key_registers_report(_) :- trace. % Catastrophic failure in batch testing.

reportTrueAndFalsePositives(CounterStream, ErrorStream, Index) :-
	% For each element in list, returns a number associated with the target axiom, or returns it back for "does not match any target affordance"
	findall(Identifier,
		(final_axiom(_R, _A, [YesLiterals,NoLiterals], _QM, _W, _C, _Configs),
		domainAxiomClassifier([YesLiterals,NoLiterals], Identifier)),
		ReturnList),
	% Number of unique numeric responses is the true positives
	sort(ReturnList, Set),
	findall(El, (member(El, Set), number(El)), NewSet),
	length(NewSet, TPs),
	% Other responses is the false positives
	findall(El, (member(El, Set), (not(number(El)), El \= ignore_axiom)), Unrecognised),
	FPs = Unrecognised,
	% Print them to the batch collater
	% Only bother with TPs before the oracle filter
	(Index == 0 -> reportTP(TPs, CounterStream) ; true),
	reportFalsePositives(FPs, ErrorStream, Index).
	
% If no false positives, store nothing. Else, store each on a new line.
reportFalsePositives([], _, _) :- !.
reportFalsePositives([A|B], ErrorStream, Index) :-
	reportFP(A, ErrorStream, Index),
	reportFalsePositives(B, ErrorStream, Index).
	
reportFP(Term, Stream, Prefix) :-
	write(Stream, '[filter '),
	write(Stream, Prefix),
	write(Stream, '] '),
	write(Stream, Term),
	write(Stream, '\n').
reportTP(Term, Stream) :-
	write(Stream, Term),
	write(Stream, '\n').
	

extractAndReportTopAxioms('lifted') :-
	writef('Number of oracle passes: final, lifted\n'),
	printLiftedAxiomListRecursive(1),
	writef('\n'),
	!.
	
extractAndReportTopAxioms('final') :-
	writef('Number of oracle passes: final\n'),
	printAxiomListRecursive(1),
	writef('\n'),
	!.
	
extractAndReportTopAxioms(NToReport) :-
	not(feedback(silent)),
	!,
	writef('\n------------------------\n'),
	writef('Number of oracle passes: '), print(NToReport), nl,
	printAxiomListRecursive(1),
	writef('\n------------------------\n').

extractAndReportTopAxioms(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_steps_in_episode :-
	one_step.

one_step :-
	enter_register('ep_steps:goalreached'),
	goalreached,
	%
	end_by_goal_counter(X),
	retractall(end_by_goal_counter(_)),
	Y is X + 1,
	assert(end_by_goal_counter(Y)),
	exit_register('ep_steps:goalreached'),
	!.
	
one_step :-
	enter_register('ep_steps:domainend'),
	domain_specified_end,
	%
	end_by_domain_counter(X),
	retractall(end_by_domain_counter(_)),
	Y is X + 1,
	assert(end_by_domain_counter(Y)),
	exit_register('ep_steps:domainend'),
	!.
	
one_step :-
	rrl(true),
	!,
	enter_register('ep_steps:main'),
	
	enter_register('ep_steps:main:1'),
	
	enter_register('ep_steps:main:1:a'),
	allValidTests(AVT),
	findall(N, (currentState(N), member(N, AVT)), StateListUnsorted),
	
	exit_register('ep_steps:main:1:a'),
	enter_register('ep_steps:main:1:b'),
	sort(StateListUnsorted, StateList),
	exit_register('ep_steps:main:1:b'),
	
	exit_register('ep_steps:main:1'),
			
	println_minor('Getting valid actions'),
	enter_register('ep_steps:main:2'),
	usableActionList(RelActions),
	
	findall(X, (validOrPotentialAction(X), member(action(X), RelActions)), ActionListX), % Actions that are valid in the current state
	exit_register('ep_steps:main:2'),
	
	(ActionListX==[] -> 
	defaultNullAction(ActionList)
	;
	ActionList = ActionListX
	),
	
	println_minor('Selecting an action...'),
	explore_parameter(Exp),
	random(F),
	
	(
		(F < Exp)
	->
		% Explore
		(println_minor('Getting random valid action'),
		random_member(Action, ActionList)
		)
	;
		% Policy
		(println_minor('Getting best valid action based on Q estimates'),
		
		enter_register('ep_steps:getBestByBDT'),
		pickBestByBinaryTreeEstimate(ActionList, Action),
		exit_register('ep_steps:getBestByBDT')
		)
		
	),
	
	print_minor('Applying action:  '),
	println_minor(Action),
	
	enter_register('ep_steps:applyAction'),
	applyActionAndReportOracleOutcome(Action, RewardValue),
	exit_register('ep_steps:applyAction'),
	
	/*
	Currently: The example is only used in the tree (i.e., trickled down) if it is actually relevant.
	Non-relevant actions can still be selected for trial, e.g. via the random policy.
	But examples constructed with non-relevant actions are not useful for the end goal, creating new domain axioms.
	*/
	
	%IF:
	
	(relevantTestLitAction(action(Action))
	->
	
	%THEN:
	
	(
		% Now in new state; find highest stored q-value from possible actions from this new state
		enter_register('ep_steps:findactions'),
		findall(Y, validOrPotentialAction(Y), ActionList2),
		exit_register('ep_steps:findactions'),
	
		enter_register('ep_steps:getBestByBDT'),
		getHighestQValueForAnyActionFromCurrentState(ActionList2, FutureValue),
		exit_register('ep_steps:getBestByBDT'),
	
		% Note that StateList is passed in as a record of the system's state when entering this step, prior to applying the action.
		println_minor('Updating Q-value'),
		append(StateList, [action(Action)], StateDescriptionWithAction),
	
		enter_register('ep_steps:trickleQVDown'),
		trickleNewQValueExampleDownTree(StateDescriptionWithAction, RewardValue, FutureValue),
		exit_register('ep_steps:trickleQVDown')
	)

	%ELSE: do nothing
	
	; true),
	
	%CONTINUE:
	
    !,
	exit_register('ep_steps:main'),
	one_step.

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pickBestByTable(StateList, ActionList, Action) :-
	findall([Act, Val], (qValueLearned(StateList, Act, Val, _Count), member(Act, ActionList)), ListOfPairs),
	( (ListOfPairs == []) -> random_member(Action, ActionList) ;
	(
		member([_,X], ListOfPairs),
		not( (member([_,Y], ListOfPairs), Y > X) ),
		findall(Act2, member([Act2,X], ListOfPairs), NewList),
		random_member(Action, NewList)
	)
	).

% Works whether or not a current example exists.
updateStoredExampleWithQV(StateList, Action, RewardValue, FutureValue) :-
	(qValueLearned(StateList, Action, CurrentQV, Count)
	->
	true
	;
	(CurrentQV = 0, Count = 0)
	),
	!,
	retractall(qValueLearned(StateList, Action, _, _)), % Remove existing score if applicable
	current_learning_rate_parameter(Gamma),	
	Count2 is Count + 1,
	Alpha is (1/Count2),
	NewQValue is CurrentQV + (Alpha * (RewardValue + (Gamma * FutureValue) - CurrentQV)),
	assert(qValueLearned(StateList, Action, NewQValue, Count2)).

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% TREE OPERATIONS %%%

splitNode(ID, Test) :-
	(leaf(ID) -> true ; (println_major('Error: Split failed\n'), trace, fail)),
	% Update count of splits made
	(split_count(CurrentCount), NewCount is CurrentCount+1, retractall(split_count(_)), assert(split_count(NewCount))),
	% Update counter, preventing convergence for a while
	(episode_count(EpCount)  -> 
		(retractall(last_split_at(_)),
		assert(last_split_at(EpCount)))
		;
		true),
	assert(test(ID, Test)),
	retract(leaf(ID)),
	retract(predicted_q_value(ID, _Val, _Count)),
	createNode(ChildYes),
	createNode(ChildNo),
	assert(child_y(ID, ChildYes)),
	assert(child_n(ID,  ChildNo)),
	assert(parent(ChildYes, ID)),
	assert(parent(ChildNo,  ID)),
	initialise_children_stats_and_q(ID, Test),
	!.
	
trickleNewQValueExampleDownTree(StateDescriptionWithAction, RewardValue, FutureValue) :-
	% Modify stored q-value for StateList:
	% Q(state, action) = current Q(state,action) +   gamma * (reward(state,action) + Max[Q(next state, all actions)] - current Q(state,action))
	root(ID),
	trickleDownTree(ID, StateDescriptionWithAction, RewardValue, FutureValue).
	
trickleDownTree(Node, StateDescriptionWithAction, RewardValue, FutureValue) :-
	not(leaf(Node)),
	!,
	test(Node, Test),
	(
	(member(Test, StateDescriptionWithAction) ; (Test = action(Action), member(Action,StateDescriptionWithAction), println_major('WARNING: Check trickleDownTree\n'))) % Test at node is positive?
	->
	(child_y(Node, NewNode), select(Test, StateDescriptionWithAction, NewStateDescriptionWithAction)) % Remove positive tests as you trickle down so end up with only remainder
	;
	(child_n(Node, NewNode), NewStateDescriptionWithAction = StateDescriptionWithAction)
	),
	!,
	trickleDownTree(NewNode, NewStateDescriptionWithAction, RewardValue, FutureValue).
trickleDownTree(Node, StateDescriptionWithAction, RewardValue, FutureValue) :-
	leaf(Node),
	!,
	updatePredictedQValue(Node, RewardValue, FutureValue),
	
	current_learning_rate_parameter(Gamma),
	
	(q_value_function(driessens)
	->
	(ExampleQValue is RewardValue + (Gamma * (FutureValue))) % Driessens et al: example Q = reward + (gamma)(max future Q)
	; true),
	(q_value_function(simple)
	->
	(ExampleQValue = RewardValue)
	; true),
	
	sort(StateDescriptionWithAction, Remainder),
	total_config_count(ConfigID),
	updateExamplesStoredAtLeaf(Node, Remainder, ExampleQValue, ConfigID),
	
	% Record that the system adjusted this leaf (only consider convergence/splitting for altered leaves)
	touchLeaf(Node).

touchLeaf(ID) :-
	affectedLeavesThisConfig(List),
	member(ID, List),
	!.
touchLeaf(ID) :-
	affectedLeavesThisConfig(List),
	retractall(affectedLeavesThisConfig(List)),
	append(List, [ID], New),
	assert(affectedLeavesThisConfig(New)),
	!.


updatePredictedQValue(LeafID, RewardValue, FutureValue) :-
	predicted_q_value(LeafID, CurrentQV, Count1),
	retractall(predicted_q_value(LeafID, _, _)),
	current_learning_rate_parameter(Gamma),
	Count2 is Count1 + 1,
	Alpha is (1/Count2),
	NewQValue is CurrentQV + (Alpha * (RewardValue + (Gamma * FutureValue) - CurrentQV)),
		%Val is CurrentValue + (Gamma * (RewardValue + FutureValue - CurrentValue)),
	asserta(predicted_q_value(LeafID, NewQValue, Count2)).

% ExampleStateList is a Prolog list of all fluents in the example's state (no longer current ones)
% TestAction is the example's action
% ExampleQValue is calculated from RewardValue and FutureValue by whatever means
updateExamplesStoredAtLeaf(LeafID, RemainderListWithAction, QV, ConfigID) :-
% Case 1: Example already exists, stored at leaf
	leaf_stored_example(LeafID, RemainderListWithAction, Count, SumOfQ, SumOfSquaredQ, ConfigID),
	!,
	Count2 is Count + 1,
	SumOfQ2 is SumOfQ + QV,
	SumOfSquaredQ2 is SumOfSquaredQ + (QV * QV),
	retractall(leaf_stored_example(LeafID, RemainderListWithAction, Count, SumOfQ, SumOfSquaredQ, ConfigID)),
	assert(leaf_stored_example(LeafID, RemainderListWithAction, Count2, SumOfQ2, SumOfSquaredQ2, ConfigID)),
	!.
updateExamplesStoredAtLeaf(LeafID, RemainderListWithAction, QV, ConfigID) :-
% Case 2: Create new example stored at leaf
	Sq is QV * QV,
	assert(leaf_stored_example(LeafID, RemainderListWithAction, 1, QV, Sq, ConfigID)),
	!.

initialise_children_stats_and_q(NodeID, NodeTest) :-
	child_y(NodeID, ChildYes),
	child_n(NodeID,  ChildNo),
	% Originally set predicted Q-value at a new leaf as the mean of those of the relevant examples.
	reward_neg(Neg),
	% 1. Look at examples responding 'yes' to new test, i.e., it is in their remainder. Count and take Q-values.
	findall( QV,
			(leaf_stored_example(NodeID, ExampleContent, Count, SumOfQ, SumOfSquaredQ, Confs), member(NodeTest, ExampleContent), QV is SumOfQ/Count),
			ListYes),
	length(ListYes, NumYes),
	sum_list(ListYes, SumQVYes),
	(	(NumYes == 0) -> 
		MeanYes = Neg
		;
		MeanYes is SumQVYes/NumYes
		),
	% 2. Look at 'no' examples likewise - just look for negation
	findall( QV,
			(leaf_stored_example(NodeID, ExampleContent, Count, SumOfQ, SumOfSquaredQ, Confs), not(member(NodeTest, ExampleContent)), QV is SumOfQ/Count),
			ListNo),
	length(ListNo, NumNo),
	sum_list(ListNo, SumQVNo),
	(	(NumNo == 0) -> 
		MeanNo = Neg
		;
		MeanNo is SumQVNo/NumNo
		),
	% 3. Calculate 'yes' and 'no' children's initial Q-values from those
	assert(predicted_q_value(ChildYes, MeanYes, NumYes)),
	assert(predicted_q_value(ChildNo,  MeanNo,  NumNo)),
	% 4. Move examples down to 'yes' and 'no' children appropriately, deleting them at the former leaf
	moveExamplesDown(NodeID, ChildYes, ChildNo, NodeTest),
	!.
	
moveExamplesDown(NodeID, ChildYes, ChildNo, NodeTest) :-
	leaf_stored_example(NodeID, ExampleContent, Count, SumOfQ, SumOfSquaredQ, Confs),
	member(NodeTest, ExampleContent),
	!,
	retractall(leaf_stored_example(NodeID, ExampleContent, Count, SumOfQ, SumOfSquaredQ, Confs)),
	select(NodeTest, ExampleContent, Remainder),
	asserta(leaf_stored_example(ChildYes, Remainder, Count, SumOfQ, SumOfSquaredQ, Confs)),
	!,
	moveExamplesDown(NodeID, ChildYes, ChildNo, NodeTest).
moveExamplesDown(NodeID, ChildYes, ChildNo, NodeTest) :-
	leaf_stored_example(NodeID, ExampleContent, Count, SumOfQ, SumOfSquaredQ, Confs),
	not(member(NodeTest, ExampleContent)),
	!,
	retractall(leaf_stored_example(NodeID, ExampleContent, Count, SumOfQ, SumOfSquaredQ, Confs)),
	asserta(leaf_stored_example(ChildNo, ExampleContent, Count, SumOfQ, SumOfSquaredQ, Confs)),
	!,
	moveExamplesDown(NodeID, ChildYes, ChildNo, NodeTest).
moveExamplesDown(_NodeID, _ChildYes, _ChildNo, _NodeTest) :- !.

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


calculateVarianceAtLeafNode(ID, Variance) :-
	enter_register('calculate_variance_at_leaf'),
	enter_register('calculate_variance_at_leaf:sums'),
	getVarianceComponents(ID, Count, SumQ, SumSquareQ),
	!,
	exit_register('calculate_variance_at_leaf:sums'),
	enter_register('calculate_variance_at_leaf:find'),
	findVariance(Count, SumQ, SumSquareQ, Variance),
	exit_register('calculate_variance_at_leaf:find'),
	exit_register('calculate_variance_at_leaf').

adjustEx(A, B, C) :-
	c1(X), New1 is X+A,
	c2(Y), New2 is Y+B,
	c3(Z), New3 is Z+C,
	retract(c1(_)), retract(c2(_)), retract(c3(_)),
	assert(c1(New1)), assert(c2(New2)), assert(c3(New3)).

getVarianceComponents(ID, CountTotal, SumQTotal, SumSquareQTotal) :-
	assert(c1(0)), assert(c2(0)), assert(c3(0)),
	forall(leaf_stored_example(ID, _, Coun, SumQ, SSqQ, _Confs), adjustEx(Coun, SumQ, SSqQ)),
	c1(CountTotal), c2(SumQTotal), c3(SumSquareQTotal),
	retract(c1(_)), retract(c2(_)), retract(c3(_)).

findVariance(Count, Sum, SumOfSquared, Variance) :-
	(((Count == 0);(Sum == 0)) -> Variance = 100000
	;
	(
	MeanQ is Sum/Count,
	SquaredMeanQ is MeanQ * MeanQ,
	MeanSquareSum is SumOfSquared/Count,
	Variance is abs(MeanSquareSum - SquaredMeanQ)
	)),
	((Variance < 0) -> (println_major('Error: Variance failed?\n'), trace) ; true).
	
calculateYesNoVariancesForTest(LeafID, ProspectiveTest, VarianceY, VarianceN) :-
	findall(	[CountYes, QYes, QSqYes],
				(leaf_stored_example(LeafID, ContentYes, CountYes, QYes, QSqYes, _ConfsYes), member(ProspectiveTest,ContentYes)),
				ListYes ),
	findall(	[CountNo, QNo, QSqNo],
				(leaf_stored_example(LeafID, ContentNo, CountNo, QNo, QSqNo, _ConfsNo), not(member(ProspectiveTest,ContentNo))),
				ListNo ),
	findall(A, member([A, _, _], ListYes), Count1), sum_list(Count1, CountY),
	findall(B, member([_, B, _], ListYes), SumQ1), 	sum_list(SumQ1, SumQY),
	findall(C, member([_, _, C], ListYes), SumSqQ1), sum_list(SumSqQ1, SumSquareQY),
	findall(D, member([D, _, _], ListNo), Count2), 	sum_list(Count2, CountN),
	findall(E, member([_, E, _], ListNo), SumQ2), 	sum_list(SumQ2, SumQN),
	findall(F, member([_, _, F], ListNo), SumSqQ2), sum_list(SumSqQ2, SumSquareQN),
	!,
	findVariance(CountY, SumQY, SumSquareQY, VarianceY),
	findVariance(CountN, SumQN, SumSquareQN, VarianceN).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

makeNewRoot :-
	retractall(root(_)),
	createNode(ID),
	assert(root(ID)),
	assert(predicted_q_value(ID, 0, 0)),
	domainGoalAction(DGA),
	splitNode(ID, action(DGA)).
	
createNode(ID) :-
	makeNewId(ID),
	assert(leaf(ID)).
	
makeNewId(ID) :-
	currentNodeId(ID),
	ID2 is ID + 1,
	retractall(currentNodeId(_)),
	asserta(currentNodeId(ID2)).

%	Find all leaves whose ancestor chain
%		(a) is a (partial) match for the current state, and
%		(b) has an action amongst their tests, and
%		(c)	that action is applicable in this state (i.e. is in ActionList).
%	Collect pairs <action, q-value at leaf>
%	Choose the best from the list and return that
pickBestByBinaryTreeEstimate(ActionList, BestAction) :-
	get_best_by_binary_tree_estimate(ActionList, BestAction, _).
	
get_best_by_binary_tree_estimate(ValidActionList, BestAction, BestValue) :-
	enter_register('ep_steps:getBestByBDT:getAQpairs'),
	getAllActionQPairs(ValidActionList, ListOfPairs),
	exit_register('ep_steps:getBestByBDT:getAQpairs'),
	enter_register('ep_steps:getBestByBDT:part2'),
	( (ListOfPairs == []) -> (random_member(BestAction, ValidActionList), reward_neg(BestValue))
		;
	(
		member([_,BestValue], ListOfPairs),
		not( (member([_,Y], ListOfPairs), Y > BestValue) ),
		findall(Act2, member([Act2,BestValue], ListOfPairs), NewList),
		random_member(BestAction, NewList)
	)
	),
	exit_register('ep_steps:getBestByBDT:part2').
	

% (If list is empty, assume default low/negative value)
getHighestQValueForAnyActionFromCurrentState([], N) :- reward_neg(N), !.
getHighestQValueForAnyActionFromCurrentState(ActionList, BestValue) :-
	rrl(true),
	!,
	get_best_by_binary_tree_estimate(ActionList, _, BestValue).

% Variant without RRL/trees, just Q-learning.
getHighestQValueForAnyActionFromCurrentState(ActionList, BestValue) :-
	rrl(false),
	!,
	findall(N, (currentState(N)), StateListUnsorted),
	sort(StateListUnsorted, StateInListForm),
	% All actions in action list are valid, but not all might have stored values. So if none do, return -1 or default.
	findall(Value, (qValueLearned(StateInListForm, InstantiatedAction, Value, _Count), member(InstantiatedAction, ActionList)), ListOfVals),
	(ListOfVals == []
	->
	(reward_neg(N), BestValue = N)
	;
	max_list(ListOfVals,BestValue)).
	
%%%%%

getAllActionQPairs(ValidActionList, ListOfPairs) :- 
	policySearch(ValidActionList),
	%
	findall([Action,Value], struc(_LeafID1,Action,Value), Triples),
	((member(X,Triples),member(Y,Triples),X\=Y,X=[Id,_,_],Y=[Id,_,_]) -> (println_major('Error in getAllActionQPairs'),trace,nl,print(X),nl,print(Y),nl) ; true),
	%
	findall([Action,Value], struc(_LeafID2,Action,Value), ListOfPairs),
	retractall(struc(_LeafID3,_Action,_QV)).

policySearch(_) :-
	root(ID),
	(not(test(ID, _)) ; leaf(ID)),
	!.
policySearch(ValidActionList) :-
	root(ID),
	!,
	enter_register('ep_steps:getBestByBDT:getAQpairs:policy'),
	recursiveBDTSearch(ValidActionList, [ ID ]),
	exit_register('ep_steps:getBestByBDT:getAQpairs:policy'),
	!.

recursiveBDTSearch([], _) :- !. % (This clause should not be reached)
recursiveBDTSearch(_, []) :- !. % Done when fringe is empty.
recursiveBDTSearch(_, [X|_]) :-
	var(X), trace.
recursiveBDTSearch(ValidActionList, [FringeElement|Tail]) :-
	FringeElement = [ID,Action], leaf(ID), % Valid: Look up Q-value and store it.
	predicted_q_value(ID, Value, _),
	assert(struc(ID, Action, Value)),
	!,
	recursiveBDTSearch(ValidActionList, Tail).
recursiveBDTSearch(ValidActionList, [FringeElement|Tail]) :-
	FringeElement \= [_,_], FringeElement = ID, leaf(ID), % No ancestor with a matching action, so remove it from the fringe and ignore it.
	!,
	recursiveBDTSearch(ValidActionList, Tail).
recursiveBDTSearch(ValidActionList, [FringeElement|Tail]) :-
	FringeElement = [ID,Action], not(leaf(ID)), % Internal node always has test - all that matters is that the new fringe entrant gets this ancestral 'Action' passed down.
	test(ID, action(_AnyAction)),
	!,
	% 1. Ignore 'yes' child
	% 2. Pass ancestral action down to 'no' child, adding it to fringe
	child_n(ID, ChildID),
	append(Tail, [[ChildID,Action]], NewFringe),
	!,
	recursiveBDTSearch(ValidActionList, NewFringe).
recursiveBDTSearch(ValidActionList, [FringeElement|Tail]) :-
	FringeElement = [ID,Action], not(leaf(ID)), % Internal node always has test - all that matters is that the new fringe entrant gets this ancestral 'Action' passed down.
	test(ID, Test), Test \= action(_), % NOT action
	(currentState(Test) -> child_y(ID, ChildID) ; child_n(ID, ChildID)),
	% 1. Pass ancestral action down to either 'yes' or 'no' child, adding it to fringe
	% 2. Ignore other child
	append(Tail, [[ChildID,Action]], NewFringe),
	!,
	recursiveBDTSearch(ValidActionList, NewFringe).
recursiveBDTSearch(ValidActionList, [FringeElement|Tail]) :-
	FringeElement \= [_,_], FringeElement = ID, not(leaf(ID)), % Internal node (always has test) with no Action to be passed down, UNLESS it's the test here.
	test(ID, action(Act)),
	!,
	% if on the permissible list, then its yes child inherits the new action and both children are added to fringe
	% else only no child is added to fringe
	child_y(ID, Yes),
	child_n(ID, No),
	(member(Act, ValidActionList) -> append(Tail, [[Yes,Act],No], NewFringe) ; append(Tail, [No], NewFringe)),
	!,
	recursiveBDTSearch(ValidActionList, NewFringe).
recursiveBDTSearch(ValidActionList, [FringeElement|Tail]) :-
	FringeElement \= [_,_], FringeElement = ID, not(leaf(ID)), % Internal node (always has test) with no Action to be passed down, UNLESS it's the test here.
	test(ID, Test), Test \= action(_), % NOT action
	(currentState(Test) -> child_y(ID, ChildID) ; child_n(ID, ChildID)),
	% 1. Pass ancestral action down to either 'yes' or 'no' child, adding it to fringe
	% 2. Ignore other child
	append(Tail, [ChildID], NewFringe),
	!,
	recursiveBDTSearch(ValidActionList, NewFringe).

	%%%%
	
getAllActionQPairs_SECONDTRY(ValidActionList, ListOfPairs) :-
	enter_register('ep_steps:getBestByBDT:?1'),
	findall(	[StateNeg, StatePos, Value],
				( leaf(ID), enter_register('ep_steps:getBestByBDT:?1-INNER'), enter_register('ep_steps:getBestByBDT:?1-getpath'), getStateActionInfoOnPath(ID, StateNeg, StatePos), exit_register('ep_steps:getBestByBDT:?1-getpath'), predicted_q_value(ID, Value, _), exit_register('ep_steps:getBestByBDT:?1-INNER') ),
				Triples ),
	((member(X,Triples),member(Y,Triples),X\=Y,X=[N,P,_],Y=[N,P,_]) -> (trace,print(X),nl,print(Y),nl) ; true),
	exit_register('ep_steps:getBestByBDT:?1'),
	% StateNeg or StatePos could contain: attr(_), action(_), etc
	% 1.
	enter_register('ep_steps:getBestByBDT:?2'),
	findall(	[Action, Value],
				( enter_register('ep_steps:getBestByBDT:?2-INNER'), member([Neg,Pos,Value],Triples), member(action(Action), Pos), member(Action, ValidActionList), enter_register('ep_steps:getBestByBDT:?2-chk'), allAreInCurrentStateOrActions(Pos), noneAreInCurrentState(Neg), exit_register('ep_steps:getBestByBDT:?2-chk'), exit_register('ep_steps:getBestByBDT:?2-INNER') ),
				ListOfPairs ),
	exit_register('ep_steps:getBestByBDT:?2'),
	% 2.
	((member(X,ListOfPairs),member(Y,ListOfPairs),X\=Y,X=[A,_],Y=[A,_]) -> (trace,print(X),nl,print(Y),nl) ; true).

noneAreInCurrentState([]).
noneAreInCurrentState([H|T]) :-	
	not(currentState(H)),
	noneAreInCurrentState(T).
	
getAllActionQPairs_ORIGINAL_VERSION(ValidActionList, ListOfPairs) :-
	findall(	[Action, Value],
				matchingAncestorChain(ValidActionList, Action, Value),
				ListOfPairs ).

matchingAncestorChain(ValidActionList, Action, Value) :-
	matchingAncestorChainX(ValidActionList, Action, Value)
	->
	true
	;
	fail.

matchingAncestorChainX(ValidActionList, Action, Value) :-
	enter_register('ep_steps:getBestByBDT:findall:aChain'),
	leaf(ID),
	getAncestorTests(ID, List),
	member(action(Action), List),
	member(Action, ValidActionList),
	enter_register('ep_steps:getBestByBDT:findall:aChain:all'),
	allAreInCurrentStateOrActions(List),
	exit_register('ep_steps:getBestByBDT:findall:aChain:all'),
	predicted_q_value(ID, Value, _),
	exit_register('ep_steps:getBestByBDT:findall:aChain').
	
/*	1. Pass in set AL
	2. Take any leaf
	3. Get set T of all its ancestors' tests
	4. Find any action in T
	5. Check it is in AL
	6. Check everything that is in T also currently holds in the world (or is an action)
*/
	
% Note a node is its own ancestor
getAncestorTests(Node, List) :-
	recursiveGetAncestorTests(Node,[],List).

% root & leaf
recursiveGetAncestorTests(Node, _, []) :-
	root(Node),
	leaf(Node),
	!.
% root & !leaf
recursiveGetAncestorTests(Node, WorkingList, ReturnList) :-
	root(Node), % but not a leaf...
	(test(Node, T) ; (println_major('recursiveGetAncestorTests 1 failed: no test\n'), trace, fail)),
	test(Node, T),
	!,
	append(WorkingList,[T],ReturnList).
% !root & leaf
recursiveGetAncestorTests(Node, WorkingList, ReturnList) :-
	parent(Node, Node2),
	leaf(Node),
	!,
	recursiveGetAncestorTests(Node2, WorkingList, ReturnList).
% !root & !leaf
recursiveGetAncestorTests(Node, WorkingList, ReturnList) :-
	parent(Node, Node2),
	(test(Node, T) ; (println_major('recursiveGetAncestorTests 2 failed: no test\n'), trace, fail)),
	!,
	append(WorkingList,[T],NewList),
	recursiveGetAncestorTests(Node2, NewList, ReturnList).

allAreInCurrentStateOrActions([]).
allAreInCurrentStateOrActions([A|B]) :-
	A = action(_),
	!,
	allAreInCurrentStateOrActions(B).
allAreInCurrentStateOrActions([A|B]) :-
	currentState(A),
	!,
	allAreInCurrentStateOrActions(B).

% Note there's, necessarily, no way to end up with two actions in the same path:
% once there is an action in the path from root to (current) leaf, then every test for any other action will never partition the examples, so the leaf will never split on that test.

% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % 
	
% For EACH current leaf node in the BDT, consider extending by splitting that leaf.
checkForTreeSplits :-
	affectedLeavesThisConfig(ListToTest),
	(is_set(ListToTest) -> true ; trace),
	!,
	checkListForTreeSplits(ListToTest),
	!.
	
checkListForTreeSplits([]) :-
	!.
checkListForTreeSplits([LeafID|Tail]) :-
	% If no variance, do nothing.
	calculateVarianceAtLeafNode(LeafID, TotalVariance),
	checkListForTreeSplits_inner([LeafID|Tail], TotalVariance).

% Final clause - catch case - not enough variance improvement to split at all
checkListForTreeSplits([_LeafID|Tail]) :-
	!,
	enter_register('tree_split_checking:clause3'),
	(no_split_count(CurrentCount), NewCount is CurrentCount+1, retractall(no_split_count(_)), assert(no_split_count(NewCount))),
	exit_register('tree_split_checking:clause3'),
	checkListForTreeSplits(Tail).

% % % % % %

checkListForTreeSplits_inner([_LeafID|Tail], TotalVariance) :-
	TotalVariance =< 0.0,
	!,
	enter_register('tree_split_checking:clause1'),
	(no_split_count(CurrentCount), NewCount is CurrentCount+1, retractall(no_split_count(_)), assert(no_split_count(NewCount))),
	exit_register('tree_split_checking:clause1'),
	checkListForTreeSplits(Tail).

checkListForTreeSplits_inner([LeafID|Tail], TotalVariance) :-
	enter_register('tree_split_checking:clause2'),
	% Get list of unique possible tests	
	findall(	Literal,
				(leaf_stored_example(LeafID, Content, _, _, _, _C), member(Literal, Content)),
				List1),
	sort(List1, List2),
		
	findall(	[ProspectiveTest, MeanReduction, YesReduction, NoReduction],
				(
					member(ProspectiveTest, List2),
					calculateYesNoVariancesForTest(LeafID, ProspectiveTest, VarianceYes, VarianceNo),
					% We already know TotalVariance > 0 otherwise the previous clause would have kicked in
					YesReduction is TotalVariance - VarianceYes,
					NoReduction is TotalVariance - VarianceNo,
					MeanReduction is (YesReduction + NoReduction)/2
				),
				ListOfTestPossibilities
				),
	% First, pick the lowest-variance test where there is some sufficient reduction in variance
		
	amountVarianceReductionRequired(AVRR),
	
	findall( Test,
		(
		member(SelectableTest, ListOfTestPossibilities),
		SelectableTest = [Test, MeanReduction, YesReduction, NoReduction],
		once((YesReduction > 0 ; NoReduction  > 0)),
		MeanReduction  > AVRR,
		not( (member(OtherTest, ListOfTestPossibilities), OtherTest \= SelectableTest, OtherTest = [_, OtherMean, _, _], OtherMean > MeanReduction) )
		),
			AllSelectableTests),
		
	% (Simply picking the first test alphabetically would cause problems)
	random_member(ChosenTest, AllSelectableTests),
	
	splitNode(LeafID, ChosenTest),
	
	affectedLeavesThisConfig(ListToTestThatNeedsRevising),
	select(LeafID, ListToTestThatNeedsRevising, NewAffectedLeafList),
	retractall(affectedLeavesThisConfig(ListToTestThatNeedsRevising)),
	asserta(affectedLeavesThisConfig(NewAffectedLeafList)),
	exit_register('tree_split_checking:clause2'),
	!,
	checkListForTreeSplits(Tail).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Helps to establish Q-value convergence by storing the Q-values for touched leaves
% At end of each episode, will find the currently highest-value leaves and check whether those values did not change by more than some small percentage over the last N episodes
storeEpisodicValueMaxes :-
	% 1. Get list of leaf nodes touched this config
	affectedLeavesThisConfig(List),
	% 2. Store current predicted Q-value for each of those nodes in episode_high_val
	episode_count(EC),
	storeEpisodicValues(EC, List).
	
storeEpisodicValues(_, []) :- !.
storeEpisodicValues(EC, [A|B]) :-
	not(leaf(A)), % Might no longer be a leaf if it has been split since it was added to the list.
	!,
	storeEpisodicValues(EC, B).
storeEpisodicValues(EC, [A|B]) :-
	predicted_q_value(A, Val, _),
	assert(episode_high_val(EC, A, Val)),
	storeEpisodicValues(EC, B).	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% To guarantee convergence to the true Q-value, slowly decrease the learning rate parameter 'gamma' (current_learning_rate_parameter) over time.
% Although in practice, it's often sufficient to simply use a small gamma.
% The method is: Each iteration/episode, current_learning_rate_parameter -= initial_learning_rate_parameter/(totalnumruns*2 +1).
% So eventually it reaches half.
updateLearningValue :-
	vary_learning_rate(false),
	!.
updateLearningValue :-
	vary_learning_rate(true),
	!,
	change_learning_rate.

change_learning_rate :-
	initial_num_of_episodes(A),
	initial_learning_rate_parameter(X),
	current_learning_rate_parameter(Y),
	Val1 is (A * 2) + 1, % Doubled for now so it simply narrows down to half the original
	Val2 is X / Val1,
	New is Y - Val2,
	retractall(current_learning_rate_parameter(_)),
	assert(current_learning_rate_parameter(New)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Term is either attr( ) or fluent( )
get_all_alternative_domain_tests(Term, ReturnList) :-
	findall(List,
			permitted_domain_test_alternatives(Term, List),
			UnflattenedList),
	flatten(UnflattenedList, FlatList),
	sort(FlatList, ReturnList),
	(ReturnList == [] -> (writef('Error: get_all_alternative_domain_tests failed due to bad argument.'), trace) ; true).

% Note that for fluents specifically, looser in terms of sharing a single argument in a single position.
% e.g. loc(x,y) will substitute both for loc(w,y) and loc(x,z)!
permitted_domain_test_alternatives(fluent(ContentTerm), ReturnList) :-
	functor(ContentTerm, ContentPred, SomeNumberOfArgs),
	SomeNumberOfArgs > 1,
	!,
	findall(	N, % Find all static attributes following the same pattern as the input argument, with same first argument, which are valid, and different to the input argument.
				(functor(N2, ContentPred, SomeNumberOfArgs), arg(SomeInt, ContentTerm, FirstArg), arg(SomeInt, N2, FirstArg), N = fluent(N2), valid(N), N2 \= ContentTerm),
				ReturnList
	).
permitted_domain_test_alternatives(attr(ContentTerm), ReturnList) :-
	functor(ContentTerm, ContentPred, SomeNumberOfArgs),
	SomeNumberOfArgs > 1,
	arg(1, ContentTerm, FirstArg),
	!,
	findall(	N, % Find all static attributes following the same pattern as the input argument, with same first argument, which are valid, and different to the input argument.
				(functor(N2, ContentPred, SomeNumberOfArgs), arg(1, N2, FirstArg), N = attr(N2), valid(N), N2 \= ContentTerm),
				ReturnList
	).
