/*
 * Name:        data_collector.pl
 * Author:      Ben Meadows
 * Date:        2018-02-19
 * Description: This file collects and collates information on false positives from batch testing trials using the system implementation in 'qRRL.pl'.
 */

:- dynamic results_filter/3.

% This function, after a batch of Q-RRL has been run, analyses the error data and for each level of filtering prints a row to the file noise_analysis.txt in the format
% X: Y Z
% where X is how many filters have been used, Y is the number of false positives overspecifying true target axioms, and Z is the number of 'genuine' false positives.
batch_collect_data :-
	output_file_error(ErrorFile),
	open(ErrorFile, read, Str),
	read_all(Str).

% Process the false positives stored in the output error file.
read_all(File) :-
    read_line_to_codes(File, Codes),
	Codes \= end_of_file,
    %split_string(Codes, " ", "", SubStringList),
	atom_string(ATOM, Codes),
	atomic_list_concat(SubStringList, ' ', ATOM),
	SubStringList = [_FilterString,NumberString,ActualString],
	attempt_to_refine_string(NumberString,ActualString),
	!,
	read_all(File).
read_all(_) :- printResult.

attempt_to_refine_string(NumberString,ActualString) :-
	split_string(NumberString, "]", "", [Head|_Tail]), atom_number(Head, ResultNum),
	refine_string(ResultNum,ActualString),
	!.
attempt_to_refine_string(_,_). % Always succeed, so never stop in the case where a line does not contain a square bracket

% Establish whether each 'false positive' axiom overfits a target axiom
refine_string(Index, Atom) :-
	atom_to_term(Atom, [Yes,No], _),
	(results_filter(Index,CY,CN)
	-> (CurrentYES = CY, CurrentNO = CN)
	; (CurrentYES = 0, CurrentNO = 0) ),
	sort(Yes,Y1),
	sort(No,N1),
	(
	(matchesWithOverfitting(Y1,N1))
	->
	(NewYes is CurrentYES +1, NewNo is CurrentNO)
	;
	(NewYes is CurrentYES, NewNo is CurrentNO +1)
	),
	retractall(results_filter(Index,CurrentYES,CurrentNO)),
	asserta(results_filter(Index,NewYes,NewNo)).

% Succeeds if the literals of a 'false positive' axiom overfits a target axiom
matchesWithOverfitting(Y1,N1) :-
	clause(domainAxiomClassifier([YesSubset, NoSubset], _ID),(domainGoalAction(Goal),_TailWithCut)), % Have to find the clause rather than call it because of the cut in the rule
	domainGoalAction(Goal),
	subset(YesSubset,Y1),
	subset(NoSubset,N1).

% Send results to 'noise_analysis.txt'
printResult :-
	open('noise_analysis.txt', write, Stream),
	printEach(0,11,Stream),
	close('noise_analysis.txt').

printEach(Current,Current,_) :- !.
printEach(Current,Finish,Stream) :-
	results_filter(Current,A,B),
	printRes(Stream,Current,A,B),
	New is Current + 1,
	printEach(New,Finish,Stream).

printRes(Stream,N,A,B) :-
	write(Stream, N), write(Stream, ': '),
	write(Stream, A), write(Stream, ' '),
	write(Stream, B), write(Stream, ' '),
	write(Stream, '\n').

