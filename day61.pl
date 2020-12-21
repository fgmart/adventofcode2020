/*
    Advent of Code Day 6

    Fred Martin, fredm@alum.mit.edu
    Mon Dec 21, 2020 6:40 PM

    Looks like we can concatenate all the letters for a single group
    together into one list, then go through that list looking for a's, 
    b's, c's, etc.
 
    Run "do_it(Answer)." to see the answer.

*/

/* start by generating a list of char codes, a thru z. */
a(Char_Code) :-
    Char_Code is "a".

z(Char_Code) :-
    Char_Code is "z".

/* insert the End char and decrement End */
/* works because we keep putting the new num at the front of the list,
   and we're counting down. */
a_to_z(List, Start, End, Result) :-
    (
	End < Start
    ->  Result = List
    ;   New_End is End - 1,
	a_to_z([End|List], Start, New_End, Result)
    ).
	
a_to_z(Result) :-
    a(A), z(Z),
    a_to_z([], A, Z, Result).

do_it(Answer) :-
    phrase_from_file(lines(Ls), "day6-input.txt"),
    group_lists(Ls, List_O_Lists),
    sum_answers(List_O_Lists, Answer).

/*  sum_answers(List_O_Lists, Sum, Result)  */
sum_answers([], Result, Result).
sum_answers([List|Rest_Lists], Sum, Result) :-
    questions_answered(List, Num),
    New_Sum is Sum + Num,
    sum_answers(Rest_Lists, New_Sum, Result).
sum_answers(List_O_Lists, Result) :-
    sum_answers(List_O_Lists, 0, Result).

/*  questions_answered(List, Questions, Count, Result)  */
questions_answered(_, [], Result, Result).
questions_answered(List, [Question|Rest_Qs], Count, Result) :-
    (
	member(Question, List)
    ->  New_Count is Count + 1,
	questions_answered(List, Rest_Qs, New_Count, Result)
    ;   questions_answered(List, Rest_Qs, Count, Result)
    ).
questions_answered(List, Result) :-
    a_to_z(Questions),
    questions_answered(List, Questions, 0, Result).

/* file input */
/* Use it with "phrase_from_file(lines(Ls), "INPUT_FN")" */
lines([])           --> call(eos), !.
lines([Line|Lines]) --> line(Line), lines(Lines).
eos([], []).
line([])     --> ( "\n" ; call(eos) ), !.
line([L|Ls]) --> [L], line(Ls).

/* 

  day6-test is:

abc

a
b
c

ab
ac

etc.

  phrase_from_file(lines(Ls), "day6-test.txt").

  Ls = [[97, 98, 99], [], [97], [98], [99], [], [97, 98], [97|...], []|...].

  OK. Now, want to walk through this list, 
    having previous line in hand, and then getting the top of the list
    peeking into the next item.
    If it's null, then we're done.
    If it's a list, append to current list, and recurse.
    If it's empty-list, add current list to output list,
     and recurse with popping the next item as the start of the "line in hand".

  group_lists(Line_In_Hand, Next, Remaining, Input, Result) 
  group_lists/2 is the interface.
  Result will be in reverse-order to the input. 

  Had to add a blank line at end of input file.

  This was sort of annoying and ugly because of the special-case
  initial condition and ending-condition.

*/

group_lists(Line_In_Hand, _, [], Input, Result) :-
    Result = [Line_In_Hand|Input].
group_lists(Line_In_Hand, Next, [Head_Rem|Tail_Rem], Input, Result) :-
    (
	length(Next, Len),
	Len > 0
    ->  append(Line_In_Hand, Next, New_List),
	group_lists(New_List, Head_Rem, Tail_Rem, Input, Result)
    ;
    	group_lists([], Head_Rem, Tail_Rem, [Line_In_Hand|Input], Result)
    ).
/* bootstrap it by picking off first two lines of Input */
/* need car and cdr from prior work */
car([X|_],X).
cdr([_|Y],Y).
group_lists(Input, Result) :-
    car(Input, Line_In_Hand),
    cdr(Input, Rem1),
    car(Rem1, Next),
    cdr(Rem1, Remainder),
    group_lists(Line_In_Hand, Next, Remainder, [], Result).
