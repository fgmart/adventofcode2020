/*
    Advent of Code Day 6 Part 2

    Fred Martin, fredm@alum.mit.edu
    Fri Dec 25 11:08:45 2020

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
sum_answers([Group|Rest_Groups], Sum, Result) :-
    questions_answered(Group, Num),
    New_Sum is Sum + Num,
    sum_answers(Rest_Groups, New_Sum, Result).
sum_answers(List_O_Lists, Result) :-
    sum_answers(List_O_Lists, 0, Result).


/* 
   Group is a list of lists
   Each list is a Person - that Person has a list of Qs that they said yes
   If Q is not in Person-list -- short-circuit and fail.
   If Q is in Person-list - then recurse to the next person list.
   If reach end of People, then the Question was found in all Persons 
     and answer is yes

?- question_answered([[1,2], [2]], 2).
true.

?- question_answered([[1,2], [2]], 1).
false.

question_answered(Group, Question)
*/
question_answered([], _) :- true.
question_answered([Person|Rest], Question) :-
    (
	not(member(Question, Person))
    ->  
	fail
    ;
    	question_answered(Rest, Question)
    ).
    

/*
   for each Question, use question_answered to look through whole
   people list and see if everyone said yes to that question.
   then count results.

   need to walk down the Qs-List, keep Group constant.

?- questions_answered([[97]], Result).
Result = 1 .

?- questions_answered([[97], [97]], Result).
Result = 1 .

?- questions_answered([[97], [97], [98]], Result).
Result = 0 .

?- questions_answered([[97], [97], [97, 98]], Result).
Result = 1 .

?- questions_answered([[97, 98]], Result).
Result = 2 .

 questions_answered(Group, Questions, Count, Result)
*/
questions_answered(_, [], Result, Result).
questions_answered(Group, [Question|Rest_Qs], Count, Result) :-
    (
	question_answered(Group, Question)
    ->  New_Count is Count + 1,
	questions_answered(Group, Rest_Qs, New_Count, Result)
    ;   questions_answered(Group, Rest_Qs, Count, Result)
    ).
questions_answered(Group, Result) :-
    a_to_z(Questions),
    questions_answered(Group, Questions, 0, Result).


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

  

  Ls = [[97, 98, 99], [], [97], [98], [99], [], [97, 98], [97|...], []|...].

  For Part 2, have to turn this into a list of list of lists! E.g.:

       item 1            item 2             item 3
 [ [[97, 98, 99]], [[97], [98], [99]], [[97, 98], [97, 99]], ... ]

  OK. Now, want to walk through this list, 
    having previous line in hand, and then getting the top of the list
    peeking into the next item.
    If Remaining is empty-list, then we're done.
    If it's a list of 1+ items, add to current list, and recurse.
    If it's empty-list, add current list to output list,
     and recurse with popping the next item as the start of the "line in hand".

  group_lists(Line_In_Hand, Next, Remaining, Input, Result) 
  group_lists/2 is the interface.
  Result will be in reverse-order to the input. 

  Have to add a blank line at end of input file.

  This was sort of annoying and ugly because of the special-case
  initial condition and ending-condition.

*/

group_lists(Line_In_Hand, _, [], Input, Result) :-
    Result = [Line_In_Hand|Input].
group_lists(Line_In_Hand, Next, [Head_Rem|Tail_Rem], Input, Result) :-
    (
	length(Next, Len),
	Len > 0
    ->  New_List = [Next|Line_In_Hand],
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
    group_lists([Line_In_Hand], Next, Remainder, [], Result).
