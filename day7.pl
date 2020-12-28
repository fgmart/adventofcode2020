/* 
  Advent of Code 
  Day 7, Part 1
  Fred Martin, fredm@alum.mit.edu
  Mon Dec 28 13:07:01 2020

  run:
  doit("day7-input.txt", A).

  This is the cleaned-up version, which in Prolog is simple:
  * read the input and assert the facts
  * ask Prolog the answer (which bags may hold 'shiny gold').

  The messy version shows all the work I did getting here.

*/

/* these are defined at runtime with assertz */
:- dynamic holds/2.

/* the recursive relation of bags inside of bags */
contains(X, Y) :- holds(X, Y).
contains(X, Y) :-
    holds(X, Z),
    contains(Z, Y).

/* split the string into the holding bag and a str with the rest */
holder_value(String, Holder, Value) :-
    sub_string(String, Before, _, After, " bags contain "), !,
    sub_string(String, 0, Before, _, HolderString),
    atom_string(Holder, HolderString),
    sub_string(String, _, After, 0, Value).

/* given a holding bag and an individual held bag descr str, assert it */
assert_holder_bagstr(Holder_Atom, BagStr) :-
    split_string(BagStr, " ", "", BagStr_List),
    length(BagStr_List, Len),
    (
	Len =:= 4
    ->  [_ /* Num_Bags */, Bag_Adj, Bag_Color, _] = BagStr_List,
	string_concat(Bag_Adj, " ", Bag_Adj2),
	string_concat(Bag_Adj2, Bag_Color, Bag_Name),
	atom_string(Bag_Atom, Bag_Name),
	assertz(holds(Holder_Atom, Bag_Atom))
    ;  	true
    ).

process_bags(_, []).
process_bags(Holder_Atom, [H|T]) :-
    assert_holder_bagstr(Holder_Atom, H),
    process_bags(Holder_Atom, T).

process_line(Char_Line) :-
    string_chars(Str, Char_Line),
    holder_value(Str, Holder_Atom, BagStr),
    split_string(BagStr, ",", " ", BagStr_List),
    process_bags(Holder_Atom, BagStr_List).

process_lines([]).
process_lines([H|T]) :-
    process_line(H),
    process_lines(T).

lines([])           --> call(eos), !.
lines([Line|Lines]) --> line(Line), lines(Lines).
eos([], []).
line([])     --> ( "\n" ; call(eos) ), !.
line([L|Ls]) --> [L], line(Ls).

doit(Fn, Ans) :-
    phrase_from_file(lines(Ls), Fn),
    process_lines(Ls), 
    setof(X, contains(X, 'shiny gold'), Z),
    length(Z, Ans).
