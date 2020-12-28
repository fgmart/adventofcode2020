/* 
  Advent of Code 
  Day 7, Part 1
  Fred Martin, fredm@alum.mit.edu
  Mon Dec 28 13:07:01 2020

  run:
  doit("day7-input.txt", A).

*/

/*
bag(light_red).
bag(dark_orange).
bag(bright_white).
bag(muted_yellow).
bag(shiny_gold).
bag(dark_olive).
bag(vibrant_plum).
bag(faded_blue).
bag(dotted_black).
*/

/* this works to TEST whether X is satisfied,
   but it won't generate X. */
/* replaced with alphabetized atom list */
/* a light_red bag holds a bright_white */
/*
holds(light_red, bright_white). 
holds(light_red, muted_yellow).
holds(dark_orange, bright_white).
holds(dark_orange, muted_yellow).
holds(bright_white, shiny_gold).
holds(muted_yellow, shiny_gold).
holds(muted_yellow, faded_blue).
holds(shiny_gold, dark_olive).
holds(shiny_gold, vibrant_plum).
holds(dark_olive, faded_blue).
holds(dark_olive, dotted_black).
holds(vibrant_plum, faded_blue).
holds(vibrant_plum, dotted_black).
*/

/* need this so we can load "holds" facts at runtime. */
:- dynamic holds/2.

contains(X, Y) :- holds(X, Y).
contains(X, Y) :-
    holds(X, Z),
    contains(Z, Y).

/*
  and then the answer is absurdly direct, just ask Prolog:

  ?- setof(X, contains(X, shiny_gold), Z).
  Z = [bright_white, dark_orange, light_red, muted_yellow].

  which translates to:
    find me the set of things X for which
    X can contain a shiny_gold bag
    and make them into a list Z.

  rest of the problem is pretty much reading in the input file
  parsing each line and transcribing it into rules.

  probably the "call" predicate is involved?

  no, it ends up being "assert".

*/

/* this will accept a Holder-bag atom and a list of named bags (strings)
   and then define the facts. */
define_holds(_, []).
define_holds(Holder, [H|T]) :-
    atom_string(Atom, H),
    assertz(holds(Holder, Atom)),
    define_holds(Holder, T).
    

/*
  OK now comes the task of reading in a line and extracting the rules.
 
  sample line:
  "light red bags contain 1 bright white bag, 2 muted yellow bags."

  looks like start off by splitting it at " bags contain "
  - to the left is the name of the container bag
  - to the right is a comma-separatd list of bags it contains.

*/

/* thanks SWI Prolog docs for this predicate :) */
/* holder = two words of holding bag (as atom)
   value = comma-sep list of holdee bags, terminated with period.
*/
holder_value(String, Holder, Value) :-
    sub_string(String, Before, _, After, " bags contain "), !,
    sub_string(String, 0, Before, _, HolderString),
    atom_string(Holder, HolderString),
    sub_string(String, _, After, 0, Value).

/* OK now have to parse the "Value" string, here it is in Z, and further
splitting it to get a list of four values in Q (need 2nd and 3rd):

?- holder_value("light red bags contain 1 bright white bag, 2 muted yellow bags.", Holder, Value), split_string(Value, ",", " ", Z), [H|T] = Z, split_string(H,
" ", "", Q).
Holder = 'light red',
Value = "1 bright white bag, 2 muted yellow bags.",
Z = ["1 bright white bag", "2 muted yellow bags."],
H = "1 bright white bag",
T = ["2 muted yellow bags."],
Q = ["1", "bright", "white", "bag"].

*/

/* parse BagStr (e.g., "1 bright white bag") and make assertion */
assert_holder_bagstr(Holder_Atom, BagStr) :-
    split_string(BagStr, " ", "", BagStr_List),
    length(BagStr_List, Len),
    (
	Len =:= 4
    ->  [_ /* Num_Bags */, Bag_Adj, Bag_Color, _] = BagStr_List,
	string_concat(Bag_Adj, " ", Bag_Adj2),
	string_concat(Bag_Adj2, Bag_Color, Bag_Name),
	atom_string(Bag_Atom, Bag_Name),
	assertz(holds(Holder_Atom, Bag_Atom)),
	write("Bag "), write(Holder_Atom),
	write(" holds "), write(Bag_Atom), write("\n")
    ;   write("Bag "), write(Holder_Atom), write(" holds no bags.\n"),
	true
    ).
    

/* Holder_Atom, BagStr_List

accept list like
Z = ["1 bright white bag", "2 muted yellow bags."]

and process */
process_bags(_, []).
process_bags(Holder_Atom, [H|T]) :-
    assert_holder_bagstr(Holder_Atom, H),
    process_bags(Holder_Atom, T).

/* line is a list of chars */
process_line(Char_Line) :-
    string_chars(Str, Char_Line),
    holder_value(Str, Holder_Atom, BagStr),
    split_string(BagStr, ",", " ", BagStr_List),
    process_bags(Holder_Atom, BagStr_List).

process_lines([]).
process_lines([H|T]) :-
    process_line(H),
    process_lines(T).

/* file input */
/* Use it with "phrase_from_file(lines(Ls), "INPUT_FN")" */
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
    


/*
this whole approach doesn't work; too literal.
replacing using info from 
http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse49

the key ideas being:
1. Don't need to keep track of how many bags hold a bag for this problem.
2. Use multiple predicates to state each bag's relation to its "children."
3. Define the recursive predicate:
   descend(X,Y)  :-  child(X,Z), descend(Z,Y).
4. Use findall, bagof, or setof as is best.

contains(light_red, [bright_white, muted_yellow, muted_yellow]).
contains(dark_orange, [bright_white, bright_white, bright_white,
		       muted_yellow, muted_yellow, muted_yellow, muted_yellow]).
contains(bright_white, [shiny_gold]).
contains(muted_yellow, [faded_blue, faded_blue, faded_blue, faded_blue,
			faded_blue, faded_blue, faded_blue, faded_blue,
			faded_blue,
			shiny_gold, shiny_gold]).
contains(shiny_gold, [dark_olive,
		      vibrant_plum, vibrant_plum]).
contains(dark_olive, [dotted_black, dotted_black, dotted_black, dotted_black,
		      faded_blue, faded_blue, faded_blue]).
contains(vibrant_plum, [dotted_black, dotted_black, dotted_black, dotted_black,
			faded_blue, faded_blue, faded_blue, faded_blue,
			faded_blue]).
contains(faded_blue, []).
contains(dotted_black, []).
*/

/*
count([], _, Result, Result).
count([H|T], Item, Sum, Result) :-
    (
	H == Item
    ->  New_Sum is Sum + 1,
	count(T, Item, New_Sum, Result)
    ;   count(T, Item, Sum, Result)
    ).
count(List, Item, Result) :-
    count(List, Item, 0, Result).
*/

