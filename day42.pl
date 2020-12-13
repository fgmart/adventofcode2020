/*
  Advent of Code 2020 Day 4, Part 2

  PROLOG!

  Fred Martin, fredm@alum.mit.edu
  Sun Dec 13 18:01:31 2020

  input looks like:
  ecl:gry pid:860033327 eyr:2020 hcl:#fffffd

  cheating: added a blank line to the end of the input file;
  my code needs that to form the last passport...

  evaluate "get_answer(Answer)." to see result.

  plan for second problem:
  - Passport will now be list-of-lists, where each sublist is a 
    field-value pair. Get rid of "full_passport_to_passport" predicate.
  - Make a predicate for validating each field that needs it.
  - Still have "required_categories" fact declaration (rename to 
    "require-fields"); still have to iterate through list to make sure all
    required fields are present, *and* that they pass validation.

  intermediate step: get old solution working with new representation of
  Passport.

 */


/* not needed for version 8.2.1. */
/* :- use_module(library(pio)). */

/* file input */
/* I don't really understand this code; */
/* Use it with "phrase_from_file(lines(Ls), "INPUT_FN")" */
lines([])           --> call(eos), !.
lines([Line|Lines]) --> line(Line), lines(Lines).
eos([], []).
line([])     --> ( "\n" ; call(eos) ), !.
line([L|Ls]) --> [L], line(Ls).


/* required_fields */
/* "fact declaration" */
required_fields(["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]).

/* a "field" is a single category/value pair; e.g. "ecl:gry" */
parse_field(Str, KVP) :-
    split_string(Str, ":", "", KVP).

/* a line is a series of fields separated by spaces.
   first, split them into a list of individual string-fields.
   then, map the parse_field goal over them.

   produces "Full_Passport", which is a list of lists, where
   each sublist is a category/value pair.

?- parse_passport_str("iyr:2011 ecl:brn hgt:59in hcl:#cfa07d eyr:2025 pid:166559648", Full_Passport).
Full_Passport = [["iyr", "2011"], ["ecl", "brn"], ["hgt", "59in"], ["hcl", "#cfa07d"], ["eyr", "2025"], ["pid", "166559648"]].
*/
parse_passport_str(Str, Full_Passport) :-
    split_string(Str, " ", "", X),
    maplist(parse_field, X, Full_Passport).


car([X|_],X). /* LOL map car onto the list */

/* thank you Mike Brayshaw, Paul Brna, and Tamsin Treasure-Jones
for this list-search predicate, from:
https://www.doc.gold.ac.uk/~mas02gw/prolog_tutorial/prologpages/lists.html
*/
in_list(Item,[Item|_]).
in_list(Item,[_|Tail]) :-
    in_list(Item,Tail).

/* 
  for each item in required_fields, try to find it in current passport.
  if walk all the way down required list and list is empty, then true.
  otherwise, fail.
  not sure how to fail early. -- just don't continue.
  maybe is there if/then in Prolog? -- yes, it's the -> operator.
*/

test_passport([], _). /* this represents success. */
test_passport([Head|Tail], Passport) :-
    /* Head is required field. */
    /* Need to walk through Passport and produce sublist */
    /*   where field of that sublist matches head. */
    (  find_field_in_passport(Head, Passport, Passport_Pair),
       passport_pair_is_valid(Passport_Pair)
    -> test_passport(Tail, Passport)
    ;  !, fail
    ).
/*
  interface predicate:
  bind required_fields to X; then get going with full list of categories.
*/
test_passport(Passport) :-
    required_fields(X), 
    test_passport(X, Passport).

passport_pair_is_valid([Field|ValueList]) :-
    [Value|_] = ValueList,
    (Field == "byr" ->
	 validate_birth_year(Value);
     Field == "iyr" ->
	 validate_issue_year(Value);
     Field == "eyr" ->
	 validate_expiration_year(Value);
     Field == "hgt" ->
	 validate_height(Value);
     Field == "hcl" ->
	 validate_hair_color(Value);
     Field == "ecl" ->
	 validate_eye_color(Value);
     Field == "pid" ->
	 validate_passport_id(Value)
     ).

validate_birth_year(Value) :-
    string_length(Value, Length),
    Length == 4,
    number_string(Val_Num, Value),
    Val_Num >= 1920,
    Val_Num =< 2002.

validate_issue_year(Value) :- 
    string_length(Value, Length),
    Length == 4,
    number_string(Val_Num, Value),
    Val_Num >= 2010,
    Val_Num =< 2020.

validate_expiration_year(Value) :- 
    string_length(Value, Length),
    Length == 4,
    number_string(Val_Num, Value),
    Val_Num >= 2020,
    Val_Num =< 2030.

validate_height(Value) :-
    (string_concat(Cm, "cm", Value) ->
	 number_string(Cm_Num, Cm),
	 Cm_Num >= 150,
	 Cm_Num =< 193;
     string_concat(Inches, "in", Value) ->
	 number_string(Inches_Num, Inches),
	 Inches_Num >= 59,
	 Inches_Num =< 76).

/* thanks 
stackoverflow.com/questions/51120481/convert-hexadecimal-string-to-integer
*/

parse_hex(H, N) :-
    atom_concat('0x', H, HexaAtom),
    atom_codes(HexaAtom, HexaCodes),
    number_codes(N, HexaCodes).

validate_hair_color(Value) :-
    string_concat("#", Hex_String, Value),
    parse_hex(Hex_String, _). /* don't care about actual value... */

validate_eye_color(Value) :-
    Value == "amb" ;
    Value == "blu";
    Value == "brn";
    Value == "gry";
    Value == "grn";
    Value == "hzl";
    Value == "oth".

validate_passport_id(Value) :-
    string_length(Value, Length),
    Length == 9,
    number_string(_, Value).
    

/* looks like this predicate will be different, because the            */
/* have to figure out how to terminate a recursion early with success. */

/* base case of Passport being empty represents failure; cut and fail. */
find_field_in_passport(_, [], _) :-
    !, fail.
find_field_in_passport(Field, [Passport_Head|Passport_Tail], Passport_Pair) :-
    [Passport_Field|_] = Passport_Head,
    (  Passport_Field == Field
    -> Passport_Pair = Passport_Head, !
    ;  find_field_in_passport(Field, Passport_Tail, Passport_Pair)
    ).

/*
  I think we're ready for the main loop:
  read in the file.
  for each line, convert to "Passport" form and test it.
  recurse with an increased count if successful.
  recurse without increasing the count if not.
  base case just reports the count.

  I think reading the file gets us a list of lines, 
  so can write a predicate that ends when the list is empty.
  and gets kicked off with count being zero.

*/

count_passports([], Total, Total).  /* end of recursion; */
count_passports([Passport|Rest], Sum, Total) :-
    Sum1 is Sum + 1,
    test_passport(Passport) -> count_passports(Rest, Sum1, Total)
    ; count_passports(Rest, Sum, Total).
count_passports(Passports, Total) :-
    count_passports(Passports, 0, Total).
    
/* 
  Remaining is to read in the source file,
  map over it the predicate to convert to a passport,
  and hand off to above.
*/

/* phrase_from_file(lines(Ls), 'test.txt'). */
/* this binds Ls to a list of list of chars */
/* I suppose map the char to string operation over it to get list of strs? */

/*
   OK here's converting one list-of-chars into a string:
phrase_from_file(lines(Ls), "day4-test.txt"), [First|Rest]=Ls, string_chars(Yeah, First).
Ls = [[101, 99, 108, 58, 103, 114, 121, 32|...], [98, 121, 114, 58, 49, 57, 51|...], [], [105, 121, 114, 58, 50|...], [104, 99, 108, 58|...], [], [104, 99|...], [101|...], [...|...]|...],
First = [101, 99, 108, 58, 103, 114, 121, 32, 112|...],
Rest = [[98, 121, 114, 58, 49, 57, 51, 55|...], [], [105, 121, 114, 58, 50, 48|...], [104, 99, 108, 58, 35|...], [], [104, 99, 108|...], [101, 121|...], [101|...], [...|...]|...],
Yeah = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd".

Yeah that was fun. Maybe it's possible to do this when the file
is being read in?

I think I really don't understand how the "phrase_from_file" thing works.
Will be easier to write a recursive function that walks down and rebuilds
as string the thing one item at a time.

*/

char_lists_to_str_list([], Result, Result). /* yay we're done */
char_lists_to_str_list([Head|Tail], So_Far_List, Result) :-
    string_chars(Str, Head),
    append(So_Far_List, [Str], Updated_List),
    char_lists_to_str_list(Tail, Updated_List, Result).
char_lists_to_str_list(Char_Lists, Result) :-
    char_lists_to_str_list(Char_Lists, [], Result).

/* spent a half hour debugging this when there was only one "Result" var
   like it would be in Scheme.
   just figured out why we need two vars named Result in the interface.
   it binds the answer (in the first "Result") to the output variable!
*/

/*
  midway through debugging the predicate above,
  discovered a whole passport is not on one line.
  have to figure out how to find blank lines as separators, ugh.

  ok, how is this going to work?

  input is a list of strs 
  output is also a list of strs, but..
    we need to carry a passport-in-progress as we build up the output
    each time recurse, we're appending the head of the main list to the
      passport-in-progress
    when we've got an empty string, then we have a complete passport
      and we put it on the output list
  so we've got these arguments -- there are lots :(
    to-do 
    passport-in-process
    done
    result variable to bind to done at the very end
  maybe not too bad

exercise with:
phrase_from_file(lines(Ls), "day4-test.txt"), char_lists_to_str_list(Ls,
Result), make_passports(Result, Result2).


*/

make_passports_strlist([], _, Result, Result).
make_passports_strlist([Head|Rest], Passport_In_Process, Done, Result) :-
    Head == "" ->
	/* strip extra space at end of passport, then */
	/* add Passport_In_Process to Done, initialize empty PiP */
	string_concat(Space_Removed_PP, " ", Passport_In_Process),
        append(Done, [Space_Removed_PP], New_Done),	
	make_passports_strlist(Rest, "", New_Done, Result)
    ;
    /* add space to line that's read in, then */
    /* build PiP with new line at end of PiP, using strcat */
        string_concat(Head, " ", New_Head),
        string_concat(Passport_In_Process, New_Head, Updated_Passport),
        make_passports_strlist(Rest, Updated_Passport, Done, Result).
make_passports_strlist(Todo, Result) :-
    make_passports_strlist(Todo, "", [], Result).

	
/* that took a lot longer than i expected. */
/* didn't realize I was reversing the lists, because I was reversing them 2x */
/* LOL I could remove the appends since they're inefficient and i am doing */
/* two reversals which restores the order :) */
/* had to cheat and add a blank space at end of the file */

/* 
so now, I think all that's left is to build a recursive predicate to:
  
  walk through passports_strlist
  convert each passport-str to a passport (parse_passport_str; 
    full_passport_to_passport)
  test the passport and count ones that pass the test
*/
process_and_count([], Result, Result).
process_and_count([Head|Tail], Count, Result) :-
    parse_passport_str(Head, Passport),
    test_passport(Passport)
    ->
	Count1 is Count + 1,
	process_and_count(Tail, Count1, Result)
    ;
        process_and_count(Tail, Count, Result).
process_and_count(Passports_Strlist, Result) :-
    process_and_count(Passports_Strlist, 0, Result).

/* get the answer */

get_answer(Answer) :-
    phrase_from_file(lines(Ls), "day4-input.txt"),
    char_lists_to_str_list(Ls, Result),
    make_passports_strlist(Result, Result2),
    process_and_count(Result2, Answer).
