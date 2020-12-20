/*
  Advent of Code 2020 Day 5, Part 1

  PROLOG!

  Fred Martin, fredm@alum.mit.edu
  Dec 19, 2020

  run "get_answer(Answer)." 

BFFFBBFRRR: row 70, column 7, seat ID 567.
FFFBBBFRRR: row 14, column 7, seat ID 119.
BBFFBBFRLL: row 102, column 4, seat ID 820.

seat ID is row * 8 + column.

maybe to string, row 
string, column
row, column, seat relations?
*/

row_column_seat(Seat_Str, Seat) :-
    row_from_seat_str(Seat_Str, Row),
    col_from_seat_str(Seat_Str, Col),    
    Seat is Row * 8 + Col.

row_from_seat_str(Seat_Str, Row) :-
    sub_string(Seat_Str, 0, 7, _, Row_Str),
    seat_str_to_val(Row_Str, Row).

col_from_seat_str(Seat_Str, Col) :-
    sub_string(Seat_Str, 7, 3, 0, Col_Str),
    seat_str_to_val(Col_Str, Col).

/* cvt from 7-char seat-str to row */
/* adapted this predicate to do the seat ID */
/* give it a str of FBs or RLs and it will work */

seat_str_to_val(Seat_Str, Val) :- seat_str_to_val(Seat_Str, 0, Val).
seat_str_to_val("", Val, Val).
seat_str_to_val(Seat_Str, Ans, Val) :-
    /* F is 0, B is 1 */
    /* L is 0, R is 1 */
    string_code(1, Seat_Str, Code), /* code is ASCII of first char */
    string_length(Seat_Str, Str_Length),
    Rest_Len is Str_Length - 1, /* need this to fork off rest of string */
    sub_string(Seat_Str, 1, Rest_Len, 0, Rest_Str),
    (  (Code == 66 ; Code == 82)
    -> New_Ans is Ans * 2 + 1
    ;  New_Ans is Ans * 2),
    seat_str_to_val(Rest_Str, New_Ans, Val).
    
lines([])           --> call(eos), !.
lines([Line|Lines]) --> line(Line), lines(Lines).
eos([], []).
line([])     --> ( "\n" ; call(eos) ), !.
line([L|Ls]) --> [L], line(Ls).

char_lists_to_str_list([], Result, Result). /* yay we're done */
char_lists_to_str_list([Head|Tail], So_Far_List, Result) :-
    string_chars(Str, Head),
    append(So_Far_List, [Str], Updated_List),
    char_lists_to_str_list(Tail, Updated_List, Result).
char_lists_to_str_list(Char_Lists, Result) :-
    char_lists_to_str_list(Char_Lists, [], Result).

process_and_max(Str_List, Ans) :- process_and_max(Str_List, 0, Ans).
process_and_max([], Ans, Ans).
process_and_max([Head|Tail], Max, Ans) :-
    row_column_seat(Head, Seat),
    (  Seat > Max
    -> process_and_max(Tail, Seat, Ans)
    ;  process_and_max(Tail, Max, Ans)
    ).

get_answer(Answer) :-
    phrase_from_file(lines(Ls), "day5-input.txt"),
    char_lists_to_str_list(Ls, Result),
    process_and_max(Result, Answer).
