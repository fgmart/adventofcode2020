# adventofcode2020

Fred's solutions for adventofcode.com/2020


## Days 1 and 2: Racket

I started in my favorite language, Racket!

## Day 3: JavaScript

I thought it would be more fun if I choose a new language very day.

I never really did anything in JavaScript before, so I installed `node` and starting playing.

I am impressed with the package management system. That's cool.

## Day 4: Prolog

I've tried to learn Prolog before, but I never really did anything in it.

Well, I figured out that I actually understand a lot of the ideas in Prolog already. It has pattern matching like Haskell and recursion like Scheme/Lisp/Racket.

It is amazing and cool the way parameters are really variables to be unified, and predicates can go in every which direction.

E.g. for string concatenation, let's concatenate `"foo"`and `"bar"` to get `"foobar"`:

    ?- string_concat("foo", "bar", X).
    X = "foobar".`
    
Not too exciting. But you can also ask, "What concatenates with `"bar"` to make `"foobar"` ?:
?- string_concat(X, "bar", "foobar").
X = "foo".
doing
    ?- string_concat(X, "bar", "foobar").
    X = "foo".
    
The big idea with Prolog is that instead of functions which return values, you have predicates which unify to answers.

So you supply a variable to the predicate, and the predicate fills in answers which make things true (or the predicate is false).

## Days 5 and 6: Prolog.

I decided to keep going with Prolog rather than start a new language because I was just starting to feel comfortable in it.

I was realizing that I'm not really writing Prolog-y code, where the system does backtracking and really searches for answers. I've been writing recursive code with a clear base-case (list is empty, meaning I'm done).

I was going to move on to my next language -- Julia -- and now I see Day 6 and maybe this is a problem where Prolog can shine!

So staying in Prolog for one more day.

## Day 7, Part 1: Prolog.

Indeed, this problem of determining which bags may hold a given bag is tailor-made for Prolog.

After defining all the facts, the solution is had just asking Prolog for the answer-set:

    setof(X, contains(X, 'shiny gold'), Z),

meaning, find all things `X` that may contain the `shiny gold` bag, and then give me the set of unique ones in a list `Z`.

It took me a while to get here. 

First I was defining the facts as a bag holding a list of bags. I had a predicate that would test if the list matched (e.g., contained one bright white and two muted yellows).

Then I thought of just having the list in alphabetical order, and had rules like this:

    contains(light_red, [bright_white, muted_yellow, muted_yellow]).

My breakthrough was re-reading a Prolog "family relations" tutorial for the nth time ([this one](http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse49)). Then I realized that it should just be a series of rules, like this:

    holds(light_red, bright_white). 
    holds(light_red, muted_yellow).

and I could use the `setof` predicate above to get the answer.

(I deliberately ignored the counts, since these don't matter for Part 1.)

The final step was figuring out how to define facts at runtime, with a combination of this directive in the source file:

    :- dynamic holds/2.
    
and using the `assertz` predicate to define facts.

After that it was just work to write the code to parse the input :)

And, I discovered that in the Prolog REPL, after you assert a fact, it stays around until you restart!

    



