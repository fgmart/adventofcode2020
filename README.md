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



