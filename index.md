---
layout: default
---

# Prolog: Programming in logic

Wednesday 4 March 2015

Peter Ljunglöf

University of Gothenburg \\
Chalmers University of Technology


## A logic puzzle

Consider a group of ten friends who want to visit a new city somewhere in the world.  
They vote on seven potential destinations:

- Cairo, London, Beijing, Moscow, Mumbai, Nairobi, Jakarta

One city received four votes, two cities received two votes each, 
two cities received one vote each, and the remaining two cities received zero votes. 
How many votes did each of the cities receive?

- Beijing and Cairo got different numbers of votes.
- Moscow either got the most votes, or it got zero votes.
- Cairo got more votes than Jakarta did.
- Jakarta got one fewer votes than London or Beijing did.
- In the list of cities above, each of the two cities that got two votes 
  has a city that got no votes immediately to the left in the list.

(Puzzle borrowed from the 
[Prolog Wikibook](https://en.wikibooks.org/wiki/Prolog/Solving_a_Logic_Puzzle))
{:.place .b}

## A logic puzzle (distribution of votes)

How many votes did each of the cities receive?

One city received four votes, two cities received two votes each, 
two cities received one vote each, and the remaining two cities received zero votes. 

    Cities = [Cairo, London, Beijing, Moscow, Mumbai, Nairobi, Jakarta]
    permutate(Cities, [4,2,2,1,1,0,0])

## A logic puzzle (rule 1)

How many votes did each of the cities receive?

    Cities = [Cairo, London, Beijing, Moscow, Mumbai, Nairobi, Jakarta]
    permutate(Cities, [4,2,2,1,1,0,0])

Beijing and Cairo got different numbers of votes:

    (Cairo \= Beijing)

## A logic puzzle (rule 2)

How many votes did each of the cities receive?

    Cities = [Cairo, London, Beijing, Moscow, Mumbai, Nairobi, Jakarta]
    permutate(Cities, [4,2,2,1,1,0,0])
    (Cairo \= Beijing)

Moscow either got the most votes, or it got zero votes:

    (Moscow = 4 ; Moscow = 0)

## A logic puzzle (rule 3)

How many votes did each of the cities receive?

    Cities = [Cairo, London, Beijing, Moscow, Mumbai, Nairobi, Jakarta]
    permutate(Cities, [4,2,2,1,1,0,0])
    (Cairo \= Beijing)
    (Moscow = 4 ; Moscow = 0)

Cairo got more votes than Jakarta did:

    (Cairo > Jakarta)

## A logic puzzle (rule 4)

How many votes did each of the cities receive?

    Cities = [Cairo, London, Beijing, Moscow, Mumbai, Nairobi, Jakarta]
    permutate(Cities, [4,2,2,1,1,0,0])
    (Cairo \= Beijing)
    (Moscow = 4 ; Moscow = 0)
    (Cairo > Jakarta)

Jakarta got one fewer votes than London or Beijing did:

    (Jakarta is London-1 ; Jakarta is Beijing-1)

## A logic puzzle (rule 5)

How many votes did each of the cities receive?

    Cities = [Cairo, London, Beijing, Moscow, Mumbai, Nairobi, Jakarta]
    permutate(Cities, [4,2,2,1,1,0,0])
    (Cairo \= Beijing)
    (Moscow = 4 ; Moscow = 0)
    (Cairo > Jakarta)
    (Jakarta is London-1 ; Jakarta is Beijing-1)

In the list of cities, each of the two cities that got two votes has 
a city that got no votes immediately to the left in the list:

    zip(Cities, Zipped)
    count(Zipped, 0:2, 2)

## A logic puzzle (solution)

How many votes did each of the cities receive?

    votes_for(Cities) :-
        Cities = [Cairo, London, Beijing, Moscow, Mumbai, Nairobi, Jakarta],
        permutate(Cities, [4,2,2,1,1,0,0]),
        (Cairo \= Beijing),
        (Moscow = 4 ; Moscow = 0),
        (Cairo > Jakarta),
        (Jakarta is London-1 ; Jakarta is Beijing-1),
        zip(Cities, Zipped),
        count(Zipped, 0:2, 2).

How is `zip()`, `count()` and `permutate()` defined?

## Prolog as a logical database

A Prolog program is a logical database:

- a list of Horn clauses

A *Horn clause* is a disjunction of literals, where exactly one is positive:

- *¬p ∨ ¬q ∨ ¬r ∨ s*  &#x00a0; ⟺ &#x00a0;  *¬(p ∧ q ∧ r) ∨ s*  &#x00a0; ⟺ &#x00a0;  *p ∧ q ∧ r → s*

A *fact* is a Horn clause without negative literals:

- *p*  &#x00a0; ⟺ &#x00a0;  *ε → p*

To run a program you state a *query* for Prolog to answer: 
Prolog tries to prove it by finding a refutation of the negated query.
The *goal clause* is the negation of the query:

- *¬r ∨ ¬s ∨ ¬t*  &#x00a0; ⟺ &#x00a0;  *¬(r ∧ s ∧ t)*  &#x00a0; ⟺ &#x00a0;  *r ∧ s ∧ t → ⊥*

## SLD resolution

Given a goal clause:

- *¬(s ∧ t)*  &#x00a0; ⟺ &#x00a0;  *¬s ∨ ¬t*  &#x00a0; ⟺ &#x00a0;  *s ∧ t → ⊥*

SLD resolution selects one of the literals (say *s*) and a definite clause from the database:

- *¬p ∨ ¬q ∨ ¬r ∨ s*  &#x00a0; ⟺ &#x00a0;  *¬(p ∧ q ∧ r) ∨ s*  &#x00a0; ⟺ &#x00a0;  *p ∧ q ∧ r → s*

Now it replaces *s* in the goal by the antecedent of the definite clause:

- *¬p ∨ ¬q ∨ ¬r ∨ ¬t*  &#x00a0; ⟺ &#x00a0;  *(p ∧ q ∧ r ∧ t → ⊥)*

Let's say that we now select the literal *p*. 
This exists as a fact in the database
so we can replace *p* with the empty antecedent:

- *¬q ∨ ¬r ∨ ¬t*  &#x00a0; ⟺ &#x00a0;  *(q ∧ r ∧ t → ⊥)*

Etcetera, until we reach the empty goal clause. 
This means that we have proved it incorrect, 
which means that the query is correct.

## Depth-first, left-to-right SLD resolution

SLD resolution implicitly defines a search tree of alternative computations, \\
where the initial goal clause is its root.

How should we traverse this tree? Prolog uses the simplest strategy:

- select literals left-to-right
- depth-first search (backtracking)

This is not the best strategy for a theorem prover, but it is:

- memory-efficient (depth-first search)
- easy to implement (all you need is stacks)
- predictable

Predictability is very important for a general-purpose programming language.

## Prolog syntax, clauses

The Horn clause *p ∧ q ∧ r → s* is written like this in a Prolog database:

    s :- p, q, r.

(The `:-` operator can be seen as a backwards-pointing arrow, \\
and the comma is conjunction)

A fact is written like this:

    p.

Note that the order between clauses matter! 

- (clauses with the same head, that is)
- the order defines which clause will be selected first in SLD resolution

## Terms and variables

A term can be compound, atomic or a variable:

- a *compound term*, `f(a,b,..,d)`, consists of a *functor* `f` and the terms `a`, `b`, .., `d`
- an *atomic term* is a functor `b` or a number `123`
- a *variable*, `X`, `Cities`, .., is an as-yet unknown term

During the Prolog resolution process, the variables get instantiated via *unification*. \\
E.g., *f(a,Y)* can be unified with *f(X,b)*, producing a *unifier*:

    ?- f(a,Y) = f(X,b).
    X = a, Y = b

Unification can also fail. E.g., *f(a,X)* cannot be unified with *f(X,b)*:

    ?- f(a,Y) = f(X,b), X = Y.
    false

## Prolog syntax, terms

A Prolog program is a list of facts or clauses, and each fact or clause ends with a period.

Facts and clauses are terms, and terms can be of the following forms:

- a variable is written with a capital first letter
- a functor of atom must be started with a lowercase first letter, or be quoted
- a number is a number, is a number

In fact, everything is a term! Unary and binary operators are just syntactic sugar:

    ?- Expression = '+'(3, '*'(2, '-'(1, 6))).
    Expression = 3 + 2 * (1 - 6).

    ?- Clause = ':-'(sum('.'(Y,Ys),Sum), ','(sum(Ys,Sum0), 'is'(Sum,'+'(Y,Sum0)))).
    Clause = (sum([Y|Ys],Sum) :- sum(Ys,Sum0), Sum is Y+Sum0).

    ?- List = '.'(a, '.'(b, '.'(c, Rest))).
    List = [a, b, c | Rest].


## Definitions of common predicates

Disjunction:

    (P ; Q) :- P.
    (P ; Q) :- Q.

Equality / unification:

    (X = X).

List concatenation:

    append([], Ys, Ys).
    append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).

Binary search trees:

    bst_member(X, t(Left, X, Right)).
    bst_member(X, t(Left, Y, Right)) :- X < Y, bst_member(X, Left).
    bst_member(X, t(Left, Y, Right)) :- X > Y, bst_member(X, Right).

## Example runs

Example of backtracking, with several answers:

    ?- append(X, [b|Y], [a,b,c,d,b,c,a]).
    X = [a], Y = [c, d, b, c, a] ;
    X = [a, b, c, d], Y = [c, a] ;
    false.

Binary search trees cannot be used in the same way:

    ?- bst_member(3, t(t(nil, 1, nil), 2, t(nil, 3, nil))).
    true.

    ?- bst_member(X, t(t(nil, 1, nil), 2, t(nil, 3, nil))).
    X = 2 ;
    ERROR: </2: Arguments are not sufficiently instantiated

## Extra-logical predicates

Numeric calculations:

    ?- A is (2 + 4 + 6) / 3.
    A = 4.
    ?- 4 is (2 + 4 + X) / 3.
    ERROR: is/2: Arguments are not sufficiently instantiated

*Note*: `is` is *not* variable assignment!

    ?- N=3, N1 is N+1.
    N1 = 4.
    ?- N=3, N is N+1.
    false.

Testing terms:

    ?- atom(a), var(X), ground(f(a)), compound(f(a)), compound(f(X)).
    true.
    ?- atom(X) ; atom(f(a)) ; var(a) ; var(f(X)) ; ground(f(X)) ; compound(a) ; compound(X).
    false.


## Negation as failure

Negation in Prolog is written as the unary operator `\+`.
It is implemented as \\
"negation as failure", which works like this:

- whenever the interpreter stumbles upon a literal `\+ Goal`, it tries to prove `Goal` 
- if can find a solution to `Goal`, the negation fails and the system backtracks
- if there is no solution, the negation succeeds

If `Goal` contains unbound variables, there can be some strange results. \\
E.g., inequality `\=` is defined as the negation of equality:

    ?- X \= b, member(X, [a,b,c]).
    false.

    ?- member(X, [a,b,c]), X \= b.
    X = a ;
    X = c.

## If-then-else

If-then-else can be defined like this:

    (Test -> IfTrue ; IfFalse) :- Test, IfTrue.
    (Test -> IfTrue ; IfFalse) :- \+ Test, IfFalse.

However, this is inefficient, so the `-> ;` construction is primitive.
Negation can easily be defined in terms of if-then-else:

    (\+ Goal) :- Goal -> fail ; true.

*Note*: the semantics is not exactly like the definition above:

- the `Test` will only succeed *at most once*!
- this makes if-then-else a non-logical predicate, but also very useful

(Parenthesis: if-then-else is really defined in terms of the *cut*, `!`,
but I won't go through that extremely extra-logical predicate)

## Looping

Looping is done with recursion, just as in functional programming.
E.g., here is a "repeat-until" predicate:

    repeat_until(Test, Goal) :- Test -> true ; Goal, repeat_until(Test, Goal).

(It is important that we use if-then-else here, otherwise the `Goal` 
will be executed at the wrong places when backtracking).

However, this definition of a while loop doesn't work (***why?***):

    repeat_while(Test, Goal) :- Test -> Goal, repeat_while(Test, Goal) ; true.

For this reason we usually don't define generic looping predicates.
Instead we incorporate the loop directly in the program (as in most programming languages):

    take_first_n(N, List, Result) :-
        ( N > 0 -> List = [X|List0], Result = [X|Result0], N0 is N-1,
            take_first_n(N0, List0, Result0)
        ; Result = []
        ).

## Side-effects

Since Prolog has a clearly defined execution order, input/output etc. can be done via side-effects:

    print_all_members(List) :- member(X, List),
                               write(X), nl,
                               fail.
    print_all_members(_).

This kind of loop is called a "failure-driven loop". It always succeeds, binding no variables, 
but it performs some side-effect while traversing the list.

Here is another common looping idiom, making use of an infinitely succeeding predicate:

    read_positive_number(Number) :- ( repeat, 
                                      read(Number), 
                                      Number > 0
                                    ) -> true.
    repeat :- true ; repeat.

*Note*: `->` is necessary here, to stop `repeat` from repeating when the number is positive.

## The logic puzzle (zip, count and permutation)

Zipping a list into adjacing pairs:

    zip([_], []).
    zip([X,Y|Zs], [X:Y|XYs]) :- zip([Y|Zs], XYs).

Counting the number of given elements in a list:

    count([], _, 0).
    count([X|Xs], Y, N) :- count(Xs, Y, N0),
                           (X = Y -> N is N0+1 ; N = N0).

A list is a permutation of another list:

    permutate([], []).
    permutate(ListX, [X|Perm]) :- delete(X, ListX, List),
                                  permutate(List, Perm).

    delete(X, [X|Ys], Ys).
    delete(X, [Y|Ys], [Y|Zs]) :- delete(X, Ys, Zs).


## The logic puzzle (solution)

Finally, how many votes did each of the cities receive?

    votes_for(Cities) :-
        Cities = [Cairo, London, Beijing, Moscow, Mumbai, Nairobi, Jakarta],
        permutate(Cities, [4,2,2,1,1,0,0]),
        (Cairo \= Beijing),
        (Moscow = 4 ; Moscow = 0),
        (Cairo > Jakarta),
        (Jakarta is London-1 ; Jakarta is Beijing-1),
        zip(Cities, Zipped),
        count(Zipped, 0:2, 2).

Let's test it:

    ?- votes_for([Cairo, London, Beijing, Moscow, Mumbai, Nairobi, Jakarta]).
    Cairo = 4, London = 0, Beijing = 2, Moscow = 0, Mumbai = 2, Nairobi = 1, Jakarta = 1
    (...)
    Cairo = 4, London = 0, Beijing = 2, Moscow = 0, Mumbai = 2, Nairobi = 1, Jakarta = 1

*Note*: The same solution is repeated 8 times. ***Why?***

## Tail recursion

Recall the predicate for counting the number of given elements in a list:

    count([], _, 0).
    count([X|Xs], Y, N) :- count(Xs, Y, N0),
                           (X = Y -> N is N0+1 ; N = N0).

The predicate has to walk through the whole list, remembering all invocations
on the call stack, until it can start calculating. 
This consumes O(*n*) memory, which is inefficient. The standard solution 
(also in Haskell) is to use an accumulator to make the predicate tail-recursive:

    count(List, Y, Count) :- count_accum(List, Y, 0, Count).
    
    count_accum([], _, N, N).
    count_accum([X|Xs], Y, N0, N) :- (X = Y -> N1 is N0+1 ; N1 = N0),
                                     count_accum(Xs, Y, N1, N).

## Accumulation

Flattening an ordered binary search tree into a sorted list:

    flatten_tree(nil, []).
    flatten_tree(t(Left, X, Right), List) :- flatten_tree(Left, LeftList),
                                             flatten_tree(Right, RightList),
                                             append(LeftList, [X|RightList], List).

This gives an O(*n<sup>2</sup>*) behaviour. Better is to accumulate while traversing:

    flatten_tree(Tree, List) :- flatten_tree_accum(Tree, [], List).
    
    flatten_tree_accum(nil, List, List).
    flatten_tree_accum(t(Left, X, Right), In, Out) :-
            flatten_tree_accum(Left, In, Mid),
            flatten_tree_accum(Right, [X|Mid], Out).

But, wait:

    ?- flatten_tree(t(t(nil,1,nil),2,t(nil,3,t(nil,4,nil))), List).
    List = [4, 3, 2, 1].

## Accumulation (ctd)

The problem is that we can only add elements to the head (left) of a list.
So let's process the right subtree first instead:

    flatten_tree_accum(nil, List, List).
    flatten_tree_accum(t(Left, X, Right), In, Out) :-
            flatten_tree_accum(Right, In, Mid),
            flatten_tree_accum(Left, [X|Mid], Out).

Now, suppose that we have to process the left subtree first 
(e.g., because we're doing some side-effects while traversing).
No problem!

    flatten_tree_accum(nil, List, List).
    flatten_tree_accum(t(Left, X, Right), In, Out) :-
            flatten_tree_accum(Left, [X|Mid], Out),
            flatten_tree_accum(Right, In, Mid).

It still works, even though, the `In` list will be uninstantiated during traversal!

    ?- flatten_tree(t(t(nil,1,nil),2,t(nil,3,t(nil,4,nil))), L).
    L = [1, 2, 3, 4].

## Difference lists

A difference list in Prolog is a normal list, except the very end of it is a logic variable, 
paired with that variable. For example:

    [a,b,c|Xs]:Xs

What the..? Why should I want to use that kind of strange beast? \\
*Answer*: List concatenation!

    dl_append(In:Mid, Mid:Out, In:Out).

    ?- dl_append([a,b,c|Xs]:Xs, [x,y,z|Ys]:Ys, Result).
    Xs = [x,y,z|Ys],
    Result = [a,b,c,x,y,z|Ys]:Ys.

Since this uses a single unification to append you have O(1) instead of O(*n*) complexity.

- in Haskell you can use a function of type `[a] -> [a]` for the same idea, \\
  e.g., the type `ShowS` type which is the same as `String -> String`

## An English grammar

Let's try to implement this grammar for a tiny English fragment:

    sentence --> noun_phrase, verb_phrase.
    verb_phrase --> verb, noun_phrase.
    noun_phrase --> det, noun, ([] ; prepositional_phrase).
    prepositional_phrase --> prep, noun_phrase.
    det --> [the] ; [a].
    noun --> [woman] ; [man] ; [park] ; [telescope].
    verb --> [saw].
    prep --> [with] ; [in].

This grammar recognises sentences such as:

- *the woman saw the woman*
- *the woman saw a man in the park*
- *a man saw a woman in a telescope*
- *the woman saw the man in the park with a telescope*

## An English grammar (first attempt)

Here's our first attempt at a Prolog grammar:

    sentence(S) :- noun_phrase(NP), verb_phrase(VP), append(NP, VP, S).
    verb_phrase(VP) :- verb(Verb), noun_phrase(NP), append(Verb, NP, VP).
    noun_phrase(NP) :- det(Det), noun(Noun), append(Det, Noun, NP).
    noun_phrase(NP) :- det(Det), noun(Noun), append(Det, Noun, DetNoun),
        prepositional_phrase(PP), append(DetNoun, PP, NP).
    prepositional_phrase(PP) :- prep(Prep), noun_phrase(NP), append(Prep, NP, PP).

    det([the]). det([a]).
    noun([man]). noun([woman]). noun([park]). noun([telescope]).
    verb([saw]).
    prep([with]). prep([in]).

Wow! It works:

    ?- sentence([the, woman, saw, the, woman]).
    true

..or does it?

    ?- sentence([the, man, saw, the, woman]).

## An English grammar (second attempt)

The problem is that concatentation occurs too late, so let's put it earlier:

    sentence(S) :- append(NP, VP, S), noun_phrase(NP), verb_phrase(VP).
    verb_phrase(VP) :- append(Verb, NP, VP), verb(Verb), noun_phrase(NP).
    noun_phrase(NP) :- append(Det, Noun, NP), det(Det), noun(Noun).
    noun_phrase(NP) :- append(Det, NounPP, NP), det(Det), 
        append(Noun, PP, NounPP), noun(Noun), prepositional_phrase(PP).
    prepositional_phrase(PP) :- append(Prep, NP, PP), prep(Prep), noun_phrase(NP).

Now it works!

    ?- sentence([the, woman, saw, the, man, in, the, park, with, a, telescope]).
    true.

But there are still problems:

- it's complicated
- the grammar is one-way: it cannot be used for generating sentences
- the initial `append` splits the sentence blindly, which is inefficient

## An English grammar (difference lists)

So, let's use difference lists instead:

    sentence(S) :- dl_append(NP, VP, S), noun_phrase(NP), verb_phrase(VP).

which by the definition of `dl_append` is the same as:

    sentence(Sin:Sout) :- noun_phrase(Sin:Smid), verb_phrase(Smid:Sout).

so, we can write the grammar like this (splitting the difference list into two arguments):

    sentence(Sin, Sout) :- noun_phrase(Sin, Smid), verb_phrase(Smid, Sout).
    verb_phrase(Vin, Vout) :- verb(Vin, Vmid), noun_phrase(Vmid, Vout).
    noun_phrase(Nin, Nout) :- det(Nin, Nmid), noun(Nmid, Nlate),
        (Nlate = Nout ; prepositional_phrase(Nlate, Nout)).
    prepositional_phrase(Pin, Pout) :- prep(Pin, Pmid), noun_phrase(Pmid, Pout).

    det([the|X], X). det([a|X], X).
    noun([man|X], X). noun([woman|X], X). noun([park|X], X). noun([telescope|X], X).
    verb([saw|X], X).
    prep([with|X], X). prep([in|X], X).

## An English grammar (DCG notation)

As it happens, Prolog has syntactic sugar for difference list grammars:

    sentence --> noun_phrase, verb_phrase.
    verb_phrase --> verb, noun_phrase.
    noun_phrase --> det, noun, ([] ; prepositional_phrase).
    prepositional_phrase --> prep, noun_phrase.
    det --> [the] ; [a].
    noun --> [woman] ; [man] ; [park] ; [telescope].
    verb --> [saw].
    prep --> [with] ; [in].

When Prolog reads the grammar, it translates it directly into difference list predicates:

    ?- listing(noun_phrase), listing(det).
    noun_phrase(A, D) :- det(A, B), noun(B, C), (C=D ; prepositional_phrase(C, D)).
    noun(A, B) :- (A=[woman|B] ; A=[man|B] ; A=[park|B] ; A=[telescope|B]).

When we parse a sentence we have to "end" the difference list with a `[]`:

    ?- sentence([the, woman, saw, the, man, in, the, park, with, a, telescope], []).
    true.

## Accumulation using DCG

Remember the binary tree flattening predicate?

    flatten_tree_accum(nil, List, List).
    flatten_tree_accum(t(Left, X, Right), In, Out) :- flatten_tree_accum(Left, [X|Mid], Out),
                                                      flatten_tree_accum(Right, In, Mid).

Let's switch the order and the names between the accumuator arguments:

    flatten_tree_dcg(nil, List, List).
    flatten_tree_dcg(t(Left, X, Right), In, Out) :- flatten_tree_dcg(Left, In, XMid),
                                                    XMid = [X|Mid], 
                                                    flatten_tree_dcg(Right, Mid, Out).

Yes, the name says it all. It's really a DCG:

    flatten_tree_dcg(nil) --> [].
    flatten_tree_dcg(t(Left, X, Right)) --> flatten_tree_dcg(Left), [X], flatten_tree_dcg(Right).

We just have to be careful about the order of the arguments:

    ?- flatten_tree_dcg(t(t(nil,1,nil),2,t(nil,3,t(nil,4,nil))), L, []).
    L = [1, 2, 3, 4].



## Alternatives to Prolog

[Datalog](https://en.wikipedia.org/wiki/Datalog):

- restricted clauses, giving efficient (polynomial) algorithms for solving queries

[Constraint logic programming](https://en.wikipedia.org/wiki/Constraint_logic_programming):

- includes constraints over finite domains, rationals or reals

[XSB](http://xsb.sourceforge.net):

- tabled resolution, i.e., resolution with memoization of predicates

[λProlog](https://en.wikipedia.org/wiki/LambdaProlog):

- (restricted) higher-order unification, i.e., unification on lambda-terms

[Mercury](http://www.mercurylang.org), 
[Curry](http://curry-language.org/):

- attempts to merge functional and logic programming



