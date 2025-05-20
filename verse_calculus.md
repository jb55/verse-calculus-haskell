The Verse Calculus: a Core Calculus for Functional Logic
Programming

LENNART AUGUSTSSON, Epic Games, Sweden
JOACHIM BREITNER
KOEN CLAESSEN, Epic Games, Sweden
RANJIT JHALA, Epic Games, USA
SIMON PEYTON JONES, Epic Games, United Kingdom
OLIN SHIVERS, Epic Games, USA
TIM SWEENEY, Epic Games, USA

Functional logic languages have a rich literature, but it is tricky to give them a satisfying semantics. In this
paper we describe the Verse calculus, VC, a new core calculus for functional logical programming. Our main
contribution is to equip VC with a small-step rewrite semantics, so that we can reason about a VC program
in the same way as one does with lambda calculus; that is, by applying successive rewrites to it.

This draft paper describes our current thinking about Verse. It is very much a work in progress, not a finished
product. The broad outlines of the design are stable. However, the details of the rewrite rules may well change; we
think that the current rules are not confluent, in tiresome ways. (If you are knowledgeable about confluence proofs,
please talk to us!)

We are eager to enagage in a dialogue with the community. Please do write to us.

1 INTRODUCTION

Functional logic programming languages add expressiveness to functional programming by intro-
ducing logical variables, equality constraints among those variables, and choice to allow multiple
alternatives to be explored. Here is a tiny example:

∃x y z. x = ⟨y, 3⟩; x = ⟨2, z⟩; y

This expression introduces three logical (or existential) variables x, y, z, constrains them with two
equalities (x = ⟨y, 3⟩ and (x = ⟨2, z⟩), and finally returns y. The only solution to the two equalities
is y = 2, z = 3, and x = ⟨2, 3⟩; so the result of the whole expression is 2.

Functional logic programming has a long history and a rich literature. But it is somewhat tricky
for programmers to reason about functional logic programs: they must think about logical variables,
narrowing, backtracking, Horn clauses, resolution, and the like. This contrasts with functional
programming, where one can say “just apply rewrite rules, such as beta reduction, let-inlining, and
case-of-known-constructor.” We therefore seek a precise expression of functional logic programming
as a term-rewriting system, to give us both a formal semantics (via small-step reductions), and a
powerful set of equivalences that programmers can use to reason about their programs, and that
compilers can use to optimize them.

We make the following contributions in this paper. First, we describe a new core calculus for
functional logic programming, the Verse calculus or VC for short (Section 2 and 2.8). Like any

Authors’ addresses: Lennart Augustsson, Epic Games, Sweden, lennart.augustsson@epicgames.com; Joachim Breitner,
mail@joachim-breitner.de; Koen Claessen, Epic Games, Sweden, koen.claessen@epicgames.com; Ranjit Jhala, Epic Games,
USA, ranjit.jhala@epicgames.com; Simon Peyton Jones, Epic Games, United Kingdom, simonpj@epicgames.com; Olin
Shivers, Epic Games, USA, olin.shivers@epicgames.com; Tim Sweeney, Epic Games, USA, tim.sweeney@epicgames.com.

2023. 2475-1421/2023/1-ART1 $15.00
https://doi.org/

Proc. ACM Program. Lang., Vol. 1, No. PLDI, Article 1. Publication date: January 2023.

1:2Lennart Augustsson, Joachim Breitner, Koen Claessen, Ranjit Jhala, Simon Peyton Jones, Olin Shivers, and Tim Sweeney

functional logic language, VC supports logical variables, equalities, and choice, but it is distinctive
in several ways:

• VC natively supports higher-order programming, just like the lambda calculus. Indeed, every
lambda calculus program is a VC program. In contrast, most of the functional-logic literature
is rooted in a first-order world, and addresses higher-order features via an encoding called
defunctionalisation [Hanus 2013, 3.3].

• All functional logic languages have some notion of “flexible” vs. “rigid” variables, or “sus-
pending” vs. “narrowing” operations. VC offers a new way to address these notions, namely
the operators one (Section 2.5) and all (Section 2.6). This enables an elegant economy of
concepts: for example, there is just one equality (other languages have a suspending equality
and a narrowing equality), and conditional expressions are driven by failure rather than
booleans (Section 2.5).

• VC uses spatial choice, meaning that the choice operator behaves a bit like a data constructor:
it appears in normal forms (Section 3.5). This makes VC deterministic, unlike most functional
logic languages which are non-deterministic (Section 5.1). In VC choices are laid out in space,
in the syntax of the term, rather than in time.

As always with a calculus, the idea is that VC distills the essence of functional logic programming.
Each construct does just one thing, and VC cannot be made smaller without losing key features.
We believe that it is possible to use VC as the compilation target for a variety of functional logic
languages such as Curry [Hanus 2016] (although see Appendix B.4). We are ourselves working on
Verse, a new general purpose programming language, built directly on VC; indeed, our motivation
for developing VC is practical rather than theoretical. No single aspect of VC is unique, but we
believe that their combination is particularly harmonious and orthogonal. We discuss the rich
related work in Section 5, and design alternatives in Appendix B.

Our second contribution is to equip VC with a small-step term-rewriting semantics (Section 3).
We said that the lambda calculus is a subset of VC, so it is natural to give its semantics using
rewrite rules, just like the lambda calculus. That seems problematical, however, because logical
variables and unification involve sharing and non-local communication that seems hard to express
in a rewrite system.

Exactly the same difficulty arises with call-by-need. For a long time, the only semantics of call-
by-need that was faithful to its sharing semantics (in which thunks are evaluated at most once) was
an operational semantics that sequentially threads a global heap through execution [Launchbury
1993]. But then Ariola et al., in a seminal paper, showed how to reify the heap into the term itself,
and thereby build a rewrite system that is completely faithful to lazy evaluation [Ariola et al. 1995].
Inspired by their idea, we present a new rewrite system for functional logic programs, that reifies
logical variables and unification into the term itself, and exploits our notion of spatial choice to
replace non-deterministic search with a (deterministic) tree of successful results. For example, the
expression above can be rewritten thus1:

−→{deref-h}
−→{u-tup}
−→{deref-s × 2}
−→{norm-seq-assoc,norm-swap-eq} ∃x y z. x = ⟨2, 3⟩; y = 2; z = 3; ⟨2, 3⟩; 2
−→{norm-val,elim-def}
2

∃x y z. x = ⟨y, 3⟩; ⟨y, 3⟩ = ⟨2, z⟩; y
∃x y z. x = ⟨y, 3⟩; (y = 2; 3 = z; ⟨y, 3⟩); y
∃x y z. x = ⟨2, 3⟩; (y = 2; 3 = z; ⟨2, 3⟩); 2

Rules may be applied anywhere they match, again just like the lambda calculus. The question of
confluence arises, as we discuss in Section 4.

1The rule names come from Fig. 3, to be discussed in Section 3; they are given here just for reference.

Proc. ACM Program. Lang., Vol. 1, No. PLDI, Article 1. Publication date: January 2023.
The Verse Calculus: a Core Calculus for Functional Logic Programming

1:3

Abstract syntax
Integers
Variables
Primops
Scalar Values
Heap Values
Head Values
Values
Expressions

Programs
Bindings

Concrete syntax:

𝑘

𝑥, 𝑦, 𝑧, 𝑓 , 𝑔

::= gt | add
::= 𝑥 | 𝑘 | 𝑜𝑝

op
𝑠
ℎ ::= ⟨s1, · · · , sn⟩ | 𝜆x. e

hnf
v
𝑒
𝑒𝑢
𝑝
𝑐

::= ℎ | 𝑘
::= 𝑠 | ℎ
::= v | 𝑒𝑢; e | ∃x. e | fail | e1 e2 | v1 v2 | one{e} | all{e}
::= e | v = e
::= one{e} where fvs(𝑒) = ∅
::= x = v

Infix operators “ ”, “;”, “=”, and “>” are all right-associative.
“=” binds more tightly than “;”.
Function application (v1 v2) is left-associatve, as usual.
“𝜆”, “∃” scope as far to the right as possible.
e.g., (𝜆y. ∃x. x = 1; x + y) means (𝜆y. (∃x. ((x = 1); (x + y)))).

Desugaring

𝑣1 + 𝑣2 means add⟨v1, v2⟩
𝑣1 > 𝑣2 means gt⟨v1, v2⟩
∃x1 x2 · · · xn. e means ∃x1. ∃x2. · · · ∃xn. e

e1(e2) means ∃f a. f = e1; a = e2; f (a)

⟨e1, · · · , en⟩ means ∃x1 x2 · · · xn. x1 = e1; · · · ; xn = en; ⟨x1, · · · , xn⟩

e1 = e2 means ∃x. x = e1; x = e2; x

if e1 then e2 else e3 means ∃y. y = one{(e1; 𝜆𝑥 . e2)
x (cid:66) e1; e2 means ∃x. x = e1; e2

(𝜆𝑥 . e3)}; y⟨⟩

fvs(𝑒) means the free variable of 𝑒; in VC, 𝜆 and ∃ are the only binders.

Fig. 1. The Verse Calculus: Syntax

f , a fresh
xi fresh
x fresh
x, y fresh

2 THE VERSE CALCULUS, INFORMALLY
We begin by presenting the Verse calculus, VC, informally. We will give its rewrite rules precisely
in Section 3. The syntax of VC is given in Fig. 1. It has a very conventional sub-language that is
just the lambda calculus with some built-in operations and tuples as data constructors:

• Values. A value v is either a scalar value s, which can be freely duplicated, or a heap value
h. A heap value is a lambda or a tuple; and tuples only have value components. In VC a
variable counts as a value, because in a functional logic language an expression may evaluate
to an as-yet-unknown logical variable.

• Built-in functions. Our tiny calculus offers only integer constants k and two illustrative

operators op, namely gt and add

• Expressions e includes values v, and applications v1 v2; we will introduce the other constructs
as we go. For clarity we sometimes write v1(v2) rather than v1 v2 when v2 is not a tuple.

Proc. ACM Program. Lang., Vol. 1, No. PLDI, Article 1. Publication date: January 2023.
1:4Lennart Augustsson, Joachim Breitner, Koen Claessen, Ranjit Jhala, Simon Peyton Jones, Olin Shivers, and Tim Sweeney

• A program, p, is a closed expression from which we extract one result using one (see Sec-

tion 2.5).

The formal syntax for e allows only applications of values, (v1 v2), but the desugaring rules in Fig. 1
show how to desugar general applications (e1 e2). This ANF-like normalisation is not fundamental;
it simply reduces the number of rewrite rules we need. Modulo this desugaring, every lambda
calculus term is a VC term, and has the same semantics. Just like the lambda calculus, VC is
untyped; adding a type system is an excellent goal, but is the subject of another paper.

Expressions also include two other key collections of constructs: logical variables and unification
(Section 2.1), and choice (Section 2.2). The details of choice and unification, and especially their
interaction, are rather tricky, so this section will do a lot of arm-waving. But fear not: Section 3 will
make all this precise. We only have space to describe one incarnation of VC; Appendix B explores
some possible alternative design choices.

2.1 Logical variables and unification

The Verse calculus includes first class logical variables and unification: you can bring a fresh logical
variable into scope with ∃; equate a value with an expression v = e; and sequence two expressions
with e1; e2 (see Fig. 1). As an example, what might be written let x = e1 in {e2} in a conventional
functional language can be written ∃x. x = e1; e2 in VC. A unification (v = e) always equates a
value to an expression, and can only appear to the left of a “; ” (see ue in Fig. 1). Again the deugaring
rules rewrite a general equality e1 = e2 into this simpler form.
A program executes by solving its equations. For example,

∃x y z. x = ⟨y, 3⟩; x = ⟨2, z⟩; y
is solved by unifying x with ⟨y, 3⟩ and with ⟨2, z⟩; that in turn unifies ⟨y, 3⟩ with ⟨2, z⟩, which unifies
y with 2 and z with 3. Finally 2 is returned as the result. Note carefully that, like any declarative
language, logical variables are not mutable; a logical variable stands for a single, immutable value.
We use “∃” to bring a fresh logical variable into scope, because we really mean “there exists an x
such that ...”. Logical variables are existential variables.

High-level functional languages usually provide some kind of pattern matching; in such a
language, we might define first by first⟨a, b⟩ = a. Such pattern matching is typically desugared to
more primitive case expressions, but in VC we do not need case expressions: unification does the
job. For example we can define first like this:

first = 𝜆pr. ∃ab. pr = ⟨a, b⟩; a
For convenience, in this presentation we allow ourselves to write a term like first⟨2, 5⟩, where we
define first separately. Formally, you can imagine each example e being wrapped with a binding for
first, thus ∃first. first = ...; e; and similarly for all other library functions.

This way of desugaring pattern matching means that the input to first is not required to be fully

determined when the function is called. For example:

∃x y. x = ⟨y, 5⟩; first (x) = 2; y
Here first (x) evaluates to y, which we then unify with 2. Another way to say this is that, as usual
in logic programming, we may constrain the output of a function (here first (x) = 2), and thereby
affect its input (here ⟨y, 5⟩).

Although “;” is called “sequencing”, the order of that sequence is immaterial for equations. For
example consider (∃x y. x = 3 + y; y = 7; x). In VC we can only unify x with a value; we will
see why in Section 2.2. So the equation x = 3 + y is stuck. No matter! We simply leave it and try
some other equation. In this case, we can make progress with y = 7; and that in turn unlocks

The Verse Calculus: a Core Calculus for Functional Logic Programming

1:5

x = 3 + y because now we know that y is 7, so we can evaluate 3 + 7 to 10 and unify x with that.
The idea of leaving stuck expressions aside, and executing other parts of the program is called
residuation [Hanus 2013]2, and is at the heart of our mantra “just solve the equations.”

2.2 Choice

In conventional functional programming, an expression evaluates to a single value. In contrast,
a VC expression evaluates to a choice of zero, one, or many values (or it can get stuck, which is
different from producing zero values). The expression fail yields no values; a value v yields one
value; and the choice e1
e2 yields all the values yielded by e1 and all the values yielded by e2.
Duplicates are not eliminated and, as we shall see in Section 2.7, order is maintained; in short, an
expression yields a sequence of values, not a bag, and certainly not a set.

The equations we saw in Section 2.1 can fail, if the arguments are not equal, yielding no results.
Thus 3 = 3 succeeds, returns a single result, namely 3, while 3 = 4 fails, returning no results. In
general, we use “fail” and “returns no results” synonymously.

What if the choice was not at the top level of an expression? For example, what does ⟨3, (7 5)⟩
mean? In VC it does not mean a pair with some kind of multi-value in its second component.
Indeed, as you can see from Fig. 1, this expression is syntactically ill-formed. We must instead
give a name to that choice, and then we can put it in the pair, thus: ∃x. x = (7 5); ⟨3, x⟩. This
is syntactically legal, but what does it mean? In VC a variable is never bound to a multi-value.
Instead, x is successively bound to 7, and then to 5, like this:

∃x. x = (7 5); ⟨3, x⟩

−→ (∃x. x = 7; ⟨3, x⟩)

(∃x. x = 5; ⟨3, x⟩))

We duplicate the context surrounding the choice, and “float the choice outwards.”

2.3 Mixing choice and unification
We saw in Section 2.1 that equations are insensitive to sequencing—but choice is not. Consider
30); ⟨x, y⟩. The choices are made left-to-right, so that the result is
∃x y. x = (3
(⟨3, 20⟩

4); y = (20
⟨4, 20⟩

⟨4, 30⟩).

⟨3, 30⟩

So much for choice under unification. What if we have unification under choice? For example:

∃x. (x = 3; x + 1)

(x = 4; x ∗ 2)

Intuitively, either unify x with 3 and return x + 1, or unify x with 4 and return x ∗ 2. But so far we
have said only “a program executes by solving its equations” (Section 2.1). Well, we can see two
equations here, (x = 3) and (x = 4), which are mutually contradictory, so clearly we need to refine
our notion of “solving.” The answer is pretty clear: in a branch of a choice, solve the equations in
that branch to get the value for some logical variables, and propagate those values to occurrences in
that branch (only). Occurrences of that variable outside the choice are unaffected. We call this local
propagation. This local-propagation rule would allow us to reason thus:

∃x. (x = 3; x + 1)

(x = 4; x ∗ 2) −→ ∃x. (x = 3; 4)

(x = 4; 8)

Are we stuck now? No, we can float the choice out as before3,
(x = 4; 8) −→ (∃x. x = 3; 4)
and now it is apparent that the sole occurrence of x in each ∃ is the equation (x = 3), or (x = 4)
respectively; so we can drop the ∃ and the equation, yielding (4 8).

∃x. (x = 3; 4)

(∃x. x = 4; 8)

2Hanus did not invent the terms “residuation” and“narrowing”, but his survey is an excellent introduction and bibliography.
3Indeed we could have done so first, had we wished.

1:6Lennart Augustsson, Joachim Breitner, Koen Claessen, Ranjit Jhala, Simon Peyton Jones, Olin Shivers, and Tim Sweeney

2.4 Pattern matching and narrowing

We remarked in Section 2.1 that we can desugar the pattern matching of a high-level language into
unification. But what about multi-equation pattern matching, such as this definition in Haskell:
append [ ] 𝑦𝑠 = 𝑦𝑠
append (x : 𝑥𝑠) 𝑦𝑠 = x : append 𝑥𝑠 𝑦𝑠

If pattern matching on the first equation fails, we want to fall through to the second. Fortunately,
choice allows us to express this idea directly4:
append = 𝜆⟨𝑥𝑠, 𝑦𝑠⟩. (𝑥𝑠 = ⟨⟩; 𝑦𝑠)

(∃x xrest. 𝑥𝑠 = ⟨x, xrest⟩; ⟨x, append⟨xrest, 𝑦𝑠⟩⟩)

If 𝑥𝑠 is ⟨⟩, the left-hand choice succeeds, returning 𝑦𝑠; and the right-hand choice fails (by attempting
to unify ⟨⟩ with ⟨x, xrest⟩). If 𝑥𝑠 is of the form ⟨x, xrest⟩, the right-hand choice succeeds, and we
make a recursive call to append. Finally if 𝑥𝑠 is built with head-normal forms other than the empty
tuple and pairs, both choices fail, and append returns no results at all.

This approach to pattern matching is akin to narrowing [Hanus 2013]. Suppose single = ⟨1, ⟨⟩⟩, a
singleton list whose only element is 1. Consider the call ∃zs. append⟨zs, single⟩ = single; zs. The
call to append expands into a choice

(zs = ⟨⟩; single)

(∃x xrest. zs = ⟨x, xrest⟩; ⟨x, append⟨xrest, single⟩⟩)

which amounts to exploring the possibility that zs is headed by ⟨⟩ or a pair—the essence of narrowing.
It should not take long to reassure yourself that the program evaluates to ⟨⟩, effectively running
append backwards in the classic logic-programming manner.

This example also illustrates that VC allows an equality (for append) that is recursive. As in any
functional language with recursive bindings, you can go into an infinite loop if you keep fruitlessly
inlining the function in its own right-hand side. It is the business of an evaluation strategy to do
only rewrites that make progress towards a solution (Section 3.7).

2.5 Conditionals and one
Every source language will provide a conditional, such as if (x = 0) then e2 else e3. But what is
the equality operator in (x = 0)? One possibility, adopted by Curry, is this: there is one “=” for
equations (as in Section 2.1), and another, say “==”, for testing equality (returning a boolean with
constructors True and False). VC takes a different, more minimalist position. In VC there is just
one equality operator, written “=” just as in Section 2.1. The expression if (x = 0) then e2 else e3
tries to unify x with 0. If that succeeds (returns one or more values) the if returns e2; otherwise it
returns e3. There are no data constructors True and False; instead failure plays the role of falsity.
But something is terribly wrong here. Consider ∃x y. y = (if (x = 0) then 3 else 4); x = 7.
Presumably this is meant to set x to 7, test if it is equal to 0 (it is not), and unify y with 4. But what
is to stop us instead unifying x with 0 (via (x = 0)), unifying y with 3, and then failing when we try
to unify x with 7? Not only is that not what we intended, but it also looks very non-deterministic:
the result is affected by the order in which we did unifications!

To address this, we give if a special property: in the expression if e1 then e2 else e3, unifications
inside e1 (the condition of the if) can only unify variables bound inside e1; variables bound outside
e1 are called “rigid.” So in our example, the x in (x = 0) is rigid and cannot be unified. Instead, the if
is stuck, and we move on to unify x = 7. That unblocks the if and all is well. This special property
is precisely the local propagation rule that we sketched for choice (Section 2.3).

4We use the empty tuple ⟨⟩ to represent the empty list and pairs to represent cons cells; and we allow ourselves to write
𝜆 ⟨x, y ⟩. body rather than 𝜆p. ∃x y. p = ⟨x, y ⟩; body

The Verse Calculus: a Core Calculus for Functional Logic Programming

1:7

In fact, VC distills the three-part if into something simpler, the unary construct one{e}. Its
specification is this: if e fails, one{e} fails; otherwise one{e} returns the first of the values yielded
by e. Now, if e1 then e2 else e3 can (nearly) be re-expressed like this:

one{(e1; e2) e3}
If e1 fails, the first branch of the choice fails, so we get e3; if e1 succeeds, we get e2, and the outer one
will select it from the choice. But this isn’t right: what if e2 or e3 themselves fail or return multiple
results? Here is a better translation, given in Fig. 1, which wraps the then and else branches in a
thunk:

(one{(e1; (𝜆𝑥 . e2))
(𝜆𝑥 . e3)})⟨⟩
· · · ) or (𝜆𝑥 . e3) depending on whether e1
The argument of one evaluates to either ((𝜆𝑥 . e2)
succeeds or fails, respectively, and one then picks that lambda and applies it to ⟨⟩. As a bonus,
provided we do no evaluation under a lambda, then e2 and e3 will remain un-evaluated until the
choice is made, just as we expect.

We use the same local-propagation rule for one that we do for choice (Section 2.3); together

with the desugaring for if into one, we get the “special property” of if described above.

2.6 Tuples and all
The main data structure in VC is the tuple. A tuple is a finite sequence of values, ⟨v1, · · · , vn⟩.
It can be used like a function: indexing is simply function application with the argument being
integers from 0 and up. Indexing out of range is fail. For example, ∃t. t = ⟨10, 27, 32⟩; t (1) reduces
to 27 and t (3) reduces to fail. The reduction rule for indexing in tuples admits multi-valued index
expressions. For instance, ∃t. t = ⟨10, 27, 32⟩; t (1

1) reduces to (27

27).

10

0

Tuples can be constructed by collecting all the results from a multi-valued expression, using
vn) then all{e} reduces to the tuple ⟨v1, · · · , vn⟩; as a
is associative, which means that

the all construct: if e reduces to (v1
consequence, if 𝑒 fails, all produces the empty tuple. Note that
we can think of a sequence or tree of binary choices as really being a single 𝑛-way choice.

· · ·

You might think that tuple indexing would be stuck until we know the index, but VC uses
narrowing to make progress. The expression ∃t. t = ⟨10, 27, 32⟩; ∃i. t (i) looks stuck because we
have no value for i, but in fact it rewrites to
∃i. (i = 0; 10)

(i = 1; 27)

(i = 2; 32)

which (as we will see in Section 3) simplifies to just (10
reified into a tuple; and (∃i. t (i)) allows a tuple to be turned back into a choice.

27

32). So all allows a choice to be

Do we even need one as a primitive construct, given that we have all? Can we not use (all{e})(0)
instead of one{e}? Indeed they behave the same if e fully reduces to finitely many choices of values.
But all really requires the evaluation of all choices before proceeding, while one only needs to
evaluate the first choice. So, supposing that loop is a non-terminating function, one{1
loop⟨⟩}
reduces to 1, while (all{1

loop⟨⟩})(0) loops.

2.7 for loops
The expression for(e1) do e2 will evaluate e2 for each of the choices in e1, rather like a list
comprehension in languages like Haskell or Python. The scoping is peculiar5 in that variables
bound in e1 also scope over e2. So, e.g., for(∃x. x = 2
5) do (x + 1) will reduce to the tuple
⟨3, 4, 6⟩.

3

Like list comprehension, for supports filtering; in VC this falls out naturally by just using a
5; x > 2) do (x + 1) reduces to ⟨4, 6⟩. Nested
3

possibly failing expression in e1. So, for(x (cid:66) 2
5But similar to C++.

1:8Lennart Augustsson, Joachim Breitner, Koen Claessen, Ranjit Jhala, Simon Peyton Jones, Olin Shivers, and Tim Sweeney

Notation

f (x) (cid:66) e means
f ⟨x, y⟩ (cid:66) e means
head (𝑥𝑠)
tail(𝑥𝑠)
cons⟨x, 𝑥𝑠⟩
append⟨𝑥𝑠, 𝑦𝑠⟩
flatMap⟨f , 𝑥𝑠⟩
map⟨f , 𝑥𝑠⟩
filter ⟨p, 𝑥𝑠⟩
find⟨p, 𝑥𝑠⟩
every⟨p, 𝑥𝑠⟩

f (cid:66) 𝜆x. e
f (cid:66) 𝜆p. ∃x y. p = ⟨x, y⟩; e

p fresh

:= 𝑥𝑠 (0)
:= all{∃i. i > 0; 𝑥𝑠 (i)}
:= all{x ∃i. 𝑥𝑠 (i)}
:= all{(∃i. 𝑥𝑠 (i))
:= all{∃i. f (𝑥𝑠 (i))}
:= if x (cid:66) head (𝑥𝑠) then cons⟨f (x), map⟨f , tail(𝑥𝑠)⟩⟩ else ⟨⟩
:= all{∃i. x (cid:66) 𝑥𝑠 (i); one{p(x)}; x}
:= one{∃i. x (cid:66) 𝑥𝑠 (i); one{p(x)}; x}
:= map⟨p, 𝑥𝑠⟩

(∃i. 𝑦𝑠 (i))}

Fig. 2. Common list functions

3) do (x + y) reduces to ⟨11, 12, 13, 21, 22, 23⟩

iteration in a for works as expected, and requires nothing special. So, for(x (cid:66) 10
1

2
Just as if is defined in terms of the primitive one (Section 2.5), we can define for in terms of the
primitive all. Again, we have to be careful when e2 itself fails or produces multiple results; simply
writing all{e1; e2} would give the wrong semantics. So we put e2 under a lambda, and apply each
element of the tuple to ⟨⟩ afterwards, using the map function defined in Fig. 2. The full desugaring
is

20; y (cid:66)

for(e1) do e2 ≡ ∃v. v = all{e1; 𝜆𝑥 . e2}; map⟨𝜆z. z⟨⟩, v⟩
for a fresh variable v. Note how this achieves that peculiar scoping rule: variables defined in
e1 are in scope in e2. Any effects (like being multivalued) in e2 will not affect the choices de-
fined by e1 since they are in a thunk. So, e.g., for(x (cid:66) 10
x + 1 } will reduce to
⟨10, 20⟩

⟨11, 21⟩. At this point it is crucial to use map, not flatMap.

20) do { x

⟨10, 21⟩

⟨11, 20⟩

Given that tuple indexing expands into choices, we can iterate over tuple indices and elements
using for. For example for(∃i x. x = t (i)) do (x + i) produces a tuple with the elements of t,
increased by their index in t.

2.8 Programming in Verse
VC is a fairly small language, but it is quite expressive. For example, we can define the typical list
functions one would expect from functional programming by using the duality between tuples and
choices, as seen in Fig. 2. A tuple can be turned into choices by indexing with a logical variable i.
Conversely, choices can be turned into a tuple using all. The choice operator, , serves as both cons
and append for choices.

Pattern matching for function definitions is simply done by unification of ordinary expressions.
This means that we can use ordinary abstraction mechanisms for patterns. For example, here is a
function that should be called like fcn⟨88, 1, 99, 2⟩.

fcn(t) (cid:66) ∃x y. t = ⟨x, 1, y, 2⟩; x + y

If we want to give a name to the pattern, it is simple to do so:

pat⟨v, w⟩ (cid:66) ⟨v, 1, w, 2⟩;

fcn(t) (cid:66) ∃x y. t = pat⟨x, y⟩; x + y

The Verse Calculus: a Core Calculus for Functional Logic Programming

1:9

3 REWRITE RULES

How can we give a precise semantics to a non-strict functional programming language? Here are
some possibilities:

• A denotational semantics is the classical approach, but it is tricky to give a (perspicuous)
denotational semantics to a functional logic language, because of the logical variables. We
have such a denotational semantics under development, which we offer for completeness in
Appendix C, but that is the subject of another paper.

• A big-step operational semantics typically involves explaining how a (heap, expression) start-
ing point evaluates to a (heap, value) pair; Launchbury’s natural semantics for lazy eval-
uation [Launchbury 1993] is the classic paper. The heap, threaded through the semantics,
accounts for updating thunks as they are evaluated.

• A small-step operational semantics. Despite its “operational semantics” title, the big-step
approach does not convey accurate operational intuition, because it goes all the way to a
value in one step. So-called “small-step” operational semantics are therefore widely used;
they typically describe how a (heap, expression, stack) configuration evolves, one small step
at a time (e.g., [Peyton Jones 1992]). The difficulty is that the description is now so low level
that it is again hard to explain to programmers.

• A rewrite semantics steers between these two extremes. For example, Ariola et al.’s “A call by
need lambda calculus” [Ariola et al. 1995] shows how to give the semantics of a call-by-need
language as a set of rewrite rules. The great advantage of this approach is that it is readily
explicable to programmers. Indeed teachers almost always explain the execution of Haskell
or ML programs as a succession of rewrites of the program (e.g., inline this call, simplify this
case expression, etc.).

Up to this point there has been no satisfying rewrite semantics for functional logic languages (see
Section 5 for previous work). Our main technical contribution is to fill this gap with a rewrite
semantics for VC, one that has the following properties:

• The semantics is expressed as a set of rewrite rules (Fig. 3 and 4).
• Any rule can be applied, in either direction, anywhere in the program term (including under

lambdas) to obtain an equivalent program.

• The rules are oriented, with the intent that using them left-to-right makes progress.
• Despite this orientation, the rules do not say which rule should be applied where; that is the

task of a separate evaluation strategy (Section 3.7).

• The rules can be applied by programmers, to reason about what their program does; and by

compilers, to transform (and hopefully optimise) the program.

• There is no “magical rewriting” (Section 5.3): all the variables on the right-hand side of a rule

are bound on the left.

3.1 Functions and function application
Looking at Fig. 3, rule app-add should be familiar: it simply rewrites an application of add to integer
constants. For example add⟨3, 4⟩ −→ 7. Rules app-gt and app-gt-fail are more interesting: gt⟨k1, k2⟩
fails if 𝑘1 ⩽ 𝑘2 (rather than returning False as is more conventional), and returns k1 otherwise
(rather than returning True). An amusing consequence is that (10 > x > 0) succeeds iff x is between
10 and 0 (comparison is right-associative).

Beta-reduction is performed quite conventionally by app-beta; the only unusual feature is that
on the RHS of the rule we use a ∃ to bind x, together with (x = v) to equate x to the argument.
The rule may appear to use call-by-value, because the argument is a value v, but remember that

1:10Lennart Augustsson, Joachim Breitner, Koen Claessen, Ranjit Jhala, Simon Peyton Jones, Olin Shivers, and Tim Sweeney

Expression context

Application context

Scope context
Choice context
Choice-free expr
Bound variables

𝐸 ::= □ | ⟨s1, · · · , □, · · · , sn⟩ | 𝜆x. E | ∃x. E | E = e | e = E
| E; e | e; E | E v | v E | E e | e E | all{E} | one{E}

𝐴 ::= □ v | op □ | □ = hnf

| v = A | ∃x. A | A; e | e; A

| A e | e A | all{A} | one{A}
𝑆𝑋 ::= □ e | e □ | one{□} | all{□}
𝐶𝑋 ::= □ | v = 𝐶𝑋 | 𝐶𝑋 ; e | ce; 𝐶𝑋 | ∃x. 𝐶𝑋
𝑐𝑒 ::= v | v = ce | ce1; ce2 | one{e} | all{e} | op(v) | ∃x. ce
bvs(𝐸) = The variables that are bound by E at the hole

e.g. bvs((∃x. x = 3)

(∃y. □ = 4))) = {𝑦}

Unification: U
deref-s
deref-h
u-scalar
u-tup ⟨v1, · · · , vn⟩ = ⟨v ′
, · · · , v ′
1
hnf
u-fail

x = s; E[ x ] −→ x = s; E[ s ]
x = h; A[ x ] −→ x = h; A[ h]

𝑥 (cid:46) 𝑠, 𝑥 ∉ bvs(E), 𝑠 ∉ bvs(E)
𝑥 ∉ bvs(𝐴), fvs(ℎ) ∉ bvs(𝐴)

s = s; e −→ e

n⟩; e −→ v1 = v ′
2 −→ fail

1 = hnf

1; · · · ; vn = v ′

n; e

if neither u-scalar nor u-tup match

(𝜆x. e) v −→ ∃x. x = v; e

⟨⟩ v −→ fail

if x ∉ fvs(v)

⟨v0 · · · vn⟩ v −→ ∃x. x = v; (x = 0; v0

· · · x = n; vn)

if x ∉ fvs(v), 𝑛 ⩾ 0

Application: A
app-beta
app-tup0
app-tup
app-add
app-gt
app-gt-fail

add⟨k1, k2⟩ −→ k1 + k2
gt⟨k1, k2⟩ −→ k1
gt⟨k1, k2⟩ −→ fail

SX [𝐶𝑋 [ e1 e2 ] ] −→ SX [𝐶𝑋 [ e1 ] 𝐶𝑋 [ e2 ] ]

(e2 e3) ]

Speculation: S
choose
choose-assoc SX [ (e1 e2) e3 ] −→ SX [ e1
SX [ fail e ] −→ SX [ e ]
choose-r
SX [ e fail] −→ SX [ e ]
choose-l
one{fail} −→ fail
one-fail
one-choice
one-value
all-fail
all-choice

e2} −→ e1
one{e} −→ e
all{fail} −→ ⟨⟩
· · ·

one{e1

all{e1

en} −→ ∃𝑥 . 𝑐; ⟨𝑣⟩

if 𝑘1 > 𝑘2
if 𝑘1 ⩽ 𝑘2

if 𝐶𝑋 ≠ □

if ∅ ⊢ e1 ⇝ (𝑥 | 𝑐 | v)
if ∅ ⊢ e ⇝ (𝑥 | 𝑐 | v)

if ⊢∗ 𝑒 ⇝ (𝑥 | 𝑐 | v), 𝑛 ⩾ 1

Fig. 3. The Verse Calculus: Rewrite Rules

values include variables, which may be bound to an as-yet-unevaluated expression. For example:
∃y. y = 3 + 4; (𝜆x. x + 1)(y) −→ ∃y. y = 3 + 4; ∃x. x = y; x + 1
Finally, the side condition 𝑥 ∉ fvs(𝑣) in app-beta ensures that the ∃ x does not capture any variables
free in v. If x appears free in v, just use α-conversion to rename x to 𝑥 ′ ∉ fvs(𝑣).

In VC, tuples behave like (finite) functions, in which application is indexing. Rule app-tup
describes how tuple application works. Notice that app-tup does not require the argument to be eval-
uated to an integer 𝑘; instead the rule works by narrowing. So the expression ∃x. ⟨2, 3, 2, 7, 9⟩(x) =

The Verse Calculus: a Core Calculus for Functional Logic Programming

1:11

2; x does not suspend awaiting a value for x; instead it explores all the alternatives, returning
(0 2). This is a free design decision: a suspending semantics would be equally easy to express.

3.2 Unification

Next we study unification, again in Fig. 3. Rules u-scalar and u-tup are the standard rules for
unification, going back nearly 60 years [Robinson 1965]. Note that when unification succeeds it
yields the common value; hence s = s rewrites to s6. Rule u-fail makes unification fail on two
different head-normal forms (see Fig. 1 for the syntax of hnf ). Note in particular that unification
fails if you attempt to unify a lambda with any other value (including itself) – see Section 4.2.

The key innovation in VC is the way bindings (that is, just ordinary equalities) of logical

variables are propagated. The key rules are:

x = s; E[ x ] −→ x = s; E[ s ]
deref-s
deref-h x = h; A[ x ] −→ x = h; A[ h]

𝑥 (cid:46) 𝑠, 𝑥 ∉ bvs(E), 𝑠 ∉ bvs(E)
𝑥 ∉ bvs(𝐴), fvs(ℎ) ∉ bvs(𝐴)

These rules make use of so-called contexts, E and A, whose syntax is given in Fig. 3 [Felleisen and
Friedman 1986; Felleisen et al. 1987]. In general, a context is an expression containing a single hole,
written □. The notation E[ s ] is the expression obtained by filling the hole in E with s.

So deref-s says that if we have an equality (x = s) to the left of a term E[ x ] that mentions x, we
can replace that (single) occurrence of x with s, yielding E[ s ] instead. There are several things to
notice:

• deref-s fires only when the right-hand side of the unification is a scalar value s; that is, a
variable or integer literal. That is because E allows the occurrence of x to be in places that
only syntactically allow scalars. [LA: Is it really true now that E can have places where only
scalars are allowed?] Rule defref-h allows substitution of heap values, but again only in
places that syntactically allow such expressions; also see Section 4.2.

• Both rules fire only when the RHS is a value, so that the substitution does not risk duplicating
either work or choices. This restriction is precisely the same as the let-v rule of [Ariola et al.
1995], and (by not duplicating choices) it neatly implements so-called call-time choice [Hanus
2013]. We do not need a heap, or thunks, or updates; the equalities of the program elegantly
suffice to express the necessary sharing.

• Both deref rules replace a single occurrence of x, leaving the original (x = v) undisturbed.
For example, we can rewrite (x = 3; y = x + 1; z = x + 3) to (x = 3; y = 3 + 1; z = x + 3),
using E = (y = □ + 1; z = x + 3). We must not drop the (x = v) because there may be other
occurrences of x, such as the x + 3 in this example. When there are no remaining occurrences
of x we may garbage collect the binding: see Section 3.4.

• Both rules substitute only to the right of a binding. How can we rewrite (y = x + 1; x = 3),
where the occurrence of x is to the left of its binding? Answer, by moving the x = 3 binding
to the left, a process we call normalization, discussed in Section 3.4.

• The 𝑥 (cid:46) 𝑠 in deref-s prevents a binding x = x from substituting infinitely often, doing nothing
each time. The guard 𝑥 ∉ bvs(𝐸) ensures that x is actually free in E[ x ], while 𝑠 ∉ bvs(𝐸)
ensures that s is not captured by E in E[ s ].

• deref-s substitutes a scalar anywhere, but deref-h is much more parsimonous: it never
substitutes a heap value h under a lambda or inside a tuple, as can be seen by examining the
syntax of application contexts A. This is a tricky point: see Section 4.2.

• Rather unconventionally, there is no “occurs check”, leading to fail. It is very important to
allow allow bindings like (f = 𝜆x. · · · (f (x − 1)) · · · ) to substitute, because that is how we

6An alternative choice would for unification to yield ⟨⟩ on success. It does not make much difference either way.

1:12Lennart Augustsson, Joachim Breitner, Koen Claessen, Ranjit Jhala, Simon Peyton Jones, Olin Shivers, and Tim Sweeney

define a recursive function! We even allow (x = ⟨1, x⟩). Of course, recursive bindings can lead
to infinite rewriting sequences; it is up to the evaluation strategy to avoid this (Section 3.7).

3.3 Local substitution
Consider this (extremely) tricky term: ∃x. x = if (x = 0; x > 1) then 33 else 55. What should this
do? At first you might think it was stuck; how can we simplify the if when its condition mentions
x which is not yet defined? But in fact, rule deref-s allows us to substitute locally in any X-context
surrounding the equality (x = 0) thus:

∃x. x = if (x = 0; x > 1) then 33 else 55
∃x. x = if (x = 0; 0 > 1) then 33 else 55

−→{deref-s}
−→{u-fail,fail-seql} ∃x. x = if fail then 33 else 55
∃x. x = 55
−→{simplify if}
−→{elim-def}
55

Minor variants of the same example get stuck instead of reducing. For example, if you replace
the (x = 0) with (x = 100) then rewriting gets stuck, as the reader may verify; and yet there
is a solution to the equations, namely 𝑥 = 55. And if you replace (x = 0) with (x = 55) then
rewriting again gets stuck, and reasonably so, since in this case there are no valid solutions to
the equations. Perhaps this is not surprising: we cannot reasonably expect the program to solve
arbitrary equations. For example, ∃x. x ∗ x = x has two solutions but discovering that involves
solving a quadratic equation.

3.4 Normalization rules
The syntax of Fig. 1 allows (∃x. e), (v = e), and (e1; e2) to occur anywhere in an expression. But
to make other rules more applicable, it may be necessary to “float” these expression upward. For
example, we can’t use deref-h to substitute for x in (x = (e; 3); x + 2), because the RHS of the
x-equality is not a value. But if we were to float the semicolon outwards to give (e; x = 3; x + 2),
we could then substitute for x.

Thus motivated, Fig. 4 gives a collection of rules that systematically move existentials and
unifications upward and to the left. The net effect is to normalise the term to a form with existentials
at the top, then scalar equalities, and then heap equalities, thus

∃x1, · · · , xn. x1 = s1; · · · ; xi = hi; xn = hn; e
You can think of this form as “an expression e wrapped in some heap bindings xi = vi”. The heap
bindings express, as a term, the possibly-recursive values of the xi, but the right-hand sides vi are
all values, so there is no computation left in the heap. This decomposition is so important that we
define a judgement Γ ⊢ e1 ⇝ (𝑥 | 𝑐 | e2) in Fig. 5, which decomposes an expression e1 into its heap,
specified by 𝑥 and 𝑐, and the expression wrapped in that heap, e2. (The non-terminal c is just short
for x = v; Fig. 1). Notice that, if invoked with Γ = ∅, this judgement checks that that the equalities
𝑐 fix only variables bound by one of the existentials 𝑥; and moreover that there is only one such
equality for any particular xi.

One very useful application of this decomposition is elim-def in Fig. 4, which allows an entire
heap of possibly-recursive (but computation-free) bindings to be discarded if none of its variables
are used. elim-def allows you to tidy up an expression, but it is not necessary for progress, and you
can omit it entirely if you want. The normalization rules of Fig. 4 also

• Associate “; ” to the right (rule norm-seq-assoc).
• Drop a value to the left of a “; ” (rule norm-val).
• Propagate fail (rules fail-seql, fail-seqr, and fail-eq).

The Verse Calculus: a Core Calculus for Functional Logic Programming

1:13

Normalization: N
norm-val
norm-seq-assoc
norm-seq-swap1
norm-seq-swap2
norm-eq-swap
norm-seq-defr
norm-seq-defl
norm-defr
norm-seqr
Fail Propagation: F
fail-seql
fail-seqr
fail-eq
Garbage Collection: G
elim-def
Structural rules

v; e −→ e
(𝑒𝑢; e1); e2 −→ 𝑒𝑢; (e1; e2)
𝑒𝑢; (x = v; e) −→ x = v; (𝑒𝑢; e)
𝑒𝑢; (x = s; e) −→ x = s; (𝑒𝑢; e)
hnf = x −→ x = hnf
(∃x. e1); e2 −→ ∃x. (e1; e2)
𝑒𝑢; (∃x. e) −→ ∃x. 𝑒𝑢; e
v = (∃y. e1); e2 −→ ∃y. v = e1; e2
v = (𝑒𝑢; e1); e2 −→ 𝑒𝑢; v = e1; e2

fail; e −→ fail
e; fail −→ fail
v = fail −→ fail

if 𝑒𝑢 not of form x ′ = v ′
if 𝑒𝑢 not of form x ′ = s′

if x ∉ fvs(e2)
if x ∉ fvs(𝑒𝑢)
if y ∉ fvs(v, e2)

e1 −→ e2

if ∅ ⊢ e1 ⇝ (𝑥 | 𝑐 | e2) and 𝑥 ∉ fvs(e2)

swap-d
swap-c

∃x. ∃y. e
x1 = v1; x2 = v2; e

≡
≡

∃y. ∃x. e
x2 = v2; x1 = v1; e

Fig. 4. The Verse Calculus: Normalization Rules

Γ ⊢ e1 ⇝ (𝑥 | 𝑐 | e2)

Γ ⊢ e ⇝ (∅ | ∅ | e)

WF-Exp

Γ, 𝑥 ⊢ 𝑒1 ⇝ (𝑥 | 𝑐 | e2)

𝑥 ∉ 𝑥

Γ ⊢ ∃x. e1 ⇝ (x, 𝑥 | 𝑐 | e2)

WF-Def

𝑣 ≠ 𝑥

𝑥 ∈ Γ
Γ − 𝑥 ⊢ e1 ⇝ (𝑥 | 𝑐 | e2)

if 𝑣 = 𝑠 then 𝑥 ∉ fvs(e1)
fvs(ℎ) ∉ 𝑥

Γ ⊢ x = v; e1 ⇝ (𝑥 | x = v, 𝑐 | e2)

WF-Eq

∅ ⊢ 𝑟1 ⇝ (𝑥 1 | 𝑐1 | e1)

· · ·

∅ ⊢ 𝑟𝑛 ⇝ (𝑥𝑛 | 𝑐𝑛 | en)

all xi distinct

⊢∗ r1, · · · , rn ⇝ (𝑥 1, · · · , 𝑥𝑛 | 𝑐1, · · · , 𝑐𝑛 | e1, · · · , en)

WF-many

Fig. 5. Well-formedness of Results

• Put a variable on the LHS of an equality, where possible (rule norm-swap-eq).

Note that the normalization rules preserve the left-to-right sequencing of expressions, which
matters because choices are made left-to-right as we saw in Section 2.3. Moreover, note that the
normalisation rules do not float equalities or existentials out of choices. That restriction is the key to
localizing unification (Section 2.3), and the flexible/rigid distinction of Section 2.5. For example,
consider the expression (y = ((x = 3; x ∗ 2)
(x = 4)); ⟨x + 1, y⟩). We must not propagate the
binding (x = 3) to the expression (x + 1), because the latter is outside the choice, and a different
branch of the choice binds x to 4. But rule deref-s can propagate it locally within the first arm of

1:14Lennart Augustsson, Joachim Breitner, Koen Claessen, Ranjit Jhala, Simon Peyton Jones, Olin Shivers, and Tim Sweeney

the choice, thus7:

y = ((x = 3; x ∗ 2)

(x = 4)); ⟨x + 1, y⟩ −→ y = ((x = 3; 3 ∗ 2)

(x = 4)); ⟨x + 1, y⟩

To make further progress, we need a rule for choice; see Section 3.5.

[LA: Somewhere we should mention that the result of a (non-stuck) reduction with be an

expression e with ∅ ⊢ e ⇝ (𝑥 | x = h | v), i.e., a value with a set of bindings for heap values.]

3.5 Rules for choice

The rules for choice are given in Fig. 3. Rules one-value, one-choice and one-fail describe the
semantics for one, just as in Section 2.5. Similarly all-fail and all-choice describe the semantics of
all (Section 2.6). These rules use the well-formed-result judgement, introduced in Section 3.4 and
defined in Fig. 5, to ensure that each arm of the choice(s) consists of a value wrapped in a heap.

The most interesting rule is choose which, just as described in Section 2.2, “floats the choice
outwards”, duplicating the surrounding context. But what “surrounding context” precisely? We
use two new contexts, SX and 𝐶𝑋 , both defined in Fig. 1. A choice context 𝐶𝑋 is like an execution
context 𝑋 , but with no possible choices to the left of the hole:

𝐶𝑋 ::= □ | v = 𝐶𝑋 | 𝐶𝑋 ; e | ce; 𝐶𝑋 | ∃x. 𝐶𝑋
Here, ce is guaranteed-choice-free expression (syntax in Fig. 1). This syntactic condition is neces-
sarily conservative; for example, a call f (x) is considered not guaranteed-choice-free, because it
depends on what function f does. We must guarantee not to have choices to the left so that we
preserve order—see Section 2.3.

The context SX (Fig. 3) is a scope context; it ensures that 𝐶𝑋 is as large as possible. This is a subtle

point: without this restriction we lose confluence. To see this, consider8:

∃x. (if (x > 0) then 55 else 44); x = 1; (77 99)
−→{norm-seq-swap2} ∃x. x = 1; (if (x > 0) then 55 else 44); (77 99)
∃x. x = 1; (if (1 > 0) then 55 else 44); (77 99)
−→{deref-s}
∃x. x = 1; 55; (77 99)
−→{simplify if}
−→{seq, elim-def}
77 99

But suppose instead we floated the choice out, part-way, like this:
∃x. (if (x > 0) then 55 else 44); x = 1; (77 99)

−→{Bogus choose} ∃x. (if (x > 0) then 55 else 44); (x = 1; 77)

(x = 1; 99)

Now the (x = 1) is inside the choice branches, so we cannot use norm-seq-swap2 to move it to
the left of the if. Nor can we use choose again to float the choice further out, because the if is
not guaranteed choice-free (for example, the branches might contains choices). So, alas, we are
stuck! Our not-entirely-satisfying solution is to force choose to float the choice all the way to the
innermost enclosing scope construct; hence the SX in the rule.

Rule choose moves choices around; only one-choice and all-choice decompose choices. So choice
behaves a bit like a data constructor, or normal form, of the language. For this reason we call this
approach spatial choice, in contrast to approaches that eliminate choice by non-deterministically
picking one branch or the other, which immediately gives up confluence.

The rules for one and all expect multiple choices to be normalized into a right-associative list of
non-failing values, and the administrative rules assoc-choice, fail-l and fail-r bring nested choices
into that form. But why do these rules need a SX context? Again, they are needed to guarantee

7You may wonder if this local propagation is useful, a point we return to in Section 3.3.
8Remember, if is syntactic sugar for a use of one, see Section 2.5, but using if makes the example easier to understand.

The Verse Calculus: a Core Calculus for Functional Logic Programming

1:15

confluence. Suppose we had a rule fail-l-no-sx that unconditionally rewrites (fail e) to e. Now
consider these two reduction sequences, starting from the same expression:

f ⟨⟩; (fail
f ⟨⟩; (fail

(3 = (1 3))) −→{fail-l-no-sx}
(3 = (1 3))) −→{choose}
−→{ulit × 2}
−→{fail-l-no-sx × 2}

f ⟨⟩; 3 = (1 3)
f ⟨⟩; (fail
f ⟨⟩; (fail
f ⟨⟩; 3

((3 = 1)
(fail 3))

(3 = 3)))

The first sequence gets stuck after one step9, while the second makes more progress; and the two
results are not joinable.

3.6 VC is lenient
VC is lenient [Schauser and Goldstein 1995], not lazy (call-by-need), nor strict (call-by-value).
Under lenient evaluation, everything is eventually evaluated, but functions can run before their
arguments have a value. Consider a function call f (e). In VC applications are in administrative
normal form (ANF), so we must actually write ∃x. x = e; f (x). This expression will not return a
value until e reduces to a value: that is, everything is eventually evaluated. But even so f (x) can
proceed to beta reduce (Section 3.1), assuming we know the definition of f .

Lenience supports abstraction. For example, we can replace an expression (x = ⟨y, 3⟩; y > 7) by
∃f . f = (𝜆⟨p, q⟩. p = ⟨q, 3⟩; q > 7); f ⟨x, y⟩
Here, we abstract over the free variables of the expression, and define a named function f . Calling
the function is just the same as writing the original expression. This transformation would not be
valid under call-by-value.

This is not just a way to get parallelism, which was the original motivation for introducing
lenience in the data-flow language Id [Schauser and Goldstein 1995]; it affects semantics. Consider
∃f x y. f = (𝜆p. x = 7; p); y = (if (x > 0) then 7 else 8); f (y)
Here, y does not get a value until x is known; but x does not get its value (in this case 7) until f is
called. Without lenience this program would be stuck. Laziness would be another possible design
choice, one that is even more expressive, as we discuss in Appendix B.4.

3.7 Evaluation strategy
Any rewrite rule can apply anywhere in the term, at any time. For example in the term (x =
3 + 4; y = 3 ∗ 2; x + y) the rewrite rules do not say whether to rewrite 3 + 4 → 7 and then 3 ∗ 2 → 6,
or the other way around. The rules do, however, require us to reduce 3 + 4 → 7 before substituting
for x in x + y, because the deref rules only fire when the RHS is a value. By choosing rewrite rules
carefully, we can for example express call-by-name, call-by-name, and call-by-need [Ariola et al.
1995].

An evaluation strategy answers the question: given a closed term, which unique redex, out
of the many possible redexes, should I rewrite next to make progress towards the result? Any
decent evaluation strategy should (a) guarantee to terminate if there is any terminating sequence of
reductions; and (b) be amenable to compilation into efficient code. For example, in the pure lambda
calculus, normal-order reduction, sometimes called leftmost outermost reduction, is an evaluation
strategy that guarantees to terminate if any strategy does so.

It would be even better if the strategy could (c) guarantee to find the result in the minimal
number of rewrite steps—so called “optimal reduction” [Asperti and Guerrini 1999; Lamping 1990;

9The strange f ⟨⟩ prevents us using choose to float the (1 3) upwards.

1:16Lennart Augustsson, Joachim Breitner, Koen Claessen, Ranjit Jhala, Simon Peyton Jones, Olin Shivers, and Tim Sweeney

Lévy 1978]—but optimal reduction is typically very hard, even in theory, and invariably involves
reducing under lambdas, so for practical purposes it is well out of reach.

Formalising an evaluation strategy for VC is beyond the scope of this paper, but we can make
some informal comments. First, in service of (b) we envisage compiling lambdas to code, and thus
we never rewrite under a lambda [Peyton Jones 1987]. Second, it never makes sense to evaluate
in the right-hand argument of a choice10, because VC’s strong-ordering semantics mean that we
must first find out what the left-hand argument is (especially, whether it fails) before the right-hand
one can be used. So the basic plan is: rewrite the leftmost-outermost redex, subject to these two
constraints.

The trouble is that it is hard to say what the “leftmost” redex is. For example in (e; ⟨x, 3⟩ = ⟨2, y⟩),
the equality may or may not be the leftmost redex, depending on whether e is stuck (i.e., contains
no redexes); and whether or not e is stuck is a not syntactic property, and (worse) may depend not
only on e itself, but on its context. Even worse, e may subsequently become un-stuck when we
rewrite the equality. Any calculus in which a redex to the “right” may unblock one to the “left”—that
is, residuation—must grapple with this problem, so we leave evaluation strategy and compilation
for future work.

4 METATHEORY

The rules of our rewrite semantics can be applied anywhere, in any order, and they give meaning
to programs without committing to a particular evaluation strategy. But then it had better be the
case that no matter how the rules are applied, one always obtains the same result!
Reductions and Confluence A reduction R is a binary relation on a set of terms E. We write R𝑘
for the 𝑘-step closure of R and R∗ for the reflexive and transitive closure of R, i.e.R∗ ≡ ∪0⩽𝑘 R𝑘 . We
write 𝑒 −→R 𝑒 ′ (𝑎 steps to 𝑏) if (𝑒, 𝑒 ′) ∈ R and 𝑒 −→→R 𝑒 ′ (𝑎 reduces to 𝑏) if (𝑒, 𝑒 ′) ∈ R∗. A reduction R
is confluent if whenever 𝑒 −→→R 𝑒1 and 𝑒 −→→R 𝑒2, there exists an 𝑒 ′ such that 𝑒1 −→→R 𝑒 ′ and 𝑒2 −→→R 𝑒 ′.
Confluence gives us the assurance that we will not get different results when choosing different
rules, or get stuck with some rules and not with others.
Normal Forms A term 𝑒 is an R-Normal Form if there does not exist any 𝑒 ′ such that 𝑒 −→R 𝑒 ′.
Confluence implies that ultimately, rewriting terminates with at most one unique normal form,
regardless of the evaluation strategy [Barendregt 1984].

Lemma 4.1 (Unicity). If R is confluent then every term reduces to at most one normal form.

4.1 Confluence
Our main result is that VC’s reduction rules are confluent:

Theorem 4.2 (Confluence). The reduction relation defined in Fig. 3 and 4 is confluent.

Proof sketch. Our proof strategy is to divide the rules into groups, named U, A, etc in the Figures,
prove confluence for each separately, and then prove that their combination is confluent. Given
two reduction relations 𝑅 and 𝑆, we say that 𝑅 commutes with 𝑆 if for all terms 𝑒, 𝑒1, 𝑒2 such that
𝑒 −→→𝑅 𝑒1 and 𝑒 −→→𝑆 𝑒2 there exists 𝑒 ′ such that 𝑒1 −→→𝑆 𝑒 ′ and 𝑒2 −→→𝑅 𝑒 ′. We prove each individual
sub-relation is confluent; and that they pairwise commute. Then confluence of their union follows,
using Huet [1980]:

Lemma 4.3 (Commutativity). If 𝑅 and 𝑆 are confluent and commute, then 𝑅 ∪ 𝑆 is confluent.
Proving confluence for R, A, N , F and G is easy: they all satisfy the diamond property, namely,
that two different reduction steps can be joined at a common term by a single step. This property can

10Except perhaps in parallel, of course.

The Verse Calculus: a Core Calculus for Functional Logic Programming

1:17

be verified easily by taking critical pairs. Any relation satisfying the diamond property is confluent
[Barendregt 1984].

Alas, the unification relation U does not satisfy the diamond property, because it may need
multiple steps to join the results of two different one-step reductions. For example, consider the
term (x = ⟨1, y⟩; x = ⟨z, 2⟩; x = ⟨1, 2⟩; 3). It can be reduced in one step by substituting x in the
final equality by either ⟨1, y⟩ or ⟨z, 2⟩. After this it will take multiple steps to join the two terms.
Following a well-trodden path in proofs of confluence for the 𝜆-calculus (e.g. [Barendregt 1984]),
our proof of confluence for U works as follows: we find a sub-relation that satisfies three properties.
First, it is locally confluent, meaning if 𝑒 single-steps to 𝑒1 and 𝑒2 then 𝑒1 and 𝑒2 can be joined at
some 𝑒 ′. Second, it is terminating. Newman’s Lemma [Barendregt 1984] then implies the relation is
confluent; and hence so is its reflexive transitive closure. Third, that the closure of the sub-relation
is the same as the full reduction relation, which then implies that the full reduction relation is also
confluent.

4.2 Design for confluence
VC is carefully designed to ensure confluence. Rule deref-h is particularly important. It prevents
substituting heap values h under lambdas and inside tuples; and the A context only permits
substitution in a place where the value h can be used immediately, by application or unification.
These restrictions matter for at least three different reasons.
Nested tuples. Our proof strategy for the confluence of U requires that U terminates. But if
deref-h substituted inside tuples, or inside lambdas, it doesn’t terminate:

∃x. x = ⟨1, x⟩; x → ∃x. x = ⟨1, x⟩; ⟨1, x⟩ → ∃x. x = ⟨1, x⟩; ⟨1, ⟨1, x⟩⟩ → . . .

Here, each step makes one substitution for x. An exactly analogous example can be made for a
lambda value. We avoid this fruitless divergence by preventing deref-h from substituting under
tuples or lambdas. Instead an equality like (x = h) is left as a “heap-constraint” which can be used
(via deref-h) whenever we actually need to access the contents of the value, via unification or
appliction; or it can be eliminated via the garbage collection rules.

The odd/even problem. Suppose we combined deref-s and deref-h into a a single rule that freely
substituted any value v for an occurrence of x. Then we would lose confluence in the case of mutual
recursion:

∃x, y. x = ⟨1, y⟩; y = 𝜆z. x; x →∗ ∃y. y = 𝜆z. ⟨1, y⟩; ⟨1, y⟩
∃x, y. x = ⟨1, y⟩; y = 𝜆z. x; x →∗ ∃x. x = ⟨1, 𝜆z. x⟩; x

(1: substitute for x first)
(2: substitute for y first)

The result of (1) and (2) have the same meaning (are indistinguishable by a VC context) but cannot
be joined by rewrite rules. This is a well known problem, and an exactly similar phenomenon
arises with inlining mutually recursive 𝜆-terms. Examples like this show that syntactic confluence
is too strong: what we really need is that our rewrites rules are semantics preserving — but of
course that requires an independent notion of semantics (see Appendix C for an initial attempt).
We restore confluence by restricting deref-h, but an interesting alternative approach would be to
seek a weaker form of confluence, such as skew confluence [Ariola and Blom 2002].
Unifying lambdas. In VC an attempt to unify two lambdas fails, even if the lambdas are semanti-
cally identical (rule u-fail). Why? Because semantic identity of functions is un-implementable. We
cannot instead say that the attempt to unify gets stuck, because that leads to non-confluence. Here
is an expression that rewrites in two different ways, depending on which equality we deref-h first:
(𝜆p. 1) = (𝜆q. 2); 1 ←−∗ ∃x. x = (𝜆p. 1); x = (𝜆q. 2); x ⟨⟩ −→∗ (𝜆q. 2) = (𝜆p. 1); 2

1:18Lennart Augustsson, Joachim Breitner, Koen Claessen, Ranjit Jhala, Simon Peyton Jones, Olin Shivers, and Tim Sweeney

These two outcomes cannot be joined. But making unification on lambdas fail, both outcomes lead
to fail, and confluence is restored.

There is a very delicate interaction between u-fail and the apparently-innocuous rule u-scalar
(Fig. 3). Consider (∃x. x = (𝜆y. y); x = x; 0). If we apply u-scalar, and then deref-h we get (𝜆y. y).
But if we first apply deref-h, twice, we get ((𝜆y. y) = (𝜆y. y); 0), and that fails. Yikes!

But in fact all is well: the A context (Fig. 3) only allows deref-h to apply in positions where the
value is immediately consumed in some way, by being applied to an argument, or being unified
with a value. So in our example, deref-h simply does not apply. Only u-scalar does, so we get
(∃x. x = (𝜆y. y); x; 0). Confluence is restored. But the ice is thin here, so it is reassuring that we
have a proof of confluence.

5 VC IN CONTEXT: REFLECTIONS AND RELATED WORK
Functional logic programming has a rich literature; an excellent starting point is Hanus’s sur-
vey [Hanus 2013]. Now that we know what VC is, we can identify its distinctive features, and
compare them to other approaches.

5.1 Choice and non-determinism

A significant difference between our presentation and earlier works is our treatment of choice.
Consider an expression like (3 + (20 30)). This is typically handled by a pair of non-deterministic
rewrite rules:

e1 e2 −→ e1

e1 e2 −→ e2

So our expression rewrites (non-deterministically) to either (3 + 20) or (3 + 30); and that in turn
allows the addition to make progress. Of course, including non-deterministic choice means the
rules are non-confluent by construction. Instead, one must generalize to say that a reduction
does not change the set of results; in the context of lambda calculi see for example [Kutzner and
Schmidt-Schauß 1998; Schmidt-Schauß and Machkasova 2008].

In contrast, our rules never pick one side or the other of a choice. And yet (3 + (20 30)) can
still make progress, by floating out the choice (rule choose in Fig. 3), thus (3 + 20)
(3 + 30).
In effect, choices are laid out in space (in the syntax of the term), rather than being explored by
non-deterministic selection. Rule choose is not a new idea: it is common in calculi with choice, see
e.g., [de’Liguoro and Piperno 1995, Section 6.1, Dal Lago et al. 2020, Section 3] and, more recently,
has been used to describe functional logic languages where it is variously called bubbling [Antoy
et al. 2006] or pull-tabbing [Antoy 2011]. However, our formulation appears simpler, because we
avoid the need for attaching an identifier to each choice with its attendant complications.

5.2 One and all

Logical variables, choice, and equalities are present in many functional logic languages. However
one and all are distinctive features of VC, with the notable exception of Smolka et al.’s language
Fresh. Introduced in a technical report nearly 40 years ago [Smolka and Panangaden 1985], Fresh
has confinement (equivalent to one) and collection (equivalent to all). It is a very interesting design,
but one does not appear to have been implemented, and its treatment of equality and thus logical
variables is rather different to ours.

Several aspects of all and one are worth noting. First, all reifies choice (a control operator)
into a tuple (a data structure); for example, all{1 7 2} returns the tuple ⟨1, 7, 2⟩. In the other
direction, indexing turns a tuple into choice (e.g., ∃i. ⟨1, 7, 2⟩(i) yields (1 7 2)). Other languages
can reify choices into a (non-deterministic) list, via an operator called bagof, or a mechanism called
set-functions in an extension of Curry [Antoy and Hanus 2021, Section 4.2.7], implemented in the

The Verse Calculus: a Core Calculus for Functional Logic Programming

1:19

Kiel Curry System interpreter [Antoy and Hanus 2009; Brassel and Huch 2007, 2009]. But this is
regarded as a somewhat sophisticated feature, whereas it is part of the foundational fabric of VC.
Curry’s set-functions need careful explanation about sharing across non-deterministic choices,
or what is “inside” and “outside” the set function, something that appears as a straightforward
consequence of VC’s single rule choose.

Second, even under the reification of all, VC is deterministic. Choice is not non-deterministic:
VC takes pains to maintain order, so that when reifying choice into a tuple, the order of elements
in that tuple is completely determined. This determinism has a price: as we saw in Section 2.3 and
Section 3.5, we have to take care to maintain the left-to-right order of choices. However, maintaining
that order has other payoffs. For example, it is relatively easy to add effects other than choice,
including mutable variables and input/output, to VC.

Thirdly, one allows us to reify failure; to try something and take different actions depending on
whether or not it succeeds. Prolog’s “cut” operator has a similar flavour, and Curry’s set-functions
allow one to do the same thing.

Finally, one and all neatly encapsulate the idea of “flexible” vs. “rigid” logical variables. As we
saw in Section 2.5, logical variables bound outside one/all cannot be unified inside it; they are
“rigid.” This notion is nicely captured by the fact that equalities cannot float outside one and all
(Section 3.4).

5.3 The semantics of logical variables
Our logical variables, introduced by ∃, are often called extra variables in the literature, because they
are typically introduced as variables that appear on the right-hand side of a function definition, but
are not bound on the left. For example, in Curry we can write
first x | x =:= (a,b) = a where a,b free
Here a and b are logical variables, not bound on the left; they get their values through unification
(written “=:=”). In Curry they are explicitly introduced by the “where a,b free” clause, while in
many other papers their introduction is implicit in the top-level rules, simply by not being bound on
the left. These extra variables (our logical variables) are at the heart of the “logic” part of functional
logic programming.

Constructor-based ReWrite Logic (CRWL) [González-Moreno et al. 1999] is the brand leader
for high-level semantics for non-strict, non-deterministic functional logic languages. CRLW is a
“big-step” rewrite semantics that rewrites a term to a value in a single step. López-Fraguas et al.
[2007] make a powerful case for instead giving the semantics of a functional logic language using
“small-step” rewrite rules, more like those of the lambda calculus, that successively rewrite the
term, one step at a time, until it reaches a normal form. Their paper does exactly this, and proves
equivalence to the CRWL framework. Their key insight (like us, inspired by Ariola et al. [1995]’s
formalisation of the call-by-need lambda calculus) is to use let to make sharing explicit.

However both CRWL and Fraguas et al. suffer from a major problem: they require something we

call magical rewriting. A key rewrite rule is this:

𝑓 (𝜃 (𝑒1), . . . , 𝜃 (𝑒𝑛)) −→ 𝜃 (𝑟ℎ𝑠)
if (𝑒1, . . . , 𝑒𝑛) −→ 𝑟ℎ𝑠 is a top-level function binding, and

𝜃 is a substitution mapping variables to closed values, s.t 𝑑𝑜𝑚(𝜃 ) = fvs(𝑒1, . . . , 𝑒𝑛, 𝑟ℎ𝑠)

The substitution for the free variables of the left-hand side can readily be chosen by matching the
left-hand side against the call. But the substitution for the extra variables must be chosen “magically”
[López-Fraguas et al. 2007, Section 7] or clairvoyantly, so as to make the future execution work
out. This is admirably high level, because it hides everything about unification, but it is not much

1:20Lennart Augustsson, Joachim Breitner, Koen Claessen, Ranjit Jhala, Simon Peyton Jones, Olin Shivers, and Tim Sweeney

help to a programmer trying to understand a program, nor is it directly executable. In a subsequent
journal paper they refine CRWL to avoid magical rewriting using “let-narrowing” [López-Fraguas
et al. 2014, Section 6]; this system looks rather different to ours, especially in its treatment of choice,
but is rather close in spirit.

To explain actual execution, the state of the art is described by Albert et al. [2005]. They give both
a big-step operational semantics (in the style of [Launchbury 1993]), and a small-step operational
semantics. These two approaches both thread a heap through the execution, which holds the
unification variables and their unification state; the small-step semantics also has a stack, to specify
the focus of execution. The trouble is that heaps and stacks are difficult to explain to a programmer,
and do not make it easy to reason about program equivalence. In addition to this machinery, the
model is further complicated with concurrency to account for residuation.

In contrast, our rewrite rules give a complete, executable (i.e., no “magic”) account of logical
variables and choice, directly as small-step rewrites on the original program, rather than as the
evolution of a (heap, control, stack) configuration. Moreover, we have no problem with residuation.

5.4 Flat vs. higher order

When giving the semantics of functional logic languages, a first-order presentation is almost univer-
sal. User-defined functions can be defined at top level only; and function symbols (the names of such
functions) are syntactically distinguished from ordinary variables. As Hanus describes, it is possible
to translate a higher-order program into a first-order form11 using defunctionalisation [Hanus 2013,
Section 3.3], and a built-in apply function. Sadly, this encoding is hardly a natural rendition of
the lambda calculus, and it obstructs the goal of using rewrite rules to explain to programmers
how their program might execute. In contrast, a strength of our VC presentation is that it deals
natively with the full lambda calculus.

5.5 Intermediate language
Hanus’s Flat Language [Albert et al. 2005, Fig 1], FLC, plays the same role as VC: it is a small core
language into which a larger surface language can be desugared. There are some common features:
variables, literals, constructor applications, and sequencing (written hnf in FLC). However, it seems
that VC has a greater economy of concepts. In particular, FLC has two forms of equality (==) and
(=:=), and two forms of case-expression, case and fcase. In each pair, the former suspends if
it encounters a logical variable; the latter unifies or narrows respectively. In contrast, VC has a
single equality (=), and the orthogonal one construct, to deal with all four concepts.

FLC has let-expressions (let x=e in b), where VC uses ∃ and (again) unification. FLC also
uses the same construct for a different purpose, to bring a logical variable into scope, using the
strange binding x=x, thus (let x=x in e). In contrast, ∃x. e seems more direct.

6 LOOKING BACK, LOOKING FORWARD
The semantics of VC is designed at a level intended to capture the computational model of the
language; not all formal semantics do so. Defining a language by giving its low-level semantics is
precise but not necessarily illuminating. For example, giving a reference compiler that compiles the
program to x86 instructions is precise, but is not helpful to a human who is trying to understand
exactly what the original program meant. Likewise, a high-level semantics simply provides the
eventual answer produced by the program, without insight into the computational steps that got us
from program start to program completion.

11Hanus does not mention this, but for a language with arbitrarily nested lambdas one would need to do lambda-lifting as
well, but that is perhaps a minor point.

The Verse Calculus: a Core Calculus for Functional Logic Programming

1:21

The moral here is that formal specifications can be obfuscatory—or illuminating. The latter kind
shed light because they are defined in terms of the intended mechanisms of the language. VC does
this; it respects the conceptual structures of the Verse language.

Note that when we say “illuminating” we mean that in multiple ways. A semantics can be
illuminating for humans who are trying to understand what a particular program does, or how
a proposed change to the language will affect the language. It can drive analyses that help the
compiler optimize programs. It underlies the use of formal methods and verification to provide
machine-derived and -checkable proofs of correctness. All of these applications depend on the
semantics being defined at the appropriate level: the level of the computational model that underlies
the language. This has been our goal in this work.

We have much left to do. The full Verse language has statically checked types. In the dynamic
semantics, the types can be represented by partial identity functions—identity of the values of
the type and fail otherwise. This gives a distinctive new perspective on type systems, one that
we intend to develop in future work. The full Verse language also has a statically-checked effect
system, including both mutable references and input/output. All these effects must be transactional,
e.g., when the condition of an if fails, any store effects in the condition must be rolled back. We
have preliminary reduction rules for updateable references, but they are not included here.

The Verse Calculus: a Core Calculus for Functional Logic Programming

1:23

A EXAMPLE

A complete reduction sequence for a small example can be found in figure 6. This example shows
how constraining the output of a function call can constrain the argument. While most of the
reductions are administrative in nature, these are the highlights: At 1○ the swap function is inlined
so that at 2○ a 𝛽-reduction can happen.

−→{desugar}

1○ −→{deref-h,elim-def}
−→{norm-seq-swap}

2○ −→{app-beta}

−→{norm-defr,norm-seq-defr}
−→{norm-seq-defl}
−→{norm-seqr,norm-seq-assoc}
−→{norm-seq-swap}
3○ −→{deref-s,elim-def}

swap ⟨x, y ⟩ (cid:66) ⟨y, x ⟩; ∃p. swap (p) = ⟨2, 3⟩; p
∃swap. swap = (𝜆xy. ∃x y. ⟨x, y ⟩ = xy; ⟨y, x ⟩); ∃p t. t = swap (p); t = ⟨2, 3⟩; p
∃p t. t = (𝜆xy. ∃x y. ⟨x, y ⟩ = xy; ⟨y, x ⟩) (p); t = ⟨2, 3⟩; p
∃p t. t = ⟨2, 3⟩; t = (𝜆xy. ∃x y. ⟨x, y ⟩ = xy; ⟨y, x ⟩) (p); p
∃p t. t = ⟨2, 3⟩; t = ∃xy. (xy = p; ∃x y. ⟨x, y ⟩ = xy; ⟨y, x ⟩); p
∃p t. t = ⟨2, 3⟩; ∃xy. t = (xy = p; ∃x y. ⟨x, y ⟩ = xy; ⟨y, x ⟩); p
∃p t xy. t = ⟨2, 3⟩; t = (xy = p; ∃x y. ⟨x, y ⟩ = xy; ⟨y, x ⟩); p
∃p t xy. t = ⟨2, 3⟩; xy = p; t = ∃x y. ( ⟨x, y ⟩ = xy; ⟨y, x ⟩); p
∃p t xy. xy = p; t = ⟨2, 3⟩; t = ∃x y. ( ⟨x, y ⟩ = xy; ⟨y, x ⟩); p
∃p t. t = ⟨2, 3⟩; t = ∃x y. ( ⟨x, y ⟩ = p; ⟨y, x ⟩); p
∃p t. t = ⟨2, 3⟩; ∃x. t = ∃y. ( ⟨x, y ⟩ = p; ⟨y, x ⟩); p
∃p t x. t = ⟨2, 3⟩; t = ∃y. ( ⟨x, y ⟩ = p; ⟨y, x ⟩); p
∃p t x. t = ⟨2, 3⟩; ∃y. t = ( ⟨x, y ⟩ = p; ⟨y, x ⟩); p
∃p t x y. t = ⟨2, 3⟩; t = ( ⟨x, y ⟩ = p; ⟨y, x ⟩); p
∃p t x y. t = ⟨2, 3⟩; ( ⟨x, y ⟩ = p; t = ⟨y, x ⟩); p
∃p x y. ( ⟨x, y ⟩ = p; ⟨2, 3⟩ = ⟨y, x ⟩); p

−→{norm-defr,norm-seq-defr}
−→{norm-seq-defl}
−→{norm-defr,norm-seq-defr}
−→{norm-seq-defl}
−→{norm-seqr}
−→{deref-h,elim-def}
−→{norm-seq-assoc,norm-swap-eq} ∃p x y. p = ⟨x, y ⟩; ⟨2, 3⟩ = ⟨y, x ⟩; p

4○ −→{u-tup,norm-seq-assoc}

−→{norm-swap-eq}
−→{norm-seq-swap}
5○ −→{deref-s,elim-def}
−→{norm-seq-assoc}
−→{norm-swap-eq}
−→{norm-seq-swap}
6○ −→{deref-s,elim-def}
7○ −→{norm-val}

−→{post-reduction-inline}

∃p x y. p = ⟨x, y ⟩; 2 = y; (3 = x; ⟨2, 3⟩); p
∃p x y. p = ⟨x, y ⟩; y = 2; (3 = x; ⟨2, 3⟩); p
∃p x y. y = 2; p = ⟨x, y ⟩; (3 = x; ⟨2, 3⟩); p
∃p x. p = ⟨x, 2⟩; (3 = x; ⟨2, 3⟩); p
∃p x. p = ⟨x, 2⟩; 3 = x; ⟨2, 3⟩; p
∃p x. p = ⟨x, 2⟩; x = 3; ⟨2, 3⟩; p
∃p x. x = 3; p = ⟨x, 2⟩; ⟨2, 3⟩; p
∃p. p = ⟨3, 2⟩; ⟨2, 3⟩; p
∃p. p = ⟨3, 2⟩; p
⟨3, 2⟩

Fig. 6. Sample reduction sequence

Step 3○ inlines the argument, and 4○ does the matching of the tuple. At 5○ and 6○ the actual

numbers are inline. After removing some garbage we reach the result at 7○.

B VARIATIONS AND CHOICES
In a calculus like VC there is room for many design variations. We discuss some of them here.

B.1 Dead existentials
Consider the term (∃x. 99). This rewrites to 99 by def-elim, but you could argue that it should
instead be stuck. For example, the term (∃x. x = (1 2); 99) rewrites to (99 99), producing two
results, one for each solution for x. So, if x is entirely unconstrained, maybe we should return an
infinite number of results? It would be easy to change this decision, by adjusting the rules in Fig. 5
for well-formed results.

B.2 Ordering and choices
As we discussed in Section 3.5, rule choose is less than satisfying, for two reasons. First, the 𝐶𝑋
context uses a conservative, syntactic analysis for choice-free expressions; and second, the SX
context is needed to force 𝐶𝑋 to be maximal. A rule like this would be more satisfying:

simpler-choose 𝐶𝑋 [ e1 e2 ] −→ 𝐶𝑋 [ e1 ] 𝐶𝑋 [ e2 ]

The trouble with that is that it may change the order of the results (Section 2.3). Another possibility
would be to accept that results may come out in the “wrong” order, but have some kind of sorting
mechanism to put them back into the “right” order. Something like this:

labeled-choose 𝐶𝑋 [ e1 e2 ] −→ 𝐶𝑋 [ L; e1 ] 𝐶𝑋 [ R; e2 ]

1:24Lennart Augustsson, Joachim Breitner, Koen Claessen, Ranjit Jhala, Simon Peyton Jones, Olin Shivers, and Tim Sweeney

Here the two branches are labeled with L and R. We can add new rules to reorder such labelled
expressions, something in the spirit of

sort

(R; e1)

(L; e2) −→ (L; e2)

(R; e1)

We believe this can be made to work, and it would allow more programs to evaluate, but it adds
unwelcome clutter to program terms, and the cure may be worse than the disease.

B.3 Generalizing one and all
In VC, we introduced one and all as the primitive choice-consuming operators, and neither is
more general than the other, as discussed in Section 2.6. We could have introduced a more general
operator split as 𝑒 ::= · · · | split{e, v1, v2} and rules

split-fail
split-choice
split-value

split{fail, f , g} −→ f ⟨⟩

split{e1

e2, f , g} −→ g⟨e1, 𝜆𝑥 . e2⟩
split{e, f , g} −→ g⟨e, 𝜆𝑥 . fail⟩

if ∅ ⊢ e1 ⇝ (𝑥 | 𝑐 | v), x fresh
if ∅ ⊢ e ⇝ (𝑥 | 𝑐 | v), x fresh

The intuition behind split is that it distinguishes a failing computation from one that returns at
least one value. If e fails, it calls f , and if e returns at least one value, passes that to g together with
the remaining computation, safely tucked away behind a lambda.

Indeed, this is more general, as we can implement one and all with split:

one{e} ≡ f (x) (cid:66) fail; g⟨x, y⟩ (cid:66) x;
all{e} ≡ f (x) (cid:66) ⟨⟩;

split{e, f , g}
g⟨x, y⟩ (cid:66) cons⟨x, split{y⟨⟩, f , g}⟩; split{e, f , g}

For this paper we stuck to the arguably simpler one and all, to avoid confusing the presentation
with these higher-order encodings, but there are no complications using split instead.

B.4 Laziness
As Section 3.6 discussed, VC is lenient. Unlike Curry however, VC is not lazy. For example,
consider: ∃x. x = loop⟨⟩; 3. In a lazy language this expression would yield 3, but in VC everything
is evaluated, and the infinite computation loop⟨⟩ will prevent the expression from returning a value.
There a good reason for this choice: the call to loop⟨⟩ might fail, and we should not return 3 until
we know there is no failure. With laziness we could easily lose confluence.

Another place that laziness could play a role is this. Remembering the duality between values
and choices, one might also want all to return a lazy stream of results, one by one, rather than
waiting for them all to terminate. For example, one might hope that this program would converge:

∃y z. ⟨y, z⟩ = all{∃onec. onec = (1 onec)}; y

Here we suppose that all returns a lazy stream of values (represented as nested pairs), from which
we may pick the first and discard the rest.

In short, there are good reasons for lenience, but a lazy variant of VC could be worth exploring.

C A DENOTATIONAL SEMANTICS FOR VC
It is highly desirable to have a denotational semantics for VC. A denotational semantics says
directly what an expression means rather than how it behaves, and that meaning can be very
perspicuous. Equipped with a denotational semantics we can, for example, prove that the left hand
side and right hand side of each rewrite rule have the same denotation; that is, the rewrites are
meaning-preserving.

The Verse Calculus: a Core Calculus for Functional Logic Programming

1:25

Domains

𝑊 = Z + ⟨𝑊 ⟩ + (𝑊 → 𝑊 ∗)
⟨𝑊 ⟩ = a finite tuple of values 𝑊
𝐸𝑛𝑣 = 𝐼𝑑𝑒𝑛𝑡 → 𝑊

Semantics of expressions and values

:

𝜌)

(cid:75)
(cid:75)
(cid:75)

𝑣
(cid:74)

E
E

𝐸𝑛𝑣 → 𝑊 ∗

e
(cid:75)
𝜌 = unit (V
(cid:75)
𝜌 = empty
𝜌 ⋓ E
e2
e1
𝜌
𝜌 = E
(cid:75)
(cid:74)
(cid:74)
𝜌 ⋒ E
e2
e1
𝜌
𝜌 = E
(cid:74)
(cid:75)
(cid:74)
e2
e1
𝜌
𝜌 = E
𝜌
E
(cid:74)
(cid:75)
(cid:35)
(cid:74)
v2
v1
𝜌 = apply(V
𝜌)
𝜌, V
(cid:74)
(cid:75)
(cid:74)
𝜌 = (cid:208)𝑤 ∈𝑊 E
(𝜌 [𝑥 ↦→ 𝑤])
𝑒
(cid:75)
(cid:74)
𝜌 = one(E
𝜌)
𝑒
(cid:75)
(cid:74)
𝜌 = unit (all(E
𝑒
(cid:74)

E
(cid:74)
v
E
(cid:74)
(cid:75)
E
fail
(cid:75)
(cid:74)
e1 e2
(cid:74)
(cid:75)
e1 = e2
(cid:75)
(cid:74)
e1; e2
E
(cid:75)
(cid:74)
v1 v2
E
(cid:74)
(cid:75)
∃x. e
E
(cid:74)
(cid:75)
one{e}
E
(cid:75)
(cid:74)
all{e}
E
(cid:75)
v
(cid:75)
𝜌 = 𝜌 (𝑥)
𝜌 = 𝑘
𝑜𝑝
𝜌 = O
(cid:74)
(cid:75)
𝑒
𝜌 = 𝜆𝑤 .E
(cid:74)
𝑣1
𝜌 = ⟨V
(cid:75)
(cid:74)

V
(cid:74)
x
V
(cid:74)
(cid:75)
k
V
(cid:75)
(cid:74)
𝑜𝑝
V
(cid:75)
(cid:74)
𝜆x. e
V
(cid:75)
(cid:74)
⟨v1, · · · , vn⟩
(cid:75)
op

(𝜌 [𝑥 ↦→ 𝑤])
(cid:75)
𝑣𝑛
𝜌, · · · , V
(cid:74)

(cid:75)

𝜌⟩

𝐸𝑛𝑣 → 𝑊

𝜌))

(cid:75)

(cid:74)

(cid:75)

:

V

(cid:74)

: 𝑊
= 𝜆𝑤 . if (𝑤 = ⟨k1, k2⟩) then unit (𝑘1 + 𝑘2) else WRONG
= 𝜆𝑤 . if (𝑤 = ⟨k1, k2⟩ ∧ 𝑘1 > 𝑘2) then unit (𝑘1) else empty
= 𝜆𝑤 . if (𝑤 = 𝑘) then unit (𝑘) else empty

O

O
(cid:75)
(cid:74)
add
(cid:75)
(cid:74)
O
gt
(cid:75)
(cid:74)
O
int
(cid:75)
𝑎𝑝𝑝𝑙𝑦

(cid:74)

:
𝑎𝑝𝑝𝑙𝑦 (𝑘, 𝑤) = WRONG
𝑎𝑝𝑝𝑙𝑦 (⟨𝑣0, . . . , 𝑣𝑛⟩, 𝑘) = unit (𝑣𝑘 )

(𝑊 × 𝑊 ) → 𝑊 ∗

= empty
𝑎𝑝𝑝𝑙𝑦 (𝑓 , 𝑤) = 𝑓 (𝑤)

𝑘 ∈ Z
0 ⩽ 𝑘 ⩽ 𝑛
otherwise
𝑓 ∈ 𝑊 → 𝑊 ∗

Fig. 7. Expression semantics

But a denotational semantics for a functional logic language is tricky. Typically one writes a

denotation function something like

E

: 𝐸𝑛𝑣 → 𝑊

e
(cid:74)

(cid:75)

where 𝐸𝑛𝑣 = 𝐼𝑑𝑒𝑛𝑡 → 𝑊 . So E takes an expession e and an environment 𝜌 : 𝐸𝑛𝑣 and returns the
value, or denotation, of the expresssion. The environment binds each free variable of e to its value.
But what is the semantics of ∃x. e? We need to extend 𝜌 with a binding for x, but what is x bound
to? In a functional logic language x is given its value by various equalities scattered throughout e.

1:26Lennart Augustsson, Joachim Breitner, Koen Claessen, Ranjit Jhala, Simon Peyton Jones, Olin Shivers, and Tim Sweeney

Domains
𝑊 ∗ = (WRONG + P (𝑊 ))⊥

Operations
Empty

empty
: 𝑊 ∗
empty = { }

Unit

unit

: 𝑊 → 𝑊 ∗

unit (𝑤) = {𝑤 }

Union

⋓ : 𝑊 ∗ → 𝑊 ∗ → 𝑊 ∗

𝑠1 ⋓ 𝑠2 = 𝑠1 ∪ 𝑠2

Intersection

⋒ : 𝑊 ∗ → 𝑊 ∗ → 𝑊 ∗

𝑠1 ⋒ 𝑠2 = 𝑠1 ∩ 𝑠2

: 𝑊 ∗ → 𝑊 ∗ → 𝑊 ∗

Sequencing

𝑠1

(cid:35)

(cid:35)
𝑠2 = 𝑠2
= { }
: 𝑊 ∗ → 𝑊 ∗

one

one(𝑠) = ???

all

: 𝑊 ∗ → ⟨𝑊 ⟩

all(𝑠) = ???

One

All

if 𝑠1 is non-empty
otherwise

The result is either empty or a singleton

All operations over 𝑊 ∗ implicitly propagate ⊥ and WRONG. E.g.
𝑠1 ⋓ 𝑠2 = ⊥

if 𝑠1 = ⊥ or 𝑠2 = ⊥

= WRONG if (𝑠1 = WRONG and 𝑠2 ≠ ⊥) or (𝑠2 = WRONG and 𝑠1 ≠ ⊥)
= 𝑠1 ∪ 𝑠2

otherwise

Fig. 8. Set semantics for 𝑊 ∗

This section sketches our approach to this challenge. It is not finished work, and does not count
as a contribution of our paper. We offer it because we have found it an illuminating alternative way
to understand VC, one that complements the rewrite rules that are the substance of the paper.

C.1 A first attempt at a denotational semantics
Our denotational semantics for VC is given in Fig. 7.

• We have one semantic function (here E and V) for each syntactic non terminal (here 𝑒 and 𝑣

respectively.)

• Each function has one equation for each form of the construct.
• Both functions take an environment 𝜌 that maps in-scope identifiers to a single value; see

• The value function V returns a single value 𝑊 , while the expression function E returns a

the definition 𝐸𝑛𝑣 = 𝐼𝑑𝑒𝑛𝑡 → 𝑊 .

collection of values 𝑊 ∗ (Appendix C.1).

The semantics is parameterised over the meaning of a “collection of values 𝑊 ∗”. To a first
approximation, think of 𝑊 ∗ a (possibly infinite) set of values 𝑊 , with union, intersection etc having
their ordinary meaning.

The Verse Calculus: a Core Calculus for Functional Logic Programming

1:27

Our first interpretation, given in Figure 8, is a little more refined: 𝑊 ∗ includes ⊥ and WRONG as
well as a set of values. Our second interpretation is given in Figure 9, and discussed in Appendix C.4.
The equations themselves, in Fig. 7 are beautifully simple and compositional, as a denotational

semantics should be.

The equations for V are mostly self-explanatory, but an equation like V

𝜌 = 𝑘 needs some
explanation: the 𝑘 on the left hand side (e.g. “3”) is a piece of syntax, but the 𝑘 on the right is
the corresponding element of the semantic world of values 𝑊 (e.g. 3). As is conventional, albeit
a bit confusing, we use the same 𝑘 for both. Same for 𝑜𝑝, where the semantic equivalent is the
corresponding mathematical function.

(cid:74)

(cid:75)

k

The equations for E are more interesting.

• Values E

v

𝜌: compute the single value for v, and return a singleton sequence of results.

The auxiliary function unit is defined at the bottom of Fig. 7.

(cid:74)

(cid:75)

(cid:75)
e1
(cid:74)

• In particular, values include lambdas. The semantics says that a lambda evaluates to a singleton
collection, whose only element is a function value. But that function value has type 𝑊 → 𝑊 ∗;
that is, it is a function that takes a single value and returns a collection of values.

• Function application E

𝜌 is easy, because V returns a single value: just apply the
meaning of the function to the meaning of the argument. The apply function is defined in
Figure 7.
• Choice E

𝜌: take the union (written ⋓) of the values returned by e1 and e2 respectively.

e1 e2

v1 v2

(cid:74)

(cid:75)

For bags this union operator is just bag union (Figure 8).

(cid:74)

(cid:75)

(cid:75)

e2

• Unification E

𝜌: take the intersection of the values returned by e1 and e2 respec-
tively. For bags, this “intersection” operator ⋒ is defined in Fig. 8. In this definition, the
equality is mathematical equality of functions; which we can’t implement for functions; see
Appendix C.1.
• Sequencing E

𝜌. Again we use an auxiliary function

to combine the meanings of
(cid:35)
(Fig. 8 again) uses a bag comprehension. Again it does a

e1; e2
(cid:74)
e1 and e2. For bags, the function
cartesian product, but without the equality constraint of ⋒.

• The semantics of (one{e}) simply applies the semantic function one : 𝑊 ∗ → 𝑊 ∗ to the
collection of values returned by e. If e returns no values, so does (one{e}); but if e returns one
or more values, (one{e}) returns the first. Of course that begs the question of what “the first”
means – for bags it would be non-deterministic. We will fix this problem in Appendix C.4,
but for now we simply ignore it.

• The semantics of (all{e}) is similar, but it always returns a singleton collection (hence the
unit in the semantics of all{·}) whose element is a (possibly-empty) tuple that contains all
the values in the collection returned by e.

(cid:35)

The fact that unification “=” maps onto intersection, and choice “ ” onto union, is very satisfying.
The big excitement is the treatment of ∃. We must extend 𝜌, but what should we bind x to?
, where we have a value 𝑤 to hand.) Our answer is simple: try

(Compare the equation for V
all possible values, and union the results:

𝜆x. e
(cid:74)

(cid:75)

E

∃x. e
(cid:74)

(cid:75)

𝜌 =

(cid:216)

𝑤 ∈𝑊

E

𝑒
(cid:74)

(cid:75)

(𝜌 [𝑥 ↦→ 𝑤])

That (cid:208)𝑤 ∈𝑊 means: enumerate all values in 𝑤 ∈ 𝑊 , in some arbitrary order, and for each: bind 𝑥 to
(𝜌 [𝑥 ↦→ 𝑤]), and take the union (in the
𝑤, find the semantics of 𝑒 for that value of 𝑥, namely E
sense of ⋓) of the results.

𝑒
(cid:74)

(cid:75)

Of course we can’t possibly implement it like this, but it makes a great specification. For example
∃x. x = 3 tries all possible values for x, but only one of them succeeds, namely 3, so the semantics
is a singleton sequence [3].

1:28Lennart Augustsson, Joachim Breitner, Koen Claessen, Ranjit Jhala, Simon Peyton Jones, Olin Shivers, and Tim Sweeney

C.2 The denotational semantics is un-implementable

This semantics is nice and simple, but we definitely can’t implement it! Consider

∃x. (x2 − x − 6) = 0; x
The semantics will iterate over all possible values for x, returning all those that satisfy the equality;
including 3, for example. But unless our implementation can guarantee to solve quadratic equations,
we can’t expect it to return 3. Instead it’ll get stuck.

Another way in which the implementation might get stuck is through unifying functions:

(𝜆x. x + x) = (𝜆y. y ∗ 2)

or even (𝜆x. x + 1) = (𝜆y. y + 1)

But not all unification-over-functions is ruled out. We do expect the implementation to succeed
with

∃f . ((𝜆x. x + 1) = f ); f 3
Here the ∃ will iterate over all values of f , and the equality will pick out the (unique) iteration in
which f is bound to the incrementing function.

So our touchstone must be:

• If the implementation returns a value at all, it must be the value given by the semantics.
• Ideally, the verifier will guarantee that the implementation does not get stuck, or go WRONG.

C.3 Getting WRONG right
Getting WRONG right is a bit tricky.

• What is the value of (3 = ⟨⟩)? The intersection semantics would say empty, the empty

collection of results, but we might want to say WRONG.

• Should WRONG be an element of 𝑊 or of 𝑊 ∗? We probably want (one{3 wrong} to

return a unit (3) rather then WRONG?

• What about fst (⟨3, wrong⟩)? Is that wrong or 3?
There is probably more than one possible choice here.

C.4 An order-sensitive denotational semantics
There is a Big Problem with this approach. Consider ∃x. x = (4 3). The existential enumerates all
possible values of x in some arbitrary order, and takes the union (i.e. concatention) of the results
from each of these bindings. Suppose that ∃ enumerates 3 before 4; then the semantics of this
expression is the sequence [3, 4], and not [4, 3] as it should be. And yet returning a sequence (not a
set nor a bag) is a key design choice in Verse. What can we do?

Figure 9 give a new denotational semantics that does account for order. The key idea (due to
Joachim Breitner) is this: return a sequence of labelled values; and then sort that sequence (in one
and all) into canonical order before exposing it to the programmer.

We do not change the equations for E, V, and O at all; they remain precisely as they are in
Figure 7. However the semantics of a collection of values, 𝑊 ∗, does change, and is given in Figure 9:
• A collection of values 𝑊 ∗ is now ⊥ or WRONG (as before), or a set of labelled values, each of

type 𝐿𝑊 .

• A labelled value (of type 𝐿𝑊 ) is just a pair ([𝐿] × 𝑊 ) of a label and a value.
• A label is a sequence of tags 𝐿, where a tag is just L or R.
• The union (or concatention) operation ⋓, defined in Fig. 9, adds a L tag to the labels of the
values in the left branch of the choice, and a R tag to those coming from the right. So the
labels specify where in the tree the value comes from.

and ⋒ both concatenate the labels from the values they combine.

• Sequencing

(cid:35)

The Verse Calculus: a Core Calculus for Functional Logic Programming

1:29

Domains
𝑊 ∗ = (WRONG + P (𝐿𝑊 ))⊥
𝑊 ? = {𝑊 }
𝐿𝑊 = [𝐿] × 𝑊
𝐿 = L + R

Set with 0 or 1 elements
Sequence of 𝐿 and a value

Operations
Empty

Singleton

Union

Intersection

Sequencing

One

All

Head

To tuple

empty
: 𝑊 ∗
empty = ∅
unit (.)
: 𝑊 → 𝑊 ∗
unit (𝑤) = {([], 𝑤)}

⋓ : 𝑊 ∗ → 𝑊 ∗ → 𝑊 ∗

𝑠1 ⋓ 𝑠2 = {(L : 𝑙, 𝑤) | (𝑙, 𝑤) ∈ 𝑠1} ∪ {(R : 𝑙, 𝑤) | (𝑙, 𝑤) ∈ 𝑠2}

⋒ : 𝑊 ∗ → 𝑊 ∗ → 𝑊 ∗

𝑠1 ⋒ 𝑠2 = {(𝑙1 ⊲⊳ 𝑙2, 𝑤1) | (𝑙1, 𝑤1) ∈ 𝑠1, (𝑙2, 𝑤2) ∈ 𝑠2, 𝑤1 = 𝑤2}

𝑠1

: 𝑊 ∗ → 𝑊 ∗ → 𝑊 ∗

(cid:35)
𝑠2 = {(𝑙1 ⊲⊳ 𝑙2, 𝑤2) | (𝑙1, 𝑤1) ∈ 𝑠1, (𝑙2, 𝑤2) ∈ 𝑠2}
(cid:35)
one

: 𝑊 ∗ → 𝑊 ∗

one(𝑠) = head (sort (𝑠))

all

: 𝑊 ∗ → 𝑊 ∗

all(𝑠) = tuple(sort (𝑠))
head

[𝑊 ] → 𝑊 ?

:
head [] = 𝑒𝑚𝑝𝑡𝑦

head (𝑤 : 𝑠) = unit (𝑤)

[𝑊 ] → ⟨𝑊 ⟩
tuple[𝑤1, · · · , 𝑤𝑛] = ⟨𝑤1, · · · , 𝑤𝑛⟩

tuple

:

Sort

sort

:

𝐿𝑊 ∗ → ([𝑊 ] + WRONG)⊥

sort (𝑠) = []

= WRONG
= 𝑤𝑠

if 𝑠 is empty
if 𝑤𝑠 has more than one element
otherwise

⊲⊳ sort{(𝑙, 𝑤) | (L : 𝑙, 𝑤) ∈ 𝑠}
⊲⊳ sort{(𝑙, 𝑤) | (R : 𝑙, 𝑤) ∈ 𝑠}

where 𝑤𝑠 = [𝑤 | ([], 𝑤) ∈ 𝑠]

Fig. 9. Labelled set semantics for 𝑊 ∗

• Finally sort puts everything in the “right” order: first the values with an empty label, then the
values whose label starts with L (notice the recursive sort of the trimmed-down sequence),
and then those that start with R. Notice that sort removes all the labels, leaving just a bare
sequence of values 𝑊 ∗.

1:30Lennart Augustsson, Joachim Breitner, Koen Claessen, Ranjit Jhala, Simon Peyton Jones, Olin Shivers, and Tim Sweeney

• Note that if sort encounters a set with more than one unlabelled element then this considered

WRONG. This makes ambiguous expressions, like one{∃x. x}, WRONG.

Let us look at our troublesome example ∃x. x = (4 3), and assume that ∃ binds x to 3 and then 4.
The meaning of this expression will be

Now if we take all of that expression we will get a singleton sequence containing ⟨4, 3⟩, because
all does a sort, stripping off all the tags.

E

∃x. x = (4 3)

𝜖

=

[(R, 3), (L, 4)]

(cid:74)

(cid:75)

C.5 Related work

E

(cid:74)

all{∃x. x = (4 3)}

𝜖

(cid:75)

=

[([], ⟨4, 3⟩)]

[Christiansen et al. 2010] gives another approach to a denotational semantics for a functional logic
language. We are keen to learn of others.

