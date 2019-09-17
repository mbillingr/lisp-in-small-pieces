# Exercise 6.1
Modify the combinator CHECKED-GLOBAL-REF so that the error message about an
uninitialized variable is more meaningful.
Hint: "symbol table" (the list g.current) indicates variable names and locations
where they are stored.

# Exercise 6.2
Define the primitive `list` for the third interpreter of this chapter. You will,
of course, do so elegantly and with little effort

# Exercise 6.3
Too much code - skipped, here...

# Exercise 6.4
Modify the last interpreter of this chapter so that activation records are
allocated before arguments are computed. Then arguments could be put directly
into the right place.

# Exercise 6.5
Define a special form, `redefine`, to take a variable in the immutable global
environment and insert it in the mutable global environment, as discussed on
page 191. The initial value of this new global variable is the value that it
had before.

# Exercise 6.6
Improve the pretreatment of functions without variables.
(No variables means no arguments; where else should variables come from? ;)
(So the environment does not need to be extended.)
