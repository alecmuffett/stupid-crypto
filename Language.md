This is an attempt at formalising some of the adhoc discussion both on the
mailing list, and before that in private correspondence.

# statements #

Statements can have side effects. what are side effects? they are things that
will modify the value of a variable, or read/write from a stream.

statements:

writing a value to a variable

writing a value to a stream

reading a value from a stream

Note that reading a value from a variable is not a statement - it has no
side effects. An expression on its own is also not a statement (?).

calling a function (which I think is not implemented at the moment)

# expressions #

expressions evaluate to values.
expressions can read variables. they cannot have side effects. that is what
statements are for.

# Values #

streams are not values.

# Types #

Base types:

Numerical types:

uint8 - unsigned 8-bit integer
uint32 - unsigned 32-bit integer
boolean (does this have a name at the moment?)

Composite types:

arrays - these are indexed by any numeric type, starting at 0. Each language
backend will have its own language-specific maximum size, and it is up to
the programmer to make sure that array use is within the maximum provided by
all desired target languages.


# Operators #

Operators within a single numeric type:

or8  - bitwise or
and8 - bitwise and

... etc ...

Operators for converting between numeric types:

to widen:
widen8to32

and to shorten:
mask32to8

# Comments #

Comments can appear anywhere a statement or a function definition can appear.
They appear inside quotes.

# Streams #

Streams are not values.
