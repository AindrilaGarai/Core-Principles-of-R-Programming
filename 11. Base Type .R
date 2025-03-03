# Object oriented programming
# We will focus only S3, R6, and S4 - 3 important OOP system

# S3 allows your functions to return rich results with user-friendly display and programmer-friendly internals. 
# R6 provides a standardised way to escape R’s copy-on-modify semantics. This is particularly important if you want to model objects that exist independently of R.
# S4 is a rigorous system that forces you to think carefully about program design. It’s particularly well-suited for building large systems that evolve over time.

# The main reason to use OOP is polymorphism (literally: many shapes). 
# Polymorphism means that a developer can consider a function’s interface separately from its implementation, 
# making it possible to use the same function form for different types of input. 
# This is closely related to the idea of encapsulation: the user doesn’t need to worry 
# about details of an object because they are encapsulated behind a standard interface.

diamonds <- ggplot2::diamonds

# polymorphism is what allows summary() to produce different outputs for numeric and factor variables.
summary(diamonds$carat)
summary(diamonds$cut)

# OO systems call the type of an object its class- a class defines what an object is 
# an implementation for a specific class is called a method- methods describe what that object can do
# The class defines the fields, the data possessed by every instance of that class
# Classes are organised in a hierarchy so that if a method does not exist for one class, its parent’s method is used, and the child is said to inherit behaviour.

# in R, an ordered factor inherits from a regular factor, and a generalised linear model inherits from a linear model.
# The process of finding the correct method given a class is called method dispatch.

# There are two main paradigms of object-oriented programming which differ in how methods and classes are related.

# In encapsulated OOP, methods belong to objects or classes, and method calls typically look like object.method(arg1, arg2). 
# This is called encapsulated because the object encapsulates both data (with fields) and behaviour (with methods).

# In functional OOP, methods belong to generic functions, and method calls look like ordinary function calls: generic(object, arg2, arg3). 
# This is called functional because from the outside it looks like a regular function call, and internally the components are also functions.

# S3 is R’s first OOP system- is an informal implementation of functional OOP.

# S4 is a formal and rigorous rewrite of S3- requires more upfront work than S3, but in return provides more guarantees and greater encapsulation. 
# S4 is implemented in the base methods package, which is always installed with R.

# RC objects are a special type of S4 objects that are also mutable, i.e., instead of using R’s usual copy-on-modify semantics, they can be modified in place. 
# R6 implements encapsulated OOP like RC, but resolves some important issues.
# R.oo provides some formalism on top of S3, and makes it possible to have mutable S3 objects.
# proto implements another style of OOP based on the idea of prototypes, which blur the distinctions between classes and instances of classes (objects).

library(sloop)
# The sloop package (think “sail the seas of OOP”) provides a number of helpers that fill in missing pieces in base R.

otype(1:10)
otype(mtcars)

mle_obj <- stats4::mle(function(x = 1) (x - 2) ^ 2)
otype(mle_obj)

# Base Type
# Object - 1. base object 2. OO objects

# Base versus OO objects:
# a base object-
is.object(1:10)
sloop::otype(1:10)

# An OO object
is.object(mtcars)
sloop::otype(mtcars)

# the difference between base and OO objects is that OO objects have a “class” attribute
attr(1:10, "class")
attr(mtcars, "class")

x <- matrix(1:4, nrow = 2)
class(x)
sloop::s3_class(x)

# While only OO objects have a class attribute, every object has a base type:
typeof(1:10)
typeof(mtcars)

# In total, there are 25 different base types- 
# Vectors, include types NULL (NILSXP), logical (LGLSXP), integer (INTSXP), double (REALSXP), complex (CPLXSXP), character (STRSXP), list (VECSXP), and raw (RAWSXP).
# Functions, include types closure (regular R functions, CLOSXP), special (internal functions, SPECIALSXP), and builtin (primitive functions, BUILTINSXP).
# Environments have type environment (ENVSXP).
# The S4 type (S4SXP) is used for S4 classes that don’t inherit from an existing base type.
# Language components, include symbol (aka name, SYMSXP), language (usually called calls, LANGSXP), and pairlist (used for function arguments, LISTSXP) types.
# expression (EXPRSXP) is a special purpose type that’s only returned by parse() and expression(). 
# The remaining types are esoteric and rarely seen in R.

typeof(NULL)
typeof(1L)
typeof(1i)

typeof(mean)
typeof(`[`)
typeof(sum)    

typeof(globalenv())

mle_obj <- stats4::mle(function(x = 1) (x - 2) ^ 2)
typeof(mle_obj)

typeof(quote(a))
typeof(quote(a + 1))
typeof(formals(mean))

# Numeric type
# R uses “numeric” to mean three slightly different things:
# 1. In some places numeric is used as an alias for the double type. For example as.numeric() is identical to as.double(), and numeric() is identical to double().
# 2. In the S3 and S4 systems, numeric is used as a shorthand for either integer or double type-
sloop::s3_class(1)
sloop::s3_class(1L)
# 3. is.numeric() tests for objects that behave like numbers. For example, factors have type “integer” but don’t behave like numbers.
typeof(factor("x"))
is.numeric(factor("x"))

