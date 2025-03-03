# S3 is R’s first and simplest OO system. S3 is informal and ad hoc, but there is a certain elegance in its minimalism:
# Since S3 has few built-in constraints, the key to its successful use is applying the constraints yourself.
# S3 classes are implemented using attributes.

library(sloop)

# An S3 object is a base type with at least a class attribute
f <- factor(c("a", "b", "c"))
typeof(f)
attributes(f) # has a class attribute of “factor”, and a levels attribute that stores the possible levels

unclass(f) # strips the class attribute

# An S3 object behaves differently from its underlying base type whenever it’s passed to a generic.
ftype(print)
ftype(str)
ftype(unclass)

# A generic function defines an interface, which uses a different implementation depending on the class of an argument
print(f)
print(unclass(f)) # stripping class reverts to integer behaviour

# Beware that str() is generic, and some S3 classes use that generic to hide the internal details.
time <- strptime(c("2017-01-01", "2020-05-04 03:21"), "%Y-%m-%d")
str(time) # the POSIXlt class used to represent date-time data is actually built on top of a list, a fact which is hidden by its str() method:
str(unclass(time))

# The generic's job is to define the interface (i.e. the arguments) then find the right implementation for the job.
# The implementation for a specific class is called a method, and the generic finds that method by performing method dispatch.

s3_dispatch(print(f)) # to see the process of method dispatch

# identify a method by the presence of . in the function name
ftype(t.test)
ftype(t.data.frame)

# we can’t see the source code for most S3 methods because S3 methods are not usually exported: 
# they live only inside the package, and are not available from the global environment. 
# we can use sloop::s3_get_method(), which will work regardless of where the method lives:
weighted.mean.Date
s3_get_method(weighted.mean.Date)

# Exercises

# 1.
# t.test() is a generic function that performs a t-test.
# t.data.frame() is a method that gets called by the generic t() to transpose data frame input.
# Due to R’s S3 dispatch rules, t.test() would also get called when t() is applied to an object of class test.

# Some base R functions with point.separated names
install.packages()
read.csv()

list.files()
download.file()

data.frame()
as.character()
Sys.Date()

all.equal()

do.call()
on.exit()

# 3.
# The function as.data.frame.data.frame() implements the data.frame() method for the as.data.frame() generic, which coerces objects to data frames.
# The name is confusing, because it does not clearly communicate the type of the function,
# which could be a regular function, a generic or a method. 

# 4.
some_days <- as.Date("2017-01-31") + sample(10, 5)

mean(some_days)
mean(unclass(some_days)) # After unclass() has removed the class attribute from some_date, calculates the mean of the underlying double.

# 5.
x <- ecdf(rpois(100, 10))
x
typeof(x)
attributes(x) # The ecdf object is built on the base type closure (a function). The expression, which was used to create it (rpois(100, 10)), is stored in the call attribute.

# 6.
x <- table(rpois(100, 5))
x
#  This code returns a table object, which is built upon the integer type. The attribute dimnames is used to name the elements of the integer vector.
typeof(x)
attributes(x)

# Classes:

x <- structure(list(), class = "my_class") # Create and assign class in one step
x <- list() # Create, then set class
class(x) <- "my_class"

# we can determine the class of an S3 object 
class(x)
inherits(x, "my_class")
inherits(x, "your_class")

# S3 has no checks for correctness which means you can change the class of existing objects:
mod <- lm(log(mpg) ~ log(disp), data = mtcars)
class(mod)
print(mod)
class(mod) <- "Date" # Turn it into a date (?!)
print(mod) # this doesn't work very well

# A low-level constructor, new_myclass(), that efficiently creates new objects with the correct structure.
# A validator, validate_myclass(), that performs more computationally expensive checks to ensure that the object has correct values.
# A user-friendly helper, myclass(), that provides a convenient way for others to create objects of your class.

# Constructors
# S3 doesn't ensure that all objects of a given class have the same structure.
# you must enforce a consistent structure by using a constructor.

# The constructor should follow three principles:
# Be called new_myclass().
# Have one argument for the base object, and one for each attribute.
# Check the type of the base object and the types of each attribute.

new_Date <- function(x = double()) {
  stopifnot(is.double(x))
  structure(x, class = "Date")
}

new_Date(c(-1, 0, 1))

new_difftime <- function(x = double(), units = "secs") {
  stopifnot(is.double(x))
  units <- match.arg(units, c("secs", "mins", "hours", "days", "weeks"))
  
  structure(x,
            class = "difftime",
            units = units
  )
}

new_difftime(c(1, 10, 3600), "secs")
new_difftime(52, "weeks")

# Validators
# A constructor only checks that types are correct, making it possible to create malformed factors:
new_factor <- function(x = integer(), levels = character()) {
  stopifnot(is.integer(x))
  stopifnot(is.character(levels))
  
  structure(
    x,
    levels = levels,
    class = "factor"
  )
}

new_factor(1:5, "a")
new_factor(0:1, "a")

# Rather than encumbering the constructor with complicated checks, it’s better to put them in a separate function. 
# Doing so allows you to cheaply create new objects when you know that the values are correct, and easily re-use the checks in other places.

validate_factor <- function(x) {
  values <- unclass(x)
  levels <- attr(x, "levels")
  
  if (!all(!is.na(values) & values > 0)) {
    stop(
      "All `x` values must be non-missing and greater than zero",
      call. = FALSE
    )
  }
  
  if (length(levels) < max(values)) {
    stop(
      "There must be at least as many `levels` as possible values in `x`",
      call. = FALSE
    )
  }
  
  x
}

validate_factor(new_factor(1:5, "a"))
validate_factor(new_factor(0:1, "a"))

# Helpers
# If you want users to construct objects from your class, you should also provide a helper method 
# that makes their life as easy as possible. A helper should always:
# 1. Have the same name as the class, e.g. myclass().
# 2. Finish by calling the constructor, and the validator, if it exists.
# 3. Create carefully crafted error messages tailored towards an end-user.
# 4. Have a thoughtfully crafted user interface with carefully chosen default values and useful conversions.

new_difftime(1:10)
difftime <- function(x = double(), units = "secs") {
  x <- as.double(x)
  new_difftime(x, units = units)
}

difftime(1:10)

# the most natural representation of a complex object is a string. 
# it’s very convenient to specify factors with a character vector. 
factor <- function(x = character(), levels = unique(x)) {
  ind <- match(x, levels)
  validate_factor(new_factor(ind, levels))
}

factor(c("a", "a", "b"))

# Some complex objects are most naturally specified by multiple simple components.
POSIXct <- function(year = integer(), 
                    month = integer(), 
                    day = integer(), 
                    hour = 0L, 
                    minute = 0L, 
                    sec = 0, 
                    tzone = "") {
  ISOdatetime(year, month, day, hour, minute, sec, tz = tzone)
}

POSIXct(2020, 1, 1, tzone = "America/New_York")

# Exercises
# 1.
# Data frames are built on named lists of vectors, which all have the same length. 
# Besides the class and the column names (names), the row.names are their only further attribute. 
# This must be a character vector with the same length as the other vectors.
# We need to provide the number of rows as an input to make it possible to create data frames with 0 columns but multiple rows.

new_data.frame <- function(x, n, row.names = NULL) {
  stopifnot(is.list(x))
  stopifnot(all(lengths(x) == n))
  
  if (is.null(row.names)) {
    row.names <- .set_row_names(n)  # Use special row names helper from base R
  } else {
    # Otherwise check that they're a character vector with the 
    # correct length
    stopifnot(is.character(row.names), length(row.names) == n)
  }
  
  structure(
    x,
    class = "data.frame",
    row.names = row.names
  )
}

x <- list(a = 1, b = 2)
new_data.frame(x, n = 1)
new_data.frame(x, n = 1, row.names = "l1")

# Create a data frame with 0 columns and 2 rows
new_data.frame(list(), n = 2)

# 2.
factor(c("a", "b", "c"), levels = c("a", "b")) # converts these values (silently) into NAs

factor <- function(x = character(), levels = unique(x)) {
  ind <- match(x, levels)
  validate_factor(new_factor(ind, levels))
}

factor2 <- function(x, levels = unique(x)) {
  new_levels <- match(x, levels)
  
  # Error if levels don't include all values
  missing <- unique(setdiff(x, levels))
  if (length(missing) > 0) {
    stop(
      "The following values do not occur in the levels of x: ",
      paste0("'", missing, "'", collapse = ", "), ".", 
      call. = FALSE
    )
  }
  
  validate_factor(new_factor(new_levels, levels))
}

factor2(c("a", "b", "c"), levels = c("a", "b"))

# 3.
# The original implementation (base::factor()) allows more flexible input for x. 
# It coerces x to character or replaces it with character(0) (in case of NULL). 
# It also ensures that the levels are unique.

# 4.
# When factor variables are used in statistical models, they are typically encoded as dummy variables 
# and by default each level is compared with the first factor level.

new_factor <- function(x = integer(), levels = character()) {
  stopifnot(is.integer(x))
  stopifnot(is.character(levels))
  
  structure(
    x,
    levels = levels,
    class = "factor"
  )
}

# Updated new_factor() constructor gets a contrasts argument, which accepts a numeric matrix or NULL (default)
new_factor <- function(
    x = integer(),
    levels = character(),
    contrasts = NULL
) {
  stopifnot(is.integer(x))
  stopifnot(is.character(levels))
  
  if (!is.null(constrasts)) {
    stopifnot(is.matrix(contrasts) && is.numeric(contrasts))
  }
  
  structure(
    x,
    levels = levels,
    class = "factor",
    contrasts = contrasts
  )
}

# 5.
# This function transforms numeric input into Roman numbers. It is built on the integer type, which results in the following constructor.
new_roman <- function(x = integer()) {
  stopifnot(is.integer(x))
  structure(x, class = "roman")
}

validate_roman <- function(x) { # that only values between 1 and 3899 are uniquely represented
  values <- unclass(x)
  
  if (any(values < 1 | values > 3899)) {
    stop(
      "Roman numbers must fall between 1 and 3899.",
      call. = FALSE
    )
  }
  
  x
}

# we allow the user to also pass real values to a helper function.
roman <- function(x = integer()) {
  x <- as.integer(x)
  
  validate_roman(new_roman(x))
}

roman(c(1, 753, 2019))
roman(0)

# Generics and methods
mean
my_new_generic <- function(x) {
  UseMethod("my_new_generic") # takes two arguments: the name of the generic function (required), and the argument to use for method dispatch (optional).
}

# Method dispatch
x <- Sys.Date()
s3_dispatch(print(x)) # it basically creates a vector of method names, then looks for each potential method in turn. 

# The output here is simple:
# => indicates the method that is called, here print.Date()
# * indicates a method that is defined, but not called, here print.default().
# The “default” class is a special pseudo-class.

x <- matrix(1:10, nrow = 2)
s3_dispatch(mean(x)) # lets you find the specific method used for a single call. 
s3_dispatch(sum(Sys.time()))

# Finding methods: if you want to find all methods defined for a generic or associated with a class
s3_methods_generic("mean")
s3_methods_class("ordered")

# Creating methods:
# two awareness
# 1. you should only ever write a method if you own the generic or the class. 
# 2. A method must have the same arguments as its generic. This is enforced in packages by R CMD check.

# Exercises:
# 1.
x <- structure(1:10, class = "test")
t(x)
t.test # a generic
ftype(t.test)
t

tools::nonS3methods("stats")
x <- structure(1:10, class = "test")
t(x)

# 2.
# the table class have methods for generic - a simple application of sloop::s3_methods_class():
s3_methods_class("table")
# the table class has a number of methods designed to help plotting with base graphics.
x <- rpois(100, 5)
plot(table(x))

# 3.
s3_methods_class("ecdf")
# The methods are primarily designed for display (plot(), print(), summary()), but you can also extract quantiles with quantile().

# 4.
# print() generic has the most defined methods.
nrow(s3_methods_generic("print"))
nrow(s3_methods_generic("summary"))
nrow(s3_methods_generic("plot"))

library(purrr)

ls(all.names = TRUE, env = baseenv()) %>% 
  mget(envir = baseenv()) %>% 
  keep(is_function) %>% 
  names() %>% 
  keep(is_s3_generic) %>% 
  map(~ set_names(nrow(s3_methods_generic(.x)), .x)) %>% 
  flatten_int() %>% 
  sort(decreasing = TRUE) %>% 
  head()

# 5.
g <- function(x) {
  x <- 10
  y <- 10
  UseMethod("g")
}
g.default <- function(x) c(x = x, y = y)

x <- 1
y <- 1
g(x)
g.default(x) #  in a special way so that variables defined inside the generic are available to methods.
g(x)

# 6.
ftype(`[`) # The subsetting operator [ is a primitive and a generic function
# For primitive functions formals([) returns NULL so we need to find another way to determine the functions arguments.
names(formals(`[.data.frame`))
names(formals(`[.table`))
names(formals(`[.Date`))
names(formals(`[.AsIs`))

library(dplyr)

s3_methods_generic("[") %>%
  filter(visible) %>%
  mutate(
    method = paste0("[.", class),
    argnames = purrr::map(method, ~ names(formals(.x))),
    args = purrr::map(method, ~ formals(.x)),
    args = purrr::map2(
      argnames, args,
      ~ paste(.x, .y, sep = " = ")
    ),
    args = purrr::set_names(args, method)
  ) %>%
  pull(args) %>%
  head()

# Object styles:

# Record style objects use a list of equal-length vectors to represent individual components of the object.
# example is POSIXlt, which underneath the hood is a list of 11 date-time components like year, month, and day.

x <- as.POSIXlt(ISOdatetime(2020, 1, 1, 0, 0, 1:3))
x
length(x)
length(unclass(x))

x[[1]] # the first date time
unclass(x)[[1]] # the first component, the number of seconds

# Data frames are similar to record style objects in that both use lists of equal length vectors.
x <- data.frame(x = 1:100, y = 1:100)
length(x)
nrow(x)

# Scalar objects typically use a list to represent a single thing. For example, an lm object is a list of length 12 but it represents one model.
mod <- lm(mpg ~ wt, data = mtcars)
length(mod)

# Exercises
# 1.
# Vector object-style: factor(), table(), as.Date(), as.POSIXct(), ordered()
# Record object-style: not observed
# Data frame object-style: not observed
# Scalar object-style: lm(), ecdf()
# The object style of I() depends on the input since this function returns a “copy of the object with class AsIs prepended to the class(es).”

# 2.
# The constructor needs to populate the attributes of an lm object and check their types for correctness.
mod <- lm(cyl ~ ., data = mtcars)
typeof(mod)
attributes(mod)
map_chr(mod, typeof) #  to find out the base types of its elements. 

new_lm <- function(
    coefficients, residuals, effects, rank, fitted.values, assign,
    qr, df.residual, xlevels, call, terms, model
) {
  
  stopifnot(
    is.double(coefficients), is.double(residuals), 
    is.double(effects), is.integer(rank), is.double(fitted.values),
    is.integer(assign), is.list(qr), is.integer(df.residual),
    is.list(xlevels), is.language(call), is.language(terms),
    is.list(model)
  )
  
  structure(
    list(
      coefficients = coefficients,
      residuals = residuals,
      effects = effects,
      rank = rank, 
      fitted.values = fitted.values,
      assign = assign,
      qr = qr,
      df.residual = df.residual,
      xlevels = xlevels,
      call = call,
      terms = terms, 
      model = model
    ),
    class = "lm"
  )
}

# Inheritance:
# S3 classes can share behaviour through a mechanism called inheritance. 
# Inheritance is powered by three ideas:

# The class can be a character vector.
class(ordered("x"))
class(Sys.time()) # two components in their class

# If a method is not found for the class in the first element of the vector, R looks for a method for the second class (and so on):
s3_dispatch(print(ordered("x")))
s3_dispatch(print(Sys.time()))

# A method can delegate work by calling NextMethod().
s3_dispatch(ordered("x")[1])
s3_dispatch(Sys.time()[1])

# ordered is a subclass of factor because it always appears before it in the class vector.
# The base type of the subclass should be that same as the superclass.
# The attributes of the subclass should be a superset of the attributes of the superclass.

# NextMethod():
# A toy class: secret class that hides its output when printed
new_secret <- function(x = double()) {
  stopifnot(is.double(x))
  structure(x, class = "secret")
}

print.secret <- function(x, ...) {
  print(strrep("x", nchar(x)))
  invisible(x)
}

x <- new_secret(c(15, 1, 456))
x
s3_dispatch(x[1]) # This works, but the default [ method doesn’t preserve the class.

# To fix this, we need to provide a [.secret method
`[.secret` <- function(x, i) {
  new_secret(x[i])
} # The naive approach won’t work because we’ll get stuck in an infinite loop:

# One approach would be to unclass() the object:
`[.secret` <- function(x, i) {
  x <- unclass(x)
  new_secret(x[i])
}
x[1]

# This works, but is inefficient because it creates a copy of x. A better approach is to use NextMethod(), 
# which concisely solves the problem of delegating to the method 

`[.secret` <- function(x, i) {
  new_secret(NextMethod())
}
x[1]
s3_dispatch(x[1]) # The => indicates that [.secret is called, 
# but that NextMethod() delegates work to the underlying internal [ method, as shown by the ->.

# As with UseMethod(), the precise semantics of NextMethod() are complex. 
# In particular, it tracks the list of potential next methods with a special variable, 
# which means that modifying the object that’s being dispatched upon will have no impact on which method gets called next.

# Allowing subclassing:
# When you create a class, you need to decide if you want to allow subclasses, because it requires some changes to the constructor.
new_secret <- function(x, ..., class = character()) {
  stopifnot(is.double(x))
  
  structure(
    x,
    ...,
    class = c(class, "secret")
  )
}
# Then the subclass constructor can just call to the parent class constructor with additional arguments as needed. 

new_supersecret <- function(x) {
  new_secret(x, class = "supersecret")
}

print.supersecret <- function(x, ...) {
  print(rep("xxxxx", length(x)))
  invisible(x)
}

x2 <- new_supersecret(c(15, 1, 456))
x2

`[.secret` <- function(x, ...) {
  new_secret(NextMethod())
}

x2[1:3]

vec_restore.secret <- function(x, to, ...) new_secret(x)
vec_restore.supersecret <- function(x, to, ...) new_supersecret(x)

`[.secret` <- function(x, ...) {
  vctrs::vec_restore(NextMethod(), x)
}
x2[1:3]

# Exercises
# 1.
`[.Date`
.Date
`[.Date` <- function(x, ..., drop = TRUE) {
  out <- NextMethod("[")
  class(out) <- class(x)
  out
}
x <- structure(1:4, test = "test", class = c("myDate", "Date"))
attributes(x[1])

# 2.
generics_t  <- s3_methods_class("POSIXt")$generic
generics_ct <- s3_methods_class("POSIXct")$generic
generics_lt <- s3_methods_class("POSIXlt")$generic

union(generics_ct, generics_lt)
setdiff(generics_t, union(generics_ct, generics_lt))

# 3.
generic2 <- function(x) UseMethod("generic2")
generic2.a1 <- function(x) "a1"
generic2.a2 <- function(x) "a2"
generic2.b <- function(x) {
  class(x) <- "a1"
  NextMethod()
}

generic2(structure(list(), class = c("b", "a2")))

# we pass an object of classes b and a2 to generic2(), which prompts R to look for a methodgeneric2.b()
# the method generic2.b() then changes the class to a1 and calls NextMethod()
# One would think that this will lead R to call generic2.a1(), but in fact, as mentioned in Advanced R, NextMethod()

generic2(structure(list(), class = c("b", "a2")))

generic2.b <- function(x) {
  class(x) <- "a1"
  print(.Class)
  NextMethod()
}

generic2(structure(list(), class = c("b", "a2")))

# Dispatch details
# S3 and base types:
class(matrix(1:5))

# implicit class:
# The string “array” or “matrix” if the object has dimensions
# The result of typeof() with a few minor tweaks
# The string “numeric” if object is “integer” or “double”

s3_class(matrix(1:5))
s3_dispatch(print(matrix(1:5)))

x1 <- 1:5
class(x1)
s3_dispatch(mean(x1))

x2 <- structure(x1, class = "integer")
class(x2)
s3_dispatch(mean(x2))

# Internal generics: don’t call UseMethod() but instead call the C functions 
s3_dispatch(Sys.time()[1]) 
#  internal generics do not dispatch to methods unless the class attribute has been set, 
# which means that internal generics do not use the implicit class

# Group generics:
# Group generics are the most complicated part of S3 method dispatch because they involve both NextMethod() and internal generics.
# There are four group generics:
# Math: abs(), sign(), sqrt(), floor(), cos(), sin(), log(), and more (see ?Math for the complete list).
# Ops: +, -, *, /, ^, %%, %/%, &, |, !, ==, !=, <, <=, >=, and >.
# Summary: all(), any(), sum(), prod(), min(), max(), and range().
# Complex: Arg(), Conj(), Im(), Mod(), Re().

s3_dispatch(sum(Sys.time()))
y <- as.difftime(10, units = "mins")
s3_dispatch(abs(y))

Math.difftime <- function(x, ...) {
  new_difftime(NextMethod(), units = attr(x, "units"))
}
# It dispatches to the next method, here the internal default, to perform the actual computation, then restore the class and attributes.

# Double dispatch:
# Generics in the Ops group, which includes the two-argument arithmetic and Boolean operators like - and &, implement a special type of method dispatch. 
# They dispatch on the type of both of the arguments, which is called double dispatch.

date <- as.Date("2017-01-01")
integer <- 1L
date + integer
integer + date

# If + dispatched only on the first argument, it would return different values for the two cases.
# There are three possible outcomes of this lookup:
# The methods are the same, so it doesn’t matter which method is used.
# The methods are different, and R falls back to the internal method with a warning.
# One method is internal, in which case R calls the other method.

# Exercises:
# 1.
length.integer <- function(x) 10

x1 <- 1:5
class(x1)
s3_dispatch(length(x1))
x2 <- structure(x1, class = "integer")
class(x2)
s3_dispatch(length(x2))

# class() returns integer in both cases. However, while the class of x1 is created implicitly and inherits from the numeric class, 
# the class of x2 is set explicitly. 
# An object has no explicit class if attr(x, "class") returns NULL:
attr(x1, "class")
attr(x2, "class")
s3_class(x1)  # implicit
s3_class(x2)  # explicit

# => method exists and is found by UseMethod().
# -> method exists and is used by NextMethod().
# * method exists but is not used.
# Nothing (and greyed out in console): method does not exist.

# 2
# The following functions belong to this group (see ?Math):
# abs, sign, sqrt, floor, ceiling, trunc, round, signif
# exp, log, expm1, log1p, cos, sin, tan, cospi, sinpi, tanpi, acos, asin, atan, cosh, sinh, tanh, acosh, asinh, atanh
# lgamma, gamma, digamma, trigamma
# cumsum, cumprod, cummax, cummin

# The following classes have a method for this group generic:
s3_methods_generic("Math")

# overwrite the data frame method:
Math.data.frame <- function(x) "hello"
abs(mtcars) # Now all functions from the math generic group, will return "hello"
exp(mtcars)
lgamma(mtcars)

Math.data.frame <- function(x, ...) {
  .Generic
}

abs(mtcars)
exp(mtcars)
lgamma(mtcars)

rm(Math.data.frame)
# The original source code of Math.data.frame() is a good example on 
# how to invoke the string returned by .Generic into a specific method. 
# Math.factor() is a good example of a method, which is simply defined for better error messages.

# 3.
# Math.difftime() also excludes cases apart from abs, sign, floor, ceiling, trunc, round 
# and signif and needs to return a fitting error message.

Math.difftime <- function(x, ...) { # in the book
    new_difftime(NextMethod(), units = attr(x, "units"))
  }
rm(Math.difftime)

Math.difftime() # in the {base} package:
Math.difftime

