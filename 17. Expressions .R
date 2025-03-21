# Expressions:
# To compute on the language, we first need to understand its structure - new vocabulary, tools, ways
y <- x * 10 # not defined x 

z <- rlang::expr(y <- x * 10) # returns an expression
z # capture the intent of the code without executing it

x <- 4
eval(z) # evaluate an expression
y

library(rlang)
library(lobstr)

# Abstract syntax trees/ Expressions - because the structure of code is hierarchical and represented as a tree.
# This tree structure is crucial for inspecting and modifying expressions (i.e. metaprogramming).

# Drawing
lobstr::ast(f(x, "y", 1))
# symbols, like f and x, and constants, like 1 or "y"

lobstr::ast(f(g(1, 2), h(3, 4, i())))
# The depth within the tree is determined by the nesting of function calls.

# Non-code components
# AST are abstract because they only capture important structural details of the code, not whitespace or comments

ast(
  f(x,  y)  # important!
)
lobstr::ast(y <- x)
lobstr::ast(y < -x) # only one place where whitespace affects the AST

# Infix calls
# Every call in R can be written in tree form because any call can be written in prefix form 
y <- x * 10
`<-`(y, `*`(x, 10))
lobstr::ast(y <- x * 10)

# There really is no difference between the ASTs, and if you generate an expression with prefix calls, R will still print it in infix form.
expr(`<-`(y, `*`(x, 10)))

# Exercises
# 1.
ast(f(g(h())))
ast(1 + 2 + 3)
ast((x + y) * z)

# 2.
f(g(h(i(1, 2, 3))))
f(1, g(2, h(3, i())))
f(g(1, 2), h(3, i(4, 5)))

ast(f(g(h(i(1, 2, 3)))))
ast(f(1, g(2, h(3, i()))))
ast(f(g(1, 2), h(3, i(4, 5))))

# 3.
ast(`x` + `y`)
ast(x ** y)
ast(1 -> x)

# The call in the first expression is translated into its prefix form.
str(expr(x ** y))
str(expr(a -> b))

# 4.
ast(function(x = 1, y = 2) {})
# The last leaf of the AST is not explicitly specified in the expression. 
# Instead, the srcref attribute, which points to the functions source code, is automatically created by base R.

# 5.
ast(
  if (FALSE) {
    1
  } else if (FALSE) {
    2
  } else if (TRUE) {
    3
  }
)
# The AST of nested else if statements might look a bit confusing because it contains multiple curly braces.
# We can see the structure more clearly if we avoid the curly braces.
ast(
  if (FALSE) 1 
  else if (FALSE) 2 
  else if (TRUE) 3
)

# Expressions
# An expression is any member of the set of base types created by parsing code: constant scalars, symbols, call objects, and pairlists.

# Constants - a constant is either NULL or a length-1 atomic vector (or scalar, Section 3.2.1) like TRUE, 1L, 2.5 or "x".

identical(expr(TRUE), TRUE) # self-quoting
identical(expr(1), 1)
identical(expr(2L), 2L)
identical(expr("x"), "x")

# Symbols - represents the name of an object
expr(x) # creating symbol by capturing code that references an object 
sym("x") # creating symbol by turning a string into a symbol

as_string(expr(x)) # a symbol back into a string
str(expr(x)) # recognise a symbol because it’s printed without quotes
is.symbol(expr(x))

# Calls - a captured function call
# Call objects are a special type of list where the first component specifies the function to call, 
# and the remaining elements are the arguments for that call. 
# Call objects create branches in the AST, because calls can be nested inside other calls.

lobstr::ast(read.table("important.csv", row.names = FALSE))
x <- expr(read.table("important.csv", row.names = FALSE))
typeof(x)
is.call(x)

# Subsetting
x[[1]]
is.symbol(x[[1]])
as.list(x[-1]) # The remainder of the elements are the arguments
x[[2]]
x$row.names
length(x) - 1

# Extracting specific arguments from calls is challenging 
rlang::call_standardise(x) # standardises all arguments to use the full name

x$header <- TRUE # Calls can be modified in the same way as lists
x

# Function position - the first element of the call object 
lobstr::ast(foo())

# While R allows you to surround the name of the function with quotes, the parser converts it to a symbol:
lobstr::ast("foo"())

lobstr::ast(pkg::foo(1))
lobstr::ast(obj$foo(1))
lobstr::ast(foo(1)(2))

# Constructing - construct a call object from its components
# The first argument is the name of the function to call (either as a string, a symbol, or another call). 
# The remaining arguments will be passed along to the call.

call2("mean", x = expr(x), na.rm = TRUE)
call2(expr(base::mean), x = expr(x), na.rm = TRUE)
call2("<-", expr(x), 10) # create complex expressions is a bit clunky

# Exercises
# 1. 
# There is no way to create raws and complex atomics without using a function call.
# Similarly, it is not possible to create an expression that evaluates to an atomic of length greater than one without using a function.

is_atomic(expr(1))
is_atomic(expr(c(1, 1)))
is_call(expr(c(1, 1)))

# 2.
# When the first element of a call object is removed, the second element moves to the first position, which is the function to call.

# 3. 
x <- 1:10

call2(median, x, na.rm = TRUE) # both median() and x are evaluated and inlined into the call.
call2(expr(median), x, na.rm = TRUE) # Once, only x and once only median() gets evaluated.
call2(median, expr(x), na.rm = TRUE) # Once, only x and once only median() gets evaluated.
call2(expr(median), expr(x), na.rm = TRUE) # In the final call neither x nor median() is evaluated.

# 4.
# mean() uses the ... argument and therefore cannot standardise the regarding arguments.
call_standardise(quote(mean(1:10, na.rm = TRUE)))
call_standardise(quote(mean(n = T, 1:10)))
call_standardise(quote(mean(x = 1:10, , TRUE)))

# 5.
# The first element of a call is always the function that gets called, so the code does not make sense
x <- expr(foo(x = 1))
names(x) <- c("x", "")

# 6.
# When we read the AST from left to right, we get the same structure: Function to evaluate, expression, 
# which is another function and is evaluated first, and two constants which will be evaluated next.
call2("if", call2(">", sym("x"), 1), "a", "b")
ast(`if`(x > 1, "a", "b"))

# Parsing and grammar
# The process by which a computer language takes a string and 
# constructs an expression is called parsing, and is governed by a set of rules known as a grammar.

# Operator precedence
lobstr::ast(1 + 2 * 3) # check the algorithm
lobstr::ast(!x %in% y) # ! has a much lower precedence (i.e. it binds less tightly)
lobstr::ast((1 + 2) * 3) # using parentheses, no need to remembering

# Associativity
# ambiguity is introduced by repeated usage of the same infix function. 
# geom_point() + geom_smooth() does not yield the same plot as geom_smooth() + geom_point()

# most operators are left-associative, i.e. the operations on the left are evaluated first
lobstr::ast(1 + 2 + 3)
lobstr::ast(2^2^3) # exception
lobstr::ast(x <- y <- z) # exception 

# Parsing and deparsing
x1 <- "y <- x + 10"
x1
is.call(x1)

x2 <- rlang::parse_expr(x1)
x2
is.call(x2)

x3 <- "a <- 1; a + 1"
rlang::parse_exprs(x3) # always returns a single expression.

as.list(parse(text = x1)) # turning the output into a list

# The inverse of parsing is deparsing: given an expression, you want the string that would generate it.
z <- expr(y <- x + 10)
expr_text(z)

# Parsing and deparsing are not perfectly symmetric because parsing generates an abstract syntax tree. 
# This means we lose backticks around ordinary names, comments, and whitespace

cat(expr_text(expr({
  # This is a comment
  x <-             `x` + 1
})))
# deparse(): it returns a character vector with one element for each line.

# Exercises
# 1.
ast(f((1)))
ast(`(`(1 + 1))
ast(((1 + 1)))

# 2.
b = c(c = 1)
ast(b = c(c = 1))
ast({b = c(c = 1)})

# 3.
-2^2
ast(-2^2)

# 4.
!1 + !1
ast(!1 + !1)

# 5.
x1 <- (x2 <- (x3 <- 0))
(x3 <- 0)

# 6.
ast(x + y %+% z)
ast(x ^ y %+% z)

# 7.
parse_expr("x + 1; y + 1")

# 8.
parse_expr("a +")
parse_expr("f())")
parse(text = "a +")
parse(text = "f())")

# 9.
expr <- expr(g(a + b + c + d + e + f + g + h + i + j + k + l + m + 
                 n + o + p + q + r + s + t + u + v + w + x + y + z))

deparse(expr)
expr <- expr(g(a + b + c + d + e + f + g + h + i + j + k + l + m + 
                 n + o + p + q + r + s + t + u + v + w + x + y + z))
deparse(expr)
expr_text(expr)

# 10.
d <- 1
pairwise.t.test(2, d + d + d + d + d + d + d + d + 
                  d + d + d + d + d + d + d + d + d)
d <- 1
pairwise.t.test(2, d + d + d + d + d + d + d + d + 
                  d + d + d + d + d + d + d + d + d)

# Walking AST with recursive functions
# findGlobals() locates all global variables used by a function. 
# checkUsage() checks for a range of common problems including unused local variables, unused parameters, and the use of partial argument matching.

# The recursive case handles the nodes in the tree.
# The base case handles the leaves of the tree. The base cases ensure that the function eventually terminates, by solving the simplest cases directly. 

expr_type <- function(x) {
  if (rlang::is_syntactic_literal(x)) {
    "constant"
  } else if (is.symbol(x)) {
    "symbol"
  } else if (is.call(x)) {
    "call"
  } else if (is.pairlist(x)) {
    "pairlist"
  } else {
    typeof(x)
  }
}

expr_type(expr("a"))
expr_type(expr(x))
expr_type(expr(f(1, 2)))

switch_expr <- function(x, ...) {
  switch(expr_type(x),
         ...,
         stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  )
}

# With these two functions in hand, we can write a basic template for any function
recurse_call <- function(x) {
  switch_expr(x,
              # Base cases
              symbol = ,
              constant = ,
              
              # Recursive cases
              call = ,
              pairlist =
  )
}

# Finding F and T

# first find the type of T versus TRUE
expr_type(expr(TRUE))
expr_type(expr(T))

# TRUE is parsed as a logical vector of length one, while T is parsed as a name. 
# a constant is never a logical abbreviation, and a symbol is an abbreviation if it’s “F” or “T”:

logical_abbr_rec <- function(x) {
  switch_expr(x,
              constant = FALSE,
              symbol = as_string(x) %in% c("F", "T")
  )
}

logical_abbr_rec(expr(TRUE))
logical_abbr_rec(expr(T))

# we’ll typically make a wrapper that quotes its input, no need of expr()
logical_abbr <- function(x) {
  logical_abbr_rec(enexpr(x))
}

logical_abbr(T)
logical_abbr(FALSE)

# implement the recursive cases
logical_abbr_rec <- function(x) {
  switch_expr(x,
              # Base cases
              constant = FALSE,
              symbol = as_string(x) %in% c("F", "T"),
              
              # Recursive cases
              call = ,
              pairlist = purrr::some(x, logical_abbr_rec)
  )
}

logical_abbr(mean(x, na.rm = T))
logical_abbr(function(x, na.rm = T) FALSE)

# Finding all variables created by assignment
ast(x <- 10)
# Assignment is a call object where the first element is the symbol <-, 
# the second is the name of variable, and the third is the value to be assigned.

find_assign_rec <- function(x) {
  switch_expr(x,
              constant = ,
              symbol = character()
  )
}
find_assign <- function(x) find_assign_rec(enexpr(x))

find_assign("x")
find_assign(x)

# we implement the recursive cases
flat_map_chr <- function(.x, .f, ...) {
  purrr::flatten_chr(purrr::map(.x, .f, ...))
}

flat_map_chr(letters[1:3], ~ rep(., sample(3, 1)))

# The recursive case for pairlists is straightforward: we iterate over every element of the pairlist and combine the results
find_assign_rec <- function(x) {
  switch_expr(x,
              # Base cases
              constant = ,
              symbol = character(),
              
              # Recursive cases
              pairlist = flat_map_chr(as.list(x), find_assign_rec),
              call = {
                if (is_call(x, "<-")) {
                  as_string(x[[2]])
                } else {
                  flat_map_chr(as.list(x), find_assign_rec)
                }
              }
  )
}

find_assign(a <- 1)
find_assign({
  a <- 1
  {
    b <- 2
  }
})

find_assign({
  a <- 1
  a <- 2
})

# It’s easiest to fix this at the level of the wrapper function
find_assign <- function(x) unique(find_assign_rec(enexpr(x)))
find_assign({
  a <- 1
  a <- 2
})

find_assign({ # we only return the first. That’s because when <- occurs we immediately terminate recursion.
  a <- b <- c <- 1
})

find_assign_call <- function(x) {
  if (is_call(x, "<-") && is_symbol(x[[2]])) {
    lhs <- as_string(x[[2]])
    children <- as.list(x)[-1]
  } else {
    lhs <- character()
    children <- as.list(x)
  }
  
  c(lhs, flat_map_chr(children, find_assign_rec))
}

find_assign_rec <- function(x) {
  switch_expr(x,
              # Base cases
              constant = ,
              symbol = character(),
              
              # Recursive cases
              pairlist = flat_map_chr(x, find_assign_rec),
              call = find_assign_call(x)
  )
}

find_assign(a <- b <- c <- 1)
find_assign(system.time(x <- print(y <- 5)))

# Exercises

# 1.
expr_type <- function(x) {
  if (rlang::is_syntactic_literal(x)) {
    "constant"
  } else if (is.symbol(x)) {
    "symbol"
  } else if (is.call(x)) {
    "call"
  } else if (is.pairlist(x)) {
    "pairlist"
  } else {
    typeof(x)
  }
}

switch_expr <- function(x, ...) {
  switch(expr_type(x),
         ...,
         stop("Don't know how to handle type ", 
              typeof(x), call. = FALSE))
}

find_T_call <- function(x) {
  if (is_call(x, "T")) {
    x <- as.list(x)[-1]
    purrr::some(x, logical_abbr_rec)
  } else {
    purrr::some(x, logical_abbr_rec)
  }
}

logical_abbr_rec <- function(x) {
  switch_expr(
    x,
    # Base cases
    constant = FALSE,
    symbol = as_string(x) %in% c("F", "T"),
    
    # Recursive cases
    pairlist = purrr::some(x, logical_abbr_rec),
    call = find_T_call(x)
  )
}

logical_abbr <- function(x) {
  logical_abbr_rec(enexpr(x))
}

logical_abbr(T(1, 2, 3))
logical_abbr(T(T, T(3, 4)))
logical_abbr(T(T))
logical_abbr(T())
logical_abbr()
logical_abbr(c(T, T, T))

# 2.
f <- function(x = TRUE) {
  g(x + T)
}
logical_abbr(!!f)

# 3.
ast(names(x) <- x)
if (is_call(x, "<-") && is_call(x[[2]])) {
  lhs <- expr_text(x[[2]])
  children <- as.list(x)[-1]
}
flat_map_chr <- function(.x, .f, ...) {
  purrr::flatten_chr(purrr::map(.x, .f, ...))
}

find_assign <- function(x) unique(find_assign_rec(enexpr(x)))

find_assign_call <- function(x) {
  if (is_call(x, "<-") && is_symbol(x[[2]])) {
    lhs <- as_string(x[[2]])
    children <- as.list(x)[-1]
  } else {
    if (is_call(x, "<-") && is_call(x[[2]])) {
      lhs <- expr_text(x[[2]])
      children <- as.list(x)[-1]
    } else {
      lhs <- character()
      children <- as.list(x)
    }}
  
  c(lhs, flat_map_chr(children, find_assign_rec))
}

find_assign_rec <- function(x) {
  switch_expr(
    x,
    # Base cases
    constant = ,symbol = character(),
    # Recursive cases
    pairlist = flat_map_chr(x, find_assign_rec),
    call = find_assign_call(x)
  )
}

# Tests functionality
find_assign(x <- y)
find_assign(names(x))
find_assign(names(x) <- y)
find_assign(names(x(y)) <- y)
find_assign(names(x(y)) <- y <- z)

# 4.
find_assign_call <- function(x) {
  if (is_call(x)) {
    lhs <- expr_text(x)
    children <- as.list(x)[-1]
  } else {
    lhs <- character()
    children <- as.list(x)
  }
  
  c(lhs, flat_map_chr(children, find_assign_rec))
}

find_assign_rec <- function(x) {
  switch_expr(
    x,
    # Base cases
    constant = ,
    symbol = character(),
    
    # Recursive cases
    pairlist = flat_map_chr(x, find_assign_rec),
    call = find_assign_call(x)
  )
}

find_assign(x <- y)
find_assign(names(x(y)) <- y <- z)
find_assign(mean(sum(1:3)))

# Specialised data structures

# Pairlists
# Pairlists are a remnant of R’s past and have been replaced by lists almost everywhere.
f <- expr(function(x, y = 10) x + y)
args <- f[[2]]
args
typeof(args)

pl <- pairlist(x = 1, y = 2) # treat it just like a regular list
length(pl)
pl$x
# pairlists are implemented using a linked list. That makes subsetting a pairlist much slower.

# Missing arguments
# The empty symbol is used to represent missing arguments (not missing values!)

missing_arg() # making an empty symbol 
typeof(missing_arg())

is_missing(missing_arg()) # An empty symbol doesn’t print anything, so to check if you have one

f <- expr(function(x, y = 10) x + y)
args <- f[[2]]
is_missing(args[[1]])

f <- expr(function(...) list(...)) # important for ... which is always associated with an empty symbol
args <- f[[2]]
is_missing(args[[1]])

m <- missing_arg() 
m # a peculiar property: if you bind it to a variable, then access that variable, you will get an error

ms <- list(missing_arg(), missing_arg())
ms[[1]] # But you won’t if you store it inside another data structure

# Expression vectors
# Expression vectors are only produced by two base functions: expression() and parse()

exp1 <- parse(text = c("
x <- 4
x
"))
exp2 <- expression(x <- 4, x)

typeof(exp1)
typeof(exp2)
exp1
exp2

length(exp1)
exp1[[1]]
 
# The only difference of expression with calls and pairlists is that calling eval() on an expression evaluates each individual expression. 
