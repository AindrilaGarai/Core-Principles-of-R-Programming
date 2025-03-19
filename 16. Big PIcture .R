# METAPROGRAMMING
# The idea that code is data that can be inspected and modified programmatically.

library(rlang)
library(lobstr) # to explore the tree structure of code.

# Code is data

expr(mean(x, na.rm = TRUE)) # capture code and returning exactly what you pass in.
expr(10 + 100 + 1000) 
# captured code is called an expression - is a collective term for any of four types (call, symbol, constant, or pairlist)

capture_it <- function(x) {
  expr(x) # need a different tool to capture code passed to a function
}
capture_it(a + b + c)

capture_it <- function(x) {
  enexpr(x) # the “en” in the context of “enrich”
}
capture_it(a + b + c) # Because capture_it() uses enexpr() we say that it automatically quotes its first argument.

# We can inspect and modify an expression after capturing.
f <- expr(f(x = 1, y = 2))
f$z <- 3 # Add a new argument
f
f[[2]] <- NULL # Or remove an argument:
f

# Code is a tree
# Almost every programming language represents code as a tree, often called the abstract syntax tree, or AST.

# Function calls form the branches of the tree, and are shown by rectangles. 
# The leaves of the tree are symbols (like a) and constants (like "b").
lobstr::ast(f(a, "b")) 

lobstr::ast(f1(f2(a, b), f3(1, f4(2)))) # nested function - deeply branches trees
lobstr::ast(1 + 2 * 3) # can be written in prefix form

# Code can generate code - we can also use code to create new trees.

call2("f", 1, 2, 3) # constructs a function call from its components
call2("+", 1, call2("*", 2, 3))

xx <- expr(x + x)
yy <- expr(y + y)
expr(!!xx / !!yy) # !!x inserts the code tree stored in x into the expression.

# To generate an expression that computes the coefficient of variation:
cv <- function(var) {
  var <- enexpr(var)
  expr(sd(!!var) / mean(!!var))
}
cv(x)
cv(x + y)
cv(`)`) # not very useful

# Evaluation runs code
eval(expr(x + y), env(x = 1, y = 10)) # takes an expression and an environment
eval(expr(x + y), env(x = 2, y = 100))

x <- 10
y <- 100
eval(expr(x + y)) # omitting the environment, eval uses the current environment

# advantages of evaluating code manually
# To temporarily override functions to implement a domain specific language.
# To add a data mask so you can refer to variables in a data frame as if they are variables in an environment.

# Customising evaluation with functions
# evaluating code in a special environment where * and + have been overridden to work with strings instead of numbers:
string_math <- function(x) {
  e <- env(
    caller_env(),
    `+` = function(x, y) paste0(x, y),
    `*` = function(x, y) strrep(x, y)
  )
  
  eval(enexpr(x), e)
}

name <- "Hadley"
string_math("Hello " + name)
string_math(("x" * 2 + "-y") * 3)

library(dplyr) # running code in an environment that generates SQL for execution in a remote database
con <- DBI::dbConnect(RSQLite::SQLite(), filename = ":memory:")
mtcars_db <- copy_to(con, mtcars)

mtcars_db %>%
  filter(cyl > 2) %>%
  select(mpg:hp) %>%
  head(10) %>%
  show_query()

DBI::dbDisconnect(con)

# Customising evaluation with data

df <- data.frame(x = 1:5, y = sample(5))
eval_tidy(expr(x + y), df) # takes a data mask
# Evaluating with a data mask is a useful technique for interactive analysis 
# because it allows you to write x + y rather than df$x + df$y.

with2 <- function(df, expr) {
  eval_tidy(enexpr(expr), df)
} # similar 
with2(df, x + y)

# Quosures

with2 <- function(df, expr) { # modifying
  a <- 1000
  eval_tidy(enexpr(expr), df)
}

df <- data.frame(x = 1:3)
a <- 10
with2(df, x + a)
# The problem arises because we need to evaluate the captured expression in the environment 
# where it was written (where a is 10), not the environment inside of with2() (where a is 1000).

# solving using quosure - bundles an expression with an environment. 
#eval_tidy() knows how to work with quosures so all we need to do is switch out enexpr() for enquo().

with2 <- function(df, expr) {
  a <- 1000
  eval_tidy(enquo(expr), df)
}
with2(df, x + a)





