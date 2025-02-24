# quizs

# 1. 
# body, arguments, environment

# 2.
x <- 10
f1 <- function(x) {
  function() {
    x + 10
  }
}
f1(1)()

# 3.
1+(2*3)

# 4.
mean(c(1:10, NA), na.rm = TRUE)

# 5.

f2 <- function(a, b) {
  a * 10
}
f2(10, stop("This is an error!"))

# Functional Fundamentals

f02 <- function(x, y) {
  # A comment
  x + y
}

formals(f02) # controls how you call the function
body(f02) # the code inside the function
environment(f02) # determines how the function finds the value associated with the names
attr(f02, "srcref") # source references

# exceptional rule
sum # as these functions exists primarily in C, only found in base package
typeof(sum)
typeof('[')
formals(sum)
body(sum)
environment(sum)

typeof(rev)
formals(rev)
body(rev)

# first-class functions
f01 <- function(x) {
  sin(1 / x ^ 2)
}

lapply(mtcars, function(x) length(unique(x))) # anonymous function
Filter(function(x) !is.numeric(x), mtcars)
integrate(function(x) sin(x) ^ 2, 0, pi)

funs <- list( # closure
  half = function(x) x / 2,
  double = function(x) x * 2
)
funs$double(10)

# invoking a function
args <- list(1:10, na.rm = TRUE)
do.call(mean, args)

# exercise

# 1.
# In R there is no one-to-one mapping between functions and names. 
# A name always points to a single object, but an object may have zero, one or many names.
function(x) sd(x) / mean(x)

f1 <- function(x) (x - min(x)) / (max(x) - min(x))
f2 <- f1
f3 <- f1

# 2.
# second one
(function(x)3)()

# 3.
# The use of anonymous functions allows concise and elegant code in certain situations. 
# However, they miss a descriptive name and when re-reading the code, 
# it can take a while to figure out what they do. 
# That’s why it’s helpful to give long and complex functions a descriptive name. 
# It may be worthwhile to take a look at your own projects or other people’s code to reflect on this part of your coding style.

# 4.
# Use is.function() to test if an object is a function. 
# Consider using is.primitive() to test specifically for primitive functions.

# 5.
library(purrr)

objs <- mget(ls("package:base", all = TRUE), inherits = TRUE)
funs <- Filter(is.function, objs)

n_args <- funs %>% 
  map(formals) %>%
  map_int(length)

n_args %>% 
  sort(decreasing = TRUE) %>%
  head()

sum(n_args == 0)
# formals() returns NULL for primitive functions, and length(NULL) is 0.

n_args2 <- funs %>% 
  discard(is.primitive) %>% 
  map(formals) %>%
  map_int(length)

sum(n_args2 == 0) # Indeed, most of the functions with no arguments are actually primitive functions.

funs <- Filter(is.primitive, objs)
length(funs)

# 7.
# Primitive functions and functions created in the global environment do not print their environment.

# function composition
square <- function(x) x^2
deviation <- function(x) x-mean(x)

x <- runif(100)
sqrt(mean(square(deviation(x))))

out <- deviation(x)
out <- square(out)
out <- mean(out)
out <- sqrt(out)
out

library(magrittr)

x %>% # and then means %>%
  deviation() %>%
  square() %>%
  mean() %>%
  sqrt()

# Lexical Scoping
# scoping- the act of finding the value associated with a name
# lexical - a technical CS term that the scoping rules are parse-time rather than a run-time structure

x <- 10
g01 <- function() {
  x <- 20
  x
}
g01()

# R’s lexical scoping follows four primary rules:
# Name masking, Functions versus variables, A fresh start, Dynamic lookup

x <- 10
y <- 20
g02 <- function() {
  x <- 1
  y <- 2
  c(x, y)
}
g02()

x <- 2
g03 <- function() {
  y <- 1
  c(x, y)
}
g03()
y

x <- 1
g04 <- function() {
  y <- 2
  i <- function() {
    z <- 3
    c(x, y, z)
  }
  i()
}
g04()

g07 <- function(x) x + 1
g08 <- function() {
  g07 <- function(x) x + 100
  g07(10)
}
g08()

g09 <- function(x) x + 100
g10 <- function() {
  g09 <- 10
  g09(g09)
}
g10()

g11 <- function() {
  if (!exists("a")) {
    a <- 1
  } else {
    a <- a + 1
  }
  a
}
g11()
g11() # everytime function is called a new env to host its execution

g12 <- function() x + 1
x <- 15
g12()
x <- 20
g12()

codetools::findGlobals(g12)
environment(g12) <- emptyenv()
g12()

# exercise

# 1.
c <- 10
c(c=c)
# This code returns a named numeric vector of length one — with one element of the value 10 and the name "c". 
# The first c represents the c() function, 
# the second c is interpreted as a (quoted) name and the third c as a value.

# 3.
f <- function(x) {
  f <- function(x) {
    f <- function() {
      x ^ 2
    }
    f() + 1
  }
  f(x) * 2
}
f(10)

# lazy evaluation

h01 <- function(x) {
  10
}
h01(stop("This is an error!")) # as x is never used
# it is powered by a data structure a promise or a thunk

# A promise has three components:
  
# 1. An expression, like x + y, which gives rise to the delayed computation.
# 2. An environment where the expression should be evaluated, i.e. the environment where the function is called. 
y <- 10
h02 <- function(x) {
  y <- 100
  x + 1
}
h02(y)
h02(y <- 1000)
y

# 3. A value, which is computed and cached the first time a promise is accessed when the expression is evaluated in the specified environment. 
double <- function(x) { 
  message("Calculating...")
  x * 2
}
h03 <- function(x) {
  c(x, x)
}
h03(double(20))

# You cannot manipulate promises with R code. Promises are like a quantum state: 
# any attempt to inspect them with R code will force an immediate evaluation, making the promise disappear. 

h04 <- function(x = 1, y = x * 2, z = a + b) {
    a <- 10
    b <- 100
    c(x, y, z)
  }
h04()

h05 <- function(x = ls()) {
    a <- 1
    x
  }
h05() # ls() evaluated inside h05
h05(ls()) # evaluated in global environment

# Missing arguments
h06 <- function(x = 10) {
    list(missing(x), x) # missing() is best used sparingly
  }
str(h06())
str(h06(10))

args(sample) # if size is not supplied, sample() uses missing() to provide a default

sample <- function(x, size = NULL, replace = FALSE, prob = NULL) {
  if (is.null(size)) {
    size <- length(x)
  }
  x[sample.int(length(x), size, replace = replace, prob = prob)]
}

# With the binary pattern created by the %||% infix function, which uses the left side if it’s not NULL 
# and the right side otherwise, we can further simplify sample():
  
`%||%` <- function(lhs, rhs) {
    if (!is.null(lhs)) {
      lhs
    } else {
      rhs
    }
}

sample <- function(x, size = NULL, replace = FALSE, prob = NULL) {
  size <- size %||% length(x)
  x[sample.int(length(x), size, replace = replace, prob = prob)]
}

# Exercises
# 1.
x_ok <- function(x) {
    !is.null(x) && length(x) == 1 && x > 0
}
x_ok(NULL)
x_ok(1)
x_ok(1:3) # && short-circuits which means that if the left-hand side is FALSE it doesn’t evaluate the right-hand side (because it doesn’t matter). 
# Similarly, if the left-hand side of || is TRUE it doesn’t evaluate the right-hand side.

x_ok <- function(x) {
    !is.null(x) & length(x) == 1 & x > 0
  }
x_ok(NULL)
x_ok(1)
x_ok(1:3) # element wise checking

# 2.
f2 <- function(x = z) {
    z <- 100
    x
  }
f2() # lazy evaluation

# 3.
y <- 10
f1 <- function(x = {y <- 1; 2}, y = 0) {
  c(x, y)
}
f1() # name masking
y

# 4.
range("Sturges")

# The xlim argument of hist() defines the range of the histogram’s x-axis. 
# In order to provide a valid axis xlim must contain a numeric vector of exactly two unique values. 
# Consequently, for the default xlim = range(breaks)), breaks must evaluate to a vector with at least two unique values.
# During execution hist() overwrites the breaks argument. 
# The breaks argument is quite flexible and allows the users to provide the breakpoints directly or compute them in several ways. 
# Therefore, the specific behaviour depends highly on the input. 
# But hist ensures that breaks evaluates to a numeric vector containing at least two unique elements before xlim is computed.

# 5.
show_time <- function(x = stop("Error!")) {
    stop <- function(...) Sys.time()
    print(x)
  }
show_time()
# This function is confusing because its behaviour changes when x’s value is supplied directly. 
# Now the value from the calling environment will be used and the overwriting of stop() won’t affect x anymore.

show_time(x = stop("Error!"))

# 6.
# library() doesn’t require any arguments. When called without arguments library() invisibly returns a list of class libraryIQR, 
# which contains a results matrix with one row and three columns per installed package. 
# These columns contain entries for the name of the package (“Package”), the path to the package (“LibPath”) and the title of the package (“Title”)

# dot-dot-dot
# Functions can have a special argument ... With it, a function can take any number of additional arguments. 
# You can also use ... to pass those additional arguments on to another function.

i01 <- function(y, z) {
  list(y = y, z = z)
}
i02 <- function(x, ...) {
  i01(...)
}
str(i02(x = 1, y = 2, z = 3))

i03 <- function(...) {
    list(first = ..1, third = ..3) # by position
  }
str(i03(1, 2, 3))
  
i04 <- function(...) {
    list(...) # More useful is list(...), which evaluates the arguments and stores them in a list
  }
str(i04(a = 1, b = 2))

x <- list(c(1, 3, NA), c(4, NA, 6))
str(lapply(x, mean, na.rm = TRUE))

# For S3 generic, we need some way to allow methods to take arbitrary extra arguments. 
print(factor(letters), max.levels = 4)
print(y ~ x, showEnv = TRUE)

# ... two downsides:
# When you use it to pass arguments to another function, you have to carefully explain to the user where those arguments go. 
# A misspelled argument will not raise an error. This makes it easy for typos to go unnoticed
sum(1, 2, NA, na_rm = TRUE)

# Exercises
# 1.
sum(1, 2, 3)
mean(1, 2, 3)
sum(1, 2, 3, na.omit = TRUE) # coerced to 1
mean(1, 2, 3, na.omit = TRUE) # coerced to 1

# 2.
plot(1:10, col = "red", pch = 20, xlab = "x", col.lab = "blue")

# Exiting a function
# Most functions exit in one of two ways40: they either return a value, indicating success, or they throw an error, indicating failure. 

j01 <- function(x) {
    if (x < 10) {
      0
    } else {
      10
    }
  }
j01(5)
j01(15)

j02 <- function(x) {
    if (x < 10) {
      return(0)
    } else {
      return(10)
    }
  }

j03 <- function() 1
j03()

j04 <- function() invisible(1) # prevent automatic printing by applying invisible()
j04()
print(j04())
(j04())
str(withVisible(j04()))

a <- 2
(a <- 2)
a <- b <- c <- d <- 2

# Errors
j05 <- function() {
  stop("I'm an error")
  return(10)
}
j05()

j06 <- function(x) {
  cat("Hello\n")
  on.exit(cat("Goodbye!\n"), add = TRUE)
  if (x) {
    return(10)
  } else {
    stop("Error")
  }
}
j06(TRUE)
j06(FALSE)

cleanup <- function(dir, code) {
    old_dir <- setwd(dir)
    on.exit(setwd(old_dir), add = TRUE)
    
    old_opt <- options(stringsAsFactors = FALSE)
    on.exit(options(old_opt), add = TRUE)
  }

  with_dir <- function(dir, code) {
    old <- setwd(dir)
    on.exit(setwd(old), add = TRUE)
    
    force(code)
  }
getwd()
with_dir("~", getwd())

j08 <- function() {
    on.exit(message("a"), add = TRUE)
    on.exit(message("b"), add = TRUE)
  }
j08()

j09 <- function() {
    on.exit(message("a"), add = TRUE, after = FALSE)
    on.exit(message("b"), add = TRUE, after = FALSE)
  }
j09()

# Exercises

# 1.
# load() loads objects saved to disk in .Rdata files by save(). 
# When run successfully, load() invisibly returns a character vector containing the names of the newly loaded objects.

# 2.
# write.table() writes an object, usually a data frame or a matrix, to disk. The function invisibly returns NULL.

# 3.
with_dir <- function(dir, code) {
  old <- setwd(dir)
  on.exit(setwd(old))
  
  force(code)
}
# with_dir() takes a path for a working directory (dir) as its first argument. 
# This is the directory where the provided code (code) should be executed. 
# Therefore, the current working directory is changed in with_dir() via setwd()

# 4.
plot_pdf <- function(code) {
  pdf("test.pdf")
  on.exit(dev.off(), add = TRUE)
  code
}

# 5.
capture.output2 <- function(code) {
  temp <- tempfile()
  on.exit(file.remove(temp), add = TRUE, after = TRUE)
  
  sink(temp)
  on.exit(sink(), add = TRUE, after = TRUE)
  
  force(code)
  readLines(temp)
}
capture.output2(cat("a", "b", "c", sep = "\n"))

capture.output({1})
capture.output2({1})

# Everything that exists is an object.
# Everything that happens is a function call.
# — John Chambers

# prefix: foofy(a, b, c)
# infix: x + y. 
# replacement: names(df) <- c("a", "b", "c")
# special: [[, if, and for

# rewriting to prefix form
x+y
`+`(x,y)

names(df) <- c("x", "y", "z")
`names<-`(df, c("x", "y", "z"))

for(i in 1:10) print(i)
`for`(i, 1:10, print(i))

`(` <- function(e1) {
  if (is.numeric(e1) && runif(1) < 0.1) {
    e1 + 1
  } else {
    e1
  }
}
replicate(50, (1 + 2))
rm("(")

add <- function(x, y) x + y
lapply(list(1:3, 4:5), add, 3)
lapply(list(1:3, 4:5), `+`, 3)

k01 <- function(abcdef, bcde1, bcde2) {
  list(a = abcdef, b1 = bcde1, b2 = bcde2)
}
str(k01(1, 2, 3))
str(k01(2, 3, abcdef = 1))
# Can abbreviate long argument names:
str(k01(2, 3, a = 1))
# But this doesn't work because abbreviation is ambiguous
str(k01(1, 3, b = 1))

#  Infix functions
`%+%` <- function(a, b) paste0(a, b)
"new " %+% "string"

`% %` <- function(a, b) paste(a, b)
`%/\\%` <- function(a, b) paste(a, b)

"a" % % "b"
"a" %/\% "b"

`%-%` <- function(a, b) paste0("(", a, " %-% ", b, ")")
"a" %-% "b" %-% "c"

-1
+10

# Replacement functions
# Replacement functions act like they modify their arguments in place, and have the special name xxx<-. 
# They must have arguments named x and value, and must return the modified object. 

`second<-` <- function(x, value) {
  x[2] <- value
  x
}
x <- 1:10
second(x) <- 5L
x

x <- 1:10
tracemem(x)
second(x) <- 6L

`modify<-` <- function(x, position, value) {
  x[position] <- value
  x
}
modify(x, 1) <- 10
x

x <- c(a = 1, b = 2, c = 3)
names(x)
names(x)[2] <- "two"
names(x)

`*tmp*` <- x
x <- `names<-`(`*tmp*`, `[<-`(names(`*tmp*`), 2, "two"))
rm(`*tmp*`)

# Special forms
# Finally, there are a bunch of language features that are usually written in special ways, but also have prefix forms. These include parentheses

# exercise

# 1.
1 + 2 + 3
1 + (2 + 3)
if (length(x) <= 5) x[[5]] else x[[n]]

`+`(`+`(1, 2), 3)

`+`(1, `(`(`+`(2, 3)))
`+`(1, `+`(2, 3))

`if`(`<=`(length(x), 5), `[[`(x, 5), `[[`(x, n))

# 2.
x <- sample(c(1:10, NA), size = 20, replace = TRUE)
y <- runif(20, min = 0, max = 1)
cor(x, y, use = "pairwise.complete.obs", method = "kendall")

# 3.
x <- 1:3

`modify<-` <- function(x, position, value) {
  x[position] <- value
  x
}

# 4.
`random<-` <- function(x, value) {
  idx <- sample(length(x), 1)
  x[idx] <- value
  x
}

# 5.
`+` <- function(a, b = 0L) {
  if (is.character(a) && is.character(b)) {
    paste0(a, b)
  } else {
    base::`+`(a, b)
  }
}
+ 1
1 + 2
"a" + "b"
rm(`+`)

# 6.
repls <- apropos("<-", where = TRUE, mode = "function")
head(repls, 30)

repls_base <- repls[names(repls) == length(search())]
repls_base

repls_base_prim <- mget(repls_base, envir = baseenv()) %>%
  Filter(is.primitive, .) %>% 
  names()

repls_base_prim

# 7.
# names of infix functions are more flexible than regular R functions: they can contain any sequence of characters except “%.”

# 8.
`%xor%` <- function(a, b) {
  xor(a, b)
}
TRUE %xor% TRUE
FALSE %xor% TRUE

# 9.
`%n%` <- function(a, b) {
  intersect(a, b)
}

`%u%` <- function(a, b) {
  union(a, b)
}

`%/%` <- function(a, b) {
  setdiff(a, b)
}

x <- c("a", "b", "d")
y <- c("a", "c", "d")

x %u% y
x %n% y
x %/% y
