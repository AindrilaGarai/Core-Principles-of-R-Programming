# The goal is to understand the difference between names and values, when R will copy an object. 

# Quiz
# 1.
df <- data.frame(runif(3), runif(3))
names(df) <- c(1, 2)
df$'3' <- df$'1' + df$'2'
df

# 2.
x <- runif(1e6)
y <- list(x, x, x)
obj_size(y) # memory occupied by y 

# 3.
a <- c(1, 5, 3, 2)
b <- a
b[[1]] <- 10 # in this line a is copied when b is modified

library(lobstr)

# Binding basics
x <- c(1, 2, 3)
# It’s creating an object, a vector of values, c(1, 2, 3).
# And it’s binding that object to a name, x.
# the object, or value, doesn’t have a name; it’s actually the name that has a value.
# a name as a reference to a value.

y <- x # you get another binding to the existing object

obj_addr(x)
obj_addr(y) # both x and y point to the same identifier

# Non-syntatic names
?Reserved

('-abc' <- 1)
('if' <- 1)

# exercises

# 1.
a <- 1:10 
b <- a 
c <- b # same object binding to different names
d <- 1:10 # different obj binding to another name

obj_addr(a); obj_addr(b); obj_addr(c); obj_addr(d)
list_of_names <- list(a, b, c, d)
obj_addrs(list_of_names) # first 3 are same, 4th is different

#2.
obj_addr(mean); obj_addr(base::mean); obj_addr(get("mean")); obj_addr(evalq(mean)); obj_addr(match.fun("mean"))
# all are same underlying function object

mean_functions <- list(
  mean,
  base::mean,
  get("mean"),
  evalq(mean),
  match.fun("mean")
)

unique(obj_addrs(mean_functions))

# 3.
# Column names are often data, and the underlying make.names() transformation is non-invertible, 
# so the default behavior corrupts data. To avoid this, set check.names = FALSE.

# 4.
make.names("")  # pre-pending "x"
make.names(".1")
make.names("non-valid") # "." replacement
make.names("@") # pre-pending "X" + "." replacement 
make.names("  R") # pre-pending "X" + ".." replacement 
make.names("if") # "." suffix

# 5.
make.names(".123e1") # pre-pending "x", because it starts with one dot which is followed by a number. 
# This makes it a double, 1.23

# Copy-on-modify

x <- c(1, 2, 3)
y <- x
y[[3]] <- 4
x; y # x and y are bound to the same underline value, then y got copied when modified

# to see when the object get copied
x <- c(1, 2, 3)
cat(tracemem(x), "\n")

y <- x
y[[3]] <- 4L 
# Most of the time it makes no difference - 4 and 4L
# but sometimes you can use it to get your code to run faster and consume less memory. A double ("numeric") vector uses 8 bytes per element. 
# An integer vector uses only 4 bytes per element.

y[[3]] <- 5L # here y won't get copied because, the new object only has a single name bound to it,
# so, R applies modify-in-place optimisation

untracemem(y) # it turns tracing off

# Function calls
f <- function(a) {
  a
} # f has a formal argument, a, which becomes a binding in the execution env when the function is run

x <- c(1, 2, 3)
cat(tracemem(x), "\n")

z <- f(x) # x and z point to the same obj.
# if f() modified x, R would create a new copy, z would bind that copy
untracemem(x)

# lists
l1 <- list(1, 2, 3) # instead of storing value itself, it stores references to them
l2 <- l1
l2[[3]] <- 4 # like vectors, copy-on-modify behavior

# shallow copy: the list objs and bindings are copied but the values pointed by the binding are not
# deep copy: the content of every ref are copied

ref(l1, l2) # to cross-ref of shared components

tracemem(l1)
obj_addrs(l1)

# Data Frames
d1 <- data.frame(x = c(1, 5, 6), y = c(2, 4, 3))
d2 <- d1
d2[, 2] <- d2[, 2] * 2 # only that column needs to be modified
d3 <- d1
d3[1, ] <- d3[1, ] * 3 # every column is copied and modified if we modify a row

ref(d1, d2, d3)

# character vectors - a vector of strings
x <- c("a", "a", "abc", "d")
# global string pool- each element of a character vector is a pointer to a unique string in the pool
ref(x, character = TRUE)

# exercise

# 1.
tracemem(1:10) # As no copies will be made, it is not useful to track the object for copying.

# 2.
x <- c(1L, 2L, 3L)
tracemem(x)
x[[3]] <- 4 # The replacement call assigns a double to the third element of x, which triggers copy-on-modify.

# 3.
a <- 1:10
b <- list(a, a)
c <- list(b, a, 1:10)
ref(c)

# 4.
x <- list(1:10)
x[[2]] <- x
ref(x)
tracemem(x)

# Object size
obj_size(letters)
obj_size(ggplot2::diamonds)

x <- runif(1e6)
obj_size(x)

y <- list(x,x,x)
obj_size(y) # same size as R uses a global string pool character vectors, repeating a tring 1000 times doesn't 
# make it take up 1000 times as much memory

obj_size(list(NULL, NULL, NULL)) # size of an empty list - 80 B

banana <- "bananas bananas bananas"
obj_size(banana)
obj_size(rep(banana, 100))

obj_size(x,y)
# if obj_size(x) + obj_size(y) = obj_size(x,y), x and y have no shared values

# ALTREP: alternative representation- 
# instead of storing every single number in the sequence, R just stores the first and last number. 
# This means that every sequence, no matter how large, is the same size

obj_size(1:3)
obj_size(1:1e3)
obj_size(1:1e6)
obj_size(1:1e9)

# exercise

# 1.
y <- rep(list(runif(1e4)), 100)
object.size(y) # doesn’t account for shared elements within lists, so it's large
obj_size(y)

# 2.
funs <- list(mean, sd, var)
obj_size(funs)
# All three functions are built-in to R as part of the {base} and {stats} packages and hence always available. 
# So, what does it mean to measure the size of something that’s already included in R?

base_pkgs <- c(
  "package:stats", "package:graphics", "package:grDevices",
  "package:utils", "package:datasets", "package:methods",
  "package:base"
)

library(magrittr) # for pipe operator
base_objs <- base_pkgs %>%
  lapply(as.environment) %>%
  lapply(function(x) mget(ls(x, all.names = TRUE), x)) %>%
  setNames(base_pkgs)

sum(lengths(base_objs))
vapply(base_objs, obj_size, double(1)) / 1024^2
as.numeric(obj_size(!!!base_objs)) / 1024^2 # Check if we've over-counted

# 3.
a <- runif(1e6)
obj_size(a)

b <- list(a, a)
obj_size(b) # same as a 
obj_size(a, b) # no modify, so same as a or b

b[[1]][[1]] <- 10
obj_size(b) # modified, so increased
obj_size(a, b) # same as highest between these two i.e. b

b[[2]][[1]] <- 10
obj_size(b) # modified, so increased
obj_size(a, b) # as b doesn’t share references with a anymore, the memory usage of the combined objects increases.

obj_size(list()); obj_size(double()); obj_size(character())
obj_size(double(1)); obj_size(double(2))
a <- runif(1e6); obj_size(a)

# Modify-in-place
# modifying an R object usually creates a copy without two exceptions:

# Objects with a single binding
v <- c(1,2,3)
v[[3]] <- 4

# When it comes to bindings, R can currently only count 0, 1, or many. 
# That means that if an object has two bindings, and one goes away, 
# the reference count does not go back to 1: one less than many is still many. 
# In turn, this means that R will make copies when it sometimes doesn’t need to.

# Whenever you call the vast majority of functions, it makes a reference to the object.
# The only exception are specially written “primitive” C functions. 

# Together, these two complications make it hard to predict whether or not a copy will occur. 
# Instead, it’s better to determine it empirically with tracemem().

x <- data.frame(matrix(runif(5 * 1e4), ncol = 5))
medians <- vapply(x, median, numeric(1))

for (i in seq_along(medians)) {
  x[[i]] <- x[[i]] - medians[[i]] # slow because each iteration of the loop copies the data frame
}

cat(tracemem(x), "\n")
for (i in 1:5) {
  x[[i]] <- x[[i]] - medians[[i]] 
}
untracemem(x)

# each iteration copies the data frame three times! Two copies are made by [[.data.frame, and 
# a further copy is made because [[.data.frame is a regular function that increments the reference count of x.
# all are shallow copies  
                                                                                                                                                      
# reduce the number of copies by using a list, modifying a list uses internal C code,
# only a single copy is made, referenced are not incremented
y <- as.list(x)
cat(tracemem(y), "\n")
#> <0x7f80c5c3de20>

for (i in 1:5) {
  y[[i]] <- y[[i]] - medians[[i]]
}                                                                                                                                                      

# Environments
# it is always modified in place: this property is called reference semantics
# when you modify an environment all existing bindings to that environment continue to have the same reference

e1 <- rlang::env(a = 1, b = 2, c = 3)
e2 <- e1
e1$c <- 4
e2$c
ref(e1, e2)

e <- rlang::env()
e$self <- e # env can contain themselves
ref(e) 

# exercise

# 1.
x <- list()
x[[1]] <- x
x # copy-on-modify prevents the creation of a circular list

x <- list()  
obj_addr(x)
tracemem(x)
x[[1]] <- x  # Copy-on-modify triggers new copy
obj_addr(x)      
obj_addr(x[[1]])  

# 2.
create_random_df <- function(nrow, ncol) {
  random_matrix <- matrix(runif(nrow * ncol), nrow = nrow)
  as.data.frame(random_matrix)
}
create_random_df(2, 2)

subtract_df <- function(x, medians) {
  for (i in seq_along(medians)) {
    x[[i]] <- x[[i]] - medians[[i]]
  }
  x}

subtract_list <- function(x, medians) {
  x <- as.list(x)
  x <- subtract_df(x, medians)
  list2DF(x)
}

benchmark_medians <- function(ncol) {
  df <- create_random_df(nrow = 1e4, ncol = ncol)
  medians <- vapply(df, median, numeric(1), USE.NAMES = FALSE)
  
  bench::mark(
    "data frame" = subtract_df(df, medians),
    "list" = subtract_list(df, medians),
    time_unit = "ms"
  )
}
benchmark_medians(1)

results <- bench::press( # allows us to run our helper across a grid of parameters
  ncol = c(1, 10, 50, 100, 250, 300, 400, 500, 750, 1000),
  benchmark_medians(ncol)
)

library(ggplot2)
ggplot(results, aes(ncol, median, col = attr(expression, "description"))) +
  geom_point(size = 2) +
  geom_smooth() +
  labs(x = "Number of Columns", y = "Execution Time (ms)", colour = "Data Structure") +
  theme(legend.position = "top")

# working with the data frame, the execution time grows quadratically with the number of columns
# the first column must be copied n times, the second column n-1 times, and so on. 
# When working with a list, the execution time increases only linearly.

# 4.
x <- new.env()
tracemem(x) # it is not useful to trace NULL, environments, promises, weak references, or external pointer objects, as these are not duplicated
# Environments are always modified in place.

# unbinding and the garbage collector
x <- 1:3
y <- 2:4
rm(x) # neither object is bound to a name

# garbage collector (GC)- it frees up memory by deleting R obj that are no longer used
# R uses a tracing GC- This means it traces every object that’s reachable from the global environment, 
# and all objects that are, in turn, reachable from those objects.

# GC runs automatically whenever R needs more memory to create a new object. 
# call gcinfo(TRUE) and GC will print a message to the console every time it runs.
# The reason to call gc() is to ask R to return memory to your operating system so other programs can use it.

gc()
mem_used() # is a wrapper around gc() that prints the total number of bytes used
# it is not equal to the amount of memory reported by OS: 3 reasons
# 1. It includes objects created by R but not by the R interpreter.
# 2. R might be holding on to memory because the OS hasn’t yet asked for it back.
# 3. R counts the memory occupied by objects but there may be empty gaps due to deleted objects. 
# This problem is known as memory fragmentation.









