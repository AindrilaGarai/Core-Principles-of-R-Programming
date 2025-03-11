# Trade-offs

# S4 versus S3

# S4 tends to require more upfront design than S3, 
# and this investment is more likely to pay off on larger projects where greater resources are available.

# One large team effort where S4 is used to good effect is Bioconductor. 
# Bioconductor is similar to CRAN: it’s a way of sharing packages amongst a wider audience. 
# Bioconductor is smaller than CRAN (~1,300 versus ~10,000 packages, July 2017) and 
# the packages tend to be more tightly integrated because of the shared domain and 
# because Bioconductor has a stricter review process. 
# Bioconductor packages are not required to use S4, but most will because the key data structures 
# (e.g. SummarizedExperiment, IRanges, DNAStringSet) are built using S4.

# S4 is also a good fit for complex systems of interrelated objects, and it’s possible 
# to minimise code duplication through careful implementation of methods. - Matrix

# S4 makes it easy to provide a general method that works for all inputs, and then 
# provide more specialised methods where the inputs allow a more efficient implementation.

# R6 versus S3:
# R6 is a profoundly different OO system from S3 and S4 
# because it is built on encapsulated objects, rather than generic functions.

# A generic is a regular function so it lives in the global namespace. 
# An R6 method belongs to an object so it lives in a local namespace. 
# This influences how we think about naming.

# R6’s reference semantics allow methods to simultaneously return a value and modify an object. 
# This solves a painful problem called “threading state”.

# You invoke an R6 method using $, which is an infix operator.

# Namespacing:
# Generic functions are global: all packages share the same namespace.
# Encapsulated methods are local: methods are bound to a single object.

# The advantage of a global namespace is that multiple packages can use the same verbs for working with different types of objects.
# The disadvantage of a global namespace is that it forces you to think more deeply about naming.
plot(data)       # plot some data
plot(bank_heist) # plot a crime
plot(land)       # create a new plot of land
plot(movie)      # extract plot of a movie

# This problem doesn’t occur with R6 methods because they are scoped to the object. 
# there is no implication that the plot method of two different R6 objects has the same meaning.
data$plot()
bank_heist$plot()
land$plot()
movie$plot()

# S3 generics must have the same core arguments.
# R6 methods can vary more widely and use more specific and evocative argument names.
# A secondary advantage of local namespacing is that creating an R6 method is very cheap.

# Threading state
# One challenge of programming with S3 is when you want to both return a value and modify the object.

# push() adds a new object to the top of the stack.
# pop() returns the top most value, and removes it from the stack.

new_stack <- function(items = list()) {
  structure(list(items = items), class = "stack")
} # pushing an object to the stack simply appends to this list.

push <- function(x, y) {
  x$items <- c(x$items, list(y))
  x
}

# Implementing pop() is more challenging because it has to both return a value and have a side-effect
pop <- function(x) {
  n <- length(x$items)
  
  item <- x$items[[n]]
  x$items <- x$items[-n]
  
  list(item = item, x = x)
}

s <- new_stack()
s <- push(s, 10)
s <- push(s, 20)

out <- pop(s)
out$item

s <- out$x
s
# This problem is known as threading state or accumulator programming, 
# because no matter how deeply the pop() is called, you have to 
# thread the modified stack object all the way back to where it lives.

library(zeallot)

c(value, s) %<-% pop(s)
value

# An R6 implementation of a stack is simpler because 
# $pop() can modify the object in place, and return only the top-most value:

Stack <- R6::R6Class("Stack", list(
  items = list(),
  push = function(x) {
    self$items <- c(self$items, x)
    invisible(self)
  },
  pop = function() {
    item <- self$items[[self$length()]]
    self$items <- self$items[-self$length()]
    item
  },
  length = function() {
    length(self$items)
  }
))

# MORE NATURAL CODE
s <- Stack$new()
s$push(10)
s$push(20)
s$pop()

s <- Stack$new()
s$
  push(10)$
  push(20)$
  pop()

# scales in ggplot2 are complex because they need to combine data across every facet and every layer.

# Method chaining:
# The pipe, %>%, is useful because it provides an infix operator that makes it easy to compose functions from left-to-right. 
# The pipe is not so important for R6 objects because they already use an infix operator: $.
# This allows the user to chain together multiple method calls in a single expression, a technique known as method chaining.

s <- Stack$new()
s$
  push(10)$
  push(20)$
  pop()

# any R6 method that is primarily called for its side-effects (usually modifying the object) should return invisible(self).
# The primary advantage of method chaining is that you can get useful autocomplete; 
# the primary disadvantage is that only the creator of the class can add new methods