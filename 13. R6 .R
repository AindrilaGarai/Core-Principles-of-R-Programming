# install.packages("R6")
library(R6)

# R6 has two special properties:
# It uses the encapsulated OOP paradigm: methods belong to objects, not generics, and you call them like object$method().
# R6 objects are mutable: they are modified in place, and hence have reference semantics.

# R6 is very similar to a base OOP system called reference classes(RC)

# Classes and methods

# R6 only needs a single function call to create both the class and its methods
Accumulator <- R6Class("Accumulator", # first argument is the classname: it improves error messages
 list( #  second argument, public, supplies a list of methods (functions) and fields (anything else) that make up the public interface of the object.
  sum = 0, # field of Accumulate class
  add = function(x = 1) { # method of Accumulate class
    self$sum <- self$sum + x 
    invisible(self)
  })
)
Accumulator

x <- Accumulator$new() # construct a new object from the class

x$add(4) 
x$sum # In this class, the fields and methods are public, which means that you can get or set the value of any field.

# Method chaining
Accumulator <- R6Class("Accumulator", list(
  sum = 0,
  add = function(x = 1) {
    self$sum <- self$sum + x 
    invisible(self)
  })
)

# Side-effect R6 methods return self invisibly. 
# This returns the “current” object and makes it possible to chain together multiple method calls:
x$add(10)$add(10)$sum

x$
  add(10)$
  add(10)$
  sum # method chaining

# Important methods- two : $initialize() and $print()
# $initialize() overrides the default behaviour of $new()
# $print() allows you to override the default printing behaviour.

Person <- R6Class("Person", list(
  name = NULL, # field
  age = NA, # field
  initialize = function(name, age = NA) { # for validation 
    stopifnot(is.character(name), length(name) == 1)
    stopifnot(is.numeric(age), length(age) == 1)
    
    self$name <- name
    self$age <- age
  }
))

# If you have more expensive validation requirements, implement them in a separate $validate() and only call when needed.

hadley <- Person$new("Hadley", age = "thirty-eight")

hadley <- Person$new("Hadley", age = 38)

Person <- R6Class("Person", list(
  name = NULL,
  age = NA,
  initialize = function(name, age = NA) {
    self$name <- name
    self$age <- age
  },
  print = function(...) {
    cat("Person: \n")
    cat("  Name: ", self$name, "\n", sep = "")
    cat("  Age:  ", self$age, "\n", sep = "")
    invisible(self) # $print() should return invisible(self).
  }
))

hadley2 <- Person$new("Hadley")
hadley2
# Because methods are bound to individual objects, the previously created hadley object does not get this new method.

hadley
hadley$print

# From the perspective of R6, there is no relationship between hadley and hadley2; 
# they just coincidentally share the same class name.

# Instead of continuously creating new classes, it’s possible to modify the fields and methods of an existing class.

Accumulator <- R6Class("Accumulator")
Accumulator$set("public", "sum", 0) # Add new elements to an existing class
Accumulator$set("public", "add", function(x = 1) {
  self$sum <- self$sum + x 
  invisible(self)
}) # new methods and fields are only available to new objects; they are not retrospectively added to existing objects.

# Inheritance
AccumulatorChatty <- R6Class("AccumulatorChatty", 
                             inherit = Accumulator, # To inherit behaviour from an existing class
                             public = list(
                               add = function(x = 1) { 
                                 cat("Adding ", x, "\n", sep = "")
                                 super$add(x = x)
                               }
                             )
)

x2 <- AccumulatorChatty$new()
x2$add(10)$add(1)$sum # $add() overrides the superclass implementation

# Introspection: Every R6 object has an S3 class that reflects its hierarchy of R6 classes.

class(hadley2) # The S3 hierarchy includes the base “R6” class. 
names(hadley2) # list of all methods and fields.

# Exercises:

# 1.
BankAccount <- R6Class(
  classname = "BankAccount", 
  public = list(
    balance = 0,
    deposit = function(dep = 0) {
      self$balance <- self$balance + dep
      invisible(self)
    },
    withdraw = function(draw) {
      self$balance <- self$balance - draw
      invisible(self)
    }
  )
)

my_account <- BankAccount$new()
my_account$balance

my_account$
  deposit(5)$
  withdraw(15)$
  balance

# reate the first subclass that prevents us from going into overdraft 
# and throws an error in case we attempt to withdraw more than our current balance
BankAccountStrict <- R6Class(
  classname = "BankAccountStrict",
  inherit = BankAccount,
  public = list(
    withdraw = function(draw = 0) {
      if (self$balance - draw < 0) {
        stop("Your `withdraw` must be smaller ",
             "than your `balance`.",
             call. = FALSE
        )
      }
      super$withdraw(draw = draw)
    }
  )
)

my_strict_account <- BankAccountStrict$new()
my_strict_account$balance

my_strict_account$
  deposit(5)$
  withdraw(15)

my_strict_account$balance

# Finally, we create another subclass that charges a constant fee of 1 
# for each withdrawal which leaves the account with a negative balance.
BankAccountCharging <- R6Class(
  classname = "BankAccountCharging",
  inherit = BankAccount,
  public = list(
    withdraw = function(draw = 0) {
      if (self$balance - draw < 0) {
        draw <- draw + 1
      }
      super$withdraw(draw = draw)
    }
  )
)

my_charging_account <- BankAccountCharging$new()
my_charging_account$balance

my_charging_account$
  deposit(5)$
  withdraw(15)$
  withdraw(0)

my_charging_account$balance

# 2.
suit <- c("SPADE", "HEARTS", "DIAMOND", "CLUB")
value <- c("A", 2:10, "J", "Q", "K")
cards <- paste(rep(value, 4), suit)

ShuffledDeck <- R6Class(
  classname = "ShuffledDeck",
  public = list(
    deck = NULL,
    initialize = function(deck = cards) {
      self$deck <- sample(deck)
    },
    reshuffle = function() {
      self$deck <- sample(cards)
      invisible(self)
    },
    n = function() {
      length(self$deck)
    },
    draw = function(n = 1) {
      if (n > self$n()) {
        stop("Only ", self$n(), " cards remaining.", call. = FALSE)
      }
      
      output <- self$deck[seq_len(n)]
      self$deck <- self$deck[-seq_len(n)]
      output
    }
  )
)

my_deck <- ShuffledDeck$new()
my_deck$draw(52)
my_deck$draw(10)
my_deck$reshuffle()$draw(5)
my_deck$reshuffle()$draw(5)

# 3.
# We can't model a bank account or a deck of cards with an S3 class Because S3 classes obey R’s usual semantics of copy-on-modify: 
# every time you deposit money into your bank account or draw a card from the deck, you’d get a new copy of the object.

# 4.
Timezone <- R6Class(
  classname = "Timezone",
  public = list(
    get = function() {
      Sys.timezone()
    },
    set = function(value) {
      stopifnot(value %in% OlsonNames())
      
      old <- self$get()
      Sys.setenv(TZ = value)
      invisible(old)
    }
  )
)

tz <- Timezone$new()

old <- tz$set("Antarctica/South_Pole")
tz$get()

tz$set(old)
tz$get()

# 5.
WorkingDirectory <- R6Class(
  classname = "WorkingDirectory",
  public = list(
    get = function() {
      getwd()
    },
    set = function(value) {
      setwd(value)
    }
  )
)

# 6. 
# We can't model the time zone or current working directory with an S3 class because 
# S3 classes are not suitable for modelling a state that changes over time. 
# S3 methods should (almost) always return the same result when called with the same inputs.

# 7.
# R6 objects are built on top of environments. They have a class attribute, which is a character vector containing the class name, 
# the name of any super classes (if existent) and the string "R6" as the last element.

# Controlling access
# private allows you to create fields and methods that are only available from within the class, not outside of it.
# active allows you to use accessor functions to define dynamic, or active, fields.

# Privacy
# The private argument to R6Class works in the same way as the public argument
# Fields and methods defined in private are available within the methods using private$ instead of self$. 
# You cannot access private fields or methods outside of the class.
Person <- R6Class("Person", 
                  public = list(
                    initialize = function(name, age = NA) {
                      private$name <- name
                      private$age <- age
                    },
                    print = function(...) {
                      cat("Person: \n")
                      cat("  Name: ", private$name, "\n", sep = "")
                      cat("  Age:  ", private$age, "\n", sep = "")
                    }
                  ),
                  private = list(
                    age = NA,
                    name = NULL
                  )
)

hadley3 <- Person$new("Hadley")
hadley3
hadley3$name

# Active fields: allow you to define components that look like fields from the outside, 
# but are defined with functions, like methods.
# Active fields are implemented using active bindings-
# Each active binding is a function that takes a single argument: value.
# If the argument is missing(), the value is being retrieved; otherwise it’s being modified.

Rando <- R6::R6Class("Rando", active = list(
  random = function(value) {
    if (missing(value)) {
      runif(1)  
    } else {
      stop("Can't set `$random`", call. = FALSE)
    }
  }
))
x <- Rando$new()
x$random
x$random
x$random

# Active fields are particularly useful in conjunction with private fields, 
# because they make it possible to implement components 
# that look like fields from the outside but provide additional checks.

Person <- R6Class("Person", 
                  private = list(
                    .age = NA,
                    .name = NULL
                  ),
                  active = list(
                    age = function(value) {
                      if (missing(value)) {
                        private$.age
                      } else {
                        stop("`$age` is read only", call. = FALSE)
                      }
                    },
                    name = function(value) {
                      if (missing(value)) {
                        private$.name
                      } else {
                        stopifnot(is.character(value), length(value) == 1)
                        private$.name <- value
                        self
                      }
                    }
                  ),
                  public = list(
                    initialize = function(name, age = NA) {
                      private$.name <- name
                      private$.age <- age
                    }
                  )
)

hadley4 <- Person$new("Hadley", age = 38)
hadley4$name
hadley4$name <- 10
hadley4$age <- 20

# Exercises

# 1.
BankAccountStrict2 <- R6Class(
  classname = "BankAccountStrict2",
  public = list(
    deposit = function(dep = 0) {
      private$balance <- private$balance + dep
      invisible(self)
    },
    withdraw = function(draw = 0) {
      if (private$balance - draw < 0) {
        stop(
          "Your `withdraw` must be smaller ",
          "than your `balance`.",
          call. = FALSE
        )
      }
      private$balance <- private$balance - draw
      invisible(self)
    }
  ),
  private = list(
    balance = 0
  )
)

my_account_strict_2 <- BankAccountStrict2$new()

my_account_strict_2$deposit(5)
my_account_strict_2$withdraw(10)

# 2.
Password <- R6Class(
  classname = "Password",
  public = list(
    print = function(...) {
      cat("<Password>: ********\n")
      invisible(self)
    },
    set = function(value) {
      private$password <- value
    },
    check = function(password) {
      identical(password, private$password)
    }
  ),
  private = list(
    password = NULL
  )
)

my_pw <- Password$new()
my_pw$set("snuffles")
my_pw$password
my_pw
my_pw$check("snuggles")
my_pw$check("snuffles")

# 3.
Rando <- R6::R6Class(
  classname = "Rando",
  private = list(
    last_random = NULL
  ),
  active = list(
    random = function(value) {
      if (missing(value)) {
        private$last_random <- runif(1)
        private$last_random
      } else {
        stop("Can't set `$random`.", call. = FALSE)
      }
    },
    previous = function(value) {
      if (missing(value)) {
        private$last_random
      }
    }
  )
)
x <- Rando$new()
x$random
x$random
x$previous

# 4.
A <- R6Class(
  classname = "A",
  private = list(
    field = "foo",
    method = function() {
      "bar"
    }
  )
)

B <- R6Class(
  classname = "B",
  inherit = A,
  public = list(
    test = function() {
      cat("Field:  ", super$field, "\n", sep = "")
      cat("Method: ", super$method(), "\n", sep = "")
    }
  )
)

B$new()$test()
# We conclude that subclasses can access private methods from their superclasses, but not private fields.

# Reference semantics:  The primary consequence of reference semantics is that objects are not copied when modified.

y1 <- Accumulator$new() 
y2 <- y1
y1$add(10)
c(y1 = y1$sum, y2 = y2$sum)

# Instead, if you want a copy, you’ll need to explicitly $clone() the object.
y1 <- Accumulator$new() 
y2 <- y1$clone()

y1$add(10)
c(y1 = y1$sum, y2 = y2$sum)

# Reasoning: reference semantics makes code harder to reason about.
x <- list(a = 1)
y <- list(b = 2)

z <- f(x, y) # For the vast majority of functions, you know that the final line only modifies z.

# Take a similar example that uses an imaginary List reference class:
x <- List$new(a = 1)
y <- List$new(b = 2)

z <- f(x, y) # The final line is much harder to reason about: if f() calls methods of x or y, it might modify them as well as z.

# Finalizer: reference semantics makes sense to think about when an R6 object is finalized, i.e. when it’s deleted. 
x <- factor(c("a", "b", "c"))
levels(x) <- c("c", "b", "a") # the second is created when the levels are modified, leaving the first to be destroyed by the garbage collector.

# Finalizers usually play a similar role to on.exit(), cleaning up any resources created by the initializer.
TemporaryFile <- R6Class("TemporaryFile", list(
  path = NULL,
  initialize = function() {
    self$path <- tempfile()
  },
  finalize = function() {
    message("Cleaning up ", self$path)
    unlink(self$path)
  }
))
# The finalize method will be run when the object is deleted or when R exits.

tf <- TemporaryFile$new()
rm(tf) # Avoid these potential problems by only using the finalizer to clean up private resources allocated by initializer.

# R6 fields
# If you use an R6 class as the default value of a field, it will be shared across all instances of the object.

TemporaryDatabase <- R6Class("TemporaryDatabase", list(
  con = NULL,
  file = TemporaryFile$new(),
  initialize = function() {
    self$con <- DBI::dbConnect(RSQLite::SQLite(), path = file$path)
  },
  finalize = function() {
    DBI::dbDisconnect(self$con)
  }
))

db_a <- TemporaryDatabase$new()
db_b <- TemporaryDatabase$new()

db_a$file$path == db_b$file$path

# The problem arises because TemporaryFile$new() is called only once when the TemporaryDatabase class is defined. To fix the problem, we need to make sure 
# it’s called every time that TemporaryDatabase$new() is called, i.e. we need to put it in $initialize():

TemporaryDatabase <- R6Class("TemporaryDatabase", list(
  con = NULL,
  file = NULL,
  initialize = function() {
    self$file <- TemporaryFile$new()
    self$con <- DBI::dbConnect(RSQLite::SQLite(), path = file$path)
  },
  finalize = function() {
    DBI::dbDisconnect(self$con)
  }
))

db_a <- TemporaryDatabase$new()
db_b <- TemporaryDatabase$new()

db_a$file$path == db_b$file$path

# Exercises:
# 1.
FileWriter <- R6::R6Class(
  classname = "FileWriter",
  public = list(
    con = NULL,
    initialize = function(filename) {
      self$con <- file(filename, open = "a")
    },
    
    finalize = function() {
      close(self$con)
    },
    
    append_line = function(x) {
      cat(x, "\n", sep = "", file = self$con)
    }
  )
)

tmp_file <- tempfile()
my_fw <- FileWriter$new(tmp_file)

readLines(tmp_file)
my_fw$append_line("First")
my_fw$append_line("Second")
readLines(tmp_file)

# Why R6?

# R6 is much simpler.
# R6 has comprehensive online documentation at https://r6.r-lib.org.
# R6 has a simpler mechanism for cross-package subclassing, which just works without you having to think about it.

# RC mingles variables and fields in the same stack of environments 
# so that you get (field) and set (field <<- value) fields like regular values. 
# R6 puts fields in a separate environment so you get (self$field) and set (self$field <- value) with a prefix. 

# R6 is much faster than RC.
# RC is tied to R. That means if any bugs are fixed, you can only take advantage of the fixes by requiring a newer version of R.
