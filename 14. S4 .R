library(methods)

# S4 provides a formal approach to functional OOP.
# S4 provides both multiple inheritance (i.e. a class can have multiple parents) 
# and multiple dispatch (i.e. method dispatch can use the class of multiple arguments).

# A component of S4 is the slot, a named component of the object that is accessed using @

# class name and a definition of its slots
setClass("Person", 
         slots = c(
           name = "character", 
           age = "numeric"
         )
) # construct new objects

john <- new("Person", name = "John Smith", age = NA_real_)

is(john)
john@name
slot(john, "age")

# we’ll create a setter and getter
setGeneric("age", function(x) standardGeneric("age"))
setGeneric("age<-", function(x, value) standardGeneric("age<-"))

# then defining methods
setMethod("age", "Person", function(x) x@age)
setMethod("age<-", "Person", function(x, value) {
  x@age <- value
  x
})

age(john) <- 50
age(john)

# to identify S4 objects and generics 
sloop::otype(john)
sloop::ftype(age)

# Exercises

# 1.
# Objects of the S4 Period class have six slots named year, month, day, hour, minute, and .Data (which contains the number of seconds). 
# All slots are of type double. Most fields can be retrieved by an identically named accessor (e.g. lubridate::year() will return the field), use second() to get the .Data slot.

example_12345 <- lubridate::period(
  c(1, 2, 3, 4, 5), 
  c("second", "minute", "hour", "day", "week")
)
example_12345
str(example_12345)

# 2.
# Besides adding ? in front of a function call (i.e. ?method()), we may find:
# general documentation for a generic via ?genericName
# general documentation for the methods of a generic via methods?genericName
# documentation for a specific method via ClassName?methodName.

# Classes: setClass() with three arguments:
# The class name. By convention, S4 class names use UpperCamelCase.
# A named character vector that describes the names and classes of the slots (fields). 
# A prototype, a list of default values for each slot. 

setClass("Person", 
         slots = c(
           name = "character", 
           age = "numeric"
         ), 
         prototype = list(
           name = NA_character_,
           age = NA_real_
         )
)

me <- new("Person", name = "Hadley")
str(me)

# Inheritance
setClass("Employee", 
         contains = "Person", 
         slots = c(
           boss = "Person"
         ),
         prototype = list(
           boss = new("Person")
         )
)

str(new("Employee"))

# Introspection: To determine what classes an object inherits
is(new("Person"))
is(new("Employee"))
is(john, "Person") # To test if an object inherits from a specific class

# Redefinition: both definition and construction occur at run time.
setClass("A", slots = c(x = "numeric")) # registering a class definition in a (hidden) global variable.
a <- new("A", x = 10)

setClass("A", slots = c(a_different_slot = "numeric"))
a

# Helper
# A helper should always:
# Have the same name as the class, e.g. myclass().
# Have a thoughtfully crafted user interface with carefully chosen default values and useful conversions.
# Create carefully crafted error messages tailored towards an end-user.
# Finish by calling methods::new().

Person <- function(name, age = NA) {
  age <- as.double(age)
  
  new("Person", name = name, age = age) # a low-level constructor suitable for use
}

Person("Hadley")

# Validator: automatically checks that the slots have correct classes

Person(mtcars)
Person("Hadley", age = c(30, 37))

setValidity("Person", function(object) { # To enforce these additional constraints 
  if (length(object@name) != length(object@age)) {
    "@name and @age must be same length"
  } else {
    TRUE
  }
})

Person("Hadley", age = c(30, 37))

# The validity method is only called automatically by new(), so you can still create an invalid object by modifying it
alex <- Person("Alex", age = 30)
alex@age <- 1:10

validObject(alex) # check the validity yourself by calling 

# Exercises:

# 1.
# The person class from the {utils} package contains the slots given, family, role, email and comment
# Definition of the Person class
setClass("Person",
         slots = c(
           age = "numeric",
           given = "character",
           family = "character",
           role = "character",
           email = "character",
           comment = "character"
         ),
         prototype = list(
           age = NA_real_,
           given = NA_character_,
           family = NA_character_,
           role = NA_character_,
           email = NA_character_,
           comment = NA_character_
         )
)

# Helper to create instances of the Person class
Person <- function(given, family,
                   age = NA_real_,
                   role = NA_character_,
                   email = NA_character_,
                   comment = NA_character_) {
  age <- as.double(age)
  
  new("Person",
      age = age,
      given = given,
      family = family,
      role = role,
      email = email,
      comment = comment
  )
}

# Validator to ensure that each slot is of length one
setValidity("Person", function(object) {
  invalids <- c()
  if (length(object@age)     != 1 ||
      length(object@given)   != 1 ||
      length(object@family)  != 1 ||
      length(object@email)   != 1 ||
      length(object@comment) != 1) {
    invalids <- paste0("@name, @age, @given, @family, @email, ",
                       "@comment must be of length 1")
  } 
  
  known_roles <- c(
    NA_character_, "aut", "com", "cph", "cre", "ctb",
    "ctr", "dtc", "fnd", "rev", "ths", "trl"
  )
  
  if (!all(object@role %in% known_roles)) {
    paste(
      "@role(s) must be one of", 
      paste(known_roles, collapse = ", ")
    )
  }
  
  if (length(invalids)) return(invalids)  
  TRUE
})

# 2.
# It depends on the other arguments. If we inherit from another class, we get the same slots. 
# But something interesting happens if we don’t inherit from an existing class. We get a virtual class. A virtual class can’t be instantiated:
  
setClass("Human")
new("Human")
setClass("Programmer", contains = "Human")

# 3.
setClass("Factor",
         slots = c(
           data = "integer",
           levels = "character",
           ordered = "logical"
         ),
         prototype = list(
           data = integer(),
           levels = character(),
           ordered = FALSE
         )
)

new("Factor", data = c(1L, 2L), levels = letters[1:3])

setClass("Date2",
         slots = list(
           data = "integer"
         ),
         prototype = list(
           data = integer()
         )
)

new("Date2", data = 1L)

setClass("DataFrame",
         slots = c(
           data = "list",
           row.names = "character"
         ),
         prototype = list(
           data = list(),
           row.names = character(0)
         )
)

new("DataFrame", data = list(a = 1, b = 2))

# Generics and methods: find the specific implementation for the combination of classes passed to the generic.
# new S4 generic
setGeneric("myGeneric", function(x) standardGeneric("myGeneric"))

# Don't do this!
setGeneric("myGeneric", function(x) {
  standardGeneric("myGeneric")
})

# Signature: allows you to control the arguments that are used for method dispatch.
setGeneric("myGeneric", 
           function(x, ..., verbose = TRUE) standardGeneric("myGeneric"),
           signature = "x"
)

# Methods: three important arguments: the name of the generic, the name of the class, and the method itself
setMethod("myGeneric", "Person", function(x) {
  # method implementation
})
# the second argument to setMethod() is called the signature.
# In S4, unlike S3, the signature can include multiple arguments.

# Show method: that controls printing
args(getGeneric("show"))

setMethod("show", "Person", function(object) {
  cat(is(object)[[1]], "\n",
      "  Name: ", object@name, "\n",
      "  Age:  ", object@age, "\n",
      sep = ""
  )
})
john

# Accessors: all user-accessible slots should be accompanied by a pair of accessors. 
# define a generic so that multiple classes can use the same interface.
person_name <- function(x) x@name

setGeneric("name", function(x) standardGeneric("name"))
setMethod("name", "Person", function(x) x@name)

name(john)

# If the slot is also writeable, you should provide a setter function.
setGeneric("name<-", function(x, value) standardGeneric("name<-"))
setMethod("name<-", "Person", function(x, value) {
  x@name <- value
  validObject(x)
  x
})

name(john) <- "Jon Smythe"
name(john)

name(john) <- letters

# Exercises
# 1.
setGeneric("age", function(x) standardGeneric("age"))
setMethod("age", "Person", function(x) x@age)
setGeneric("age<-", function(x, value) standardGeneric("age<-"))
setMethod("age<-", "Person", function(x, value) {
  x@age <- value
  validObject(x)
  x
})

# 2.
# Within setGeneric() the name (1st argument) is needed as the name of the generic. 
# Then, the name also explicitly incorporates method dispatch via standardGeneric() within the generic’s body. 

# 3.
# is(object) returns the class of the object. is(object) also contains the superclass, for subclasses like Employee. 
# In order to always return the most specific class (the subclass), show() returns the first element of is(object).

# 4.
.Person <- setClass(
  "Person",
  slots = c(name = "character", age = "numeric")
)

hadley <- .Person(name = "Hadley")
hadley

formals("show")

# When we supply another name as a first element of our method, this element 
# will be matched to the correct object argument and we receive a warning.

setMethod("show", "Person", function(object) {
  cat(object@name, "creates hard exercises")
})

setMethod("show", "Person", function(x) {
  cat(x@name, "creates hard exercises")
})

hadley

# If we add more arguments to our method than our generic can handle, we will get an error.
setMethod("show", "Person", function(x, y) {
  cat(x@name, "is", x@age, "years old")
})

setMethod("show", "Person", function(object, y) {
  cat(object@name, "is", object@age, "years old")
})

# Method dispatch:
# Multiple inheritance, i.e. a class can have multiple parents,
# Multiple dispatch, i.e. a generic can use multiple arguments to pick a method.
# In practice, keep method dispatch as simple as possible by avoiding multiple inheritance, 
# and reserving multiple dispatch only for where it is absolutely necessary.

# To find the method that gets called, you start with the most specific class of the actual arguments, 
# then follow the arrows until you find a method that exists. 

# There are two pseudo-classes that you can define methods for. These are called pseudo-classes 
# because they don’t actually exist, but allow you to define useful behaviours.
# ANY, MISSING

# Things get more complicated when the class has multiple parents.
# The basic process remains the same:  The wrinkle is that now there are multiple arrows to follow, so you might find multiple methods. 
# If that happens, you pick the method that is closest, i.e. requires travelling the fewest arrows.

# With multiple inheritances it is hard to simultaneously prevent ambiguity, 
# ensure that every terminal method has an implementation, and minimise the number of defined methods 

# Multiple dispatch
# You follow multiple arrows in the same way as previously,
# but now each method is specified by two classes (separated by a comma)

# The main difference between multiple inheritance and multiple dispatch is that there are many more arrows to follow. 

# S4 and S3

# CLASSES
# To use an S3 class, you must first register it with setOldClass(). 
# You call this function once for each S3 class, giving it the class attribute.

setOldClass("data.frame")
setOldClass(c("ordered", "factor"))
setOldClass(c("glm", "lm"))

# it’s generally better to be more specific and provide a full S4 definition with slots and a prototype

setClass("factor",
         contains = "integer",
         slots = c(
           levels = "character"
         ),
         prototype = structure(
           integer(),
           levels = character()
         )
)
setOldClass("factor", S4Class = "factor")

# If an S4 object inherits from an S3 class or a base type, it will have a special virtual slot called .Data.

RangedNumeric <- setClass(
  "RangedNumeric",
  contains = "numeric",
  slots = c(min = "numeric", max = "numeric"),
  prototype = structure(numeric(), min = NA_real_, max = NA_real_)
)
rn <- RangedNumeric(1:10, min = 1, max = 10)
rn@min

rn@.Data

# It is possible to define S3 methods for S4 generics, and S4 methods for S3 generics 

# Generics:
setGeneric("mean")
selectMethod("mean", "ANY")
# setMethod() will automatically call setGeneric() if the first argument isn’t already a generic, 
# enabling you to turn any existing function into an S4 generic. 
# It is OK to convert an existing S3 generic to S4, but you should avoid converting regular functions to S4 generics in packages 
# because that requires careful coordination if done by multiple packages.

# Exercises:
# 1.
# The purpose of setOldClass() lies in registering an S3 class as a “formally defined class,” 
# so that it can be used within the S4 object-oriented programming system.
setOldClass("factor")    # use build-in definition for brevity

OrderedFactor <- setClass(
  "OrderedFactor",
  contains = "factor",   # inherit from registered S3 class
  slots = c(
    levels = "character",
    ordered = "logical"  # add logical order slot
  ),
  prototype = structure(
    integer(),
    levels = character(),
    ordered = logical()  # add default value
  )
)
# We can now register the (S3) ordered-class, while providing an “S4 template.” 
# We can also use the S4-class to create new object directly.

setOldClass("ordered", S4Class = "OrderedFactor")

x <- OrderedFactor(
  c(1L, 2L, 2L),
  levels = c("a", "b", "c"),
  ordered = TRUE
)
str(x)

# 2.
# We keep things simple and will just return "180cm" when the length() method is called on a Person object. 
# The method can be defined either as an S3 or S4 method.

length.Person <- function(x) "180cm"  # S3
setMethod("length", "Person", function(x) "180cm")  # S4