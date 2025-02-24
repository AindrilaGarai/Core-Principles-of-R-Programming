# ENVIRONMENT is a data structure of power scoping

# quiz

# 1.
# four ways: every object in an environment must have a name; 
# order doesn’t matter; 
# environments have parents; 
# environments have reference semantics.

# 2.
# The parent of the global environment is the last package that you loaded. 
# The only environment that doesn’t have a parent is the empty environment.

# 3.
# The enclosing environment of a function is the environment where it was created. 
# It determines where a function looks for variables.

# 4. 
# determine the env from which the function was called
# Use caller_env() or parent.frame().

# 5.
# <- always creates a binding in the current environment;
# <<- rebinds an existing name in a parent of the current environment.

library(rlang)

# Every name must be unique.
# The names in an environment are not ordered.
# An environment has a parent.
# Environments are not copied when modified.

e1 <- env(
  a = FALSE,
  b = "a",
  c = 2.3,
  d = 1:3,
)
e1$a
e1$d <- e1
e1$d$c # env is a bag of names with no order

# environments have reference semantics: 
# unlike most R objects, when you modify them, you modify them in place, and don’t create a copy. 
# that environments can contain themselves.

env_print(e1) # more information
env_name(e1)
env_names(e1)

# To compare environments, you need to use identical() and not ==. This is because == is a vectorised operator, and environments are not vectors.
identical(global_env(), current_env())
global_env() == current_env()

# parents
# every env has a parent, another env
e2a <- env(d = 4, e = 5)
e2b <- env(e2a, a = 1, b = 2, c = 3)

env_parent(e2b)
env_parent(e2a)

env_print(e2b)
env_print(e2a)

e2c <- env(empty_env(), d = 4, e = 5)
e2d <- env(e2c, a = 1, b = 2, c = 3)
# The ancestors of every environment eventually terminate with the empty environment. 

env_parents(e2b)
env_parents(e2d)
env_parents(e2b, last = empty_env())

x <- 0
f <- function() {
  x <<- 1
}
f()
x
# Super assignment, <<-, never creates a variable in the current environment, 
# but instead modifies an existing variable found in a parent environment.

e3 <- env(x = 1, y = 2)
e3$x
e3$z <- 3
e3[["z"]]
e3[[1]]
e3[c("x", "y")]
e3$xyz
env_get(e3, "xyz")
env_get(e3, "xyz", default = NA)

# There are two other ways to add bindings to an environment:

env_poke(e3, "a", 100)
e3$a

env_bind(e3, a = 10, b = 20)
env_names(e3)

env_has(e3, "a")

e3$a <- NULL
env_has(e3, "a")

env_unbind(e3, "a")
env_has(e3, "a")

# advanced bindings

env_bind_lazy(current_env(), b = {Sys.sleep(1); 1}) # creates delayed bindings, evaluated for the first time they are accessed

system.time(print(b))
system.time(print(b))

# active bindings - recomputed every time they are accessed
env_bind_active(current_env(), z1 = function(val) runif(1))
z1
z1

# exercise
# 1.
# done

# 2.
e1 <- env()
e1$loop <- e1
env_print(e1)
lobstr::ref(e1)

# 3.
e1 <- env()
e2 <- env()
e1$loop   <- e2
e2$dedoop <- e1
lobstr::ref(e1)
lobstr::ref(e2)

# 4.
# The first option doesn’t make sense, because elements of an environment are not ordered. 
# The second option would return two objects at the same time. 
           
# 5.
e3 <- new.env()
env_poke(e3, "a", 100)
e3$a
env_poke(e3, "a", 200)
e3$a

env_poke2 <- function(env, name, value) {
  if (env_has(env, name)) {
    abort(paste0("\"", name, "\" is already assigned to a value."))
  }
  
  env_poke(env, name, value)
  invisible(env)
}
env_poke2(e3, "b", 100)
e3$b
env_poke2(e3, "b", 200)

# 6.
rebind <- function(name, value, env = caller_env()) {
  if (identical(env, empty_env())) {
    stop("Can't find `", name, "`", call. = FALSE)
  } else if (env_has(env, name)) {
    env_poke(env, name, value)
  } else {
    rebind(name, value, env_parent(env))
  }
}
rebind("a", 10)
a <- 5
rebind("a", 10)
a

# Recursing over environments- to operate on every ancestor of an environment
where <- function(name, env = caller_env()) {
  if (identical(env, empty_env())) {
    # Base case - we’ve reached the empty environment and haven’t found the binding. We can’t go any further, so we throw an error.
    stop("Can't find ", name, call. = FALSE)
  } else if (env_has(env, name)) {
    # Success case - he name exists in this environment, so we return the environment.
    env
  } else {
    # Recursive case - the name was not found in this environment, so try the parent.
    where(name, env_parent(env))
  }
}

where("yyy")
x <- 5
where("x")
where("mean")

e4a <- env(empty_env(), a = 1, b = 2)
e4b <- env(e4a, x = 10, a = 11)

# removing the specifics of where() shows the structure more clearly
f <- function(..., env = caller_env()) {
  if (identical(env, empty_env())) {
    # base case
  } else if (success) {
    # success case
  } else {
    # recursive case
    f(..., env = env_parent(env))
  }
}

# Iteration versus recursion
f2 <- function(..., env = caller_env()) {
  while (!identical(env, empty_env())) {
    if (success) {
      # success case
      return()
    }
    # inspect parent
    env <- env_parent(env)
  }
  
  # base case
}

# exercise
# 1.
where <- function(name, env = caller_env()) {
  if (identical(env, empty_env())) {
    # Base case
    stop("Can't find `", name, "`.", call. = FALSE)
  } else if (env_has(env, name)) {
    # Success case
    env
  } else {
    # Recursive case
    where(name, env_parent(env))
  }
}
where2 <- function(name, env = caller_env(), results = list()) {
  if (identical(env, empty_env())) {
    # Base case
    results
  } else {
    # Recursive case
    if (env_has(env, name)) {
      results <- c(results, env)
    }
    where2(name, env_parent(env), results)
  }
}

# Test
e1a <- env(empty_env(), a = 1, b = 2)
e1b <- env(e1a, b = 10, c = 11)
e1c <- env(e1b, a = 12, d = 13)

where2("a", e1c)

# 2.
fget <- function(name, env = caller_env(), inherits = TRUE) {
  # Base case
  if (env_has(env, name)) {
    obj <- env_get(env, name)
    
    if (is.function(obj)) {
      return(obj)
    }
  }
  
  if (identical(env, emptyenv()) || !inherits) {
    stop("Could not find a function called \"", name, "\".",
         call. = FALSE
    )
  }
  
  # Recursive Case
  fget(name, env_parent(env))
}

# Test
mean <- 10
fget("mean", inherits = TRUE)

# Special environments
search()
search_envs()

# A function binds the current environment 
# when it is created. This is called the function 
# environment, and is used for lexical scoping. 
# Across computer languages, functions that capture 
# (or enclose) their environments are called closures, 
# which is why this term is often used interchangeably with function in R’s documentation.

y <- 1
f <- function(x) x + y
fn_env(f)

e <- env()
e$g <- function() 1
fn_env(e$g)
e
search_envs()

sd

# Every namespace environment has the same set of ancestors
# Each namespace has an imports environment that contains bindings to all the functions used by the package.
# Explicitly importing every base function would be tiresome, so the parent of the imports environment is the base namespace. 

# Execution environments
g <- function(x) {
  if (!env_has(current_env(), "p")) {
    message("Defining p")
    p <- 1
  } else {
    p <- p + 1
  }
  p
}
g(10) # This function returns the same value every time because of the fresh start principle
# Each time a function is called, a new environment is created to host execution. This is called the execution environment, and its parent is the function environment.

h <- function(x) {
  # 1.
  a <- 2 # 2.
  x + a
}
y <- h(1) # 3.
y

# An execution environment is usually ephemeral; once the function has completed, the environment will be garbage collected.

h2 <- function(x) {
  a <- x * 2
  current_env()
}

e <- h2(x = 10)
env_print(e)
fn_env(h2)

plus <- function(x) {
  function(y) x + y
}

plus_one <- plus(1)
plus_one
plus_one(2) # Its execution environment will have the captured execution environment of plus() as its parent

# exercise

# 1.
# search_envs() returns all the environments on the search path, which is “a chain of environments containing exported functions of attached packages” 
# env_parents(global_env()) will list all the ancestors of the global environment, therefore the global environment itself is not included. This also includes the “ultimate ancestor,” the empty environment. 

# 2.
f1 <- function(x1) {
  f2 <- function(x2) {
    f3 <- function(x3) {
      x1 + x2 + x3
    }
    f3(3)
  }
  f2(2)
}
f1(1)
# When f1 is defined it binds its parent environment, which is the global environment. 
# But f2 will only be created at runtime of f1 and will therefore bind f1’s execution environment. The value 1 will also bind to the name x1 at execution time. 
#The same holds true for x2, f3 and x3.

f1 <- function(x1) {
  f2 <- function(x2) {
    f3 <- function(x3) {
      x1 + x2 + x3
      print("f3")
      print(env_print())
    }
    f3(3)
    print("f2")
    print(env_print())
  }
  f2(2)
  print("f1") # the execution of f1(1) will print different results each time we run it.
  print(env_print())
}

f1(1)

# 3.
fget2 <- function(name, env = caller_env()) {
  # Base case
  if (env_has(env, name)) {
    obj <- env_get(env, name)
    
    if (is.function(obj)) {
      return(list(fun = obj, env = env))
    }
  }
  
  if (identical(env, emptyenv())) {
    stop("Could not find a function called \"", name, "\"",
         call. = FALSE
    )
  }
  
  # Recursive Case
  fget2(name, env_parent(env))
}

fstr <- function(fun_name, env = caller_env()) {
  if (!is.character(fun_name) && length(fun_name) == 1) {
    stop("`fun_name` must be a string.", call. = FALSE)
  }
  fun_env <- fget2(fun_name, env)
  
  list(
    where = fun_env$env,
    enclosing = fn_env(fun_env$fun)
  )
}

# Test
fstr("mean")

# call stacks: caller env 
# This provides the environment from which the function was called, and hence varies based 
# on how the function is called, not how the function was created

f <- function(x) {
  g(x = 2)
}
g <- function(x) {
  h(x = 3)
}
h <- function(x) {
  stop()
}

f(x = 1)
traceback()

# Instead of stop() + traceback() to understand the call stack, we’re going to use lobstr::cst() to print out the call stack tree:
h <- function(x) {
  lobstr::cst()
}
f(x = 1)

a <- function(x) b(x)
b <- function(x) c(x)
c <- function(x) x

a(f()) # x is lazily evaluated so this tree gets two branches

# Each element of the call stack is a frame44, also known as an evaluation context.
# Looking up variables in the calling stack rather than in the enclosing environment is called dynamic scoping. 

# exercise
ls2 <- function(env = caller_env()) {
  sort(env_names(env))
}

ls(all.names = TRUE)
ls2()
e1 <- env(a = 1, b = 2)
ls(e1)
ls2(e1)

# Explicit environments are useful in packages because they allow you to maintain state across function calls.
my_env <- new.env(parent = emptyenv())
my_env$a <- 1

get_a <- function() {
  my_env$a
}
set_a <- function(value) {
  old <- my_env$a
  my_env$a <- value
  invisible(old)
}

# A hashmap is a data structure that takes constant, O(1), time to find an object based on its name. Environments provide this behaviour by default, so can be used to simulate a hashmap.



