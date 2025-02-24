# Function operators
# A function operator is a function that takes one (or more) functions as input and returns a function as output. 
chatty <- function(f) {
  force(f)
  
  function(x, ...) {
    res <- f(x, ...)
    cat("Processing ", x, "\n", sep = "")
    res
  }
} # it gives you a window to see how functionals
f <- function(x) x ^ 2
s <- c(3, 2, 1)

purrr::map_dbl(s, chatty(f))

library(purrr)
library(memoise)

# One advantage of for-loops is that if one of the iterations fails, you can still access all the results up to the failure:

x <- list(
  c(0.512, 0.165, 0.717),
  c(0.064, 0.781, 0.427),
  c(0.890, 0.785, 0.495),
  "oops"
)

out <- rep(NA_real_, length(x))
for (i in seq_along(x)) {
  out[[i]] <- sum(x[[i]])
}
out

map_dbl(x, sum)
safe_sum <- safely(sum) # is a function operator that transforms a function to turn errors into data.
safe_sum

str(safe_sum(x[[1]]))
str(safe_sum(x[[4]]))

out <- map(x, safely(sum)) # takes a function and returns a wrapped function which we can call 
str(out) # returns a list with two elements, result and error

out <- transpose(map(x, safely(sum)))
str(out)

ok <- map_lgl(out$error, is.null)
ok

x[!ok]
out$result[ok]

# GLMs can sometimes fail because of optimisation problems, but you still want to be able to try to fit all the models, 
# and later look back at those that failed:
fit_model <- function(df) {
  glm(y ~ x1 + x2 * x3, data = df)
}

models <- transpose(map(datasets, safely(fit_model)))
ok <- map_lgl(models$error, is.null)

datasets[!ok] # which data failed to converge?

models[ok] # which models were successful?

slow_function <- function(x) {
  Sys.sleep(1)
  x * 10 * runif(1)
}
system.time(print(slow_function(1)))

system.time(print(slow_function(1)))

slow_function <- function(x) {
  Sys.sleep(1)
  x * 10 * runif(1)
}
system.time(print(slow_function(1)))

system.time(print(slow_function(1)))

fib <- function(n) {
  if (n < 2) return(1)
  fib(n - 2) + fib(n - 1)
}
system.time(fib(23))
system.time(fib(24))

# It memoises a function, meaning that the function will remember previous inputs and return cached results. 
# Memoisation is an example of the classic computer science tradeoff of memory versus speed. A memoised function can run much faster, 
# but because it stores all of the previous inputs and outputs, it uses more memory.
fib2 <- memoise::memoise(function(n) {
  if (n < 2) return(1)
  fib2(n - 2) + fib2(n - 1)
})
system.time(fib2(23))

system.time(fib2(24))
# This is an example of dynamic programming, where a complex problem can be broken down into many overlapping subproblems, 
# and remembering the results of a subproblem considerably improves performance.

urls <- c(
  "adv-r" = "https://adv-r.hadley.nz", 
  "r4ds" = "http://r4ds.had.co.nz/"
  # and many many more
)
path <- paste(tempdir(), names(urls), ".html")

walk2(urls, path, download.file, quiet = TRUE)

# Add a small delay between each request to avoid hammering the server.
# Display a . every few URLs so that we know that the function is still working.

for(i in seq_along(urls)) {
  Sys.sleep(0.1)
  if (i %% 10 == 0) cat(".")
  download.file(urls[[i]], paths[[i]])
}

# we can use function operators to extract out pausing and showing progress and make them reusable.
delay_by <- function(f, amount) {
  force(f) # write a function operator that adds a small delay
  force(amount)
  
  function(...) {
    Sys.sleep(amount)
    f(...)
  }
} # The main trick is forcing evaluation of all arguments 
system.time(runif(100))

system.time(delay_by(runif, 0.1)(100))

walk2(urls, path, delay_by(download.file, 0.1), quiet = TRUE)

# Creating a function to display the occasional dot is a little harder, because we can no longer rely on the index from the loop. 
# we’ll use another function factory trick, so that the progress wrapper can manage its own internal counter

dot_every <- function(f, n) {
  force(f)
  force(n)
  
  i <- 0
  function(...) {
    i <<- i + 1
    if (i %% n == 0) cat(".")
    f(...)
  }
}
walk(1:100, runif)
walk(1:100, dot_every(runif, 10))

walk2(
  urls, path, 
  download.file %>% dot_every(10) %>% delay_by(0.1), 
  quiet = TRUE
)

# Exercises

# 1.
# Vectorised has two meanings. First, it means (broadly) that a function inputs a vector or vectors 
# and does something to each element. Secondly, it usually implies that these operations are implemented in a compiled language such as C or Fortran, so that the implementation is very fast.

# 2.
#  possibly() modifies functions to return a specified default value (otherwise) in case of an error and to suppress any error messages (quiet = TRUE).

# 3.
# safely() modifies functions to return a list, containing the elements result and error.
# safely() also provides the otherwise and quiet arguments. 

# 4.
# Both commands will print a dot every 10 downloads and will take the same amount of time to run, so the differences may seem quite subtle.

# 5.
# Memoising file.download() will only work if the files are immutable, i.e. if the file at a given URL is always the same.

# 6.
dir_compare <- function(old, new) {
  if (setequal(old, new)) {
    return()
  }
  
  added <- setdiff(new, old)
  removed <- setdiff(old, new)
  
  changes <- c(
    if (length(added) > 0) paste0(" * '", added, "' was added"),
    if (length(removed) > 0) paste0(" * '", removed ,
                                    "' was removed")
  )
  message(paste(changes, collapse = "\n"))
}

dir_compare(c("x", "y"), c("x", "y"))
dir_compare(c("x", "y"), c("x", "a"))

track_dir <- function(f) {
  force(f)
  function(...) {
    dir_old <- dir()
    on.exit(dir_compare(dir_old, dir()), add = TRUE)
    
    f(...)
  }
}

file_create <- track_dir(file.create)
file_remove <- track_dir(file.remove)

file_create("delete_me")
file_remove("delete_me")

# 4.
# a function operator that logs a timestamp and message to a file every time a function is run.
append_line <- function(path, ...) { 
  cat(..., "\n", sep = "", file = path, append = TRUE)
}

logger <- function(f, log_path) {
  force(f)
  force(log_path)
  
  append_line(log_path, "created at: ", as.character(Sys.time()))
  function(...) {
    append_line(log_path, "called at: ", as.character(Sys.time()))
    f(...)
  }
}

log_path <- tempfile()
mean2 <- logger(mean, log_path)
Sys.sleep(5)
mean2(1:4) 
Sys.sleep(1)
mean2(1:4)

readLines(log_path)

# 6.
delay_by <- function(f, amount) {
  force(f)
  force(amount)
  
  function(...) {
    Sys.sleep(amount)
    f(...)
  }
}
delay_atleast <- function(amount, f) {
  force(f)
  force(amount)
  
  # Store the last time the function was run
  last_time <- NULL
  
  # Return modified "delay-aware" function
  function(...) {
    if (!is.null(last_time)) {
      wait <- (last_time - Sys.time()) + amount
      if (wait > 0) {
        Sys.sleep(wait)
      }
    }
    
    # Update the time after the function has finished
    on.exit(last_time <<- Sys.time()) 
    
    f(...)
  }
}







