# The condition system provides a paired set of tools 
# that allow the author of a function to indicate 
# that something unusual is happening, 
# and the user of that function to deal with it.

library(rlang)

# QUIZ

# 1.
# error, warning, and message.
# 2.
# try() or tryCatch().
# 3. 
# tryCatch() creates exiting handlers which will terminate the execution of wrapped code; 
# withCallingHandlers() creates calling handlers which don’t affect the execution of wrapped code.
# 4.
# Because you can then capture specific types of error with tryCatch(), 
# rather than relying on the comparison of error strings, which is risky, especially when messages are translated.

# Signalling conditions

# an interrupt, which indicates that the user has interrupted execution 
# by pressing Escape, Ctrl + Break, or Ctrl + C (depending on the platform).

stop("This is what an error looks like")
warning("This is what a warning looks like")
message("This is what a message looks like")

f <- function() g()
g <- function() h()
h <- function() stop("This is an error!")
f() # errors are signalled, or thrown, by stop()
h <- function() stop("This is an error!", call. = FALSE)
f()

fw <- function() {
  cat("1\n")
  warning("W1")
  cat("2\n")
  warning("W2")
  cat("3\n")
  warning("W3")
}
fw()

# To make warnings appear immediately, set options(warn = 1).
# To turn warnings into errors, set options(warn = 2). 
# Restore the default behaviour with options(warn = 0).

formals(1)
file.remove("this-file-doesn't-exist")
lag(1:3, k = 1.5)
as.numeric(c("18", "30", "50+", "345,678"))

# using a warning is clearly appropriate:
# When you deprecate a function you want to allow older code to continue to work (so ignoring the warning is OK) 
# but you want to encourage the user to switch to a new function.
# When you are reasonably certain you can recover from a problem

fm <- function() {
  cat("1\n")
  message("M1")
  cat("2\n")
  message("M2")
  cat("3\n")
  message("M3")
}

fm()

# EXERCISE
# 1.
file_remove_strict <- function(path) {
  if (!file.exists(path)) {
    stop("Can't delete the file \"", path, 
         "\" because it doesn't exist.",
         call. = FALSE
    )
  }
  file.remove(path)
}
saveRDS(mtcars, "mtcars.rds")
file_remove_strict("mtcars.rds")
file_remove_strict("mtcars.rds")

# 2.
multiline_msg <- function(appendLF = TRUE) {
  message("first", appendLF = appendLF)
  message("second", appendLF = appendLF)
  cat("third")
  cat("fourth")
}

multiline_msg(appendLF = TRUE)
multiline_msg(appendLF = FALSE)

# Ignoring conditions
# try() allows execution to continue even after an error has occurred.

f1 <- function(x) {
  log(x)
  10
}
f1("x")

f2 <- function(x) {
  try(log(x))
  10
}
f2("a")

default <- NULL
try(default <- read.csv("possibly-bad-input.csv"), silent = TRUE) # the argument is evaluated in the calling environment, not inside the function.
# Ignore errors 

suppressWarnings({ # suppress all warnings and messages
  warning("Uhoh!")
  warning("Another warning")
  1
})
suppressMessages({ # Ignore message
  message("Hello there")
  2
})
suppressWarnings({
  message("You can still see me")
  3
})

# Handling conditions
# Condition handlers allow us to temporarily override or supplement the default behaviour.

tryCatch( # defines exiting handlers
  error = function(cnd) {
    # code to run when error is thrown
  },
  code_to_run_while_handlers_are_active
)

withCallingHandlers( # defines calling handlers
  warning = function(cnd) {
    # code to run when warning is signalled
  },
  message = function(cnd) {
    # code to run when message is signalled
  },
  code_to_run_while_handlers_are_active
)

# tryCatch() defines exiting handlers; after the condition is handled, control returns to the context where tryCatch() was called. 
# withCallingHandlers() defines calling handlers; after the condition is captured control returns to the context where the condition was signalled. 

# Condition objects
cnd <- catch_cnd(stop("An error"))
str(cnd)
# message, a length-1 character vector containing the text to display to a user. To extract the message, use conditionMessage(cnd).
# call, the call which triggered the condition. As described above, we don’t use the call, so it will often be NULL. To extract it, use conditionCall(cnd).

# Exiting handlers
f3 <- function(x) {
  tryCatch(
    error = function(cnd) NA,
    log(x)
  )
}

f3("x")

# registers exiting handlers, and is typically used to handle error conditions.
# It allows you to override the default error behaviour.
tryCatch(
  error = function(cnd) 10,
  1 + 1
)
# If no conditions are signalled, or the class of the signalled condition does not match the handler name, the code executes normally:

tryCatch(
  error = function(cnd) 10,
  {
    message("Hi!")
    1 + 1
  }
)

# The handlers set up by tryCatch() are called exiting handlers because after the condition is signalled, 
# control passes to the handler and never returns to the original code, 
# effectively meaning that the code exits

tryCatch(
  message = function(cnd) "There",
  {
    message("Here")
    stop("This code is never run!")
  }
)

# The protected code is evaluated in the environment of tryCatch(), 
# but the handler code is not, because the handlers are functions.

tryCatch(
  error = function(cnd) {
    paste0("--", conditionMessage(cnd), "--")
  },
  stop("This is an error")
)

path <- tempfile()
tryCatch(
  {
    writeLines("Hi!", path) # ...
  },
  finally = {
    # always run
    unlink(path)
  }
)

# Calling handlers
# The handlers set up by tryCatch() are called exiting handlers, because 
# they cause code to exit once the condition has been caught.
tryCatch(
  message = function(cnd) cat("Caught a message!\n"), 
  {
    message("Someone there?")
    message("Why, yes!")
  }
)

# One important side-effect unique to calling handlers is the ability to muffle the signal
withCallingHandlers(
  message = function(cnd) cat("Caught a message!\n"), 
  {
    message("Someone there?")
    message("Why, yes!")
  }
)

withCallingHandlers(
  message = function(cnd) message("Second message"),
  message("First message")
)

withCallingHandlers(
  message = function(cnd) cat("Level 2\n"),
  withCallingHandlers(
    message = function(cnd) cat("Level 1\n"),
    message("Hello")
  )
)

tryCatch(
  message = function(cnd) cat("Level 2\n"),
  withCallingHandlers(
    message = function(cnd) cat("Level 1\n"),
    message("Hello")
  )
)

withCallingHandlers(
  message = function(cnd) {
    cat("Level 2\n")
    cnd_muffle(cnd)
  },
  withCallingHandlers(
    message = function(cnd) cat("Level 1\n"),
    message("Hello")
  )
)

# Muffles level 2 handler and the default handler
withCallingHandlers(
  message = function(cnd) cat("Level 2\n"),
  withCallingHandlers(
    message = function(cnd) {
      cat("Level 1\n")
      cnd_muffle(cnd)
    },
    message("Hello")
  )
)

# Call stacks
f <- function() g()
g <- function() h()
h <- function() message("!")

withCallingHandlers(f(), message = function(cnd) {
  lobstr::cst()
  cnd_muffle(cnd)
})

tryCatch(f(), message = function(cnd) lobstr::cst())

# exercise
# 1.
catch_cnd(stop("An error"))
catch_cnd(abort("An error"))

str(catch_cnd(stop("An error")))
str(catch_cnd(abort("An error")))

# 2.
show_condition <- function(code) {
  tryCatch(
    error = function(cnd) "error",
    warning = function(cnd) "warning",
    message = function(cnd) "message",
    {
      code
      NULL
    }
  )
}


show_condition(stop("!"))
show_condition(10)
show_condition(warning("?!"))
show_condition({
  10
  message("?")
  warning("?!")
})

show_condition(stop("!"))      
show_condition(10)             
show_condition(warning("?!"))  

show_condition({
  10
  message("?")
  warning("?!")
})

# 3.
withCallingHandlers(  # (1)
  message = function(cnd) message("b"),
  withCallingHandlers(  # (2)
    message = function(cnd) message("a"),
    message("c")
  )
) # because we haven’t handled the message, so it bubbles up to the outer calling handler.

# 4.
catch_cnd <- function(expr) {
  tryCatch(
    condition = function(cnd) cnd,
    {
      force(expr)
      return(NULL)
    }
  )
}
# catch_cnd() is a simple wrapper around tryCatch(). 
# If a condition is signalled, it’s caught and returned. 
# If no condition is signalled, execution proceeds sequentially and 
# the function returns NULL.

rlang::catch_cnd

# 5.
show_condition2 <- function(code) {
  tryCatch(
    condition = function(cnd) {
      if (inherits(cnd, "error"))   return("error")
      if (inherits(cnd, "warning")) return("warning")
      if (inherits(cnd, "message")) return("message")
    },
    {
      code
      NULL
    }
  )
}

# Test
show_condition2(stop("!"))
show_condition2(10)
show_condition2(warning("?!"))

show_condition2({
  10
  message("?")
  warning("?!")
})
# tryCatch() executes the code and captures any condition raised. 
# The function provided as the condition handles this condition. 
# In this case it dispatches on the class of the condition.

# custom condition
# the message might change over time, but also because messages can be translated into other languages.

abort(
  "error_not_found",
  message = "Path `blah.csv` not found", 
  path = "blah.csv"
)

log(letters)
log(1:10, base = letters)

my_log <- function(x, base = exp(1)) {
  if (!is.numeric(x)) {
    abort(paste0(
      "`x` must be a numeric vector; not ", typeof(x), "."
    ))
  }
  if (!is.numeric(base)) {
    abort(paste0(
      "`base` must be a numeric vector; not ", typeof(base), "."
    ))
  }
  
  base::log(x, base = base)
}

my_log(letters)
my_log(1:10, base = letters)

abort_bad_argument <- function(arg, must, not = NULL) {
  msg <- glue::glue("`{arg}` must {must}")
  if (!is.null(not)) {
    not <- typeof(not)
    msg <- glue::glue("{msg}; not {not}.")
  }
  
  abort("error_bad_argument", 
        message = msg, 
        arg = arg, 
        must = must, 
        not = not
  )
}

stop_custom <- function(.subclass, message, call = NULL, ...) {
  err <- structure(
    list(
      message = message,
      call = call,
      ...
    ),
    class = c(.subclass, "error", "condition")
  )
  stop(err)
}

err <- catch_cnd(
  stop_custom("error_new", "This is a custom error", x = 10)
)
class(err)
err$x

my_log <- function(x, base = exp(1)) {
  if (!is.numeric(x)) {
    abort_bad_argument("x", must = "be numeric", not = x)
  }
  if (!is.numeric(base)) {
    abort_bad_argument("base", must = "be numeric", not = base)
  }
  
  base::log(x, base = base)
}
my_log(letters)
my_log(1:10, base = letters)

# handling
# The following code captures the error, and then asserts it has the structure that we expect.

library(testthat)

err <- catch_cnd(my_log("a"))
expect_s3_class(err, "error_bad_argument")
expect_equal(err$arg, "x")
expect_equal(err$not, "character")

tryCatch(
  error_bad_argument = function(cnd) "bad_argument",
  error = function(cnd) "other error",
  my_log("a")
)

# exercise
# 1.
check_installed <- function(package) {
  if (!requireNamespace(package, quietly = FALSE)) {
    abort(
      "error_pkg_not_found",
      message = paste0("package '", package, "' not installed."),
      package = package
    )
  }
  
  TRUE
}

check_installed("ggplot2")
check_installed("ggplot3")

# 2.
# Instead of returning an error it might be preferable to throw a customised condition and place a standardised error message inside the metadata. 
# Then the downstream package could check for the class of the condition, rather than inspecting the message.

# applications:

fail_with <- function(expr, value = NULL) {
  tryCatch(
    error = function(cnd) value,
    expr
  )
}

fail_with(log(10), NA_real_)
fail_with(log("x"), NA_real_)

try2 <- function(expr, silent = FALSE) {
  tryCatch(
    error = function(cnd) {
      msg <- conditionMessage(cnd)
      if (!silent) {
        message("Error: ", msg)
      }
      structure(msg, class = "try-error")
    },
    expr
  )
}

try2(1)
try2(stop("Hi"))
try2(stop("Hi"), silent = TRUE)

foo <- function(expr) {
  tryCatch(
    error = function(cnd) error_val,
    {
      expr
      success_val
    }
  )
}

# We can use this to determine if an expression fails:
does_error <- function(expr) {
  tryCatch(
    error = function(cnd) TRUE,
    {
      expr
      FALSE
    }
  )
}

# or to capture any condition
catch_cnd <- function(expr) {
  tryCatch(
    condition = function(cnd) cnd, 
    {
      expr
      NULL
    }
  )
}

safety <- function(expr) {
  tryCatch(
    error = function(cnd) {
      list(result = NULL, error = cnd)
    },
    list(result = expr, error = NULL)
  )
}

str(safety(1 + 10))
str(safety(stop("Error!")))

warning2error <- function(expr) {
  withCallingHandlers(
    warning = function(cnd) abort(conditionMessage(cnd)),
    expr
  )
}
warning2error({
  x <- 2 ^ 4
  warn("Hello")
})

catch_cnds <- function(expr) {
  conds <- list()
  add_cond <- function(cnd) {
    conds <<- append(conds, list(cnd))
    cnd_muffle(cnd)
  }
  
  withCallingHandlers(
    message = add_cond,
    warning = add_cond,
    expr
  )
  
  conds
}

catch_cnds({
  inform("a")
  warn("b")
  inform("c")
})

catch_cnds <- function(expr) {
  conds <- list()
  add_cond <- function(cnd) {
    conds <<- append(conds, list(cnd))
    cnd_muffle(cnd)
  }
  
  tryCatch(
    error = function(cnd) {
      conds <<- append(conds, list(cnd))
    },
    withCallingHandlers(
      message = add_cond,
      warning = add_cond,
      expr
    )
  )
  
  conds
}

catch_cnds({
  inform("a")
  warn("b")
  abort("C")
})

log <- function(message, level = c("info", "error", "fatal")) {
  level <- match.arg(level)
  signal(message, "log", level = level)
}
log("This code was run")

record_log <- function(expr, path = stdout()) {
  withCallingHandlers(
    log = function(cnd) {
      cat(
        "[", cnd$level, "] ", cnd$message, "\n", sep = "",
        file = path, append = TRUE
      )
    },
    expr
  )
}

record_log(log("Hello"))

ignore_log_levels <- function(expr, levels) {
  withCallingHandlers(
    log = function(cnd) {
      if (cnd$level %in% levels) {
        cnd_muffle(cnd)
      }
    },
    expr
  )
}

record_log(ignore_log_levels(log("Hello"), "info"))

withRestarts(signalCondition(cond), muffle = function() NULL)

# exercises
# 1.
suppressErrors <- function(expr) {
  tryCatch(
    error = function(cnd) invisible(cnd),
    interrupt = function(cnd) {
      stop("Terminated by the user.",
           call. = FALSE
      )
    },
    expr
  )
}
suppressConditions <- function(expr) {
  suppressErrors(suppressWarnings(suppressMessages(expr)))
}
error_obj <- suppressConditions({
  message("message")
  warning("warning")
  abort("error")
})

error_obj

# 2.
message2error <- function(code) {
  withCallingHandlers(code, message = function(e) stop(e))
}
message2error <- function(code) {
  tryCatch(code, message = function(e) stop(e))
}

# Both functions differ in the way conditions are handled. withCallingHandlers() creates a calling handler, 
# which is executed from within the signalling function. This makes it possible to record a detailed call stack, 
# which helps us identify the signalling condition.

# tryCatch() defines an exiting handler, which means that the signalling function is terminated as soon as 
# a condition is raised. It also returns control to the context where tryCatch() was called.

message2error1 <- function(code) {
  withCallingHandlers(code, message = function(e) stop("error"))
}

message2error1({1;  message("hidden error"); NULL})
traceback()

message2error2 <- function(code) {
  tryCatch(code, message = function(e) (stop("error")))
}

message2error2({1; stop("hidden error"); NULL})
traceback()

# 3.
catch_cnds <- function(expr) {
  conds <- list()
  add_cond <- function(cnd) {
    conds <<- append(conds, list(cnd))
    cnd_muffle(cnd)
  }
  
  tryCatch(
    error = function(cnd) {
      conds <<- append(conds, list(cnd))
    },
    withCallingHandlers(
      message = add_cond,
      warning = add_cond,
      expr
    )
  )
  
  conds
}

catch_cnds({
  inform("message a")
  warn("warning b")
  inform("message c")
})

# 4.
bottles_of_beer <- function(i = 99) {
  message(
    "There are ", i,
    " bottles of beer on the wall, ", i,
    " bottles of beer."
  )
  while (i > 0) {
    tryCatch(
      Sys.sleep(1),
      interrupt = function(err) {
        i <<- i - 1
        if (i > 0) {
          message(
            "Take one down, pass it around, ", i,
            " bottle", if (i > 1) "s", " of beer on the wall."
          )
        }
      }
    )
  }
  message(
    "No more bottles of beer on the wall, ",
    "no more bottles of beer."
  )
}
bottles_of_beer()





