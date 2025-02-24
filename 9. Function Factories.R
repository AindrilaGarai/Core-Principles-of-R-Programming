# A function factory is a function that makes functions. 
power1 <- function(exp) {
  function(x) {
    x ^ exp
  }
}

square <- power1(2)
cube <- power1(3)

square(3) # manufactured functions
cube(3)

library(rlang)
library(ggplot2)
library(scales)

# The enclosing environment of the manufactured function is an execution environment of the function factory.

square
cube # printing the manufactured functions is not revealing because the bodies are identical; 
# the contents of the enclosing environment are the important factors.

env_print(square)
env_print(cube) # That shows us that we have two different environments
# The environments have the same parent, which is the enclosing environment of power1(), the global environment.

# env_print() shows us that both environments have a binding to exp, but to see its value,
# We can do that by first getting the environment of the function, and then extracting the values:
fn_env(square)$exp
fn_env(cube)$exp
# This is what makes manufactured functions behave differently from one another: names in the enclosing environment are bound to different values.

square(10)

x <- 2
square <- power1(x)
x <- 3

x <- 2
square <- power1(x)
x <- 3

power2 <- function(exp) {
  force(exp) # forcing evaluation
  function(x) {
    x ^ exp
  }
}

x <- 2
square <- power2(x)
x <- 3
square(2)

# Stateful functions

# The enclosing environment of the manufactured function is unique and constant.
# R has a special assignment operator, <<-, which modifies bindings in the enclosing environment.

new_counter <- function() {
  i <- 0
  
  function() {
    i <<- i + 1 # The super assignment operator, <<- rebinds an existing name found in a parent environment.
    i
  }
}

counter_one <- new_counter()
counter_two <- new_counter()

counter_one()
counter_one()
counter_two()

# Garbage collection
# the garbage collector to clean up any large temporary objects created inside a function.
# rm()
f1 <- function(n) {
  x <- runif(n)
  m <- mean(x)
  function() m
}

g1 <- f1(1e6)
lobstr::obj_size(g1)

f2 <- function(n) {
  x <- runif(n)
  m <- mean(x)
  rm(x)
  function() m
}

g2 <- f2(1e6)
lobstr::obj_size(g2)

# Exercises

# 1.
force 
# we prefer this explicit form, because using this function clearly indicates that 
# you’re forcing evaluation, not that you’ve accidentally typed x."

# 2.
# approxfun() takes a combination of data points (x and y values) as input and returns a stepwise linear (or constant) interpolation function.

x <- runif(10)
y <- runif(10)
plot(x, y, lwd = 10)

f_lin <- approxfun(x, y)
f_con <- approxfun(x, y, method = "constant")

# Both functions exactly reproduce their input y values
identical(f_lin(x), y)
identical(f_con(x), y)

# When we apply these functions to new x values, these are mapped to the lines connecting 
# the initial y values (linear case) or to the same y value as for the next smallest initial x value (constant case).

x_new <- runif(1000)

plot(x, y, lwd = 10)
points(x_new, f_lin(x_new), col = "cornflowerblue", pch = 16)
points(x_new, f_con(x_new), col = "firebrick", pch = 16)

f_lin(range(x))
f_con(range(x))

(eps <- .Machine$double.neg.eps)

f_lin(c(min(x) - eps, max(x) + eps))
f_con(c(min(x) - eps, max(x) + eps))

f_lin <- approxfun(x, y, rule = 2)
f_con <- approxfun(x, y, method = "constant", rule = 2)

f_lin(c(-Inf, Inf))
f_con(c(-Inf, Inf))

f_lin <- approxfun(x, y, yleft = 5)
f_con <- approxfun(x, y, method = "constant", yleft = 5, yright = -5)

f_lin(c(-Inf, Inf))
f_con(c(-Inf, Inf))

f_con <- approxfun(x, y, method = "constant", f = .5)

plot(x, y, lwd = 10)
points(x_new, f_con(x_new), pch = 16)

x <- runif(10)
f_ecdf <- ecdf(x)
class(f_ecdf)

plot(x, f_ecdf(x), lwd = 10, ylim = 0:1)

x_new <- runif(1000)

plot(x, f_ecdf(x), lwd = 10, ylim = 0:1)
points(x_new, f_ecdf(x_new), ylim = 0:1)

# 3.
pick <- function(i) {
  force(i)
  
  function(x) x[[i]]
}

x <- 1:3
identical(x[[1]], pick(1)(x))

identical(
  lapply(mtcars, function(x) x[[5]]),
  lapply(mtcars, pick(5))
)

# 4.
moment <- function(i) {
  force(i)
  
  function(x) sum((x - mean(x)) ^ i) / length(x)
}

m1 <- moment(1)
m2 <- moment(2)

x <- runif(100)
all.equal(m1(x), 0)  # removed stopifnot() for clarity
all.equal(m2(x), var(x) * 99 / 100)

# 5.
i <- 0
new_counter2 <- function() {
  i <<- i + 1
  i
}

new_counter2()
i
new_counter2()
i

i <- 0
new_counter2()
i

# 6.
new_counter3 <- function() {
  i <- 0
  function() {
    i <- i + 1
    i
  }
}

new_counter_3 <- new_counter3()

new_counter_3()
new_counter_3()

# Graphical factories
# One of the goals of the scales package is to make it easy to customise the labels on ggplot2.

y <- c(12345, 123456, 1234567)
comma_format()(y)
number_format(scale = 1e-3, suffix = " K")(y) 
# the primary interface is a function factory.

df <- data.frame(x = 1, y = y)
core <- ggplot(df, aes(x, y)) + 
  geom_point() + 
  scale_x_continuous(breaks = 1, labels = NULL) +
  labs(x = NULL, y = NULL)

core
core + scale_y_continuous(
  labels = comma_format()
)
core + scale_y_continuous(
  labels = number_format(scale = 1e-3, suffix = " K")
)
core + scale_y_continuous(
  labels = scientific_format()
)

# Histogram bins
sd <- c(1, 5, 15)
n <- 100

df <- data.frame(x = rnorm(3 * n, sd = sd), sd = rep(sd, n))
# Here each facet has the same number of observations, but the variability is very different.

ggplot(df, aes(x)) + 
  geom_histogram(binwidth = 2) + 
  facet_wrap(~ sd, scales = "free_x") + 
  labs(x = NULL)

binwidth_bins <- function(n) {
  force(n)
  
  function(x) {
    (max(x) - min(x)) / n
  }
}

ggplot(df, aes(x)) + 
  geom_histogram(binwidth = binwidth_bins(20)) + 
  facet_wrap(~ sd, scales = "free_x") + 
  labs(x = NULL)

# We could use this same pattern to wrap around the base R functions that automatically find 
# the so-called optimal58 binwidth, nclass.Sturges(), nclass.scott(), and nclass.FD():

base_bins <- function(type) {
  fun <- switch(type,
                Sturges = nclass.Sturges,
                scott = nclass.scott,
                FD = nclass.FD,
                stop("Unknown type", call. = FALSE)
  )
  
  function(x) {
    (max(x) - min(x)) / fun(x)
  }
}

ggplot(df, aes(x)) + 
  geom_histogram(binwidth = base_bins("FD")) + 
  facet_wrap(~ sd, scales = "free_x") + 
  labs(x = NULL)

# save the plots
plot_dev <- function(ext, dpi = 96) {
  force(dpi)
  
  switch(ext,
         eps =  ,
         ps  =  function(path, ...) {
           grDevices::postscript(
             file = filename, ..., onefile = FALSE, 
             horizontal = FALSE, paper = "special"
           )
         },
         pdf = function(filename, ...) grDevices::pdf(file = filename, ...),
         svg = function(filename, ...) svglite::svglite(file = filename, ...),
         emf = ,
         wmf = function(...) grDevices::win.metafile(...),
         png = function(...) grDevices::png(..., res = dpi, units = "in"),
         jpg = ,
         jpeg = function(...) grDevices::jpeg(..., res = dpi, units = "in"),
         bmp = function(...) grDevices::bmp(..., res = dpi, units = "in"),
         tiff = function(...) grDevices::tiff(..., res = dpi, units = "in"),
         stop("Unknown graphics extension: ", ext, call. = FALSE)
  )
}

plot_dev("pdf")
Plot_dev("png")

# exercises
# 1.
# Both functions will help you in styling your output, e.g. in your plots and they do this by returning the desired formatting function to you.
# ggplot2::label_bquote() takes relatively straightforward plotmath expressions and uses them 
# for faceting labels in ggplot2. Because this function is used in ggplot2 it needs to return a function of class = "labeller".
# scales::number_format() initially force()s the computation of all parameters. It’s essentially a parametrised wrapper around scales::number() and 
# will help you format numbers appropriately. It will return a simple function.

# Statistical factories
# The Box-Cox transformation.
# Bootstrap resampling.
# Maximum likelihood estimation.

# to transform data towards normality.
boxcox1 <- function(x, lambda) {
  stopifnot(length(lambda) == 1)
  
  if (lambda == 0) {
    log(x)
  } else {
    (x ^ lambda - 1) / lambda
  }
}

boxcox2 <- function(lambda) {
  if (lambda == 0) {
    function(x) log(x)
  } else {
    function(x) (x ^ lambda - 1) / lambda
  }
}

stat_boxcox <- function(lambda) {
  stat_function(aes(colour = lambda), fun = boxcox2(lambda), size = 1)
}

ggplot(data.frame(x = c(0, 5)), aes(x)) + 
  lapply(c(0.5, 1, 1.5), stat_boxcox) + 
  scale_colour_viridis_c(limits = c(0, 1.5))

# visually, log() does seem to make sense as the transformation
# for lambda = 0; as values get smaller and smaller, the function
# gets close and closer to a log transformation
ggplot(data.frame(x = c(0.01, 1)), aes(x)) + 
  lapply(c(0.5, 0.25, 0.1, 0), stat_boxcox) + 
  scale_colour_viridis_c(limits = c(0, 1.5))

# In general, this allows you to use a Box-Cox transformation 
# with any function that accepts a unary transformation function

# a bootstrap generator, a function that yields a fresh bootstrap every time it is called
boot_permute <- function(df, var) {
  n <- nrow(df)
  force(var)
  
  function() {
    col <- df[[var]]
    col[sample(n, replace = TRUE)]
  }
}

boot_mtcars1 <- boot_permute(mtcars, "mpg")
head(boot_mtcars1())
head(boot_mtcars1())

boot_model <- function(df, formula) {
  mod <- lm(formula, data = df)
  fitted <- unname(fitted(mod))
  resid <- unname(resid(mod))
  rm(mod)
  
  function() {
    fitted + sample(resid)
  }
} 

boot_mtcars2 <- boot_model(mtcars, mpg ~ wt)
head(boot_mtcars2())
head(boot_mtcars2())

# The advantage of a function factory is more clear with a parametric bootstrap where we have to first fit a model.

# Maximum likelihood estimation
lprob_poisson <- function(lambda, x) {
  n <- length(x)
  (log(lambda) * sum(x)) - (n * lambda) - sum(lfactorial(x))
}

x1 <- c(41, 30, 31, 38, 29, 24, 30, 29, 31, 38)

lprob_poisson(10, x1)

lprob_poisson(20, x1)

lprob_poisson(30, x1)


ll_poisson1 <- function(x) {
  n <- length(x)
  
  function(lambda) {
    log(lambda) * sum(x) - n * lambda - sum(lfactorial(x))
  }
}

# we’re going to need to call this function many times to find the best lambda.
ll_poisson2 <- function(x) {
  n <- length(x)
  sum_x <- sum(x)
  c <- sum(lfactorial(x))
  
  function(lambda) {
    log(lambda) * sum_x - n * lambda - c
  }
}

ll1 <- ll_poisson2(x1)

ll1(10)
ll1(20)
ll1(30)
 
optimise(ll1, c(0, 100), maximum = TRUE)
optimise(lprob_poisson, c(0, 100), x = x1, maximum = TRUE) # use the log-probability function directly

# The advantage of using a function factory here is fairly small, but there are two niceties:
# We can precompute some values in the factory, saving computation time in each iteration.
# The two-level design better reflects the mathematical structure of the underlying problem.

# exercise
# 1.
# boot_model() ultimately returns a function, and whenever you return a function 
# you need to make sure all the inputs are explicitly evaluated.
boot_model <- function(df, formula) {
  mod <- lm(formula, data = df)
  fitted <- unname(fitted(mod))
  resid <- unname(resid(mod))
  rm(mod)
  
  function() {
    fitted + sample(resid)
  }
} 

# 2.  This allows us to apply and test different transformations for different inputs and give them a descriptive name.
boxcox3 <- function(x) {
  function(lambda) {
    if (lambda == 0) {
      log(x)
    } else {
      (x ^ lambda - 1) / lambda
    }
  }  
}

boxcox_airpassengers <- boxcox3(AirPassengers)

plot(boxcox_airpassengers(0))
plot(boxcox_airpassengers(1))
plot(boxcox_airpassengers(2))
plot(boxcox_airpassengers(3))

# 3.
boot_permute <- function(df, var) {
  n <- nrow(df)
  force(var)
  
  function() {
    col <- df[[var]]
    col[sample(n, replace = TRUE)]
  }
}
# We don’t need to worry that it stores a copy of the data, 
# because it actually doesn’t store one; it’s just a name that points to the same underlying object in memory.

# 4.
ll_poisson1 <- function(x) {
  n <- length(x)
  
  function(lambda) {
    log(lambda) * sum(x) - n * lambda - sum(lfactorial(x))
  }
}

ll_poisson2 <- function(x) {
  n <- length(x)
  sum_x <- sum(x)
  c <- sum(lfactorial(x))
  
  function(lambda) {
    log(lambda) * sum_x - n * lambda - c
  }
}

x1 <- c(41, 30, 31, 38, 29, 24, 30, 29, 31, 38)

bench::mark(
  llp1 = optimise(ll_poisson1(x1), c(0, 100), maximum = TRUE),
  llp2 = optimise(ll_poisson2(x1), c(0, 100), maximum = TRUE)
) # to see the time consumption

bench_poisson <- function(x_length) {
  x <- rpois(x_length, 100L)
  
  bench::mark(
    llp1 = optimise(ll_poisson1(x), c(0, 100), maximum = TRUE),
    llp2 = optimise(ll_poisson2(x), c(0, 100), maximum = TRUE),
    time_unit = "ms"
  )
}

performances <- map_dfr(10^(1:5), bench_poisson)

df_perf <- tibble(
  x_length = rep(10^(1:5), each = 2),
  method   = rep(attr(performances$expression, "description"), 5),
  median   = performances$median
)

ggplot(df_perf, aes(x_length, median, col = method)) +
  geom_point(size = 2) +
  geom_line(linetype = 2) +
  scale_x_log10() +
  labs(
    x = "Length of x",
    y = "Execution Time (ms)",
    color = "Method"
  ) +
  theme(legend.position = "top")

# Function factories + functionals
# combine functionals and function factories to turn data into many functions.
names <- list(
  square = 2, 
  cube = 3, 
  root = 1/2, 
  cuberoot = 1/3, 
  reciprocal = -1
)
funs <- purrr::map(names, power1)

funs$root(64)
funs$root
with(funs, root(100)) # For a very temporary effect

attach(funs)
root(100)
detach(funs)

rlang::env_bind(globalenv(), !!!funs)
root(100)

rlang::env_unbind(globalenv(), names(funs))

funs <- list(
  mean = function(x) mean(x, na.rm = TRUE),
  sum = function(x) sum(x, na.rm = TRUE)
)

attach(funs)

mean <- function(x) stop("Hi!")
detach(funs)

env_bind(globalenv(), !!!funs)
mean <- function(x) stop("Hi!") 
env_unbind(globalenv(), names(funs))

# exercise
# 1.
f <- mean
z <- 1
x <- list(f = mean, z = 1)

identical(with(x, f(z)), x$f(x$z))
identical(with(x, f(z)), f(x$z))
identical(with(x, f(z)), x$f(z))
identical(with(x, f(z)), f(z))

# 2.
funs <- list(
  mean = function(x) mean(x, na.rm = TRUE),
  sum = function(x) sum(x, na.rm = TRUE)
)
# One annoying downside of using attach() is the possibility to attach the same object multiple times, 
# making it necessary to call detach() equally often.
attach(funs)
mean <- function(x) stop("Hi!")
detach(funs)

env_bind(globalenv(), !!!funs)
mean <- function(x) stop("Hi!") 
env_unbind(globalenv(), names(funs))

env_bind(globalenv(), !!!funs)
head(search())