library(ggplot2)

# Two types of scoping: lexical and dynamic scoping

# Lexical Scoping: the values of free variables are searched for in the
# environment in which function was defined

# Dynamic Scoping: the value of free variables is looked up
# in the environment in which function was called


# LEXICAL
make.power <- function(n) {
      pow <- function(x) {
            x^n
      }
      pow
}
# ls(environment(cube))
# get("n", environment(cube))


# Test: 
cube <- make.power(3)
square <- make.power(2)
cube(3)
square(3)




# DYNAMIC: 
y <- 10

f <- function(x) {
      y <- 2
      y^2 + g(x)
}
g <- function(x) {
      x*y
}

# lexical: value y in function g is from place of definition y=10
# dynamic: value y in function f is from calling environment y=2


# LEXICAL/ DYNAMIC: sometimes, calling is same as defining environment



# -------- Application: Optimization Example --------------------------

make.NegLogLik <- function(data, fixed=c(FALSE, FALSE)) {
      params <- fixed
      function(p) {
            params[!fixed] <- p
            mu <- params[1]
            sigma <- params[2]
            a <- -0.5*length(data)*log(2*pi*sigma^2)
            b <- -0.5*sum((data-mu)^2) / (sigma^2)
            -(a + b)
      }
}

# Example
set.seed(1); normals <- rnorm(100, mean=1, sd=2)
nLL <- make.NegLogLik(normals)
nLL # shows environment HEX where variables are located
ls(environment(nLL))

# estimating parameters
optim(c(mu = 0, sigma = 1), nLL)$par
# fixing sigma=2
nLL <- make.NegLogLik(normals, c(FALSE, 2))
optimize(nLL, c(-1, 3))$minimum
# fixing mu=1
nLL <- make.NegLogLik(normals, c(1, FALSE))
optimize(nLL, c(1e-6, 10))$minimum

# Plotting
nLL <- make.NegLogLik(normals, c(1, FALSE))
x <- seq(1.7, 1.9, len=100)
y <- sapply(x, nLL)
func <- exp(-(y - min(y)))
ggplot(data.frame(x, func), aes(x=x, y=func)) + geom_line()

nLL <-  make.NegLogLik(normals, c(FALSE, 2))
x <- seq(0.5, 1.5, len = 100)
y <- sapply(x, nLL)
func <- exp(-(y - min(y)))
ggplot(data.frame(x, func), aes(x=x, y=func)) + geom_line()
