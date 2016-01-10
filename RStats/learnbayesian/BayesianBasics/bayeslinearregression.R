set.seed(8675309)

# Create N x k matrix of covariates
N = 250
K = 3

covariates <- replicate(K, rnorm(n=N)); covariates
colnames(covariates) <- c("X1", "X2", "X3")

# create model matrix with intercept
X <- cbind(Intercept=1, covariates); X

# create normally distributed variable that is a function of covariates
# y = 5 + 0.2*X1 - 1.5*X2 + 0.9*X3 + rnorm(N, mean=0, sd=2)
coefs <- c(5, 0.2, -1.5, 0.9)
mu <- X %*% coefs; mu
sigma <- 2
y <- rnorm(N, mu, sigma); y

modlm <- lm(y ~., data=data.frame(X[, -1]))
modlm
summary.lm(modlm)


# Create data list object for stan input
dat <- list(N=N, K=ncol(X), y=y, X=X)

#... need to install rstan
