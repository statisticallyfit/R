# Penn state stat 504 example: 
# https://onlinecourses.science.psu.edu/stat504/node/225



### FITTING LOGISTIC REGRESSION FOR 2X2 TABLE 


#                  |  student smokes  | student does not smoke
# -------------------------------------------------------------
# parents smoke    |    816           |      3203
# no parents smoke |    188           |      1168


# predictor has two levels: X = 1 (parents smoke), X = 0 (no parents smoke)
parentSmoke <- as.factor(c(1,0)); parentSmoke

# response vector with counts for both success and failure (students smoke)
response <- cbind(yes = c(816, 188), no = c(3203, 1168)); response 

# for logistic model 
smoke.logistic <- glm(response ~ parentSmoke, family = binomial(link = "logit"))
smoke.logistic
summary(smoke.logistic) # fisher scoring is variant of newton-raphson but they
# are equivalent in logistic regression. 
anova(smoke.logistic)

# Estimated β0 =−1.827 with standard error 0.078 is significant and it says 
# that log-odds of a child smoking versus not smoking if neither 
# parents is smoking (the baseline level) is -1.827 (statistically significant).


# Estimated β1 = 0.459 with standard error 0.088 is significant and it says 
# that log-odds-ratio of a child smoking versus not smoking if at least one 
# parent is smoking versus neither parents is smoking (the baseline level) 
# is 0.459 (statistically significant). 
# exp(0.459)=1.58 are the estimated odds-ratios





### FITTING LOGISTIC REGRESSION FOR 2X3 TABLE 
