"
* Logistic regression: 
      * is multiple regression with categorical outcome variable and
      continuous + categorical predictor variables. 
      * binary log.reg: only two categorical outcomes. 
      * multinomial log.reg: more than two categorical outcomes. 
* Logistic reg. equation: 
      P(Y) = 1 / (1 + e^ (-(b0 + b1x1)))
* log-likelihood (equivalent of R^2):
      sum(1->N) [Yi * ln(P(Yi)) + (1 - Yi) * ln(1 - P(Yi))]
* deviance = -2LL
       = -2 * loglikelihood
* improvement of model statistic: X^2 (chi-squared)
      X^2 = -2LL(baseline) - (-2LL(new))
          = 2LL(new) - 2LL(baseline)
      df = k_new - k_baseline
"