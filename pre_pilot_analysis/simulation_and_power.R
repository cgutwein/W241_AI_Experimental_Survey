## W241 - Artificial Intelligence Final Project
#  
#  Investigation into Statistical Power for prior to survey launch

## Making some assumptions in order to carry out simulation
#
# 1.) Even distribution of answer to questions with variance of 1
# 2.) Estimating effect size between 0.1 and 0.3
# 3.) Targeting 1,000 subjects: 400 control, 600 treatment for each question 
# 4.) outcome variable is a score on a diverging scale from 1 to 5, with 
#     3 being neutral.


#Defining variables
n_total <- 1000
n_control <- 0.5*n_total
n_treatment <- 0.5*n_total
sigma2 <- 1.2

#####
#Because blocking, we need to make more assumptions.
# Block for (1,2) is 30% of total
# avg control for (1,2) is 2.2
# avg treatment is t
#####

##

#Default control values
po.control <- round(rnorm(n_control, 3, sigma2))
po.control[po.control < 1] = 1
#sum(po.control > 5)
po.control[po.control > 5] = 5
po.control <- abs(3-po.control)
hist(po.control)
mean(po.control)
var(po.control)

#Default treatment values
t_1 <- -0.2
po.treatment <- round(rnorm(n_treatment, mean(po.control) + t_1, var(po.control)))
sum(po.treatment > 2)
sum(po.treatment < 0)
po.treatment[po.treatment < 0] = 0
po.treatment[po.treatment > 2] = 2
hist(po.treatment)
mean(po.treatment)
var(po.treatment)

#Randomize control and treatment vector
randomize <- function() sample(c(rep(0,250), rep(1, 250)))
est.ate <- function(outcome, treat) mean(outcome[treat==1]) - mean(outcome[treat==0]) 

treatment <- randomize()
outcomes <- po.treatment * treatment + po.control*(1-treatment) 
ate <- est.ate(outcomes, treatment)

est.ate(outcomes, randomize())
est.ate(outcomes, randomize())
est.ate(outcomes, randomize())

distribution_01 <- replicate(5000, est.ate(outcomes, randomize())) 
plot(density(distribution_01))
abline(v=ate)
mean(ate <= distribution_01)

## Getting Statistical Power
#
simulate.study <- function(t_1){ 
  po.control <- round(rnorm(500, 3, 1.2))
  po.control[po.control < 1] = 1
  po.control[po.control > 5] = 5
  po.control <- abs(3-po.control) 
  po.treatment <- round(rnorm(500, mean(po.control) + t_1, var(po.control)))
  po.treatment[po.treatment < 0] = 0
  po.treatment[po.treatment > 2] = 2 
  treatment <- randomize() 
  outcomes <-  po.treatment * treatment + po.control * (1-treatment) 
  ate <- est.ate(outcomes, treatment) 
  distribution.under.sharp.null <- replicate(1000, est.ate(outcomes, randomize())) 
  return(mean(ate > distribution.under.sharp.null)) 
} 

pvals_0 <- replicate(1000, simulate.study(0))
plot(density(pvals_0)) 
mean(pvals_0 < 0.05)
pvals_1 <- replicate(1000, simulate.study(-0.1))
plot(density(pvals_1)) 
mean(pvals_1 < 0.05)
pvals_2 <- replicate(1000, simulate.study(-0.2))
plot(density(pvals_2)) 
mean(pvals_2 < 0.05)