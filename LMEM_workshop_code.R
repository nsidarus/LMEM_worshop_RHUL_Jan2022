# Nura Sidarus, 26/01/2022
# Drawing on workshop with Naomi Havron, https://github.com/rladies-paris/2018-07-12_Mixed_effect_regression_models


# load required packages - if you don't have them, run e.g. install.package("arm")
library(tidyverse)   # data wrangling and visualisation
library(lme4)        # model specification / estimation
library(lmerTest)    # provides p-values in the output
library(arm)         # for generating model predictions
library(lattice)


# now read in the file to an object called "d"
d <- read.csv("lexicalpriming.csv", header=T, na.strings = "NULL", comment.char="") 

## what does the data look like?
head(d)
summary(d)



# Plot data  ------ 

# At an effect on average (complete pooling)
p1 <- ggplot(d, aes(x = wordness, y = RT)) +
  geom_point(shape = 1) +
  geom_smooth(method = "lm", fill = "dodgerblue", level = .95)
p1

# vs per subject
p2 <- p1 + facet_wrap(~subject, nrow = 6)
p2

# LMEMs will give us something in between


# For now, let's recode some variables to turn them to factors
## this way can many at once:
d[, c("wordness","prime","subject", "item")] <- lapply(d[,c("wordness","prime","subject", "item")] , factor)




# Random intercepts  ------ 

LMEM1 <- lmer(RT ~ wordness + # fixed effects
               (1 | subject) + (1 | item), # random effects (here, crossed design)
             data = d, control=lmerControl(optimizer="bobyqa"))
summary(LMEM1)

# have a look at the random effects
ranef(LMEM1)

#  visualise 
qqmath(ranef(LMEM1))


# With lmerTest we can now get p-values immediately, but could also estimate the relevance of the predictor "wordness",
# by comparing that model to a simpler one, without it, only an intercept (represented by the 1, normally implied)
LMEM0 <- lmer(RT ~ 1 + (1 | subject) + (1 | item), data = d, control=lmerControl(optimizer="bobyqa"))

# Then compare the models - through aka likelihood ratio test
anova(LMEM1, LMEM0)

# NB: the warning message that the models were refit with Maximum Likelihood (ML) instead of the defautl Restricted ML (REML)
# REML gives unbiased estimates of the variance parameters, so should be used when interpreting the parameters
# However, for model comparison, ML should be used when the models differ only in fixed effects (or REML if differing only in random effects)
# cf. https://stats.stackexchange.com/questions/91652/when-no-model-comparison-should-i-use-reml-vs-ml ,
# or https://stats.stackexchange.com/questions/99895/why-does-one-have-to-use-reml-instead-of-ml-for-choosing-among-nested-var-cova






# Random Slopes  ------ 

LMEM2 <- lmer(RT ~ wordness + (1 + wordness | subject) + (1 | item), data = d, control=lmerControl(optimizer="bobyqa"))
summary(LMEM2)

# visualise 
qqmath(ranef(LMEM2))


# Could use model comparison to assess whether to include the varying slopes
anova(LMEM2, LMEM1, refit=FALSE) # refit=F to stick to REML estimates



# Could try models with more random effects

LMEM3 <- lmer(RT ~ wordness + (1 + wordness | subject) + (1 + prime + luminance | item), data = d, control=lmerControl(optimizer="bobyqa"))
summary(LMEM3)
# but model does not converge! e.g.
# boundary (singular) fit: see ?isSingular
# Warning message:
#   Model failed to converge with 2 negative eigenvalues: -2.6e-02 -1.8e+02 


# Worth trying to remove the correlations between random effects linked to the item  (use || instead of |) 
LMEM4 <- lmer(RT ~ wordness + (1 + wordness | subject) + (1 + prime + luminance || item), data = d, control=lmerControl(optimizer="bobyqa"))
summary(LMEM4)


# Could drop one of the random effects
LMEM5 <- lmer(RT ~ wordness + (1 + wordness | subject) + (1 + prime | item), data = d, control=lmerControl(optimizer="bobyqa"))
summary (LMEM5)


# Alternatively, consider whether the coding of the predictors may be unhelpful




# Coding categorical data with 2 levels -----
## Important for the interpretation of the 

# 1. dummy - One level is the baseline
levels(d$wordness)
levels(d$wordness) <- c("nonword", "word")
contrasts(d$wordness) <- c(0, 1)

LMEM1 <- lmer(RT ~ wordness + (1 + wordness | subject) + (1 | item), data = d, control=lmerControl(optimizer="bobyqa"))
summary(LMEM1)
# Intercept reflects the average RT for non-words, and the slope is the difference to the "word" condition


# 2. sum / orthogonal - relative to the grand mean
d$wordness_sum <- d$wordness
contrasts(d$wordness_sum) <- c(-1, 1)

LMEM1.2 <- lmer(RT ~ wordness_sum + (1 + wordness_sum | subject) + (1 | item), data = d, control=lmerControl(optimizer="bobyqa"))
summary(LMEM1.2)
# Note the change in the coefficients: the intercept now reflects the grand mean RT, across wordness conditions
# the slope reflects what needs to be added/removed from that average for either condition (given the +/- of the conditions)


# Using "contrast" attributes could get confusing, so can also simply code the predictors with numbers
# Also, if coded as +/- .5, then the coefficient is the difference between the conditions 
d$word_c <- as.character(d$wordness)
d$word_c <- ifelse(d$word_c == "word", .5, -.5)

LMEM1.3 <- lmer(RT ~ word_c + (1 + word_c | subject) + (1 | item), data = d, control=lmerControl(optimizer="bobyqa"))
summary(LMEM1.3)



###### -> back to presentation!






# Interactions  ------

## Create orthogonal contrasts for the predictors, or centred predictors
# # NB. Especially useful when you have more than 1 predictors, as you could later just set them to 0, i.e. their average
unique(d$luminance)
d$lum_c <- scale(d$luminance, center = TRUE, scale = FALSE)


## Model with interaction term & using the sum coded variable
LMEM6 <- lmer(RT ~ lum_c * wordness_sum + (1 + wordness_sum | subject) + (1 | item), data = d, control=lmerControl(optimizer="bobyqa"))
summary(LMEM6)


## Could check relevance of interaction with  model comparison
LMEM7 <- lmer(RT ~ lum_c + wordness_sum + (1 + wordness_sum | subject) + (1 | item), data = d, control=lmerControl(optimizer="bobyqa"))
summary(LMEM7)

anova(LMEM6, LMEM7)



### Using simple slopes to understand the direction of the effect -----
# Not very familiar, but works if the categorical variable is contrast coded
# tells you the effect of luminance at each level of wordness
LMEM6.2 <- lmer(RT ~ lum_c * wordness_sum - lum_c + (1 + wordness_sum | subject) + (1 | item), data = d, control=lmerControl(optimizer="bobyqa"))
summary(LMEM6.2)




### Plot the data to understand the interaction  ------

ggplot(data = subset(d), aes(y=RT, x=luminance, colour = wordness))+
  stat_smooth(aes(colour=wordness), method = "lm", formula = y ~ x, size=1.4)+
  theme_bw()+
  theme(text = element_text(size = 20))

# Useful to see the effect here, but
# NB: The lm model used by ggplot is simple regression, not the model we used! - more below
        
 



### Generate model predictions at the average level (i.e. fixed effects)  ---------------------------------------

# More info and alternative strategies can be found in this helpful tutorial by Matti Vuorre: https://mvuorre.github.io/posts/2016-03-06-multilevel-predictions/
# Could use "predict" to get point estimates
# Here, we'll get the point estimates with their confidence intervals, using the arm package: Gelman, A., & Su, Y.-S. (2015). arm: Data Analysis Using Regression and Multilevel/Hierarchical Models.


# Create new model with simple orthogonal predictors
LMEM8 <- lmer(RT ~ lum_c * word_c + (1 + word_c | subject) + (1 | item), data = d, control=lmerControl(optimizer="bobyqa"))
summary(LMEM8)



# Create a matrix with our predictors and the levels we want to compare, to later combine with the model predictions
newdat <- expand.grid(
  lum_c = c(-1, 0, 1),
  word_c = c(-.5, .5)  )


# 1 - Simulating plausible parameter values
sims <- arm::sim(LMEM8, n.sims = 1000)


# 2 - Saving the simulated samples (a faux posterior distribution) in a data frame - for the fixed effects (fixef)
sims.fix <- fixef(sims)

# 3 - Creating a predictor matrix
Xmat <- model.matrix( ~ lum_c*word_c, data = newdat)

# 4 - Creating a matrix for the fitted values
fitmat <- matrix(ncol=nrow(sims.fix), nrow=nrow(newdat))

# 5 - Calculating fitted values for each combination of the predictor values, for each plausible combination of the parameter values
for (i in 1:nrow(sims.fix)) { fitmat[,i] <- Xmat %*% as.matrix(sims.fix)[i,]  }

# 6 - Calculating the desired quantiles of the fitted values, i.e. 95% CI
newdat$lower <- apply(fitmat, 1, quantile, prob=0.05)
newdat$median <- apply(fitmat, 1, quantile, prob=0.5)
newdat$upper <- apply(fitmat, 1, quantile, prob=0.95)



### Plotting the interaction  

# for plotting purposes
newdat <- newdat %>%
  mutate(word_c = factor(word_c, levels = c(-.5, .5), labels = c("nonword", "word")))
  

# Plot model predictions overlayed on average data
d %>%
  mutate(word_c = factor(word_c, levels = c(-.5, .5), labels = c("nonword", "word"))) %>%   
  group_by(subject, lum_c, word_c) %>%
  summarise(RT = mean(RT)) %>%
  ggplot(aes(x = lum_c, y = RT, col = word_c, fill = word_c)) +
  
  # Participants' data - dots (with SE across Ss, thanks to previously averaging within Ss)
  stat_summary(fun.data="mean_se", geom="pointrange", size=.8) +
  
  # Model Predictions - lines and shading
  geom_ribbon(data=newdat, aes(ymax=upper, ymin=lower, y=median), alpha=.2, col=NA) +
  geom_line(data=newdat, aes(y=median))









### General Linear Hypothesis testing  ---------------------------------------

# Do t-tests on point estimates (or the point estimates of a contrast)
# Snijders, T. A. B., & Bosker, R. J. (1999). Multilevel Analysis: An Introduction to Basic and Advanced Multilevel Modeling. London: SAGE.

## dfs should be based on sample size -1, or some other reasonable number
DF = length(unique(d$subject)) - 1 

## Remember the fixed effects in the model, as the contrasts are defined based on these, with 1 value (column) per effect
fixef(LMEM8)
# i.e. Intercept, luminance, wordness, luminance:wordness - so, always need 4 columns


####### Any effect of Luminance for Wordness == 1, i.e. 0.5 ?

# Define the two conditions we want to contrast, or could input the difference contrast directly
# i.e. holding wordness = .5, compare luminance at -1 vs. 1
word1.lumLo <- matrix( c(0, -1, .5, -1*.5), nrow=1, byrow=T)
word1.lumHi <- matrix( c(0,  1, .5,  1*.5), nrow=1, byrow=T)
word1 <- word1.lumLo - word1.lumHi
word1

# Get beta value for the difference
b.word1 <- word1 %*% fixef(LMEM8)
b.word1

# Get variance
v.word1 <- word1 %*% vcov(LMEM8) %*% t(word1)
v.word1

# Calculate standard error
se.word1 <- sqrt(v.word1)
se.word1

# Calculate t-test
t.word1 <- as.numeric(b.word1 / se.word1)
t.word1

# Compute p-value from the t-test and p distribution
p.word1 <- 2 * pt(t.word1, df = DF, lower.tail = T)  # lower.tail needs to be False if t > 0
p.word1 # Yes!



####### Any effect of Luminance for Wordness == 0, i.e. -0.5 ?

word0 <- matrix( c(0, -1, -.5, -1*-.5), nrow=1, byrow=T) - 
  matrix( c(0,  1, -.5,  1*-.5), nrow=1, byrow=T)

b.word0 <- word0 %*% fixef(LMEM8)                # beta
v.word0 <- word0 %*% vcov(LMEM8) %*% t(word0)    # variance
t.word0 <- as.numeric(b.word0 / sqrt(v.word0) ) # t-test
t.word0
p.word0 <- 2 * pt(t.word0, df = DF, lower.tail = T)  # lower.tail needs to be False if t > 0
p.word0 # No




####### Or, Is there a significant effect of Wordness at Low Luminance ?

# i.e. holding luminance = -1, compare wordness at -.5 vs. .5
lumLo <- matrix( c(0, -1, -.5, -1*-.5), nrow=1, byrow=T) -
  matrix( c(0, -1,  .5, -1* .5), nrow=1, byrow=T)

b.lumLo <- lumLo %*% fixef(LMEM8)                # beta
v.lumLo <- lumLo %*% vcov(LMEM8) %*% t(lumLo)    # variance
t.lumLo <- as.numeric(b.lumLo / sqrt(v.lumLo))  # t-test
t.lumLo
p.lumLo <- 2 * pt(t.lumLo, df = DF, lower.tail = F)  # lower.tail needs to be False if t > 0
p.lumLo # Yes!



####### Any effect of Wordness at High Luminance ?

lumHi <- matrix( c(0, 1, -.5, 1*-.5), nrow=1, byrow=T) -
  matrix( c(0, 1,  .5, 1* .5), nrow=1, byrow=T)

b.lumHi <- lumHi %*% fixef(LMEM8)                # beta
v.lumHi <- lumHi %*% vcov(LMEM8) %*% t(lumHi)    # variance
t.lumHi <- as.numeric(b.lumHi / sqrt(v.lumHi))  # t-test
t.lumHi
p.lumHi <- 2 * pt(t.lumHi, df = DF, lower.tail = F)  # lower.tail needs to be False if t > 0
p.lumHi # Yes!





#  Plot Coefficients  ---------------------------------------
# For models with multiple predictors, can be helpful to visualise the effects, rather than look at a table

# There various tools out there e.g.
# sjPlot has various tools that can be useful for visualising 
# install.packages("sjPlot")
# library(sjPlot)

sjPlot::plot_model(MEM8)



# I once created a function, adapted from others, to allow customisation of things as I wished
# No guarantees, just shared here an example of using functions

mycoefplot <- function(model, cint, cols="black", intercept=0,
                       varPlotNames=NULL, figTitle=NULL,
                       save=TRUE, dev="png", figName=NULL, outPath=NULL,
                       width=6, height=3.5) {
  
  xAxisLab = ''
  yAxisLab = 'Parameter Estimates'
  
  if(!intercept){
    varNames = names(fixef(model))[-1] # without the intercept
  } else {
    varNames = names(fixef(model))
  }
  
  if(is.null(varPlotNames)){
    varPlotNames = rev(varNames)
  }
  
  if(is.null(figTitle)){
    figTitle = ""
    if(is.null(figName)){
      figName  = "mycoefplot"
    }
  }
  
  d = data.frame(varPlotNames = factor(varPlotNames, varPlotNames),
                 y = rev(fixef(model)[varNames]),
                 ylo = rev(as.numeric(cint[varNames, 1])),
                 yhi = rev(as.numeric(cint[varNames, 2])) )
  
  p = ggplot(d, aes(x=varPlotNames, y=y, ymin=ylo, ymax=yhi)) + 
    geom_pointrange(size=.75, colour=cols) +
    geom_hline(aes(yintercept=0), lty=2) +
    coord_flip() +
    labs(x=xAxisLab, y=yAxisLab,
         title=figTitle) +
    theme( panel.grid = element_blank(),
           axis.text.y  = element_text(size=12, face="bold"),
           plot.margin=unit(c(.1, .5, .1, .1),"cm") )
  p
  
  if(save){
    
    if(is.null(figName)){
      
      if(!is.null(figTitle)){
        figName = figTitle
      } else {
        figName = "mycoefplot"
      }
    }
    
    ggsave(file=paste(figName, dev, sep="."), plot=p, path=outPath, width=6, height=3.5, dpi = 300)
  }
  
  return(p)
}

### using this requires first obtaining CIs
varNames <- names(fixef(MEM8)) # restricted to the fixed effects
MEM8.ci <- confint(MEM8, varNames, method="Wald", # or method="boot" for bootstrapped CIs
                    parallel="multicore", ncpus=4) # useful for "boot"

# to have a look (by default, the intercept is ignored)
mycoefplot(MEM8, MEM8.ci, save=F)

# could specify different colours per effect, e.g. to emphasise sig effects
mycoefplot(MEM8, MEM8.ci, cols=c("black","blue","red"), save=F)

# for prettier names and to save automatically           
varPlotNames <- rev(c("Luminance", "Wordness", "Luminance x Wordness"))
mycoefplot(MEM8, MEM8.ci, varPlotNames=varPlotNames,
                        dev="png", figName= "MEM8_coefplot",
                                  width=6, height=4)

