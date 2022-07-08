# VIC supplmental materials
# Analysis of SCRs (to outcomes and during decision phase) of all participants (responders and non-responders)
# 1 participant excluded due to equipment malfuction (n=61)

# Hayley Roper Brooks
# Sokol-Hessner Lab
# University of Denver

# clear environment
rm(list=ls());

# load the datasets

# SCRs to outcomes
load("/Volumes/shlab/Projects/VIC/data analysis/SCR/vicBscr.Rdata"); # outcome SCRs
outcomeSCRb = vicBscr; # rename dataframe for clarity
load("/Volumes/shlab/Projects/VIC/data analysis/SCR/scrStatsAllsubs.Rdata"); # summary stats for outcome SCRs (61 rows, one for each participant)

# SCRs during decision phase
load("/Volumes/shlab/Projects/VIC/data analysis/SCR/decisionAnalysis/vicBscr.Rdata"); # SCRs during decision phase
decisionSCRb = vicBscr;# rename for clarify + default name is the same as the following dataset for outcome SCRs
load("/Volumes/shlab/Projects/VIC/data analysis/SCR/decisionAnalysis/scrStatsAllsubs.Rdata"); # summary stats for SCRs during decision phase (61 rows, one for each participant)


# load packages
library('lme4'); #load package for glmer
library('lmerTest'); #load package to get p value for lmer models


nSubAll = length(unique(outcomeSCRb$subjectIndex)); # number of subjects 
nSubAll == length(unique(decisionSCRb$subjectIndex)); # double check we have same number of participants in both datasets





########## Part 1: SCRS TO OUTCOMES  ##########

# how many gain, loss, safe trials are there?
WinInd = outcomeSCRb$outcome == outcomeSCRb$riskyGain; # 3161 trials (23% of trials)
LossInd = outcomeSCRb$outcome == outcomeSCRb$riskyLoss; # 3348 trials (24% of trials)
SafeInd = outcomeSCRb$outcome == outcomeSCRb$alternative; # 7335 trials (53% of trials)


#NOTE: like analysis in manuscript, this uses max norm (SCRs are normalized by each participant's max scr)

# mean outcome SCRs:
meanSCRwinsNorm = mean(outcomeSCRb$maxNorm[WinInd]); #.137 (sd = .22)
meanSCRlossNorm  = mean(outcomeSCRb$maxNorm[LossInd]); #.143 (sd = .22)
meanSCRsafeNorm  =mean(outcomeSCRb$maxNorm[SafeInd]);# .13 (sd=.22)
# all medians = 0


# Is there a differnence in SCRs to outcomes across trial type?
# Use each person's mean SCR response to wins, loss, and safe (scrStats)

#mean outcome SCRs
mean(scrStats$mnW); #.1392 - mean of the mean SCRs; range = .01 - .38
sd(scrStats$mnW); #.09
mean(scrStats$mnL); #.141 - mean of the mean SCRs; range = 0 -.33
sd(scrStats$mnL); #.0867
mean(scrStats$mnS); #.133 - mean of the mean SCRs; range = .01 - .34
sd(scrStats$mnS); #.0734

#t-test comparing mean outcome SCRs
t.test(scrStats$mnW, scrStats$mnL, paired = T); #ns; t = -0.21, df = 60, p-value = 0.8353
t.test(scrStats$mnW, scrStats$mnS, paired = T); #ns; t = 0.8, df = 60, p-value = 0.4277
t.test(scrStats$mnL, scrStats$mnS, paired = T); #ns; t = 0.97 df = 60, p-value = 0.3348

#### SCRs to outcomes and three timescales of temporal context (outcomes, positive shift, earnings) ####

# MODEL 5a FULL: SCRs & outcomes
scrM5aEveryone = lmer(maxNorm ~ 1 + outcomeSC + (1|subjectIndex), data=outcomeSCRb, REML = F); 
# Fixed effects:
#                 Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)  1.412e-01  9.497e-03  6.710e+01  14.862   <2e-16 ***
#   outcomeSC   -1.724e-02  7.523e-03  1.380e+04  -2.292   0.0219 * 
#   AIC = -4326.5

# MODEL 5b FULL= SCRs and positive shift
scrM5bEveryone = lmer(maxNorm ~ 1 + shiftDiffscPOS + (1|subjectIndex), data = outcomeSCRb, REML=F); 
# Fixed effects:
#                     Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)     1.368e-01  9.264e-03  6.114e+01  14.767   <2e-16 ***
#   shiftDiffscPOS -8.264e-02  8.391e-02  1.378e+04  -0.985    0.325 
#   AIC = -4322.2

# MODEL 5c FULL (+ variations) = SCRs and cumulative earnings
scrM5cEveryone <- lmer(maxNorm ~ 1 + cumEarningsSC01 + linExpectation + (1|subjectIndex), data=outcomeSCRb, REML = F);
# Fixed effects:
#                   Estimate Std. Error        df t value Pr(>|t|)    
#   (Intercept)     9.162e-02  9.723e-03 7.466e+01   9.423 2.44e-14 ***
#   cumEarningsSC01 4.774e-02  3.679e-02 7.963e+03   1.298   0.1944    
#   linExpectation  5.263e-02  2.843e-02 8.291e+03   1.851   0.0642 .  
#   AIC = -4537.6

# Earnings only (removing linExpectation)
scrM5cEveryone_earnOnly <- lmer(maxNorm ~ 1 + cumEarningsSC01 + (1|subjectIndex), data=outcomeSCRb, REML = F);
# Fixed effects:
#                      Estimate Std. Error        df t value Pr(>|t|)    
#   (Intercept)     9.243e-02  9.720e-03 7.430e+01   9.509 1.76e-14 ***
#   cumEarningsSC01 1.143e-01  7.766e-03 1.383e+04  14.719  < 2e-16 ***
#   AIC = -4536.2

# MODEL 5d FULL= SCRs and interaction bewteen earnings and outcome
scrM5dEveryone <- lmer(maxNorm ~ 1 + outcomeSC*cumEarningsSC01  + (1|subjectIndex), data=outcomeSCRb, REML = F); 
# Fixed effects:
#                                 Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                9.671e-02  1.055e-02  1.027e+02   9.165 5.43e-15 ***
#   outcomeSC                 -1.477e-02  1.414e-02  1.380e+04  -1.044    0.296    
#   cumEarningsSC01            1.123e-01  1.183e-02  1.382e+04   9.493  < 2e-16 ***
#   outcomeSC:cumEarningsSC01  5.210e-03  3.110e-02  1.380e+04   0.168    0.867   
#   AIC = -4535.1 
#   also tried model adding interaction with linExpectation and interaction not significant

#### SCRs to outcomes and risk-taking ####

# MODEL 6a FULL: SCR along with pastoutcome, earnings*pastoutcome, and positive shift to predict risk-taking
scrM6aEveryone = glmer(choice ~ 0 + maxNormPast + pastOC1sc*cumEarningsSC01 + shiftDiffscPOS+ (1|subjectIndex), data = outcomeSCRb, family="binomial", offset=pred);
# Fixed effects:
#                               Estimate Std. Error z value Pr(>|z|)    
#   maxNormPast               -0.06312    0.09634  -0.655    0.512    
#   pastOC1sc                 -0.70092    0.11421  -6.137 8.40e-10 ***
#   cumEarningsSC01            0.06853    0.07425   0.923    0.356    
#   shiftDiffscPOS             5.16409    1.01466   5.089 3.59e-07 ***
#   pastOC1sc:cumEarningsSC01  1.26019    0.29010   4.344 1.40e-05 ***
#   AIC =  13796.5

# MODEL 6b FULL: replace cumulative earnings with SCR - does SCR behave like cumulative earnings to predict risk-taking?
scrM6bEveryone = glmer(choice ~ 0 + maxNormPast*pastOC1sc + shiftDiffscPOS+ (1|subjectIndex), data = outcomeSCRb, family="binomial", offset=pred);
# Fixed effects:
#                           Estimate Std. Error z value Pr(>|z|)    
#  maxNormPast            0.04392    0.12456   0.353    0.724    
#  pastOC1sc             -0.16574    0.07007  -2.365    0.018 *  
#  shiftDiffscPOS         4.72721    1.00270   4.714 2.42e-06 ***
#  maxNormPast:pastOC1sc -0.14982    0.38302  -0.391    0.696    
#  AIC = 13826.9






########## Part 2: SCRS DURING DECISION PHASE  ##########

# how many gamble and safe trials are there?
gamInd = decisionSCRb$choice == 1; # 6505 trials (47% of trials)
safeInd = decisionSCRb$choice == 0; # 7335 trials (53% of trials)

# mean SCR during decision-phase for each decision type across all participants
meanSCRgamNorm = mean(decisionSCRb$maxNorm[gamInd]); #.088
meanSCRsafeNorm  = mean(decisionSCRb$maxNorm[safeInd]); #.09
# all medians = 0

#mean outcome SCRs (using each person's mean SCR)
mean(scrStatsDecision$mnG); #.09 - mean of the mean SCRs; range = 0 - .36
sd(scrStatsDecision$mnG); #.07
mean(scrStatsDecision$mnS); #.09- mean of the mean SCRs; range = 0 -.33
sd(scrStatsDecision$mnS); #.07

# Is there a difference in mean SCRs following gamble and safe decisions? (scrStatsDecision)
wilcox.test(scrStatsDecision$mnG,scrStatsDecision$mnS, paired=T); #ns, V = 932, p = .9
# 10341 trials have SCRs =0, 75% of trials


#### SCRs during decision phase and three timescales of temporal context ####

#MODEL 7 FULL: Do SCRs during decision phase vary as a function of choice?
model7Everyone = lmer(maxNorm ~ 1 + choice1 + (1|subjectIndex), data=decisionSCRb, REML=F);
# Fixed effects:
#               Estimate     Std. Error   df    t value Pr(>|t|)    
#   (Intercept) 9.112e-02  8.636e-03 6.064e+01  10.551 2.37e-15 ***
#   choice1     3.879e-04  1.630e-03 1.384e+04   0.238    0.812   
#   AIC = -7953.9


#MODEL 8a FULL: SCRs during decision phase + 3 timescales of temporal context (outcomes, positive shift, earnings)
model8aEveryone = lmer(maxNorm ~ 1 + pastOC1sc + shiftDiffscPOS + cumEarningsSC01 + linExpectation+ (1|subjectIndex), data=decisionSCRb, REML = F);
# Fixed effects:
#                     Estimate Std. Error     df     t value Pr(>|t|)    
#   (Intercept)      7.428e-02  9.264e-03  8.050e+01   8.018 7.18e-12 ***
#   pastOC1sc       -6.899e-03  6.602e-03  1.373e+04  -1.045   0.2961    
#   shiftDiffscPOS   1.692e-01  7.398e-02  1.373e+04   2.287   0.0222 *  
#   cumEarningsSC01  1.939e-02  3.270e-02  8.610e+03   0.593   0.5533    
#   linExpectation  1.914e-02  2.523e-02  8.934e+03   0.758   0.4482
#   AIC =  -8105.2


# MODEL 8b FULL: How far back does positive shift effect go?
model8bEveryone= lmer(maxNorm ~ 1 + shiftDiffscPOS + shiftDiffscPOS1 + (1|subjectIndex), data = decisionSCRb, REML=F); 
# Fixed effects:
#                     Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)      9.031e-02  8.620e-03  6.093e+01  10.477 2.95e-15 ***
#   shiftDiffscPOS   1.323e-01  7.335e-02  1.372e+04   1.804   0.0712 .  
#   shiftDiffscPOS1 -1.406e-01  7.335e-02  1.372e+04  -1.917   0.0552 .  
#   AIC = -8070.8 

# positive shift several trials back (plotted in figure 4b)
model8bEveryone_6trials= lmer(maxNorm ~ 1 + shiftDiffscPOS + shiftDiffscPOS1 + shiftDiffscPOS2 + shiftDiffscPOS3 + shiftDiffscPOS4 + shiftDiffscPOS5 + (1|subjectIndex), data = decisionSCRb, REML=F); 
# Fixed effects:
#                     Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)      9.128e-02  8.707e-03  6.133e+01  10.484 2.65e-15 ***
#   shiftDiffscPOS   1.479e-01  7.462e-02  1.347e+04   1.983   0.0474 *  
#   shiftDiffscPOS1 -1.129e-01  7.448e-02  1.347e+04  -1.516   0.1295    
#   shiftDiffscPOS2  3.130e-02  7.431e-02  1.347e+04   0.421   0.6736    
#   shiftDiffscPOS3 -6.300e-02  7.412e-02  1.347e+04  -0.850   0.3953    
#   shiftDiffscPOS4 -5.120e-02  7.391e-02  1.347e+04  -0.693   0.4885    
#   shiftDiffscPOS5 -1.350e-01  7.378e-02  1.347e+04  -1.830   0.0673 . 
#   AIC = -7879.1


# MODEL 8a FULL: Does shift effect interact with choice to influence decision SCR?
model8cEveryone= lmer(maxNorm ~ 1 + shiftDiffscPOS*choice1 + (1|subjectIndex), data = decisionSCRb, REML=F); 
# Fixed effects:
#                             Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)             9.061e-02  8.645e-03  6.081e+01  10.481 2.98e-15 ***
#   shiftDiffscPOS          9.728e-02  7.426e-02  1.378e+04   1.310   0.1903    
#   choice1                -2.197e-04  1.660e-03  1.384e+04  -0.132   0.8947    
#   shiftDiffscPOS:choice1  1.250e-01  7.441e-02  1.378e+04   1.680   0.0929 .  
#   AIC =  -7955.1

 
#MODEL 8d FULL: Do SCRs during decision phase account for additional risk-taking?
model8dEveryone = glmer(choice~ 0 + pastOC1sc + shiftDiffscPOS + cumEarningsSC01*pastOC1sc + maxNorm+ (1|subjectIndex), data = decisionSCRb, family="binomial", offset=pred);
# Fixed effects:
#                               Estimate   Std. Error z value Pr(>|z|)    
#   pastOC1sc                 -0.70876    0.11387  -6.224 4.84e-10 ***
#   shiftDiffscPOS             5.15014    1.01425   5.078 3.82e-07 ***
#   cumEarningsSC01            0.05570    0.07192   0.774    0.439    
#   maxNorm                   -0.02547    0.10854  -0.235    0.814    
#   pastOC1sc:cumEarningsSC01  1.27500    0.28970   4.401 1.08e-05 ***
#   AIC = 13796.9





# Lets plot what happens when the coefficients for linear expectation and earnings change (a figure to add to supplement to demonstrate what our methodology for combining expectation and earnings can show us)


ntrials = 50;
trial_numbers = 1:ntrials

outcome_history = array(dim = c(ntrials,1));

outcome_history[1:10] = runif(10, min = 4, max = 8);
outcome_history[11:25] = runif(15, min = 30, max = 47);
outcome_history[26:35] = runif(10, min = 2, max = 7);
outcome_history[36:50] = runif(15, min = 19, max = 30);

zero_outcomes = sample.int(ntrials, size = 20);

outcome_history[zero_outcomes] = 0;

plot(outcome_history)

earnings = cumsum(outcome_history)
earnings = earnings/max(earnings)
plot(earnings)

trial_numbers_normalized = trial_numbers/ntrials;

line1 = earnings*.5 - trial_numbers_normalized*.4;
#line1 = 1/(1+exp(-1*( (earnings*.5) + (trial_numbers_normalized* -.4) )))
line2 = earnings*.5 - trial_numbers_normalized*.5;
line3 = earnings*.5 - trial_numbers_normalized*.6;

pdf("/Volumes/shlab/Projects/VIC/data analysis/figures/manuscript_figSI_earningExp.pdf")
plot(trial_numbers, line1, type = 'l', col = 'green', lwd = 2,
     ylim = c(-.2, .2), ylab = 'p(gamble)', xlab = 'Trial Number')
lines(trial_numbers, line2, col = 'blue', lwd = 2)
lines(trial_numbers, line3, col = 'red', lwd = 2)
abline(h = 0, col = 'black', lty = 'dashed', lwd = 2)
abline(v = 25, col = 'black', lty = 'dotted')
points(x = 25, y = line1[25], col = 'green', lwd = 3)
points(x = 25, y = line2[25], col = 'blue', lwd = 3)
points(x = 25, y = line3[25], col = 'red', lwd = 3)

legend("topleft", legend=c("earnings > expectations", "earnings = expectations" , "earnings < expectations"), lty = 1, lwd=3, bty="n", col=c("green","blue",  "red"), cex=1.25)
dev.off()