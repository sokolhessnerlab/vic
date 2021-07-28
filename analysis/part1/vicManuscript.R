# VIC manuscript analysis (part 1 behavior and SCR outcome and decision phase)
# Hayley Roper Brooks
# Sokol-Hessner Lab
# University of Denver

rm(list=ls());


# load data
load("/Volumes/shlab/Projects/VIC/data analysis/SCR/decisionAnalysis/vicBscrResp.Rdata"); # decision SCR dataset (responders only N = 26)
decisionSCRb = vicBscrResp; # rename because the outcome SCR file will have the same name;

load("/Volumes/shlab/Projects/VIC/data analysis/vicDataB.Rdata"); # load the behavior dataset
# this includes the predicted values (prior to softmax) for our trial-level model in the column "pred"
load("/Volumes/shlab/Projects/VIC/data analysis/SCR/vicBscrResp.Rdata"); # load the SCR following outcomes dataset (responders only N = 46)

#load packages
library('lme4');
library('lmerTest');

# number of participants
nSub = length(unique(vicDataB$subjectIndex));

####### Using Offset ########

# The predicted values from the model below are already saved in the vicDataB and vicBscrRespDont ($pred) so the model below does not need to be run however, the steps for getting the predicted values are outlined below

# Model 1: Current-trial regression (commented out)

  #mode1_currTrial = glmer(choice ~ 0+gainSC + altSC+ groundEVsc + (0+gainSC + altSC| subjectIndex), data = vicDataB,   family='binomial', control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)));

# Extract the predicted values from this regression PRIOR to going through the softmax (type='link'). That means   that 1/(1+exp(-1*(THIS))) will give the response values which we know are in probability space.
    # vicDataB$pred = predict(mode1_currTrial,type='link'); # these are already saved in dataframe.
    # predicted value summary stats: mean= -.203, median = -.05, range = -8.68-7.16
# To see this, note that the first two values for vicDataB$pred are
      -0.053545272 # trial 1
       0.003000541 # trial 2
# corresponding to... 
    33.49889 * 0.01451379 + -18.287337 * 0.009869376 + -49.506 * 0.007256894; # trial 1
    33.49889 * 0.07256894 + -18.287337 * 0.034542816 + -49.506 * 0.036284470; # trial 2
    # (using subject-specific betas & scaled gains, alts, and groundEVs from trials 1 & 2)  
    # which is within reasonable numeric precision (< 0.00002).

# We'll now INCLUDE these as an "offset" in future regressions. They'll be applied inside
# the softmax with a weight of 1 every time, in effect accounting for our best 
# current-trial predictions by directly entering those predictions on a trial-by-trial level.

# This lets us retain the binomial analysis structure vs using residuals ("resids"), which is much more accurate.

    
#### Linear mixed effects regression models ####
    
# MODEL 2: PAST OUTCOME
model_pastotc = glmer(choice ~ 0 + pastOC1sc + (1 | subjectIndex),
               data = vicDataB, family = 'binomial', offset = pred)
# Fixed effects:
#           Estimate Std. Error z value Pr(>|z|)  
#  pastOC1sc -0.14853    0.06124  -2.425   0.0153 *
#  AIC = 14025.1

pastotcbeta = as.double(fixef(model_pastotc));

# Assuming indifference in trial-level land, this effect means...
1/(1+exp(-1*(pastotcbeta))) # 0.463 is p(gamble | max prev. outcome), vs. 0.5, a decline of 3.7% in risk-taking


# MODEL 3A: SHIFTS
model_shift = glmer(choice ~ 0 + pastOC1sc + shiftDiffscPOS + shiftDiffscNEG + (1 | subjectIndex), data = vicDataB, family = 'binomial', offset = pred)

# Fixed effects:
#                   Estimate Std. Error z value Pr(>|z|)    
#   pastOC1sc      -0.18331    0.06052  -3.029  0.00246 ** 
#   shiftDiffscPOS  4.79699    0.99266   4.832 1.35e-06 ***
#   shiftDiffscNEG -0.36913    0.90010  -0.410  0.68173  
#   AIC = 14005.4

shiftbeta = as.list(fixef(model_shift));

# Assuming indifference in trial-level and pastOutcome land, this effect means...
1/(1+exp(-1*(shiftbeta$shiftDiffscPOS*max(vicDataB$shiftDiffscPOS)))) # 0.7396859 is p(gamble | max shift up), vs. 0.5, an increase of 24% in risk-taking


# MODEL 3B: POSITIVE SHIFT: How far back?
model_shift_howfarback = glmer(choice ~ 0 + shiftDiffscPOS + shiftDiffscPOS1 + pastOC1sc + (1 | subjectIndex),
                    data = vicDataB, family = 'binomial', offset = pred)
# ONE TRIAL BACK ONLY

# Fixed effects:
#                   Estimate Std. Error z value Pr(>|z|)    
#   shiftDiffscPOS   4.59259    1.00959   4.549 5.39e-06 ***
#   shiftDiffscPOS1  1.53100    0.99774   1.534  0.12491    
#   pastOC1sc       -0.19347    0.06045  -3.200  0.00137 ** 
#   AIC = 14003.2


# MODEL 3C: Does positive shift interact with outcome?
model_shift_intrxn = glmer(choice ~ 0 + shiftDiffscPOS*pastOC1sc + (1 | subjectIndex), data = vicDataB, family = 'binomial', offset = pred)

# Fixed effects:
#                             Estimate Std. Error z value Pr(>|z|)    
#   shiftDiffscPOS            6.20723    1.41259   4.394 1.11e-05 ***
#   pastOC1sc                -0.16994    0.06016  -2.825  0.00473 ** 
#   shiftDiffscPOS:pastOC1sc -7.70914    5.40107  -1.427  0.15348    
#   AIC =  14004.0 

# MODEL 4A: Cumulative earnings & linear expectations 

model_earnings_Expectations = glmer(choice ~ 0 + pastOC1sc + shiftDiffscPOS + cumEarningsSC01 + linExpectation + (1 | subjectIndex),data = vicDataB, family = 'binomial', offset = pred);

# Fixed effects:
#                   Estimate Std. Error z value Pr(>|z|)    
#   pastOC1sc      -0.34749    0.07617  -4.562 5.07e-06 ***
#   shiftDiffscPOS  4.78708    0.99825   4.795 1.62e-06 ***
#   cumEarningsSC01  0.75778    0.27833   2.723  0.00648 ** 
#   linExpectation  -0.43454    0.21381  -2.032  0.04212 *  
#   AIC =  13991.3

cumearningsbeta = as.list(fixef(model_earnings_Expectations));

# Assuming indifference in linear expectation and pastOutcome land, this effect means...
1/(1+exp(-1*(cumearningsbeta$cumEarningsSC01))) # 0.6808716 is p(gamble | max earnings), vs. 0.5, an increase of 18% in risk-taking

# mean cumulative earnings at the end of the task is ~4229 
# earnings are scaled and $4229 coressponds to ~.75
1/(1+exp(-1*(cumearningsbeta$cumEarningsSC01*.75)));
#p(gamble | mean max earning) = .64 vs. 0.5, a 14% increase in risk-taking by the end of the task (on average)


# plot a few sub's earnings and pgamble over the task as a function of earnings and expectations
# FIGURE 4 FOR MANUSCRIPT

earnCoef = cumearningsbeta$cumEarningsSC01
expCoef = cumearningsbeta$linExpectation

# plot a couple of subs' earnings and pgamble over the task as a function of earnings and trial
sub1 = vicDataB[vicDataB$subjectIndex==20,];
PgamSub1EarnTrial =1/(1+exp(-1*( (expCoef*sub1$linExpectation) + (earnCoef*sub1$cumEarningsSC01) )));

sub2 = vicDataB[vicDataB$subjectIndex==22,];
PgamSub2EarnTrial =1/(1+exp(-1*( (expCoef*sub2$linExpectation) + (earnCoef*sub2$cumEarningsSC01) )))

sub3 = vicDataB[vicDataB$subjectIndex==34,];
PgamSub3EarnTrial =1/(1+exp(-1*( (expCoef*sub3$linExpectation) + (earnCoef*sub3$cumEarningsSC01) )))

sub4 = vicDataB[vicDataB$subjectIndex==13,];
PgamSub4EarnTrial =1/(1+exp(-1*( (expCoef*sub4$linExpectation) + (earnCoef*sub4$cumEarningsSC01) )))

sub5 = vicDataB[vicDataB$subjectIndex==29,];
PgamSub5EarnTrial =1/(1+exp(-1*( (expCoef*sub5$linExpectation) + (earnCoef*sub5$cumEarningsSC01) )))

sub6 = vicDataB[vicDataB$subjectIndex==62,];
PgamSub6EarnTrial =1/(1+exp(-1*( (expCoef*sub6$linExpectation) + (earnCoef*sub6$cumEarningsSC01) )))

pdf("/Volumes/shlab/Projects/VIC/data analysis/figures/PGAMearnings_expectation.pdf")
plot(PgamSub1EarnTrial, type="l", col="darkgreen", lwd=3, ylim=c(.4,.6), ylab="p(gamble)", xlab="trial", main="risk taking as a function\n of earnings and expectations", axes=F, cex.lab=1, xlim=c(1,240))
abline(a=.5, b=0, lty="dashed", col="darkgrey", lwd=2)
lines(PgamSub2EarnTrial, col="darkred", lwd=3);
lines(PgamSub3EarnTrial, col="darkblue", lwd=3);
lines(PgamSub4EarnTrial, col="darkorange", lwd=3);
lines(PgamSub5EarnTrial, col="goldenrod", lwd=3);
lines(PgamSub6EarnTrial, col="red", lwd=3);

legend("bottomright", legend=c("sub 20", "sub 34" , "sub 62","sub 22","sub 13", "sub 29"), lty = 1, lwd=3, bty="n", col=c("darkgreen","darkblue",  "red", "darkred","darkorange","goldenrod"), cex=1.25)
axis(1, at=c(1,120,240),tick =T, cex.axis = 1.25, cex=2, lwd = 4)
axis(2, tick =T, cex.axis = 1.25, cex=2, lwd = 4)

dev.off();

pdf("/Volumes/shlab/Projects/VIC/data analysis/figures/earningsAcrossTask.pdf")
plot(sub1$cumEarningsSC01,type="l", col="darkgreen", lwd=3, xlab="trial", ylab="earnings (normalized)", main="earnings across the task", ylim=c(0,1), axes=F)
lines(sub2$cumEarningsSC01, col="darkred", lwd=3)
lines(sub3$cumEarningsSC01, col="darkblue", lwd=3)
lines(sub4$cumEarningsSC01, col="darkorange", lwd=3)
lines(sub5$cumEarningsSC01, col="goldenrod", lwd=3)
lines(sub6$cumEarningsSC01, col="red", lwd=3)
legend("bottomright", legend=c("sub 20", "sub 34" , "sub 62","sub 22","sub 13", "sub 29"), lty = 1, lwd=3, bty="n", col=c("darkgreen","darkblue",  "red", "darkred","darkorange","goldenrod"), cex=.75)
axis(1, lwd=4)
axis(2, lwd=4)
dev.off()


# comparing linear expectation in model alone with cumulative earnings in model alone 
model_linExp = glmer(choice ~ 0 + pastOC1sc + shiftDiffscPOS + linExpectation + (1 | subjectIndex),data = vicDataB, family = 'binomial', offset = pred);

# Fixed effects:
#                   Estimate Std. Error z value Pr(>|z|) 
# pastOC1sc      -0.31834    0.07548  -4.218 2.47e-05 ***
# shiftDiffscPOS  4.61678    1.00259   4.605 4.13e-06 ***
# linExpectation   0.13548    0.04625   2.929   0.0034 ** 
# AIC = 13996.9

model_earnings = glmer(choice ~ 0 + pastOC1sc + shiftDiffscPOS + cumEarningsSC01 + (1 | subjectIndex),data = vicDataB, family = 'binomial', offset = pred);

# Fixed effects:
#                   Estimate Std. Error z value Pr(>|z|) 
# pastOC1sc       -0.34711    0.07611  -4.561 5.10e-06 ***
# shiftDiffscPOS   4.63525    1.00426   4.616 3.92e-06 ***
# cumEarningsSC01  0.20711    0.05995   3.455 0.000551 ***
#AIC = 13993.5

#AIC is lower with earnings, earnings explains more variance


# We assume above that expectations are linear but it is possible they are not linear. let's see if there is any indication of nonlinear expectations (using the level tracking variable - calling it piecewise linear. This variable is normalized by OVERALL max level tracking)

# These models are included in the supplement:
model_levelTrackingLinExp = glmer(choice ~ pastOC1sc + shiftDiffscPOS + cumEarningsSC01+levelTrackingNormOverall + linExpectation+  (1 | subjectIndex),data = vicDataB, family = 'binomial', offset = pred);


# (Intercept)              -0.08691    0.04950  -1.756  0.07917 .  
# pastOC1sc                -0.26470    0.08876  -2.982  0.00286 ** 
# shiftDiffscPOS            5.24208    1.03787   5.051  4.4e-07 ***
# cumEarningsSC01           1.29624    0.70411   1.841  0.06563 .  
# levelTrackingNormOverall -0.58979    0.70568  -0.836  0.40328    
# linExpectation           -0.30096    0.22430  -1.342  0.17966  
#  AIC = 13991.1
# level tracking and linear expectation not significant, earnings now trending.

# remove linear expectation:
model_levelTracking = glmer(choice ~ pastOC1sc + shiftDiffscPOS + cumEarningsSC01+levelTrackingNormOverall +  (1 | subjectIndex),data = vicDataB, family = 'binomial', offset = pred);

#Estimate Std. Error z value Pr(>|z|)    
#  (Intercept)              -0.10150    0.04821  -2.105  0.03526 *  
#  pastOC1sc                -0.24989    0.08809  -2.837  0.00456 ** 
#  shiftDiffscPOS            5.22186    1.04122   5.015  5.3e-07 ***
#  cumEarningsSC01           1.09399    0.68266   1.603  0.10904    
# levelTrackingNormOverall -0.74967    0.69376  -1.081  0.27988    
# AIC =  13990.9 - which is an improvement in AIC over the model with earnings and expectation AIC = 13991.3
# hmm, no effect of earnings or level tracking
# Note: looking for nonlinear effects using glmer is not a great approach because its a linear regression but its unclear how to mathematically account for the difference between earnings and expectations because here they are both weighted differently so we can't just take the difference (how do we then decide the weight for each variable?). 


# MODEL 4B: Cumulative earnings interaction with past otc?
model_cumearningspastOutcomeinterxn = glmer(choice ~ 0 + shiftDiffscPOS + pastOC1sc*cumEarningsSC01 + linExpectation + (1 | subjectIndex),data = vicDataB, family = 'binomial', offset = pred)

#                               Estimate Std. Error z value Pr(>|z|)
#   shiftDiffscPOS              5.2581     1.0118   5.197 2.03e-07 ***
#   pastOC1sc                  -0.7082     0.1131  -6.259 3.86e-10 ***
#   cumEarningsSC01             0.3780     0.2889   1.309    0.191    
#   linExpectation             -0.2582     0.2164  -1.193    0.233    
#   pastOC1sc:cumEarningsSC01   1.2627     0.2921   4.323 1.54e-05 ***
#   AIC = 13974.6 (AIC slightly larger than model w/o trial) - using this in manuscript 




#### Three timescales of temporal context and SCRs #######
# Do not need offset analysis here since SCRs are dependent variable

# MODEL 5A - Do outcome amounts predict SCRs following outcomes?
model_SCRpastOutcome = lmer(maxNorm ~ 1 + outcomeSC+ (1|subjectIndex), data=vicBscrResp, REML = F);

# Fixed effects: 
#                 Estimate Std. Error         df t value Pr(>|t|)  
#   (Intercept)  1.674e-01  9.444e-03  5.330e+01  17.724   <2e-16 ***
#   outcomeSC   -1.297e-02  9.291e-03  1.028e+04  -1.396    0.163  
#   AIC = -1883.6


# MODEL 5B (w/ variations)- Does positive shift predict SCRs following outcomes?
model_SCRshiftDiffPos = lmer(maxNorm ~ 1 + shiftDiffscPOS +(1|subjectIndex), data = vicBscrResp, REML=F); #n.s. 

# Fixed effects: 
#                     Estimate Std. Error         df t value Pr(>|t|) 
#   (Intercept)     1.644e-01  9.086e-03  4.612e+01  18.095   <2e-16 ***
#   shiftDiffscPOS -1.321e-01  1.042e-01  1.026e+04  -1.268    0.205  
#   AIC = -1883.2
# *added negative shift to this model and it is not significant

# Variations of MODEL 5B:
model_SCRshiftDiff = lmer(maxNorm ~ 1 + shiftDiff + (1|subjectIndex), data = vicBscrResp, REML=F); # n.s.
model_SCRshiftDiffNeg = lmer(maxNorm ~ 1 + shiftDiffscNEG + (1|subjectIndex), data = vicBscrResp, REML=F); #n.s.
model_SCRshiftDiffAbs= lmer(maxNorm ~ 1 + shiftDiffscABs + (1|subjectIndex), data = vicBscrResp, REML=F); #n.s.
 

# MODEL 5C - do cumulative earnings predict SCRs following outcomes?
model_SCRcumearningsTrial <- lmer(maxNorm ~ 1 + linExpectation +cumEarningsSC01 + (1|subjectIndex), data=vicBscrResp, REML =F);

# Fixed effects: 
#                     Estimate Std. Error         df t value Pr(>|t|) 
#   (Intercept)      1.101e-01  9.609e-03  6.344e+01  11.459  < 2e-16 ***
#   linExpectation   -1.599e-02  3.547e-02  3.528e+03  -0.451 0.652231    
#   cumEarningsSC01  1.621e-01  4.620e-02  3.315e+03   3.510 0.000455 ***
#   AIC =  -2093.4


# MODEL 5D - Do outcomes interact with cumulative earnings to predict SCRs following outcomes?
model_SCRcumearningsPastoutcomeInterxn <- lmer(maxNorm ~ 1 + outcomeSC*cumEarningsSC01  + (1|subjectIndex), data=vicBscrResp, REML = F); 
# Fixed effects: 
#                                Estimate Std. Error         df t value Pr(>|t|) 
#   (Intercept)                 1.132e-01  1.084e-02  1.012e+02  10.442   <2e-16 ***
#   outcomeSC                 -1.147e-02  1.742e-02  1.028e+04  -0.658    0.510    
#   cumEarningsSC01             1.380e-01  1.459e-02  1.030e+04   9.458   <2e-16 ***
#   outcomeSC:cumEarningsSC01  1.239e-02  3.875e-02  1.028e+04   0.320    0.749    
#   AIC =-2091.8 
#   no interaction, only cumulative earnings are significant and positive
#   including linear expectation - which is ns

#### SCRs and risk-taking - Using Offset ####

# MODEL 6A: does SCR predict residual gambling behavior in addition to outcome, shift, and earnings?
model_pastOutcomeShiftEarningsSCR = glmer(choice ~ 0 + maxNormPast + pastOC1sc*cumEarningsSC01 + shiftDiffscPOS+ (1|subjectIndex), data = vicBscrResp, family="binomial", offset=pred);
# Fixed effects: 
#                             Estimate Std. Error z value Pr(>|z|) 
#   maxNormPast               -0.09447    0.10792  -0.875  0.38140    
#   pastOC1sc                 -0.75210    0.13540  -5.555 2.78e-08 ***
#   cumEarningsSC01            0.21491    0.08972   2.395  0.01661 *  
#   shiftDiffscPOS             4.75220    1.20814   3.933 8.37e-05 ***
#   pastOC1sc:cumEarningsSC01  1.14658    0.35044   3.272  0.00107 ** 
#   AIC =  10039.4 

# MODEL 6B: replace cumulative earnings with SCR - does SCR behave like cumulative earnings to predict risk-taking?
model_pastOutcomeShiftSCRinterxn = glmer(choice ~ 0 + maxNormPast*pastOC1sc + shiftDiffscPOS+ (1|subjectIndex), data = vicBscrResp, family="binomial", offset=pred);
# Fixed effects: 
#                         Estimate Std. Error z value Pr(>|z|) 
#   maxNormPast            0.09605    0.13411   0.716 0.473895    
#   pastOC1sc             -0.18145    0.08575  -2.116 0.034333 *  
#   shiftDiffscPOS         4.35229    1.19558   3.640 0.000272 ***
#   maxNormPast:pastOC1sc -0.14738    0.41899  -0.352 0.725032  
#   AIC = 10070.3


#### Decision-SCRs analysis ####

#MODEL 7: Do decsion SCRs vary as a function of choice?
model_decSCRsChoice = lmer(maxNorm ~ 1 + choice1 + (1|subjectIndex), data=decisionSCRb, REML=F);

# Fixed effects: 
#                 Estimate Std. Error         df t value Pr(>|t|) 
#   (Intercept) 1.436e-01  1.352e-02 2.568e+01  10.621 6.87e-11 ***
#   choice1     8.390e-04  2.938e-03 5.826e+03   0.286    0.775  
#   AIC = -1575.2

#MODEL 8A: Effect of 3 timescales on decision SCRs?
model_decSCRs3timescales <- lmer(maxNorm ~ 1 + pastOC1sc + shiftDiffscPOS + cumEarningsSC01 + linExpectation+ (1|subjectIndex), data=decisionSCRb, REML = F);
# Fixed effects: 
#                      Estimate Std. Error        df   t value Pr(>|t|) 
#   (Intercept)        0.13019    0.01458   36.94156   8.928 9.32e-11 ***
#   pastOC1sc         -0.02226    0.01142 5788.31457  -1.949  0.05140 .  
#   shiftDiffscPOS     0.40963    0.13219 5784.59269   3.099  0.00195 ** 
#   cumEarningsSC01   -0.04378    0.05855 2603.87392  -0.748  0.45467    
#   linExpectation     0.06715    0.04568 2743.28542   1.470  0.14168 
#   AIC =  -1632.4

#MODEL 8B: How far back does shift effect go?
model_decSCRshift_howfarback = lmer(maxNorm ~ 1 + shiftDiffscPOS + shiftDiffscPOS1 + (1|subjectIndex), data = decisionSCRb, REML=F); 
# Fixed effects: 
#                      Estimate Std. Error        df   t value Pr(>|t|) 
#   (Intercept)        0.14179    0.01343   25.88401  10.561 7.07e-11 ***
#   shiftDiffscPOS     0.39837    0.13123 5779.86732   3.036  0.00241 ** 
#   shiftDiffscPOS1   -0.17054    0.13123 5779.86732  -1.300  0.19380    
#   AIC = -1620.7 

# shift several trials back (plotted in figure 4b)
model_decSCRshift_howfarback_6trials = lmer(maxNorm ~ 1 + shiftDiffscPOS + shiftDiffscPOS1 + shiftDiffscPOS2 + shiftDiffscPOS3 + shiftDiffscPOS4 + shiftDiffscPOS5 + (1|subjectIndex), data = decisionSCRb, REML=F); 
#   Fixed effects:
#                         Estimate Std. Error         df t value Pr(>|t|)   
#   (Intercept)        0.14419    0.01362   26.11294  10.587 6.07e-11 ***
#   shiftDiffscPOS     0.44500    0.13356 5675.80422   3.332 0.000868 ***
#   shiftDiffscPOS1   -0.12880    0.13331 5675.80535  -0.966 0.334008    
#   shiftDiffscPOS2    0.01385    0.13297 5675.80365   0.104 0.917024    
#   shiftDiffscPOS3   -0.16477    0.13260 5675.80977  -1.243 0.214075    
#   shiftDiffscPOS4   -0.10016    0.13223 5675.81783  -0.757 0.448816    
#   shiftDiffscPOS5   -0.23801    0.13200 5675.82641  -1.803 0.071423 .  
#   AIC = -1566.1

#MODEL 8C: Does shift effect interact with choice to influence decision SCR?
model_decSCRshiftChoiceInterxn = lmer(maxNorm ~ 1 + shiftDiffscPOS*choice1 + (1|subjectIndex), data = decisionSCRb, REML=F); 
#   Fixed effects:
#                           Estimate Std. Error         df t value Pr(>|t|) 
#   (Intercept)             1.420e-01  1.353e-02  2.577e+01  10.495 8.49e-11 ***
#   shiftDiffscPOS          3.431e-01  1.336e-01  5.806e+03   2.568   0.0102 *  
#   choice1                -2.112e-04  2.990e-03  5.827e+03  -0.071   0.9437    
#   shiftDiffscPOS:choice1  1.937e-01  1.338e-01  5.808e+03   1.447   0.1479   
#   AIC = -1581.5


#MODEL 8D: Does SCR during decision-phase account for additional risk-taking?
model_decSCRearningsPOCinterxn = glmer(choice~ 0 + pastOC1sc + shiftDiffscPOS + cumEarningsSC01*pastOC1sc + maxNorm+ (1|subjectIndex), data = decisionSCRb, family="binomial", offset=pred);
#   Fixed effects:
#                             Estimate Std. Error z value Pr(>|z|)  
#   pastOC1sc                 -0.77901    0.18191  -4.282 1.85e-05 ***
#   shiftDiffscPOS             4.51789    1.64779   2.742  0.00611 ** 
#   cumEarningsSC01            0.12934    0.12046   1.074  0.28295    
#   maxNorm                   -0.04317    0.14696  -0.294  0.76892    
#   pastOC1sc:cumEarningsSC01  1.34873    0.46039   2.930  0.00339 ** 
#   AIC = 5366.2 




# FIGURES 2 and 3a-c for manuscript:
# Figure 2: Part 1 choice set (across the task; alternatives only)
pdf("/Volumes/shlab/Projects/VIC/data analysis/figures/manuscriptFig2.pdf")
choiceset = vicDataB[vicDataB$subjectIndex==45,];
plot(choiceset$groundEV, col="red", pch = "-", main = "Part 1 example choice set\nguaranteed alternative", xaxt = "n", yaxt = "n", xlab = "Trial", ylab = "Amount ($)", ylim = c(0,35), cex.main = 1, cex.lab = 1, axes =F);
points(choiceset$alternative, pch = 21, bg ="blue");
axis(1,at = c(1,seq(from =24, to =240, by =24)), label = c(1,seq(from =24, to =240, by =24)), tick = TRUE,cex.axis=1, lwd=2, lwd.ticks = 2);
axis(2,tick = TRUE,cex.axis=1, lwd=2);
dev.off();

# Figure 3a: Effect size of past outcome ($15 and $45) - left off here with formatting the figure!
pastOutcomeResults = summary(model_pastotc)
poc = seq(from=0, to = 1, by = .2); # values are scaled here
vicPOCgam = 1/(1+exp(-1*(fixef(model_pastotc)*poc)))  # from the model where past outcome is alone 
pdf("/Volumes/shlab/Projects/VIC/data analysis/figures/manuscript_fig3a_offset_pocES.pdf")
par(mar=c(5,6,6,3));#change margin so ylab is not cut off
plot(vicPOCgam, type = "l",axes = FALSE, xaxt="n", ann=F, lwd = 4, cex.lab=1, ylim=c(.25,.75))
title(ylab = "p(gamble)", line = 3.75, cex.lab=1.35)
title(xlab = "Past outcome", line = 2.5, cex.lab=1.35)
title(main = sprintf("Risk-taking following outcomes \npast outcome: %.2f(%.2f), p=%.2f", fixef(model_pastotc),pastOutcomeResults$coefficients[2] ,pastOutcomeResults$coefficients[4]))
poclab = c("$0", "$14","27","$41", "$54", "$68")
axis(1, at = c(1:6), labels =poclab, tick =T, cex.axis = 1.25, cex=2, lwd = 4)
#axis(2, at = round(vicPOCgam, digits=3), tick = T, las =2, cex.axis = 1, cex=2, lwd=2)
axis(2, at = c(.25,round(vicPOCgam[2],digits=2),round(vicPOCgam[5], digits=2),.75), tick = T, las =2, cex.axis = 1.25, cex=2, lwd=4)
abline(a=.5,b=0, col="grey", lty=1, lwd=3)
lines(c(2,2), c(0,vicPOCgam[2]), lty = 4, col="red", lwd = 4);#vertical line at poc $14
lines(c(1,2),c(vicPOCgam[2],vicPOCgam[2]), lty=4, col="red", lwd =4); #horizontal line 
lines(c(5,5), c(0,vicPOCgam[5]), lty = 5, col="darkgreen", lwd = 4);
lines(c(1,5),c(vicPOCgam[5],vicPOCgam[5]), lty=5, col="darkgreen", lwd =4); 
lines(vicPOCgam, lwd=4); #plotting the line again so its on top.
dev.off();

# Figure 3b: Effect size of positive shift ($0 and $15)
shiftResults = summary(model_shift)
1/(1+exp(-1*(shiftbeta$shiftDiffscPOS*max(vicDataB$shiftDiffscPOS)))) # 0.7396859 is p(gamble | max shift up), vs. 0.5
# an increase of 24%
shiftvals = seq(from=0, to = max(vicDataB$shiftDiffscPOS), length.out= 6)
vicShiftgam = 1/(1+exp(-1*(shiftbeta$shiftDiffscPOS*shiftvals)))  # from the model where past outcome is alone 

pdf("/Volumes/shlab/Projects/VIC/data analysis/figures/manuscript_fig3b_offset_shiftES.pdf")
par(mar=c(5,6,6,3));#change margin so ylab is not cut off
plot(vicShiftgam, type = "l",axes = FALSE, xaxt="n", ann=F, lwd = 4, cex.lab=1, ylim=c(.25,.75))
title(ylab = "p(gamble)", line = 3.75, cex.lab=1.35)
title(xlab = "Positive shift", line = 2.5, cex.lab=1.35)
title(main = sprintf("Risk-taking following positive shift \npositive shift: %.2f(%.2f), p=%f", shiftbeta$shiftDiffscPOS,shiftResults$coefficients[5] ,shiftResults$coefficients[11]))
shiftlab = c("$0", "$3","$6","$9", "$12", "$15")
abline(a=.5,b=0, col="grey", lty=1, lwd=3)

axis(1, at = c(1:6), labels =shiftlab, tick =T, cex.axis = 1.25, cex=2, lwd = 4)
axis(2, at = c(.25,round(vicShiftgam[2],digits=2),round(vicShiftgam[5], digits = 2),.75), tick = T, las =2, cex.axis = 1.25, cex=2, lwd=4)

lines(c(2,2), c(-2,vicShiftgam[2]), lty = 4, col="red", lwd = 4);#
lines(c(1,2),c(vicShiftgam[2],vicShiftgam[2]), lty=4, col="red", lwd =4); #horizontal line 
lines(c(5,5), c(-2,vicShiftgam[5]), lty = 5, col="darkgreen", lwd = 4);
lines(c(1,5),c(vicShiftgam[5],vicShiftgam[5]), lty=5, col="darkgreen", lwd =4); 
lines(vicShiftgam, lwd=4); #plotting the line again so its on top.
dev.off();

# Figure 3c: Effect size of cumulative earnings and outcome 
# pick a point in the task (say half way, so .5)
# what is the effect of outcome (small, medium, large) as cumulative earnings increase (small, medium, large)?
potcEarnResults = summary(model_cumearningspastOutcomeinterxn);
pocBeta = potcEarnResults$coefficients[2]
earnBeta = potcEarnResults$coefficients[3]
linExpBeta = potcEarnResults$coefficients[4]
potcEarnBeta = potcEarnResults$coefficients[5];


linExp = .5; # middle of the task
earnings = c(.2,.5,.8); # below, at, and above linear expectation
#earnings = seq(from=0, to=.75, length.out=length(poc));# mean cumulative earnings (scaled between 0 and 1) at the end of the task is ~4229 (scaled ~.75)

#earnings below expectation:
lowEarnGam = 1/(1+exp(-1*(pocBeta*poc + earnBeta*earnings[1]+ potcEarnBeta*earnings[1]*poc + linExpBeta*linExp)))

#earnings equal expectations
equalEarnGam = 1/(1+exp(-1*(pocBeta*poc + earnBeta*earnings[2]+ potcEarnBeta*earnings[2]*poc + linExpBeta*linExp )))

#earnings above expectations
aboveEarnGam= 1/(1+exp(-1*(pocBeta*poc + earnBeta*earnings[3]+ potcEarnBeta*earnings[3]*poc + linExpBeta*linExp )))

pdf("/Volumes/shlab/Projects/VIC/data analysis/figures/manuscript_fig3c_offset_pocEarnES.pdf")
par(mar=c(5,6,6,3));#change margin so ylab is not cut off
plot(lowEarnGam, type="l", ylim = c(.25,.75),axes = FALSE, xaxt="n", ann=F, lwd = 4, cex.lab=1, col="salmon2")
lines(equalEarnGam, lwd=4, col="salmon3")
lines(aboveEarnGam, lwd=4, col ="salmon4")
title(ylab = "p(gamble)", line = 3.75, cex.lab=1.35, cex=2)
title(xlab = "Past outcome", line = 2.5, cex.lab=1.35, cex=2)
title(main = sprintf("Risk-taking following earnings & outcome \niteraction: %.2f(%.2f), p=%f", potcEarnBeta ,potcEarnResults$coefficients[10] ,potcEarnResults$coefficients[20]))
poclab = c("$0", "$14","28","$42", "$56", "$70")
axis(1, at = c(1:6), labels =poclab, tick =T, cex.axis = 1.25, cex=2, lwd = 4)
axis(2, at = c(.25, .5, .75), tick = T, las =2,las=2, cex.axis = 1.25, cex=2, lwd=4)
dev.off()


# Figure 3d-f in pdmAnalysis script
# Figure 4 is plotted above starting at line 129
# Figure 5a and b - DECISION SCRS is in "vicOffsetSCRdecisionAnalysis.R"

