# Part 2 of VIC manuscript
# created 11/23/20
# HRB

# CLEAR ENVIRONMENT
rm(list=ls());

#LOAD PACKAGES
library(lme4);
library(lmerTest);

# LOAD DATA (output from data set up script)
load("/Volumes/shlab/Projects/PDM/code/pdmData.Rdata");

# DOES RISK-TAKING CHANGE AS A FUNCTION OF RISKY GAIN, RISKY LOSS, AND SAFE VALUES, AND DAY?
model9_trialLevel = glmer(choice~ gainSC + lossSC + safeSC + day+ (1|subID), data=pdmData, family = "binomial");
#AIC = 13550.8
#             Estimate   Std. Error   z value   Pr(>|z|)    
#  (Intercept)   0.13705    0.13746   0.997    0.319    
#  gainSC        9.05427    0.26737  33.865   <2e-16 ***
#  lossSC       12.14485    0.24667  49.234   <2e-16 ***
#  safeSC      -18.65470    0.53944 -34.581   <2e-16 ***
#  day          -0.18830    0.02132  -8.834   <2e-16 ***

#MODEL 10: DOES RISK-TAKING CHANGE AS A FUNCTION OF PAST OUTCOME
model10_pastOutcome= glmer(choice~gainSC + lossSC + safeSC + day+  pOC1sc + (1|subID), data=pdmData, family = "binomial",control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=1000000)));

# AIC = 13385.3
#             Estimate   Std. Error   z value   Pr(>|z|)    
#  (Intercept)   0.18369    0.13848   1.326    0.185    
#  gainSC        9.14857    0.27044  33.828   <2e-16 ***
#  lossSC       12.21458    0.24902  49.051   <2e-16 ***
#  safeSC      -18.81036    0.54616 -34.441   <2e-16 ***
#  day          -0.19025    0.02147  -8.863   <2e-16 ***
#  pOC1sc       -0.95299    0.11180  -8.524   <2e-16 ***

#MODEL 11A-B: DOES RISK-TAKING CHANGE AS A FUNCTION OF DIFFERENCE BETWEEN MEAN EV ON TRIAL T AND AVERAGE MEAN EV ACROSS PREVIOUS 3 AND 20 TRIALS?
model11a_meanEVdiff3trials = glmer(choice ~1 + gainSC + lossSC + safeSC + day + pOC1sc + meanEVdiff3tBackscNEG + meanEVdiff3tBackscPOS + (1|subID), data = pdmData, family="binomial");
# AIC =  13180.0 
#                         Estimate Std. Error z value Pr(>|z|)    
# (Intercept)             0.21926    0.13977   1.569  0.11672    
#  gainSC                  9.13973    0.28742  31.800  < 2e-16 ***
#  lossSC                 12.03993    0.26381  45.638  < 2e-16 ***
#  safeSC                -18.50302    0.66597 -27.784  < 2e-16 ***
#  day                    -0.18946    0.02164  -8.754  < 2e-16 ***
#  pOC1sc                 -0.90435    0.11906  -7.596 3.05e-14 ***
#  meanEVdiff3tBackscNEG   1.49395    0.40901   3.653  0.00026 ***
#  meanEVdiff3tBackscPOS  -0.55934    0.65874  -0.849  0.39582   

1/(1+exp(-1*(1.5*-.35))); # (p gamble | large negative shift) = .37 (13% decrease from no difference in mean ev)

model11b_meanEVdiff20trials = glmer(choice ~1 + gainSC + lossSC + safeSC + day + pOC1sc + meanEVdiff20tBackscPOS + meanEVdiff20tBackscNEG+ (1|subID), data = pdmData, family="binomial"); # AIC = 11664.0
#                         Estimate Std. Error z value Pr(>|z|)    
# (Intercept)              0.26505    0.14426   1.837   0.0662 .  
# gainSC                   8.54531    0.35695  23.940  < 2e-16 ***
# lossSC                  10.94793    0.32259  33.938  < 2e-16 ***
# safeSC                 -16.32370    1.01378 -16.102  < 2e-16 ***
# day                     -0.17706    0.02301  -7.695 1.42e-14 ***
# pOC1sc                  -0.94941    0.12179  -7.795 6.43e-15 ***
# meanEVdiff20tBackscPOS  -1.71920    1.29925  -1.323   0.1858    
# meanEVdiff20tBackscNEG   5.91915    0.90364   6.550 5.74e-11 ***


1/(1+exp(-1*(5.9*-.18)));# (p gamble | large negative shift) = .26 (24% decrease from no difference in mean ev)


#MODEL 11c: DOES RISK-TAKING CHANGE AS A FUNCTION OF MEAN EV DIFF BETWEEN TRIAL T AND T-1?
model11c_meanEVdiff1trial = glmer(choice ~1 + gainSC + lossSC + safeSC + day + pOC1sc + meanEVdiffPOSsc + meanEVdiffNEGsc + (1|subID), data = pdmData, family="binomial"); 
# AIC = 13387.3
#                 Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       0.18241    0.13897   1.313    0.189    
#  gainSC            9.24361    0.28223  32.752  < 2e-16 ***
#  lossSC           12.30474    0.25851  47.599  < 2e-16 ***
#  safeSC          -18.55055    0.61185 -30.319  < 2e-16 ***
#  day              -0.19021    0.02147  -8.860  < 2e-16 ***
#  pOC1sc           -1.05837    0.13594  -7.786 6.93e-15 ***
#  meanEVdiffPOSsc  -0.47944    0.49303  -0.972    0.331    
#  meanEVdiffNEGsc  -0.23689    0.26382  -0.898    0.369    

# signed mean EV diff: ns
# abs mean EV diff: ns


# MODEL 12A-B: DO CUMULATIVE EARNINGS INTERACT WITH PAST OUTCOME TO INFLUENCE RISK-TAKING (AS NOTED IN VIC). AND DOES LINEAR EXPECTATION INTERACT WITH PAST OUTCOME?
# note that this dataset is not set up to look at a piecewise linear expectation as done in part 1 

# summary of max cumulative earnings per day
# Day 1: mean = 252.3; median = 249; range = 155.4 - 379.2
# Day 2: mean = 235.1; mediam = 235.0; range = 138 - 319.5
# both days: mean = 243.7; median = 242.5; range = 138 - 379.2
# mean max earnings (243.7) is ~64% of max (379.2)

model12a_earnPOCintxn = glmer(choice~ 1+ gainSC + lossSC + safeSC + day + pOC1sc*earningSeshSC + linExpectation +(1|subID), data=pdmData, family = "binomial",control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=100000))); 
# #AIC = 13359.0
#                      Estimate   Std. Error   z value   Pr(>|z|)    
#   (Intercept)            0.38796    0.14514   2.673 0.007519 ** 
#   gainSC                 9.15671    0.27107  33.780  < 2e-16 ***
#   lossSC                12.30689    0.25084  49.062  < 2e-16 ***
#   safeSC               -18.77966    0.54702 -34.331  < 2e-16 ***
#   day                   -0.19030    0.02150  -8.852  < 2e-16 ***
#   pOC1sc                -1.42550    0.19633  -7.261 3.85e-13 ***
#   earningSeshSC          0.03045    0.01780   1.711 0.087082 .  
#   linExpectation        -0.62624    0.16214  -3.862 0.000112 ***
#   pOC1sc:earningSeshSC   0.12023    0.04116   2.921 0.003485 ** 


model12b_trialPOCintxn = glmer(choice~ 1+ gainSC + lossSC + safeSC + day + pOC1sc*earningSeshSC + linExpectation*pOC1sc +(1|subID), data=pdmData, family = "binomial",control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=100000))); 

# linear expectation is significant in previous  model, does it interact with outcomes too? Answer: no it doesnt, suggesting that linear expectation and cumulative earnings are separate things
# AIC = 13360.9
#                      Estimate   Std. Error   z value   Pr(>|z|)    
#(Intercept)            0.38386    0.14553   2.638 0.008349 ** 
#gainSC                 9.15997    0.27127  33.767  < 2e-16 ***
#lossSC                12.30689    0.25091  49.050  < 2e-16 ***
#safeSC               -18.78090    0.54713 -34.326  < 2e-16 ***
#day                   -0.19029    0.02150  -8.852  < 2e-16 ***
#pOC1sc                -1.37926    0.23106  -5.969 2.38e-09 ***
#earningSeshSC          0.02910    0.01816   1.602 0.109069    
#linExpectation        -0.60966    0.16798  -3.629 0.000284 ***
#pOC1sc:earningSeshSC   0.14040    0.06731   2.086 0.036985 *  
#pOC1sc:linExpectation  -0.24970    0.65936  -0.379 0.704911  

#MODEL 13: DOES PROPRANOLOL (ACCOUNTING FOR BMI) INTERACT WITH THREE LEVELS OF RISK-TAKING?

model13_pastOCearningsPropran = glmer(choice~ gainSC + lossSC + safeSC + day + pOC1sc*earningSeshSC*medicine*bmiGroup + linExpectation +(1|subID), data=pdmData, family = "binomial",control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=100000))); 
# AIC = 13341.1
#                                           Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                              0.364023   0.150512   2.419 0.015582 *  
# gainSC                                   9.192920   0.272029  33.794  < 2e-16 ***
# lossSC                                  12.365720   0.252102  49.050  < 2e-16 ***
# safeSC                                 -18.864489   0.549325 -34.341  < 2e-16 ***
# day                                     -0.212417   0.021984  -9.662  < 2e-16 ***
# pOC1sc                                  -1.589866   0.274212  -5.798 6.71e-09 ***
# earningSeshSC                            0.029982   0.019707   1.521 0.128163    
# medicine                                 0.063047   0.078753   0.801 0.423377    
# bmiGroup                                 0.015244   0.142458   0.107 0.914781    
# linExpectation                          -0.696657   0.163473  -4.262 2.03e-05 ***
# pOC1sc:earningSeshSC                     0.209629   0.058546   3.581 0.000343 ***
# pOC1sc:medicine                          0.257081   0.394042   0.652 0.514130    
# earningSeshSC:medicine                   0.018938   0.016930   1.119 0.263295    
# pOC1sc:bmiGroup                          0.119940   0.273291   0.439 0.660753    
# earningSeshSC:bmiGroup                  -0.001266   0.011922  -0.106 0.915415    
# medicine:bmiGroup                        0.095575   0.078957   1.210 0.226097    
# pOC1sc:earningSeshSC:medicine           -0.153465   0.082938  -1.850 0.064264 .  
# pOC1sc:earningSeshSC:bmiGroup           -0.101838   0.058430  -1.743 0.081352 .  
# pOC1sc:medicine:bmiGroup                 0.018403   0.394067   0.047 0.962752    
# earningSeshSC:medicine:bmiGroup          0.020518   0.016951   1.210 0.226095    
# pOC1sc:earningSeshSC:medicine:bmiGroup   0.087981   0.082941   1.061 0.288797 

# 1 = low BMI; -1 = high BMI
# propranolol = 1; placebo = 0

# MODEL 14A-B
# Does propranolol interact with difference between mean ev on trial t and the average positive and negative mean ev difference on previous 3 and 20 trials
model14a_meanEVdiff3trialsPropranolol= glmer(choice~ gainSC + lossSC + safeSC + day + pOC1sc*earningSeshSC+ meanEVdiff3tBackscPOS*bmiGroup*medicine + meanEVdiff3tBackscNEG*bmiGroup*medicine + linExpectation + (1|subID), data=pdmData, family = "binomial",control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=100000))); 
# AIC = 13141.6
#                                           Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                               0.31979    0.14993   2.133 0.032932 *  
# gainSC                                    9.17266    0.28799  31.850  < 2e-16 ***
# lossSC                                   12.16227    0.26614  45.699  < 2e-16 ***
# safeSC                                  -18.62511    0.66612 -27.960  < 2e-16 ***
# day                                      -0.20772    0.02209  -9.405  < 2e-16 ***
# pOC1sc                                   -1.45792    0.20026  -7.280 3.34e-13 ***
# earningSeshSC                             0.04394    0.01808   2.431 0.015068 *  
# meanEVdiff3tBackscPOS                    -0.34300    0.69602  -0.493 0.622153    
# bmiGroup                                  0.03603    0.13898   0.259 0.795458    
# medicine                                  0.14613    0.06262   2.334 0.019621 *  
# meanEVdiff3tBackscNEG                     1.13443    0.57330   1.979 0.047843 *  
# linExpectation                           -0.66121    0.16480  -4.012 6.01e-05 ***
# pOC1sc:earningSeshSC                      0.14121    0.04145   3.406 0.000659 ***
# meanEVdiff3tBackscPOS:bmiGroup           -0.48185    0.32673  -1.475 0.140275    
# meanEVdiff3tBackscPOS:medicine           -0.24482    0.46229  -0.530 0.596410    
# bmiGroup:medicine                         0.15744    0.06332   2.487 0.012900 *  
# bmiGroup:meanEVdiff3tBackscNEG            0.47714    0.56055   0.851 0.394662    
# medicine:meanEVdiff3tBackscNEG            0.79192    0.79372   0.998 0.318415    
# meanEVdiff3tBackscPOS:bmiGroup:medicine  -0.06650    0.46242  -0.144 0.885653    
# bmiGroup:medicine:meanEVdiff3tBackscNEG  -1.12779    0.79403  -1.420 0.155509    


model14b_meanEVdiff20trialsPropranolol=glmer(choice~ gainSC + lossSC + safeSC + day + pOC1sc*earningSeshSC+ meanEVdiff20tBackscPOS*bmiGroup*medicine+ meanEVdiff20tBackscNEG*bmiGroup*medicine + linExpectation+ (1|subID), data=pdmData, family = "binomial",control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=100000))); 
# AIC = 11631.1
#                                             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                0.34547    0.15970   2.163 0.030515 *  
# gainSC                                     8.54938    0.36129  23.663  < 2e-16 ***
# lossSC                                    11.02754    0.33151  33.264  < 2e-16 ***
# safeSC                                   -16.60003    1.02157 -16.250  < 2e-16 ***
# day                                       -0.19428    0.02348  -8.273  < 2e-16 ***
# pOC1sc                                    -1.56783    0.23664  -6.625 3.47e-11 ***
# earningSeshSC                              0.06780    0.01911   3.548 0.000389 ***
# meanEVdiff20tBackscPOS                    -1.22627    1.34166  -0.914 0.360722    
# bmiGroup                                  -0.02482    0.14353  -0.173 0.862730    
# medicine                                   0.17063    0.07710   2.213 0.026899 *  
# meanEVdiff20tBackscNEG                     5.19937    1.21197   4.290 1.79e-05 ***
# linExpectation                            -0.77842    0.17960  -4.334 1.46e-05 ***
# pOC1sc:earningSeshSC                       0.13911    0.04614   3.015 0.002568 ** 
# meanEVdiff20tBackscPOS:bmiGroup           -0.15210    0.35312  -0.431 0.666668    
# meanEVdiff20tBackscPOS:medicine           -0.47288    0.49992  -0.946 0.344188    
# bmiGroup:medicine                          0.16468    0.07785   2.115 0.034395 *  
# bmiGroup:meanEVdiff20tBackscNEG           -1.11613    1.07983  -1.034 0.301315    
# medicine:meanEVdiff20tBackscNEG            2.03217    1.53300   1.326 0.184966    
# meanEVdiff20tBackscPOS:bmiGroup:medicine  -0.07082    0.49993  -0.142 0.887348    
# bmiGroup:medicine:meanEVdiff20tBackscNEG  -1.20454    1.53297  -0.786 0.432010  


# Figures for manuscript

#Figure 5d: effect size of past outcome in part 2

pastOutcomeResults = summary(model10_pastOutcome);
poc = seq(from=min(pdmData$pOC1sc, na.rm=T), to = max(pdmData$pOC1sc, na.rm=T), by = .3); # values are scaled here
pdmPOCgam = 1/(1+exp(-1*(pastOutcomeResults$coefficients[6]*poc)));  # from the model where past outcome is alone 

pdf("/Volumes/shlab/Projects/PDM/code/figures/manuscript_PDM_fig3d_pocES.pdf")
par(mar=c(5,6,6,3));#change margin so ylab is not cut off
plot(pdmPOCgam, type = "l",axes = FALSE, xaxt="n", ann=F, lwd = 4, cex.lab=1, ylim=c(0,1))
abline(a=.5,b=0, col="grey", lty=1, lwd=3)
title(ylab = "p(gamble)", line = 3.75, cex.lab=1.35)
title(xlab = "Past outcome", line = 2.5, cex.lab=1.35)
title(main = sprintf("Risk-taking following outcomes \npast outcome: %.2f(%.2f), p=%f", pastOutcomeResults$coefficients[6],pastOutcomeResults$coefficients[12], pastOutcomeResults$coefficients[24]))
poclab = c("-$24", "-$15","-$6","+$3","+$12" ,"+$21", "+$30")
axis(1, at = c(1:7), labels =poclab, tick =T, cex.axis = 1.25, cex=2, lwd = 4)
axis(2, at = c(0,round(pdmPOCgam[2], digits=2),round(pdmPOCgam[6], digits=2),1), tick = T, las =2, cex.axis = 1.25, cex=2, lwd=4)
lines(c(2,2), c(0,pdmPOCgam[2]), lty = 4, col="red", lwd = 4);#vertical line at poc $14
lines(c(1,2),c(pdmPOCgam[2],pdmPOCgam[2]), lty=4, col="red", lwd =4); #horizontal line 
lines(c(6,6), c(0,pdmPOCgam[6]), lty = 5, col="darkgreen", lwd = 4);
lines(c(1,6),c(pdmPOCgam[6],pdmPOCgam[6]), lty=5, col="darkgreen", lwd =4); 
lines(pdmPOCgam, lwd=4); #plotting the line again so its on top.
dev.off()




#Figure 3e: effect size of negative mean ev difference (3 trials)

meanEV3trialsResults = summary(model11a_meanEVdiff3trials)
meanEV = seq(from=min(pdmData$meanEVdiff3tBackscNEG, na.rm=T), to = max(pdmData$meanEVdiff3tBackscNEG, na.rm=T), length.out=7); # values are scaled here
pdmMeanEVgam = 1/(1+exp(-1*(meanEV3trialsResults$coefficients[7]*meanEV)))  # from the model where past outcome is alone 

pdf("/Volumes/shlab/Projects/PDM/code/figures/manuscript_PDM_fig3e_meanEVes.pdf")
par(mar=c(5,6,6,3));#change margin so ylab is not cut off
plot(pdmMeanEVgam, type = "l",axes = FALSE, xaxt="n", ann=F, lwd = 4, cex.lab=1, ylim=c(0,1));
abline(a=.5,b=0, col="grey", lty=1, lwd=3);
title(ylab = "p(gamble)", line = 3.75, cex.lab=1.35)
title(xlab = "Difference in mean EV ($) \n Previous 3 trials", line = 3.5, cex.lab=1.35)
title(main = sprintf("Risk-taking following change in mean EV 3 trials back \nmean EV: %.2f(%.2f), p=%f", meanEV3trialsResults$coefficients[7],meanEV3trialsResults$coefficients[15], meanEV3trialsResults$coefficients[31]))
meanEVlab = c("-10.4", "-8.7","-6.9","-5.2","-3.5" ,"-1.7", "0")
axis(1, at = c(1:7), labels =meanEVlab, tick =T, cex.axis = 1.25, cex=2, lwd = 4)
axis(2, at = c(0,round(pdmMeanEVgam[2], digits=2),round(pdmMeanEVgam[6],digits=2),1), tick = T, las =2, cex.axis = 1.25, cex=2, lwd=4);
lines(c(2,2), c(0,pdmMeanEVgam[2]), lty = 4, col="red", lwd = 4);#vertical line at poc $14
lines(c(1,2),c(pdmMeanEVgam[2],pdmMeanEVgam[2]), lty=4, col="red", lwd =4); #horizontal line 
lines(c(6,6), c(0,pdmMeanEVgam[6]), lty = 5, col="darkgreen", lwd = 4);
lines(c(1,6),c(pdmMeanEVgam[6],pdmMeanEVgam[6]), lty=5, col="darkgreen", lwd =4); 
lines(pdmMeanEVgam, lwd=4); #plotting the line again so its on top.
dev.off();

#Figure 3f: effect size of earning x outcome interaction
# Pick a time in the task, so .5
# what happens when cumulative earnings are less (.4), same (.5) and more (.6) than expected

# max cumulative earnings is 379.99 but the average max earning is 243.70
potcEarnResults = summary(model12a_earnPOCintxn);
pocBeta = potcEarnResults$coefficients[6];
earnBeta = potcEarnResults$coefficients[7];
expBeta = potcEarnResults$coefficients[8]
potcEarnBeta = potcEarnResults$coefficients[9];

# earnings are scaled from -1.8 to 8.1, median = 3.5
# trial is 0 to 1, median = .5
earnings = c(1.5, 3.5, 6.1); # below, at, and above expecations (1Q, median, 3Q)
linExp = .5; # median
#earnings = seq(from=-1.821, to=8.123, length.out=length(poc));# mean max earnings (243.7 or 8.123 scaled) is mean max cumulative earnings; overall max = 379.9 or 12.67 scaled.


#earnings less than expected:
lessEarnGam = 1/(1+exp(-1*(pocBeta*poc + earnBeta*earnings[1]+ potcEarnBeta*earnings[1]*poc + expBeta*linExp)))

#earnings equal expectations
equalEarnGam = 1/(1+exp(-1*(pocBeta*poc + earnBeta*earnings[2]+ potcEarnBeta*earnings[2]*poc + expBeta*linExp)))

#earnings more than expectations
moreEarnGam= 1/(1+exp(-1*(pocBeta*poc + earnBeta*earnings[3]+ potcEarnBeta*earnings[3]*poc + + expBeta*linExp)))

pdf("/Volumes/shlab/Projects/PDM/code/figures/manuscript_PDM_fig3f_pocEarnES.pdf")
par(mar=c(5,6,6,3));#change margin so ylab is not cut off
plot(lessEarnGam, type="l", ylim = c(0,1),axes = FALSE, xaxt="n", ann=F, lwd = 6, cex.lab=1, col="black", lty="twodash")
lines(equalEarnGam, lwd=6, col="gray33", lty="dashed")
lines(moreEarnGam, lwd=6, col ="gray60", lty="dotted")
title(ylab = "p(gamble)", line = 3.75, cex.lab=1.35, cex=2)
title(xlab = "Past outcome", line = 2.5, cex.lab=1.35, cex=2)
title(main = sprintf("Risk-taking following earnings & outcome \niteraction: %.2f(%.2f), p=%f", potcEarnBeta ,potcEarnResults$coefficients[8] ,potcEarnResults$coefficients[16]))
poclab = c("-$24", "-$15","-$6","+$3","+$12" ,"+$21", "+$30")
axis(1, at = c(1:7), labels =poclab, tick =T, cex.axis = 1.25, cex=2, lwd = 4)
axis(2, at = c(0.00, 0.5, 1.00), tick = T, las =2,las=2, cex.axis = 1.25, cex=2, lwd=4)
dev.off()



