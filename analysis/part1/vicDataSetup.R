# VIC (value in context) data analysis 
# importing, subsetting data and simple stats
# this script also runs model 1 (trial level) and save predicted values to dataset
# 03/01/18 HR
# updated 7/22/21 
# N = 62
rm(list = ls()); #clear global environment

#load packages
library('lme4');
library('lmerTest');

# Functions:
  # for finding the standard error of the mean:
    sem <- function(x) sd(x)/sqrt(length(x));


# Load data:
vicDataB = read.csv('/Volumes/shlab/Projects/VIC/data analysis/vicData.csv', header = TRUE); 
#vicDataB (B for behavioral data)
# 14124 rows with 12 variables

# Number of participants
nSub = max(unique(vicDataB$subjectIndex));

#Missed trials
NaNind = which(is.nan(vicDataB$choice)); #index missed trials and save to use in SCR data as well.
save(NaNind, file= "/Volumes/shlab/Projects/VIC/data analysis/behNaNs.Rdata");

totNan = sum(is.nan(vicDataB$choice)); #120 nans where ps did not respond in time
subsWhoMissed = unique(vicDataB$subjectIndex[NaNind]); # subs who missed trials
missedTs = vector()

for (s in 1:nSub){
  missedTs[s] = sum(vicDataB$subjectIndex[NaNind] == s); # total # of trials missed by participant
} #median missed = 1, mean missed = 1.935

hist(missedTs); # display the number of missed trials pers person
#' 3 participants missed 10 or more of 240 trials (p 16 missed 18, p29 missed 10, p58 missed 32 trials). 
#' 26 people missed no trials 
#' 20 people missed 1 trial
#' 5 people missed 2 trials
#' 5 people missed 3 trials
#' 2 people missed 4 trials
#' 1 person missed 7 trials

# REMOVE MISSED TRIALS
vicDataB = vicDataB[which(!is.nan(vicDataB$choice)),]; #store only those rows with no NaNs

#how many trials does each person have now that we have cleaned the data?
T1 = c(1,which(diff(vicDataB$subjectIndex)==1) + 1); #which row in vicDataB is each participants 1st trial?
nTxSub = vector(length=nSub); # to store each participants number of trials
for (n in 1:nSub){
  nTxSub[n] = sum(vicDataB$subjectIndex==n)
}

##### Reaction Times #######
# there is a 2s forced viewing
# range = 0.0000808s to 1.9921336s
# mean = 0.4257686
# median = 0.3472703
# variance =  0.07602492

RTlist = list();
for (n in 1:nSub){
  RTlist[[n]] = vicDataB$RT[vicDataB$subjectIndex==n]
};

#plot each participants RTs
# most participants' means are around the group mean
boxplot(RTlist); 

# 3) Add age and gender to entire dataset
genAge<- read.csv("/Volumes/shlab/Projects/VIC/data/VICageGender.csv", header=TRUE);#load age and gender data

gamtx = matrix(nrow=nrow(vicDataB), ncol = 3, dimnames =list(c(NULL), c("subjInd", "gender","age"))); 
gamtx= as.data.frame(gamtx); #make gamtx into data frame
tmpvec <-as.vector(unique(vicDataB$subjectIndex));
for (i in 1:length(tmpvec)){
  gamtx$subjInd[vicDataB$subjectIndex==tmpvec[i]] <- genAge$subID[i]
  gamtx$gender[vicDataB$subjectIndex==tmpvec[i]] <- genAge$gender[i]
  gamtx$age[vicDataB$subjectIndex==tmpvec[i]] <- genAge$age[i]
};

vicDataB$age <- gamtx$age; # add age to vicDataB
vicDataB$gender <- gamtx$gender; # add gender to vicDataB (1=female, 2 = male)

#add variable where choices are coded -1 (safe) and 1 (gamble) 
vicDataB$choice1 <- vicDataB$choice
vicDataB$choice1[vicDataB$choice1==0] = -1; #change 0s to -1 for safe, keep 1 for gamble taken

# Mean EV of the option (both safe and gamble)
# safe x probability (=1) + (risky gain x .5 + risky loss x .5) / 2
# simplified: (safe + (riskygain x .5))/2
vicDataB$meanEVOption = (vicDataB$alternative + (.5*vicDataB$riskyGain))/2; # calculate mean EV of the option
vicDataB$EVdiff = vicDataB$EV - vicDataB$alternative; # calculate EV difference, ev gamble - alternative


# to help GLMER models run smoother, we will scaled a lot of the monetary amounts 
# use the same thing to scale = max risky gain amount
scaleBy = max(vicDataB$riskyGain); 

# scale outcomes
vicDataB$outcomeSC = vicDataB$outcome/scaleBy; 

#Cumulative earnings 
#   what participants have made up until trial t

first = 1; #starting row number
allcumEarnings = vector(); #vector for storing values for all ps
trinum = vector(); # to store trial number
indivMaxEarn = vector(mode = "numeric",length=nSub); # to store individual max cumulative earnings

for(s in 1:nSub){
  sub = vicDataB[vicDataB$subjectIndex==s,]; #pull out subject
  cumEarnings = vector(); #empty vector to store values for one subject
  tri = 1:nrow(sub)/nrow(sub); # normalize each participant's trial by the max number of trials
  for(t in 1:nrow(sub)){ #for each trial
    cumEarnings[t] = sum(sub$outcome[1:t]); #calculate total outcomes 
  }
  cumEarnings = c(0,cumEarnings[1:length(cumEarnings)-1]); # put zero in first trial, get rid of last trial
  indivMaxEarn[s] = max(cumEarnings);
  last = first+nrow(sub)-1; #last row to store in allcumEarnings
  allcumEarnings[first:last] = cumEarnings; # store subjects cumulative earnings
  trinum[first:last] = tri
  first = last +1; #set first to be the row after the previous subjects last trial
};

vicDataB$trial = trinum; # store trial number which is 0 to 1
vicDataB$cumEarning = allcumEarnings; #store cumulative earnings in vicDataB
vicDataB$cumEarningSC = vicDataB$cumEarning/scaleBy ; #scaled cumulative earnings by riskyGain amount
vicDataB$cumEarningsSC01 = vicDataB$cumEarning/max(vicDataB$cumEarning); # scale cumulative earnings by max cumulative earnings so that its 0-1

summary(indivMaxEarn); # range = 2975 - 5636; mean = 4229; median = 4152 

earnXsub = matrix(data=NA, nrow=nSub, ncol = 2);
for(s in 1:nSub){
  earnXsub[s,1] = max(vicDataB$cumEarning[vicDataB$subjectIndex==s])
  earnXsub[s,2] = max(vicDataB$cumEarningSC[vicDataB$subjectIndex==s])
};


# scaling values to be between 0 and 1
vicDataB$gainSC = vicDataB$riskyGain/scaleBy; # scale gains
vicDataB$lossSC = vicDataB$riskyLoss/scaleBy; #scale losses - although they are zero
vicDataB$altSC = vicDataB$alternative/scaleBy; # scale alternatives


# Linear and piecewise linear expectation terms:
vicDataB$linExpectation = vicDataB$trial; # linear expectation term is captured by trial because it is increasing over the task (and its normalized 0 to 1 which makes it coparable to cumulative earnings)

# piecewise linear expectaiton term is basically a level tracking variable (adding up EV level over time)
subDiff = c(0,diff(vicDataB$subjectIndex));
levelTracking = vector(); # level tracking variable
levelTrackingNorm = vector(); # for normalized variable
for (s in 1:nSub) {
  sub = vicDataB[vicDataB$subjectIndex==s,];
  tmpvec = cumsum(sub$groundEV); # cumulative the sum of each increasing row
  tmpvecNorm = tmpvec/max(tmpvec); # normalize by max level tracking value
  levelTracking = c(levelTracking, tmpvec);
  levelTrackingNorm = c(levelTrackingNorm, tmpvecNorm);
};

# add to vicDataB dataset
vicDataB$levelTracking = levelTracking; # piecewise linear - true value (not normalized)
vicDataB$levelTrackingNorm = levelTrackingNorm; # piecewise linear normalized by each participants max value
vicDataB$levelTrackingNormOverall = levelTracking/max(levelTracking); #piecewise linear normalized by max level tracking value across all participants 

## calculating change in mean EV options from 1 trial back and putting NaN for the first trial for each person
EVchanges = vector(length = nrow(vicDataB)-1)
for(i in 2:nrow(vicDataB)){
  EVchanges[i] = vicDataB$meanEVOption[i] - vicDataB$meanEVOption[i-1];
};

vicDataB$meanEVchange = EVchanges; # store mean EV changes
newmtx = matrix(data=NA,nrow = nrow(vicDataB), ncol = 2); # create new matrix
newmtx[,1] = vicDataB$meanEVchange; # store mean EV change in first column of matrix
newmtx[1,1] <- NaN #put Nan in for first row (first trial for subject 1, bc there is no past trial)
newvec <-diff(vicDataB$subjectIndex); # differenecs between new subject index
newmtx[,2]<-c(0,newvec); # store new vec in newmatx
newmtx[newmtx[,2] == 1,] = NaN; #put nan for first trial for each person
vicDataB$meanEVchange = newmtx[,1]; #put back into vicDataB

## calculating change in mean EV options from 2 trial backs and putting NaN for the first two trials for each person
EVchanges2 = vector(length = nrow(vicDataB)-2)
for(i in 3:nrow(vicDataB)){
  EVchanges2[i] = vicDataB$meanEVOption[i] - vicDataB$meanEVOption[i-2];
};

EVchanges3 = vector(length = nrow(vicDataB)-2)
for(i in 4:nrow(vicDataB)){
  EVchanges3[i] = vicDataB$meanEVOption[i] - vicDataB$meanEVOption[i-3];
};


vicDataB$meanEVchange2 = EVchanges2; # store mean EV changes
vicDataB$meanEVchange3 = EVchanges3; # store mean EV changes
allmeanEVchange2 = vector();
allmeanEVchange3 = vector();

first = 1; #starting row number

for(s in 1:nSub){
  sub = vicDataB[vicDataB$subjectIndex==s,]
  sub$meanEVchange2[1:2] <-NaN; #put nan in first two spots
  sub$meanEVchange3[1:3] <-NaN; #put nan in first three spots
  last=first+nrow(sub)-1; #last row to store in meanEVchange2
  allmeanEVchange2[first:last] = sub$meanEVchange2
  allmeanEVchange3[first:last] = sub$meanEVchange3
  first = last+1
};


vicDataB$meanEVchange2 = allmeanEVchange2;#store back in meanEVchange2, now with two nans for first two trials for each person
vicDataB$meanEVchange3 = allmeanEVchange3;#store back in meanEVchange3, now with two nans for first two trials for each person


#-------SUBSETTING DATA TO SEE PAST TRIAL EFFECTS------#

# FX that shifts rows down to be replaced by NaN
pastTrials <-function(data,startShift,oneBcomes,rowNaN){ #oneBcomes: first row pushed to down to which row? rowNaN: which rows will have nans
  newmtx <- data[,startShift:ncol(data)]; #take data from columns 
  data[,startShift:ncol(data)]<-NULL; #remove those columns completly from vicDataB
  newmtx[oneBcomes:nrow(data),] <- newmtx[rowNaN:(nrow(data)-1),]; # removes first row, shifts everything up
  newmtx[rowNaN,] <- NaN #put Nan in for first row (first trial for subject 1, bc there is no past trial)
  newvector<-diff(data$subjectIndex); #put differences between NewSubjectIndex into newvector, 1s show up when subject changes
  newmtx$diff<-append(newvector,0,1); #put new vector into newmtx
  newmtx[newmtx$diff==1,]=NaN; #in newmtx, when diff = 1, replace with NaN, this replaces first trial with nan
  data = data.frame(data,newmtx); #merge data frames back together
  return(data) #return the dataframe
};


# Set up variables:
  # this part just saves the variables before they are shifted to account for one trial back
vicDataB$pastMEVoption = vicDataB$meanEVOption; # mean ev of the option one trial back  
vicDataB$pastOutcome = vicDataB$outcome;        # outcome one trial back
vicDataB$pastChoice = vicDataB$choice1;         # choice one trial back

vicDataB$pastriskyG = vicDataB$riskyGain;       # risky gain one trial back
vicDataB$pastAlt = vicDataB$alternative;        # safe one trial back

vicDataB$pastChoiceOC = vicDataB$choice1;       # past decision and outcome
vicDataB$pastChoiceOC[vicDataB$pastChoiceOC == -1] = 0; #if past choice was no gamble (-1), make 0
vicDataB$pastChoiceOC[vicDataB$pastChoiceOC == 1 & vicDataB$outcome == 0] = -1 ; #if past choice was gamble and not won, make -1


columns = colnames(vicDataB); #save column names

startShift = which(columns == "pastMEVoption"); # index the first column to start shifting rows 1 down
#now shift:
vicDataB = pastTrials(vicDataB,startShift,2,1); #inargs = (1)data, (2)see line above, (3)row 1 moves down to which row, (4)how many NaNs

# TWO TRIALS BACK
  # prep variables
vicDataB$pastMEVoption2 = vicDataB$pastMEVoption; # mean ev option two trials back
vicDataB$pastOutcome2 = vicDataB$pastOutcome;     # outcome two trials back
vicDataB$pastChoice2 = vicDataB$pastChoice;       # choice two trials back
vicDataB$pastriskyG2 = vicDataB$pastriskyG;       # risky gain two trials back
vicDataB$pastAlt2 = vicDataB$pastAlt;             # safe two trials back


columns = colnames(vicDataB); #save column names
startShift = which(columns == "pastMEVoption2"); # index the first column to start shifting rows 1 down

#now shift
vicDataB = pastTrials(vicDataB,startShift,3,2); #inargs = (1)data, (2)see line above, (3)row 1 moves down to which row, (4)how many NaNs


#3 trials back
vicDataB$pastMEVoption3 = vicDataB$pastMEVoption2;  # mean ev option two trials back
vicDataB$pastOutcome3 = vicDataB$pastOutcome2;      # outcome two trials back
vicDataB$pastChoice3 = vicDataB$pastChoice2;        # choice two trials back
vicDataB$pastriskyG3 = vicDataB$pastriskyG2;        # risky gain two trials back
vicDataB$pastAlt3 = vicDataB$pastAlt2;              # safe two trials back

columns = colnames(vicDataB); #save column names
startShift = which(columns == "pastMEVoption3"); # index the first column to start shifting rows 1 down

#now shift
vicDataB = pastTrials(vicDataB,startShift,4,3); #inargs = (1)data, (2)see line above, (3)row 1 moves down to which row, how many NaNs


vicDataB$pastOutcomeWeighted = (vicDataB$pastOutcome + .25* vicDataB$pastOutcome2 + .125*vicDataB$pastOutcome3)/3;  # combine past 3 outcomes

#scaling more variables:
vicDataB$pastOC1sc <- vicDataB$pastOutcome/scaleBy;
vicDataB$pastOC2sc <- vicDataB$pastOutcome2/scaleBy;
vicDataB$pastOC3sc <- vicDataB$pastOutcome3/scaleBy;
vicDataB$pastOCweightSC <- vicDataB$pastOutcomeWeighted/scaleBy;


vicDataB$pastMEVoptionSC = vicDataB$pastMEVoption/scaleBy;
vicDataB$pastMEVoption2SC = vicDataB$pastMEVoption2/scaleBy;
vicDataB$pastMEVoption3SC = vicDataB$pastMEVoption3/scaleBy;

vicDataB$pocRiskyGAmt = vicDataB$pastOutcome*((vicDataB$pastChoice==1)&(vicDataB$pastOutcome > 0))/scaleBy;
vicDataB$pocAltAmt = vicDataB$pastOutcome*(vicDataB$pastChoice==-1)/scaleBy;

vicDataB$pocRiskyGType = as.numeric(vicDataB$pocRiskyGAmt > 0);
vicDataB$pocRiskyLType = as.numeric(vicDataB$pastOutcome == 0);
vicDataB$pocAltType = as.numeric(vicDataB$pastChoice==-1);

#make a new regressor for loss that is the gain that is not received 
vicDataB$missedGain = vicDataB$pastriskyG*((vicDataB$pastChoice== 1) & (vicDataB$pastOutcome== 0));
vicDataB$missedGain = vicDataB$missedGain/scaleBy;

vicDataB$groundEVsc = vicDataB$groundEV/scaleBy;
vicDataB$EVdiffsc = vicDataB$EVdiff/scaleBy; #scale ev diff


#------------ PLOT EACH PERSON'S CHOICES ------------#
#create function to do it:
# input args: (1)dataframe to pull subject's choices (2)subject number
plotVPcs<- function(data,subnum){ #fx name = plot vic pilot choice set
  subjectn = vicDataB[vicDataB$subjectIndex==subnum,]; # pull out subject data
  maxAlt = max(subjectn$alternative); #max alternative value for this subject
  subP = round(mean(subjectn$choice),digits = 2); #find the probabilit of gambling
  ind0 = which(subjectn$outcome==0); #which outcomes were zeroes (gamble taken and not won)
  par(mfrow = c(1,1), mai = c(1,1,1,1))
  plot(subjectn$alternative, col = subjectn$choice/.55+2, main = sprintf("Choice Set for s%g \n probability of gambling = %g", subnum, subP), xlab="Trial", ylab =
       sprintf("Alternatives ($)\n max: $%g", maxAlt), ylim = c(0,45), cex.axis=1, cex.main = 1, cex.lab = 1); #plot alternative, gambles taken are in green
  points(subjectn$groundEV, col = "black", pch = "-"); # plot ground EV
  points(ind0, subjectn$alternative[ind0], col = "darkgreen", pch = 1);#fill in points where gamble was taken and not won
  legend(1, 45, legend=c("Gamble taken","Outcome $0", "Alternative taken", "Ground EV"), col=c("green","darkgreen", "red", "black"), pch = c(1,1,1,NA), lty = c(0,0,0,1), cex=1, ncol=1, bty="n")#, horiz = "TRUE");
};

# NOW PLOT CHOICES, CALCULATE/STORE PROBABILITIES
par(mfrow=c(1,1))
pdf(file = "/Volumes/shlab/Projects/VIC/data analysis/figures/choices.pdf")
subP = vector();
for(p in 1:nSub){
  plotVPcs(vicDataB,p)
  subjectn = vicDataB[vicDataB$subjectIndex==p,]
  subP[p] = round(mean(subjectn$choice), digits = 3)
};
dev.off();

#plot participants choices across different levels of gains and alternatives 
pdf(file = "/Volumes/shlab/Projects/VIC/data analysis/figures/choicesAcrossValues.pdf")
for(p in 1:nSub){
  sub1 = vicDataB[vicDataB$subjectIndex ==p,]
  plot(sub1$riskyGain, sub1$alternative, col=sub1$choice/.55+2, main = sprintf("choices across gain and safe options for s%g \n pgamble =%g", p, subP[p]), xlab = "gains", ylab = "alternatives", ylim = c(0,35), xlim = c(0,70))
};
dev.off();

# looking at point of indifference for various rhos
yvals = seq(from=0,to=70,by=10);#  gain values
rho = 1.5; #risk seeking
yvalsSeeking = (0.5*(yvals^rho))^(1/rho); # when the utility of the y value is equal to utility of x value
rho = 0.5; #risk averse
yvalsAverse = (0.5*(yvals^rho))^(1/rho)
plot(yvals*.5,yvals,type='l'); #risk neutral
lines(yvals,yvals,type='l', col ="red"); # the limit; people will never gamble here
lines(yvalsSeeking,yvals,col='green')
lines(yvalsAverse,yvals,col='blue')
points(vicDataB$alternative[vicDataB$subjectIndex==1],vicDataB$riskyGain[vicDataB$subjectIndex==1]); # plot VIC gains x safe

meanProb = round(mean(vicDataB$choice), digits = 3); #probability of gambling across the group, N=62 p(gam) = .47
semProb = round(sem(subP),digits = 3); # sem across the group, N = 62, sem(p(gam)) = .022

#box plot of probability of gambling
par(mfrow = c(1,1))
pdf(file="/Volumes/shlab/Projects/VIC/data analysis/figures/pgamble.pdf") # save pdf file
boxplot(subP, main = sprintf("Probability of gambling (N=%d) \n mean(SEM) = %g(%g)", nSub, meanProb, semProb));
dev.off();

#     DOES MAGNITUDE INFLUENCE PROBABILITY OF GAMBLING?
#1. GLM
MagProbglm = glm(choice ~ riskyGain, data = vicDataB, family = binomial); # N=62; yes, risky gain values positively influence gambling p = .00006; beta =.005(.001)

# FX that shifts rows down to be replaced by NaN
pastTrials <-function(data,startShift,oneBcomes,rowNaN){ #oneBcomes: first row pushed to down to which row? rowNaN: which rows will have nans
  newmtx <- data[,startShift:ncol(data)]; #take data from columns 
  data[,startShift:ncol(data)]<-NULL; #remove those columns completly from vicDataB
  newmtx[oneBcomes:nrow(data),] <- newmtx[rowNaN:(nrow(data)-1),]; # removes first row, shifts everything up
  newmtx[rowNaN,] <- NaN #put Nan in for first row (first trial for subject 1, bc there is no past trial)
  newvector<-diff(data$subjectIndex); #put differences between NewSubjectIndex into newvector, 1s show up when subject changes
  newmtx$diff<-append(newvector,0,1); #put new vector into newmtx
  newmtx[newmtx$diff==1,]=NaN; #in newmtx, when diff = 1, replace with NaN, this replaces first trial with nan
  data = data.frame(data,newmtx); #merge data frames back together
  return(data) #return the dataframe
};

# CREATE SOME VARIABLES FOR THE FOLLOWING ANALYSIS:
#vicDataB$shiftDiff = c(0,diff(vicDataB$shift)); #find difference between the shifts - THIS WAY IS NOT NECESSARILY ACCURATE BECUASE IT SHOWS THE MAX SHIFT DIFFERENCE AS BIGGER (28.75) THAN IT ACTUALLY IS ($15)

vicDataB$shiftDiff = c(0,diff(vicDataB$groundEV)); # pulling out the difference in ground EV will get the accurate shift amount - still need to replace each person's first trial with zero otherwise it counts the difference in ground EV across people

# replace first trial for each person with zero
newmtx = matrix(data=NA,nrow = nrow(vicDataB), ncol = 2); # create new matrix
newmtx[,1] = vicDataB$shiftDiff ; # store mean EV change in first column of matrix
newmtx[1,1] <- 0 #put Nan in for first row (first trial for subject 1, bc there is no past trial)
newvec <-diff(vicDataB$subjectIndex); # differenecs between new subject index
newmtx[,2]<-c(0,newvec); # store new vec in newmatx
newmtx[newmtx[,2] == 1,] = 0; #put nan for first trial for each person
vicDataB$shiftDiff = newmtx[,1]; #put back into vicDataB

# NOW SHIFT DIFF ACCURATELY CAPTURES THE SHIFTS
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -15.00000   0.00000   0.00000   0.07924   0.00000  15.00000 

scaleBy = max(vicDataB$riskyGain);

vicDataB$shiftDiffsc = vicDataB$shiftDiff/scaleBy; # scale by max(riskyGain) as we have been doing with all the other variables
vicDataB$shiftDiffscABs = abs(vicDataB$shiftDiffsc); # absolute value of differences in shifts

vicDataB$shiftDiffscPOS = vicDataB$shiftDiffsc;
vicDataB$shiftDiffscPOS[vicDataB$shiftDiffsc < 0] = 0; # 0 for a negative shifts, positive shifts from 0-1 because scaled

vicDataB$shiftDiffscPOSInd = vicDataB$shiftDiffscPOS; 
vicDataB$shiftDiffscPOSInd[vicDataB$shiftDiffscPOSInd > 0] = 1; # 1 for positive shift, 0 for negative shift

vicDataB$shiftDiffscNEG = vicDataB$shiftDiffsc;
vicDataB$shiftDiffscNEG[vicDataB$shiftDiffsc > 0] = 0; # 0 for positive shift, negative shift from -1 to 0

vicDataB$shiftDiffscNEGind = vicDataB$shiftDiffscNEG; 
vicDataB$shiftDiffscNEGind[vicDataB$shiftDiffscNEG < 0] = 1; # 1 for negative shift, 0 for positive shift

vicDataB$runSizeAdjust = c(0,vicDataB$runlength[-c(nrow(vicDataB))])
vicDataB$runSizePOSind = vicDataB$runSizeAdjust*vicDataB$shiftDiffscPOSInd; # how long was the run proceeding a positive shift?

vicDataB$runSizeNEGind = vicDataB$runSizeAdjust*vicDataB$shiftDiffscNEGind; # how long was the run proceeding a positive shift?


# create recent event variables for shift analysis:

vicDataB$shiftDiffsc1 = vicDataB$shiftDiffsc;
vicDataB$shiftDiffscAbs1 = vicDataB$shiftDiffscAbs;
vicDataB$shiftDiffscPOS1 = vicDataB$shiftDiffscPOS;
vicDataB$shiftDiffscPOSInd1 = vicDataB$shiftDiffscPOSInd;
vicDataB$shiftDiffscNEG1 = vicDataB$shiftDiffscNEG;


columns = colnames(vicDataB); #save column names
startShift = which(columns == "shiftDiffsc1"); # index the first column to start shifting rows 1 down
#now shift:
vicDataB = pastTrials(vicDataB,startShift,2,1); #inargs = (1)data, (2)see line above, (3)row 1 moves down to which row or how many NaNs

vicDataB$absPOC2minusPOC1 = abs(vicDataB$pastOC2sc - vicDataB$pastOC1sc);
vicDataB$truePOC2minusPOC1 = vicDataB$pastOC2sc - vicDataB$pastOC1sc;



# Run our trial-level model so that we can save pred values and then use them in downstream datasets (e.g. SCR following outcome and SCR during decision phase)

# Model 1: Current-trial regression

model1_currTrial = glmer(choice ~ 0+gainSC + altSC+ groundEVsc + (0+gainSC + altSC| subjectIndex), data = vicDataB,   family='binomial', control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)));

# Extract the predicted values from this regression PRIOR to going through the softmax (type='link'). That means that 1/(1+exp(-1*(THIS))) will give the response values which we know are in probability space.
vicDataB$pred = predict(model1_currTrial,type='link'); # these are already saved in dataframe.
# predicted value summary stats: mean= -.203, median = -.05, range = -8.68-7.16




# save the cleaned data
save(vicDataB, file="/Volumes/shlab/Projects/VIC/data analysis/vicDataB.Rdata")


