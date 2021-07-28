# Setting up data for analysis (part 2 of VIC analysis)
# Reanalysis of Sokol-Hessner et al (2015) examining context effects
# This script loads the PDM data that was collated using the collateData.m file, combines data with demographic info, creates past event variables, and other important variables like cumulative earnings
# HB 05/03/19

# Clear environment
rm(list=ls());

#Load data
alldata = read.csv("/Volumes/shlab/Projects/PDM/data/alldata.csv", header = FALSE); # behavior + propranolol data
alldata = alldata[,-6]; #remove column of NANs
colnames(alldata) = c('subID', 'session', 'alternative', 'riskyLoss', 'riskyGain', 'possibleOC', 'choice', 'rt');


demoData = read.csv("/Volumes/shlab/Projects/PDM/data/demoData.csv", header = FALSE); #load demographic data
ageGen = read.csv("/Volumes/shlab/Projects/PDM/data/PDM_Gender+Age.csv", header=FALSE); #load demographic data

# subject numbers
subIDs = unique(alldata$subID); # subject ID numbers
numSub = length(subIDs); # 47 participants


# Missed trials
nanInd = which(is.nan(alldata$choice)); # index missed trials
subsMissed = unique(alldata$subID[nanInd]); # which subs missed trials
length(subsMissed);  # 31 participants missed atleast one trial

missedTs = vector()

for (s in 1:numSub){
  missedTs[s] = sum(alldata$subID[nanInd] == subIDs[s]); # total # of trials missed by participant
} #median missed = 1, mean missed = 1.404, total of missed trials = 66 across 31 participants


# remove missed trials;
alldata = alldata[-which(is.nan(alldata$choice)),]; 

# recode choice variable so that accept gamble = 1 and reject gamble = 0
alldata$choice[alldata$choice==2]=0; 

# Data is a little weird because it was collected using ePrime. 
# The outcomes were set prior to participants choosing where possibleOC is the outcome of the gamble were a participant to select the gamble
# So we need to figure out what the actual outcomes were based on if they gambled or nah

alldata$outcome[alldata$choice==1] = alldata$possibleOC[alldata$choice==1]; #if participant took gamble, then their outcome was the possible outcome that was preset before the study
alldata$outcome[alldata$choice==0] = alldata$alternative[alldata$choice==0]; # if participant took safe then their outcome was the safe value

# In the original spreadsheet data for participant 45, the session number is recorded as 1 for both files.
# Session two starts in row 12443 where the risky loss is -7.5, gain is 4, RT is 445
s45sesh2start = which(alldata$subID ==45 & alldata$riskyGain == 4.00 & alldata$riskyLoss == -7.5 & alldata$choice==0 & alldata$rt==445)
s45sesh2end = which(alldata$subID==45 & alldata$riskyGain == 5.00 & alldata$riskyLoss == -7.50 & alldata$rt==840)
alldata$session[s45sesh2start:s45sesh2end]=2;



demomtx = as.data.frame(matrix(nrow=nrow(alldata), ncol =4, dimnames =list(c(NULL), c("diffVis", "gen","BMI", "age")))); #colnames mean = days between visits, gender (male = 0), Body mass index, age in years

# populate demomtx (length of alldata) with information from demoData 

for (i in 1:length(subIDs)){
  demomtx$diffVis[alldata$subID==subIDs[i]] <- demoData$V1[i]; # time between visits
  demomtx$gen[alldata$subID==subIDs[i]] <- demoData$V2[i]; # gender
  demomtx$BMI[alldata$subID==subIDs[i]] <- demoData$V3[i]; # BMI
  demomtx$age[alldata$subID==subIDs[i]] <-ageGen$V3[i]; # age
};

pdmData = cbind(alldata, demomtx); #combine demo info with allData, store as pdmData which we will use moving forward.



#Create trial type variable (a varieties): 

# 1) Trial Type(1=mixed, -1= gain only)
pdmData$trialType[pdmData$alternative==0] =1; #mixed valence
pdmData$trialType[pdmData$alternative>0] = -1;# gain only

#2) Mixed valence trials (1=yes, 0=no)
pdmData$mixedType[pdmData$alternative==0] =1; #yes
pdmData$mixedType[pdmData$alternative>0]=0; #no

#3) Gain only trials (1=yes, 0=no)
pdmData$gainType[pdmData$alternative==0] = 0;# no
pdmData$gainType[pdmData$alternative>0] = 1; #yes

# check some stuff:
mean(pdmData$choice); #pgamble for whole group = .41, risk averse
summary(pdmData$BMI); #same as results reported in PSH 2015 paper
sum(demoData$V2 ==1); #how many females? 22, matchtes with 2015 paper
summary(demoData$V1); #mean 7.5 days of separation on avg between test days, 5-14 range, matches up with 2015 paper

# Create medicine variable
# Ps who had propranolol on the first day: 3,5,7,9,12,13,15,18,19,22,23,26,29,30,32,34,37,39,40,44,48,49,51
# Ps who had propranolol on the second day: 2,4,6,8,10,11,14,16,17,21,24,25,27,28,31,33,35,38,41,42,43,45,46,50

propDay1= c(3,5,7,9,12,13,15,18,19,22,23,26,29,30,32,34,37,39,40,44,48,49,51) #these subs should have -1 for medicine
propDay2= c(2,4,6,8,10,11,14,16,17,21,24,25,27,28,31,33,35,38,41,42,43,45,46,50) # these subs should have 1 for medicine

for (i in 1:length(propDay1)){
  pdmData$medicine[pdmData$subID == propDay1[i] & pdmData$session == 1] = 1
  pdmData$medicine[pdmData$subID == propDay1[i] & pdmData$session == 2] = 0
}

for(i in 1:length(propDay2)){
  pdmData$medicine[pdmData$subID == propDay2[i] & pdmData$session == 1] = 0
  pdmData$medicine[pdmData$subID == propDay2[i] & pdmData$session == 2] = 1
}


# Create some variables for our context analysis
pdmData$meanEV = (pdmData$alternative + ((.5*pdmData$riskyGain)+ (.5*pdmData$riskyLoss)))/2; # mean EV of option
pdmData$choice1 = pdmData$choice; # save choice variable as new variable
pdmData$choice1[pdmData$choice==0] = -1 # recode so that reject gamble = -1 and accept gamble stays = 1

#Create recent event variables (one trial back)
newmtx = cbind(pdmData$outcome,pdmData$choice1,pdmData$meanEV,pdmData$trialType); # combine variables into new matrix
newmtx[2:nrow(pdmData),]= newmtx[1:(nrow(pdmData)-1),]; # shift rows
newmtx[1,] = NaN; # store Nan in first row
newvector = c(0,diff(pdmData$subID)); # pull out differences in subID
newvector2 = c(0,diff(pdmData$session)); # pull out differences in session
newmtx[newvector>0,]=NaN; # put Nan in first trial for each participant's first session
newmtx[newvector2!=0]=NaN; # put Nan in first trial for each participant's second session

# store our new recent event variables in big dataset
pdmData$pastOutcome = newmtx[,1]; # past outcome
pdmData$pastChoice = newmtx[,2]; # past choice
pdmData$pastmeanEV = newmtx[,3]; # past mean ev of the option
pdmData$pastTrialType = newmtx[,4]; # past trial type


# recent event variables (two trials back)
newmtx = cbind(pdmData$outcome,pdmData$choice1,pdmData$meanEV);
newmtx[3:nrow(pdmData),]= newmtx[1:(nrow(pdmData)-2),];
newmtx[1:2,] = NaN;
newvector = c(0,diff(pdmData$subID));
newvector2 = c(0,diff(pdmData$session));
startNan1 = which(newvector>0);
startNan2 = which(newvector2!=0);
putNan1 = c(startNan1, startNan1+1);
newmtx[putNan1,]=NaN;
putNan2 = c(startNan2, startNan2+1);
newmtx[putNan2,]=NaN;
pdmData$pastOutcome2 = newmtx[,1];
pdmData$pastChoice2 = newmtx[,2];
pdmData$pastmeanEV2 = newmtx[,3];

# recent event variables (three trials back)
newmtx = cbind(pdmData$outcome,pdmData$choice1,pdmData$meanEV);
newmtx[4:nrow(pdmData),]= newmtx[1:(nrow(pdmData)-3),];
newmtx[1:3,] = NaN;
newvector = c(0,diff(pdmData$subID));
newvector2 = c(0,diff(pdmData$session));
startNan1 = which(newvector>0);
startNan2 = which(newvector2!=0);
putNan1 = c(startNan1, startNan1+1, startNan1+2);
newmtx[putNan1,]=NaN;
putNan2 = c(startNan2, startNan2+1, startNan2+2);
newmtx[putNan2,]=NaN;
pdmData$pastOutcome3 = newmtx[,1];
pdmData$pastChoice3 = newmtx[,2];
pdmData$pastmeanEV3 = newmtx[,3];

# recent event variables (four trials back)
newmtx = cbind(pdmData$outcome);
newmtx[5:nrow(pdmData),]= newmtx[1:(nrow(pdmData)-4),];
newmtx[1:4,] = NaN;
newvector = c(0,diff(pdmData$subID));
newvector2 = c(0,diff(pdmData$session));
startNan1 = which(newvector>0);
putNan1 = c(startNan1, startNan1+1, startNan1+2,startNan1+3);
newmtx[putNan1,]=NaN;
startNan2 = which(newvector2!=0);
putNan2 = c(startNan2, startNan2+1, startNan2+2,startNan2+3);
newmtx[putNan2,]=NaN;
pdmData$pastOutcome4 = newmtx[,1];



# create outcome valence and type for past outcome 1 trial back
pdmData$pocValence[pdmData$pastOutcome == 0] = 0;
pdmData$pocValence[pdmData$pastOutcome > 0 ] = 1;
pdmData$pocValence[pdmData$pastOutcome < 0 ] =-1;

pdmData$pocType[pdmData$pastChoice==1 & pdmData$pastOutcome <=0] = -1;  #loss
pdmData$pocType[pdmData$pastChoice==1 & pdmData$pastOutcome >0] = 1;    # win
pdmData$pocType[pdmData$pastChoice==-1] = 0;                            #safe
pdmData$pocType[is.nan(pdmData$pastChoice)] = NaN;

pdmData$pastWinAmt = as.numeric(pdmData$pocType==1)*pdmData$pastOutcome; # past win amount
pdmData$pastLossAmt = as.numeric(pdmData$pocType==-1)*pdmData$pastOutcome; # past loss amount
pdmData$pastSafeAmt = as.numeric(pdmData$pocType==0)*pdmData$pastOutcome; # past safe amount

# BMI group based on median split
bmiMed = median(demoData$V3); # median BMI
pdmData$bmiGroup[pdmData$BMI >= bmiMed] = -1; # above median BMI
pdmData$bmiGroup[pdmData$BMI < bmiMed] = 1; # below median BMI

# day variable where day 1 = -1 and day 2 = 1
pdmData$day[pdmData$session==1] = -1;
pdmData$day[pdmData$session==2] = 1;

# Cumulative earnings

# this part does not separate by session:
first = 1; #starting row number
allcumEarnings = vector(); #vector for storing values for all ps
trinum = vector();

for(s in 1:numSub){
  sub = pdmData[pdmData$subID==subIDs[s],]; #pull out subject
  cumEarnings = vector(); #empty vector to store values for one subject
  tri = 1:nrow(sub)/nrow(sub); # normalized trial number by the max trial number for each participant
  for(t in 1:nrow(sub)){ #for each trial
    cumEarnings[t] = sum(sub$outcome[1:t]); #calculate total outcomes 
  }
  cumEarn = c(0,cumEarnings[1:length(cumEarnings)-1]); # put zero in first trial, get rid of last trial
  last=first+nrow(sub)-1; #last row to store in allcumEarnings
  allcumEarnings[first:last] = cumEarn; # store subjects cumulative earnings
  trinum[first:last] = tri
  first = last +1; #set first to be the row after the previous subjects last trial
};
pdmData$trial = trinum
pdmData$cumEarning = allcumEarnings; #store cumulative earnings in pdmData


# CUMULATIVE EARNINGS BY SESSION:

eachDayEarnings = vector();
first = 1;
indivMaxEarnDay1 = vector(mode = "numeric",length=numSub);
indivMaxEarnDay2 = vector(mode = "numeric",length=numSub);

for(s in 1:numSub){
  sub = pdmData[pdmData$subID==subIDs[s],]
  earningsDay = vector();
  indEndSesh = which(diff(sub$session)!=0); # end of day 1
  indStartSesh = indEndSesh+1; #start of day 2
  endSub = nrow(sub); # end of day 2
  
  for (t in 1:indEndSesh){
    earningsDay[t] = sum(sub$outcome[1:t]); #calculate total outcomes for day 1
  }
  
  indivMaxEarnDay1[s]= earningsDay[indEndSesh];
  
  for (t in indStartSesh:endSub){
    earningsDay[t] = sum(sub$outcome[indStartSesh:t]); # calculate total outcomes for day 2
  }
  
  indivMaxEarnDay2[s] =earningsDay[endSub]
  
  tri1 = 1:indEndSesh/indEndSesh
  tri2 = 1:(endSub-indEndSesh)/(endSub-indEndSesh)
  trial = c(tri1,tri2)
  
  allSeshEarn = c(0,earningsDay[1:(indEndSesh-1)],0,earningsDay[indStartSesh:(endSub-1)])
  
  last=first+(endSub-1); #last row to store in eachDayEarnings
  eachDayEarnings[first:last] = allSeshEarn; # store subjects cumulative earnings
  trinum[first:last] = trial
  first = last +1; #set first to be the row after the previous subjects last trial
}

pdmData$trialSesh = trinum; # normalized by each participant's max trial
pdmData$earningSesh = eachDayEarnings; #store cumulative earnings in pdmData


# summary of max cumulative earnings per day
# Day 1: mean = 252.3; median = 249; range = 155.4 - 379.2
# Day 2: mean = 235.1; mediam = 235.0; range = 138 - 319.5
# both days: mean = 243.7; median = 242.5; range = 138 - 379.2


# Linear expectation terms:
pdmData$linExpectation = pdmData$trialSesh; # linear expectation term is captured by trial because it is increasing over the task (and its normalized 0 to 1 which makes it coparable to cumulative earnings)



# Scale variables by max risky gain value:
scaleBy = max(pdmData$riskyGain);
pdmData$outcomeSC = pdmData$outcome/scaleBy;
pdmData$earningSC = pdmData$cumEarning/scaleBy;
pdmData$earningSeshSC = pdmData$earningSesh/scaleBy;
pdmData$gainSC = pdmData$riskyGain/scaleBy;
pdmData$lossSC = pdmData$riskyLoss/scaleBy;
pdmData$safeSC = pdmData$alternative/scaleBy;
pdmData$pOC1sc = pdmData$pastOutcome/scaleBy;


#make dataset for propranolol and placebo
propranolol = pdmData[pdmData$medicine==1,];
placebo = pdmData[pdmData$medicine==0,];


## make two datasets for day1 and day 2

day1=pdmData[pdmData$session==1,];
day2=pdmData[pdmData$session==2,];


# Plot alternatives for one participant and one session
plot(sub$alternative[sub$day==1],col = (sub$alternative[1:150] >0)+3, pch=16, axes = F, xlab="trial", ylab="alternative $", main="alternatives for one sub (1 session)");
axis(1, at = c(1,150), tck=0, lwd=3);
axis(2, at = c(0, max(sub$alternative[1:150])), tck=0, lwd=3)


# Create past outcome type variables
pdmData$pastWinType = as.numeric(pdmData$pastOutcome == pdmData$pastWinAmt);
pdmData$pastLossType = as.numeric(pdmData$pastOutcome == pdmData$pastLossAmt);
pdmData$pastSafeType = as.numeric(pdmData$pastOutcome == pdmData$pastSafeAmt);


# Mean EV difference from trial t and trial t-1 (with variations: absolute difference, positive and negative)
pdmData$meanEVdiff =  pdmData$meanEV - pdmData$pastmeanEV;
pdmData$absMeanEVdiff = abs(pdmData$meanEVdiff);
pdmData$meanEVdiffPOS = as.numeric(pdmData$meanEVdiff >0)*pdmData$meanEVdiff;
pdmData$meanEVdiffNEG = as.numeric(pdmData$meanEVdiff <0)*pdmData$meanEVdiff;

# scale the variables:
pdmData$meanEVdiffPOSsc = pdmData$meanEVdiffPOS/scaleBy;
pdmData$meanEVdiffNEGsc = pdmData$meanEVdiffNEG/scaleBy;


# We do not have runs in this dataset like in VIC, so we can look at the change in mean expected value (in an attempt to be analagous with runs + shifts)
# create a dynamic variable that can just be changed based on the number of trials we want to incorporate in the mean
subNum = unique(pdmData$subID);
nSub = length(subNum)
nTback = c(3,20);
dfColNames = c(sprintf("meanEV%dback",nTback[1]), sprintf("meanEV%dback", nTback[2]))
tmpMatAll = as.data.frame(matrix(data=NA, ncol=length(nTback),dimnames=list(c(NULL), dfColNames)));

for (s in 1:nSub) {
  sub = pdmData[pdmData$subID==subNum[s],]; #one participant
  x =which(c(0,diff(sub$day))!=0);
  
  tmp = as.data.frame(matrix(data=NA,nrow = nrow(sub), ncol=length(nTback), dimnames=list(c(NULL),dfColNames)));
  
  for (t in 1:length(nTback)) {
    for (n in (nTback[t]+1):nrow(tmp)) {
      tmp[n,t] = mean(sub$meanEV[(n-nTback[t]):(n-1)]); # does not include current trial
      #tmp[n,t] = mean(sub$meanEV[(n-nTback[t]):(n)]); # does include current trial 
    };
    tmp[1:(nTback[t]),t] = NaN
    tmp[x:(x+(nTback[t]-1)),t] = NaN; 
  };
  
  
  tmpMatAll = rbind(tmpMatAll, tmp)
};

tmpMatAll = tmpMatAll[-1,]; # remove the first row (an extra NA because of how HRB made this dataframe)
pdmData = cbind(pdmData,tmpMatAll); # add new variables to pdm data frame

pdmData$meanEVdiff3tBack = pdmData$meanEV - pdmData$meanEV3back; # mean EV from 3 trials
pdmData$meanEVdiff20tBack = pdmData$meanEV - pdmData$meanEV20back; # mean EV of 20 trials

# scaled
pdmData$meanEVdiff3tBacksc = pdmData$meanEVdiff3tBack/max(pdmData$riskyGain);
pdmData$meanEVdiff20tBacksc = pdmData$meanEVdiff20tBack/max(pdmData$riskyGain);

# positive change
pdmData$meanEVdiff3tBackscPOS = pdmData$meanEVdiff3tBacksc
pdmData$meanEVdiff3tBackscPOS[pdmData$meanEVdiff3tBackscPOS<0] = 0;  

pdmData$meanEVdiff20tBackscPOS = pdmData$meanEVdiff20tBacksc
pdmData$meanEVdiff20tBackscPOS[pdmData$meanEVdiff20tBackscPOS<0] = 0; 

# negative change
pdmData$meanEVdiff3tBackscNEG = pdmData$meanEVdiff3tBacksc
pdmData$meanEVdiff3tBackscNEG[pdmData$meanEVdiff3tBackscNEG>0] = 0;  

pdmData$meanEVdiff20tBackscNEG = pdmData$meanEVdiff20tBacksc
pdmData$meanEVdiff20tBackscNEG[pdmData$meanEVdiff20tBackscNEG>0] = 0;  

# Create regressor for past outcome t-1, t-2, t-3 for propranolol
pdmData$pastOC1prop = pdmData$pastOutcome*as.numeric(pdmData$medicine==1);
pdmData$pastOC2prop = pdmData$pastOutcome2*as.numeric(pdmData$medicine==1);
pdmData$pastOC3prop = pdmData$pastOutcome3*as.numeric(pdmData$medicine==1);


# save the datafiles.
save(pdmData, file="/Volumes/shlab/Projects/PDM/code/pdmData.Rdata")
save(day1, file="/Volumes/shlab/Projects/PDM/code/day1.Rdata")
save(day2, file="/Volumes/shlab/Projects/PDM/code/day2.Rdata")
save(propranolol, file="/Volumes/shlab/Projects/PDM/code/propranolol.Rdata")
save(placebo, file="/Volumes/shlab/Projects/PDM/code/placebo.Rdata")
