# VIC SCR data analysis
# HRB 02/9/20 
# outcome is the onset marker and SCRs are .5 seconds after onset to 4.5 after outcome onset
# anything over .02 microsiemens is set to 0
# SCR scored using matlab script procGSR_vic.m
rm(list=ls())

load("/Volumes/shlab/Projects/VIC/data analysis/vicDataB.Rdata"); # load the behavior dataset
load("/Volumes/shlab/Projects/VIC/data analysis/behNaNs.Rdata"); # load indices where subs missed a trial

# import scored scr file
scr = read.csv("/Volumes/shlab/Projects/VIC/data analysis/SCR/vicSCRdataUpdatedOC.csv", header = TRUE);
colnames(scr) = c("trial#", 'amplitude', 'subjectIndex');

scr = scr[-NaNind,]; #remove rows corresponding to NaNs in choice data.
#check it:
nrow(scr) == nrow(vicDataB); # should be true

#subject 5 removed for equipment malfunction
vicBscr = vicDataB[vicDataB$subjectIndex != 5,]; #remove from behavioral dataframe
scr = scr[scr$subjectIndex !=5,]; #remove subject 5 from scr dataframe

#check again
nrow(scr) == nrow(vicBscr); # should be true
plot(scr$subjectIndex); plot(vicBscr$subjectIndex); #check another way (should be mssing sub Id 5)

# all scr amplitudes should be more than .02 or they are zero
which(scr$amplitude <.02 & scr$amplitude !=0); # no values below .02 that are not zero
scr$SRamp = sqrt(scr$amplitude); # sqrt it


vicBscr = cbind(scr,vicBscr); #combine data from vicBscr to be able to call when using lme4
subSCR = unique(vicBscr$subjectIndex); # save sub ID numbers
nSubSCR = length(subSCR); # number of subjects  - should be 61 (alls sub but #5 - includes responders and non responders)

# normalize by each person's max SCR ( their SCRs will be between 0 and 1)
# doing this because the last handful of people have very high SCRs, almost double those from the beginning.
for(s in 1:length(subSCR)){
  normBy = max(vicBscr$SRamp[vicBscr$subjectIndex==subSCR[s]], na.rm = TRUE)
  vicBscr$maxNorm[vicBscr$subjectIndex==subSCR[s]] = vicBscr$SRamp[vicBscr$subjectIndex==subSCR[s]]/normBy
};
#a bunch of zeroes, but also heavily skewed toward .04 and .3


# create vector for SCRs to past outcome (previous trial)
maxNormPast <- vicBscr$maxNorm; # save the variable we want to create shift 
newmtx = as.data.frame(maxNormPast);  # make it a data frame
newmtx$maxNormPast[2:nrow(vicBscr)] <- newmtx$maxNormPast[1:(nrow(vicBscr)-1)]; # removes first row, shifts everything up
newmtx$maxNormPast[1] <- NaN #put Nan in for first row (first trial for subject 1, bc there is no past trial)
newmtx$diffInd<-c(0,diff(vicBscr$subjectIndex)); #put differences between NewSubjectIndex into newvector, 1s show up when subject changes

# when diff = 1 or 2, replace with NaN and this replaces first trials for each participant
newmtx$maxNormPast[newmtx$diffInd==1]=NaN; 
newmtx$maxNormPast[newmtx$diffInd==2]=NaN; 
vicBscr$maxNormPast = newmtx$maxNormPast; # save new variable to dataframe

# How many SCR trials were win , loss and safe outcomes
WinInd = vicBscr$outcome == vicBscr$riskyGain; # 3161 trials (23% of trials)
LossInd = vicBscr$outcome == vicBscr$riskyLoss; # 3348 trials (24% of trials)
SafeInd = vicBscr$outcome == vicBscr$alternative; # 7335 trials (53% of trials)

# save new variables to dataframe as numeric (0/1)
vicBscr$Win = as.numeric(WinInd); 
vicBscr$Loss = as.numeric(LossInd);
vicBscr$Safe = as.numeric(SafeInd);

# look at each participant: maxnorm, p(Gamble), # of non-responses (scr<.02)
scrStats= array(data = NA, dim = c(nSubSCR,4), dimnames = list(NULL, c('maxNorm', 'pGamble', 'percent0', 'subInd')))
scrStats = as.data.frame(scrStats);

for (s in 1:length(subSCR)){
  subjectn = vicBscr[vicBscr$subjectIndex==subSCR[s],]; # get one participant's data
  scrStats$subInd[s] = subSCR[s]; # subject index
  scrStats$maxNorm[s] = mean(subjectn$maxNorm); #mean amplitude response after sqrt transformation
  scrStats$pGamble[s] = mean(subjectn$choice); # p(gamble)
  scrStats$percent0[s] = sum(subjectn$maxNorm == 0)/nrow(subjectn); # percent of scr amplitudes that are zero
  scrStats$mnW[s] = mean(vicBscr$maxNorm[(vicBscr$subjectIndex==subSCR[s])&(WinInd)]); # mean scr for wins
  scrStats$mnL[s] = mean(vicBscr$maxNorm[(vicBscr$subjectIndex==subSCR[s])&(LossInd)]); # mean scr for loss
  scrStats$mnS[s] = mean(vicBscr$maxNorm[(vicBscr$subjectIndex==subSCR[s])&(SafeInd)]); # mean scr for safe
};



#   Remove non-responder participants (those with percent0 > 0.75), we have 46/62 responders
respInd = scrStats$subInd[scrStats$percent0 < .74]; # who are the sub # with percent0 less than 74%
vicBscrResp = subset(vicBscr, subjectIndex %in% respInd); # pull out the people who are responders
scrStatsResp = subset(scrStats, subInd %in% respInd)
row.names(scrStatsResp) = 1:nrow(scrStatsResp);

# non responders: sub #s12 14 16 18 22 25 26 30 33 45 51 53 55 56 61
# how many non-zero trials would these participants be contributing?
T1 = c(1,which(diff(vicBscr$subjectIndex)==1) + 1); #which row in vicBscr is each participants 1st trial?
nTxSub = vector(length=nSubSCR); # to store each participants number of trials
for (n in 1:nSubSCR){
  nTxSub[n] = sum(vicBscr$subjectIndex==subSCR[n])
}

nonRespInd = which(scrStats$percent0 > .74); 
#11 13 15 17 21 24 25 29 32 44 50 52 54 55 60 (these are not the subject numbers but the row numbers)

nTxSub[nonRespInd]; # number of trials for nonresponders
scrStats$percent0[nonRespInd]; # percent 0 for nonresponders (all above 75%)

#create a dataset where the zero SCR responses are removed from the data set of responders
notZeroInd = which(vicBscrResp$maxNorm >0); # which SCRs > 0
vicBscrRespNo0 = vicBscrResp[notZeroInd,];#remove those trials

nonRespRespTris = nTxSub[nonRespInd]* (1-scrStats$percent0[nonRespInd]); # total number of nonzero trials for each of the nonresponding participants
sum(nonRespRespTris); #nonresponders would be contributing 490 trials where the SCR response was not zero!

# responders but remove their  SCRs = 0 trials
scrStatsRespNo0= array(data = NA, dim = c(length(respInd),4), dimnames = list(NULL, c('maxNorm', 'pGamble', 'percent0', 'subInd')))
scrStatsRespNo0 = as.data.frame(scrStatsRespNo0);

WinIndRespNo0 = vicBscrRespNo0$outcome == vicBscrRespNo0$riskyGain; #  1190trials (24% of trials)
LossIndRespNo0 = vicBscrRespNo0$outcome == vicBscrRespNo0$riskyLoss; # 1293 trials (26% of trials)
SafeIndRespNo0 = vicBscrRespNo0$outcome == vicBscrRespNo0$alternative; # 2537 trials (50% of trials)

for (s in 1:length(respInd)){
  subjectn = vicBscrRespNo0[vicBscrRespNo0$subjectIndex==respInd[s],];
  scrStatsRespNo0$subInd[s] = subSCR[s]; # subject index, because we dont have sub 5 in this analysis.
  scrStatsRespNo0$maxNorm[s] = mean(subjectn$maxNorm); #mean amplitude response after sqrt transformation
  scrStatsRespNo0$pGamble[s] = mean(subjectn$choice); # p(gamble)
  scrStatsRespNo0$percent0[s] = sum(subjectn$maxNorm == 0)/nrow(subjectn); # percent of scr amplitudes that are zero
  scrStatsRespNo0$mnW[s] = mean(vicBscrRespNo0$maxNorm[(vicBscrRespNo0$subjectIndex==respInd[s])&(WinIndRespNo0)]);
  scrStatsRespNo0$mnL[s] = mean(vicBscrRespNo0$maxNorm[(vicBscrRespNo0$subjectIndex==respInd[s])&(LossIndRespNo0)]);
  scrStatsRespNo0$mnS[s] = mean(vicBscrRespNo0$maxNorm[(vicBscrRespNo0$subjectIndex==respInd[s])&(SafeIndRespNo0)]);
};


save(scrStats, file="/Volumes/shlab/Projects/VIC/data analysis/SCR/scrStatsAllsubs.Rdata");
save(scrStatsResp, file="/Volumes/shlab/Projects/VIC/data analysis/SCR/scrStatsResp.Rdata");
save(scrStatsRespNo0, file="/Volumes/shlab/Projects/VIC/data analysis/SCR/scrStatsRespNo0.Rdata");
save(vicBscr, file="/Volumes/shlab/Projects/VIC/data analysis/SCR/vicBscr.Rdata")
save(vicBscrResp, file="/Volumes/shlab/Projects/VIC/data analysis/SCR/vicBscrResp.Rdata")
save(vicBscrRespNo0, file="/Volumes/shlab/Projects/VIC/data analysis/SCR/vicBscrRespNo0.Rdata")

