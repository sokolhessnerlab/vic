# VIC SCR ANALYSIS - scrs during decision phase
# HR 7/5/20 - updated 11/2/20 

rm(list=ls());

# import scr file
scr = read.csv("/Volumes/shlab/Projects/VIC/data analysis/SCR/mergedDecisionFiles/mergedVicDecision.csv", header = TRUE); #THIS DATASET DOES NOT HAVE PARTICIPANT 5 BECAUSE WE DID NOT SCORE THEIR DECISION SCRS BECAUSE THEIR DATA WAS EXCLUDED DUE TO EQUIPMENT ISSUES
colnames(scr) = c('subjectIndex',"trial#",'amplitude');

# because we don't have the same number of rows in scr (due to sub 5), let's add a placeholder for this sub to make sure that we are removing the same missed trials from the SCR that we did in the behavioral dataset
subDiff = c(0,diff(scr$subjectIndex));
tmpSub5 = matrix(data=0, nrow=171, ncol = ncol(scr), dimnames = list(c(NULL), colnames(scr))); # matrix with all zeroes
tmpSCR = rbind(scr[1:(which(subDiff==2)-1),], tmpSub5,scr[which(subDiff==2):nrow(scr),])
scr = tmpSCR; # now we have a dataset that has the same number of rows as our behavioral dataset before NaNs have been removed.

load("/Volumes/shlab/Projects/VIC/data analysis/behNaNs.Rdata"); # load indices where participants missed trials
# now we can remove those 120 rows from our SCR data:
scr = scr[-NaNind,]; # should have 14004 rows now

load("/Volumes/shlab/Projects/VIC/data analysis/vicDataB.Rdata"); # load the behavior dataset - nans have already been removed; all participants included (n=62); this dataset also has the predicted values from the behavioral analysis (trial level model) which we will want to use for this analysis.

nrow(scr) == nrow(vicDataB); # should be true

vicDataBdec = vicDataB[-which(vicDataB$subjectIndex==5),]; # remove subject 5 bc equipment issues
scr = scr[-which(scr$subjectIndex==0),]; # in SCR the place holder to subID for sub 5 is 0
nrow(vicDataBdec) == nrow(scr); # should be true 


subNums = unique(vicDataBdec$subjectIndex); # subject ID numbers
nSub = length(subNums); # number of subs

# check that each participant has the same number of trials across the SCR and behavioral dataframes.
T1 = c(1,which(diff(vicDataBdec$subjectIndex)==1) + 1); #which row in vicDataB is each participants 1st trial?
nTxSub = vector(length=nSub); # to store each participants number of trials
for (n in 1:nSub){
  nTxSub[n] = sum(vicDataBdec$subjectIndex==subNums[n])
}

T1 = c(1,which(diff(scr$subjectIndex)==1) + 1); #which row in vicDataB is each participants 1st trial?
nTxSubSCR = vector(length=nSub); # to store each participants number of trials
for (n in 1:nSub){
  nTxSubSCR[n] = sum(scr$subjectIndex==subNums[n])
}
# same trial numbers across participants across these two datasets.

# all scr amplitudes should be more than .02 or they are zero
which(scr$amplitude <.02 & scr$amplitude !=0); # no values below .02 that are not zero
scr$SRamp = sqrt(scr$amplitude); # sqrt it


vicBscr = cbind(vicDataBdec, scr); #combine data from vicBscr to be able to call when using lme4
subSCR = unique(vicBscr$subjectIndex); # sub id numbers
nSubSCR = length(subSCR); # should be 61 (alls sub but #5 - includes responders and non responders)

# normalize by each person's max SCR ( their SCRs will be between 0 and 1)
# doing this because the last handful of people have very high SCRs, almost double those from the beginning.
for(s in 1:length(subSCR)){
  normBy = max(vicBscr$SRamp[vicBscr$subjectIndex==subSCR[s]], na.rm = TRUE)
  vicBscr$maxNorm[vicBscr$subjectIndex==subSCR[s]] = vicBscr$SRamp[vicBscr$subjectIndex==subSCR[s]]/normBy
};

vicBscr$maxNorm[vicBscr$subjectIndex==45] = 0; # subject 45 has no non-zero amplitudes so when normalized, the output is NaN. Replace w/0s
# create vector for SCRs to past outcome
maxNormPast <- vicBscr$maxNorm
newmtx = as.data.frame(maxNormPast)
newmtx$maxNormPast[2:nrow(vicBscr)] <- newmtx$maxNormPast[1:(nrow(vicBscr)-1)]; # removes first row, shifts everything up
newmtx$maxNormPast[1] <- NaN #put Nan in for first row (first trial for subject 1, bc there is no past trial)
newmtx$diffInd<-c(0,diff(vicBscr$subjectIndex)); #put differences between NewSubjectIndex into newvector, 1s show up when subject changes
newmtx$maxNormPast[newmtx$diffInd==1]=NaN; #in newmtx, when diff = 1, replace with NaN, this replaces first trial with nan
newmtx$maxNormPast[newmtx$diffInd==2]=NaN
vicBscr$maxNormPast = newmtx$maxNormPast

# which trials were when gamble was accepted and rejected
GamInd =  vicBscr$choice == 1; # 6505 trials (47% of trials)
SafeInd = vicBscr$choice == 0; # 7335 trials (53% of trials)

# create variable in big dataframe for gamble trials and safe trials as numeric (0/1)
vicBscr$Gam = as.numeric(GamInd);
vicBscr$Safe = as.numeric(SafeInd);

# look at each participant: maxnorm, p(Gamble), # of non-responses (scr<.02)
scrStatsDecision= array(data = NA, dim = c(nSubSCR,4), dimnames = list(NULL, c('maxNorm', 'pGamble', 'percent0', 'subInd')))
scrStatsDecision = as.data.frame(scrStatsDecision);

for (s in 1:length(subSCR)){
  subjectn = vicBscr[vicBscr$subjectIndex==subSCR[s],];
  scrStatsDecision$subInd[s] = subSCR[s]; # subject index, because we dont have sub 5 in this analysis.
  scrStatsDecision$maxNorm[s] = mean(subjectn$maxNorm); #mean amplitude response after sqrt transformation
  scrStatsDecision$pGamble[s] = mean(subjectn$choice); # p(gamble)
  scrStatsDecision$percent0[s] = sum(subjectn$maxNorm == 0)/nrow(subjectn); # percent of scr amplitudes that are zero
  scrStatsDecision$mnG[s] = mean(vicBscr$maxNorm[(vicBscr$subjectIndex==subSCR[s])&(GamInd)]); # mean scr for gambles
  scrStatsDecision$mnS[s] = mean(vicBscr$maxNorm[(vicBscr$subjectIndex==subSCR[s])&(SafeInd)]); # mean scr for safe
};



# CREATE SOME VARIABLES FOR THE SHIFT ANALYSIS:
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


# create shift variables that go back 2-5 trials back
shiftDiffscPOS2 <-vicBscr$shiftDiffscPOS
tmp = as.data.frame(shiftDiffscPOS2);
tmp$shiftDiffscPOS3 <-vicBscr$shiftDiffscPOS
tmp$shiftDiffscPOS4 <-vicBscr$shiftDiffscPOS
tmp$shiftDiffscPOS5 <-vicBscr$shiftDiffscPOS
tmp$shiftDiffscPOS2[3:nrow(tmp)] <-tmp$shiftDiffscPOS2[1:(nrow(tmp)-2)];
tmp$shiftDiffscPOS3[4:nrow(tmp)] <-tmp$shiftDiffscPOS3[1:(nrow(tmp)-3)];
tmp$shiftDiffscPOS4[5:nrow(tmp)] <-tmp$shiftDiffscPOS4[1:(nrow(tmp)-4)];
tmp$shiftDiffscPOS5[6:nrow(tmp)] <-tmp$shiftDiffscPOS5[1:(nrow(tmp)-5)];
tmp$shiftDiffscPOS2[1:2] <- NaN;
tmp$shiftDiffscPOS3[1:3] <- NaN; 
tmp$shiftDiffscPOS4[1:4] <- NaN;
tmp$shiftDiffscPOS5[1:5] <- NaN;

tmp$diffInd<-c(0,diff(vicBscr$subjectIndex)); #put differences between NewSubjectIndex into newvector, 1s show up when subject changes
subChange = which(tmp$diffInd!=0); #change in participant number

for (t in 1:length(subChange)) {
  tmp$shiftDiffscPOS2[subChange[t]:(subChange[t]+1)]=NaN
  tmp$shiftDiffscPOS3[subChange[t]:(subChange[t]+2)]=NaN
  tmp$shiftDiffscPOS4[subChange[t]:(subChange[t]+3)]=NaN
  tmp$shiftDiffscPOS5[subChange[t]:(subChange[t]+4)]=NaN
}

vicBscr = cbind(vicBscr,tmp[,1:4]); #add new shift variables to the big dataset. 


#   Remove non-responder participants (those with percent0 > 0.75), we have 26/62 responders
respInd = scrStatsDecision$subInd[scrStatsDecision$percent0 < .74]; # who are the sub # with percent0 less than 74%
vicBscrResp = subset(vicBscr, subjectIndex %in% respInd); # pull out the people who are responders
scrStatsDecResp = subset(scrStatsDecision, subInd %in% respInd)
row.names(scrStatsDecResp) = 1:nrow(scrStatsDecResp);


save(scrStatsDecision, file="/Volumes/shlab/Projects/VIC/data analysis/SCR/decisionAnalysis/scrStatsAllsubs.Rdata");
save(scrStatsDecResp, file="/Volumes/shlab/Projects/VIC/data analysis/SCR/decisionAnalysis/scrStatsResp.Rdata");
save(vicBscr, file="/Volumes/shlab/Projects/VIC/data analysis/SCR/decisionAnalysis/vicBscr.Rdata")
save(vicBscrResp, file="/Volumes/shlab/Projects/VIC/data analysis/SCR/decisionAnalysis/vicBscrResp.Rdata")

