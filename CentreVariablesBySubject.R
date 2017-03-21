# There are a couple of things you need to have in place before the code snippet below is useful.

# Step 1: Create a Trial variable in your overall data set that stores the trial sequence for each subject (e.g., numbered from 1 - 160 if you have 160 trials)
#  I try to make sure my data files already have this info, so I don't have to create it.

# Step 2: drop any trials that you don't want to analyze - errors, no response, outliers
#  My argument here is that if I don't think an RT should be used for the DV, I also don't think it makes
#  sense to include it as a predictor (IV)

# in this example, ldt.pj is the RT data with all errors, outliers, and non-responses removed.
# Trial stores the trial number
# RT stores the reaction time I'm interested in as the DV, and as the prevRT variable.

ldt.pj$PrevTrial = ldt.pj$Trial-1 # calculates the trial number for the previous trial
# That line should mean that you now have, e.g. "Trial 100, PrevTrial 99", for every row in the dataset.

# This assumes the original RTs are in a "RT" variable, and gets the previous RT.
ldt.pj= merge(ldt.pj, ldt.pj[,c("Subject", "Trial", "RT")], all.x=TRUE, all.y=FALSE, by.x=c("Subject", "PrevTrial"), by.y=c("Subject", "Trial"))
# merge is a very useful function when you're trying to match data up from two data sets. In this case,
# I'm saying take the raw data set, and a dataset that stores only the original RTs, and merge them by linking
# Trial to PrevTrial.
# This will produce two "RT" variables (RT.x and RT.y) - so I rename them
# You'll have to chance the c(5, 13) to match your data set.
colnames(ldt.pj)[c(5,13)]=c("RT", "PrevRT")

#This is the tricky bit that centres and scales each RT according to the subject.
# First, subtract each subject's mean RT
ldt.pj$cPrevRT = ldt.pj$PrevRT-tapply(ldt.pj$PrevRT, INDEX=list(ldt.pj$Subject), FUN="mean", na.rm=TRUE, simplify=TRUE)[ldt.pj$Subject]
# Next, divide by each subject's SD.
ldt.pj$cPrevRT = ldt.pj$cPrevRT/tapply(ldt.pj$cPrevRT, INDEX=list(ldt.pj$Subject), FUN="sd", na.rm=TRUE, simplify=TRUE)[ldt.pj$Subject]
