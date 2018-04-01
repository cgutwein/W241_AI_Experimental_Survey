## W241 Experiments and Causal Inference
#
# Final Project: Artificial Intelligence
# The purpose of this script is to read a .csv of data collected by our Qualitrics survey
# and return a list of codes for subjects who did not comply with the requirement of spending
# at least 2 minutes to complete the survey

library(Hmisc)              # describe()
library(data.table)
library(foreign)            # ??? what does this package do ????????
library(sandwich)           # vcovHC for robust SE calculation
library(lmtest)             # coeftest 
library(AER)                # ivreg
library(multiwayvcov)

setwd('C:/Users/Gutwein/Google Drive/MIDS/W241 Experiments and Causal Inference/git_cg/final_local/experiment_results/')
data_file <- "./ai_survey_040118.csv"

df <- fread(data_file)

# Remove useless rows
df <- df[-c(1,2)]                           # remove rows 1, 2 (redundent headers)

# Remove useless columns
df <- df[df$Status != 'Survey Preview']     # remove previews (the ones we used for testing survey)
df <- subset(df, select = -c(RecipientLastName, RecipientFirstName, RecipientEmail, ExternalReference))  # empty columns
df <- subset(df, select = -c(Status, DistributionChannel, UserLanguage))        # columns with the same value for all
head(df)
df[, 'Duration (in seconds)'] <- sapply(df[, 'Duration (in seconds)'], as.numeric)
df[, 'mTurkCode'] <- sapply(df[, 'mTurkCode'], as.numeric)

## Trimming data for dates of interest
library(lubridate)
df$StartDate <- as.POSIXct(df$StartDate)
df$EndDate <- as.POSIXct(df$EndDate)
start <- as.POSIXct("2018-03-31 16:04:00")
stop <- as.POSIXct("2018-04-01 10:00:00")
df_adj <- df[(df$StartDate > start) & (df$EndDate < stop) ,]

## List of codes for non-compliers
never_takers <- df_adj$mTurkCode[df_adj$`Duration (in seconds)` < 120]

## Load MTurk Batch Results CSV
df_mturk <- read.csv('./batch_02/Batch_3176019_batch_results_02.csv')
sum(df_mturk$Answer.surveycode %in% df_adj$mTurkCode)
df_mturk <- df_mturk[df_mturk$Answer.surveycode %in% never_takers ,] ## only new responses

df_mturk$Reject[df_mturk$Answer.surveycode %in% never_takers] <- "x"

df_reject <- data.frame(Reject=df_mturk$Reject, HITID=df_mturk$HITId, AssignmentID=df_mturk$AssignmentId, stringsAsFactors = FALSE)
df_reject <- na.omit(df_reject[(df_reject$Reject == "x") ,])
#write.csv(df_reject, './batch_02/rejections_batch_02_03.csv', row.names=FALSE)
rejections <- df_mturk$Answer.surveycode[df_mturk$AssignmentId %in% df_reject$AssignmentID]
hist(df_adj$`Duration (in seconds)`[df_adj$mTurkCode %in% rejections])


## Checking my rejections from yesterday
reject_02_01 <- read.csv('./batch_02/rejections_batch_02_01.csv', stringsAsFactors = FALSE)
reject_02_02 <- read.csv('./batch_02/rejections_batch_02_02.csv', stringsAsFactors = FALSE)
rm <- (df_mturk$AssignmentId %in% reject_02_01$AssignmentID) | (df_mturk$AssignmentId %in% reject_02_02$AssignmentID)
already_rejected <- df_mturk[rm,]
spare_these <- already_rejected$AssignmentId
rm_spare <- df_reject$AssignmentID %in% spare_these
df_reject <- df_reject[!rm_spare ,]
write.csv(df_reject, './batch_02/rejections_batch_02_03.csv', row.names=FALSE)

## Approving erroneous rejections
app <- already_rejected$Answer.surveycode %in% never_takers
approvals <- already_rejected[!app ,]
approvals$Approve <- "x"
hist(df_adj$`Duration (in seconds)`[df_adj$mTurkCode %in% approvals$Answer.surveycode])
df_approve <- data.frame(Approve=approvals$Approve, HITID=approvals$HITId, AssignmentID=approvals$AssignmentId, stringsAsFactors = FALSE)
write.csv(df_approve, './batch_02/approvals_batch_02_01.csv', row.names=FALSE)


df_check <- df_mturk[(df_mturk$AssignmentId %in% reject_02_01$AssignmentID) | (df_mturk$AssignmentId %in% reject_02_02$AssignmentID) ,]
sum(df_check$Answer.surveycode %in% never_takers)
already_rejected <- df_check$AssignmentId
df_adj$`Duration (in seconds)`[df_adj$mTurkCode %in% df_check$Answer.surveycode]
df_adj$`Duration (in seconds)`[df_adj$mTurkCode %in% never_takers]
