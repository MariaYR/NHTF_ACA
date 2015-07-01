#New QP Code
#For PSJ resubmission 

#June 29th, 2015


library(RTextTools)
load("~/Desktop/NHTF_ACA/R/PSJ_6.30.15.RData")
#run code for Deep Core Beliefs, using the full sample of excerpts 

#Set working directory to the policy core beleifs folder
setwd ("~/Desktop/NHTF_ACA/data")
list.files()

#load data
#this data covers only Coalition A & B Deep Core Beliefs 
DeepCoreBeliefs<- read.csv("~/Desktop/NHTF_ACA/data/Deep_Core_Beliefs_4R.csv",header = (TRUE))

#create a term document matrix that represents word frequencies in each excerpt
DeepCoreBeliefs_matrix <- create_matrix(cbind(DeepCoreBeliefs["Document"],DeepCoreBeliefs["Excerpt.Copy"]), language="english",
                          removeNumbers=TRUE, stemWords=FALSE)

#check out the matrix
DeepCoreBeliefs_matrix

#create a corpus that is split into a training set and a testing set
#defined are a 75 article training set and a 25 aticle testing set
#I want a training set of 46 and a testing set of 23
DCcontainer <- create_container(DeepCoreBeliefs_matrix,DeepCoreBeliefs$Code,trainSize=1:244, testSize=245:365,virgin=FALSE)
#take a look at the object
DCcontainer

#Taking a quick look at the Document Term Matrix
#example_mat <- DCcontainer@training_matrix
#example_names <- DCcontainer@column_names
#example_mat2 <- as.matrix(example_mat)
#colnames(example_mat2) <- example_names
#example_mat2[1:10,1:10]

##########################################
#         TRAIN MODELS  			 #
##########################################
#One at a time
DC_SVM <- train_model(DCcontainer,"SVM")
DC_GLMNET <- train_model(DCcontainer,"GLMNET")

#to remove a value you no longer want = rm(value)

#DC_MAXENT <- train_model(DCcontainer,"MAXENT")
#problem with Rstudio crashing when trying to run. Contacted Rstudio 
#premium support on 6.29.15

DC_SLDA <- train_model(DCcontainer,"SLDA")
#Warning message:
#In lda.default(x, grouping, ...) : variables are collinear

DC_BOOSTING <- train_model(DCcontainer,"BOOSTING")
DC_BAGGING <- train_model(DCcontainer,"BAGGING")
DC_RF <- train_model(DCcontainer,"RF")
DC_NNET <- train_model(DCcontainer,"NNET")
DC_TREE <- train_model(DCcontainer,"TREE")

##########################################
#     	  CLASSIFY MODELS		     #
##########################################
SVM_CLASSIFY <- classify_model(DCcontainer, DC_SVM)
GLMNET_CLASSIFY <- classify_model(DCcontainer, DC_GLMNET)

#MAXENT_CLASSIFY <- classify_model(DCcontainer, DC_MAXENT)
#problem with Rstudio crashing when trying to run. Contacted Rstudio 
#premium support on 6.29.15

SLDA_CLASSIFY <- classify_model(DCcontainer, DC_SLDA)
BOOSTING_CLASSIFY <- classify_model(DCcontainer, DC_BOOSTING)
BAGGING_CLASSIFY <- classify_model(DCcontainer, DC_BAGGING)

RF_CLASSIFY <- classify_model(DCcontainer, DC_RF)
#NNET_CLASSIFY <- classify_model(DCcontainer, DC_NNET)
#Error in data.frame(as.character(nnet_pred), nnet_prob) : 
  #arguments imply differing number of rows: 0, 121 (6.29.15)

TREE_CLASSIFY <- classify_model(DCcontainer, DC_TREE)

##########################################
# VIEW THE RESULTS BY CREATING ANALYTICS #
##########################################
analytics <- create_analytics(DCcontainer,cbind(SVM_CLASSIFY, GLMNET_CLASSIFY, SLDA_CLASSIFY,  BOOSTING_CLASSIFY, BAGGING_CLASSIFY, RF_CLASSIFY, TREE_CLASSIFY))

# RESULTS WILL BE REPORTED BACK IN THE analytics VARIABLE.
# analytics@algorithm_summary: SUMMARY OF PRECISION, RECALL, F-SCORES, AND ACCURACY SORTED BY TOPIC CODE FOR EACH ALGORITHM
# analytics@label_summary: SUMMARY OF LABEL (e.g. TOPIC) ACCURACY
# analytics@document_summary: RAW SUMMARY OF ALL DATA AND SCORING
# analytics@ensemble_summary: SUMMARY OF ENSEMBLE PRECISION/COVERAGE. USES THE n VARIABLE PASSED INTO create_analytics()

head(analytics@algorithm_summary)
head(analytics@label_summary)
head(analytics@document_summary)
analytics@ensemble_summary

#quick summary
summary(analytics)

setwd("~/Desktop/NHTF_ACA/results")

# WRITE OUT THE DATA TO A CSV --- look in your working directory
write.csv(analytics@algorithm_summary,"DeepCore_AlgorithmSummary.csv")
write.csv(analytics@label_summary,"DeepCore_LabelSummary.csv")
write.csv(analytics@document_summary,"DeepCore_DocumentSummary.csv")
write.csv(analytics@ensemble_summary,"DeepCore_EnsembleSummary.csv")
write.csv(summary(analytics),"DeepCore_Summary.csv")

###################
#Plots
###################

somePDFPath = "./results/test.pdf"
pdf(file=somePDFPath)  

for (i in seq(5,10))   
{   
  par(mfrow = c(2,2))
  VAR1=rnorm(i)  
  VAR2=rnorm(i)  
  plot(VAR1,VAR2)   
} 
dev.off() 

# Plot SVM Recall to see which labels are poor
x <- as.numeric(rownames(analytics@algorithm_summary))[-20]
y <- analytics@algorithm_summary$SVM_RECALL[-20]
plot(x, y, type="l", lwd=3, main="Support Vector Machine Deep Core Beliefs Code Accuracy", ylab="Recall Accuracy", xlab="Code")
abline(h=.75, lwd=2, col="maroon")
text(x, y, adj=1.2)

#SLDA Recall 
x <- as.numeric(rownames(analytics@algorithm_summary))[-20]
y <- analytics@algorithm_summary$SLDA_RECALL[-20]
plot(x, y, type="l", lwd=3, main="SLDA Deep Core Beliefs Code Accuracy", ylab="Recall Accuracy", xlab="Code")
abline(h=.75, lwd=2, col="maroon")
text(x, y, adj=1.2)

#Logitboost_recall (ie. BOOSTING)
x <- as.numeric(rownames(analytics@algorithm_summary))[-20]
y <- analytics@algorithm_summary$LOGITBOOST_RECALL[-20]
plot(x, y, type="l", lwd=3, main="Boosting Deep Core Beliefs Code Accuracy", ylab="Recall Accuracy", xlab="Code")
abline(h=.75, lwd=2, col="maroon")
text(x, y, adj=1.2)

#Bagging Recall
x <- as.numeric(rownames(analytics@algorithm_summary))[-20]
y <- analytics@algorithm_summary$BAGGING_RECALL[-20]
plot(x, y, type="l", lwd=3, main="Bagging Deep Core Beliefs Code Accuracy", ylab="Recall Accuracy", xlab="Code")
abline(h=.75, lwd=2, col="maroon")
text(x, y, adj=1.2)

#RF (random forests)
x <- as.numeric(rownames(analytics@algorithm_summary))[-20]
y <- analytics@algorithm_summary$FORESTS_RECALL[-20]
plot(x, y, type="l", lwd=3, main="Random Forest Deep Core Beliefs Code Accuracy", ylab="Recall Accuracy", xlab="Code")
abline(h=.75, lwd=2, col="maroon")
text(x, y, adj=1.2)

#GLM_NET
x <- as.numeric(rownames(analytics@algorithm_summary))[-20]
y <- analytics@algorithm_summary$GLMNET_RECALL[-20]
plot(x, y, type="l", lwd=3, main="GLM_NET Machine Deep Core Beliefs Code Accuracy", ylab="Recall Accuracy", xlab="Code")
abline(h=.75, lwd=2, col="maroon")
text(x, y, adj=1.2)

#Tree Recall 
x <- as.numeric(rownames(analytics@algorithm_summary))[-20]
y <- analytics@algorithm_summary$TREE_RECALL[-20]
plot(x, y, type="l", lwd=3, main="TREE Deep Core Beliefs Code Accuracy", ylab="Recall Accuracy", xlab="Code")
abline(h=.75, lwd=2, col="maroon")
text(x, y, adj=1.2)

#NNET (revisit - this failed at the classification step)
#x <- as.numeric(rownames(analytics@algorithm_summary))[-20]
#y <- analytics@algorithm_summary$SVM_RECALL[-20]
#plot(x, y, type="l", lwd=3, main="Support Vector Machine Deep Core Beliefs Code Accuracy", ylab="Recall Accuracy", xlab="Code")
#abline(h=.75, lwd=2, col="maroon")
#text(x, y, adj=1.2)

#MAX_ENT Plot 
#x <- as.numeric(rownames(analytics@algorithm_summary))[-20]
#y <- analytics@algorithm_summary$SVM_RECALL[-20]
#plot(x, y, type="l", lwd=3, main="Support Vector Machine Deep Core Beliefs Code Accuracy", ylab="Recall Accuracy", xlab="Code")
#abline(h=.75, lwd=2, col="maroon")
#text(x, y, adj=1.2)

# Confusion Matrices -- look for possible problems
table(true = analytics@document_summary$MANUAL_CODE, predict = analytics@document_summary$CONSENSUS_CODE)
table(true = analytics@document_summary$MANUAL_CODE, predict = analytics@document_summary$PROBABILITY_CODE)
table(true = analytics@document_summary$MANUAL_CODE, predict = analytics@document_summary$SVM_LABEL)
#table(true = analytics@document_summary$MANUAL_CODE, predict = analytics@document_summary$MAXENTROPY_LABEL)
table(true = analytics@document_summary$MANUAL_CODE, predict = analytics@document_summary$BOOSTING_LABEL)
table(true = analytics@document_summary$MANUAL_CODE, predict = analytics@document_summary$RF_LABEL)
table(true = analytics@document_summary$MANUAL_CODE, predict = analytics@document_summary$TREE_LABEL)
table(true = analytics@document_summary$MANUAL_CODE, predict = analytics@document_summary$SLDA_LABEL)
table(true = analytics@document_summary$MANUAL_CODE, predict = analytics@document_summary$BAGGING_LABEL)

# CHECK OVERALL ACCURACY OF ALGORITHMS
recall_accuracy (analytics@document_summary$MANUAL_CODE, analytics@document_summary$CONSENSUS_CODE)
recall_accuracy (analytics@document_summary$MANUAL_CODE, analytics@document_summary$PROBABILITY_CODE)
recall_accuracy (analytics@document_summary$MANUAL_CODE, analytics@document_summary$SVM_LABEL)
#recall_accuracy (analytics@document_summary$MANUAL_CODE, analytics@document_summary$MAXENTROPY_LABEL)
recall_accuracy (analytics@document_summary$MANUAL_CODE, analytics@document_summary$BOOSTING_LABEL)
recall_accuracy (analytics@document_summary$MANUAL_CODE, analytics@document_summary$RF_LABEL)
recall_accuracy (analytics@document_summary$MANUAL_CODE, analytics@document_summary$TREE_LABEL)
recall_accuracy (analytics@document_summary$MANUAL_CODE, analytics@document_summary$SLDA_LABEL)
recall_accuracy (analytics@document_summary$MANUAL_CODE, analytics@document_summary$BAGGING_LABEL)

###########################
#    Ensemble Agreement   #
###########################

# Set standard of accuracy. If everything needs to be at over 80%, then you will need to 
# manually code the documents that do not reach that.

analytics@ensemble_summary

DeepCore_Final <- analytics@document_summary
DeepCore_Final$recall_ensemble[DeepCore_Final$CONSENSUS_AGREE == 2] <- "88% recall"
DeepCore_Final$recall_ensemble[DeepCore_Final$CONSENSUS_AGREE == 1] <- "72% recall"
head(DeepCore_Final)

# Write out data for analysis 
getwd()
write.csv(DeepCore_Final, "test_deepcore_beliefs_coded.csv", row.names=FALSE)


#############################################
#
#Do this again for Policy Core Beliefs
#
##############################################
setwd("~/Desktop/NHTF_ACA/data")

#load data
#this data covers only Coalition A & B Policy Core Beliefs 
PCexcerpts<- read.csv("Policy_Core_Beliefs_4R.csv",header = (TRUE))

#create a term document matrix that represents word frequencies in each excerpt
PCmatrix <- create_matrix(cbind(PCexcerpts["Document"],PCexcerpts["Excerpt.Copy"]), language="english",
                        removeNumbers=TRUE, stemWords=FALSE, weighting=weightTfIdf)

#check out the matrix
PCmatrix

#create a corpus that is split into a training set and a testing set
#defined are a 75 article training set and a 25 aticle testing set
#I want a training set of 46 and a testing set of 23
PCcontainer <- create_container(PCmatrix,PCexcerpts$Code,trainSize=1:390, testSize=391:582,virgin=FALSE)
#take a look at the object
PCcontainer

#Taking a quick look at the Document Term Matrix
example_mat <- PCcontainer@training_matrix
example_names <- PCcontainer@column_names
example_mat2 <- as.matrix(example_mat)
colnames(example_mat2) <- example_names
example_mat2[1:10,1:10]

PC_SVM <- train_model(PCcontainer,"SVM")
#GLMNET <- train_model(PCcontainer,"GLMNET")
#Error in validObject(.Object) : 
#invalid class "dgRMatrix" object: slot j is not increasing inside a column

PC_MAXENT <- train_model(PCcontainer,"MAXENT")
PC_SLDA <- train_model(PCcontainer,"SLDA")
#Error: cannot allocate vector of size 356.1 Mb

PC_BOOSTING <- train_model(PCcontainer,"BOOSTING")
#BAGGING <- train_model(PCcontainer,"BAGGING")
#Error: cannot allocate vector of size 59.3 Mb

PC_RF <- train_model(PCcontainer,"RF")
PC_NNET <- train_model(PCcontainer,"NNET")
PC_TREE <- train_model(PCcontainer,"TREE")

##########################################
#         CLASSIFY MODELS		     #
##########################################
PC_SVM_CLASSIFY <- classify_model(PCcontainer, SVM)
#PC_GLMNET_CLASSIFY <- classify_model(PCcontainer, GLMNET)

PC_MAXENT_CLASSIFY <- classify_model(PCcontainer, MAXENT)
PC_SLDA_CLASSIFY <- classify_model(PCcontainer, SLDA)
PC_BOOSTING_CLASSIFY <- classify_model(PCcontainer, BOOSTING)
#PC_BAGGING_CLASSIFY <- classify_model(PCcontainer, BAGGING)
PC_RF_CLASSIFY <- classify_model(PCcontainer, RF)
#PC_NNET_CLASSIFY <- classify_model(PCcontainer, NNET)
#Error in data.frame(as.character(nnet_pred), nnet_prob) : 
#arguments imply differing number of rows: 0, 192
PC_TREE_CLASSIFY <- classify_model(PCcontainer, TREE)


##########################################
# VIEW THE RESULTS BY CREATING ANALYTICS #
##########################################
PC_analytics <- create_analytics(PCcontainer,cbind(PC_SVM_CLASSIFY, PC_SLDA_CLASSIFY,PC_BOOSTING_CLASSIFY, PC_RF_CLASSIFY, PC_TREE_CLASSIFY, PC_MAXENT_CLASSIFY))

# RESULTS WILL BE REPORTED BACK IN THE analytics VARIABLE.
# analytics@algorithm_summary: SUMMARY OF PRECISION, RECALL, F-SCORES, AND ACCURACY SORTED BY TOPIC CODE FOR EACH ALGORITHM
# analytics@label_summary: SUMMARY OF LABEL (e.g. TOPIC) ACCURACY
# analytics@document_summary: RAW SUMMARY OF ALL DATA AND SCORING
# analytics@ensemble_summary: SUMMARY OF ENSEMBLE PRECISION/COVERAGE. USES THE n VARIABLE PASSED INTO create_analytics()

head(PC_analytics@algorithm_summary)
head(PC_analytics@label_summary)
head(PC_analytics@document_summary)
PC_analytics@ensemble_summary

#quick summary
summary(PC_analytics)

setwd("~/Desktop/NHTF_ACA/results")

# WRITE OUT THE DATA TO A CSV --- look in your working directory
write.csv(PC_analytics@algorithm_summary,"PolicyCore_AlgorithmSummary.csv")
write.csv(PC_analytics@label_summary,"PolicyCore_LabelSummary.csv")
write.csv(PC_analytics@document_summary,"PolicyCore_DocumentSummary.csv")
write.csv(PC_analytics@ensemble_summary,"PolicyCore_EnsembleSummary.csv")
write.csv(summary(PC_analytics),"PolicyCore_Summary.csv")

# Plot SVM Recall to see which labels are poor
x <- as.numeric(rownames(PC_analytics@algorithm_summary))[-20]
y <- PC_analytics@algorithm_summary$SVM_RECALL[-20]
plot(x, y, type="l", lwd=3, main="Support Vector Machine Policy Core Beliefs Code Accuracy", ylab="Recall Accuracy", xlab="Code")
abline(h=.75, lwd=2, col="maroon")
text(x, y, adj=1.2)

# Confusion Matrices -- look for possible problems
table(true = PC_analytics@document_summary$MANUAL_CODE, predict = PC_analytics@document_summary$CONSENSUS_CODE)
table(true = PC_analytics@document_summary$MANUAL_CODE, predict = PC_analytics@document_summary$PROBABILITY_CODE)
table(true = PC_analytics@document_summary$MANUAL_CODE, predict = PC_analytics@document_summary$SVM_LABEL)
table(true = PC_analytics@document_summary$MANUAL_CODE, predict = PC_analytics@document_summary$MAXENTROPY_LABEL)
table(true = PC_analytics@document_summary$MANUAL_CODE, predict = PC_analytics@document_summary$FORESTS_LABEL)
table(true = PC_analytics@document_summary$MANUAL_CODE, predict = PC_analytics@document_summary$TREE_LABEL)
#table(true = PC_analytics@document_summary$MANUAL_CODE, predict = PC_analytics@document_summary$BOOSTING_LABEL)


# CHECK OVERALL ACCURACY OF ALGORITHMS
recall_accuracy (PC_analytics@document_summary$MANUAL_CODE, PC_analytics@document_summary$CONSENSUS_CODE)
recall_accuracy (PC_analytics@document_summary$MANUAL_CODE, PC_analytics@document_summary$PROBABILITY_CODE)
recall_accuracy (PC_analytics@document_summary$MANUAL_CODE, PC_analytics@document_summary$SVM_LABEL)
recall_accuracy (PC_analytics@document_summary$MANUAL_CODE, PC_analytics@document_summary$MAXENTROPY_LABEL)
recall_accuracy (PC_analytics@document_summary$MANUAL_CODE, PC_analytics@document_summary$FORESTS_LABELL)
recall_accuracy (PC_analytics@document_summary$MANUAL_CODE, PC_analytics@document_summary$TREE_LABELL)
recall_accuracy (PC_analytics@document_summary$MANUAL_CODE, PC_analytics@document_summary$BOOSTING_LABEL)

###########################
#    Ensemble Agreement   #
###########################

# Set standard of accuracy. If everything needs to be at over 80%, then you will need to 
# manually code the documents that do not reach that.

PC_analytics@ensemble_summary

PolicyCore_Final <- PC_analytics@document_summary
PolicyCore_Final$recall_ensemble[PolicyCore_Final$CONSENSUS_AGREE == 2] <- "88% recall"
PolicyCore_Final$recall_ensemble[PolicyCore_Final$CONSENSUS_AGREE == 1] <- "72% recall"
head(PolicyCore_Final)

# Write out data for analysis in your favorite statistics program (aka Excel)
getwd()
write.csv(PolicyCore_Final, "test_policycore_beliefs_coded.csv", row.names=FALSE)


#############################################
#
#Do this test again for Secondary Beliefs
#
##############################################

getwd()
list.files()
setwd ("~/Desktop/NHTF_ACA/data")

#load data
SBexcerpts<- read.csv("~/Desktop/NHTF_ACA/data/Secondary_Beliefs_4R.csv",header = (TRUE))

#create a term document matrix that represents word frequencies in each excerpt
matrix <- create_matrix(cbind(SBexcerpts["Document"],SBexcerpts["Excerpt.Copy"]), language="english",
                        removeNumbers=TRUE, stemWords=FALSE, weighting=weightTfIdf)

#check out the matrix
matrix

#create a corpus that is split into a training set and a testing set
#defined are a 75 article training set and a 25 aticle testing set
#I want a training set of 46 and a testing set of 23
SBcontainer <- create_container(matrix,SBexcerpts$Code,trainSize=1:252, testSize=253:377,virgin=FALSE)
#take a look at the object
SBcontainer

#Taking a quick look at the Document Term Matrix
example_mat <- PCcontainer@training_matrix
example_names <- PCcontainer@column_names
example_mat2 <- as.matrix(example_mat)
colnames(example_mat2) <- example_names
example_mat2[1:10,1:10]

##########################################
#         TRAIN MODELS    		 #
##########################################
#BATCH training method (SEVERAL ALGORITHMS AT ONCE)
models <- train_models(SBcontainer, algorithms=c("MAXENT","SVM"))

##########################################
#     	  CLASSIFY MODELS		     #
##########################################
results <- classify_models(SBcontainer, models)

##########################################
# VIEW THE RESULTS BY CREATING ANALYTICS #
##########################################
analytics <- create_analytics(SBcontainer, results)

# RESULTS WILL BE REPORTED BACK IN THE analytics VARIABLE.
# analytics@algorithm_summary: SUMMARY OF PRECISION, RECALL, F-SCORES, AND ACCURACY SORTED BY TOPIC CODE FOR EACH ALGORITHM
# analytics@label_summary: SUMMARY OF LABEL (e.g. TOPIC) ACCURACY
# analytics@document_summary: RAW SUMMARY OF ALL DATA AND SCORING
# analytics@ensemble_summary: SUMMARY OF ENSEMBLE PRECISION/COVERAGE. USES THE n VARIABLE PASSED INTO create_analytics()

head(analytics@algorithm_summary)
head(analytics@label_summary)
head(analytics@document_summary)
analytics@ensemble_summary

#quick summary
summary(analytics)

setwd ("~/Desktop/NHTF_ACA/results")
# WRITE OUT THE DATA TO A CSV --- look in your working directory
write.csv(analytics@algorithm_summary,"Secondary_AlgorithmSummary.csv")
write.csv(analytics@label_summary,"Secondary_LabelSummary.csv")
write.csv(analytics@document_summary,"Secondary_DocumentSummary.csv")
write.csv(analytics@ensemble_summary,"Secondary_EnsembleSummary.csv")
write.csv(summary(analytics),"Secondary_Summary.csv")

# Plot SVM Recall to see which labels are poor
x <- as.numeric(rownames(analytics@algorithm_summary))[-20]
y <- analytics@algorithm_summary$SVM_RECALL[-20]
plot(x, y, type="l", lwd=3, main="Support Vector Machine Secondary Beliefs Code Accuracy", ylab="Recall Accuracy", xlab="Code")
abline(h=.75, lwd=2, col="maroon")
text(x, y, adj=1.2)

# Confusion Matrices -- look for possible problems
table(true = analytics@document_summary$MANUAL_CODE, predict = analytics@document_summary$CONSENSUS_CODE)
table(true = analytics@document_summary$MANUAL_CODE, predict = analytics@document_summary$PROBABILITY_CODE)
table(true = analytics@document_summary$MANUAL_CODE, predict = analytics@document_summary$SVM_LABEL)
table(true = analytics@document_summary$MANUAL_CODE, predict = analytics@document_summary$MAXENTROPY_LABEL)

# CHECK OVERALL ACCURACY OF ALGORITHMS
recall_accuracy (analytics@document_summary$MANUAL_CODE, analytics@document_summary$CONSENSUS_CODE)
recall_accuracy (analytics@document_summary$MANUAL_CODE, analytics@document_summary$PROBABILITY_CODE)
recall_accuracy (analytics@document_summary$MANUAL_CODE, analytics@document_summary$SVM_LABEL)
recall_accuracy (analytics@document_summary$MANUAL_CODE, analytics@document_summary$MAXENTROPY_LABEL)

###########################
#    Ensemble Agreement   #
###########################

# Set standard of accuracy. If everything needs to be at over 80%, then you will need to 
# manually code the documents that do not reach that.

analytics@ensemble_summary

Secondary_Final <- analytics@document_summary
Secondary_Final$recall_ensemble[Secondary_Final$CONSENSUS_AGREE == 2] <- "88% recall"
Secondary_Final$recall_ensemble[Secondary_Final$CONSENSUS_AGREE == 1] <- "72% recall"
head(Secondary_Final)

# Write out data for analysis in your favorite statistics program (aka Excel)
getwd()
write.csv(Secondary_Final, "test_secondary_beliefs_coded.csv", row.names=FALSE)

#############################################
#
#Do this test again for Coalitions
#
##############################################

getwd()
setwd ("~/Desktop/NHTF_ACA/data")
list.files()

#load data
#this data covers only Coalition A & B Policy Core Beliefs 
Cexcerpts<- read.csv("~/Desktop/NHTF_ACA/data/Coalitions_4R.csv",header = (TRUE))

#create a term document matrix that represents word frequencies in each excerpt
Cmatrix <- create_matrix(cbind(Cexcerpts["Document"],Cexcerpts["Excerpt.Copy"]), language="english",
                         removeNumbers=TRUE, stemWords=FALSE, weighting=weightTfIdf)

#check out the matrix
Cmatrix

#create a corpus that is split into a training set and a testing set
#defined are a 75 article training set and a 25 aticle testing set
#I want a training set of 46 and a testing set of 23
Ccontainer <- create_container(Cmatrix,Cexcerpts$Code,trainSize=1:667, testSize=668:1000,virgin=FALSE)
#take a look at the object
Ccontainer

#Taking a quick look at the Document Term Matrix
example_mat <- Ccontainer@training_matrix
example_names <- Ccontainer@column_names
example_mat2 <- as.matrix(example_mat)
colnames(example_mat2) <- example_names
example_mat2[1:10,1:10]

##########################################
#         TRAIN MODELS      	 #
##########################################
#BATCH training method (SEVERAL ALGORITHMS AT ONCE)
models <- train_models(Ccontainer, algorithms=c("MAXENT","SVM"))

##########################################
#     	  CLASSIFY MODELS		     #
##########################################
results <- classify_models(Ccontainer, models)

##########################################
# VIEW THE RESULTS BY CREATING ANALYTICS #
##########################################
analytics <- create_analytics(Ccontainer, results)

# RESULTS WILL BE REPORTED BACK IN THE analytics VARIABLE.
# analytics@algorithm_summary: SUMMARY OF PRECISION, RECALL, F-SCORES, AND ACCURACY SORTED BY TOPIC CODE FOR EACH ALGORITHM
# analytics@label_summary: SUMMARY OF LABEL (e.g. TOPIC) ACCURACY
# analytics@document_summary: RAW SUMMARY OF ALL DATA AND SCORING
# analytics@ensemble_summary: SUMMARY OF ENSEMBLE PRECISION/COVERAGE. USES THE n VARIABLE PASSED INTO create_analytics()

head(analytics@algorithm_summary)
head(analytics@label_summary)
head(analytics@document_summary)
analytics@ensemble_summary

#quick summary
summary(analytics)

# WRITE OUT THE DATA TO A CSV --- look in your working directory
write.csv(analytics@algorithm_summary,"Coalitions_AlgorithmSummary.csv")
write.csv(analytics@label_summary,"Coalitions_LabelSummary.csv")
write.csv(analytics@document_summary,"Coalitions_DocumentSummary.csv")
write.csv(analytics@ensemble_summary,"Coalitions_EnsembleSummary.csv")
write.csv(summary(analytics),"Coalitions_Summary.csv")

# Plot SVM Recall to see which labels are poor
x <- as.numeric(rownames(analytics@algorithm_summary))[-20]
y <- analytics@algorithm_summary$SVM_RECALL[-20]
plot(x, y, type="l", lwd=3, main="Support Vector Machine Coalition Code Accuracy", ylab="Recall Accuracy", xlab="Code")
abline(h=.75, lwd=2, col="maroon")
text(x, y, adj=1.2)

# Confusion Matrices -- look for possible problems
table(true = analytics@document_summary$MANUAL_CODE, predict = analytics@document_summary$CONSENSUS_CODE)
table(true = analytics@document_summary$MANUAL_CODE, predict = analytics@document_summary$PROBABILITY_CODE)
table(true = analytics@document_summary$MANUAL_CODE, predict = analytics@document_summary$SVM_LABEL)
table(true = analytics@document_summary$MANUAL_CODE, predict = analytics@document_summary$MAXENTROPY_LABEL)

# CHECK OVERALL ACCURACY OF ALGORITHMS
recall_accuracy (analytics@document_summary$MANUAL_CODE, analytics@document_summary$CONSENSUS_CODE)
recall_accuracy (analytics@document_summary$MANUAL_CODE, analytics@document_summary$PROBABILITY_CODE)
recall_accuracy (analytics@document_summary$MANUAL_CODE, analytics@document_summary$SVM_LABEL)
recall_accuracy (analytics@document_summary$MANUAL_CODE, analytics@document_summary$MAXENTROPY_LABEL)

###########################
#    Ensemble Agreement   #
###########################

# Set standard of accuracy. If everything needs to be at over 80%, then you will need to 
# manually code the documents that do not reach that.

analytics@ensemble_summary

Coalitions_Final <- analytics@document_summary
Coalitions_Final$recall_ensemble[Coalitions_Final$CONSENSUS_AGREE == 2] <- "88% recall"
Coalitions_Final$recall_ensemble[Coalitions_Final$CONSENSUS_AGREE == 1] <- "72% recall"
head(Coalitions_Final)

# Write out data for analysis in your favorite statistics program (aka Excel)
getwd()
write.csv(Coalitions_Final, "test_coalitions_coded.csv", row.names=FALSE)

