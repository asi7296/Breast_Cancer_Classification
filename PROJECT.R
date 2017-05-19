install.packages("gmodels")
install.packages("ROCR")
library(gmodels)
library(ROCR)
library(class) 

data <- read.csv(file="data.csv",header=TRUE,sep=",")
dataset = data

train_data <<- NULL
test_data <<- NULL
original_labels <<- NULL
train_data_normalized <<- NULL
test_data_normalized <<- NULL


######################################### PREPROCESS AND SET GENERATION ##########################

preprocess <- function() {
  train_ratio = 9
  test_ratio = 1
  train_sample_size = floor( (train_ratio / (train_ratio + test_ratio)) * nrow(dataset) )
  test_sample_size = ceiling( (test_ratio / (train_ratio + test_ratio)) * nrow(dataset) )
  train_data <<- dataset[sample(nrow(dataset), train_sample_size), 1:12]
  test_data <<- dataset[sample(nrow(dataset), test_sample_size), 1:12]
  train_data <<- train_data[-6]
  test_data <<- test_data[-6]
  original_labels <<- test_data["diagnosis"]
    # test data from original dataset with area mean removed, normalized
  train_data_normalized <<- train_data
  for(col in 3:ncol(train_data_normalized)) {
    for(row in 1:nrow(train_data_normalized)) {
      train_data_normalized[row, col] <<- ( train_data_normalized[row, col] - min(train_data[,col]) ) / (max(train_data[,col]) - min(train_data[,col]))
    }
  }
  
  test_data_normalized <<- test_data
  for(col in 3:ncol(test_data_normalized)) {
    for(row in 1:nrow(test_data_normalized)) {
      test_data_normalized[row, col] <<- ( test_data_normalized[row, col] - min(test_data[,col]) ) / (max(test_data[,col]) - min(test_data[,col]))
    }
  }
}



############################# NON REDUCED LOGISTIC REGRESSION CLASSIFIER ##################
# prediction with the normalized model and test dataset

non_reduced_logistic_regressed_classification <- function(compare_mode) {
  if(!compare_mode) {
    print('Not in comparision mode, creating new sets ...')
    train_data <<- NULL
    test_data <<- NULL
    original_labels <<- NULL
    train_data_normalized <<- NULL
    test_data_normalized <<- NULL
    preprocess()
  }
  
  nonreduced_logit_model <- glm(diagnosis ~ radius_mean + texture_mean + perimeter_mean + 
                                        perimeter_mean + smoothness_mean + compactness_mean + concavity_mean + 
                                        concave.points_mean + symmetry_mean + fractal_dimension_mean, 
                                      family = "binomial", data = train_data_normalized[2:11])
  pred_train_data_normalized = predict(nonreduced_logit_model, test_data_normalized, type="response")
  pred_labels_train_data_normalized = rep("B", 57)
  pred_labels_train_data_normalized[pred_train_data_normalized > 0.45] = "M" 
  
  # DRAW TABLE
  CrossTable(x=original_labels$diagnosis, y=pred_labels_train_data_normalized, prop.chisq=FALSE)
  
  if(!compare_mode) {
    p <- prediction(pred_train_data_normalized, original_labels)
    perf <- performance(p, "tpr", "fpr")
    auc <- performance(p, "auc")
    print('AuC: ') 
    auc <- as.numeric(auc@y.values)
    auc 
    plot(perf, xlab=paste('Area Under Curve: ', auc), main="Non Reduced Logistic Regression Classifier")
  }
}

#################### PCA REDUCED LOGISTIC REGRESSION CLASSIFIER ####################################

pca_reduced_logistic_regressed_classification <- function(compare_mode) {
  if(!compare_mode) {
    print('Not in comparision mode, creating new sets ...')
    train_data <<- NULL
    test_data <<- NULL
    original_labels <<- NULL
    train_data_normalized <<- NULL
    test_data_normalized <<- NULL
    preprocess()
  }
  num_prin_axes = 6
  
  new_train_ivs = NULL
  # creating a pca reduced train set
  train_normalized_pca = princomp(train_data_normalized[, 3:11])
  new_train_ivs = train_normalized_pca$scores[, 1:num_prin_axes]
  new_train_ivs = as.data.frame(new_train_ivs)
  new_train_ivs = cbind(diagnosis = train_data_normalized$diagnosis, new_train_ivs)
  
  # creating a pca reduced test set
  test_normalized_pca = princomp(test_data_normalized[, 3:11])
  new_test_ivs = test_normalized_pca$scores[, 1:num_prin_axes]
  new_test_ivs = as.data.frame(new_test_ivs)
  #new_test = cbind(diagnosis = original_labels[470:569, 1], new_test)
  
  # building the model using the pca reduced train set
  pca_reduced_logit_model = glm(diagnosis ~ Comp.1 + Comp.2 + Comp.3 + Comp.4 + Comp.5 + Comp.6, 
                                           family="binomial", 
                                           data=new_train_ivs )
  pred_train_data_normalized_pca = predict(pca_reduced_logit_model, new_test_ivs, type="response")
  pred_labels_train_data_normalized_pca = rep("B", 57)
  pred_labels_train_data_normalized_pca[pred_train_data_normalized_pca > 0.45] = "M"

  CrossTable(x=original_labels$diagnosis, y=pred_labels_train_data_normalized_pca, prop.chisq=FALSE)
  
  if(!compare_mode) {
    p <- prediction(pred_train_data_normalized_pca, original_labels)
    perf <- performance(p, "tpr", "fpr")
    auc <- performance(p, "auc")
    print('AuC: ') 
    auc <- as.numeric(auc@y.values)
    auc 
    plot(perf, xlab=paste('Area Under Curve: ', auc), main="PCA (PA=6) Reduced Logistic Regression Classifier")
  }
}

################# CORRELATION REDUCED LOGISTIC REGRESSION CLASSIFIER ###########

correlation_reduced_logistic_regressed_classification <- function(compare_mode) {
  if(!compare_mode) {
    print('Not in comparision mode, creating new sets ...')
    train_data <<- NULL
    test_data <<- NULL
    original_labels <<- NULL
    train_data_normalized <<- NULL
    test_data_normalized <<- NULL
    preprocess()
  }
  
  correlation_reduced_logit_model <-  glm(diagnosis ~ radius_mean + texture_mean + 
                                          smoothness_mean + compactness_mean +  
                                            + symmetry_mean + fractal_dimension_mean, 
                                          family = "binomial", data = train_data_normalized)
  pred_train_data_normalized = predict(correlation_reduced_logit_model, test_data_normalized, type="response")
  pred_labels_train_data_normalized = rep("B", 57)
  pred_labels_train_data_normalized[pred_train_data_normalized > 0.40] = "M" 
 
  CrossTable(x=original_labels$diagnosis, y=pred_labels_train_data_normalized, prop.chisq=FALSE)
  
  if(!compare_mode) {
    p <- prediction(pred_train_data_normalized, original_labels)
    perf <- performance(p, "tpr", "fpr")
    auc <- performance(p, "auc")
    print('AuC: ') 
    auc <- as.numeric(auc@y.values)
    auc
    plot(perf, xlab=paste('Area Under Curve: ', auc), main="Correlation Reduced Logistic Regression Classifier")
  }
}

################### KNN CLASSIFIER ########################

knn_classification <- function(compare_mode, ...) {
  if(!compare_mode) {
    print('Not in comparision mode, creating new sets ...')
    train_data <<- NULL
    test_data <<- NULL
    original_labels <<- NULL
    train_data_normalized <<- NULL
    test_data_normalized <<- NULL
    preprocess()
  }
  knn_pred = knn(train=train_data_normalized[,3:11], test=test_data_normalized[,3:11], k=11, cl=train_data_normalized$diagnosis)
  CrossTable(x=original_labels$diagnosis, y=knn_pred, prop.chisq=FALSE)
}

################# KMEANS CLUSTERING CLASSIFIER ################

kmc_classification <- function(compare_mode, ...) {
  if(!compare_mode) {
    print('Not in comparision mode, creating new sets ...')
    train_data <<- NULL
    test_data <<- NULL
    original_labels <<- NULL
    train_data_normalized <<- NULL
    test_data_normalized <<- NULL
    preprocess()
  }
  kmc_pred = kmeans(train_data_normalized[,3:11], 2, nstart=10)
  CrossTable(x=train_data_normalized$diagnosis, y=kmc_pred$cluster, prop.chisq=FALSE)
}

################### COMPARE CLASSIFIERS ########################

compare_classifiers <- function() {
  print('Creating a new train and test set for all classifiers ...')
  train_data <<- NULL
  test_data <<- NULL
  original_labels <<- NULL
  train_data_normalized <<- NULL
  test_data_normalized <<- NULL
  preprocess()
  print('     KNN CLASSIFIER:   ')
  knn_classification(1)
  print('     KMEANSCLUSTERING CLASSIFIER:   ')
  kmc_classification(1)
  print('     NON REDUCED LOGISTIC REGRESSION CLASSIFIER:   ')
  non_reduced_logistic_regressed_classification(1)
  print('     CORRELATION REDUCED LOGISTIC REGRESSION CLASSIFIER:   ')
  correlation_reduced_logistic_regressed_classification(1)
  print('     PCA REDUCED LOGISTIC REGRESSION CLASSIFIER:   ')
  pca_reduced_logistic_regressed_classification(1)
}


################################################ API #############################################



#function to call knn classifier with randomly picked test and train set, compare param=0
knn_classification(0) 
#function to call kmc classifier with randomly picked test and train set, compare param=0
kmc_classification(0) 
#function to execute logit regressed classification on a randomly picked test and train set
#compare param=0
non_reduced_logistic_regressed_classification(0)
#function to execute logit regressed classification on a randomly picked test and train set
#compare param=0, model built on manual elimination of features through highly correlated pairs
correlation_reduced_logistic_regressed_classification(0) 
#function to execute pca & logit regressed classification on a randomly picked test and train set
#compare param=0
pca_reduced_logistic_regressed_classification(0) 

# function to compare the above 3 classifiers by running the same randomly picked test and train 
# sets through all of them
compare_classifiers()

## GENERATING RoC CURVES
par(mfrow=c(2,3))
for(i in 1:6) {
  #non_reduced_logistic_regressed_classification(0)
  #correlation_reduced_logistic_regressed_classification(0) 
  #pca_reduced_logistic_regressed_classification(0) 
}

##################################################################################################



