install.packages("corrplot")
library(corrplot)

data <- read.csv(file="data.csv",header=TRUE,sep=",")
dataset = data

summary(dataset[, 2:12])

# CORRELATION ACROSS THE DATASET
cor(dataset[, 3:12])
# A visual representation of the correlation across the dataset
par(mfrow=c(1,1))
cor_obj = cor(dataset[, 3:12])
corrplot(cor_obj, method="number", type="upper")
# Taking a 'high' positive/negative correlation threshold as 0.70, we can see that:
# radius_mean is almost perfectly linearly correlated to perimeter_mean and area_mean, which is obvious.
# radius_mean is highly positively correlated to concave.points_mean
# perimeter_mean is highly positively correlated to concavity_mean and concave.points_mean
# area_mean -> concave.points_mean
# compactness_mean -> concavity_mean, concave.points_mean
# concavity_mean -> concave.points_mean
# We can use the above information to reduce the number of features to build models

# COMPARING BENIGN & MALIGNANT CASES
benign_cases <- dataset[dataset$diagnosis == 'B', ]
malignant_cases <- dataset[dataset$diagnosis == 'M', ]
benign_cases_normalized = benign_cases
malignant_cases_normalized = malignant_cases

#Normalizing the above:
for(col in 3:ncol(benign_cases_normalized)) {
  for(row in 1:nrow(benign_cases_normalized)) {
    benign_cases_normalized[row, col] <- ( benign_cases_normalized[row, col] - min(benign_cases[,col]) ) / (max(benign_cases[,col]) - min(benign_cases[,col]))
  }
}
for(col in 3:ncol(malignant_cases_normalized)) {
  for(row in 1:nrow(malignant_cases_normalized)) {
    malignant_cases_normalized[row, col] <- ( malignant_cases_normalized[row, col] - min(malignant_cases[,col]) ) / (max(malignant_cases[,col]) - min(malignant_cases[,col]))
  }
}

# Visualizing distribution and modes - Histogram
par(mfrow=c(2, 5))
hist(benign_cases_normalized$radius_mean, main="Benign radius_mean", xlab="radius_mean")
hist(benign_cases_normalized$texture_mean, main="Benign texture_mean", xlab="texture_mean")
hist(benign_cases_normalized$perimeter_mean, main="Benign perimeter_mean", xlab="perimeter_mean")
hist(benign_cases_normalized$area_mean, main="Benign area_mean", xlab="area_mean")
hist(benign_cases_normalized$smoothness_mean, main="Benign smoothness_mean", xlab="smoothness_mean")
hist(malignant_cases_normalized$radius_mean, main="Malignant radius_mean", xlab="radius_mean")
hist(malignant_cases_normalized$texture_mean, main="Malignant texture_mean", xlab="texture_mean")
hist(malignant_cases_normalized$perimeter_mean, main="Malignant perimeter_mean", xlab="perimeter_mean")
hist(malignant_cases_normalized$area_mean, main="Malignant area_mean", xlab="area_mean")
hist(malignant_cases_normalized$smoothness_mean, main="Malignant smoothness_mean", xlab="smoothness_mean")

par(mfrow=c(2, 5))
hist(benign_cases_normalized$compactness_mean, main="Benign compactness_mean", xlab="compactness_mean")
hist(benign_cases_normalized$concavity_mean, main="Benign concavity_mean", xlab="concavity_mean")
hist(benign_cases_normalized$concave.points_mean, main="Benign concave.points_mean", xlab="concave.points_mean")
hist(benign_cases_normalized$symmetry_mean, main="Benign symmetry_mean", xlab="symmetry_mean")
hist(benign_cases_normalized$fractal_dimension_mean, main="Benign fractal_dimension_mean", xlab="fractal_dimension_mean")
hist(malignant_cases_normalized$compactness_mean, main="Malignant compactness_mean", xlab="compactness_mean")
hist(malignant_cases_normalized$concavity_mean, main="Malignant concavity_mean", xlab="concavity_mean")
hist(malignant_cases_normalized$concave.points_mean, main="Malignant concave.points_mean", xlab="concave.points_mean")
hist(malignant_cases_normalized$symmetry_mean, main="Malignant symmetry_mean", xlab="symmetry_mean")
hist(malignant_cases_normalized$fractal_dimension_mean, main="Malignant fractal_dimension_mean", xlab="fractal_dimension_mean")


# FINDING OUTLIERS & LOOKING AT GENERAL DISTRIBUTION
par(mfrow=c(2, 5))
boxplot(benign_cases_normalized$radius_mean, main="Benign radius_mean")
boxplot(benign_cases_normalized$texture_mean, main="Benign texture_mean")
boxplot(benign_cases_normalized$perimeter_mean, main="Benign perimeter_mean")
boxplot(benign_cases_normalized$area_mean, main="Benign area_mean")
boxplot(benign_cases_normalized$smoothness_mean, main="Benign smoothness_mean")
boxplot(malignant_cases_normalized$radius_mean, main="Malignant radius_mean")
boxplot(malignant_cases_normalized$texture_mean, main="Malignant texture_mean")
boxplot(malignant_cases_normalized$perimeter_mean, main="Malignant perimeter_mean")
boxplot(malignant_cases_normalized$area_mean, main="Malignant area_mean")
boxplot(malignant_cases_normalized$smoothness_mean, main="Malignant smoothness_mean")
par(mfrow=c(2, 5))
boxplot(benign_cases_normalized$compactness_mean, main="Benign radius_mean")
boxplot(benign_cases_normalized$concavity_mean, main="Benign texture_mean")
boxplot(benign_cases_normalized$concave.points_mean, main="Benign perimeter_mean")
boxplot(benign_cases_normalized$symmetry_mean, main="Benign area_mean")
boxplot(benign_cases_normalized$fractal_dimension_mean, main="Benign smoothness_mean")
boxplot(malignant_cases_normalized$compactness_mean, main="Malignant radius_mean")
boxplot(malignant_cases_normalized$concavity_mean, main="Malignant texture_mean")
boxplot(malignant_cases_normalized$concave.points_mean, main="Malignant perimeter_mean")
boxplot(malignant_cases_normalized$symmetry_mean, main="Malignant area_mean")
boxplot(malignant_cases_normalized$fractal_dimension_mean, main="Malignant smoothness_mean")



