##### 1. Libraries

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(caTools)
library(caret)
library(glmnet)
library(pROC)
library(PRROC)
library(ROCR)
library(reshape2)
library(cowplot)

##### 2. Data loading

openpowerliftingdata <- read_csv("C:/Users/andre/OneDrive/Escritorio/TFM/openpowerliftingdata3.csv")

##### 3. Data division 

ipf_male_data <- openpowerliftingdata %>% filter(Federation == "IPF", Sex == "M")
ipf_female_data <- openpowerliftingdata %>% filter(Federation == "IPF", Sex == "F")

##### 4. Data cleaning


# 4.1. Convert variables to factors.

ipf_male_data <- ipf_male_data %>%
  mutate(
    WeightClassKg = as.factor(WeightClassKg),
    Division = as.factor(Division),
    Equipment = as.factor(Equipment)
  )

ipf_female_data <- ipf_female_data %>%
  mutate(
    WeightClassKg = as.factor(WeightClassKg),
    Division = as.factor(Division),
    Equipment = as.factor(Equipment)
  )

# 4.2. Classifying competitions by movements.


# Male

ipf_male_data <- ipf_male_data %>%
  mutate(
    BooleanSquat = ifelse(
      is.na(Bench1Kg) & is.na(Bench2Kg) & is.na(Bench3Kg) & is.na(Bench4Kg) & is.na(Best3BenchKg) &
        is.na(Deadlift1Kg) & is.na(Deadlift2Kg) & is.na(Deadlift3Kg) & is.na(Deadlift4Kg) & is.na(Best3DeadliftKg) &
        (!is.na(Squat1Kg) | !is.na(Squat2Kg) | !is.na(Squat3Kg) | !is.na(Squat4Kg) | !is.na(Best3SquatKg)), 
      1, 0
    ),
    BooleanBench = ifelse(
      is.na(Squat1Kg) & is.na(Squat2Kg) & is.na(Squat3Kg) & is.na(Squat4Kg) & is.na(Best3SquatKg) &
        is.na(Deadlift1Kg) & is.na(Deadlift2Kg) & is.na(Deadlift3Kg) & is.na(Deadlift4Kg) & is.na(Best3DeadliftKg) &
        (!is.na(Bench1Kg) | !is.na(Bench2Kg) | !is.na(Bench3Kg) | !is.na(Bench4Kg) | !is.na(Best3BenchKg)), 
      1, 0
    ),
    BooleanDeadlift = ifelse(
      is.na(Squat1Kg) & is.na(Squat2Kg) & is.na(Squat3Kg) & is.na(Squat4Kg) & is.na(Best3SquatKg) &
        is.na(Bench1Kg) & is.na(Bench2Kg) & is.na(Bench3Kg) & is.na(Bench4Kg) & is.na(Best3BenchKg) &
        (!is.na(Deadlift1Kg) | !is.na(Deadlift2Kg) | !is.na(Deadlift3Kg) | !is.na(Deadlift4Kg) | !is.na(Best3DeadliftKg)), 
      1, 0
    ),
    BooleanDisq = ifelse(
      (is.na(Squat1Kg) & is.na(Squat2Kg) & is.na(Squat3Kg) & is.na(Squat4Kg) & is.na(Best3SquatKg) &
         is.na(Bench1Kg) & is.na(Bench2Kg) & is.na(Bench3Kg) & is.na(Bench4Kg) & is.na(Best3BenchKg) &
         is.na(Deadlift1Kg) & is.na(Deadlift2Kg) & is.na(Deadlift3Kg) & is.na(Deadlift4Kg) & is.na(Best3DeadliftKg)) | 
        Place == "DQ",
      1, 0
    )
  )

# Female

ipf_female_data <- ipf_female_data %>%
  mutate(
    BooleanSquat = ifelse(
      is.na(Bench1Kg) & is.na(Bench2Kg) & is.na(Bench3Kg) & is.na(Bench4Kg) & is.na(Best3BenchKg) &
        is.na(Deadlift1Kg) & is.na(Deadlift2Kg) & is.na(Deadlift3Kg) & is.na(Deadlift4Kg) & is.na(Best3DeadliftKg) &
        (!is.na(Squat1Kg) | !is.na(Squat2Kg) | !is.na(Squat3Kg) | !is.na(Squat4Kg) | !is.na(Best3SquatKg)), 
      1, 0
    ),
    BooleanBench = ifelse(
      is.na(Squat1Kg) & is.na(Squat2Kg) & is.na(Squat3Kg) & is.na(Squat4Kg) & is.na(Best3SquatKg) &
        is.na(Deadlift1Kg) & is.na(Deadlift2Kg) & is.na(Deadlift3Kg) & is.na(Deadlift4Kg) & is.na(Best3DeadliftKg) &
        (!is.na(Bench1Kg) | !is.na(Bench2Kg) | !is.na(Bench3Kg) | !is.na(Bench4Kg) | !is.na(Best3BenchKg)), 
      1, 0
    ),
    BooleanDeadlift = ifelse(
      is.na(Squat1Kg) & is.na(Squat2Kg) & is.na(Squat3Kg) & is.na(Squat4Kg) & is.na(Best3SquatKg) &
        is.na(Bench1Kg) & is.na(Bench2Kg) & is.na(Bench3Kg) & is.na(Bench4Kg) & is.na(Best3BenchKg) &
        (!is.na(Deadlift1Kg) | !is.na(Deadlift2Kg) | !is.na(Deadlift3Kg) | !is.na(Deadlift4Kg) | !is.na(Best3DeadliftKg)), 
      1, 0
    ),
    BooleanDisq = ifelse(
      (is.na(Squat1Kg) & is.na(Squat2Kg) & is.na(Squat3Kg) & is.na(Squat4Kg) & is.na(Best3SquatKg) &
         is.na(Bench1Kg) & is.na(Bench2Kg) & is.na(Bench3Kg) & is.na(Bench4Kg) & is.na(Best3BenchKg) &
         is.na(Deadlift1Kg) & is.na(Deadlift2Kg) & is.na(Deadlift3Kg) & is.na(Deadlift4Kg) & is.na(Best3DeadliftKg)) | 
        Place == "DQ",
      1, 0
    )
  )


# 4.3. Removing competitions with only one movement.

# Male

ipf_male_data_clean <- ipf_male_data %>%
  filter(
    (BooleanSquat + BooleanBench + BooleanDeadlift != 1) & 
      (BooleanDisq == 0)
  )

# Female

ipf_female_data_clean <- ipf_female_data %>%
  filter(
    (BooleanSquat + BooleanBench + BooleanDeadlift != 1) & 
      (BooleanDisq == 0)
  )

# 4.4. Removing boolean variables created, since they no longer serve any purpose.

ipf_male_data_clean <- ipf_male_data_clean %>%
  select(-BooleanSquat, -BooleanBench, -BooleanDeadlift, -BooleanDisq)

ipf_female_data_clean <- ipf_female_data_clean %>%
  select(-BooleanSquat, -BooleanBench, -BooleanDeadlift, -BooleanDisq)

# 4.5. Removing old divisions.

excluded_divisions <- c("Light", "Middle", "Heavy", "Super", "SOI", "SuperHeavy")

ipf_male_data_clean <- ipf_male_data_clean %>%
  filter(!Division %in% excluded_divisions)

ipf_female_data_clean <- ipf_female_data_clean %>%
  filter(!Division %in% excluded_divisions)

# 4.6. Removing disqualified athletes.

excluded_places <- c("DD", "DQ", "G")

ipf_male_data_clean <- ipf_male_data_clean %>%
  filter(!Place %in% excluded_places)

ipf_female_data_clean <- ipf_female_data_clean %>%
  filter(!Place %in% excluded_places)

# 4.7. Filter 82.5+ female weight class, as it complicates calculations and code.

ipf_female_data_clean <- ipf_female_data_clean %>%
  filter(!WeightClassKg == "82.5+")


##### 5. Proportions model. 

# 5.1. We create new variables to calculate the proportions of each movement respect to the total.

ipf_male_data_clean <- ipf_male_data_clean %>%
  mutate(
    SquatProportion = Best3SquatKg / TotalKg,
    BenchPressProportion = Best3BenchKg / TotalKg,
    DeadliftProportion = Best3DeadliftKg / TotalKg
  )


ipf_female_data_clean <- ipf_female_data_clean %>%
  mutate(
    SquatProportion = Best3SquatKg / TotalKg,
    BenchPressProportion = Best3BenchKg / TotalKg,
    DeadliftProportion = Best3DeadliftKg / TotalKg
  )


# 5.2. Proportions must be normalized, since the sum of averages can be different from 1.

# Normalized proportions

# Male

ipf_male_data_clean <- ipf_male_data_clean %>%
  mutate(
    SquatProportion = ifelse(is.na(SquatProportion), mean(SquatProportion, na.rm = TRUE), SquatProportion),
    BenchPressProportion = ifelse(is.na(BenchPressProportion), mean(BenchPressProportion, na.rm = TRUE), BenchPressProportion),
    DeadliftProportion = ifelse(is.na(DeadliftProportion), mean(DeadliftProportion, na.rm = TRUE), DeadliftProportion)
  ) %>%
  mutate(
    TotalProportion = SquatProportion + BenchPressProportion + DeadliftProportion,
    SquatProportion = SquatProportion / TotalProportion,
    BenchPressProportion = BenchPressProportion / TotalProportion,
    DeadliftProportion = DeadliftProportion / TotalProportion
  ) %>%
  select(-TotalProportion)


# Female

ipf_female_data_clean <- ipf_female_data_clean %>%
  mutate(
    SquatProportion = ifelse(is.na(SquatProportion), mean(SquatProportion, na.rm = TRUE), SquatProportion),
    BenchPressProportion = ifelse(is.na(BenchPressProportion), mean(BenchPressProportion, na.rm = TRUE), BenchPressProportion),
    DeadliftProportion = ifelse(is.na(DeadliftProportion), mean(DeadliftProportion, na.rm = TRUE), DeadliftProportion)
  ) %>%
  mutate(
    TotalProportion = SquatProportion + BenchPressProportion + DeadliftProportion,
    SquatProportion = SquatProportion / TotalProportion,
    BenchPressProportion = BenchPressProportion / TotalProportion,
    DeadliftProportion = DeadliftProportion / TotalProportion
  ) %>%
  select(-TotalProportion)


# 5.3. Average proportions and standard deviations by weight class, division, equipment, sex, competition and year.


# Male

average_proportions_male <- ipf_male_data_clean %>%
  group_by(WeightClassKg, Division, Equipment, Sex, MeetName, Date) %>%
  summarize(
    AvgSquatProportion = mean(SquatProportion, na.rm = TRUE),
    AvgBenchPressProportion = mean(BenchPressProportion, na.rm = TRUE),
    AvgDeadliftProportion = mean(DeadliftProportion, na.rm = TRUE),
    StandDevSquat = sd(SquatProportion, na.rm = TRUE),
    StandDevBench = sd(BenchPressProportion, na.rm = TRUE),
    StandDevDeadlift = sd(DeadliftProportion, na.rm = TRUE)
  ) 


# Female

average_proportions_female <- ipf_female_data_clean %>%
  group_by(WeightClassKg, Division, Equipment, Sex, MeetName, Date) %>%
  summarize(
    AvgSquatProportion = mean(SquatProportion, na.rm = TRUE),
    AvgBenchPressProportion = mean(BenchPressProportion, na.rm = TRUE),
    AvgDeadliftProportion = mean(DeadliftProportion, na.rm = TRUE),
    StandDevSquat = sd(SquatProportion, na.rm = TRUE),
    StandDevBench = sd(BenchPressProportion, na.rm = TRUE),
    StandDevDeadlift = sd(DeadliftProportion, na.rm = TRUE)
  ) 


# 5.4. Transferring average proportions and standard deviations to the datasets.


ipf_male_data_clean <- ipf_male_data_clean %>%
  left_join(average_proportions_male, by = c("WeightClassKg", "Division", "Equipment", "Sex", "MeetName", "Date"))

ipf_female_data_clean <- ipf_female_data_clean %>%
  left_join(average_proportions_female, by = c("WeightClassKg", "Division", "Equipment", "Sex", "MeetName", "Date"))


# 5.5. Classifying athletes' specialty movement.


ipf_male_data_clean <- ipf_male_data_clean %>%
  filter(!is.na(StandDevSquat) & 
           !is.na(StandDevBench) & 
           !is.na(StandDevDeadlift))

ipf_female_data_clean <- ipf_female_data_clean %>%
  filter(!is.na(StandDevSquat) & 
           !is.na(StandDevBench) & 
           !is.na(StandDevDeadlift))


ipf_male_data_clean <- ipf_male_data_clean %>%
  mutate(
    SquatSpecialist = ifelse(SquatProportion > (AvgSquatProportion + StandDevSquat), 1, 0),
    BenchSpecialist = ifelse(BenchPressProportion > (AvgBenchPressProportion + StandDevBench), 1, 0),
    DeadliftSpecialist = ifelse(DeadliftProportion > (AvgDeadliftProportion + StandDevDeadlift), 1, 0),
    BalancedSpecialist = ifelse(SquatSpecialist == 0 & BenchSpecialist == 0 & DeadliftSpecialist == 0, 1, 0)
  )

ipf_female_data_clean <- ipf_female_data_clean %>%
  mutate(
    SquatSpecialist = ifelse(SquatProportion > (AvgSquatProportion + StandDevSquat), 1, 0),
    BenchSpecialist = ifelse(BenchPressProportion > (AvgBenchPressProportion + StandDevBench), 1, 0),
    DeadliftSpecialist = ifelse(DeadliftProportion > (AvgDeadliftProportion + StandDevDeadlift), 1, 0),
    BalancedSpecialist = ifelse(SquatSpecialist == 0 & BenchSpecialist == 0 & DeadliftSpecialist == 0, 1, 0)
  )



##### 6. Logistic regression: ML model to predict who reaches the podium.

# 6.1. "Podium" will be the target variable.

ipf_male_data_clean <- ipf_male_data_clean %>%
  mutate(Podium = ifelse(Place %in% c(1, 2, 3), 1, 0))

ipf_female_data_clean <- ipf_female_data_clean %>%
  mutate(Podium = ifelse(Place %in% c(1, 2, 3), 1, 0))



# 6.2. Divide train and test data.

# Male

set.seed(123)  
split_male <- sample.split(ipf_male_data_clean$Podium, SplitRatio = 0.7)

train_data_male <- subset(ipf_male_data_clean, split_male == TRUE) %>%
  filter(Equipment != "Wraps") %>%
  filter(!is.na(WeightClassKg) & !is.na(Division) & !is.na(Equipment) &
           !is.na(Best3SquatKg) & !is.na(Best3BenchKg) & !is.na(Best3DeadliftKg) &
           !is.na(SquatSpecialist) & !is.na(BenchSpecialist) & !is.na(DeadliftSpecialist) & !is.na(BalancedSpecialist)) %>%
  droplevels()

test_data_male <- subset(ipf_male_data_clean, split_male == FALSE) %>%
  filter(Equipment != "Wraps") %>%
  filter(!is.na(WeightClassKg) & !is.na(Division) & !is.na(Equipment) &
           !is.na(Best3SquatKg) & !is.na(Best3BenchKg) & !is.na(Best3DeadliftKg) &
           !is.na(SquatSpecialist) & !is.na(BenchSpecialist) & !is.na(DeadliftSpecialist) & !is.na(BalancedSpecialist)) %>%
  droplevels() 

# Female

set.seed(123)  
split_female <- sample.split(ipf_female_data_clean$Podium, SplitRatio = 0.7)

train_data_female <- subset(ipf_female_data_clean, split_female == TRUE) %>%
  filter(!is.na(WeightClassKg) & !is.na(Division) & !is.na(Equipment) & 
           !is.na(Best3SquatKg) & !is.na(Best3BenchKg) & !is.na(Best3DeadliftKg) &
           !is.na(SquatSpecialist) & !is.na(BenchSpecialist) & !is.na(DeadliftSpecialist) & !is.na(BalancedSpecialist)) %>%
  droplevels()

test_data_female <- subset(ipf_female_data_clean, split_female == FALSE) %>%
  filter(!is.na(WeightClassKg) & !is.na(Division) & !is.na(Equipment) & 
           !is.na(Best3SquatKg) & !is.na(Best3BenchKg) & !is.na(Best3DeadliftKg) &
           !is.na(SquatSpecialist) & !is.na(BenchSpecialist) & !is.na(DeadliftSpecialist) & !is.na(BalancedSpecialist)) %>%
  droplevels()


# 6.2.1. Synchronizing female weight classes.

# Male

test_data_male$WeightClassKg <- factor(
  test_data_male$WeightClassKg,
  levels = levels(train_data_male$WeightClassKg)
)

# Female

test_data_female$WeightClassKg <- factor(
  test_data_female$WeightClassKg,
  levels = levels(train_data_female$WeightClassKg)
)


# 6.3. Model set up.

# Male

model_male <- glm(Podium ~ Equipment + WeightClassKg + Division + 
                    Best3SquatKg + Best3BenchKg + Best3DeadliftKg +
                    SquatSpecialist + BenchSpecialist + DeadliftSpecialist + BalancedSpecialist +
                    WeightClassKg * Best3SquatKg + WeightClassKg * Best3BenchKg,
                  family = binomial, data = train_data_male)


summary(model_male)

# Female

model_female <- glm(Podium ~ Equipment + WeightClassKg + Division + 
                      Best3SquatKg + Best3BenchKg + Best3DeadliftKg +
                      SquatSpecialist + BenchSpecialist + DeadliftSpecialist + BalancedSpecialist +
                      WeightClassKg * Best3SquatKg + WeightClassKg * Best3BenchKg,
                    family = binomial, data = train_data_female)


summary(model_female)



# 6.4. Model testing.


# Male

predictions_male <- predict(model_male, newdata = test_data_male, type = "response")

predicted_classes_male <- ifelse(predictions_male > 0.4318561, 1, 0)

confusionMatrix(as.factor(predicted_classes_male), as.factor(test_data_male$Podium))

accuracy_male <- mean(predicted_classes_male == test_data_male$Podium)
print(paste("Logistic Regression Accuracy (Male): ", accuracy_male))

# Female

predictions_female <- predict(model_female, newdata = test_data_female, type = "response")

predicted_classes_female <- ifelse(predictions_female > 0.5053197, 1, 0)

confusionMatrix(as.factor(predicted_classes_female), as.factor(test_data_female$Podium))

accuracy_female <- mean(predicted_classes_female == test_data_female$Podium)
print(paste("Logistic Regression Accuracy (Female): ", accuracy_female))



# 6.5. Threshold adjustment

# 6.5.1. 

# Male

pred_male <- prediction(predictions_male, test_data_male$Podium)
perf_male <- performance(pred_male, measure = "tpr", x.measure = "fpr")

# Optimal threshold 

auc_male <- performance(pred_male, measure = "auc")@y.values[[1]]
cat("AUC (Male):", auc_male, "\n")

thresholds <- data.frame(
  Threshold = pred_male@cutoffs[[1]],
  TPR = perf_male@y.values[[1]],
  FPR = perf_male@x.values[[1]]
)

optimal_threshold_male <- thresholds[which.max(thresholds$TPR - thresholds$FPR), "Threshold"]
cat("Optimal Threshold (Male):", optimal_threshold_male, "\n")

# Predictions with optimal threshold 

predicted_classes_male_opt <- ifelse(predictions_male > optimal_threshold_male, 1, 0)
confusionMatrix(as.factor(predicted_classes_male_opt), as.factor(test_data_male$Podium))


# 6.5.2. 

# Female

pred_female <- ROCR::prediction(as.numeric(predictions_female), as.numeric(test_data_female$Podium))

pred_female <- prediction(predictions_female, test_data_female$Podium)
perf_female <- performance(pred_female, measure = "tpr", x.measure = "fpr")

# Optimal threshold 

auc_female <- performance(pred_female, measure = "auc")@y.values[[1]]
cat("AUC (Female):", auc_female, "\n")

thresholds_female <- data.frame(
  Threshold = pred_female@cutoffs[[1]],
  TPR = perf_female@y.values[[1]],
  FPR = perf_female@x.values[[1]]
)

optimal_threshold_female <- thresholds_female[which.max(thresholds_female$TPR - thresholds_female$FPR), "Threshold"]
cat("Optimal Threshold (Female):", optimal_threshold_female, "\n")

# Predictions with optimal threshold 

predicted_classes_female_opt <- ifelse(predictions_female > optimal_threshold_female, 1, 0)
confusionMatrix(as.factor(predicted_classes_female_opt), as.factor(test_data_female$Podium))





##### 7. Model evaluation.


# 7.1. Confusion matrix.

cm_male <- confusionMatrix(as.factor(predicted_classes_male), as.factor(test_data_male$Podium))
fourfoldplot(cm_male$table, color = c("red", "green"), main = "Confusion Matrix (Male)")

cm_female <- confusionMatrix(as.factor(predicted_classes_female), as.factor(test_data_female$Podium))
fourfoldplot(cm_female$table, color = c("red", "green"), main = "Confusion Matrix (Female)")


# 7.2. ROC curves.

roc_curve_male <- roc(test_data_male$Podium, predictions_male)
plot(roc_curve_male, col = "blue", main = "ROC Curve for Male Logistic Regression")

roc_curve_female <- roc(test_data_female$Podium, predictions_female)
plot(roc_curve_female, col = "red", main = "ROC Curve for Female Logistic Regression")



# 7.3. Precision-Recall curves.

logistic_regression_pr_male <- pr.curve(scores.class0 = predictions_male, weights.class0 = test_data_male$Podium, curve = TRUE)
plot(logistic_regression_pr_male, main = "Precision-Recall Curve for Male Logistic Regression")

logistic_regression_pr_female <- pr.curve(scores.class0 = predictions_female, weights.class0 = test_data_female$Podium, curve = TRUE)
plot(logistic_regression_pr_female, main = "Precision-Recall Curve for Female Logistic Regression")



##### 8. Coefficients of feature importance by movement specialty.

# 8.1. Coefficients by movement.

male_coeffs <- c(
  SquatSpecialist = 0.4009138,
  BenchSpecialist = 0.5795760,
  DeadliftSpecialist = -0.3380272,
  BalancedSpecialist = 0.3973726
)

female_coeffs <- c(
  SquatSpecialist = 0.5847690,
  BenchSpecialist = 0.9696019,
  DeadliftSpecialist = -0.3186478,
  BalancedSpecialist = 0.6267206
)


# 8.2. Standard errors by movement. 

male_standard_errors <- c(
  SquatSpecialist = 1.803e-01,
  BenchSpecialist = 1.830e-01,
  DeadliftSpecialist = 1.838e-01,
  BalancedSpecialist = 1.843e-01
)

female_standard_errors <- c(
  SquatSpecialist = 2.730e-01,
  BenchSpecialist = 2.773e-01,
  DeadliftSpecialist = 2.752e-01,
  BalancedSpecialist = 2.776e-01
)


# 8.3. Calculating variances using standard errors.

male_variances <- male_standard_errors^2
female_variances <- female_standard_errors^2

# 8.4. Combined coefficients for two movement specialists. 

male_combined <- c(
  SquatBench = male_coeffs["SquatSpecialist"] + male_coeffs["BenchSpecialist"],
  SquatDeadlift = male_coeffs["SquatSpecialist"] + male_coeffs["DeadliftSpecialist"],
  BenchDeadlift = male_coeffs["BenchSpecialist"] + male_coeffs["DeadliftSpecialist"]
)

female_combined <- c(
  SquatBench = female_coeffs["SquatSpecialist"] + female_coeffs["BenchSpecialist"],
  SquatDeadlift = female_coeffs["SquatSpecialist"] + female_coeffs["DeadliftSpecialist"],
  BenchDeadlift = female_coeffs["BenchSpecialist"] + female_coeffs["DeadliftSpecialist"]
)


# 8.5. Combined variances for two movement specialists.

male_combined_variances <- c(
  SquatBench = male_variances["SquatSpecialist"] + male_variances["BenchSpecialist"],
  SquatDeadlift = male_variances["SquatSpecialist"] + male_variances["DeadliftSpecialist"],
  BenchDeadlift = male_variances["BenchSpecialist"] + male_variances["DeadliftSpecialist"]
)

female_combined_variances <- c(
  SquatBench = female_variances["SquatSpecialist"] + female_variances["BenchSpecialist"],
  SquatDeadlift = female_variances["SquatSpecialist"] + female_variances["DeadliftSpecialist"],
  BenchDeadlift = female_variances["BenchSpecialist"] + female_variances["DeadliftSpecialist"]
)


# 8.6. Calculate p-values for two movement specialists.

male_combined_pvalues <- pnorm(abs(male_combined / sqrt(male_combined_variances)), lower.tail = FALSE) * 2
female_combined_pvalues <- pnorm(abs(female_combined / sqrt(female_combined_variances)), lower.tail = FALSE) * 2


# 8.7. Calculate p-values for one movement specialists.

male_pvalues <- pnorm(abs(male_coeffs / sqrt(male_variances)), lower.tail = FALSE) * 2
female_pvalues <- pnorm(abs(female_coeffs / sqrt(female_variances)), lower.tail = FALSE) * 2


# 8.9. Renaming variables to avoid bugs on table.

names(male_combined) <- c("SquatBench", "SquatDeadlift", "BenchDeadlift")
names(female_combined) <- c("SquatBench", "SquatDeadlift", "BenchDeadlift")
names(male_combined_variances) <- c("SquatBench", "SquatDeadlift", "BenchDeadlift")
names(female_combined_variances) <- c("SquatBench", "SquatDeadlift", "BenchDeadlift")
names(male_combined_pvalues) <- names(male_combined)
names(female_combined_pvalues) <- names(female_combined)


# 8.10. Summary of coefficients and p values by movement specialty.

summary_table <- data.frame(
  Specialist_Type = c(
    "Balanced Specialist", 
    "Squat Specialist", 
    "Bench Specialist", 
    "Deadlift Specialist", 
    "Squat & Bench Specialist", 
    "Squat & Deadlift Specialist", 
    "Bench & Deadlift Specialist"
  ),
  Male_Coefficients = c(
    male_coeffs["BalancedSpecialist"],
    male_coeffs["SquatSpecialist"],
    male_coeffs["BenchSpecialist"],
    male_coeffs["DeadliftSpecialist"],
    male_combined["SquatBench"],
    male_combined["SquatDeadlift"],
    male_combined["BenchDeadlift"]
  ),
  Male_pvalues = c(
    male_pvalues["BalancedSpecialist"],
    male_pvalues["SquatSpecialist"],
    male_pvalues["BenchSpecialist"],
    male_pvalues["DeadliftSpecialist"],
    male_combined_pvalues["SquatBench"],
    male_combined_pvalues["SquatDeadlift"],
    male_combined_pvalues["BenchDeadlift"]
  ),
  Female_Coefficients = c(
    female_coeffs["BalancedSpecialist"],
    female_coeffs["SquatSpecialist"],
    female_coeffs["BenchSpecialist"],
    female_coeffs["DeadliftSpecialist"],
    female_combined["SquatBench"],
    female_combined["SquatDeadlift"],
    female_combined["BenchDeadlift"]
  ),
  Female_pvalues = c(
    female_pvalues["BalancedSpecialist"],
    female_pvalues["SquatSpecialist"],
    female_pvalues["BenchSpecialist"],
    female_pvalues["DeadliftSpecialist"],
    female_combined_pvalues["SquatBench"],
    female_combined_pvalues["SquatDeadlift"],
    female_combined_pvalues["BenchDeadlift"]
  )
)

print(summary_table)


# 8.11. Coefficients plot.

# 8.11.1. Specialist types ordered.

specialist_order <- c(
  "Balanced Specialist", 
  "Squat Specialist", 
  "Bench Specialist", 
  "Deadlift Specialist", 
  "Squat & Bench Specialist", 
  "Squat & Deadlift Specialist", 
  "Bench & Deadlift Specialist"
)

# 8.11.2. Melting and labeling.

summary_table_melted <- melt(
  summary_table[, c("Specialist_Type", "Male_Coefficients", "Female_Coefficients")], 
  id.vars = "Specialist_Type", 
  variable.name = "Sex", 
  value.name = "Coefficient"
)


summary_table_melted$Sex <- recode(summary_table_melted$Sex, 
                                   Male_Coefficients = "Male", 
                                   Female_Coefficients = "Female")

# 8.11.3. Significance criteria. 

summary_table_melted <- summary_table_melted %>%
  mutate(Significance = case_when(
    Sex == "Male" & summary_table$Male_pvalues < 0.05 ~ "***",
    Sex == "Male" & summary_table$Male_pvalues < 0.1 ~ "**",
    Sex == "Female" & summary_table$Female_pvalues < 0.05 ~ "***",
    Sex == "Female" & summary_table$Female_pvalues < 0.1 ~ "**",
    TRUE ~ "*"
  ))


summary_table_melted$Specialist_Type <- factor(
  summary_table_melted$Specialist_Type,
  levels = specialist_order
)

# 8.11.4. Coefficients plot with significance criteria.

main_plot <- ggplot(summary_table_melted, aes(x = Specialist_Type, y = Coefficient, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Significance), 
            position = position_dodge(width = 0.9), 
            vjust = 1,
            hjust = -1,
            size = 4, 
            color = "black") +
  coord_flip() +
  scale_fill_brewer(palette = "Set2", name = "Sex") +  
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    panel.grid = element_blank(),  
    plot.background = element_rect(fill = "white", color = NA),  
    axis.text.x = element_text(angle = 45, hjust = 1),  
    legend.title = element_blank(),
    legend.position = "right"
  ) +
  labs(
    title = "Coefficients of Specialist Types by Sex with Significance Levels",
    x = "Specialist Type",
    y = "Coefficient",
    fill = "Sex"
  )


annotation_box <- ggplot() +
  annotate(
    "text", x = 0.5, y = 0.5, 
    label = "* No statistical significance\n** Weak statistical significance\n*** Strong statistical significance", 
    size = 2, hjust = 0
  ) +
  theme_void() +
  theme(
    plot.margin = margin(t = 0, r = 10, b = 10, l = 10)  
  )

final_plot <- plot_grid(
  main_plot, 
  annotation_box, 
  ncol = 1, 
  rel_heights = c(3, 0.4)
)

final_plot


##### 9. Coefficients of feature importance by best attempt per movement.

# 9.1. Coefficients by movement.

male_coeffs_1 <- c(
  Best3SquatKg = 3.324e-02,
  Best3BenchKg =  -4.981e-03,
  Best3DeadliftKg = 3.233e-02
)

female_coeffs_1 <- c(
  Best3SquatKg = 3.760e-02,
  Best3BenchKg =  2.141e-02,
  Best3DeadliftKg = 4.310e-02
)


# 9.2. Standard errors by movement. 

male_standard_errors_1 <- c(
  Best3SquatKg = 3.306e-03,
  Best3BenchKg =  4.476e-03,
  Best3DeadliftKg = 1.332e-03
)

female_standard_errors_1 <- c(
  Best3SquatKg = 2.634e-02,
  Best3BenchKg =  4.643e-02,
  Best3DeadliftKg = 2.543e-03
)


# 9.3. Calculating variances using standard errors.

male_variances_1 <- male_standard_errors_1^2
female_variances_1 <- female_standard_errors_1^2

# 9.4. Calculate p-values for one movement specialists.

male_pvalues_1 <- pnorm(abs(male_coeffs_1 / sqrt(male_variances_1)), lower.tail = FALSE) * 2
female_pvalues_1 <- pnorm(abs(female_coeffs_1 / sqrt(female_variances_1)), lower.tail = FALSE) * 2


# 9.5. Summary of coefficients and p values by best attempt by movement.

summary_table_1 <- data.frame(
  Movement_Attempt = c(
    "Best Squat Attempt", 
    "Best Bench Attempt", 
    "Best Deadlift Attempt"
  ),
  Male_Coefficients_1 = c(
    male_coeffs_1["Best3SquatKg"],
    male_coeffs_1["Best3BenchKg"],
    male_coeffs_1["Best3DeadliftKg"]
  ),
  Male_pvalues_1 = c(
    male_pvalues_1["Best3SquatKg"],
    male_pvalues_1["Best3BenchKg"],
    male_pvalues_1["Best3DeadliftKg"]
  ),
  Female_Coefficients_1 = c(
    female_coeffs_1["Best3SquatKg"],
    female_coeffs_1["Best3BenchKg"],
    female_coeffs_1["Best3DeadliftKg"]
  ),
  Female_pvalues_1 = c(
    female_pvalues_1["Best3SquatKg"],
    female_pvalues_1["Best3BenchKg"],
    female_pvalues_1["Best3DeadliftKg"]
  )
)

print(summary_table_1)


# 9.6. Coefficients plot.

# 9.6.1. Specialist types ordered.

movement_attempt_order <- c(
  "Best Squat Attempt", 
  "Best Bench Attempt", 
  "Best Deadlift Attempt"
)

# 9.6.2. Melting and labeling.

summary_table_melted_1 <- melt(
  summary_table_1[, c("Movement_Attempt", "Male_Coefficients_1", "Female_Coefficients_1")], 
  id.vars = "Movement_Attempt", 
  variable.name = "Sex", 
  value.name = "Coefficient"
)


summary_table_melted_1$Sex <- recode(summary_table_melted_1$Sex, 
                                   Male_Coefficients_1 = "Male", 
                                   Female_Coefficients_1 = "Female")

# 9.6.3. Significance criteria. 

summary_table_melted_1 <- summary_table_melted_1 %>%
  mutate(PValue = case_when(
    Sex == "Male" ~ summary_table_1$Male_pvalues_1[match(Movement_Attempt, summary_table_1$Movement_Attempt)],
    Sex == "Female" ~ summary_table_1$Female_pvalues_1[match(Movement_Attempt, summary_table_1$Movement_Attempt)]
  ))

summary_table_melted_1 <- summary_table_melted_1 %>%
  mutate(Significance = case_when(
    PValue < 0.05 ~ "***",
    PValue < 0.1 ~ "**",
    TRUE ~ "*"
  ))

summary_table_melted_1$Movement_Attempt <- factor(
  summary_table_melted_1$Movement_Attempt,
  levels = movement_attempt_order
)

# 9.6.4. Coefficients plot with significance criteria.

main_plot_1 <- ggplot(summary_table_melted_1, aes(x = Movement_Attempt, y = Coefficient, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Significance), 
            position = position_dodge(width = 0.9), 
            vjust = 1,
            hjust = -1,
            size = 6, 
            color = "black") +
  coord_flip() +
  scale_fill_brewer(palette = "Set2", name = "Sex") +  
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    panel.grid = element_blank(),  
    plot.background = element_rect(fill = "white", color = NA),  
    axis.text.x = element_text(angle = 45, hjust = 1),  
    legend.title = element_blank(),
    legend.position = "right"
  ) +
  labs(
    title = "Coefficients of Best Movement Attempts by Sex with Significance Levels",
    x = "Best Movement Attempt",
    y = "Coefficient",
    fill = "Sex"
  )

annotation_box <- ggplot() +
  annotate(
    "text", x = 0.5, y = 0.5, 
    label = "* No statistical significance\n** Weak statistical significance\n*** Strong statistical significance", 
    size = 2, hjust = 0
  ) +
  theme_void() +
  theme(
    plot.margin = margin(t = 0, r = 10, b = 10, l = 10)  
  )

final_plot_1 <- plot_grid(
  main_plot_1, 
  annotation_box, 
  ncol = 1, 
  rel_heights = c(3, 0.4)
)

final_plot_1
  

