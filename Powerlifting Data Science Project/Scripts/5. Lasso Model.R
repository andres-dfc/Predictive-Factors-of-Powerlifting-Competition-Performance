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


##### 6. Model regularization: Lasso model


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



# 6.3. Model set up.

# Male

x_male_train <- model.matrix(Podium ~ Equipment + Division + WeightClassKg +
                               Best3SquatKg + Best3BenchKg + Best3DeadliftKg +
                               SquatSpecialist + BenchSpecialist + DeadliftSpecialist + BalancedSpecialist,
                             data = train_data_male)[, -1]

y_male_train <- as.factor(train_data_male$Podium)

# Female

x_female_train <- model.matrix(Podium ~ Equipment + Division + WeightClassKg +
                                 Best3SquatKg + Best3BenchKg + Best3DeadliftKg +
                                 SquatSpecialist + BenchSpecialist + DeadliftSpecialist + BalancedSpecialist,
                               data = train_data_female)[, -1]

y_female_train <- as.factor(train_data_female$Podium)


# 6.4. Regularization: model adjustment.

# Male

male_lasso_cv <- cv.glmnet(x_male_train, y_male_train, alpha = 1, family = "binomial")

plot(male_lasso_cv)

male_best_lambda <- male_lasso_cv$lambda.min
print(male_best_lambda)

# Female

female_lasso_cv <- cv.glmnet(x_female_train, y_female_train, alpha = 1, family = "binomial")

plot(female_lasso_cv)

female_best_lambda <- female_lasso_cv$lambda.min
print(female_best_lambda)



# 6.5. Examine coefficients.

# Male

coef(male_lasso_cv, s = "lambda.min")


x_male_test <- model.matrix(Podium ~ Equipment + Division + WeightClassKg +
                              Best3SquatKg + Best3BenchKg + Best3DeadliftKg +
                              SquatSpecialist + BenchSpecialist + DeadliftSpecialist + BalancedSpecialist, 
                            data = test_data_male)[, -1]

# Female

coef(female_lasso_cv, s = "lambda.min")


x_female_test <- model.matrix(Podium ~ Equipment + Division + WeightClassKg +
                                Best3SquatKg + Best3BenchKg + Best3DeadliftKg +
                                SquatSpecialist + BenchSpecialist + DeadliftSpecialist + BalancedSpecialist, 
                              data = test_data_female)[, -1]



# 6.6. Probabilities and class probabilities.

male_lasso_probs <- predict(male_lasso_cv, s = male_best_lambda, newx = x_male_test, type = "response")

female_lasso_probs <- predict(female_lasso_cv, s = female_best_lambda, newx = x_female_test, type = "response")


male_lasso_probs <- as.numeric(male_lasso_probs)
female_lasso_probs <- as.numeric(female_lasso_probs)


male_lasso_predicted_classes <- ifelse(male_lasso_probs > 0.443093742039308, 1, 0)
female_lasso_predicted_classes <- ifelse(female_lasso_probs > 0.532546730602538, 1, 0)

male_lasso_predicted_classes <- factor(male_lasso_predicted_classes, levels = c(0, 1))
female_lasso_predicted_classes <- factor(female_lasso_predicted_classes, levels = c(0, 1))

confusionMatrix(as.factor(male_lasso_predicted_classes), as.factor(test_data_male$Podium))
confusionMatrix(as.factor(female_lasso_predicted_classes), as.factor(test_data_female$Podium))


# 6.7. Model accuracy.

male_lasso_accuracy <- mean(male_lasso_predicted_classes == test_data_male$Podium)
print(paste("Lasso Accuracy (Male): ", male_lasso_accuracy))

female_lasso_accuracy <- mean(female_lasso_predicted_classes == test_data_female$Podium)
print(paste("Lasso Accuracy (Female): ", female_lasso_accuracy))


# 6.8. Threshold optimization for Lasso model.

# Male

roc_male <- roc(test_data_male$Podium, male_lasso_probs)  
optimal_threshold_male <- coords(roc_male, "best", ret = "threshold", best.method = "youden")
print(paste("Optimal Threshold (Male): ", optimal_threshold_male))


male_lasso_predicted_classes_opt <- ifelse(male_lasso_probs > as.numeric(optimal_threshold_male), 1, 0)


confusionMatrix(as.factor(male_lasso_predicted_classes_opt), as.factor(test_data_male$Podium))
male_lasso_accuracy_opt <- mean(male_lasso_predicted_classes_opt == test_data_male$Podium)
print(paste("Lasso Accuracy (Male) with Optimal Threshold: ", male_lasso_accuracy_opt))


# Female

roc_female <- roc(test_data_female$Podium, female_lasso_probs)  
optimal_threshold_female <- coords(roc_female, "best", ret = "threshold", best.method = "youden")
print(paste("Optimal Threshold (Female): ", optimal_threshold_female))


female_lasso_predicted_classes_opt <- ifelse(female_lasso_probs > as.numeric(optimal_threshold_male), 1, 0)


confusionMatrix(as.factor(female_lasso_predicted_classes_opt), as.factor(test_data_female$Podium))
female_lasso_accuracy_opt <- mean(female_lasso_predicted_classes_opt == test_data_female$Podium)
print(paste("Lasso Accuracy (Female) with Optimal Threshold: ", female_lasso_accuracy_opt))


# 6.9. Lasso graphs.


# 6.9.1. Coefficient Path.

plot(male_lasso_cv$glmnet.fit, xvar = "lambda", label = TRUE)
title("Male Lasso Coefficient Path")

plot(female_lasso_cv$glmnet.fit, xvar = "lambda", label = TRUE)
title("Female Lasso Coefficient Path")


# 6.9.2. ROC Curves.

male_roc <- roc(test_data_male$Podium, male_lasso_probs)
plot(male_roc, col = "blue", main = "ROC Curve - Male Lasso")

female_roc <- roc(test_data_female$Podium, female_lasso_probs)
plot(female_roc, col = "red", main = "ROC Curve - Female Lasso")


# 6.9.3. PR Curves.

lasso_male_pr <- pr.curve(scores.class0 = male_lasso_probs, weights.class0 = test_data_male$Podium, curve = TRUE)
plot(lasso_male_pr, main = "Precision-Recall Curve - Male Lasso")

lasso_female_pr <- pr.curve(scores.class0 = female_lasso_probs, weights.class0 = test_data_female$Podium, curve = TRUE)
plot(lasso_female_pr, main = "Precision-Recall Curve - Female Lasso")


# 6.9.4. Confusion matrix. 

lasso_male_cm <- confusionMatrix(as.factor(male_lasso_predicted_classes), as.factor(test_data_male$Podium))
print(lasso_male_cm)

lasso_female_cm <- confusionMatrix(as.factor(female_lasso_predicted_classes), as.factor(test_data_female$Podium))
print(lasso_female_cm)

draw_confusion_matrix <- function(cm, title) {
  as.data.frame(cm$table) %>%
    ggplot(aes(x = Reference, y = Prediction)) +
    geom_tile(aes(fill = Freq), color = "white") +
    geom_text(aes(label = Freq), vjust = 1) +
    scale_fill_gradient(low = "white", high = "blue") +
    labs(title = title, fill = "Frequency")
}

draw_confusion_matrix(lasso_male_cm, "Confusion Matrix - Male Lasso")
draw_confusion_matrix(lasso_female_cm, "Confusion Matrix - Female Lasso")


# 6.9.5. Feature Importance Plot

# Male 

male_coefficients <- coef(male_lasso_cv, s = "lambda.min")
male_coeff_df <- as.data.frame(as.matrix(male_coefficients))
colnames(male_coeff_df) <- c("Coefficient")
male_coeff_df$Variable <- rownames(male_coeff_df)

# Female

female_coefficients <- coef(female_lasso_cv, s = "lambda.min")
female_coeff_df <- as.data.frame(as.matrix(female_coefficients))
colnames(female_coeff_df) <- c("Coefficient")
female_coeff_df$Variable <- rownames(female_coeff_df)


# 6.9.6. Filtering values different from 0.

male_coeff_df <- male_coeff_df[male_coeff_df$Coefficient != 0, ]

female_coeff_df <- female_coeff_df[female_coeff_df$Coefficient != 0, ]


ggplot(male_coeff_df, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance - Male Lasso", x = "Variable", y = "Coefficient") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),   
    panel.border = element_blank(), 
    plot.background = element_rect(fill = "white", color = NA), 
    panel.background = element_rect(fill = "white", color = NA) 
  )

# Female Lasso Plot
ggplot(female_coeff_df, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_flip() +
  labs(title = "Variable Importance - Female Lasso", x = "Variable", y = "Coefficient") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),   
    panel.border = element_blank(), 
    plot.background = element_rect(fill = "white", color = NA), 
    panel.background = element_rect(fill = "white", color = NA) 
  )
