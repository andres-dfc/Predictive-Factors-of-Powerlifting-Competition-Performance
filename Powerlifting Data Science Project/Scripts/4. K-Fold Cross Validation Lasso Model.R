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

openpowerliftingdata <- read_csv("C:/Users/andre/OneDrive/Escritorio/TFM/openpowerliftingdata.csv")

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


##### 6. K-Fold Cross Validation.

ipf_male_data_clean <- ipf_male_data_clean %>%
  mutate(Podium = ifelse(Place %in% c(1, 2, 3), 1, 0))

ipf_female_data_clean <- ipf_female_data_clean %>%
  mutate(Podium = ifelse(Place %in% c(1, 2, 3), 1, 0))


# 6.1. Folds and reproducibility.

k_folds <- 5
set.seed(123)


# 6.2. Cross validation.

# Male

cv_results_male <- data.frame(Fold = 1:k_folds, Accuracy = NA, AUC = NA)

folds_male <- createFolds(ipf_male_data_clean$Podium, k = k_folds, list = TRUE)


for (i in seq_along(folds_male)) {
  
  train_indices_male <- setdiff(1:nrow(ipf_male_data_clean), folds_male[[i]])
  train_data_male <- ipf_male_data_clean[train_indices_male, ] %>%
    filter(Equipment != "Wraps") %>%
    filter(!is.na(WeightClassKg) & !is.na(Division) & !is.na(Equipment) &
             !is.na(Best3SquatKg) & !is.na(Best3BenchKg) & !is.na(Best3DeadliftKg) &
             !is.na(SquatSpecialist) & !is.na(BenchSpecialist) & !is.na(DeadliftSpecialist) & !is.na(BalancedSpecialist)) %>%
    droplevels()
  
  test_data_male <- ipf_male_data_clean[folds_male[[i]], ] %>%
    filter(Equipment != "Wraps") %>%
    filter(!is.na(WeightClassKg) & !is.na(Division) & !is.na(Equipment) &
             !is.na(Best3SquatKg) & !is.na(Best3BenchKg) & !is.na(Best3DeadliftKg) &
             !is.na(SquatSpecialist) & !is.na(BenchSpecialist) & !is.na(DeadliftSpecialist) & !is.na(BalancedSpecialist)) %>%
    droplevels()
  
  
  model_lasso_male <- glm(Podium ~ Equipment + WeightClassKg + Division + 
                 Best3SquatKg + Best3BenchKg + Best3DeadliftKg +
                 SquatSpecialist + BenchSpecialist + DeadliftSpecialist + BalancedSpecialist,
               family = binomial, data = train_data_male)
  
  
  predictions_male <- predict(model_lasso_male, newdata = test_data_male, type = "response")
  predicted_classes_male <- ifelse(predictions_male > 0.4206642, 1, 0)
  
  
  accuracy_male <- mean(predicted_classes_male == test_data_male$Podium)
  pred_obj_male <- prediction(predictions_male, test_data_male$Podium)
  auc_male <- performance(pred_obj_male, measure = "auc")@y.values[[1]]
  
  
  cv_results_male[i, ] <- c(i, accuracy_male, auc_male)
}


# Cross validation male results.

print(cv_results_male)
cat("Average Accuracy (Male):", mean(cv_results_male$Accuracy), "\n")
cat("Average AUC (Male):", mean(cv_results_male$AUC), "\n")



# Female

cv_results_female <- data.frame(Fold = 1:k_folds, Accuracy = NA, AUC = NA)

folds_female <- createFolds(ipf_female_data_clean$Podium, k = k_folds, list = TRUE)


for (i in seq_along(folds_female)) {
  
  train_indices_female <- setdiff(1:nrow(ipf_female_data_clean), folds_female[[i]])
  train_data_female <- ipf_female_data_clean[train_indices_female, ] %>%
    filter(!is.na(WeightClassKg) & !is.na(Division) & !is.na(Equipment) &
             !is.na(Best3SquatKg) & !is.na(Best3BenchKg) & !is.na(Best3DeadliftKg) &
             !is.na(SquatSpecialist) & !is.na(BenchSpecialist) & !is.na(DeadliftSpecialist) & !is.na(BalancedSpecialist)) %>%
    droplevels()
  
  test_data_female <- ipf_female_data_clean[folds_female[[i]], ] %>%
    filter(!is.na(WeightClassKg) & !is.na(Division) & !is.na(Equipment) &
             !is.na(Best3SquatKg) & !is.na(Best3BenchKg) & !is.na(Best3DeadliftKg) &
             !is.na(SquatSpecialist) & !is.na(BenchSpecialist) & !is.na(DeadliftSpecialist) & !is.na(BalancedSpecialist)) %>%
    droplevels()
  
  
  model_lasso_female <- glm(Podium ~ Equipment + WeightClassKg + Division + 
                 Best3SquatKg + Best3BenchKg + Best3DeadliftKg +
                 SquatSpecialist + BenchSpecialist + DeadliftSpecialist + BalancedSpecialist,
               family = binomial, data = train_data_female)
  
  
  predictions_female <- predict(model_lasso_female, newdata = test_data_female, type = "response")
  predicted_classes_female <- ifelse(predictions_female > 0.4996129, 1, 0)  
  
  
  accuracy_female <- mean(predicted_classes_female == test_data_female$Podium)
  pred_obj_female <- prediction(predictions_female, test_data_female$Podium)
  auc_female <- performance(pred_obj_female, measure = "auc")@y.values[[1]]
  
  
  cv_results_female[i, ] <- c(i, accuracy_female, auc_female)
}


# Cross validation female results.

print(cv_results_female)
cat("Average Accuracy (Female):", mean(cv_results_female$Accuracy), "\n")
cat("Average AUC (Female):", mean(cv_results_female$AUC), "\n")
