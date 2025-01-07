##### 1. Libraries

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)


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

# Some of the values in the proportions are not available. Instead of removing this rows, we could fill them with values to optimize the use of data.
# First, we verify NA values are statistically insignificant compared to the data.

# Hypothesis contrast

# H0: Proportion of rows with NA values = 0
# HA: Proportion of rows with NA values > 0

total_rows_male <- nrow(ipf_male_data_clean)
total_rows_female <- nrow(ipf_female_data_clean)

na_rows_male <- sum(is.na(ipf_male_data_clean$SquatProportion) | 
                      is.na(ipf_male_data_clean$BenchPressProportion) | 
                      is.na(ipf_male_data_clean$DeadliftProportion))

na_rows_female <- sum(is.na(ipf_female_data_clean$SquatProportion) | 
                        is.na(ipf_female_data_clean$BenchPressProportion) | 
                        is.na(ipf_female_data_clean$DeadliftProportion))

proportion_na_male <- na_rows_male / total_rows_male
proportion_na_female <- na_rows_female / total_rows_female

cat("Proportion of rows with NA values (Male):", proportion_na_male, "\n")
cat("Proportion of rows with NA values (Female):", proportion_na_female, "\n")

binom_test_male <- binom.test(na_rows_male, total_rows_male, p = 0)
binom_test_female <- binom.test(na_rows_female, total_rows_female, p = 0)

cat("Binomial test for NA proportion (Male):\n")
print(binom_test_male)

cat("\nBinomial test for NA proportion (Female):\n")
print(binom_test_female)


# Bootstrap test

set.seed(123) 

calculate_na_proportion <- function(data, total_rows) {
  na_rows <- data %>%
    filter(is.na(SquatProportion) | is.na(BenchPressProportion) | is.na(DeadliftProportion)) %>%
    nrow()
  return(na_rows / total_rows)
}

n_bootstrap <- 1000

male_na_proportions <- numeric(n_bootstrap)
female_na_proportions <- numeric(n_bootstrap)

for (i in 1:n_bootstrap) {
  
  male_sample <- ipf_male_data_clean %>% sample_frac(replace = TRUE)
  female_sample <- ipf_female_data_clean %>% sample_frac(replace = TRUE)
  
  
  male_na_proportions[i] <- calculate_na_proportion(male_sample, nrow(male_sample))
  female_na_proportions[i] <- calculate_na_proportion(female_sample, nrow(female_sample))
}

male_ci <- quantile(male_na_proportions, c(0.025, 0.975))
female_ci <- quantile(female_na_proportions, c(0.025, 0.975))

cat("95% CI for proportion of NA values (Male):", male_ci, "\n")
cat("95% CI for proportion of NA values (Female):", female_ci, "\n")

# Conclusion: Statistically and practically, NA values represent an insignificant amount of the data.


# 5.2. Data will be filled with the average values of the proportions, since the data has a normal distribution.

# Male proportions distribution

male_proportions_long <- ipf_male_data_clean %>%
  select(SquatProportion, BenchPressProportion, DeadliftProportion) %>%
  pivot_longer(cols = everything(), names_to = "Proportion", values_to = "Value")

ggplot(male_proportions_long, aes(x = Value, fill = Proportion, color = Proportion)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plots of Proportions for Male Athletes",
       x = "Proportion",
       y = "Density") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") + 
  scale_color_brewer(palette = "Set2") +
  theme(
    panel.grid = element_blank(),   
  )


# Female proportions distribution

female_proportions_long <- ipf_female_data_clean %>%
  select(SquatProportion, BenchPressProportion, DeadliftProportion) %>%
  pivot_longer(cols = everything(), names_to = "Proportion", values_to = "Value")

ggplot(female_proportions_long, aes(x = Value, fill = Proportion, color = Proportion)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plots of Proportions for Female Athletes",
       x = "Proportion",
       y = "Density") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") + 
  scale_color_brewer(palette = "Set2") +
  theme(
    panel.grid = element_blank(),   
  )

# 5.3. Proportions must be normalized, since the sum of averages can be different from 1.

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


# 5.4. Checking if proportions were normalized correctly.


ipf_male_data_clean <- ipf_male_data_clean %>%
  mutate(
    ProportionsSum = SquatProportion + BenchPressProportion + DeadliftProportion,
    IsProportionsEqualToOne = ifelse(abs(ProportionsSum - 1) < 1e-6, TRUE, FALSE) 
  ) %>%
  select(-ProportionsSum, -IsProportionsEqualToOne)

ipf_female_data_clean <- ipf_female_data_clean %>%
  mutate(
    ProportionsSum = SquatProportion + BenchPressProportion + DeadliftProportion,
    IsProportionsEqualToOne = ifelse(abs(ProportionsSum - 1) < 1e-6, TRUE, FALSE) 
  ) %>%
  select(-ProportionsSum, -IsProportionsEqualToOne)


# 5.5. Correlation matrix for the proportions between the three movements.


correlation_matrix_male <- ipf_male_data_clean %>%
  select(SquatProportion, BenchPressProportion, DeadliftProportion) %>%
  cor()

correlation_matrix_female <- ipf_female_data_clean %>%
  select(SquatProportion, BenchPressProportion, DeadliftProportion) %>%
  cor()

print(correlation_matrix_female)
print(correlation_matrix_male)


# 5.6. Average proportions and standard deviations by weight class, division, equipment, sex, competition and year.


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


# 5.7. Transferring average proportions and standard deviations to the datasets.


ipf_male_data_clean <- ipf_male_data_clean %>%
  left_join(average_proportions_male, by = c("WeightClassKg", "Division", "Equipment", "Sex", "MeetName", "Date"))

ipf_female_data_clean <- ipf_female_data_clean %>%
  left_join(average_proportions_female, by = c("WeightClassKg", "Division", "Equipment", "Sex", "MeetName", "Date"))


# 5.8. Classifying athletes' specialty movement.


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


# 5.8. Verifying movement specialists in the podiums.

ipf_male_data_clean <- ipf_male_data_clean %>%
  mutate(
    FirstPlaceSQSpecialist = ifelse(Place == 1 & SquatSpecialist >= 1, 1, 0),
    SecondPlaceSQSpecialist = ifelse(Place == 2 & SquatSpecialist >= 1, 1, 0),
    ThirdPlaceSQSpecialist = ifelse(Place == 3 & SquatSpecialist >= 1, 1, 0),
    FirstPlaceBPSpecialist = ifelse(Place == 1 & BenchSpecialist >= 1, 1, 0),
    SecondPlaceBPSpecialist = ifelse(Place == 2 & BenchSpecialist >= 1, 1, 0),
    ThirdPlaceBPSpecialist = ifelse(Place == 3 & BenchSpecialist >= 1, 1, 0),
    FirstPlaceDLSpecialist = ifelse(Place == 1 & DeadliftSpecialist >= 1, 1, 0),
    SecondPlaceDLSpecialist = ifelse(Place == 2 & DeadliftSpecialist >= 1, 1, 0),
    ThirdPlaceDLSpecialist = ifelse(Place == 3 & DeadliftSpecialist >= 1, 1, 0),
    FirstPlaceBalancedSpecialist = ifelse(Place == 1 & BalancedSpecialist >= 1, 1, 0),
    SecondPlaceBalancedSpecialist = ifelse(Place == 2 & BalancedSpecialist >= 1, 1, 0),
    ThirdPlaceBalancedSpecialist = ifelse(Place == 3 & BalancedSpecialist >= 1, 1, 0)
  )

ipf_female_data_clean <- ipf_female_data_clean %>%
  mutate(
    FirstPlaceSQSpecialist = ifelse(Place == 1 & SquatSpecialist >= 1, 1, 0),
    SecondPlaceSQSpecialist = ifelse(Place == 2 & SquatSpecialist >= 1, 1, 0),
    ThirdPlaceSQSpecialist = ifelse(Place == 3 & SquatSpecialist >= 1, 1, 0),
    FirstPlaceBPSpecialist = ifelse(Place == 1 & BenchSpecialist >= 1, 1, 0),
    SecondPlaceBPSpecialist = ifelse(Place == 2 & BenchSpecialist >= 1, 1, 0),
    ThirdPlaceBPSpecialist = ifelse(Place == 3 & BenchSpecialist >= 1, 1, 0),
    FirstPlaceDLSpecialist = ifelse(Place == 1 & DeadliftSpecialist >= 1, 1, 0),
    SecondPlaceDLSpecialist = ifelse(Place == 2 & DeadliftSpecialist >= 1, 1, 0),
    ThirdPlaceDLSpecialist = ifelse(Place == 3 & DeadliftSpecialist >= 1, 1, 0),
    FirstPlaceBalancedSpecialist = ifelse(Place == 1 & BalancedSpecialist >= 1, 1, 0),
    SecondPlaceBalancedSpecialist = ifelse(Place == 2 & BalancedSpecialist >= 1, 1, 0),
    ThirdPlaceBalancedSpecialist = ifelse(Place == 3 & BalancedSpecialist >= 1, 1, 0)
  )


# 5.9. Number of athletes by movement specialty in the overall competitions' podiums.

# Male

podiums_specialty_male <- ipf_male_data_clean %>%
  group_by(WeightClassKg, Division, Equipment, Sex, MeetName) %>%
  summarise(
    SQSpecialists = sum(FirstPlaceSQSpecialist == 1 | SecondPlaceSQSpecialist == 1 | ThirdPlaceSQSpecialist == 1, na.rm = TRUE),
    BPSpecialists = sum(FirstPlaceBPSpecialist == 1 | SecondPlaceBPSpecialist == 1 | ThirdPlaceBPSpecialist == 1, na.rm = TRUE),
    DLSpecialists = sum(FirstPlaceDLSpecialist == 1 | SecondPlaceDLSpecialist == 1 | ThirdPlaceDLSpecialist == 1, na.rm = TRUE),
    BalancedSpecialists = sum(FirstPlaceBalancedSpecialist == 1 | SecondPlaceBalancedSpecialist == 1 | ThirdPlaceBalancedSpecialist == 1, na.rm = TRUE)
  ) %>%
  ungroup()

# Female

podiums_specialty_female <- ipf_female_data_clean %>%
  group_by(WeightClassKg, Division, Equipment, Sex, MeetName) %>%
  summarise(
    SQSpecialists = sum(FirstPlaceSQSpecialist == 1 | SecondPlaceSQSpecialist == 1 | ThirdPlaceSQSpecialist == 1, na.rm = TRUE),
    BPSpecialists = sum(FirstPlaceBPSpecialist == 1 | SecondPlaceBPSpecialist == 1 | ThirdPlaceBPSpecialist == 1, na.rm = TRUE),
    DLSpecialists = sum(FirstPlaceDLSpecialist == 1 | SecondPlaceDLSpecialist == 1 | ThirdPlaceDLSpecialist == 1, na.rm = TRUE),
    BalancedSpecialists = sum(FirstPlaceBalancedSpecialist == 1 | SecondPlaceBalancedSpecialist == 1 | ThirdPlaceBalancedSpecialist == 1, na.rm = TRUE)
  ) %>%
  ungroup()


# 5.10. Overall athletes by movement specialty.


# Male

overall_summary_male <- ipf_male_data_clean %>%
  summarise(
    Squat = sum(SquatSpecialist, na.rm = TRUE),
    Bench = sum(BenchSpecialist, na.rm = TRUE),
    Deadlift = sum(DeadliftSpecialist, na.rm = TRUE),
    Balanced = sum(BalancedSpecialist, na.rm = TRUE)
  ) %>%
  mutate(Sex = "Male")


# Female

overall_summary_female <- ipf_female_data_clean %>%
  summarise(
    Squat = sum(SquatSpecialist, na.rm = TRUE),
    Bench = sum(BenchSpecialist, na.rm = TRUE),
    Deadlift = sum(DeadliftSpecialist, na.rm = TRUE),
    Balanced = sum(BalancedSpecialist, na.rm = TRUE)
  ) %>%
  mutate(Sex = "Female")


overall_combined_summary <- bind_rows(overall_summary_male, overall_summary_female)


overall_combined_long <- overall_combined_summary %>%
  pivot_longer(cols = c(Squat, Bench, Deadlift, Balanced),
               names_to = "Metric", values_to = "Value")


ggplot(overall_combined_long, aes(x = Metric, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Sex, scales = "free_y") +
  labs(title = "Number of Athletes by Movement Specialty",
       x = "Movement Specialty",
       y = "Total Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Dark2") +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.border = element_blank(),      
  )


# 5.11. Number of athletes in the podiums by movement specialty.


# Male

podiums_summary_male <- podiums_specialty_male %>%
  summarise(
    Squat = sum(SQSpecialists, na.rm = TRUE),
    Bench = sum(BPSpecialists, na.rm = TRUE),
    Deadlift = sum(DLSpecialists, na.rm = TRUE),
    Balanced = sum(BalancedSpecialists, na.rm = TRUE)
  ) %>%
  mutate(Sex = "Male")


# Female

podiums_summary_female <- podiums_specialty_female %>%
  summarise(
    Squat = sum(SQSpecialists, na.rm = TRUE),
    Bench = sum(BPSpecialists, na.rm = TRUE),
    Deadlift = sum(DLSpecialists, na.rm = TRUE),
    Balanced = sum(BalancedSpecialists, na.rm = TRUE)
  ) %>%
  mutate(Sex = "Female")


podiums_combined_summary <- bind_rows(podiums_summary_male, podiums_summary_female)


podiums_combined_long <- podiums_combined_summary %>%
  pivot_longer(cols = c(Squat, Bench, Deadlift, Balanced),
               names_to = "Metric", values_to = "Value")


ggplot(podiums_combined_long, aes(x = Metric, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Sex, scales = "free_y") +
  labs(title = "Number of Athletes in the Podiums by Movement Specialty",
       x = "Movement Specialty",
       y = "Total Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Dark2") +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.border = element_blank(),      
  )