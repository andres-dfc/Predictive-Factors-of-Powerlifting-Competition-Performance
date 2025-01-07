# 1. Libraries

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

# 2. Data loading

openpowerliftingdata <- read_csv("C:/Users/andre/OneDrive/Escritorio/TFM/openpowerliftingdata3.csv")

# 3. Data division 

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


##### 5. Descriptive statistics for the Powerlifting Project.


# 5.1. Number of athletes by equipment and sex.

combined_data_clean <- bind_rows(
  ipf_male_data_clean %>% mutate(Sex = "Male"),
  ipf_female_data_clean %>% mutate(Sex = "Female")
)

combined_equipment_plot <- combined_data_clean %>%
  group_by(Sex, Equipment) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = Equipment, y = Count, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  labs(title = "Number of Athletes by Equipment and Sex",
       x = "Equipment",
       y = NULL) +  
  theme(
    panel.grid = element_blank(),  
    legend.position = "none",      
    axis.title.y = element_blank(),
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank() 
  ) +
  facet_wrap(~Sex)


print(combined_equipment_plot)


# 5.2. Divisions by equipment.

division_order <- c("Sub-Juniors", "Juniors", "Open", "Masters 1", "Masters 2", "Masters 3", "Masters 4")

ipf_male_data_clean <- ipf_male_data_clean %>%
  mutate(Division = factor(Division, levels = division_order))

ipf_female_data_clean <- ipf_female_data_clean %>%
  mutate(Division = factor(Division, levels = division_order))


# Raw

raw_combined_plot <- combined_data_clean %>%
  filter(Equipment == "Raw", !is.na(Division)) %>%
  group_by(Sex, Division) %>%
  summarise(Count = n()) %>%
  mutate(Division = factor(Division, levels = division_order)) %>%
  ggplot(aes(x = Division, y = Count, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  labs(title = "Number of Athletes By Division in Raw Equipment",
       x = "Division",
       y = NULL) +  
  theme(
    panel.grid = element_blank(),        
    legend.position = "none",            
    axis.title.y = element_blank(),      
    axis.text.y = element_blank(),       
    axis.ticks.y = element_blank(),      
    axis.text.x = element_text(angle = 45, hjust = 1)  
  ) +
  facet_wrap(~Sex)  

print(raw_combined_plot)


# Single-ply

single_ply_combined_plot <- combined_data_clean %>%
  filter(Equipment == "Single-ply" & !is.na(Division)) %>%
  group_by(Sex, Division) %>%
  summarise(Count = n()) %>%
  mutate(Division = factor(Division, levels = division_order)) %>%
  ggplot(aes(x = Division, y = Count, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  labs(title = "Number of Athletes By Division in Single-ply Equipment",
       x = "Division",
       y = NULL) +  
  theme(
    panel.grid = element_blank(),        
    legend.position = "none",            
    axis.title.y = element_blank(),      
    axis.text.y = element_blank(),       
    axis.ticks.y = element_blank(),      
    axis.text.x = element_text(angle = 45, hjust = 1)  
  ) +
  facet_wrap(~Sex)  

print(single_ply_combined_plot)


# 5.3. Number of athletes by weight classes by division and equipment.

# Ordering weight classes.

male_weight_order <- c("52", "53", "56", "59", "60", "66", "67.5", "74", "75", "82.5", "82.5+", "83", "90", "90+", "93", "100", "100+", "105", "105+", "110", "110+", "120", "120+", "125", "125+")

ipf_male_data_clean <- ipf_male_data_clean %>%
  mutate(WeightClassKg = factor(WeightClassKg, levels = male_weight_order))

female_weight_order <- c("43", "44", "47", "48", "52", "56", "57", "60", "63", "67.5", "67.5+", "69", "72", "72+", "75", "75+", "76", "76+", "82.5", "82.5+", "84", "84+", "90", "90+")

ipf_female_data_clean <- ipf_female_data_clean %>%
  mutate(WeightClassKg = factor(WeightClassKg, levels = female_weight_order))


# Filtering top 3 divisions.

get_top_divisions <- function(data, equipment) {
  data %>%
    filter(Equipment == equipment) %>%
    group_by(Division) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    arrange(desc(Count)) %>%
    slice_head(n = 3) %>%
    pull(Division)
}

top_divisions_male_raw <- get_top_divisions(ipf_male_data_clean, "Raw")
top_divisions_male_single_ply <- get_top_divisions(ipf_male_data_clean, "Single-ply")
top_divisions_female_raw <- get_top_divisions(ipf_female_data_clean, "Raw")
top_divisions_female_single_ply <- get_top_divisions(ipf_female_data_clean, "Single-ply")

filter_top_divisions <- function(data, divisions) {
  data %>%
    filter(Division %in% divisions)
}

filtered_male_raw <- filter_top_divisions(ipf_male_data_clean, top_divisions_male_raw)
filtered_male_single_ply <- filter_top_divisions(ipf_male_data_clean, top_divisions_male_single_ply)
filtered_female_raw <- filter_top_divisions(ipf_female_data_clean, top_divisions_female_raw)
filtered_female_single_ply <- filter_top_divisions(ipf_female_data_clean, top_divisions_female_single_ply)


# Plotting.

historical_weight_class_plot <- function(data, equipment, sex_label) {
  
  division_colors <- c(
    "Sub-Juniors" = "#E41A1C",  
    "Juniors" = "#377EB8",      
    "Open" = "#4DAF4A",         
    "Masters 1" = "#984EA3"     
  )
  
  data %>%
    filter(!is.na(WeightClassKg)) %>%  
    group_by(Division, WeightClassKg) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    ggplot(aes(x = WeightClassKg, y = Count, fill = Division)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = division_colors) +  
    theme_minimal() +
    labs(title = paste("Weight Classes by Division for", sex_label, "Athletes with", equipment, "Equipment"),
         x = "Weight Class (Kg)",
         y = "Count of Athletes") +
    theme(
      panel.grid = element_blank(),               
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_blank()
    )
}


plot_male_raw <- historical_weight_class_plot(filtered_male_raw, "Raw", "Male")
plot_male_single_ply <- historical_weight_class_plot(filtered_male_single_ply, "Single-ply", "Male")
plot_female_raw <- historical_weight_class_plot(filtered_female_raw, "Raw", "Female")
plot_female_single_ply <- historical_weight_class_plot(filtered_female_single_ply, "Single-ply", "Female")

print(plot_male_raw)
print(plot_male_single_ply)
print(plot_female_raw)
print(plot_female_single_ply)



# 5.4. Weight classes by division and equipment from 2011 and 2018.

# 2011

weight_class_plot_2011 <- function(data, divisions, equipment, sex) {
  
  division_colors <- c(
    "Sub-Juniors" = "#E41A1C",  
    "Juniors" = "#377EB8",      
    "Open" = "#4DAF4A",         
    "Masters 1" = "#984EA3"     
  )
  
  data %>%
    filter(Division %in% divisions, 
           Equipment == equipment, 
           !is.na(WeightClassKg), 
           Date >= as.Date("2011-01-01")) %>%  
    group_by(Division, WeightClassKg) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    ggplot(aes(x = WeightClassKg, y = Count, fill = Division)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = division_colors) +
    theme_minimal() +
    labs(title = paste("Number of", sex, "Athletes in Top 3 Divisions with", equipment, "Equipment by Weight Class (2011 Onwards)"),
         x = "Weight Class (Kg)",
         y = "Count of Athletes") +
    theme(
      panel.grid = element_blank(),                
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_blank()
    )
}


plot_male_raw_2011 <- weight_class_plot_2011(filtered_male_raw, top_divisions_male_raw, "Raw", "Male")
print(plot_male_raw_2011)

plot_male_single_ply_2011 <- weight_class_plot_2011(filtered_male_single_ply, top_divisions_male_single_ply, "Single-ply", "Male")
print(plot_male_single_ply_2011)

plot_female_raw_2011 <- weight_class_plot_2011(filtered_female_raw, top_divisions_female_raw, "Raw", "Female")
print(plot_female_raw_2011)

plot_female_single_ply_2011 <- weight_class_plot_2011(filtered_female_single_ply, top_divisions_female_single_ply, "Single-ply", "Female")
print(plot_female_single_ply_2011)


# 2018 

weight_class_plot_2018 <- function(data, divisions, equipment, sex) {
  
  division_colors <- c(
    "Sub-Juniors" = "#E41A1C",  
    "Juniors" = "#377EB8",      
    "Open" = "#4DAF4A",         
    "Masters 1" = "#984EA3"     
  )
  
  data %>%
    filter(Division %in% divisions, 
           Equipment == equipment, 
           !is.na(WeightClassKg), 
           Date >= as.Date("2018-01-01")) %>%  
    group_by(Division, WeightClassKg) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    ggplot(aes(x = WeightClassKg, y = Count, fill = Division)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = division_colors) +
    theme_minimal() +
    labs(title = paste("Number of", sex, "Athletes in Top 3 Divisions with", equipment, "Equipment by Weight Class (2018 Onwards)"),
         x = "Weight Class (Kg)",
         y = "Count of Athletes") +
    theme(
      panel.grid = element_blank(),                
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_blank()
    )
}


plot_male_raw_2018 <- weight_class_plot_2018(filtered_male_raw, top_divisions_male_raw, "Raw", "Male")
print(plot_male_raw_2018)

plot_male_single_ply_2018 <- weight_class_plot_2018(filtered_male_single_ply, top_divisions_male_single_ply, "Single-ply", "Male")
print(plot_male_single_ply_2018)

plot_female_raw_2018 <- weight_class_plot_2018(filtered_female_raw, top_divisions_female_raw, "Raw", "Female")
print(plot_female_raw_2018)

plot_female_single_ply_2018 <- weight_class_plot_2018(filtered_female_single_ply, top_divisions_female_single_ply, "Single-ply", "Female")
print(plot_female_single_ply_2018)



# 5.5. Weight classes timeline.

filtered_male <- ipf_male_data_clean %>%
  filter(!is.na(WeightClassKg))

filtered_female <- ipf_female_data_clean %>%
  filter(!is.na(WeightClassKg))


create_timeline_plot <- function(data, sex) {
  data %>%
    group_by(Year = as.integer(format(Date, "%Y")), WeightClassKg) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    ggplot(aes(x = Year, y = WeightClassKg, group = WeightClassKg, color = WeightClassKg)) +
    geom_line() +
    geom_point() +
    theme_minimal() +        
    labs(title = paste("Timeline of Weight Classes for", sex, "Athletes Across All Divisions"),
         x = "Year",
         y = "Weight Class (Kg)") +        
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none",       
          panel.grid = element_blank(),   
          panel.grid.major.x = element_line(color = "gray80"))
}


plot_male_timeline <- create_timeline_plot(filtered_male, "Male")
plot_female_timeline <- create_timeline_plot(filtered_female, "Female")

print(plot_male_timeline)
print(plot_female_timeline)


# 5.6. Bodyweight density plot by sex (historical perspective).

combined_data <- bind_rows(
  ipf_male_data_clean %>%
    mutate(Sex = "Male"),
  ipf_female_data_clean %>%
    mutate(Sex = "Female")
)

density_bodyweight <- ggplot(combined_data, aes(x = BodyweightKg, fill = Sex)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Density Plot of Body Weight by Sex",
       x = "Body Weight (Kg)",
       y = "Density") +
  scale_fill_brewer(palette = "Set1") +
  theme(
    legend.title = element_blank(),
    panel.grid = element_blank()  
  )

print(density_bodyweight)


# 5.7. Bodyweight density plot from 2011 and 2018.


# 2011

ipf_male_data_2011_onwards <- ipf_male_data_clean %>%
  filter(Date >= as.Date("2011-01-01"))

ipf_female_data_2011_onwards <- ipf_female_data_clean %>%
  filter(Date >= as.Date("2011-01-01"))

combined_data_2011_onwards <- bind_rows(
  ipf_male_data_2011_onwards %>%
    mutate(Sex = "Male"),
  ipf_female_data_2011_onwards %>%
    mutate(Sex = "Female")
)

density_bodyweight_2011_onwards <- ggplot(combined_data_2011_onwards, aes(x = BodyweightKg, fill = Sex)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Density Plot of Body Weight by Sex (2011 Onwards)",
       x = "Body Weight (Kg)",
       y = "Density") +
  scale_fill_brewer(palette = "Set1") +
  theme(
    legend.title = element_blank(),
    panel.grid = element_blank()
  )

print(density_bodyweight_2011_onwards)


# 2018

ipf_male_data_2018_onwards <- ipf_male_data_clean %>%
  filter(Date >= as.Date("2018-01-01"))

ipf_female_data_2018_onwards <- ipf_female_data_clean %>%
  filter(Date >= as.Date("2018-01-01"))

combined_data_2018_onwards <- bind_rows(
  ipf_male_data_2018_onwards %>%
    mutate(Sex = "Male"),
  ipf_female_data_2018_onwards %>%
    mutate(Sex = "Female")
)

density_bodyweight_2018_onwards <- ggplot(combined_data_2018_onwards, aes(x = BodyweightKg, fill = Sex)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Density Plot of Body Weight by Sex (2018 Onwards)",
       x = "Body Weight (Kg)",
       y = "Density") +
  scale_fill_brewer(palette = "Set1") +
  theme(
    legend.title = element_blank(),
    panel.grid = element_blank()
  )

print(density_bodyweight_2018_onwards)


# 5.8. Age density plot, Open division.

open_division_data <- ipf_male_data_clean %>%
  filter(Division == "Open") %>%
  bind_rows(
    ipf_female_data_clean %>%
      filter(Division == "Open")
  )

age_density_plot <- ggplot(open_division_data, aes(x = Age, fill = Sex)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Density Plot of Age for Open Division",
       x = "Age",
       y = "Density") +
  scale_fill_brewer(palette = "Set1") +
  theme(
    legend.title = element_blank(),
    panel.grid = element_blank()
  )

print(age_density_plot)


# 5.9. Boxplot of totals per weight class on Open division, Raw equipment, from 2011 and 2018.

# 2011

# Male

ipf_male_data_2011_onwards_open <- ipf_male_data_clean %>%
  filter(Date >= as.Date("2011-01-01") & Division == "Open") %>%
  filter(!is.na(WeightClassKg))

male_boxplot_2011_onwards_open <- ggplot(ipf_male_data_2011_onwards_open, aes(x = as.factor(WeightClassKg), y = TotalKg, fill = Equipment)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot of TotalKg by Weight Class for Male Athletes (2011 onwards, Open Division)",
       x = "Weight Class (Kg)",
       y = "TotalKg") +
  scale_fill_brewer(palette = "Set2") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

print(male_boxplot_2011_onwards_open)


# Female

ipf_female_data_2011_onwards_open <- ipf_female_data_clean %>%
  filter(Date >= as.Date("2011-01-01") & Division == "Open") %>%
  filter(!is.na(WeightClassKg))

female_boxplot_2011_onwards_open <- ggplot(ipf_female_data_2011_onwards_open, aes(x = as.factor(WeightClassKg), y = TotalKg, fill = Equipment)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot of TotalKg by Weight Class for Female Athletes (2011 onwards, Open Division)",
       x = "Weight Class (Kg)",
       y = "TotalKg") +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()
  )

print(female_boxplot_2011_onwards_open)


# 2018

# Male

ipf_male_data_2018_onwards_open <- ipf_male_data_clean %>% 
  filter(Date >= as.Date("2018-01-01") & Division == "Open") %>%
  filter(!is.na(WeightClassKg))

male_boxplot_2018_onwards_open <- ggplot(ipf_male_data_2018_onwards_open, aes(x = as.factor(WeightClassKg), y = TotalKg, fill = Equipment)) +
  geom_boxplot() +
  theme_minimal() +
labs(title = "Boxplot of TotalKg by Weight Class for Male Athletes (2018 onwards, Open Division)",
     x = "Weight Class (Kg)",
     y = "TotalKg") +
  scale_fill_brewer(palette = "Set2") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

print(male_boxplot_2018_onwards_open)


# Female

ipf_female_data_2018_onwards_open <- ipf_female_data_clean %>% 
  filter(Date >= as.Date("2018-01-01") & Division == "Open") %>%
  filter(!is.na(WeightClassKg))

female_boxplot_2018_onwards_open <- ggplot(ipf_female_data_2018_onwards_open, aes(x = as.factor(WeightClassKg), y = TotalKg, fill = Equipment)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot of TotalKg by Weight Class for Female Athletes (2018 onwards, Open Division)",
       x = "Weight Class (Kg)",
       y = "TotalKg") +
  scale_fill_brewer(palette = "Set2") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()
  )

print(female_boxplot_2018_onwards_open)


# 5.10. Regression of totals by bodyweight, Open division, from 2011 onwards.


# Male

ipf_male_data_2011_onwards_open <- ipf_male_data_clean %>% 
  filter(Date >= as.Date("2011-01-01") & Division == "Open")

male_lm <- lm(TotalKg ~ BodyweightKg + Equipment, data = ipf_male_data_2011_onwards_open)

male_regression_plot_2011_onwards_open <- ggplot(ipf_male_data_2011_onwards_open, aes(x = BodyweightKg, y = TotalKg, color = Equipment)) +
  geom_point(alpha = 0.5, position = position_jitter(width = 0.2)) +
  geom_smooth(method = "lm", se = FALSE, aes(group = Equipment)) +
  theme_minimal() +
  labs(title = "Regression of TotalKg by BodyweightKg for Male Athletes (2011 onwards, Open Division)",
       x = "Body Weight (Kg)",
       y = "TotalKg") +
  scale_color_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()
  )

print(male_regression_plot_2011_onwards_open)


# Female

ipf_female_data_2011_onwards_open <- ipf_female_data_clean %>% 
  filter(Date >= as.Date("2011-01-01") & Division == "Open")

female_lm <- lm(TotalKg ~ BodyweightKg + Equipment, data = ipf_female_data_2011_onwards_open)

female_regression_plot_2011_onwards_open <- ggplot(ipf_female_data_2011_onwards_open, aes(x = BodyweightKg, y = TotalKg, color = Equipment)) +
  geom_point(alpha = 0.5, position = position_jitter(width = 0.2)) +
  geom_smooth(method = "lm", se = FALSE, aes(group = Equipment)) +
  theme_minimal() +
  labs(title = "Regression of TotalKg by BodyweightKg for Female Athletes (2011 onwards, Open Division)",
       x = "Body Weight (Kg)",
       y = "TotalKg") +
  scale_color_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()
  )

print(female_regression_plot_2011_onwards_open)


# 5.11. ANOVA of totals by weight class, Open division.

# Male

ipf_male_data_2011_onwards_open <- ipf_male_data_clean %>% 
  filter(Date >= as.Date("2011-01-01") & Division == "Open")

anova_male_result <- aov(TotalKg ~ as.factor(WeightClassKg) * Equipment, data = ipf_male_data_2011_onwards_open)
summary(anova_male_result)

# Female

ipf_female_data_2011_onwards_open <- ipf_female_data_clean %>% 
  filter(Date >= as.Date("2011-01-01") & Division == "Open")

anova_female_result <- aov(TotalKg ~ as.factor(WeightClassKg) * Equipment, data = ipf_female_data_2011_onwards_open)
summary(anova_female_result)


# 5.12. Tukey's HSD. 

# Male

tukey_male_result <- TukeyHSD(anova_male_result)
print(tukey_male_result)

# Female

tukey_female_result <- TukeyHSD(anova_female_result)
print(tukey_female_result)


# Results of Tukey's Test.

# Male

tukey_male_weightclass <- as.data.frame(tukey_male_result$`as.factor(WeightClassKg)`)
tukey_male_equipment <- as.data.frame(tukey_male_result$Equipment)
tukey_male_interaction <- as.data.frame(tukey_male_result$`as.factor(WeightClassKg):Equipment`)

tukey_male_weightclass$Comparison <- "WeightClass"
tukey_male_equipment$Comparison <- "Equipment"
tukey_male_interaction$Comparison <- "WeightClass:Equipment"

tukey_male_weightclass$Pair <- rownames(tukey_male_weightclass)
tukey_male_equipment$Pair <- rownames(tukey_male_equipment)
tukey_male_interaction$Pair <- rownames(tukey_male_interaction)

tukey_male_results <- rbind(tukey_male_weightclass, tukey_male_equipment, tukey_male_interaction)
rownames(tukey_male_results) <- NULL

print(tukey_male_results)

# Female

tukey_female_weightclass <- as.data.frame(tukey_female_result$`as.factor(WeightClassKg)`)
tukey_female_equipment <- as.data.frame(tukey_female_result$Equipment)
tukey_female_interaction <- as.data.frame(tukey_female_result$`as.factor(WeightClassKg):Equipment`)

tukey_female_weightclass$Comparison <- "WeightClass"
tukey_female_equipment$Comparison <- "Equipment"
tukey_female_interaction$Comparison <- "WeightClass:Equipment"

tukey_female_weightclass$Pair <- rownames(tukey_female_weightclass)
tukey_female_equipment$Pair <- rownames(tukey_female_equipment)
tukey_female_interaction$Pair <- rownames(tukey_female_interaction)

tukey_female_results <- rbind(tukey_female_weightclass, tukey_female_equipment, tukey_female_interaction)
rownames(tukey_female_results) <- NULL

print(tukey_female_results)

# Tukey's HSD graph: Comparison between weight classes.

ggplot(tukey_female_weightclass, aes(x = Pair, y = diff)) +
  geom_bar(stat = "identity", fill="#377EB8") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  labs(
    title = "Tukey HSD Results for Female Weight Class",
    x = "Weight Class Comparison",
    y = "Difference in Means"
  ) +
  theme_minimal(base_family = "sans") +  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),        
    legend.title = element_blank()       
  )


# 5.13. Case study of female weight classes -69 & -72.


# Number of female athletes on weight classes -69 & -72.

ipf_female_69_72 <- ipf_female_data_clean %>%
  filter(Date >= as.Date("2011-01-01") & 
           WeightClassKg %in% c(69, 72))

athlete_counts <- ipf_female_69_72 %>%
  mutate(Year = as.numeric(format(Date, "%Y"))) %>%
  group_by(Year, WeightClassKg) %>%
  summarise(Count = n(), .groups = 'drop')


ggplot(athlete_counts, aes(x = Year, y = Count, color = as.factor(WeightClassKg))) +
  geom_line() +
  geom_point() +
  labs(title = "Number of Female Athletes Competing in 69 kg and 72 kg Weight Classes (2011 Onwards)",
       x = "Year",
       y = "Number of Athletes",
       color = "Weight Class (Kg)") +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2") +
  theme(
    panel.grid = element_blank(),        
    axis.line = element_line(color = "black"),  
    panel.border = element_rect(color = "black", fill = NA),  
    axis.ticks = element_line(color = "black")  
  )


# 5.14. Average GL points per year in weight classes -69 & -72.

average_goodlift <- ipf_female_69_72 %>%
  mutate(Year = as.numeric(format(Date, "%Y"))) %>%
  group_by(Year, WeightClassKg) %>%
  summarise(Average_Goodlift = mean(Goodlift, na.rm = TRUE), .groups = 'drop')


ggplot(average_goodlift, aes(x = Year, y = Average_Goodlift, color = as.factor(WeightClassKg))) +
  geom_line() +
  geom_point() +
  labs(title = "Average Goodlift for Female Athletes in 69 kg and 72 kg Weight Classes (2011 Onwards)",
       x = "Year",
       y = "Average Goodlift",
       color = "Weight Class (Kg)") +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2") +
  theme(
    panel.grid = element_blank(),        
    axis.line = element_line(color = "black"),  
    panel.border = element_rect(color = "black", fill = NA),  
    axis.ticks = element_line(color = "black")  
  )


# 5.15. Density plot of totals by sex.


ipf_male_data_2011_onwards_open <- ipf_male_data_clean %>%
  filter(Date >= as.Date("2011-01-01") & Division == "Open")

ipf_female_data_2011_onwards_open <- ipf_female_data_clean %>%
  filter(Date >= as.Date("2011-01-01") & Division == "Open")

combined_data_2011_onwards_open <- bind_rows(ipf_male_data_2011_onwards_open, ipf_female_data_2011_onwards_open)

ggplot(combined_data_2011_onwards_open, aes(x = TotalKg, fill = Sex)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Equipment) +  
  labs(title = "Density Plot of TotalKg for Male and Female Athletes (2011 Onwards, Open Division)",
       x = "TotalKg",
       y = "Density") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1",
                    labels = c("M" = "Male", "F" = "Female")) +
  theme(
    panel.grid = element_blank()
  )