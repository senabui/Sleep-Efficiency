## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Loading data in 

ds <- read.csv("/Users/user/Desktop/DSC 510 Health Data Science/Final Project/Sleep_Efficiency.csv", stringsAsFactors = TRUE)

ds


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Remove ID from ds and assign to 'sleep'
sleep <- ds %>% select(!ID)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Removes children (those 17 or younger) from the dataset by filtering for only adults 
sleep <- sleep %>% filter(Age > 17)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Handle missing values by imputing the with median values. We chose to impute with median values so that it does not skew the distribution too much. We address missing values in 'Awakenings', 'Caffeine.consumption', 'Alcohol.consumption', and 'Exercise.frequency'.

sleep <- sleep %>% replace_na(list(
  Alcohol.consumption = median(sleep$Alcohol.consumption, TRUE),
  Exercise.frequency = median(sleep$Exercise.frequency, TRUE), 
  Caffeine.consumption = median(sleep$Caffeine.consumption, TRUE), 
  Awakenings = median(sleep$Awakenings, TRUE)))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# First, convert Gender to numeric type (1 for Female, 0 for Male)
sleep <- sleep %>%
  mutate(Gender = ifelse(Gender == "Female", 1, 0))

# Also convert Smoking_status to numeric
sleep <- sleep_corr %>%
  mutate(Smoking = ifelse(Smoking.status == "Yes", 1, 0))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sleep


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Drop columns Smoking_num, Smoking.status, Gender
sleep <-sleep %>% select(!c(Smoking_num, Smoking.status, Gender))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sleep


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Creating New Variables

# Alcohol.consumption relates to the number of drinks a person has had in the last 24hrs. 

# Creating a new variable called 'Drinks', a binary variable indicating whether or not alcohol was consumed
sleep <- sleep %>% mutate(Drinks = ifelse(Alcohol.consumption == 0, 'No', 'Yes'))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Looking at 'Sleep.efficiency' unique values and range
range(sleep$Sleep.efficiency)
unique(sleep$Sleep.efficiency)

## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Creating a column named 'target', which is our target variable for classification modeling later

# Categorizing patients into Low SE (Sleep Efficiency) as 0-0.85, High SE as 0.90-1, and Optimal SE as 0.85-0.89

sleep <- sleep %>% mutate(target = ifelse(Sleep.efficiency >= 0.9, "High",
                                      ifelse(Sleep.efficiency < 0.85, "Low", "Optimal")))

# Observe 'target' column in our dataset, which is our target variable for classification later

sleep


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Calculating sleep efficiency dataset reference points


# Function to convert time to decimal hours
time_to_decimal <- function(time_str) {
  # Extract time part from datetime strings
  # This assumes format like "2021-03-06 01:00:00" or "MM/DD/YY HH:MM"
  if (grepl(" ", time_str)) {
    time_part <- str_split(time_str, " ")[[1]][2]
  } else {
    time_part <- time_str
  }
  
  # Extract hours and minutes
  time_components <- str_split(time_part, ":")[[1]]
  hours <- as.numeric(time_components[1])
  minutes <- as.numeric(time_components[2])
  
  # Convert to decimal
  decimal_time <- hours + (minutes/60)
  
  return(decimal_time)
}

# Process time variables
sleep_times <- sleep %>%
  # Apply time conversion to each row
  mutate(
    # Convert bedtime to decimal
    Bedtime_decimal = map_dbl(Bedtime, ~time_to_decimal(.x)),
    
    # Handle bedtimes after midnight
    Bedtime_decimal = ifelse(Bedtime_decimal < 12 &
                            !grepl("21:|22:|23:", Bedtime),
                            Bedtime_decimal + 24, 
                            Bedtime_decimal),
    
    # Convert wake time to decimal
    Wakeup_decimal = map_dbl(`Wakeup.time`, ~time_to_decimal(.x)),
    
    # Calculate sleep midpoint (wrap around to 24-hour clock)
    Sleep_midpoint = (Bedtime_decimal + (Sleep.duration/2)) %% 24
  )

# Calculate reference statistics for bedtime
bedtime_mean <- mean(sleep_times$Bedtime_decimal, na.rm = TRUE)
bedtime_median <- median(sleep_times$Bedtime_decimal, na.rm = TRUE)
bedtime_q1 <- quantile(sleep_times$Bedtime_decimal, 0.25, na.rm = TRUE)
bedtime_q3 <- quantile(sleep_times$Bedtime_decimal, 0.75, na.rm = TRUE)

# Calculate reference statistics for wake-up time
wakeup_mean <- mean(sleep_times$Wakeup_decimal, na.rm = TRUE)
wakeup_median <- median(sleep_times$Wakeup_decimal, na.rm = TRUE)
wakeup_q1 <- quantile(sleep_times$Wakeup_decimal, 0.25, na.rm = TRUE)
wakeup_q3 <- quantile(sleep_times$Wakeup_decimal, 0.75, na.rm = TRUE)

# Calculate reference statistics for sleep midpoint
midpoint_mean <- mean(sleep_times$Sleep_midpoint, na.rm = TRUE)
midpoint_median <- median(sleep_times$Sleep_midpoint, na.rm = TRUE)
midpoint_q1 <- quantile(sleep_times$Sleep_midpoint, 0.25, na.rm = TRUE)
midpoint_q3 <- quantile(sleep_times$Sleep_midpoint, 0.75, na.rm = TRUE)

# Function to convert decimal hours to time format
decimal_to_time <- function(decimal_hours) {
  # Handle 24-hour wrap-around
  decimal_hours <- decimal_hours %% 24
  
  # Extract hours and minutes
  hours <- floor(decimal_hours)
  minutes <- round((decimal_hours - hours) * 60)
  
  # Format as HH:MM
  formatted_time <- sprintf("%02d:%02d", hours, minutes)
  
  return(formatted_time)
}

# Create a summary data frame
reference_points <- data.frame(
  Metric = c("Bedtime", "Bedtime", "Bedtime", "Bedtime",
             "Wake-up Time", "Wake-up Time", "Wake-up Time", "Wake-up Time",
             "Sleep Midpoint", "Sleep Midpoint", "Sleep Midpoint", "Sleep Midpoint"),
  Statistic = c("Mean", "Median", "Q1 (25%)", "Q3 (75%)",
                "Mean", "Median", "Q1 (25%)", "Q3 (75%)",
                "Mean", "Median", "Q1 (25%)", "Q3 (75%)"),
  Decimal_Hours = c(bedtime_mean, bedtime_median, bedtime_q1, bedtime_q3,
                    wakeup_mean, wakeup_median, wakeup_q1, wakeup_q3,
                    midpoint_mean, midpoint_median, midpoint_q1, midpoint_q3)
)

# Add time format column
reference_points$Time_Format <- sapply(reference_points$Decimal_Hours, decimal_to_time)

# Print the results
print(reference_points, row.names = FALSE)

# You can also examine the distributions visually
par(mfrow = c(3, 1))

# Bedtime distribution
hist(sleep_times$Bedtime_decimal %% 24, 
     main = "Bedtime Distribution", 
     xlab = "Time (decimal hours)",
     breaks = 24)
abline(v = bedtime_median %% 24, col = "red", lwd = 2)
abline(v = bedtime_q1 %% 24, col = "blue", lty = 2)
abline(v = bedtime_q3 %% 24, col = "blue", lty = 2)

# Wake-up time distribution
hist(sleep_times$Wakeup_decimal, 
     main = "Wake-up Time Distribution", 
     xlab = "Time (decimal hours)",
     breaks = 24)
abline(v = wakeup_median, col = "red", lwd = 2)
abline(v = wakeup_q1, col = "blue", lty = 2)
abline(v = wakeup_q3, col = "blue", lty = 2)

# Sleep midpoint distribution
hist(sleep_times$Sleep_midpoint, 
     main = "Sleep Midpoint Distribution", 
     xlab = "Time (decimal hours)",
     breaks = 24)
abline(v = midpoint_median, col = "red", lwd = 2)
abline(v = midpoint_q1, col = "blue", lty = 2)
abline(v = midpoint_q3, col = "blue", lty = 2)

# Bedtime median 24.0, Wake-up Time median 7.0, and Sleep Midpoint median 3.5 represent the typical sleep timing in our study population within our dataset and serves as anchors for calculating relative measures. 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Time data preprocessing

# Define median reference points (reference points from the analysis above)
BEDTIME_MEDIAN <- 24.0     # 00:00
WAKEUP_MEDIAN <- 7.0       # 07:00
MIDPOINT_MEDIAN <- 3.5     # 03:30



# Time conversion functions: writing a function that converts strings into decimal hours (continuous numerical value e.g 07:30 becomes 7.5). 
time_to_decimal <- function(time_str) {
  # Extract time part from datetime strings
  # This assumes format like "2021-03-06 01:00:00" or "MM/DD/YY HH:MM"
  if (grepl(" ", time_str)) {
    time_part <- str_split(time_str, " ")[[1]][2]
  } else {
    time_part <- time_str
  }
  
  # Extract hours and minutes
  time_components <- str_split(time_part, ":")[[1]]
  hours <- as.numeric(time_components[1])
  minutes <- as.numeric(time_components[2])
  
  # Convert to decimal
  decimal_time <- hours + (minutes/60)
  
  return(decimal_time)
}



# Basic time extraction performed on the time components within our dataset: extracts the time components from the datetime strings in our dataset and converts them to decimal hours. Handles the midnight crossing problem by adding 24 to early morning bedtimes (e.g 1:00AM becomes 25.0)

sleep <- sleep %>%
  # Step 1: Extract and convert time components to decimal hours
  mutate(
    # Apply time conversion to each row and handle bedtimes after midnight
    Bedtime_hour = map_dbl(Bedtime, ~time_to_decimal(.x)),
    Bedtime_hour = ifelse(Bedtime_hour < 12 & 
                          !grepl("21:|22:|23:", Bedtime), 
                          Bedtime_hour + 24, 
                          Bedtime_hour),
    
    # Convert wake time to decimal
    Wakeup_hour = map_dbl(`Wakeup.time`, ~time_to_decimal(.x))
  ) %>%
  
  
  # Step 2: Create sleep timing features with dataset-specific references
  mutate(
    # Calculate midpoint of each person's sleep period (in hours since midnight)
    Sleep_midpoint = (Bedtime_hour + (Sleep.duration/2)) %% 24,
    
    # Calculate deviations from dataset median reference points (How much earlier/later each person's time is compared to the population median)
    # Negative values mean earlier than median, positive mean later
    Bedtime_dev_from_median = Bedtime_hour - BEDTIME_MEDIAN,
    Wakeup_dev_from_median = Wakeup_hour - WAKEUP_MEDIAN,
    Midpoint_dev_from_median = Sleep_midpoint - MIDPOINT_MEDIAN,
    
    # Calculate standardized scores (z-scores) for time variables
    # These represent how many standard deviations each person is from the mean
    Bedtime_zscore = as.numeric(scale(Bedtime_hour)),
    Wakeup_zscore = as.numeric(scale(Wakeup_hour)),
    Midpoint_zscore = as.numeric(scale(Sleep_midpoint))
  ) %>%
  
  
  # Step 3: Calculate sleep regularity features for sleep pattern features
  mutate(
    # Time from median bedtime (absolute deviation)
    Bedtime_irregularity = abs(Bedtime_dev_from_median),
    
    # Time from median wake time (absolute deviation)
    Wakeup_irregularity = abs(Wakeup_dev_from_median),
  )
  
  # Keep all remaining columns (no circular representations to remove)

# Preview the transformed features
sleep_time_features <- sleep %>% 
  select(contains("time") | contains("hour") | 
         contains("midpoint") | contains("dev") |
         contains("irreg") | contains("chrono"))

# Show the first few rows of the processed dataset
head(sleep_time_features)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Processed time columns defintions and meanings:

# Bedtime_hour: Bedtime converted to decimal hours, a continuous numerical format and are adjusted to be after midnight. Values such as 25.0 means 1:00AM, 26.0 means 2:00AM, 21.5 means 9:30PM

# Wakeup_hour: Wake-up time converted to decimal hours. 7.0 means 7:00AM, 9.0 means 9:00AM. No adjustment needs since these are all morning hours. 

# Sleep_midpoint: The midpoint of the person's sleep period in decimal hours. For example, 4.0 means the middle of the sleep period was at 4:00AM. This could be an important metric representing a person's circadian phase. 

# Bedtime_dev_from_median: A deviation metric calculating how many hours earlier or later a person goes to bed compared to the population median of 24.0 (midnight). Positive values like 1.0 or 2.0, means going to bed later than the population median. Negative values like -2.5 means going to bed 2 and a half hours earlier than the population median. For example, 1.0 means the person goes to bed 1 hour later than the typical person. 

# Wakeup_dev_from_median: A deviation metric calculating how many hours earlier or later a person wakes up compared to the population median of 7.0 (7:00AM).Negative values means a person wakes up earlier than the population median. Positive values means the person wakes up later than the population median. 

# Midpoint_dev_from_median (Chronotype): A deviation metric measuring how much a person's sleep midpoint deviates from the population median midpoint of 3.5 hours (3:30AM). Can show whether someone is an earlier or later sleeper compared to others. Negative values means a person's sleep midpoint is earlier than the population median. Positive values means the person's sleep midpoint is later than the population median. Can be used to reflect a person's internal biological clock or circadian rhythm and a person's natural inclination with regard to the time of the day and when they are most alert or energetic (early bird vs. night owl).

# Bedtime_irregularity: An absolute deviation metric from the median bedtime, regardless of the direction. Shows how unusual/abnormal someone's bedtime is. Higher values indicate more atypical sleep timing. For example, 2.5 means the person's bedtime is 2.5 hours away from the population median (either earlier or later)

# Wakeup_irregularity: An absolute deviation metric that measures how much a person's wake-up time deviates from the population median, regardless of direction (earlier or later). Low values close to 0 indicate wake-up times close to the population median. Higher values indicate more unusual wake-up times (either very early or very late). For example, a value of 2.0 means the person wakes up 2 hours away from the typical wake-up time (could be 5:00AM or 9:00AM, since the population median's wake-up time is 7:00AM)

# Bedtime_zscore: Number of standard deviations a person's bedtime is from the population mean. Positive values indicate later than average bedtimes. Negative values indicate earlier than average bedtimes. For example, 1.70 means the person's bedtime is 1.70 standard deviations later than the average. 

# Wakeup_zscore: Number of standard deviations a person's wake-up time is from the population mean. Positive values indicate later than average wake-up times. Negative values indicate earlier than average wake-up times For example, 2.0 means the person's wake-up time is 2.0 standard deviations later than the average. 

# Midpoint_zscore: Number of standard deviations a person's sleep midpoint is from the population mean. Negative values: sleep midpoint is earlier than the average. Positive values: sleep midpoint is later than the average.   



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Drop Chronotype column since it is the same as Midpoint_dev_from_median
sleep <- sleep %>% select(!Chronotype)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Observe if 'Chronotype' dropped
sleep


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Drop 'Bedtime' and 'Wakeup.time' to avoid redundancy
sleep <- sleep %>% select(!c(Bedtime,Wakeup.time,))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Observe if 'Bedtime' and 'Wakeup.time' dropped
sleep


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Variable scaling for sleep percentages (REM.sleep.percentage, Deep.sleep.percentage, Light.sleep.percentage)

# Transformation of three sleep stage columns based on recommended percentages in each stage
# Normalizes each sleep stage by subtracting the ideal percentage and dividing by 100
## light sleep ideal is 5%, deep sleep ideal is 70%, REM sleep ideal 25%
## decimal scaling to preserve 0.
sleep <- sleep %>% rename(REM = REM.sleep.percentage, #renaming columns
                          deep = Deep.sleep.percentage,
                          light = Light.sleep.percentage)

sleep <- sleep %>%
  mutate(REM = (REM-25)/100) %>%
  mutate(deep = (deep-70)/100) %>%
  mutate(light = (light-5)/100)

# This transformation centers the values around 0, where 0 represents the ideal percentage for each sleep stage. Positive values indicate above-level percentages, and negative values indicate below-ideal percentages. 

# Examples: A value of 0.00 means exactly 70% of deep sleep, which is the ideal for deep sleep phase. A value of 0.13 means about 83% deep sleep (so 13% above ideal deep sleep phase of 70%). A value of -0.42 means about 28% deep sleep (42% below the 70% deep sleep ideal).

#### Double check the reference points for REM, Light, and Deep sleep through other clinical standards or studies


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Standardizing rest of the variables

# Variables to standardize
vars_to_standardize <- c("Sleep.duration", "Caffeine.consumption", 
                         "Alcohol.consumption", "Exercise.frequency", 
                         "Awakenings", "Age")

# Standardize the selected variables
sleep_standardized <- sleep %>%
  mutate(across(all_of(vars_to_standardize), 
                ~ as.numeric(scale(.)), 
                .names = "{.col}_scaled"))

# Check the result with summary statistics of the original and scaled variables
summary_stats <- sleep_standardized %>%
  select(all_of(vars_to_standardize), 
         ends_with("_scaled")) %>%
  summary()

print(summary_stats)

# Verify the standardization worked correctly
verification <- sleep_standardized %>%
  select(ends_with("_scaled")) %>%
  summarise(across(everything(), 
                  list(mean = ~mean(., na.rm = TRUE), 
                       sd = ~sd(., na.rm = TRUE))))

print(verification)

# Replace original variables with scaled versions (optional)
# Uncomment these lines if you want to replace the original variables
# sleep <- sleep_standardized %>%
#   mutate(Sleep.duration = Sleep.duration_scaled,
#          Caffeine.consumption = Caffeine.consumption_scaled,
#          Alcohol.consumption = Alcohol.consumption_scaled,
#          Exercise.frequency = Exercise.frequency_scaled,
#          Awakenings = Awakenings_scaled,
#          Age = Age_scaled) %>%
#   select(-ends_with("_scaled"))

# If you prefer to keep both original and scaled versions:
sleep <- sleep_standardized


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sleep


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# converting 'Drinks' to binary (1 for Yes, 0 for No)
sleep <- sleep %>%
  mutate(Drinks_binary = ifelse(Drinks == "Yes", 1, 0))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Dropping the 'Drinks' column which was str type
sleep <- sleep %>% select (!Drinks)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sleep


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Creating a new dataframe with all the necessary scaled and standardized variables for analysis. 

# Create a new dataframe with selected variables
sleep_new <- sleep %>%
  select(
    # Target variable
    target,
    
    # Demographics (standardized)
    Age_scaled,
    Gender,
    
    # Sleep architecture (already normalized)
    Sleep.efficiency,
    REM,
    deep,
    light,
    
    # Behavioral factors (standardized)
    Sleep.duration_scaled,
    Awakenings_scaled,
    Caffeine.consumption_scaled,
    Alcohol.consumption_scaled,
    Exercise.frequency_scaled,
    Drinks_binary,
    Smoking,
    
    # Sleep timing features
    # Chronotype and irregularity measures
    Midpoint_dev_from_median,  # Chronotype
    Bedtime_irregularity,
    Wakeup_irregularity
  )

# Check the structure of the new dataframe
str(sleep_new)

# View the first few rows
head(sleep_new)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Switching to decimal-scaling for Sleep.duration_scaled, Awakenings_scaled, Caffeine.consumption_scaled, Alcohol.consumption_scaled, Exercise.frequency_scaled to preserve the original 0 point. This is important and useful for when 0 values have meaningful interpretations (e.g. no caffeine, no alcohol consumption, no awakenings)

# removed the previous z-score standardized columns and created new decimal-scaled versions with the same column names
# Switching to decimal-scaling for Sleep.duration_scaled, Awakenings_scaled, Caffeine.consumption_scaled, 
# Alcohol.consumption_scaled, Exercise.frequency_scaled to preserve the original 0 point. 
# This is important and useful for when 0 values have meaningful interpretations 
# (e.g. no caffeine, no alcohol consumption, no awakenings)

# Variables to be decimal scaled (keeping Age as z-score)
vars_to_decimal_scale <- c("Sleep.duration", "Caffeine.consumption", 
                           "Alcohol.consumption", "Exercise.frequency", 
                           "Awakenings")

# Function for decimal scaling
decimal_scale <- function(x) {
  # Find the appropriate power of 10
  max_abs_val <- max(abs(x), na.rm = TRUE)
  power <- ceiling(log10(max_abs_val))
  
  # Scale the variable
  x / (10^power)
}

# First, we need to ensure sleep_new has the original unscaled variables
# Extract original variables from sleep dataframe if needed
original_vars <- sleep %>%
  select(all_of(vars_to_decimal_scale))

# Create a new dataframe with decimal scaled variables
sleep_decimal <- sleep_new %>%
  # Remove the old scaled columns for the variables we're changing
  select(-c(Sleep.duration_scaled, Caffeine.consumption_scaled, 
            Alcohol.consumption_scaled, Exercise.frequency_scaled, 
            Awakenings_scaled))

# Add the original variables (in case they're not already in sleep_new)
# If sleep_new already has these columns, you can skip this step
for (var in vars_to_decimal_scale) {
  if (!var %in% names(sleep_decimal)) {
    sleep_decimal[[var]] <- original_vars[[var]]
  }
}

# Now apply decimal scaling to selected variables
sleep_decimal <- sleep_decimal %>%
  mutate(across(all_of(vars_to_decimal_scale), 
                ~ decimal_scale(.), 
                .names = "{.col}_scaled"))

# Keep Age_scaled as it is (already z-score standardized)
# Verify the scaling
summary_comparison <- sleep_decimal %>%
  select(all_of(vars_to_decimal_scale), 
         ends_with("_scaled"), 
         Age_scaled) %>%
  summary()

print(summary_comparison)

# Check minimum values to verify zeros are preserved
min_values <- sleep_decimal %>%
  select(all_of(vars_to_decimal_scale), 
         ends_with("_scaled")) %>%
  summarise(across(everything(), ~min(., na.rm = TRUE)))

print("Minimum values to verify zero preservation:")
print(min_values)

# Example: Show a few rows of original and scaled values
example_rows <- sleep_decimal %>%
  select(all_of(vars_to_decimal_scale), 
         ends_with("_scaled"), 
         Age_scaled) %>%
  head(10)

print("Example rows with original and scaled values:")
print(example_rows)

# Assign back to sleep_new dataframe
sleep_new <- sleep_decimal



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sleep_new


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Dropping the original unscaled columns from sleep_new dataframe, now that we have the scaled versions.

sleep_new <- sleep_new %>% select(!c("Sleep.duration", "Caffeine.consumption", "Alcohol.consumption", "Exercise.frequency", "Awakenings"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Add Gender column back to sleep_new from the original sleep dataframe
sleep_new <- sleep_new %>% mutate(Gender = sleep$Gender)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Dropping Sleep.efficiency column from our sleep_new data frame when predicting the target variable because the target variable (Low, Optimal, High) was directly derived from Sleep.efficiency. Keeping Sleep.efficiency in our. model would create a data leakage problem, making our model performance artificially high but not genuinely predictive because the target variable is essentially a binned version of Sleep.efficiency. Including Sleep.efficiency means our model will be 'seeing' what it's trying to predict. This would prevent our model from learning the actual relationships between our other variables and sleep efficiency. 

sleep_new <- sleep_new %>% select(-Sleep.efficiency)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Checking sleep_new dataframe to see all variables
sleep_new


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Comprehensive correlation analysis of sleep_new dataset
library(corrplot)

# Convert target to numeric for correlation analysis
sleep_numeric <- sleep_new %>%
  mutate(target_num = case_when(
    target == "Low" ~ 0,
    target == "Optimal" ~ 1,
    target == "High" ~ 2,
    TRUE ~ NA_real_
  ))

# Select only numeric variables for correlation
sleep_corr <- sleep_numeric %>%
  select_if(is.numeric)

# Calculate correlation matrix
cor_matrix <- cor(sleep_corr, use = "pairwise.complete.obs")

# Print correlation matrix with variable names
print(cor_matrix, digits = 2)

# Create correlation plot
corrplot(cor_matrix, 
         method = "color", 
         type = "upper", 
         order = "hclust", 
         tl.col = "black", 
         tl.srt = 45, 
         addCoef.col = "black", 
         number.cex = 0.7,
         title = "Correlation Matrix of Sleep Variables",
         mar = c(0, 0, 1, 0))

# Convert correlation matrix to long format for more detailed analysis
cor_df <- as.data.frame(as.table(cor_matrix))
names(cor_df) <- c("Variable1", "Variable2", "Correlation")

# Remove duplicate pairs and self-correlations
cor_df <- cor_df %>%
  filter(as.character(Variable1) < as.character(Variable2))

# Sort by absolute correlation value
cor_df <- cor_df %>%
  mutate(Abs_Correlation = abs(Correlation)) %>%
  arrange(desc(Abs_Correlation))

# Print top 20 strongest correlations
cat("\nTop 20 Strongest Correlations:\n")
print(cor_df %>% head(20), digits = 3)

# Create a more informative correlation chart for visualization
# Focus on correlations with Sleep.efficiency and target_num
key_vars <- c("Sleep.efficiency", "target_num")

important_cors <- cor_df %>%
  filter(Variable1 %in% key_vars | Variable2 %in% key_vars) %>%
  mutate(Var = ifelse(Variable1 %in% key_vars, Variable2, Variable1),
         Key_Var = ifelse(Variable1 %in% key_vars, Variable1, Variable2)) %>%
  select(Var, Key_Var, Correlation, Abs_Correlation) %>%
  arrange(Key_Var, desc(Abs_Correlation))

# Print correlations with key variables
cat("\nCorrelations with Sleep Efficiency and Target Class:\n")
print(important_cors, digits = 3)

# Create a correlation heatmap with all variables
ggplot(cor_df, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                      midpoint = 0, limit = c(-1, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8)) +
  labs(title = "Correlation Heatmap of Sleep Variables",
       x = "", y = "")

# Top correlations bar chart
top_cors <- cor_df %>% 
  head(15)

ggplot(top_cors, aes(x = reorder(paste(Variable1, "&", Variable2), Abs_Correlation), 
                    y = Correlation)) +
  geom_bar(stat = "identity", aes(fill = Correlation > 0)) +
  scale_fill_manual(values = c("darkred", "darkblue"), 
                   labels = c("Negative", "Positive")) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 15 Strongest Correlations",
       x = "", 
       y = "Correlation Coefficient",
       fill = "Direction")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Random forest model WITH train/test split, WITHOUT interactions and WITHOUT SMOTE
# This provides a baseline for comparison with the SMOTE model
library(randomForest)
library(caret)

# Ensure target is of factor type
sleep_new <- sleep_new %>%
  mutate(target = factor(target, levels = c("Low", "Optimal", "High")))

# Check full dataset class distribution
cat("Full dataset class distribution:\n")
table(sleep_new$target)

# Create train/test split (70% train, 30% test)
set.seed(123)  # For reproducible split
train_indices <- createDataPartition(sleep_new$target, p = 0.7, list = FALSE, times = 1)
train_data <- sleep_new[train_indices, ]
test_data <- sleep_new[-train_indices, ]

# Check class distribution in train and test sets
cat("\nTraining set class distribution:\n")
table(train_data$target)
cat("\nTest set class distribution:\n")
table(test_data$target)

# Run random forest model on training data only
set.seed(123)  # For reproducibility
rf_model_baseline <- randomForest(target ~ .,
                        data = train_data,
                        ntree = 500,
                        importance = TRUE)

# Print model summary
cat("\nBaseline model performance on training data (OOB error):\n")
print(rf_model_baseline)

# Extract and print confusion matrix from OOB estimates
conf_matrix_baseline <- rf_model_baseline$confusion
print(conf_matrix_baseline)

# Calculate class-specific metrics from confusion matrix
n_classes <- length(levels(train_data$target))
class_metrics <- data.frame(
  Class = levels(train_data$target),
  Training_Samples = table(train_data$target),
  OOB_Error_Rate = conf_matrix_baseline[1:n_classes, "class.error"],
  OOB_Accuracy = 1 - conf_matrix_baseline[1:n_classes, "class.error"]
)

# Print class-specific metrics
cat("\nClass-specific metrics on training data (OOB estimates):\n")
print(class_metrics)

# Evaluate on held-out test data
test_predictions <- predict(rf_model_baseline, newdata = test_data)
conf_matrix_test <- confusionMatrix(test_predictions, test_data$target)
cat("\nPerformance on held-out test data:\n")
print(conf_matrix_test)

# Extract class-specific metrics from test data evaluation
test_metrics <- data.frame(
  Class = levels(test_data$target),
  Test_Samples = table(test_data$target),
  Sensitivity = conf_matrix_test$byClass[, "Sensitivity"],
  Specificity = conf_matrix_test$byClass[, "Specificity"],
  Precision = conf_matrix_test$byClass[, "Pos Pred Value"],
  F1_Score = conf_matrix_test$byClass[, "F1"],
  Balanced_Accuracy = (conf_matrix_test$byClass[, "Sensitivity"] + 
                      conf_matrix_test$byClass[, "Specificity"]) / 2
)

# Print class-specific metrics for test data
cat("\nClass-specific metrics on held-out test data:\n")
print(test_metrics)

# Extract variable importance
var_importance <- importance(rf_model_baseline)
var_importance_df <- as.data.frame(var_importance)
var_importance_df$Variable <- rownames(var_importance_df)

# Sort by Mean Decrease in Accuracy
var_importance_df <- var_importance_df %>%
  arrange(desc(MeanDecreaseAccuracy))

# Print variable importance
print(head(var_importance_df, 10))

# Create a visualization of the test confusion matrix
conf_mat_data <- as.data.frame(as.table(conf_matrix_test$table))
names(conf_mat_data) <- c("Reference", "Prediction", "Freq")

# Plot confusion matrix
ggplot(conf_mat_data, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 4) +
  scale_fill_gradient(low = "steelblue", high = "darkblue") +
  theme_minimal() +
  labs(title = "Confusion Matrix: Baseline Model on Test Data",
       subtitle = "No SMOTE, No Interactions",
       x = "Actual Class",
       y = "Predicted Class")

# Perform cross-validation for more stable metrics
set.seed(456)
train_control <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = multiClassSummary
)

# Train model with cross-validation
rf_cv_baseline <- train(
  target ~ .,
  data = sleep_new,  # Using full dataset with CV for comparison with SMOTE
  method = "rf",
  trControl = train_control,
  tuneLength = 3,
  metric = "Balanced_Accuracy"
)

# Print cross-validation results
cat("\nCross-validation results (no SMOTE):\n")
print(rf_cv_baseline)
print(rf_cv_baseline$results)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Use SMOTE to handle class imbalance in "Optimal" class for target variable
# Followed by train/test split on the balanced data
library(themis)  # For SMOTE implementation
library(recipes) # Required for themis

# Ensure target is of factor type
sleep_new <- sleep_new %>%
  mutate(target = factor(target, levels = c("Low", "Optimal", "High")))

# Check class distribution before SMOTE
cat("Class distribution before SMOTE:\n")
table(sleep_new$target)

# Create a recipe for SMOTE
smote_recipe <- recipe(target ~ ., data = sleep_new) %>%
  step_smote(target, over_ratio = 1.0) # Creates equal class sizes

# Prepare the data with SMOTE
smote_prep <- prep(smote_recipe)
sleep_balanced <- bake(smote_prep, new_data = NULL)

# Check class distribution after SMOTE
cat("\nClass distribution after SMOTE:\n")
table(sleep_balanced$target)

# Create train/test split on the SMOTE-balanced data (70% train, 30% test)
set.seed(123)  # For reproducible split
train_indices <- createDataPartition(sleep_balanced$target, p = 0.7, list = FALSE)
train_data <- sleep_balanced[train_indices, ]
test_data <- sleep_balanced[-train_indices, ]

# Check class distribution in training and test sets
cat("\nTraining set class distribution:\n")
table(train_data$target)
cat("\nTest set class distribution:\n")
table(test_data$target)

# Run random forest model on training data
set.seed(123)  # For reproducibility
rf_model_balanced <- randomForest(target ~ .,
                         data = train_data,
                         ntree = 500,
                         importance = TRUE)

# Print model summary
cat("\nSMOTE model performance (on balanced training data):\n")
print(rf_model_balanced)

# Extract class-specific metrics from the SMOTE model's confusion matrix on training data
n_classes <- length(levels(train_data$target))
smote_train_metrics <- data.frame(
  Class = levels(train_data$target),
  Training_Samples = table(train_data$target),
  OOB_Error_Rate = rf_model_balanced$confusion[1:n_classes, "class.error"],
  OOB_Accuracy = 1 - rf_model_balanced$confusion[1:n_classes, "class.error"]
)

# Print class-specific metrics for the SMOTE model on training data
cat("\nClass-specific metrics on SMOTE-balanced training data (OOB estimates):\n")
print(smote_train_metrics)

# Evaluate on balanced test data
test_predictions <- predict(rf_model_balanced, newdata = test_data)
test_conf_matrix <- confusionMatrix(test_predictions, test_data$target)
cat("\nPerformance on balanced test data:\n")
print(test_conf_matrix)

# Extract class-specific metrics for balanced test data
test_metrics <- data.frame(
  Class = levels(test_data$target),
  Test_Samples = table(test_data$target),
  Sensitivity = test_conf_matrix$byClass[, "Sensitivity"],
  Specificity = test_conf_matrix$byClass[, "Specificity"],
  Precision = test_conf_matrix$byClass[, "Pos Pred Value"],
  F1_Score = test_conf_matrix$byClass[, "F1"],
  Balanced_Accuracy = (test_conf_matrix$byClass[, "Sensitivity"] + 
                     test_conf_matrix$byClass[, "Specificity"]) / 2
)

# Print class-specific metrics for balanced test data
cat("\nClass-specific metrics on balanced test data:\n")
print(test_metrics)

# Extract variable importance
var_importance <- importance(rf_model_balanced)
var_importance_df <- as.data.frame(var_importance)
var_importance_df$Variable <- rownames(var_importance_df)

# Sort by Mean Decrease in Accuracy
var_importance_df <- var_importance_df %>%
  arrange(desc(MeanDecreaseAccuracy))

# Print variable importance
print(head(var_importance_df, 10))

# Plot variable importance
ggplot(var_importance_df, aes(x = reorder(Variable, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Variable Importance with Balanced Classes",
       subtitle = "Based on Mean Decrease in Accuracy",
       x = "",
       y = "Mean Decrease in Accuracy (higher = more important)")

# Create a visualization of the test confusion matrix
test_conf_data <- as.data.frame(as.table(test_conf_matrix$table))
names(test_conf_data) <- c("Reference", "Prediction", "Freq")

# Plot test confusion matrix
ggplot(test_conf_data, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 4) +
  scale_fill_gradient(low = "steelblue", high = "darkblue") +
  theme_minimal() +
  labs(title = "Confusion Matrix: SMOTE Model on Balanced Test Data",
       x = "Actual Class",
       y = "Predicted Class")

# Also evaluate on original unbalanced data to check real-world performance
predictions_original <- predict(rf_model_balanced, newdata = sleep_new)
conf_matrix_original <- confusionMatrix(predictions_original, sleep_new$target)
cat("\nPerformance on original unbalanced data:\n")
print(conf_matrix_original)

# Extract class-specific metrics from the detailed confusion matrix on original data
original_data_metrics <- data.frame(
  Class = levels(sleep_new$target),
  Samples = table(sleep_new$target),
  Sensitivity = conf_matrix_original$byClass[, "Sensitivity"],
  Specificity = conf_matrix_original$byClass[, "Specificity"],
  Precision = conf_matrix_original$byClass[, "Pos Pred Value"],
  F1_Score = conf_matrix_original$byClass[, "F1"],
  Balanced_Accuracy = (conf_matrix_original$byClass[, "Sensitivity"] +
                        conf_matrix_original$byClass[, "Specificity"]) / 2
)

# Print class-specific metrics for predictions on original data
cat("\nClass-specific metrics when applied to original unbalanced data:\n")
print(original_data_metrics)

# Create a visualization of the original data confusion matrix
orig_conf_data <- as.data.frame(as.table(conf_matrix_original$table))
names(orig_conf_data) <- c("Reference", "Prediction", "Freq")

# Plot original data confusion matrix
ggplot(orig_conf_data, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 4) +
  scale_fill_gradient(low = "steelblue", high = "darkblue") +
  theme_minimal() +
  labs(title = "Confusion Matrix: SMOTE Model on Original Unbalanced Data",
       x = "Actual Class",
       y = "Predicted Class")

# Cross-validation using stratified sampling to preserve class ratios
set.seed(456)
train_control <- trainControl(
  method = "cv",
  number = 10,
  sampling = "smote",  # Apply SMOTE during cross-validation
  classProbs = TRUE,
  summaryFunction = multiClassSummary
)

# Train model with cross-validation
rf_cv_model <- train(
  target ~ .,
  data = sleep_new,
  method = "rf",
  trControl = train_control,
  tuneLength = 3,
  metric = "Balanced_Accuracy"  # Focus on balanced accuracy across classes
)

# Print cross-validation results
cat("\nCross-validation results with SMOTE:\n")
print(rf_cv_model)
print(rf_cv_model$results)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# check corelation between light, deep, and REM variables
cor(sleep_new[, c("light", "deep", "REM")])

# only REM is not highly correlated with light or deep. 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# drop highly correlated variables ('deep') since they are both higly correlated with 'light'. Keep 'light'
sleep_new2 <- subset(sleep_new, select = -deep)

sleep_new2


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## Observe new analsis results for SMOTE balanced dataset without 'deep' variable to see if there is a change in the analysis results for important variable features. 

# Use SMOTE to handle class imbalance in "Optimal" class for target variable
# Followed by train/test split on the balanced data

library(randomForest)
library(caret)
library(themis)  # For SMOTE implementation
library(recipes) # Required for themis

# Ensure target is of factor type
sleep_new2 <- sleep_new2 %>%
  mutate(target = factor(target, levels = c("Low", "Optimal", "High")))

# Check class distribution before SMOTE
cat("Class distribution before SMOTE:\n")
table(sleep_new2$target)

# Create a recipe for SMOTE
smote_recipe <- recipe(target ~ ., data = sleep_new2
                       ) %>%
  step_smote(target, over_ratio = 1.0) # Creates equal class sizes

# Prepare the data with SMOTE
smote_prep <- prep(smote_recipe)
sleep_balanced <- bake(smote_prep, new_data = NULL)

# Check class distribution after SMOTE
cat("\nClass distribution after SMOTE:\n")
table(sleep_balanced$target)

# Create train/test split on the SMOTE-balanced data (70% train, 30% test)
set.seed(123)  # For reproducible split
train_indices <- createDataPartition(sleep_balanced$target, p = 0.7, list = FALSE)
train_data <- sleep_balanced[train_indices, ]
test_data <- sleep_balanced[-train_indices, ]

# Check class distribution in training and test sets
cat("\nTraining set class distribution:\n")
table(train_data$target)
cat("\nTest set class distribution:\n")
table(test_data$target)

# Run random forest model on training data
set.seed(123)  # For reproducibility
rf_model_balanced <- randomForest(target ~ .,
                         data = train_data,
                         ntree = 500,
                         importance = TRUE)

# Print model summary
cat("\nSMOTE model performance (on balanced training data):\n")
print(rf_model_balanced)

# Extract class-specific metrics from the SMOTE model's confusion matrix on training data
n_classes <- length(levels(train_data$target))
smote_train_metrics <- data.frame(
  Class = levels(train_data$target),
  Training_Samples = table(train_data$target),
  OOB_Error_Rate = rf_model_balanced$confusion[1:n_classes, "class.error"],
  OOB_Accuracy = 1 - rf_model_balanced$confusion[1:n_classes, "class.error"]
)

# Print class-specific metrics for the SMOTE model on training data
cat("\nClass-specific metrics on SMOTE-balanced training data (OOB estimates):\n")
print(smote_train_metrics)

# Evaluate on balanced test data
test_predictions <- predict(rf_model_balanced, newdata = test_data)
test_conf_matrix <- confusionMatrix(test_predictions, test_data$target)
cat("\nPerformance on balanced test data:\n")
print(test_conf_matrix)

# Extract class-specific metrics for balanced test data
test_metrics <- data.frame(
  Class = levels(test_data$target),
  Test_Samples = table(test_data$target),
  Sensitivity = test_conf_matrix$byClass[, "Sensitivity"],
  Specificity = test_conf_matrix$byClass[, "Specificity"],
  Precision = test_conf_matrix$byClass[, "Pos Pred Value"],
  F1_Score = test_conf_matrix$byClass[, "F1"],
  Balanced_Accuracy = (test_conf_matrix$byClass[, "Sensitivity"] + 
                     test_conf_matrix$byClass[, "Specificity"]) / 2
)

# Print class-specific metrics for balanced test data
cat("\nClass-specific metrics on balanced test data:\n")
print(test_metrics)

# Extract variable importance
var_importance <- importance(rf_model_balanced)
var_importance_df <- as.data.frame(var_importance)
var_importance_df$Variable <- rownames(var_importance_df)

# Sort by Mean Decrease in Accuracy
var_importance_df <- var_importance_df %>%
  arrange(desc(MeanDecreaseAccuracy))

# Print variable importance
print(head(var_importance_df, 10))

# Plot variable importance
ggplot(var_importance_df, aes(x = reorder(Variable, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Variable Importance with Balanced Classes",
       subtitle = "Based on Mean Decrease in Accuracy",
       x = "",
       y = "Mean Decrease in Accuracy (higher = more important)")

# Create a visualization of the test confusion matrix
test_conf_data <- as.data.frame(as.table(test_conf_matrix$table))
names(test_conf_data) <- c("Reference", "Prediction", "Freq")

# Plot test confusion matrix
ggplot(test_conf_data, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 4) +
  scale_fill_gradient(low = "steelblue", high = "darkblue") +
  theme_minimal() +
  labs(title = "Confusion Matrix: SMOTE Model on Balanced Test Data",
       x = "Actual Class",
       y = "Predicted Class")

# Also evaluate on original unbalanced data to check real-world performance
predictions_original <- predict(rf_model_balanced, newdata = sleep_new2)
conf_matrix_original <- confusionMatrix(predictions_original, sleep_new2$target)
cat("\nPerformance on original unbalanced data:\n")
print(conf_matrix_original)

# Extract class-specific metrics from the detailed confusion matrix on original data
original_data_metrics <- data.frame(
  Class = levels(sleep_new2$target),
  Samples = table(sleep_new2$target),
  Sensitivity = conf_matrix_original$byClass[, "Sensitivity"],
  Specificity = conf_matrix_original$byClass[, "Specificity"],
  Precision = conf_matrix_original$byClass[, "Pos Pred Value"],
  F1_Score = conf_matrix_original$byClass[, "F1"],
  Balanced_Accuracy = (conf_matrix_original$byClass[, "Sensitivity"] +
                        conf_matrix_original$byClass[, "Specificity"]) / 2
)

# Print class-specific metrics for predictions on original data
cat("\nClass-specific metrics when applied to original unbalanced data:\n")
print(original_data_metrics)

# Create a visualization of the original data confusion matrix
orig_conf_data <- as.data.frame(as.table(conf_matrix_original$table))
names(orig_conf_data) <- c("Reference", "Prediction", "Freq")

# Plot original data confusion matrix
ggplot(orig_conf_data, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 4) +
  scale_fill_gradient(low = "steelblue", high = "darkblue") +
  theme_minimal() +
  labs(title = "Confusion Matrix: SMOTE Model on Original Unbalanced Data",
       x = "Actual Class",
       y = "Predicted Class")

# Cross-validation using stratified sampling to preserve class ratios
set.seed(456)
train_control <- trainControl(
  method = "cv",
  number = 10,
  sampling = "smote",  # Apply SMOTE during cross-validation
  classProbs = TRUE,
  summaryFunction = multiClassSummary
)

# Train model with cross-validation
rf_cv_model <- train(
  target ~ .,
  data = sleep_new2,
  method = "rf",
  trControl = train_control,
  tuneLength = 3,
  metric = "Balanced_Accuracy"  # Focus on balanced accuracy across classes
)

# Print cross-validation results
cat("\nCross-validation results with SMOTE:\n")
print(rf_cv_model)
print(rf_cv_model$results)

