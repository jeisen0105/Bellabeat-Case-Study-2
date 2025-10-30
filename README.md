# Case Study: Cyclistic Bike-Share Analysis (Google Data Analytics Capstone)

## Introduction

## Background

## Scenario

## Step 1: Ask

## Step 2: Prepare

### Data Integrity Check (ROCCC)

### Preparing RStudio

```r
install.packages("tidyverse")
install.packages("janitor")
install.packages("lubridate")

library(tidyverse)
library(janitor) 
library(lubridate)

daily_activity <- read_csv("dailyActivity_merged.csv")
daily_sleep <- read_csv("sleepDay_merged.csv")
hourly_steps <- read_csv("hourlySteps_merged.csv")
hourly_calories <- read_csv("hourlyCalories_merged.csv")
```

## Step 3: Process

### Data Cleaning 

```r
daily_activity_clean <- daily_activity %>%
  clean_names() %>%
  distinct() %>%
  mutate(id = as.character(id)) %>%
  rename(date = activity_date) %>%
  mutate(date = mdy(date)) %>%
  select(-tracker_distance, -logged_activities_distance, -sedentary_active_distance)

daily_sleep_clean <- daily_sleep %>%
  clean_names() %>%
  distinct() %>%
  mutate(id = as.character(id)) %>%
  rename(date = sleep_day) %>%
  mutate(date = as_date(mdy_hms(date))) %>%
  select(-total_sleep_records)

hourly_steps_clean <- hourly_steps %>%
  clean_names() %>%
  mutate(id = as.character(id)) %>%
  mutate(
    activity_hour = mdy_hms(activity_hour),
    hour = hour(activity_hour) 
  )

hourly_calories_clean <- hourly_calories %>%
  clean_names() %>%
  mutate(id = as.character(id)) %>%
  mutate(
    activity_hour = mdy_hms(activity_hour),
    hour = hour(activity_hour)
  )
```

### Data Merging

```r
daily_combined <- full_join(
  daily_activity_clean,
  daily_sleep_clean,
  by = c("id", "date")
)

hourly_combined <- full_join(
  hourly_steps_clean,
  hourly_calories_clean,
  by = c("id", "activity_hour", "hour")
)
```

### Adding Columns

#### Adding a day of week column to both combined datasetes

```r
daily_combined <- daily_combined %>%
  mutate(
    day_of_week = lubridate::wday(date, label = TRUE, abbr = FALSE)
  )

hourly_combined <- hourly_combined %>%
  mutate(
    day_of_week = lubridate::wday(activity_hour, label = TRUE, abbr = FALSE)
  )
```

#### Adding a total active minutes column to daily combined dataset

```r
daily_combined <- daily_combined %>%
  mutate(
    total_active_minutes = very_active_minutes + fairly_active_minutes + lightly_active_minutes
  )
```

## Step 4: Analyze

### Graphing Average Steps by Day of Week

```r
daily_combined %>%
  group_by(day_of_week) %>%
  summarise(average_steps = mean(total_steps, na.rm = TRUE)) %>%
  ggplot(aes(x = day_of_week, y = average_steps)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Average Steps by Day of the Week",
    x = "Day of the Week",
    y = "Average Steps"
  ) +
  theme_minimal()
```

### Graphing Average Steps by Hour of Day

```r
hourly_combined %>%
  group_by(hour) %>%
  summarise(average_steps = mean(step_total, na.rm = TRUE)) %>%
  ggplot(aes(x = hour, y = average_steps)) +
  geom_bar(stat = "identity", fill = "coral") +
  labs(
    title = "Average Steps by Hour of the Day",
    x = "Hour of the Day (0-23)",
    y = "Average Steps"
  ) +
  theme_minimal()
```

### Calculating Pearson Correlation Coefficent between total steps and calories

```r
r_steps_calories <- daily_combined %>%
  filter(!is.na(total_steps) & !is.na(calories)) %>%
  summarise(r = cor(total_steps, calories)) %>%
  pull(r)
r_steps_calories <- round(r_steps_calories, 2)
```

### Graphing the correlation betwen total steps and calories

```r
daily_combined %>%
  ggplot(aes(x = total_steps, y = calories)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = paste0("Correlation: Daily Steps vs. Calories Burned (R = ", r_steps_calories, ")"),
    x = "Total Steps",
    y = "Calories Burned"
  ) +
  theme_minimal()
```

### Calculating Pearson Correlation Coefficent between total steps and total minutes asleep

```r
r_steps_sleep <- daily_combined %>%
  filter(!is.na(total_minutes_asleep)) %>%
  summarise(r = cor(total_steps, total_minutes_asleep)) %>%
  pull(r)
r_steps_sleep <- round(r_steps_sleep, 2)
```

### Graphing the correlation betwen total steps and total minutes asleep

```r
daily_combined %>%
  filter(!is.na(total_minutes_asleep)) %>%
  ggplot(aes(x = total_steps, y = total_minutes_asleep)) +
  geom_point(alpha = 0.5, color = "darkred") +
  geom_smooth(method = "lm", color = "blue") +
  labs(
    title = paste0("Correlation: Total Steps vs. Total Minutes Asleep (R = ", r_steps_sleep, ")"),
    x = "Total Steps",
    y = "Total Minutes Asleep"
  ) +
  theme_minimal()
```

### Calculating Pearson Correlation Coefficent between sedentary minutes and total minutes asleep

```r
r_sedentary_sleep <- daily_combined %>%
  filter(!is.na(total_minutes_asleep)) %>%
  summarise(r = cor(sedentary_minutes, total_minutes_asleep)) %>%
  pull(r)
r_sedentary_sleep <- round(r_sedentary_sleep, 2)
```

### Graphing the correlation betwen sedentary minutes and total minutes asleep

```r
daily_combined %>%
  filter(!is.na(total_minutes_asleep)) %>%
  ggplot(aes(x = sedentary_minutes, y = total_minutes_asleep)) +
  geom_point(alpha = 0.5, color = "purple") +
  geom_smooth(method = "lm", color = "green") +
  labs(
    title = paste0("Correlation: Sedentary Minutes vs. Total Minutes Asleep (R = ", r_sedentary_sleep, ")"),
    x = "Sedentary Minutes",
    y = "Total Minutes Asleep"
  ) +
  theme_minimal()
```

### Calculating Pearson Correlation Coefficent between lightly active minutes and total minutes asleep

```r
r_lightly_sleep <- daily_combined %>%
  filter(!is.na(total_minutes_asleep)) %>%
  summarise(r = cor(lightly_active_minutes, total_minutes_asleep)) %>%
  pull(r)
r_lightly_sleep <- round(r_lightly_sleep, 2)
```

### Graphing the correlation betwen lightly active minutes and total minutes asleep

```r
daily_combined %>%
  filter(!is.na(total_minutes_asleep)) %>%
  ggplot(aes(x = lightly_active_minutes, y = total_minutes_asleep)) +
  geom_point(alpha = 0.5, color = "orange") +
  geom_smooth(method = "lm", color = "black") +
  labs(
    title = paste0("Correlation: Lightly Active Minutes vs. Total Minutes Asleep (R = ", r_lightly_sleep, ")"),
    x = "Lightly Active Minutes",
    y = "Total Minutes Asleep"
  ) +
  theme_minimal()
```

### Calculating Pearson Correlation Coefficent between fairly active minutes and total minutes asleep

```r
r_fairly_sleep <- daily_combined %>%
  filter(!is.na(total_minutes_asleep)) %>%
  summarise(r = cor(fairly_active_minutes, total_minutes_asleep)) %>%
  pull(r)
r_fairly_sleep <- round(r_fairly_sleep, 2)
```

### Graphing the correlation betwen fairly active minutes and total minutes asleep

```r
daily_combined %>%
  filter(!is.na(total_minutes_asleep)) %>%
  ggplot(aes(x = fairly_active_minutes, y = total_minutes_asleep)) +
  geom_point(alpha = 0.5, color = "brown") +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = paste0("Correlation: Fairly Active Minutes vs. Total Minutes Asleep (R = ", r_fairly_sleep, ")"),
    x = "Fairly Active Minutes",
    y = "Total Minutes Asleep"
  ) +
  theme_minimal()
```

### Calculating Pearson Correlation Coefficent between very active minutes and total minutes asleep

```r
r_very_sleep <- daily_combined %>%
  filter(!is.na(total_minutes_asleep)) %>%
  summarise(r = cor(very_active_minutes, total_minutes_asleep)) %>%
  pull(r)
r_very_sleep <- round(r_very_sleep, 2)
```

### Graphing the correlation betwen very active minutes and total minutes asleep

```r
daily_combined %>%
  filter(!is.na(total_minutes_asleep)) %>%
  ggplot(aes(x = very_active_minutes, y = total_minutes_asleep)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", color = "purple") +
  labs(
    title = paste0("Correlation: Very Active Minutes vs. Total Minutes Asleep (R = ", r_very_sleep, ")"),
    x = "Very Active Minutes",
    y = "Total Minutes Asleep"
  ) +
  theme_minimal()
```
