library(tidyverse)

# 1. Load the data
trials <- read.csv("C:\\Users\\stefa\\OneDrive\\Documents\\UNI\\YEAR 3\\II\\Thesis\\experiments\\results\\results\\merged\\merged_ld_all.csv", header = TRUE, 
    colClasses = c("numeric", "numeric", "numeric", "factor", "factor", "numeric", "factor", "factor", "factor")) 

# 2. Aggregate the data by subject, category and calculate the mean response_time
trials_agg <- trials %>%
  group_by(target, category) %>%
  summarise(mean_rt = mean(response_time, na.rm = TRUE),
            mean_correct = mean(correct, na.rm = TRUE)) %>%
  ungroup()

# 3. Remove outliers 
trials_agg <- trials_agg %>%
  filter(mean_rt < median(mean_rt, na.rm = TRUE) + 2*IQR(mean_rt, na.rm = TRUE))

# calculate mean accuracy per category
accuracy_table <- trials_agg %>%
  group_by(category) %>%
  summarise(mean_accuracy = mean(mean_correct, na.rm = TRUE))

# calculate overall mean accuracy
overall_accuracy <- data.frame(category = "Overall", mean_accuracy = mean(trials_agg$mean_correct, na.rm = TRUE))

# bind the two dataframes
accuracy_table <- rbind(accuracy_table, overall_accuracy)

print(accuracy_table)


# Plot for accuracies
ggplot(trials_agg, aes(x = category, y = mean_correct, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Category", y = "Accuracy", fill = "Category", 
       title = "Accuracy across categories") +
  theme_minimal()


# Reaction times table
rt_table <- trials_agg %>%
  group_by(category) %>%
  summarise(mean_rt = mean(mean_rt, na.rm = TRUE))

overall_rt <- data.frame(category = "Overall", mean_rt = mean(trials_agg$mean_rt, na.rm = TRUE))


rt_table <- rbind(rt_table, overall_rt)

print(rt_table)

# Plot for reaction times
ggplot(trials_agg, aes(x = category, y = mean_rt, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Category", y = "Reaction Time", fill = "Category", 
       title = "Reaction Time across categories") +
  theme_minimal()

