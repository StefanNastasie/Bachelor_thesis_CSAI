library(tidyverse)
# 1. Load the trial data
trials <- read.csv('C:\\Users\\stefa\\Desktop\\Thesis\\Thesis_v3\\data_long_modified.csv', header = TRUE, 
    colClasses = c("numeric", "numeric", "factor", "numeric", "numeric", "factor", "numeric", "factor", "factor", "numeric", "numeric", "factor", "factor"), 
    na.strings = c("none", "NA"))

# 2. Load stimulus characteristics
blp.stim <- read.delim("C:\\Users\\stefa\\OneDrive\\Documents\\UNI\\YEAR 3\\II\\Thesis\\words\\blp-stimuli.txt", header = TRUE, sep = "\t")

# Load blp-items data
blp.items <- read.delim("C:\\Users\\stefa\\OneDrive\\Documents\\UNI\\YEAR 3\\II\\Thesis\\words\\blp-items.txt", header = TRUE, sep = "\t")

# 3. Merge trial data with stimulus characteristics
d <- merge(trials, blp.stim, by.x = "target", by.y = "spelling", all.x = TRUE)

# Merge with d
d <- merge(d, blp.items, by.x = "target", by.y = "spelling", all.x = TRUE)


# 4. Prepare data sets by tasks

# Split the data by task
d.ld <- d %>% filter(task == "ld")
d.pdm <- d %>% filter(task == "pdm")
d.prpc <- d %>% filter(task == "prpc")

# 4.1 Aggregate ld trial data by task and and target to get average response_time and accuracy (proportion correct)
# Keep subtlex.frequency data
d.ld.agg.1 <- d.ld %>% group_by(target) %>% summarize(mean_rt = mean(response_time), mean_acc = mean(correct), subtlex.frequency = mean(subtlex.frequency))

# filter out trials with response times greater than the median + 1.5xIQR
upper_threshold_ld <- with(d.ld.agg.1, median(mean_rt, na.rm = TRUE) + 1.5 * IQR(mean_rt, na.rm = TRUE))
lower_threshold_ld <- with(d.ld.agg.1, median(mean_rt, na.rm = TRUE) - 1.5 * IQR(mean_rt, na.rm = TRUE))

d.ld.agg.2 <- d.ld.agg.1 %>% filter(mean_rt < upper_threshold_ld, mean_rt > lower_threshold_ld)


# see the leftovers
filtered_out_data_ld <- setdiff(d.ld.agg.1, d.ld.agg.2)
summary_filtered_out_data_ld <- filtered_out_data_ld %>%
    summarise(across(where(is.numeric), list(mean = ~mean(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE))),
              across(where(is.factor), list(nlevels = ~length(unique(.)))))


# 4.2 Aggregate pdm trial data by task and and target to get average actual_exposure and accuracy (proportion correct)
# Keep subtlex.frequency data
d.pdm.agg.1 <- d.pdm %>% group_by(target) %>% summarize(mean_exposure = mean(actual_exposure), mean_acc = mean(correct), subtlex.frequency = mean(subtlex.frequency))
# filter out trials with average exposures greater than the median + 2xIQR
upper_threshold_pdm <- with(d.pdm.agg.1, median(mean_exposure, na.rm = TRUE) + 1.5 * IQR(mean_exposure, na.rm = TRUE))
lower_threshold_pdm <- with(d.pdm.agg.1, median(mean_exposure, na.rm = TRUE) - 1.5 * IQR(mean_exposure, na.rm = TRUE))

d.pdm.agg.2 <- d.pdm.agg.1 %>% filter(mean_exposure < upper_threshold_pdm, mean_exposure > lower_threshold_pdm)


# see the leftovers
filtered_out_data_pdm <- setdiff(d.pdm.agg.1, d.pdm.agg.2)
summary_filtered_out_data_pdm <- filtered_out_data_pdm %>%
    summarise(across(where(is.numeric), list(mean = ~mean(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE))),
              across(where(is.factor), list(nlevels = ~length(unique(.)))))



# 4.3 Aggregate prpc trial data by task and and target to get average components and accuracy (proportion correct)
# Keep subtlex.frequency data
d.prpc.agg.1 <- d.prpc %>% group_by(target) %>% summarize(mean_components = mean(components), mean_acc = mean(correct), subtlex.frequency = mean(subtlex.frequency), mean_timestamp = mean(timestamp))

# remove rows where target is 'nan'
d.prpc.agg.1 <- d.prpc.agg.1 %>%
  filter(target != 'nan')

# filter out trials with average components greater than the median + 2xIQR
upper_threshold_prpc_2 <- with(d.prpc.agg.1, median(mean_components, na.rm = TRUE) + 1.5 * IQR(mean_components, na.rm = TRUE))
lower_threshold_prpc_2 <- with(d.prpc.agg.1, median(mean_components, na.rm = TRUE) - 1.5 * IQR(mean_components, na.rm = TRUE))

d.prpc.agg.2 <- d.prpc.agg.1 %>% filter(mean_components < upper_threshold_prpc_2, mean_components > lower_threshold_prpc_2)

# see the leftovers
filtered_out_data_prpc_2 <- setdiff(d.prpc.agg.1, d.prpc.agg.2)
summary_filtered_out_data_prpc_2 <- filtered_out_data_prpc_2 %>%
    summarise(across(where(is.numeric), list(mean = ~mean(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE))),
              across(where(is.factor), list(nlevels = ~length(unique(.)))))


# filter out trials with average timestamp greater/lower than the median + 2xIQR
upper_threshold_prpc_3 <- with(d.prpc.agg.1, median(mean_timestamp, na.rm = TRUE) + 1.5 * IQR(mean_timestamp, na.rm = TRUE))
lower_threshold_prpc_3 <- with(d.prpc.agg.1, median(mean_timestamp, na.rm = TRUE) - 1.5 * IQR(mean_timestamp, na.rm = TRUE))

d.prpc.agg.3 <- d.prpc.agg.1 %>% filter(mean_timestamp < upper_threshold_prpc_3, mean_timestamp > lower_threshold_prpc_3)

# see the leftovers
filtered_out_data_prpc_3 <- setdiff(d.prpc.agg.1, d.prpc.agg.3)
summary_filtered_out_data_prpc_3 <- filtered_out_data_prpc_3 %>%
    summarise(across(where(is.numeric), list(mean = ~mean(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE))),
              across(where(is.factor), list(nlevels = ~length(unique(.)))))

# join the prpc data
d.prpc.agg.4 <- inner_join(d.prpc.agg.2, d.prpc.agg.3, by = "target")


# make a histogram of average response times for the ld data




plot1 <- ggplot(d.ld.agg.2, aes(x = mean_rt)) + 
         geom_histogram(binwidth = 50, fill = "#1a008e") + 
         xlab("Response Time (ms)") + ylab("Log10(Subtlex Frequency)") +
         theme(axis.title.x = element_text(size = 25),  
            axis.title.y = element_text(size = 25),  
            axis.text.x = element_text(size = 15),  
            axis.text.y = element_text(size = 15))   
ggsave("C:\\Users\\stefa\\OneDrive\\Documents\\UNI\\YEAR 3\\II\\Thesis\\Tables and figures\\hist_ld_new.png", plot1)


# make a histogram of average exposure times for the pdm data
plot2 <- ggplot(d.pdm.agg.2, aes(x = mean_exposure)) + 
  geom_histogram(binwidth = 1, fill = "#ff0000") + 
  xlab("Exposure Time (ms)") + ylab("Log10(Subtlex Frequency)")
  theme(axis.title.x = element_text(size = 25),  
            axis.title.y = element_text(size = 25),  
            axis.text.x = element_text(size = 15),  
            axis.text.y = element_text(size = 15))
ggsave("C:\\Users\\stefa\\OneDrive\\Documents\\UNI\\YEAR 3\\II\\Thesis\\Tables and figures\\hist_pdm_new.png", plot2)



# make a histogram of number of components for the prpc data
plot3 <- ggplot(d.prpc.agg.4, aes(x = mean_components.x)) + 
  geom_histogram(binwidth = 5, fill = "#00a455") + 
  xlab("Components") + ylab("Log10(Subtlex Frequency)")
  theme(axis.title.x = element_text(size = 25),  
            axis.title.y = element_text(size = 25),  
            axis.text.x = element_text(size = 15),  
            axis.text.y = element_text(size = 15))
ggsave("C:\\Users\\stefa\\OneDrive\\Documents\\UNI\\YEAR 3\\II\\Thesis\\Tables and figures\\hist_prpc_components_new.png", plot3)

plot4 <- ggplot(d.prpc.agg.4, aes(x = mean_timestamp.x)) + 
  geom_histogram(binwidth = 200, fill = "#f2ff00") + 
  xlab("Trial time (ms)") + ylab("Log10(Subtlex Frequency)")
  theme(axis.title.x = element_text(size = 25),  
            axis.title.y = element_text(size = 25),  
            axis.text.x = element_text(size = 15),  
            axis.text.y = element_text(size = 15))
ggsave("C:\\Users\\stefa\\OneDrive\\Documents\\UNI\\YEAR 3\\II\\Thesis\\Tables and figures\\hist_prpc_time_new.png", plot4)



# make a plot of response time vs word frequency for lexical decision
plot4 <- ggplot(d.ld.agg.2, aes(x = log10(subtlex.frequency), y = mean_rt)) + 
  geom_point(color = "blue") +
  ggtitle("Response Time vs. Word Frequency") + 
  xlab("Word Log10(Subtlex Frequency)") + ylab("Response Time (ms)")

# make a plot of exposure time vs word frequency for progressive demasking
plot5 <- ggplot(d.pdm.agg.2, aes(x = log10(subtlex.frequency), y = mean_exposure)) + 
  geom_point(color = "blue") +
  ggtitle("Exposure Time vs. Word Frequency") + 
  xlab("Log10(Subtlex Frequency)") + ylab("Exposure Time (ms)")

# make a plot of number of components vs word frequency for PRPC
plot6 <- ggplot(d.prpc.agg.4, aes(x = log10(subtlex.frequency.x), y = mean_components.x)) + 
  geom_point(color = "blue") +
  ggtitle("Components vs. Word Frequency") + 
  xlab("Log10(Subtlex Frequency)") + ylab("Components (n)")


# 5. Run linear regression models (to compute effect sizes)

ld.model1 <- lm(mean_rt~log10(subtlex.frequency), data=d.ld.agg.2)
pdm.model1 <- lm(mean_exposure~log10(subtlex.frequency), data=d.pdm.agg.2)
prpc.model1 <- lm(mean_components~log10(subtlex.frequency), data=d.prpc.agg.2)
prpc.model2 <- lm(mean_timestamp~log10(subtlex.frequency), data=d.prpc.agg.3)
prpc.model3 <- lm(mean_timestamp.x~log10(subtlex.frequency.x), data=d.prpc.agg.4)
prpc.model4 <- lm(mean_components.x~log10(subtlex.frequency.x), data=d.prpc.agg.4)




# Plot with regression line and confidence interval
plot7 <- ggplot(d.ld.agg.2, aes(x=log10(subtlex.frequency), y=mean_rt)) + 
  geom_point() +
  geom_smooth(method=lm, se=TRUE, color="#ff0000", fill="lightblue") +
  theme(axis.title.x = element_text(size = 25),  
            axis.title.y = element_text(size = 25),  
            axis.text.x = element_text(size = 15),  
            axis.text.y = element_text(size = 15))
ggsave("C:\\Users\\stefa\\OneDrive\\Documents\\UNI\\YEAR 3\\II\\Thesis\\Tables and figures\\ld_model_new.png", plot7)


plot8 <- ggplot(d.pdm.agg.2, aes(x=log10(subtlex.frequency), y=mean_exposure)) + 
  geom_point() +
  geom_smooth(method=lm, se=TRUE, color="#ff0000", fill="lightblue") +
  theme(axis.title.x = element_text(size = 25),  
            axis.title.y = element_text(size = 25),  
            axis.text.x = element_text(size = 15),  
            axis.text.y = element_text(size = 15))
ggsave("C:\\Users\\stefa\\OneDrive\\Documents\\UNI\\YEAR 3\\II\\Thesis\\Tables and figures\\pdm_model_new.png", plot8)


plot9 <- ggplot(d.prpc.agg.2, aes(x=log10(subtlex.frequency), y=mean_components)) + 
  geom_point() +
  geom_smooth(method=lm, se=TRUE, color="#ff0000", fill="lightblue") +
  theme(axis.title.x = element_text(size = 25),  
            axis.title.y = element_text(size = 25),  
            axis.text.x = element_text(size = 15),  
            axis.text.y = element_text(size = 15))
ggsave("C:\\Users\\stefa\\OneDrive\\Documents\\UNI\\YEAR 3\\II\\Thesis\\Tables and figures\\hist_prpc_time_new.png", plot9)


plot10 <- ggplot(d.prpc.agg.3, aes(x=log10(subtlex.frequency), y=mean_timestamp)) + 
  geom_point() +
  geom_smooth(method=lm, se=TRUE, color="#ff0000", fill="lightblue") +
  theme(axis.title.x = element_text(size = 25),  
            axis.title.y = element_text(size = 25),  
            axis.text.x = element_text(size = 15),  
            axis.text.y = element_text(size = 15))
ggsave("C:\\Users\\stefa\\OneDrive\\Documents\\UNI\\YEAR 3\\II\\Thesis\\Tables and figures\\hist_prpc_time_new.png", plot10)


plot11 <- ggplot(d.prpc.agg.4, aes(x=log10(subtlex.frequency.x), y=mean_timestamp.x)) + 
  geom_point() +
  geom_smooth(method=lm, se=TRUE, color="#ff0000", fill="lightblue") +
  theme(axis.title.x = element_text(size = 25),  
            axis.title.y = element_text(size = 25),  
            axis.text.x = element_text(size = 15),  
            axis.text.y = element_text(size = 15))
ggsave("C:\\Users\\stefa\\OneDrive\\Documents\\UNI\\YEAR 3\\II\\Thesis\\Tables and figures\\prpc_model_time_new.png", plot11)


plot12 <- ggplot(d.prpc.agg.4, aes(x=log10(subtlex.frequency.x), y=mean_components.x)) + 
  geom_point() +
  geom_smooth(method=lm, se=TRUE, color="#ff0000", fill="lightblue") +
  theme(axis.title.x = element_text(size = 25),  
            axis.title.y = element_text(size = 25),  
            axis.text.x = element_text(size = 15),  
            axis.text.y = element_text(size = 15))
ggsave("C:\\Users\\stefa\\OneDrive\\Documents\\UNI\\YEAR 3\\II\\Thesis\\Tables and figures\\prpc_model_components_new.png", plot12)

 
# Residual analysis

res.ld.model1 <- resid(ld.model1)
res.pdm.model1 <- resid(pdm.model1)
res.prpc.model1 <- resid(prpc.model1)
res.prpc.model2 <- resid(prpc.model2)


# ld
plot(ld.model1$fitted.values, res.ld.model1, 
     xlab = "Fitted values (ld)", ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")

qqnorm(res.ld.model1)
qqline(res.ld.model1)

library(car)
durbinWatsonTest(res.ld.model1)

# pdm
plot(pdm.model1$fitted.values, res.pdm.model1, 
     xlab = "Fitted values (pdm)", ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")

qqnorm(res.pdm.model1)
qqline(res.pdm.model1)

library(car)
durbinWatsonTest(res.pdm.model1)


# prpc.1
plot(prpc.model1$fitted.values, res.prpc.model1, 
     xlab = "Fitted values (prpc)", ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")

qqnorm(res.prpc.model1)
qqline(res.prpc.model1)

library(car)
durbinWatsonTest(res.prpc.model1)

# prpc.2
plot(prpc.model2$fitted.values, res.prpc.model2, 
     xlab = "Fitted values (prpc)", ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")

qqnorm(res.prpc.model2)
qqline(res.prpc.model2)

library(car)
durbinWatsonTest(res.prpc.model2)



# 6. Extras
# make a plot of response time vs word frequency for PRPC
ggplot(d.prpc.agg.3, aes(x = log10(subtlex.frequency), y = mean_timestamp)) + geom_point() +
ggtitle("Response time vs. Word Frequency") + xlab("Word Frequency") + ylab("RT (ms)")

# calculate the accuracy per task for each participant
accuracy_per_task_per_participant <- d %>%
  mutate(subject_nr = as.numeric(as.character(subject_nr))) %>%
  group_by(subject_nr, task) %>%
  summarise(accuracy = mean(correct, na.rm = TRUE)) %>%
  mutate(subject_nr = factor(subject_nr, levels = 1:max(subject_nr)))

# print the data
print(accuracy_per_task_per_participant)

# plot accuracy per task for each participant
ggplot(accuracy_per_task_per_participant, aes(x=subject_nr, y=accuracy, fill=task)) +
  geom_bar(stat="identity", position=position_dodge(), width=0.7) +
  geom_text(aes(label=round(accuracy, 2)), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  labs(title = "Accuracy by Task for Each Participant", 
       x="Participant", y="Accuracy", fill="Task") +
  scale_fill_brewer(palette="Set1")

# Calculate mean, median and mode per task
summary_stats_per_task <- d %>%
  group_by(task) %>%
  summarise(mean_accuracy = mean(correct, na.rm = TRUE),
            median_accuracy = median(correct, na.rm = TRUE),
            mode_accuracy = as.numeric(names(which.max(table(correct))))) %>%
  pivot_longer(cols = -task, names_to = "Statistic", values_to = "Accuracy")

# Plot mean, median and mode per task
ggplot(summary_stats_per_task, aes(x=task, y=Accuracy, fill=Statistic)) +
  geom_bar(stat="identity", position=position_dodge(), width=0.7) +
  geom_text(aes(label=round(Accuracy, 2)), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  labs(title = "Summary Statistics of Accuracy by Task", 
       x="Task", y="Accuracy", fill="Statistic") +
  scale_fill_brewer(palette="Set2")

# Calculate mean accuracy per task
mean_accuracy_per_task <- d %>%
  group_by(task) %>%
  summarise(mean_accuracy = mean(correct, na.rm = TRUE))

# Print the table
print(mean_accuracy_per_task)



# Calculate mean correct responses for each 'target' in 'ld' task
# Merge 'blp.items' with 'd.ld.agg.2'
d.ld.agg.2 <- merge(d.ld.agg.2, blp.items, by.x = "target", by.y = "spelling", all.x = TRUE)

# Make a scatter plot for 'ld' task
ggplot(d.ld.agg.2, aes(x = accuracy, y = mean_rt)) +
  geom_point() +
  labs(title = "Mean Response Time vs Prevalence for Each Target in 'ld' Task",
       x = "Prevalence",
       y = "Mean Response Time (ms)")

# Merge 'blp.items' with 'd.pdm.agg.2'
d.pdm.agg.2 <- merge(d.pdm.agg.2, blp.items, by.x = "target", by.y = "spelling", all.x = TRUE)

# Make a scatter plot for 'pdm' task
ggplot(d.pdm.agg.2, aes(x = accuracy, y = mean_exposure)) +
  geom_point() +
  labs(title = "Mean Exposure vs Prevalence for Each Target in 'pdm' Task",
       x = "Prevalence",
       y = "Mean Exposure (ms)")

# Merge 'blp.items' with 'd.prpc.agg.2'
d.prpc.agg.2 <- merge(d.prpc.agg.2, blp.items, by.x = "target", by.y = "spelling", all.x = TRUE)

# Make a scatter plot for 'prpc' task
ggplot(d.prpc.agg.2, aes(x = accuracy, y = mean_components)) +
  geom_point() +
  labs(title = "Mean Components vs Prevalence for Each Target in 'prpc' Task",
       x = "Prevalence",
       y = "Mean Components")

# Merge 'blp.items' with 'd.prpc.agg.3'
d.prpc.agg.3 <- merge(d.prpc.agg.3, blp.items, by.x = "target", by.y = "spelling", all.x = TRUE)

# Make a scatter plot for 'prpc' task
ggplot(d.prpc.agg.3, aes(x = accuracy, y = mean_timestamp)) +
  geom_point() +
  labs(title = "Mean Timestamp vs Prevalence for Each Target in 'prpc' Task",
       x = "Prevalence",
       y = "Mean Timestamp")




# Remove rows with missing values
d_complete <- na.omit(d_numeric)

# Compute correlation matrix
cor_matrix <- cor(d_complete)

# Impute missing values with column means
d_imputed <- apply(d_numeric, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

# Compute correlation matrix
cor_matrix <- cor(d_imputed)

# Print correlation matrix
print(cor_matrix)

# Plot correlation matrix
corrplot(cor_matrix, method = "color")


