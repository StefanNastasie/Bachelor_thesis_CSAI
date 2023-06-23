library(tidyverse)

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

# Transform 'response_time' column in d.ld to log space
d.ld$response_time <- log(d.ld$response_time)

# Transform 'actual_exposure' column in d.pdm to log space
d.pdm$actual_exposure <- log(d.pdm$actual_exposure)


d.ld_original <- d.ld
d.pdm_original <- d.pdm
d.prpc_original <- d.prpc



# For Lexical Decision task
d.ld <- d.ld %>% 
  group_by(subject_nr) %>% 
  mutate(rt_IQR = IQR(response_time, na.rm = TRUE),
         rt_median = median(response_time, na.rm = TRUE)) %>% 
  filter(response_time > (rt_median - 1.5*rt_IQR) & response_time < (rt_median + 1.5*rt_IQR)) %>% 
  select(-rt_IQR, -rt_median) 

# For Progressive Demasking task
d.pdm <- d.pdm %>% 
  group_by(subject_nr) %>% 
  mutate(rt_IQR = IQR(actual_exposure, na.rm = TRUE),
         exposure_median = median(actual_exposure, na.rm = TRUE)) %>% 
  filter(actual_exposure > (exposure_median - 1.5*rt_IQR) & actual_exposure < (exposure_median + 1.5*rt_IQR)) %>% 
  select(-rt_IQR, -exposure_median) 

# For PRPC task by components
d.prpc.components <- d.prpc %>% 
  group_by(subject_nr) %>% 
  mutate(rt_IQR = IQR(components, na.rm = TRUE),
         components_median = median(components, na.rm = TRUE)) %>% 
  filter(components > (components_median - 1.5*rt_IQR) & components < (components_median + 1.5*rt_IQR)) %>% 
  select(-rt_IQR, -components_median) 


# For PRPC task by timestamp
d.prpc.timestamp <- d.prpc %>% 
  group_by(subject_nr) %>% 
  mutate(rt_IQR = IQR(timestamp, na.rm = TRUE),
         timestamp_median = median(timestamp, na.rm = TRUE)) %>% 
  filter(timestamp > (timestamp_median - 1.5*rt_IQR) & timestamp < (timestamp_median + 1.5*rt_IQR)) %>% 
  select(-rt_IQR, -timestamp_median) 


# Identify excluded data and summarize 
excluded.ld <- anti_join(d.ld_original, d.ld, by = c("subject_nr", "target"))
excluded.pdm <- anti_join(d.pdm_original, d.pdm, by = c("subject_nr", "target"))
excluded.prpc.components <- anti_join(d.prpc_original, d.prpc.components, by = c("subject_nr", "target"))
excluded.prpc.timestamp <- anti_join(d.prpc_original, d.prpc.timestamp, by = c("subject_nr", "target"))


# Count how many times each target was excluded
summary.ld <- excluded.ld %>% group_by(target) %>% summarise(exclusions = n())
summary.pdm <- excluded.pdm %>% group_by(target) %>% summarise(exclusions = n())
summary.prpc.components <- excluded.prpc.components %>% group_by(target) %>% summarise(exclusions = n())
summary.prpc.timestamp <- excluded.prpc.timestamp %>% group_by(target) %>% summarise(exclusions = n())

# Count total exclusions
total_exclusions.ld <- nrow(excluded.ld)
total_exclusions.pdm <- nrow(excluded.pdm)
total_exclusions.prpc.components <- nrow(excluded.prpc.components)
total_exclusions.prpc.timestamp <- nrow(excluded.prpc.timestamp)


# Print total exclusions
print(paste("Total exclusions for ld task: ", total_exclusions.ld))
print(paste("Total exclusions for pdm task: ", total_exclusions.pdm))
print(paste("Total exclusions for prpc task (components): ", total_exclusions.prpc.components))
print(paste("Total exclusions for prpc task (timestamp): ", total_exclusions.prpc.timestamp))


# Sort and print summaries
summary.ld_sorted <- summary.ld %>% arrange(desc(exclusions))
summary.pdm_sorted <- summary.pdm %>% arrange(desc(exclusions))
summary.prpc_sorted.components <- summary.prpc.components %>% arrange(desc(exclusions))
summary.prpc_sorted.timestamp <- summary.prpc.timestamp %>% arrange(desc(exclusions))



print(summary.ld_sorted)
print(summary.pdm_sorted)
print(summary.prpc_sorted.components)
print(summary.prpc_sorted.timestamp)

# Create boxplot for each task before removing outliers
library(gridExtra)

plot_ld_original <- ggplot(d.ld_original, aes(x = "LD Original", y = response_time)) + 
  geom_boxplot(fill = "blue") +
  labs(title = "Lexical Decision Task - Response Time", y = "Response Time") +
  theme_minimal()

plot_pdm_original <- ggplot(d.pdm_original, aes(x = "PDM Original", y = actual_exposure)) + 
  geom_boxplot(fill = "red") +
  labs(title = "Progressive Demasking Task - Exposure Time", y = "Exposure Time") +
  theme_minimal()

plot_prpc_original_comp <- ggplot(d.prpc_original, aes(x = "PRPC Original", y = components)) + 
  geom_boxplot(fill = "green") +
  labs(title = "PRPC Task - Components", y = "Components") +
  theme_minimal()

plot_prpc_original_time <- ggplot(d.prpc_original, aes(x = "PRPC Original", y = timestamp)) + 
  geom_boxplot(fill = "purple") +
  labs(title = "PRPC Task - Timestamp", y = "Timestamp") +
  theme_minimal()

grid.arrange(plot_ld_original, plot_pdm_original, plot_prpc_original_comp, plot_prpc_original_time, ncol = 4, nrow = 1)




d.ld.agg.1 <- d.ld %>% group_by(target) %>% summarize(mean_rt = mean(response_time), mean_acc = mean(correct), subtlex.frequency = mean(subtlex.frequency))
d.pdm.agg.1 <- d.pdm %>% group_by(target) %>% summarize(mean_exposure = mean(actual_exposure), mean_acc = mean(correct), subtlex.frequency = mean(subtlex.frequency))
d.prpc.agg.1 <- d.prpc.components %>% group_by(target) %>% summarize(mean_components = mean(components), mean_acc = mean(correct), subtlex.frequency = mean(subtlex.frequency), mean_timestamp = mean(timestamp))
d.prpc.agg.2 <- d.prpc.timestamp %>% group_by(target) %>% summarize(mean_components = mean(components), mean_acc = mean(correct), subtlex.frequency = mean(subtlex.frequency), mean_timestamp = mean(timestamp))



# Make histograms for the data distribution

ggplot(d.ld.agg.1, aes(x = mean_rt)) + 
         geom_histogram(binwidth = 0.05, fill = "#1a008e") + 
         xlab("Response Time (ms)") + ylab("Log10(Subtlex Frequency)") +
         theme(axis.title.x = element_text(size = 25),  
            axis.title.y = element_text(size = 25),  
            axis.text.x = element_text(size = 15),  
            axis.text.y = element_text(size = 15))  


ggplot(d.pdm.agg.1, aes(x = mean_exposure)) + 
  geom_histogram(binwidth = 0.05, fill = "#ff0000") + 
  xlab("Exposure Time (ms)") + ylab("Log10(Subtlex Frequency)")
  theme(axis.title.x = element_text(size = 25),  
            axis.title.y = element_text(size = 25),  
            axis.text.x = element_text(size = 15),  
            axis.text.y = element_text(size = 15))

ggplot(d.prpc.agg.1, aes(x = mean_components)) + 
  geom_histogram(binwidth = 2, fill = "#00a455") + 
  xlab("Components") + ylab("Log10(Subtlex Frequency)")
  theme(axis.title.x = element_text(size = 25),  
            axis.title.y = element_text(size = 25),  
            axis.text.x = element_text(size = 15),  
            axis.text.y = element_text(size = 15))

ggplot(d.prpc.agg.2, aes(x = mean_timestamp)) + 
  geom_histogram(binwidth = 250, fill = "#f2ff00") + 
  xlab("Trial time (ms)") + ylab("Log10(Subtlex Frequency)")
  theme(axis.title.x = element_text(size = 25),  
            axis.title.y = element_text(size = 25),  
            axis.text.x = element_text(size = 15),  
            axis.text.y = element_text(size = 15))


# make a plot of response time vs word frequency for lexical decision
ggplot(d.ld.agg.1, aes(x = log10(subtlex.frequency), y = mean_rt)) + 
  geom_point(color = "blue") +
  ggtitle("Response Time vs. Word Frequency") + 
  xlab("Word Log10(Subtlex Frequency)") + ylab("Response Time (ms)")


# make a plot of exposure time vs word frequency for progressive demasking
ggplot(d.pdm.agg.1, aes(x = log10(subtlex.frequency), y = mean_exposure)) + 
  geom_point(color = "blue") +
  ggtitle("Exposure Time vs. Word Frequency") + 
  xlab("Log10(Subtlex Frequency)") + ylab("Exposure Time (ms)")

# make a plot of number of components vs word frequency for PRPC
ggplot(d.prpc.agg.1, aes(x = log10(subtlex.frequency), y = mean_components)) + 
  geom_point(color = "blue") +
  ggtitle("Components vs. Word Frequency") + 
  xlab("Log10(Subtlex Frequency)") + ylab("Components (n)")

# make a plot of exposure time vs word frequency for progressive demasking
plot5 <- ggplot(d.pdm.agg.2, aes(x = log10(subtlex.frequency), y = mean_exposure)) + 
  geom_point(color = "blue") +
  ggtitle("Exposure Time vs. Word Frequency") + 
  xlab("Log10(Subtlex Frequency)") + ylab("Exposure Time (ms)")

# make a plot of timestamp vs word frequency for PRPC
ggplot(d.prpc.agg.2, aes(x = log10(subtlex.frequency), y = mean_timestamp)) + 
  geom_point(color = "blue") +
  ggtitle("Timestamp vs. Word Frequency") + 
  xlab("Log10(Subtlex Frequency)") + ylab("Timestamp (ms)")


# 5. Run linear regression models (to compute effect sizes)

ld.model1 <- lm(mean_rt~log10(subtlex.frequency), data=d.ld.agg.1)
pdm.model1 <- lm(mean_exposure~log10(subtlex.frequency), data=d.pdm.agg.1)
prpc.model1 <- lm(mean_components~log10(subtlex.frequency), data=d.prpc.agg.1)
prpc.model2 <- lm(mean_timestamp~log10(subtlex.frequency), data=d.prpc.agg.2)

#Plot with regression line and confidence interval

ggplot(d.ld.agg.1, aes(x=log10(subtlex.frequency), y=mean_rt)) + 
  geom_point() +
  geom_smooth(method=lm, se=TRUE, color="#ff0000", fill="lightblue") +
  theme(axis.title.x = element_text(size = 25),  
            axis.title.y = element_text(size = 25),  
            axis.text.x = element_text(size = 15),  
            axis.text.y = element_text(size = 15))


ggplot(d.pdm.agg.1, aes(x=log10(subtlex.frequency), y=mean_exposure)) + 
  geom_point() +
  geom_smooth(method=lm, se=TRUE, color="#ff0000", fill="lightblue") +
  theme(axis.title.x = element_text(size = 25),  
            axis.title.y = element_text(size = 25),  
            axis.text.x = element_text(size = 15),  
            axis.text.y = element_text(size = 15))

ggplot(d.prpc.agg.1, aes(x=log10(subtlex.frequency), y=mean_components)) + 
  geom_point() +
  geom_smooth(method=lm, se=TRUE, color="#ff0000", fill="lightblue") +
  theme(axis.title.x = element_text(size = 25),  
            axis.title.y = element_text(size = 25),  
            axis.text.x = element_text(size = 15),  
            axis.text.y = element_text(size = 15))

ggplot(d.prpc.agg.2, aes(x=log10(subtlex.frequency), y=mean_timestamp)) + 
  geom_point() +
  geom_smooth(method=lm, se=TRUE, color="#ff0000", fill="lightblue") +
  theme(axis.title.x = element_text(size = 25),  
            axis.title.y = element_text(size = 25),  
            axis.text.x = element_text(size = 15),  
            axis.text.y = element_text(size = 15))

