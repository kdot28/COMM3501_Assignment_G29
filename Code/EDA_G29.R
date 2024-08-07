               #### Exploratory Data Analysis - Team 29 ####

options(scipen = 999)
# This code will be to conduct EDA as part of the major group assignment for COMM3501 #
               
# Installing and loading packages
package_list <- c("ggplot2", "dplyr", "tidyr", "janitor", "skimr",  "corrplot",
                  "lubridate", "forcats", "VIM", "Outliers", "hexbin")

install.packages(package_list)

library(ggplot2)
library(dplyr)
library(tidyr)
library(janitor)
library(skimr)
library(corrplot)
library(lubridate)
library(forcats)
library(VIM)
library(outliers)
library(hexbin)
# Uploading the dataset
original_data <- read.csv("A3_Dataset_2023.csv")

# General data cleaning - gets rid of the external ref column (the whole column was empty)
original_data <- original_data %>%
  clean_names() %>%
  remove_empty(c("rows", "cols"))

# Converting all forms of missing data to NA - so that we are able to capture everything
# It is recognised that the alternative column will have many missing entries since
# advisers may not provide an alternative to every single client. Therefore, NA
# will not be assigned to any missing values for the alternative variable.

# This extracts all the column names except for alternative.
NA_cols <- setdiff(names(original_data), "alternative")

for (col in NA_cols) {
  original_data[[col]][original_data[[col]] == ""] <- NA
  original_data[[col]][original_data[[col]] == "unknown"] <- NA
  original_data[[col]][original_data[[col]] == "N/A"] <- NA
}

# Checking - answer should be zero
sum(is.na(original_data$alternative))

# Summarising the dataset to attain better understanding of the data
skim(original_data)

# Checking for missing values
missing_data <- colSums(is.na(original_data))
print(missing_data)

# Visualising the missing data to understand its impacts and make a decision
# on imputation or removal.

# Decide on one graph to use
aggr(original_data, col = c('#404080', '#69b3a2'), numbers = TRUE, sortVars = TRUE, sortCombs = TRUE, Plot = TRUE,
     labels = names(original_data), cex.axis = 0.4, gap = 3, ylab = c("Missing data Proportion", "Pattern"))


ggplot(data = gather(as.data.frame(is.na(original_data))), aes(x = key, fill = value)) + 
  geom_bar() + 
  labs(title = "Missing Data by Variables", x = "Variables", y = "Missing Count", fill = "Missing") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# dataset with the missing values
 na_subset <- original_data[apply(
   original_data, 
   1, 
   function(x) any(is.na(x))
 ), ]
 
# Quick plots to see if the distribution of the missing values tells us anything
# e.g. if a certain underwriter has majority of the missing data
 
ggplot(data = na_subset, aes(underwriter)) + geom_bar() 
# NEOS Life has the most missing therefore further analysis. (apart from the NAs)

na_NEOS <- na_subset %>% filter(underwriter == "NEOS Life")
na_NEOS[na_NEOS == 0] <- NA
skim(na_NEOS)

# Roughly 125, zero premiums, best to remove in my opinion as zero premiums don't make sense.

ggplot(data = na_subset, aes(age_next)) + geom_bar()
ggplot(data = na_subset, aes(gender)) + geom_bar()
ggplot(data = na_subset, aes(smoker_status)) + geom_bar()
ggplot(data = na_subset, aes(home_state)) + geom_bar()
 
# Removing rows with missing data

# Removing all missing data since they look like obvious data errors, with missing
# life insurance providers, zero premiums, missing adviser IDs, missing age, 
# zero income with no information on employment.
# Additionally, it constitutes approx 1.25% of the data, thus making no significant
# impact to data quality and validity. - Link this back to Yadav, Roychaudhary (Handling Missing Values)
# data was checked if imputation would be reasonable - not at this moment, since we believe
# it is not worth the computational effort for just 1.25% of the data.



# Final cleaned data - 
# have not removed any unnecessary variables yet (rows removed 166775-164822 = 2080 (1.25%))
cleaned_data <- na.omit(original_data)

# removing variables which are expected to be not used
cleaned_data <- cleaned_data %>% dplyr::select(-time, -alternative, -recommendation_id,
                                        -request_id, -life_id, -premium)

skim(cleaned_data)

# annual income = -1 -> set annual income to be greater than or equal to 0.
# removes 164822 - 164622 = 200 rows of data. 
cleaned_data <- cleaned_data %>% filter(annual_income >=0)

# Outliers analysis

# Box-Whisker Plots and Histograms for Outliers

# Boxplot for 'annualised_premium' - can clearly see outliers exist
ggplot(data = cleaned_data, aes(y = annualised_premium)) + 
  geom_boxplot(fill = "#69b3a2") + 
  labs(title = "Boxplot of Premium", y = "Premium")

ggplot(cleaned_data, aes(x=annualised_premium)) +
  geom_histogram(aes(y=..density..), binwidth=100, fill="#404080", color="#69b3a2", alpha=0.8) +
  geom_density(alpha=.2, fill="#F371FF") +
  labs(title="Distribution of Annualised Premiums", x="Annualised Premiums", y="Density") +
  theme_minimal()

# Boxplot for 'annual_income' - can clearly see outliers exist
ggplot(data = cleaned_data, aes(y = annual_income)) + 
  geom_boxplot(fill = "#69b3a2") + 
  labs(title = "Boxplot of Annual Income", y = "Annual Income")

ggplot(cleaned_data, aes(x=annual_income)) +
  geom_histogram(aes(y=..density..), binwidth=100000, fill="#404080", color="#69b3a2", alpha=0.8) +
  geom_density(alpha=.2, fill="#F371FF") +
  labs(title="Distribution of Annual Income", x="Annual Income", y="Density") +
  theme_minimal()

# Boxplot for 'age_next' - can clearly see outliers exist
ggplot(data = cleaned_data, aes(y = age_next)) + 
  geom_boxplot(fill = "#69b3a2") + 
  labs(title = "Boxplot of Age Next", y = "Age Next")

ggplot(cleaned_data, aes(x=age_next)) +
  geom_histogram(aes(y=..density..), binwidth=1, fill="#404080", color="#69b3a2", alpha=0.8) +
  geom_density(alpha=.2, fill="#F371FF") +
  labs(title="Distribution of AgeNext", x="AgeNext", y="Density") +
  theme_minimal()

# Removing extreme outliers using z-scores where data outside 3 standard deviations will be excluded

cleaned_data_v2 <- cleaned_data %>%
  mutate(z_annualised_premium = (annualised_premium - mean(annualised_premium, na.rm = TRUE)) / sd(annualised_premium, na.rm = TRUE),
         z_annual_income = (annual_income - mean(annual_income, na.rm = TRUE)) / sd(annual_income, na.rm = TRUE),
         z_age_next = (age_next - mean(age_next, na.rm = TRUE)) / sd(age_next, na.rm = TRUE))

# Define z-score threshold - standard is three however, we don't want to a lot of
# right tail data for annualised premiums and annualised income (only the most extreme values
# are excluded - a very conservative approach)
z_threshold <- 4 

# Filter out rows with z-scores beyond the threshold
# removes 164622 - 162836 = 1786 rows of data.
cleaned_data_v3 <- cleaned_data_v2 %>%
  filter(abs(z_annualised_premium) <= z_threshold, abs(z_annual_income) <= z_threshold,
         abs(z_age_next) <= z_threshold)

# new age range = 14 to 76
# new annualised premium range = 0 to 23,493.56
# new annual income range = 0 to 1,350,586

# Drop the z-score columns
cleaned_data_v3 <- cleaned_data_v3 %>%
  dplyr::select(-z_annualised_premium, -z_annual_income, -z_age_next)

# plotting histograms and density plots again
# Boxplot for 'annualised_premium' 
ggplot(data = cleaned_data_v3, aes(y = annualised_premium)) + 
  geom_boxplot(fill = "#69b3a2") + 
  labs(title = "Boxplot of Annualised Premiums - excl Outliers", y = "Premium")

ggplot(cleaned_data_v3, aes(x=annualised_premium)) +
  geom_histogram(aes(y=..density..), binwidth=100, fill="#404080", color="#69b3a2", alpha=0.8) +
  geom_density(alpha=.2, fill="#F371FF") +
  labs(title="Distribution of Annualised Premiums - excl Outliers", x="Ansld Premiums", y="Density") +
  theme_minimal()

# Boxplot for 'annual_income'
ggplot(data = cleaned_data_v3, aes(y = annual_income)) + 
  geom_boxplot(fill = "#69b3a2") + 
  labs(title = "Boxplot of Annual Income - excl Outliers", y = "Annual Income")

ggplot(cleaned_data_v3, aes(x=annual_income)) +
  geom_histogram(aes(y=..density..), binwidth=10000, fill="#404080", color="#69b3a2", alpha=0.8) +
  geom_density(alpha=.2, fill="#F371FF") +
  labs(title="Distribution of Annual Income - excl Outliers", x="Annual Income", y="Density") +
  theme_minimal()

# Boxplot for 'age_next' 
ggplot(data = cleaned_data_v3, aes(y = age_next)) + 
  geom_boxplot(fill = "#69b3a2") + 
  labs(title = "Boxplot of Age Next - excl Outliers", y = "Age Next")

ggplot(cleaned_data_v3, aes(x=age_next)) +
  geom_histogram(aes(y=..density..), binwidth=1, fill="#404080", color="#69b3a2", alpha=0.8) +
  geom_density(alpha=.2, fill="#F371FF") +
  labs(title="Distribution of AgeNext - excl Outliers", x="AgeNext", y="Density") +
  theme_minimal()


# number of levels for occupation
nlevels(factor(cleaned_data$Occupation))
#keep the top 20 most frequent occupations and turn other ones to other
top_n_levels <- 20
occupation_freq <- table(cleaned_data_v3$occupation)
cleaned_data_v3$occupation <- fct_lump(cleaned_data_v3$occupation, n = top_n_levels, other_level = "Other")
#double check the number of levels after cleaning
nlevels(factor(cleaned_data_v3$occupation))

# keep the top 10 most frequent underwriters and turn other ones to other
top_U_levels <- 10
underwriter_freq <- table(cleaned_data_v3$underwriter)
cleaned_data_v3$underwriter <- fct_lump(cleaned_data_v3$underwriter, n = top_U_levels, other_level = "Other")
#double check the number of levels after cleaning
nlevels(factor(cleaned_data_v3$underwriter))

# converting variables into their relevant types before analysis.
cleaned_data_v3$date <- dmy(cleaned_data_v3$date)
cleaned_data_v3$home_state <- as.factor(cleaned_data_v3$home_state)
cleaned_data_v3$occupation <- as.factor(cleaned_data_v3$occupation)


# extra-visualisations

# Underwriters
ggplot(data = cleaned_data_v3 %>%
         filter(underwriter %in% (cleaned_data_v3 %>%
                                    count(underwriter) %>%
                                    top_n(11, n) %>%
                                    pull(underwriter))) %>%
         mutate(underwriter = fct_reorder(underwriter, desc(table(underwriter)[underwriter]))),
       aes(x = underwriter)) + 
  geom_bar(fill = "#69b3a2", colour = "black") + 
  coord_flip() + 
  labs(title = "Policies split by Top 11 Underwriters (Grouped)", x = "Underwriter") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Split by gender
ggplot(data = cleaned_data_v3, aes(x = gender)) + 
  geom_bar(fill = "#69b3a2", color = "black") + labs(title = "Policies split by Gender")
# Split by smoking status
ggplot(data = cleaned_data_v3, aes(x = smoker_status)) + 
  geom_bar(fill = "#69b3a2", color = "black") + labs(title = "Policies split by Smoking Status")
# Split by home state
ggplot(data = cleaned_data_v3, aes(x = home_state)) + 
  geom_bar(fill = "#69b3a2", color = "black") + labs(title = "Policies split by Home State")
# Split by super yes/no
ggplot(data = cleaned_data_v3, aes(x = super)) + 
  geom_bar(fill = "#69b3a2", color = "black") + labs(title = "Policies split by super")


# Visualizing relationships between key variables - likely a better way to do this tbh
ggplot(data = cleaned_data_v3, aes(x = annualised_premium, fill = gender)) +
  geom_density(alpha = 0.6) +
  labs(title = "Density of Annualised Premium by Gender")

ggplot(data = cleaned_data_v3, aes(x = annualised_premium, fill = smoker_status)) +
  geom_density(alpha = 0.6) +
  labs(title = "Density of Annualised Premium by Smoker Status")

ggplot(data = cleaned_data_v3, aes(x = annual_income, y = annualised_premium)) +
  geom_hex(bins = 30) +
  scale_fill_continuous(type = "viridis") +
  facet_wrap(~ gender) +
  labs(title = "Annual Income vs Annualised Premium by Gender")

ggplot(data = cleaned_data_v3, aes(x = annual_income, y = annualised_premium)) +
  geom_hex(bins = 30) +
  scale_fill_continuous(type = "viridis") +
  facet_wrap(~ smoker_status) +
  labs(title = "Annual Income vs Annualised Premium by Smoker Status")
# --------------- EDA Finished (for speech) -------------- #

# P/H's with zero income
zero_income <- cleaned_data %>% filter(annual_income == 0)
zero_income_freq <- data.frame(tabyl(zero_income$occupation))


# Saving the dataset into the repository
write.csv(cleaned_data_v3, "cleaned_data_v3")


# --------------- EDA for final presentation -------------- #
# Violin plot for age distribution by underwriter
ggplot(cleaned_data_v3, aes(x = underwriter, y = age_next, fill = underwriter)) +
  geom_violin() +
  labs(title = "Age Distribution by Underwriter", x = "Underwriter", y = "Age") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Violin plot for Income distribution by Underwriter and Cluster
ggplot(cleaned_data_v3, aes(x = underwriter, y = annual_income, fill = underwriter)) +
  geom_violin() +
  labs(title = "Income Distribution by Underwriter", x = "Underwriter", y = "Annual Income") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
