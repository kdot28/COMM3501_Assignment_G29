# Modelling

# Setup

# Function to install required packages
check_install_packages <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages)) {
    install.packages(new_packages)
  }
  sapply(packages, require, character.only = TRUE)
}

# List of packages needed
packages <- c("dplyr", "caret", "cluster", "tidyverse", "ggplot2", "tree",
              "arules", "arulesViz","klaR", "clustMixType", "reshape2")
check_install_packages(packages)

library(dplyr)
library(caret)
library(cluster)
library(tidyverse)
library(ggplot2)
library(tree)
library(arules)
library(arulesViz)
library(klaR)
library(reshape2)

# loading the dataset
modelling_data <- read.csv("cleaned_data_v3", T)

options(scipen = 999)

#### Unsupervised Learning - KARAN ####

# removing variables that are likely not required
modelling_data_1 <- modelling_data %>% dplyr::select(-X ,-date,-package, -commission_structure,
                                              -be, -severity, -life_cover_amount,
                                              -tpd_cover_amount, -trauma_cover_amount,-ip_cover_amount,
                                              -inside_super_premium, -be_cover_amount, -severity_cover_amount,
                                              -outside_super_premium, -adviser_id, -indexation_rate)
## Changing categorical data to dummy data

# changing into factors first
modelling_data_1 <- modelling_data_1 %>%
                    mutate(across(where(is.character), as.factor))

# scaling the numerical data due to existence of vars like income
modelling_data_1 <- modelling_data_1 %>%
  mutate(across(where(is.numeric), scale))

#### K-Prototypes clustering ####
set.seed(5)
# since we want to decide on the number of clusters which minimises dispersion
# within clusters, we will create a function that helps us do so.

elbow_chart <- function(k) {
  kproto_clust <- kproto(modelling_data_1[,-1], k)
  return(kproto_clust$tot.withinss)
}

# Computing the dispersion within clusters for k = 1 to 10
k_vals <- c(1:10)
with_clust_disp <- sapply(k_vals, elbow_chart) # this takes a while to run

# Elbow chart
elbow_chart_data <- data.frame(k = k_vals, within_clust_disp =
                                 with_clust_disp)

ggplot(elbow_chart_data, aes(x = k, y = within_clust_disp)) +
  geom_line() + geom_point() +
  labs(title = "Elbow Method for Optimal K",
       x = "Number of clusters (k)",
       y = "Total Within Cluster Dispersion")

# Lets use k = 4
set.seed(5)
kproto_model <- kproto(modelling_data_1[,-c(1)], k =4, nstart = 10) # also takes a while to run

# viewing results
kproto_model
summary(kproto_model)
print(kproto_model$centers)

# Attaching results
modelling_data_1$Cluster <- kproto_model$cluster

# Analysis

# Underwriters
ggplot(data = modelling_data_1 %>%
         filter(underwriter %in% (modelling_data_1 %>%
                                    count(underwriter) %>%
                                    top_n(11, n) %>%
                                    pull(underwriter))) %>%
         mutate(underwriter = fct_reorder(underwriter, desc(table(underwriter)[underwriter]))),
       aes(x = underwriter)) + 
  geom_bar(fill = "#69b3a2", colour = "black") + 
  coord_flip() + 
  labs(title = "Policies split by Top 11 Underwriters (Grouped)", x = "Underwriter") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

underwriter_cluster_summary <- modelling_data_1 %>%
  group_by(underwriter, Cluster) %>%
  summarize(Count = n(), .groups = 'drop') %>%
  group_by(underwriter) %>%
  mutate(Proportion = Count / sum(Count))

ggplot(underwriter_cluster_summary , aes(x = underwriter, y = Proportion, fill = as.factor(Cluster))) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Proportion of Clusters Across Underwriters",
       x = "Underwriter",
       y = "Proportion",
       fill = "Cluster") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Super
ggplot(modelling_data_1, aes(x = Cluster, fill = super)) +
  geom_bar(position = "fill") +
  labs(title = "Distribution of Super by Cluster",
       x = "Cluster",
       y = "Proportion",
       fill = "Super")

# Age distribution by cluster
ggplot(modelling_data_1, aes(x = as.factor(Cluster), y = age_next, fill = as.factor(Cluster))) +
  geom_violin() +
  labs(title = "Age Distribution by Cluster",
       x = "Cluster",
       y = "Age (Scaled)",
       fill = "Cluster")

# Income distribution by cluster
ggplot(modelling_data_1, aes(x = as.factor(Cluster), y = annual_income, fill = as.factor(Cluster))) +
  geom_violin() +
  labs(title = "Income Distribution by Cluster",
       x = "Cluster",
       y = "Annual Income (Scaled)",
       fill = "Cluster")

# Proportions of trauma insurance coverage by underwriter and cluster
trauma_proportions <- modelling_data_1 %>%
  group_by(underwriter, Cluster, trauma) %>%
  summarize(Count = n(), .groups = 'drop') %>%
  group_by(underwriter, Cluster) %>%
  mutate(Proportion = Count / sum(Count)) %>%
  filter(trauma == "Yes")

# Plot the proportions of trauma insurance coverage by underwriter and cluster
ggplot(trauma_proportions, aes(x = underwriter, y = Proportion, fill = as.factor(Cluster))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Proportion of Trauma Insurance Coverage by Underwriter and Cluster",
       x = "Underwriter",
       y = "Proportion",
       fill = "Cluster") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate proportions of premium frequency by underwriter and cluster
premium_freq_proportions <- modelling_data_1 %>%
  group_by(underwriter, Cluster, premium_frequency) %>%
  summarize(Count = n(), .groups = 'drop') %>%
  group_by(underwriter, Cluster) %>%
  mutate(Proportion = Count / sum(Count))

# Plot the distribution of premium frequency by underwriter and cluster
ggplot(premium_freq_proportions, aes(x = underwriter, y = Proportion, fill = premium_frequency)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~Cluster) +
  labs(title = "Distribution of Premium Frequency by Underwriter and Cluster",
       x = "Underwriter",
       y = "Proportion",
       fill = "Premium Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Extra charts
# Violin plot for Age distribution by Underwriter and Cluster
ggplot(modelling_data_1, aes(x = underwriter, y = age_next, fill = Cluster)) +
  geom_violin() +
  facet_wrap(~ Cluster) +
  labs(title = "Age Distribution by Underwriter and Cluster", x = "Underwriter", y = "Age") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Violin plot for Income distribution by Underwriter and Cluster
ggplot(modelling_data_1, aes(x = underwriter, y = annual_income, fill = Cluster)) +
  geom_violin() +
  facet_wrap(~ Cluster) +
  labs(title = "Income Distribution by Underwriter and Cluster", x = "Underwriter", y = "Annual Income") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Violin plot for Annualized Premium by Underwriter and Cluster
ggplot(modelling_data_1, aes(x = underwriter, y = annualised_premium, fill = Cluster)) +
  geom_violin() +
  facet_wrap(~ Cluster) +
  labs(title = "Annualized Premium Distribution by Underwriter and Cluster", x = "Underwriter", y = "Annualized Premium") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Scatter plot for Age and Annual Income by Underwriter and Cluster
ggplot(modelling_data_1, aes(x = age_next, y = annual_income, color = Cluster)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ underwriter) +
  labs(title = "Age vs Annual Income by Underwriter and Cluster", x = "Age", y = "Annual Income") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Box plot for Annualized Premium by Underwriter and Smoker Status
ggplot(modelling_data_1, aes(x = underwriter, y = annualised_premium, fill = smoker_status)) +
  geom_boxplot() +
  labs(title = "Annualized Premium by Underwriter and Smoker Status", x = "Underwriter", y = "Annualized Premium") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# market share analysis based on clusters
# Calculate the number of policies by underwriter and cluster
market_share_by_cluster <- modelling_data_1 %>%
  group_by(underwriter, Cluster) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Total = sum(Count), Share = Count / Total)

# Filter for NEOS Life and plot the market share
neos_market_share <- market_share_by_cluster %>%
  filter(underwriter == "NEOS Life")

# Plotting market share
ggplot(neos_market_share, aes(x = Cluster, y = Share, fill = Cluster)) +
  geom_bar(stat = "identity") +
  labs(title = "Market Share of NEOS Life by Cluster", x = "Cluster", y = "Market Share")

## Product penetration!
# Product penetration of each product within each cluster compared to the total
product_columns <- c("life", "tpd", "trauma", "ip")

# Because they are 'yes' 'no', converting to numeric with yes = 1
modelling_data_2 <- modelling_data_1 %>%
  mutate(across(all_of(product_columns), ~ as.numeric(.x == "Yes")))

product_penetration <- modelling_data_2 %>%
  group_by(underwriter, Cluster) %>%
  summarise(across(all_of(product_columns), mean, .names = "Pen_{.col}"), .groups = "drop")

# Plotting product penetration
ggplot(melt(product_penetration, id.vars = c("underwriter", "Cluster")), aes(x = Cluster, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~underwriter) +
  labs(title = "Product Penetration by Underwriter and Cluster", x = "Cluster", y = "Penetration Rate")


#### Associate Rules Analysis ####

## this is to figure out what products should be packaged together (emphasis in class)

product_data_ARA <- modelling_data_1[c("life", "ip", "tpd", "trauma")]

# Convert to transactions
product_transaction_ARA <- as(product_data_ARA, "transactions")

# Generate rules -> starting with high support and confidence so we initially
# detect stronger and more obvious relationships. minimum length = 2 is default.
rules <- apriori(product_transaction_ARA, parameter = list(supp = 0.19, conf = 0.8, minlen = 2))

# Inspect rules
srules <- sort(rules, by = 'lift', decreasing= T)
inspect(srules)

# focus from row 12 onwards?

# Generate a scatter plot for support vs confidence
plot(rules, measure=c("support", "confidence"), shading="lift")

# Generate a grouped matrix plot
plot(rules, method="grouped")

# Generate a graph-based visualization
plot(rules, method="graph")

# Generate a parallel coordinate plot
plot(rules, method="paracoord")

# Generate a heatmap (for smaller sets of rules due to readability)
if (length(rules) <= 20) {  # Adjust this based on your specific needs
  plot(rules, method="heatmap")
}
