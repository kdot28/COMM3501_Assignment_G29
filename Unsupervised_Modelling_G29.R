# Modelling

# Setup
scipen(999)

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
              "arules", "arulesViz","klaR", "clustMixType")
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

# loading the dataset
modelling_data <- read.csv("cleaned_data_v3", T)

#### Unsupervised Learning - KARAN ####

# removing variables that are likely not required
modelling_data_1 <- modelling_data %>% select(-X ,-date,-package, -commission_structure,
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
set.seed(5903)
# since we want to decide on the number of clusters which minimises dispersion
# within clusters, we will create a function that helps us do so.

elbow_chart <- function(k) {
  kproto_clust <- kproto(modelling_data_1[,-1], k)
  return(kproto_clust$tot.withinss)
}

# Computing the dispersion within clusters for k = 1 to 15
k_vals <- c(1:15)
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
ggplot(data = modelling_data_1 %>%
         filter(underwriter %in% (modelling_data_1 %>%
                                    count(underwriter) %>%
                                    top_n(20, n) %>%
                                    pull(underwriter))) %>%
         mutate(underwriter = fct_reorder(underwriter, desc(table(underwriter)[underwriter]))),
       aes(x = underwriter)) + 
  geom_bar(fill = "#69b3a2", colour = "black") + 
  coord_flip() + 
  labs(title = "Policies split by Top 20 Underwriters", x = "Underwriter") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

underwriter_freq <- modelling_data_1 %>% count(underwriter) %>%
  arrange(desc(n))

insurer_summary <- modelling_data_1 %>%
  group_by(underwriter, Cluster) %>%
  summarise(Count = n(), .groups = 'drop')


# Creating datasets based on the results for further exploration
# model_df <- factor(kproto_model$cluster, order = T,
#                    level = c(1:4))
# model <- data.frame(modelling_data_1[,-c(1)], model_df)
# 
# result_df <- kproto_model$centers
# Member <- kproto_model$size
# result <- data.frame(Member, result_df)

# plotting results
ggplot(model, aes(x = occupation, y = annual_income , color = model_df)) +
  geom_point() +
  labs(title = "Occupation ~ Annualised Income",
       x = "Occupation", y = "Annualised Income") + guides(color = guide_legend(title = "Cluster")) +
  theme(axis.text.x = element_text(angle = 90))
#### Associate Rules Analysis ####

