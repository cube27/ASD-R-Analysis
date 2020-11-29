# Required libraries
library(vtreat)
library(tidyr)
library(dplyr)
library(ggplot2)

# Load ASD Data, converting most numbers to numeric
library(readr)
Toddler_Autism_July_2018 <-
  read_csv(
    "Data/Toddler Autism dataset July 2018.csv",
    col_types = cols(
      Case_No = col_number(),
      A1 = col_number(),
      A2 = col_number(),
      A3 = col_number(),
      A4 = col_number(),
      A5 = col_number(),
      A6 = col_number(),
      A7 = col_number(),
      A8 = col_number(),
      A9 = col_number(),
      A10 = col_number(),
      Age_Mons = col_number(),
      `Qchat-10-Score` = col_factor(levels = c(
        "0",
        "1", "2", "3", "4", "5", "6",
        "7", "8", "9", "10"
      )),
      Sex = col_factor(levels = c("m",
                                  "f")),
      Ethnicity = col_character(),
      Jaundice = col_factor(levels = c("yes",
                                       "no")),
      Family_mem_with_ASD = col_factor(levels = c("yes",
                                                  "no")),
      `Class/ASD Traits` = col_factor(levels = c("Yes",
                                                 "No"))
    )
  )

# Look at the data structure
str(Toddler_Autism_July_2018)
names(Toddler_Autism_July_2018)
head(Toddler_Autism_July_2018, 5)

# We dont need the last two columns for now because they are more informational.
# Who completed the test and Class/ASD Traits don't add value to training the
# models. Also, we will convert the factor type columns to 1 and 0 using one-hot
# encoding..
varlist = c('Sex', 'Ethnicity', 'Jaundice', 'Family_mem_with_ASD')
treatplan <- designTreatmentsZ(Toddler_Autism_July_2018, varlist)
scoreFrame <- treatplan %>% 
  magrittr::use_series(scoreFrame) %>% 
  select(varName, origName, code)
newvarlist <- scoreFrame %>% 
  filter(code %in% c("clean", "lev")) %>%
  magrittr::use_series(varName)
one_hot_encoded <- prepare(treatplan, Toddler_Autism_July_2018, varRestriction = newvarlist)

ASD_Data <- cbind(Toddler_Autism_July_2018[,1:13], one_hot_encoded)

# Look at histogram of score
ASD_Data %>% ggplot(aes(as.numeric(Toddler_Autism_July_2018$`Qchat-10-Score`))) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.3, col = "black") +
  xlab("ASD Score") +
  ggtitle("ASD Score Histogram") +
  ggthemes::theme_clean()

ASD_prob <- mean(as.numeric(Toddler_Autism_July_2018$`Qchat-10-Score`) >=0 & as.numeric(Toddler_Autism_July_2018$`Qchat-10-Score`) <=3)
nonASD_prob <- 1 - ASD_prob

any(is.na(ASD_Data))

# K-Means Clustering
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

df <- ASD_Data
df <- na.omit(df)
df <- sapply( df, as.numeric )
df <- scale(df)
head(df)

distance <- get_dist(df)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k_means <- kmeans(df, centers = 3, nstart = 20)
fviz_cluster(k_means, data = df, ellipse.alpha = 0.2, ggtheme = theme_minimal(), geom = "point")


table(k_means$cluster, Toddler_Autism_July_2018$`Qchat-10-Score`)

Toddler_Autism_July_2018 %>%
  as_tibble() %>%
  mutate(cluster = k_means$cluster) %>%
  ggplot(aes(Family_mem_with_ASD, `Qchat-10-Score`, color = factor(cluster))) +
  geom_point()

table(k_means$cluster, Toddler_Autism_July_2018$Ethnicity)

colSums(table(k_means$cluster, Toddler_Autism_July_2018$Ethnicity))

table(k_means$cluster, Toddler_Autism_July_2018$`Class/ASD Traits`)

table(k_means$cluster, Toddler_Autism_July_2018$Sex)

table(k_means$cluster, Toddler_Autism_July_2018$Age_Mons)
colSums(table(k_means$cluster, Toddler_Autism_July_2018$Age_Mons))
plot(12:36, colSums(table(k_means$cluster, Toddler_Autism_July_2018$Age_Mons)))
# multi-class target
# binomial target
