
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
# models. In addition, we will conver the Ethinicity one-hot encoding