# Inlämningsuppgift i R programmering: Försäkringskostnader R_analys
#Installation och nödvändiga patek.

install.packages("tidyverse")

library(tidyverse)
library(corrplot)
library(broom)

# DATA FÖRSTÅELSE

# Läser in data i R
#data <- read_csv("data/raw/insurance_costs.csv", na = c("", "NA", "null"))
df <- read_csv("data/df/insurance_costs.csv", stringsAsFactors= FALSE)


str(df)
summary(df)


# Sknade värden per kolumn
print(colSums(is.na(df)))

# DATASTÄDNING

data_clean <- df %>%
  mutate(
    region =str_trim(tolower(region)),
    smoker = str_trim(tolower(smoker)),
    exercise_level = str_trim(tolower(exercise_level)),
    plan_type = str_trim(tolower(plan_type)),
    chronic_condition = str_trim(tolower(chronic_condition)),
    
    # Importing numersika variabler
    bmi = ifelse(is.na(bmi), median(bmi, na.rm = TRUE), bmi),
    
    annual_checkups = ifelse(is.na(annual_checkups), 
                             median(annual_checkups, na.rm = TRUE),
                             annual_checkups),
                       
     # Kategorisk importing                  
    exercise_level= ifelse(is.na(exercise_level), "medium", exercise_level),
    
    # BMI kategorier

    bmi_category = case_when(
      bmi < 18.5 ~ "Undervikt",
      bmi < 25 ~ "Normalvikt",
      bmi < 30 ~ "Övervikt",
      TRUE ~ "Fetma"
    ),
        
     # Åldersgrupp   
    age_group = case_when(
      age < 30 ~ "ung",
      age < 50 ~ "medelålder",
      TRUE ~ "gammal"
    ),
    
    # Sammanlagd skadehistorik
    
    claim_history = coalesce(prior_accidents, 0) + coalesce(prior_claim, 0),
    
    #Log-transform (för snedfördelning)
    
    log_charges =log(charges)
  )
        
# Kontroll efter städning

sum(is.na(data_clean))



# MINST 4 VISUALISERINGAR.

#Figur1 histogram: Fördelning av försäkringskostnader

ggplot(data_clean, aes(x = charges)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  labs(
    title = "Fördelning av försäkringskostnader",
       x = "kostnad (USD)", 
       y = "Antal kunder"
  ) + 
  theme_minimal()

# Figur 2: Boxplot diagram: Kostnader per rökstatus

ggplot(data_clean, aes(x = smoker, y = charges, fill = smoker)) +
  geom_boxplot() +
  labs(title = " Försäkringskostnad för rökare vs icke -rökare",
       x = "Rökare", y = "Kostnad (USD)"
  ) +
  theme_minimal()
  theme(legend.position = "none")

# Figur 3. Punktdiagori: Ålder vs kostnad (med rökning)

ggplot(data_clean, aes(x =age, y = charges, color = smoker)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se =FALSE) +
  labs(title = "Samband mellan ålder och kostnad, uppdelat på rökning",
       x = "Ålder", 
       y = "Kostnad (USD)"
  ) +
  theme_minimal()

# Figur 4. Genomsnittlig kostnad per BMI-kategori och rökstatus

data_clean %>%
  group_by(bmi_category, smoker) %>%
  summarise(
    mean_charge = mean(charges, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  
  ggplot(aes(x = bmi_category, y = mean_charge, fill = smoker)) +
  geom_col(position = "dodge") +
  labs(
    title = "Genomsnittlig kostnad per BMI-kategori och rökstatus", 
    x = "BMI-kategori", 
    y = "Genomsnittlig kostnad (USD)"
  ) +
  theme_minimal()
  
  