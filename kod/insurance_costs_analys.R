# Inlämningsuppgift i R programmering: Försäkringskostnader R_analys
#Installation och nödvändiga patek.

install.packages("tidyverse")

library(tidyverse)
library(ggplot2)
library(broom)

# DATA FÖRSTÅELSE

# Läser in data i R
df <- read_csv("data/insurance_costs.csv")

# Visa struktur
str(df)
summary(df)
head(df)

# Antal rader och kolumner
cat("Rader", nrow(df), "\nKolumner:", ncol(df))

# Datastädning och förberedelse

# Hantera saknade värden
df_clean <- df %>%
drop_na(bmi, annual_checkups, charges)  # behåll bara fullständiga rader
cat("Rader kvar efter borttagning av NA:", nrow(df_clean))


# DATASTÄDNING

df_clean <- df %>%
  mutate(
    sex = tolower(trimws(sex)),
    region = tolower(trimws(region)),
    smoker = tolower(trimws(smoker)),
    chronic_condition = tolower(trimws(chronic_condition)),
    exercise_level = tolower(trimws(exercise_level)),
    plan_type = tolower(trimws(plan_type)),
  )

# Kontrollera unika värden
df_clean %>% select(sex, region, smoker, chronic_condition, exercise_level, plan_type) %% summary()

df_clean <- df_clean %>% 
  mutate(
    bmi < 18.5 ~ "Undervikt", 
    bmi < 25 ~ "Normalvikt", 
    bmi < 30 ~ "Övervikt", 
    TRUE ~ "Fetma"
  ))

# Åldersgrupp
df_clean <- df_clean %>% 
  mutage(age_group = case_when(
      age < 30 ~ "ung (18-29)",
      age < 50 ~ "medelålder (30-49)",
      TRUE ~ "gammal (50+)"
    ))
    
# Historikvariabel (tidigare olyckor + cliams)
df_clean <- df_clean %>%
  mutate(history_score = prior_accidents + prior_claims)

# Analysera minst 4 figurer/tabeller

#Figur1 histogram: Fördelning av försäkringskostnader

ggplot(df_clean, aes(x = charges)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  labs(
    title = "Fördelning av försäkringskostnader",
       x = "kostnad (USD)", 
       y = "Antal")
  theme_minimal()

# Figur 2: Boxplot diagram: Kostnader per rökstatus

ggplot(df_clean, aes(x = smoker, y = charges, fill = smoker)) +
  geom_boxplot() +
  labs(title = " Försäkringskostnad för rökare vs icke -rökare",
       x = "Rökare", y = "Kostnad (USD)")
  theme_minimal()
  theme(legend.position = "none")

# Figur 3. Punktdiagori: Ålder vs kostnad (med rökning)

ggplot(data_clean, aes(x =age, y = charges, color = smoker)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se =FALSE) +
  labs(title = "Samband mellan ålder och kostnad, uppdelat på rökning",
       x = "Ålder", y = "Kostnad (USD)")


# Figur 4. Genomsnittlig kostnad per BMI-kategori och rökstatus
  
ggplot(df_clean, aes(x = age, y = charges, color = smoker)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se =FALSE, color = "red") +
  labs(title = "BMI vs försäkringskostnad", x = "BMI", y = "kostnad (USD)")

  