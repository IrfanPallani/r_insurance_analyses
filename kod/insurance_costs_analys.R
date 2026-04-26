# Inlämningsuppgift i R programmering: Försäkringskostnader R_analys
#Installation och nödvändiga patek.
#-------------------------------------------------------------
#install.packages("tidyverse", "broom")
#-------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(dplyr)
library(broom)

#=============================================================
# DATA FÖRSTÅELSE
#=============================================================

# Läser in data i R
df <- read_csv("data/insurance_costs.csv", show_col_types = FALSE)


dir.create("figurer", recursive = TRUE, showWarnings = FALSE)

dir.create("tabeller", recursive = TRUE, showWarnings = FALSE)


getwd()
# Visa struktur
str(df)
summary(df)
head(df)

# Antal rader och kolumner
cat("Rader", nrow(df), "\nKolumner:", ncol(df))


# Kontrollera saknade värden per kolumn

cat("\nSaknade värden per kolumn:\n")


colSums(is.na(df))

#=============================================================
# DATASTÄDNING
#=============================================================

# Städa kategoriska variabler och hantera saknade värden i ett steg.


df_clean <- df %>%
  mutate(
    sex               = tolower(trimws(sex)),
    region            = tolower(trimws(region)),
    smoker            = tolower(trimws(smoker)),
    chronic_condition = tolower(trimws(chronic_condition)),
    exercise_level    = tolower(trimws(exercise_level)),
    plan_type         = tolower(trimws(plan_type)),
  ) %>%
  
  # Behåll bara rader med fullständiga värden i nyckelkolumnerna
  drop_na(bmi, annual_checkups, charges)

cat("Rader kvar efter borttagning av NA:", nrow(df_clean), "\n")

# Kontrollera unika värden i kategoriska variabler
df_clean %>% 
  select(sex, region, smoker, chronic_condition, exercise_level, plan_type) %>% 
  summary()

#Skapa nya variabler---

# BMI-kategori

df_clean <- df_clean %>% 
  mutate(bmi_category = case_when(
    bmi < 18.5 ~ "Undervikt", 
    bmi < 25 ~ "Normalvikt", 
    bmi < 30 ~ "Övervikt", 
    TRUE ~ "Fetma"
  ))

# Åldersgrupp
df_clean <- df_clean %>% 
  mutate(age_group = case_when(
    age < 30 ~ "ung (18-29)",
    age < 50 ~ "medelålder (30-49)",
      TRUE ~ "gammal (50+)"
  ))
    
# Historikvariabel (tidigare olyckor + cliams)
df_clean <- df_clean %>%
  mutate(history_score = prior_accidents + prior_claims)

# ============================================================
# Analysera minst 4 figurer/tabeller
# ============================================================

#Figur1 Histogram - Fördelning av försäkringskostnader


fig1 <- ggplot(df_clean, aes(x = charges)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  labs(
    title = "Fördelning av försäkringskostnader", 
    x = "kostnad", 
    y = "Antal")

ggsave("figurer/1.Histogram.png", plot = fig1, width = 8, height = 5)

#-------------------------------------------------------------
#Tolkning: Kostnaderna är högerskeva-de flesta kunder har relativt
# låga kostnader, men en mindre grupp har mycket höga kostnader.
#-------------------------------------------------------------


# Figur 2: Boxplot - Kostnader per rökstatus


fig2 <- ggplot(df_clean, aes(x = smoker, y = charges, fill = smoker)) +
  geom_boxplot() +
  labs(
    title = " Försäkringskostnad för rökare vs icke -rökare", 
    x = "Rökare", 
    y = "Kostnad")
ggsave("figurer/2.Boxplot.png", plot = fig2)
#-------------------------------------------------------------
# Tolkning: Rökare har marknat högre mediankostnad och
#större spridning jämfört med icke-rökare.
#-------------------------------------------------------------


# Figur 3. Punktdiagori: Ålder vs kostnad (med rökning)

fig3 <- ggplot(df_clean, aes(x =age, y = charges, color = smoker)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se =FALSE) +
  labs(
    title = "Ålder vs kostnad, uppdelat på rökstatus",
    x = "Ålder", 
    y = "Kostnad")
ggsave("figurer/3.Punktdiagram.png", plot = fig3)
#-------------------------------------------------------------
# Tolkning: Kostnaderna ökar med ålder för bådea grupperna, men
# rökare har genomgående betydligt högre kostnader
#-------------------------------------------------------------


# Figur 4. BMI vs kostnad, med regressionslinje

fig4 <- ggplot(df_clean, aes(x = bmi, y = charges)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se =FALSE, color = "red") +
  labs(
    title = "BMI vs försäkringskostnad", 
    x = "BMI", 
    y = "kostnad")
ggsave("figurer/4.Regressionslinje.png", plot = fig4)

#-------------------------------------------------------------
#Tolkning: Det finns ett svagt positivt samband mellan BMI och kostnad.
#-------------------------------------------------------------


# Tabell med medelkostnad per region och rökstatus

tabell1 <- df_clean %>%
  group_by(region, smoker) %>%
  summarise(
    medel_kostnad = mean(charges), 
    antal = n(), 
    .groups = "drop"
  ) %>%
  arrange(desc(medel_kostnad))
write_csv(tabell1, "tabeller/medel_kostnad_region_smoker.csv")
tabell1
# ============================================================
# Regressionsanalys
# ============================================================

vars <- c("charges", "age", "bmi", "smoker", "prior_claims")

df_model <- df_clean %>%
  drop_na(all_of(vars))

# Modell1 - fullständig modell med all relevanta prediktorer

model1 <- lm(charges ~ age + bmi + smoker + prior_claims, data = df_model)
summary(model1)


# Modell2 - förenklad modell med de starkaste prediktorerna

model2 <- lm(charges ~ age + bmi, data = df_model)

summary(model2)




# Konvertera kategoriska variabler till faktorer

# jämförelse av modeller:
#=============================================================

AIC(model1, model2)
BIC(model1, model2)

# F-test: testar om model1 bättre än model2.

anova(model2, model1)
