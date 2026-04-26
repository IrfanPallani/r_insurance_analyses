# Analys av försäkringskostnader.
Den analyserar vilka faktorer som påverkar försäkringskostnader.


## Jag gjorde dataförståelse, data städning, data visualisering, 
tabell analys och regressionsanalys.

### Fyra figurer, en tabel och två regressionsmodller.

## Mapp struktur
- data/ - rådata
- figurer/ - sparade grafer (PNG)
- tabeller/ - exporterade tabeller (CSV)

## 
- insurance_costs_analys. - R-skript med all kod (städn ing, diagram, regressionsmodelller)
- figurer/ - mapp där all diagram sparas som PNG.
- data/insurance_cost.csv - datasetet.


## Kör instruktioner

** Öppnade Rstudio** och skapade ett nytt projekt.
***placerade filerna** så här:
- insurance_costs_analys.R i projektmappen
-insurance_cost.csv i en undermap som heter 'data'
-installerat paket install.package ("tidyverse"", "broom")


## Vad skriptet gör:
- Läser in och städer data.
- Skapar mappen figurer och sparar fyra diagram ( histogram, boxplot, 
punktdiagram och regressionslinje)
Bygger två regressionsmodeller och jämförde dem (AIC, BIC, F-test)


## Slutsats: 
- Analysen visar att rökning och ålder har stark påverkan på förskärkingskostnader.
- BMI har ett svagare men positivt samband med kostnader.
- Rökare har generell betydligt högre kostnader än icke- rökare



