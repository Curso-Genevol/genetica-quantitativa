if(!require(rsconnect)){install.packages("rsconnect"); library(rsconnect)}
rsconnect::setAccountInfo(name='curso-genevol', 
                          token='95346A5518D36EC95824A1AC86E965D1', 
                          secret='secret_key')

# Deploy
if(!require(here)){install.packages("here"); library(here)}
rsconnect::deployApp(here("Eq-de-Lande"))
rsconnect::deployApp(here("Superficie-Adaptativa"))

# Run local

runGitHub("genetica-quantitativa", "Curso-Genevol", subdir = "Eq-de-Lande")
runGitHub("genetica-quantitativa", "Curso-Genevol", subdir = "Superficie-Adaptativa")
