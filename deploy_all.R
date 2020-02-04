if(!require(rsconnect)){install.packages("rsconnect"); library(rsconnect)}
rsconnect::setAccountInfo(name='curso-genevol', 
                          token='95346A5518D36EC95824A1AC86E965D1', 
                          secret='secret_key')

# Deploy

rsconnect::deployApp("/home/diogro/projects/CursoGenevol-quantGen/Eq-de-Lande")
rsconnect::deployApp("/home/diogro/projects/CursoGenevol-quantGen/Superficie-Adaptativa")

# Run local

runGitHub("genetica-quantitativa", "Curso-Genevol", subdir = "Eq-de-Lande")
runGitHub("genetica-quantitativa", "Curso-Genevol", subdir = "Superficie-Adaptativa")
