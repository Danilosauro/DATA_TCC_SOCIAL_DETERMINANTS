

# Curso Spring Iyaleta
# Introdução ao Software R: Análise de dados secundários em estudos epidemiológicos


## AULA 12/12/23 ##



# Carregar pacote pacman (para instalar e carregar pacotes)
library(pacman)

pacman::p_load("rio", 
               "tidyverse",
               "lubridate")


# Diretório de trabalho ####

setwd("D:\\Curso_R_Spring_Iyaleta") 


##importar as bases de dados

sragBA_22 <- import("sivepBA22.xlsx")
sragBA_23 <- import("sivepBA23.xlsx")


## Selecionar as mesmas colunas para as duas bases de dados

sragBA_22 <- sragBA_22 %>%
  dplyr::select(DT_NASC, DT_SIN_PRI, DT_NOTIFIC, CS_SEXO,TOSSE)

sragBA_23 <- sragBA_23 %>%
  dplyr::select(DT_NASC, DT_SIN_PRI, DT_NOTIFIC, CS_SEXO,TOSSE)


# Juntar as bases

sragBA<- rbind (sragBA_22,sragBA_23)





