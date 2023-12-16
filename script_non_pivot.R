install.packages('dplyr')
install.packages('readxl') 
install.packages('ggplot2') 
install.packages('tidyr') 
install.packages('forcats') 
install.packages('rio') 
install.packages('stringr') 
install.packages('purrr') 
install.packages('lmtest', dependencies = TRUE)
install.packages('plm', dependencies = TRUE)
install.packages('fastDummies') 


library(dbplyr) 
library(readxl) 
library(ggplot2) 
library(tidyr) 
library(forcats) 
library(rio) 
library(stringr) 
library(purrr)
library(plm) 
library(fastDummies) 

install_formats()


setwd('/home/danilo.dias/R_files/DATA_TCC') 
setwd(getwd())

# tratamento dos dados de determinantes sociais ~ atlas do desenvolvimento humano ipea 
rm(aids_data) 
rm(determinantes) 
rm(na_removed_determinantes)

determinantes <- read_excel('IPEA_DATA.xlsx') 

na_removed_determinantes <- determinantes %>% 
  drop_na() 

determinantes <- na_removed_determinantes 

determinantes <- determinantes %>% 
  dplyr::rename('UF' = 'Territorialidades') 

determinantes <- determinantes %>% 
  subset(UF == 'Bahia'| UF == 'Pernambuco' | UF == 'Alagoas' | UF == 'Paraíba' | UF == 'Ceará' | UF == 'Sergipe' | UF == 'Rio Grande do Norte' | UF == 'Maranhão' | UF == 'Piauí')

determinantes <- determinantes %>% 
  tidyr::pivot_longer(
    cols = !UF,
    names_to = "DETERMINANTES",
    values_to = "SCORE" ) 

determinantes <- determinantes %>% 
  dplyr::mutate(ano='')

determinantes$ano <- substr(determinantes$DETERMINANTES, nchar(determinantes$DETERMINANTES)-4,nchar(determinantes$DETERMINANTES)) 
determinantes$ano <- as.numeric(determinantes$ano)


# tratamento dos dados de  notificacoes de aids desde 1980 ~ sinan 

aids_data <- read_excel('DATA_SINAN.xlsx') 

selected_aids_data <- aids_data %>% 
  dplyr::select('UF Notificação','2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020', '2021') 

aids_data <- selected_aids_data 
rm(selected_aids_data)

aids_data <- aids_data %>% 
  dplyr::rename('UF'= 'UF Notificação') %>% 
  dplyr::rename('casos aids 2010' = '2010', 'casos aids 2011' = '2011', 'casos aids 2012' = '2012', 'casos aids 2013' = '2013', 
                'casos aids 2014' = '2014', 'casos aids 2015' = '2015', 'casos aids 2016' = '2016', 'casos aids 2017' = '2017',
                'casos aids 2018' ='2018', 'casos aids 2019' = '2019', 'casos aids 2020' = '2020', 'casos aids 2021' ='2021')

aids_data  <- aids_data %>% 
  subset(UF == 'Bahia'| UF == 'Pernambuco' | UF == 'Alagoas' | UF == 'Paraíba' | UF == 'Ceará' | UF == 'Sergipe' | UF == 'Rio Grande do Norte' | UF == 'Maranhão' | UF == 'Piauí') 

aids_data <- aids_data %>% 
  tidyr::pivot_longer(
    cols = !UF,
    names_to = "CASOS_AIDS",
    values_to = "QUANTIDADE_CASOS_AIDS" ) 

aids_data <- aids_data %>% 
  dplyr::mutate(ano='') 

aids_data$ano <-substr(aids_data$CASOS_AIDS, nchar(aids_data$CASOS_AIDS)-4,nchar(aids_data$CASOS_AIDS)) 
aids_data$ano <- as.numeric(aids_data$ano)


# tratamento dos dados de notificacoes de sifilis adquirida desde 2010 ~ sinan 
sifilis_data <- import('SIFILIS_ADQUIRIDA.csv') 

sifilis_data <- sifilis_data %>% 
  dplyr::rename('UF' = 'UF de notificao') %>% 
  dplyr::rename( 'casos sifilis 2010' = '2010', 'casos sifilis 2011' = '2011', 'casos sifilis 2012' = '2012', 'casos sifilis 2013' = '2013', 
                 'casos sifilis 2014' = '2014', 'casos sifilis 2015' = '2015', 'casos sifilis 2016' = '2016', 'casos sifilis 2017' = '2017',
                 'casos sifilis 2018' ='2018', 'casos sifilis 2019' = '2019', 'casos sifilis 2020' = '2020', 'casos sifilis 2021' ='2021')

sifilis_data$UF=substr(sifilis_data$UF,3,22) 

sifilis_data <- sifilis_data %>%
  dplyr::mutate_if(is.character, str_trim)

sifilis_data <- sifilis_data %>% 
  subset(UF == 'Bahia'| UF == 'Pernambuco' | UF == 'Alagoas' | UF == 'Paraíba' | UF == 'Ceará' | UF == 'Sergipe' | UF == 'Rio Grande do Norte' | UF == 'Maranhão' | UF == 'Piauí') 

sifilis_data <- sifilis_data %>% 
  dplyr::select (-'Total') 

sifilis_data$`casos sifilis 2010` <- as.numeric(sifilis_data$`casos sifilis 2010`)

sifilis_data <- sifilis_data %>% 
  tidyr::pivot_longer(
    cols = !UF,
    names_to = "CASOS_SIFILIS",
    values_to = "QUANTIDADE_CASOS_SIFILIS" ) 

sifilis_data <- sifilis_data %>% 
  dplyr::mutate(ano='')

sifilis_data$ano=substr(sifilis_data$CASOS_SIFILIS, nchar(sifilis_data$CASOS_SIFILIS)-4,nchar(sifilis_data$CASOS_SIFILIS)) 
sifilis_data$ano <- as.numeric(sifilis_data$ano)


# linkage de dados (merge por UF e ano)
merged_data <- merge(aids_data, sifilis_data, by=c('UF','ano'))
merged_data <- merge(merged_data, determinantes, by=c('UF','ano')) 


# tratamento do painel de dados 

merged_data$DETERMINANTES = substr(merged_data$DETERMINANTES,1,nchar(merged_data$DETERMINANTES)-4) 

merged_data <- merged_data %>%
  dplyr::mutate_if(is.character, str_trim)

merged_data <- merged_data %>% 
  dplyr::mutate(factor(ano)) 

merged_data <- merged_data %>% 
  dplyr::select(-ano)

merged_data <- merged_data %>% 
  dplyr::rename('ANO' = 'factor(ano)')

merged_data <- dummy_cols(merged_data, select_columns = 'DETERMINANTES')

merged_data <- merged_data %>% 
  dplyr::rename('DUMMY_E_V_N' = 'DETERMINANTES_Esperança de vida ao nascer') %>% 
  dplyr::rename('DUMMY_M_A_E' = 'DETERMINANTES_Média de anos de estudo') %>% 
  dplyr::rename('DUMMY_R_P_C' = 'DETERMINANTES_Renda per capita')

merged_data <- merged_data %>% 
  dplyr::glimpse()

############## ajustes necessários 

# Ajuste um modelo de regressão por dados em painel
modelo_sifilis <- plm(QUANTIDADE_CASOS_SIFILIS ~ SCORE, data = merged_data, model="pooling")

# Visualize os resultados
summary(modelo_sifilis)
