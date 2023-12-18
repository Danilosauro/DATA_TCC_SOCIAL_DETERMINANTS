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
install.packages('RColorBrewer') 



library(dbplyr) 
library(readxl) 
library(ggplot2) 
library(tidyr) 
library(forcats) 
library(rio) 
library(stringr) 
library(purrr)
library(fastDummies)
library(RColorBrewer) 
library(plm)
#library(viridis)
#library(gtsummary)

install_formats()

setwd('/home/danilo.dias/R_files/DATA_TCC_SOCIAL_DETERMINANTS')

# tratamento dos dados de determinantes sociais ~ atlas do desenvolvimento humano ipea
determinantes <- read_excel('DADOS/IPEA_DATA.xlsx')  

determinantes <- determinantes %>% 
  drop_na() 

determinantes <- determinantes %>% 
  dplyr::rename('UF' = 'Territorialidades') 

determinantes <- determinantes %>% 
  subset(UF == 'Bahia'| UF == 'Pernambuco' | UF == 'Alagoas' | UF == 'Paraíba' | UF == 'Ceará' | UF == 'Sergipe' | UF == 'Rio Grande do Norte' | UF == 'Maranhão' | UF == 'Piauí')

esperanca_vida_nasc <- determinantes %>% 
  dplyr::select(starts_with(c('UF', 'Esperança')))

renda_per_capita <- determinantes %>% 
  dplyr::select(starts_with(c('UF', 'Renda'))) 

media_anos_estudo <- determinantes %>% 
  dplyr::select(starts_with(c('UF', 'Média')))
  

esperanca_vida_nasc <- esperanca_vida_nasc %>% 
  tidyr::pivot_longer(
    cols = !UF,
    names_to = "DETERMINANTES",
    values_to = "SCORE" )  

renda_per_capita <- renda_per_capita %>% 
  tidyr::pivot_longer(
    cols = !UF,
    names_to = "DETERMINANTES",
    values_to = "SCORE" ) 

media_anos_estudo <- media_anos_estudo %>% 
  tidyr::pivot_longer(
    cols = !UF,
    names_to = "DETERMINANTES",
    values_to = "SCORE" ) 

media_anos_estudo <- media_anos_estudo %>% 
  dplyr::mutate(ano='') 

renda_per_capita <- renda_per_capita %>% 
  dplyr::mutate(ano='') 

esperanca_vida_nasc <- esperanca_vida_nasc %>% 
  dplyr::mutate(ano='')

media_anos_estudo$ano <- substr(media_anos_estudo$DETERMINANTES, nchar(media_anos_estudo$DETERMINANTES)-4,nchar(media_anos_estudo$DETERMINANTES)) 
media_anos_estudo$ano <- as.factor(media_anos_estudo$ano) 

renda_per_capita$ano <- substr(renda_per_capita$DETERMINANTES, nchar(renda_per_capita$DETERMINANTES)-4,nchar(renda_per_capita$DETERMINANTES)) 
renda_per_capita$ano <- as.factor(renda_per_capita$ano) 

esperanca_vida_nasc$ano <- substr(esperanca_vida_nasc$DETERMINANTES, nchar(esperanca_vida_nasc$DETERMINANTES)-4,nchar(esperanca_vida_nasc$DETERMINANTES)) 
esperanca_vida_nasc$ano <- as.factor(esperanca_vida_nasc$ano)


# tratamento dos dados de  notificacoes de aids desde 1980 ~ sinan 

aids_data <- read_excel('DADOS/DATA_SINAN.xlsx') 

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
aids_data$ano <- as.factor(aids_data$ano)

# tratamento dos dados de notificacoes de sifilis adquirida desde 2010 ~ sinan 
sifilis_data <- import('DADOS/SIFILIS_ADQUIRIDA.csv') 

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
sifilis_data$ano <- as.factor(sifilis_data$ano)


# linkage de dados (merge por UF e ano)
merged_data_esperanca_vida_nasc  <- merge(esperanca_vida_nasc, aids_data, by=c('UF', 'ano'))
merged_data_esperanca_vida_nasc <- merge(merged_data_esperanca_vida_nasc, sifilis_data, by=c('UF', 'ano'))


merged_data_media_anos_est <- merge(media_anos_estudo, aids_data, by=c('UF', 'ano'))
merged_data_media_anos_est <- merge(merged_data_media_anos_est, sifilis_data, by=c('UF','ano'))

merged_data_renda_per_capita <- merge(renda_per_capita, aids_data, by=c('UF', 'ano'))
merged_data_renda_per_capita <- merge(merged_data_renda_per_capita, sifilis_data, by=c('UF','ano')) 


remove_all <- function(dataframe, ...){  
  lista <- c(...)
  dataframe <- dataframe %>% dplyr::select(-lista)
  }

merged_data_esperanca_vida_nasc <- remove_all(merged_data_esperanca_vida_nasc, 'CASOS_AIDS', 'CASOS_SIFILIS') 
merged_data_media_anos_est <- remove_all(merged_data_media_anos_est, 'CASOS_AIDS', 'CASOS_SIFILIS') 
merged_data_renda_per_capita <- remove_all(merged_data_renda_per_capita, 'CASOS_AIDS', 'CASOS_SIFILIS') 

painel_esperanca_vida_nasc <- merged_data_esperanca_vida_nasc
painel_media_anos_estudo <- merged_data_media_anos_est
painel_renda_per_capita <- merged_data_renda_per_capita


painel_data <- painel_renda_per_capita  

# tratamentos no painel
painel_data <- painel_data %>% 
  dplyr::rename('RENDA_PER_CAPITA' = 'SCORE') 

painel_data <- painel_data %>% 
  dplyr::mutate('MEDIA_ANOS_ESTUDO' = '') 

painel_data$MEDIA_ANOS_ESTUDO <- painel_media_anos_estudo$SCORE

painel_data <- painel_data %>%  
  dplyr::mutate('ESPERANCA_VIDA_NASC'= '') 

painel_data$ESPERANCA_VIDA_NASC <- painel_esperanca_vida_nasc$SCORE 

painel_data <- painel_data %>% 
  dplyr::select(-DETERMINANTES) 

rm(aids_data, determinantes, esperanca_vida_nasc, media_anos_estudo, merged_data_esperanca_vida_nasc, merged_data_esperanca_vida_nasc, merged_data_media_anos_est, painel_esperanca_vida_nasc, painel_media_anos_estudo, painel_renda_per_capita)
rm(merged_data_renda_per_capita, renda_per_capita, sifilis_data)


### subdivisões dos dataframes por estados para análises individualizadas

alagoas <- painel_data %>% 
  subset(UF == 'Alagoas') 
bahia <- painel_data %>% 
  subset(UF == 'Bahia') 
ceara <- painel_data %>% 
  subset(UF == 'Ceará') 
paraiba <- painel_data %>% 
  subset(UF == 'Paraíba')
pernambuco <- painel_data %>% 
  subset(UF == 'Pernambuco') 
piaui <- painel_data %>% 
  subset(UF == 'Piauí') 
maranhao <- painel_data %>% 
  subset(UF == 'Maranhão') 
sergipe <- painel_data %>%
  subset(UF=='Sergipe') 
rn <- painel_data %>% 
  subset(UF=='Rio Grande do Norte')






