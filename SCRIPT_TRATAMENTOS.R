install.packages('dplyr')
install.packages('readxl') 
install.packages('ggplot2') 
install.packages('tidyr') 
install.packages('forcats') 
install.packages('rio') 
install.packages('stringr') 
install.packages('purrr')

library(dbplyr) 
library(readxl) 
library(ggplot2) 
library(tidyr) 
library(forcats) 
library(rio) 
library(stringr) 
library(purrr)

setwd('/home/danilo.dias/R_files/DATA_TCC')

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

test <- determinantes %>% 
  tidyr::pivot_longer(
    cols = !UF,
    names_to = "DETERMINANTES",
    values_to = "SCORE" ) 

test <- test %>% 
  dplyr::mutate(ano='')

test$ano <- substr(test$DETERMINANTES, nchar(test$DETERMINANTES)-4,nchar(test$DETERMINANTES)) 
test$ano <- as.numeric(test$ano)



ggplot(determinantes, aes(x = UF)) +
  geom_bar(aes(y = determinantes$`Esperança de vida ao nascer 2012`, fill = "Esperança de vida ao nascer 2012"), stat = "identity", position = "stack", color='lightgreen', fill='grey') + 
  labs(title = "Esperança de vida ao nascer por Estado em 2012",
       x = "UF",
       y = "Esperança de vida ao nascer",
       fill = "Esperança de vida ao nascer")

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

test_aids <- aids_data %>% 
  tidyr::pivot_longer(
    cols = !UF,
    names_to = "CASOS_AIDS",
    values_to = "QUANTIDADE_CASOS_AIDS" ) 

test_aids <- test_aids %>% 
  dplyr::mutate(ano='') 

test_aids$ano <-substr(test_aids$CASOS_AIDS, nchar(test_aids$CASOS_AIDS)-4,nchar(test_aids$CASOS_AIDS)) 
test_aids$ano <- as.numeric(test_aids$ano)

ggplot(aids_data, aes(x = UF)) +
  geom_bar(aes(y = aids_data$`casos aids 2010`, fill = "casos aids 2010"), stat = "identity", position = "stack", color='lightgreen', fill='grey') + 
  labs(title = "Número de Casos por UF e Ano",
       x = "UF",
       y = "Número de Casos",
       fill = "Ano") 

# tratamento dos dados de notificacoes de sifilis adquirida desde 2010 ~ sinan 
rm(sifilis_data)

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

test_sifilis <- sifilis_data %>% 
  tidyr::pivot_longer(
    cols = !UF,
    names_to = "CASOS_SIFILIS",
    values_to = "QUANTIDADE_CASOS_SIFILIS" ) 

test_sifilis <- test_sifilis %>% 
  dplyr::mutate(ano='')

test_sifilis$ano=substr(test_sifilis$CASOS_SIFILIS, nchar(test_sifilis$CASOS_SIFILIS)-4,nchar(test_sifilis$CASOS_SIFILIS)) 
test_sifilis$ano <- as.numeric(test_sifilis$ano)


# unir dados 
rm('teste_merged_data')

teste_merged_data <- merge(test_aids, test_sifilis, by=c('UF','ano'))
teste_merged_data <- merge(teste_merged_data, test, by=c('UF','ano')) 

# merged_data<-  merge(aids_data, sifilis_data, by='UF') 
# merged_data <- merge(merged_data, determinants, by = 'UF') 

list_data_to_merge <-  list(test, test_aids, test_sifilis)

teste_merged_data <- list_data_to_merge %>% 
  purrr::reduce(dplyr::inner_join, by='UF') 



rm(list_data_to_merge)


# análises do painel de dados 

summary(merged_data) 
dplyr::glimpse(merged_data) 


merged_data <- merged_data %>%  
  dplyr::mutate_at(c('casos aids 2010', 'casos aids 2011', 'casos aids 2012', 'casos aids 2013', 
                     'casos aids 2014', 'casos aids 2015', 'casos aids 2016', 'casos aids 2017',
                     'casos aids 2018', 'casos aids 2019', 'casos aids 2020', 'casos aids 2021'), as.numeric)

dplyr::glimpse(merged_data) 
