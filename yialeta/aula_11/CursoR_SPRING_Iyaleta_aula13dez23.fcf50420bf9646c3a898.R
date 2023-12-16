

# Curso Spring Iyaleta
# Introdução ao Software R: Análise de dados secundários em estudos epidemiológicos


## AULA 13/12/23 ##



library(pacman)

p_load( tidyverse, lubridate, ggplot2,
        rio,readxl,catdata)



#------------------------------------------------------------


# Diretório de trabalho ####

path <- getwd()
setwd(path) 


#população por IDADE 

PopUF <- import("Pop_fxetar_0a80_anos.xls",sheet = "Pop2")


### deixando a base PopUF em formato long

pop <-  PopUF %>% 
  pivot_longer(
    cols = "0 a 4 anos":"60+ anos", 
    names_to = "faixa_etaria",
    values_to = "Pop") 


###filtrando apenas o estado da BAHIA e apenas os anos de 2022 e 2023

popBA <-  pop %>% 
  filter(UF=="Bahia") %>% 
  filter(Ano>2021)



#### base de sindrome respiratória aguda grave - Bahia


sragBA_22 <- import("sivepBA22.xlsx")
sragBA_23 <- import("sivepBA23.xlsx")


sragBA_22 <- sragBA_22 %>%
  dplyr::select(DT_NASC, DT_SIN_PRI, DT_NOTIFIC, CS_SEXO,TOSSE)

sragBA_23 <- sragBA_23 %>%
  dplyr::select(DT_NASC, DT_SIN_PRI, DT_NOTIFIC, CS_SEXO,TOSSE)



# Juntar as bases

sragBA<- rbind (sragBA_22,sragBA_23)

#------------------------------------------------------------

## criar variáveis

base_BA_exemplo <- sragBA %>%  
  rename(dtsint = DT_SIN_PRI,
         dtnasc=DT_NASC) %>%  
  mutate(idade = trunc(as.numeric(difftime(dtsint, dtnasc, units = "days")) / 365.25))


base_BA_exemplo <- base_BA_exemplo %>%
  mutate(faixa_etaria = case_when(idade>=0 & idade<5 ~ "0 a 4 anos",
                                  idade>=5 & idade<10 ~ "5 a 9 anos",
                                  idade>=10 & idade<20 ~ "10 a 19 anos",
                                  idade>=20 & idade<30 ~ "20 a 29 anos",
                                  idade>=30 & idade<40 ~ "30 a 39 anos",
                                  idade>=40 & idade<50 ~ "40 a 49 anos",
                                  idade>=50 & idade<60 ~ "50 a 59 anos",
                                  idade>=60 & idade<=100 ~ "60 a 100 anos",
                                  TRUE ~ "Ignorado"),
         faixa_etaria = factor(faixa_etaria, levels = c("0 a 4 anos", "5 a 9 anos","10 a 19 anos",
                                                        "20 a 29 anos", "30 a 39 anos", "40 a 49 anos", "50 a 59 anos", "60 a 100 anos","Ignorado")))

base_BA_exemplo <- base_BA_exemplo %>% 
  mutate(sexo = factor(CS_SEXO, 
                       levels = c("F", "M", "I"),
                       labels = c("Feminino", "Masculino", "Ignorado")))


base_BA_exemplo <- base_BA_exemplo %>% 
  mutate(tosse = factor(TOSSE, 
                        levels = c(1, 2, 9),
                        labels = c("Sim", "Não", "Ignorado")))



### Agregar os dados para contabilizar os casos por faixa etária, ano

hosp_BA<- base_BA_exemplo %>%
  mutate(Ano= format(as.Date(dtsint, format="%d/%m/%Y"),"%Y")) %>% 
  group_by(faixa_etaria,Ano) %>%
  summarise(hosp = n()) 


## Juntar a base popBA com a base hosp_BA

Hosp_BA_incid <- merge(hosp_BA,popBA, by = c("Ano", "faixa_etaria"),all = T)


### Retirando os NA´s 

Hosp_BA_incid <-  Hosp_BA_incid %>%  
  drop_na()



### Calcular o coeficiente de incidência


Hosp_BA_incid <- Hosp_BA_incid %>%
  group_by(faixa_etaria,Ano, Pop) %>%
  summarise(hosp = sum(hosp)) %>% 
  mutate(taxa_hosp = hosp/Pop * 100000)



### GRÁRFICO

cbbPalette <- c("#01665e", "#8c2958","#fb2e01", "#666666", "#cb0e40")


plot<-     ggplot() +
  geom_bar(data= Hosp_BA_incid,aes(x=faixa_etaria,y=taxa_hosp, fill=faixa_etaria),stat="identity",position = position_dodge(), alpha = 0.75)+
  labs(title="Taxa de hospitalização de SRAG segundo faixa etária", 
       x="Faixa etária",y="Taxa de hospitalização \n (por 100.000 habitantes)",fill="") +
  facet_wrap(~Ano)+
  scale_fill_manual("", values = cbbPalette) +
  theme_classic()+
  theme(legend.key.height = unit(0.2, "cm")) +
  guides(fill=guide_legend(ncol=3))+
  theme(legend.position = "none",legend.box = "vertical")+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 11, angle = 0, hjust = 1, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.text=element_text(size=14),
        text=element_text(size=15),
        title = element_text(size=14))


plot


ggsave("plot.png", width = 10, height = 7)





######################## Prática #######################



## Escolha um conjunto de dados de sua preferência e faça: 

   # Análise descritiva
   # 1 tabela
   # 2 gráficos a sua escolha
   # breve interpretação de seus achados.



## Base de dados 1  - economics

data(economics) 

economic_data <- read_excel('economics.xlsx') 

glimpse(economic_data)

economic_data <- economic_data %>% 
  mutate(ano = year(date))

ggplot() +
  geom_bar(data=economic_data ,aes(x=ano,y=unemploy, fill=unemploy),stat="identity",position = position_dodge(), alpha = 0.75)+
  labs(title="Unemployed persons per year.", 
       x="Year",y="Absolute unemployed",fill="") 

library(gtsummary)

economic_data %>%
  tbl_summary(include = c(pce, pop, psavert, unemploy)) %>% 
  modify_header(label = "**Variable**")

table1
  
#O conjunto de dados econômicos é um conjunto de dados com 6 variáveis e 574 observações. 
# Contém informações sobre população, desemprego e consumo pessoal com relação ao ano e ao mês. 
#produzido a partir de dados de séries temporais econômicas dos EUA disponíveis no Federal Reserve Bank of St. Louis. Os dados já foram pré-processados.

economic_data <- read

## Descrição da variável

# date: mês da coleta de dados
# pce: despesas de consumo pessoal, em bilhões de dólares
# pop: população total, em milhares
# psavert:  taxa de poupança pessoal
# uempmed:  duração mediana do desemprego, em semanas
# unemploy: number of unemployed (número de desempregados), em milhares




## Base de dados 2  - aids

data(aids)

# Um dataframe com 2376 observações nas 8 variáveis a seguir.
# cd4 - número de células CD4
# time -  anos desde a soroconversão
# drugs -  uso de drogas recreativas (sim=1/não=0)
# partners - número de parceiros sexuais
# packs - maços de cigarros por dia
# cesd  - escore de doença mental
# age - Idade centrada em torno de 30 anos
# person - Número de identificação



## Base de dados 3  - airquality 

data(airquality)

# dataframe com 153 observações em 6 variáveis.

#Leituras diárias dos seguintes valores de qualidade do ar de 1º de maio de 1973 a 30 de setembro de 1973.

#Ozône: Ozônio médio em partes por bilhão de 1300 a 1500 horas na Ilha Roosevelt

#Solar.R: Radiação solar em Langleys na faixa de frequência de 4000 a 7700 Angstroms, das 8h00 às 12h00, no Central Park

#wind: Velocidade média do vento em milhas por hora às 07h00 e às 1000 horas no Aeroporto LaGuardia

#Temp: Temperatura máxima diária em graus Fahrenheit no Aeroporto La Guardia.



