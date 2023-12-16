

# Curso Spring Iyaleta
# Introdução ao Software R: Análise de dados secundários em estudos epidemiológicos


## AULA 29/11/23 ##



# Para acompanhar a aula ####

# Instalar/Carregar pacotes

#install.packages(pacman)

pacman::p_load(rio,         # Importação e exportação de dados
               tidyverse,   # Manipulação e tratamento de dados     
               lubridate,   # Manipulação de variáveis do tipo data
               abjutils)    # Limpeza de texto


# Diretório de trabalho ####

# Mostrar o diretório de trabalho atual
getwd()


# Como alterar o diretório de trabalho

setwd("D:\\Curso_R_Spring_Iyaleta") 
setwd("D:/Curso_R_Spring_Iyaleta")

base <- import("base.xlsx")



###################### retomar aula a partir daqui ##############

# Transformação de variáveis numéricas ####

# Números inteiros
base_exemplo <- base %>% 
  mutate(idade = as.integer(idade))


### Retirar os "NA´s" de todas as variáveis do banco de dados

base_exemplo <- base %>% 
    drop_na()


### Retirar os "NA´s" de uma variável específica

base_exemplo <- base %>% 
  drop_na(idade)



# Transformação de variáveis do tipo fator ####


# Transformar de caracter para fator usando a lógica básica
glimpse(base)
base$UF <- as.factor(base$SG_UF_NOT)
class(base$UF)

names(base)


# Criar variável fator e trocar os rótulos
base_exemplo <- base %>% 
  mutate(sexo = factor(CS_SEXO, 
                       levels = c("F", "M", "I"),
                       labels = c("Feminino", "Masculino", "Ignorado")))

base_exemplo %>% 
  count(CS_SEXO)

base_exemplo %>% 
  count(sexo)


# Fatores em ordem inversa

base_exemplo %>% 
  count(fct_infreq(sexo))


#Transformação de variáveis do tipo texto ####

# Transformar em caractere
caractere <- 12345
class(caractere)
caractere <- as.character(caractere)
class(caractere)



# Recortar textos com base na posição

texto <- "Epidemiologia"
class(texto)

str_sub(string = texto, 
        end = 3, 
        start = 1)

str_sub("Epidemiologia", 1, 3)


# Espaços são caracteres
str_sub("Vigilância epidemiológica em emergências de saúde pública", -28)


# Encontrar partes de texto

str_detect("Vigilância epidemiológica em emergências de saúde pública", 
           pattern = "saúde") 


# Mudar um texto para caixa baixa
base_exemplo <- base %>% 
  mutate(sexo=str_to_lower(CS_SEXO))

# Mudar um texto para caixa alta
base <- base %>% 
  mutate(sexo=str_to_upper(sexo))


##(Re) categorizar variáveis: recode() e replace_na() ####

base_exemplo <-  base %>% 
  count(CS_RACA)


base_exemplo <- base_exemplo %>% 
  mutate(raca = recode(CS_RACA,
                       "1" = "Branca",
                       "2" = "Preta",
                       "3" = "Amarela",
                       "4" = "Parda",
                       "5" = "Indígena",
                       "9" = "Ignorado"),
         raca = replace_na(raca, "Ignorado"))


base_exemplo %>% 
  select(CS_RACA, raca)



## (Re) categorizar variáveis: case_when() ####


# Frequência da variável idade 


base %>% 
  count(idade)


# Criar variável faixa etária com labels 

base_exemplo <- base %>%
  mutate(faixa_etaria = case_when(idade>=0 & idade<5 ~ "0 a 4 anos",
                                                            idade>=5 & idade<10 ~ "5 a 9 anos",
                                                            idade>=10 & idade<20 ~ "10 a 19 anos",
                                                            idade>=20 & idade<30 ~ "20 a 29 anos",
                                                            idade>=30 & idade<40 ~ "30 a 39 anos",
                                                            idade>=40 & idade<50 ~ "40 a 49 anos",
                                                            idade>=50 & idade<60 ~ "50 a 59 anos",
                                                            idade>=60 & idade<=100 ~ "60+ anos",
                                                            TRUE ~ "Ignorado"),
         faixa_etaria = factor(faixa_etaria, levels = c("0 a 4 anos", "5 a 9 anos","10 a 19 anos",
            "20 a 29 anos", "30 a 39 anos", "40 a 49 anos", "50 a 59 anos", "60+ anos","Ignorado")))





#### A função filter
#plyr package
#RFetornar linhas com condições específicas determinadas pelo usuário
#utilizada para obter um subconjunto de um dataset, retendo todas as linhas que satisfazem suas condições. 

#Há muitas funções e operadores que são úteis na construção das expressões usadas para filtrar os dados:
# ==, >, >= etc
# &, |, !, xor()
#is.na()
#between(), near()

##exempl0 1- filtar no banco apenas pessoas com idade entre 0 e 100 

base_exemplo <-  base %>%
  filter(idade>=0 & idade <=100) 


##exempl0 2 - filtrar crianças abaixo de 5 anos
base_exemplo <-  base %>%
  filter(idade < 5) 


## Filtar apenas quem tem informação sobre sexo -- nesse caso não queremos o ignorado

base_exemplo <-  base %>%
  filter(CS_SEXO !="I") 

glimpse(base_exemplo)



##Filtrar período entre duas datas


base_exemplo <- base %>% 
  filter(dtsint >= "2023-01-01" &
           dtsint < "2023-10-01")
  
  

#### A função group_by
# A função group_by() do pacote dplyr e é usada para agrupar linhas por valores de coluna no banco de dados
# É usado para coletar dados idênticos em grupos no DataFrame e executar funções de agregação nos dados agrupados.
# Em geral, a operação Group by envolve a divisão dos dados, a aplicação de algumas funções e, por fim, a agregação dos resultados.


pacman::p_load(dplyr)

# Exemplo

##Passo 1: group_by() por sexo -- casos de sindrome respiratoria aguda grave por sexo

## vou primeiro transformar os dados de sexo em fator


base <- base %>% 
  mutate(SEXO= factor(CS_SEXO))


base_exemplo <- base %>% 
 group_by(SEXO)



##Passo 2:  utilizar o comando summaRise após o group_by() para fazer um resumo dos dados com a função n()

# a função n() conta o nº de observações em cada grupo


base_exemplo <- base_exemplo %>% 
  summarise(n = n())
   
base_exemplo


##ou

## utilizar o comando summarise após o group_by() para fazer um resumo dos dados com a função mean

 ## temos que tirar os NA´s da idade 


base_exemplo_sexo <- base_exemplo %>% 
  group_by(SEXO) %>% 
  drop_na(idade)


## agora vamos calcular a média da idade segundo o sexo e arredondar as casas decimais

base_exemplo_sexo <- base_exemplo_sexo %>% 
  summarise(media = round(mean(idade), digits=1))

base_exemplo_sexo




##### Prática

## Ler de novo o dataset chamado de "base"

# selecione variáveis de interesse

# Exclua duas variáveis do bano

# mudar o nome de alguma variável ---escolha o nome que desejar

# frequência por unidade da federação de notificação (SG_UF_NOT)

# criar uma variável sexo do tipo fator usando o case_when

# Crie um banco de dados apenas com informações de pessoas do sexo feminino com idade entre 18 e 59 anos

# faça análise descritiva desse novo banco

# utilize o group_by por sexo e o summarise ccom a função median para calcular a mediana da idade por grupo


