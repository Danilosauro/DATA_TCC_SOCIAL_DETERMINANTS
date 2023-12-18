
# Curso Spring Iyaleta
# Introdução ao Software R: Análise de dados secundários em estudos epidemiológicos


## AULA 27/11/23 ##



# Para acompanhar a aula ####

# Instalar/Carregar pacotes

#install.packages(pacman)

pacman::p_load(rio,         # Importação e exportação de dados
               tidyverse,   # Manipulação e tratamento de dados     
               lubridate,   # Manipulação de variáveis do tipo data
               abjutils)    # Limpeza de texto


# Diretório de trabalho ####

# Mostrar o diretório de trabalho atual
path <- getwd()
# Como alterar o diretório de trabalho

setwd(path) 

base <- import("base.xlsx")

##Estrutura geral do banco de dados e das variáveis ####
# Para visualizar a base de dados
View(base)

# Nomes da variáveis
names (base)

# Primeiras observações
head(base)
head(base, 10)

# Últimas observações
tail(base)
tail(base,2)

# Estrutura geral (classes das variáveis e primeiras observações)
# MUITO UTILIZADO!!
glimpse(base) 
str(base)


# Análise exploratória de dados ####
# Vamos explorar nossas variáveis
# Objetivo: Identificar necessidade de transformação


# class() ou glimpse() para a classe da variável

class(base$dtsint) 
# Transformações necessárias: transformar para data

# Função table() para tabela de frequência exploratória
# Uso do cifrão para dropdown list das variáveis

# Resultado do teste
table(base$CS_RACA)

# count() para tabela de frequência exploratória
# Resultado do teste
base %>% 
  count(CS_RACA)

# Transformações necessárias: Tranformar em categórico, segundo dicionário (NA = Ignorado)

# Sexo
base %>% 
  count(CS_SEXO)

class(base$CS_SEXO)


# Transformações necessárias: nenhuma

# Idade
base %>% 
  count(idade)

class(base$idade)

# summary() para gerar medidas resumo
summary(base$idade) 
summary(base)


# Transformações necessárias: transformar >117 em ignorado
# Criar faixas-etárias


# Selecionar colunas: select() ####

# Revisar a base de dados
glimpse(base)

count(base$CS_SEXO)
# Abrir o banco de dados para identificar a posição das colunas  
View(base)

# Selecionar a base de dados por posição
base_exemplo <- base %>% 
  select(1,2,3,4)

glimpse(base_exemplo)
head(base_exemplo)

# Por nome
base_exemplo <- base %>% 
  select(CS_SEXO, dtsint)

head(base_exemplo)


# Excluir dataframe usado para exemplo
rm(base_exemplo)


# Pode ser usado para reorganizar as variáveis no dataframe
# Primeiro vamos observar os nomes das variáveis 
glimpse(base)


# Seleção das colunas de interesse e reorganização das posições na base base
base_exemplo <- base %>% 
  select(CS_SEXO, dtsint, CS_RACA) 

# Vamos observar como ficaram as colunas na base base
glimpse(base_exemplo)

# Seleção de variáveis com operador "-" e função c()
base_exemplo <- base %>%
  select(-CS_RACA)

glimpse(base_exemplo)

# Remover mais de uma variável da base de dados
base_exemplo <- base %>%
  select(-dtsint, -CS_SEXO)


glimpse(base_exemplo)



#Renomear variáveis: rename() ####

# Primeiro vamos relembrar os nomes das variáveis na base base e sua estrutura
glimpse(base)

# Para renomear uma variável use a função rename()
# Ex.: Vamos transformar datanotificacao para maiúscula em base
base_exemplo <- base %>%    
  rename(datainiciosintomas = dtsint) 

glimpse(base_exemplo)


# Use crases quando há espaços ou acentos especiais no nome da variável
# Ex.: Vamos conferir a base de população municipal em 2020

##Exemplo =
# pop_20 <- pop_20 %>%    
#   rename(MUNICIPIO = `Município de residência`,
#          POP_2020 = pop_2020) 


#Remover objetos
rm(list=ls())


# Criar e transformar colunas: mutate() ####
# Criar uma coluna com base em outra variável do banco
##mutate() cria novas colunas que são funções de variáveis existentes. 
##Também pode modificar (se o nome for o mesmo de uma coluna existente)
#liminar colunas (definindo o seu valor como NULL).

base_exemplo <- base %>% 
  mutate(datanotificacao = dtsint)

glimpse(base_exemplo)


# Remover o objeto 
rm(base_exemplo)


# A função mutate permite múltiplos argumentos
base <- base %>% 
  mutate(datanotificacao    = as_date(dtsint),
         datainiciosintomas = as_date(dtsint))

glimpse(base)


## excluir variáveis criadas

base <- base %>% 
  select(-datanotificacao,-datainiciosintomas)


# Transformações de variáveis do tipo data ####
# Dia-Mês-Ano (Day-Month-Year)

base <- base %>% 
  mutate(dt_sin_pri  = ymd(dtsint),  ##observar o formato da data: se é ymd ou mdy
         dt_nasc     = ymd(dtnasc))


# glimpse() com variáveis selecionadas para observar o resultado
base %>% 
  select(dtsint, dt_sin_pri,
         DT_NASC, dt_nasc) %>% 
  glimpse()



# Mês-Dia-Ano (Month-Day-Year)
mdy("11/03/2020")
mdy("Mar 11 2020")

# Ano-Dia-Mês (Year-Day-Month)
ydm("2020 11 03")
ydm("2020 11 Mar")

# Ano-Mês-Dia (Year-Month-Day)
ymd("20200311")


# Apenas o mês
base <- base %>% 
  mutate(dt_nasc_mes = month(dtnasc))


# Apenas o ano   
base <- base %>% 
  mutate(dt_nasc_ano = year(dtnasc))


base %>% 
  count(dt_nasc_ano)  ## frequência de pessoas nascidas em cada ano



# Ano epidemiológico 
epiyear("2022/01/01")
epiyear("2021/12/31")

# Semana epidemiológica 
epiweek("2021/12/31")
epiweek("2022/01/01")

# Semana epidemiológica 
base %>% 
  mutate(se_notific  = epiweek(dtsint))%>% 
  count(se_notific)

# Ano epidemiológico 
base %>% 
  count(epiyear(dtsint))


# Datas e Operadores aritméticos
base <- base %>% 
  mutate(Idade = trunc(as.numeric(difftime(dtsint, dtnasc, units = "days")) / 365.25))


# Tabulação e classe da variável Idade para observar o resultado
base %>% 
    count(Idade)

class(base$Idade)


# Método strptime 
format(as_date("2020-03-11"), "%d/%m/%Y")
format(as_date("2020-03-11"), "%d-%m-%y")
format(as_date("2020-03-11"), "%d %B %Y")
format(as_date("2020-03-11"), "%d %b %Y")


# Transformação de variáveis numéricas ####

# Números inteiros
base <- base %>% 
  mutate(idade = as.integer(Idade))

base_exemplo <- base %>% mutate(dtnasc_ano = year(dtnasc)) 
base_exemplo$dtnasc_ano <- as.factor(base_exemplo$dtnasc_ano) 
summary(base_exemplo$dtnasc_ano)
class(base_exemplo$dtnasc_ano)

# Transformação de variáveis do tipo fator ####

# Criar fator e trocar os rótulos
base <- base %>% 
  mutate(sexo = factor(CS_SEXO, 
                       levels = c("F", "M", "I"),
                       labels = c("Feminino", "Masculino", "Ignorado")))

base %>% 
  count(CS_SEXO)

base %>% 
  count(sexo)


# Fatores em ordem inversa
base %>% 
  count(fct_infreq(sexo))


#Transformação de variáveis do tipo texto ####

# Transformar em caractere
caractere <- 12345
class(caractere)
caractere <- as.character(caractere)
class(caractere)

# Transformar de caracter para fator 
glimpse(base)
base$UF <- as.factor(base$SG_UF_NOT)
class(base$UF)


names(base)

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
base <- base %>% 
  mutate(sexo=str_to_lower(CS_SEXO)) 

base <- base %>% 
  select(7)
rm(base$teste) 

base <- base %>% 
  mutate(teste = idade > 18)

# Mudar um texto para caixa alta
base <- base %>% 
  mutate(sexo=str_to_upper(sexo))

base_exemplo <- base_exemplo %>% select(-teste)

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


# Frequência da variável CS_SEXO

base %>% 
  count(CS_SEXO)


# Criar variável Raca com labels 
base <- base %>%
  mutate(sexo = case_when(CS_SEXO == "F" ~ "Feminino", 
                          CS_SEXO == "M" ~ "Masculino",
                           TRUE     ~ "Ignorado"))




### Prática

## Ler de novo o dataset chamado de "base"

# selecione variáveis de interesse

# Exclua duas variáveis do bano

# mudar o nome de alguma variável ---escolha o nome que desejar

# frequência por unidade da federação de notificação (SG_UF_NOT)
