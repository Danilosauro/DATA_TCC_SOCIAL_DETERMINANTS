
# Curso Spring Iyaleta
# Introdução ao Software R: Análise de dados secundários em estudos epidemiológicos


## AULA 06/12/23 ##



# Para acompanhar a aula ####

# Instalar/Carregar pacotes

#install.packages(pacman)

pacman::p_load(rio,         # Importação e exportação de dados
               tidyverse,   # Manipulação e tratamento de dados     
               lubridate,   # Manipulação de variáveis do tipo data
               abjutils)    # Limpeza de texto


# Diretório de trabalho ####

# Como alterar o diretório de trabalho

setwd("D:\\Curso_R_Spring_Iyaleta") 
setwd("D:/Curso_R_Spring_Iyaleta")



base2 <- import("base2.xlsx")

glimpse(base2)



####  Relembrando a última aula: função group_by

# A função group_by() do pacote dplyr e é usada para agrupar linhas por valores de coluna no banco de dados
# É usado para coletar dados idênticos em grupos no DataFrame e executar funções de agregação nos dados agrupados.
# Em geral, a operação Group by envolve a divisão dos dados, a aplicação de algumas funções e, por fim, a agregação dos resultados.


pacman::p_load(dplyr)


# Exemplo

##Passo 1: group_by() por faixa etária -- casos de sindrome respiratoria aguda grave por faixa etária

## vou primeiro transformar os dados de sexo em fator

base_exemplo <- base2 %>%  
  rename(dtsint = DT_SIN_PRI,
         dtnasc=DT_NASC) %>%  
  mutate(idade = trunc(as.numeric(difftime(dtsint, dtnasc, units = "days")) / 365.25))


base_exemplo <- base_exemplo %>%
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


base_exemplo_idade <- base_exemplo %>% 
  group_by(faixa_etaria)



## Passo 2:  utilizar o comando summarise após o group_by() para fazer um resumo dos dados com a função n()

# a função n() conta o nº de observações em cada grupo


base_exemplo_idade <- base_exemplo_idade %>% 
  summarise(n = n())

base_exemplo


## outra análise

## utilizar o comando summarise após o group_by() para fazer um resumo dos dados com a função mean

## temos que tirar os NA´s da idade 


base_exemplo <- base_exemplo %>% 
    drop_na(idade)


## agora vamos calcular a média da idade segundo o sexo e arredondar as casas decimais


base_exemplo <- base_exemplo %>% 
  mutate(sexo = factor(CS_SEXO, 
                       levels = c("F", "M", "I"),
                       labels = c("Feminino", "Masculino", "Ignorado")))


base_exemplo2 <- base_exemplo %>% 
  group_by(sexo) %>% 
  summarise(media = round(mean(idade), digits=1))

base_exemplo2



### Exercício com group_by

# Calcular/comparar a idade média, mediana para a variável Tosse (teve ou não teve sintoma de tosse):





### Para acompanhar a aula de hoje -  Tabulação ####

# Instalar/Carregar pacotes
pacman::p_load(
  tidyverse,
  lubridate,
  rio,
  janitor,  # Pacote para explorar e limpar dados
  gtsummary # Pacote para tabulação de dados
)



# Página do pacote:
## https://www.danieldsjoberg.com/gtsummary/

# FAQ e galeria de exemplos:
## https://www.danieldsjoberg.com/gtsummary/articles/gallery.html


## Importar a base 2
base2 <- import("base2.xlsx")


## criar a idade e a faixa etária

base2 <- base2 %>%  
  rename(dtsint = DT_SIN_PRI,
         dtnasc=DT_NASC) %>%  
  mutate(idade = trunc(as.numeric(difftime(dtsint, dtnasc, units = "days")) / 365.25))


base2 <- base2 %>%
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


## Medidas resumo por grupo ----

base_grupo <-  base2 %>% 
  group_by(CS_SEXO,faixa_etaria) %>% 
  summarise(
    n = n()
  )

# Mesma coisa:

base_grupo <-  base2 %>% 
  count(CS_SEXO,faixa_etaria)



# As funções estatísticas básicas não fucionam no universo tidy depois do 
## operador %>% (pipe):

#base2 %>% 
# group_by(CS_SEXO) %>% 
# min(idade)



# A função summarise() pode ter mais de um argumento

base_grupo <-  base2 %>%
  drop_na(idade) %>%    ## excluir os NA´s da idade
  group_by(CS_SEXO) %>%
  summarise(
    n = n(),
    idade_min = min(idade),
    idade_media = mean(idade),
    idade_max = max(idade), 
    idade_mediana = median(idade)) 



### Sem o group_by: resultados para toda a base


base_grupo <-  base2 %>%
  drop_na(idade) %>%    ## excluir os NA´s da idade
  summarise(
    n = n(),
    idade_min = min(idade),
    idade_media = mean(idade),
    idade_max = max(idade), 
    idade_mediana = median(idade)) 




#### TABULAÇÃO --------

## Tabelas de Frequência (até 3 var.) ----

# Funções tabyl e adorn_
# adorn = adornar, enfeitar...


# número de casos de síndrome respiratória aguda grave 
base_grupo <-  base2 %>%
  tabyl(CS_SEXO, faixa_etaria)


# número de casos removendo categorias sem observação


base_grupo <-  base2 %>%
  tabyl(
    CS_SEXO, faixa_etaria,      ## sexo na linha e faixa etária na coluna
    show_missing_levels = F
  )


# freq relativa por linha (default)

base2 %>%
  tabyl(CS_SEXO, faixa_etaria) %>%
  adorn_percentages()


base_grupo <-  base2 %>%
  tabyl(CS_SEXO, faixa_etaria) %>%
  adorn_percentages()


# freq relativa por coluna
base2 %>%
  tabyl(CS_SEXO, faixa_etaria) %>%
  adorn_percentages(
    denominator = 'col')



# freq relativa: alterar número de dígitos

base2 %>%
  tabyl(CS_SEXO, faixa_etaria) %>%
  adorn_percentages(
    denominator = 'col') %>%
  adorn_pct_formatting(
    digits = 1)


# freq relativa: retirar %

base2 %>%
  tabyl(CS_SEXO, faixa_etaria) %>%
  adorn_percentages(
    denominator = 'col') %>%
  adorn_pct_formatting(
    digits = 1,
    affix_sign = F)


# Incluir N e %, default: % (N)

 Base_grupo <-  base2 %>%
  tabyl(CS_SEXO, faixa_etaria) %>%
  adorn_percentages() %>%
  adorn_pct_formatting(
    digits = 1,
    affix_sign = F) %>%
  adorn_ns()



# Modificar para N (%)
 
 
 Base_grupo <-  base2 %>%
   tabyl(CS_SEXO, faixa_etaria) %>%
   adorn_percentages() %>%
  adorn_pct_formatting(
    digits = 1,
    affix_sign = F) %>%
  adorn_ns(
    position = 'front')
 
 
 

# Incluir terceira variável  - zona de moradia
 
 base2 <-  base2 %>% 
   mutate(zona= case_when(CS_ZONA==1 ~ "Urbana",
                          CS_ZONA==2~"Rural",
                          CS_ZONA==3~"Periurbana",
                          CS_ZONA==9~"Ignorado"),
          zona=factor(zona, levels=c("Urbana","Rural","Periurbana","Ignorado"))) %>% 
    drop_na(zona)
 
 
 
 
tab1 <- base2 %>%
  tabyl(CS_SEXO, faixa_etaria,zona)

tab1


tab1 %>%
  bind_rows(.id = 'zona')




### Tabelas prontas: gtsummary ----


## Com variáveis qualitativas

## Incluir variável febre

base2 <-  base2 %>% 
  mutate(febre= case_when(FEBRE==1 ~ "Sim",
                          FEBRE==2~"Não",
                          FEBRE==9~"Ignorado"),
         febre=factor(febre, levels=c("Sim","Não","Ignorado"))) %>% 
  drop_na(febre)



base2 %>%
  select(
    CS_SEXO, faixa_etaria,zona, febre) %>%
  tbl_summary()


# Mudando os rótulos:

tab2 <- base2 %>%
  select(
    CS_SEXO, faixa_etaria,zona, febre) %>%
  tbl_summary(
    label = list(
      CS_SEXO ~ 'Sexo',
      faixa_etaria ~ 'Faixa etária',
      zona ~ 'Zona',
      febre ~ 'Febre'))

tab2


# Mudando o cabeçalho:

show_header_names(tab2)

tab2 %>%
  modify_header(
    label = '**Variável**')



### Tabela com variáveis numéricas ----

# Default: Mediana (IIQ)

base2 %>%
  drop_na(idade, zona) %>% 
    select(idade, zona) %>%
  tbl_summary()


# idade x zona de mnoradia


base_grupo <-  base2 %>%
  drop_na(idade, zona) %>% 
  select(idade, zona) %>%
  tbl_summary(
    by = zona)

base_grupo


# Média e DP

tab3 <- base2 %>%
  drop_na(idade, zona) %>% 
  select(idade, zona) %>%
  tbl_summary(
    by = zona,
    statistic = list(idade ~ "{mean} ({sd})"))

tab3


# Adicionando coluna de total


tab3 %>%
  add_overall(
    last = T
  ) %>%
  modify_header(
    label = '**Variável**')



# Modificando footnote --rodapé

tab3 %>%
  modify_footnote(
    all_stat_cols() ~ 'Média (dp)')



## Exportação ----

export(tab3, "tabela3.xlsx")

