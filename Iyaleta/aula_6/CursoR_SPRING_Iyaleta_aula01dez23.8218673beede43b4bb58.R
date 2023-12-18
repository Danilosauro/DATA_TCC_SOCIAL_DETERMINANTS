
# Curso Spring Iyaleta
# Introdução ao Software R: Análise de dados secundários em estudos epidemiológicos


## AULA 01/12/23 ##



# Para acompanhar a aula ####

# Instalar/Carregar pacotes

#install.packages(pacman)

pacman::p_load(rio,         # Importação e exportação de dados
               tidyverse,   # Manipulação e tratamento de dados     
               lubridate,   # Manipulação de variáveis do tipo data
               abjutils)    # Limpeza de texto


# Diretório de trabalho ####

# Mostrar o diretório de trabalho atual
#getwd()


# Como alterar o diretório de trabalho

setwd("D:\\Curso_R_Spring_Iyaleta") 
setwd("D:/Curso_R_Spring_Iyaleta")


### Corrigir exercício prático passado para casa


##### Prática de casa

## Ler de novo o dataset chamado de "base"

# selecione variáveis de interesse

# Exclua duas variáveis do bano

# mudar o nome de alguma variável ---escolha o nome que desejar

# frequência por unidade da federação de notificação (SG_UF_NOT)

# criar uma variável sexo do tipo fator usando o case_when

# Crie um banco de dados apenas com informações de pessoas do sexo feminino com idade entre 18 e 59 anos

# faça análise descritiva desse novo banco




### Mais prática 


# Transformações na base de dados ####
# Ler no R a base2
# A base2 tem 8 variaveis com diferentes classes 
# Tosse/Febre: 1-Sim; 2-Não; 9-Ignorado
# CS_ZONA: 1-Urbana; 2-Rural; 3-Periurbana; 9-Ignorado


base2 <- import("base2.xlsx")

glimpse(base2)

# Transformar as variáveis de data em formato data no R
# Retirar os NA´s do banco
# Criar a idade a partir da data de notificação
# Criar a faixa etária
# Recategorizar variáveis que são qualitativas, mas estão sem label
# Colocar as variáveis qualitativas como fator
# Criar uma variável chamada UF_res a partir da variável CO_MUN_RES
# Restringir o período da base - apenas dados de inicio de sintomas de jan a abril/2023
# Exportar a base transformada




#### A função group_by
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



##Passo 2:  utilizar o comando summarise após o group_by() para fazer um resumo dos dados com a função n()

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

#Calcular/comparar a idade média, mediana para a variável Tosse (teve ou não teve sintoma de tosse):

