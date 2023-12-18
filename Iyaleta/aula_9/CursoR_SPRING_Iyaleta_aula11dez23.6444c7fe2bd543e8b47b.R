

# Curso Spring Iyaleta
# Introdução ao Software R: Análise de dados secundários em estudos epidemiológicos


## AULA 11/12/23 ##


### Verificar se todos estão com as bases de dados que será utilizada no trabalho prático


#Carregar pacote pacman (para instalar e carregar pacotes)
library(pacman)

pacman::p_load("rio", 
               "tidyverse",
               "lubridate")


# Diretório de trabalho ####

setwd("D:\\Curso_R_Spring_Iyaleta") 
setwd("D:/Curso_R_Spring_Iyaleta")

##importar a base2
base2 <- import("base2.xlsx")

glimpse(base2)


## criar variáveis

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

base_exemplo <- base_exemplo %>% 
  mutate(sexo = factor(CS_SEXO, 
                       levels = c("F", "M", "I"),
                       labels = c("Feminino", "Masculino", "Ignorado")))


base_exemplo <- base_exemplo %>% 
  mutate(tosse = factor(TOSSE, 
                       levels = c(1, 2, 9),
                       labels = c("Sim", "Não", "Ignorado")))



# ESTRUTURA GERAL DE CONFIGURAÇÃO DE UM OBJETO GGPLOT2 ####

# Precisamos de três elementos essenciais: função + estética + geometria:

# 1) Função - ggplot()

ggplot(data = base_exemplo)

ggplot(base_exemplo)


# 2) Estética -  aes(x =, y = )

class(base_exemplo$dtsint)

ggplot(data = base_exemplo, 
       aes(x = dtsint)) 


# 3) Geometria - geom_bar(), geom_histogram(), geom_point(), geom_line()

ggplot(data = base_exemplo) +
  geom_histogram(aes(x = dtsint))


# O QUE SÃO ESTÉTICAS ####

# Estética é diferente da aparência do gráfico, é como as variáveis são mapeadas no gráfico 
# A aparência do gráfico é fornecida pelos atributos da função
# A estética é como o sistema de coordenadas do gráfico - propriedades visuais

# A base de dados, a estética e geometria podem ser declaradas juntas, fora da função ggplot()
ggplot() +
  geom_histogram(data = base_exemplo,
                 aes(x = dtsint))

# A função estética pode ser declarada dentro da função ggplot
ggplot(data = base_exemplo, aes(x = dtsint)) +
  geom_histogram()

# A função estética por ser declarada dentro da geometria 
ggplot(data = base_exemplo) +
  geom_histogram(aes(x = dtsint))

# Algumas geometrias exigem que os argumentos x e y da estética sejam declarados
base_exemplo2 <- base_exemplo %>%
  count(dtsint) %>%
  rename(casos = n)

ggplot(data = base_exemplo2) +
  geom_line(aes(x = dtsint, 
                y = casos))

# Argumento color ou colour dentro da função estética 
ggplot(data = base_exemplo2) +
  geom_line(aes(x = dtsint, 
                y = casos, 
                color = "blue"))

# Argumento fill dentro da função estética 
ggplot(data = base_exemplo2) +
  geom_line(aes(x = dtsint, 
                y = casos, 
                fill = "blue"))

# O argumento fill não serve para gráfico de linhas, 
# precisamos utilizar outro tipo de geometria
ggplot(data = base_exemplo) +
  geom_histogram(aes(x = dtsint, 
                     fill = "blue"))



# x, y, alpha, colour, fill, group, shape, size and stroke

# O QUE SÃO GEOMETRIAS ####

# Para variáveis quantitativas temos algumas opções: 

# Diagrama de caixas ou boxplots ####

# Boxplot é uma das maneiras mais simples de apresentar uma variável quantitativa

# Plotar somente uma variável numérica
ggplot(data = base_exemplo) +
  geom_boxplot(aes(y = idade))

# Plotar uma variável numérica e outra categórica
ggplot(data = base_exemplo) +
  geom_boxplot(aes(x = sexo, 
                   y = idade))

# Adicionar contorno ao gráfico - argumento color ou colour fora da estética
ggplot(data = base_exemplo) +
  geom_boxplot(aes(x = sexo, 
                   y = idade), 
               color = "red")


# Adicionar preenchimento ao gráfico - argumento fill
ggplot(data = base_exemplo) +
  geom_boxplot(aes(x = sexo, 
                   y = idade), 
               fill = "red")


# Adicionar contorno e preenchimento ao gráfico - argumentos color e fill
ggplot(data = base_exemplo) +
  geom_boxplot(aes(x = sexo, 
                   y = idade), 
               color = "blue", 
               fill = "green")


# Para verificar o nome de todas as cores dos argumentos fill e color
colours()

# Adicionar transparência ao gráfico - argumento alpha 
ggplot(data = base_exemplo) +
  geom_boxplot(aes(x = sexo,
                   y = idade), 
               fill = "red", 
               alpha = 0.5)

# Mudar a orientação do gráfico
ggplot(data = base_exemplo) +
  geom_boxplot(aes(x = idade, 
                   y = sexo), 
               fill = "red")

# Histograma ####
ggplot(data = base_exemplo) +
  geom_histogram(aes(x = dtsint)) 

# Declarar o argumento stat 
ggplot(data = base_exemplo) +
  geom_histogram(aes(x = dtsint), 
                 stat = "count") 

# Adicionar contorno ao gráfico 
ggplot(data = base_exemplo) +
  geom_histogram(aes(x = dtsint), 
                 stat = "count", 
                 color = "darkred") 

# Adicionar preenchimento ao gráfico
ggplot(data = base_exemplo) +
  geom_histogram(aes(x = dtsint), 
                 stat = "count", 
                 fill = "darkred") 

# Apresentar o histograma por sexo - argumento fill dentro da estética
ggplot(data = base_exemplo) +
  geom_histogram(aes(x = dtsint, 
                     fill = sexo), 
                 stat = "count") 

# Se declararmos o argumento fill dentro da geometria 
ggplot(data = base_exemplo) +
  geom_histogram(aes(x = dtsint, 
                     fill = sexo), 
                 fill = "red", 
                 stat = "count") 

# Gráfico de linhas ####
ggplot(data = base_exemplo) +
  geom_line(aes(x = dtsint), 
            stat="count")

# Colocar cor na linha - argumento color 
ggplot(data = base_exemplo) +
  geom_line(aes(x = dtsint), 
            fill = "blue", 
            stat = "count")

ggplot(data = base_exemplo) +
  geom_line(aes(x = dtsint), 
            color = "blue", 
            stat="count")

# Mudar o tipo de linha - argumento linetype
# Solid - sólida
ggplot(data = base_exemplo) +
  geom_line(aes(x = dtsint), 
            stat="count", 
            color = "blue", 
            linetype = "solid")

# Dashed - tracejada
ggplot(data = base_exemplo) +
  geom_line(aes(x = dtsint), 
            stat="count", 
            color = "blue", 
            linetype = "dashed")

# Dotted - pontilhada
ggplot(data = base_exemplo) +
  geom_line(aes(x = dtsint), 
            stat="count", 
            color = "blue", 
            linetype = "dotted")

# Mudar a espessura da linha - argumento size/linewidth
ggplot(data = base_exemplo) +
  geom_line(aes(x = dtsint), 
            stat="count")

ggplot(data = base_exemplo) +
  geom_line(aes(x = dtsint), 
            stat = "count", 
            linewidth = 1)

# Argumento group para apresentar uma variável categórica
ggplot(data = base_exemplo) +
  geom_line(aes(x = dtsint), 
            stat="count")

ggplot(data = base_exemplo) +
  geom_line(aes(x = dtsint, 
                group = sexo), 
            stat="count")

ggplot(data = base_exemplo) +
  geom_line(aes(x = dtsint, 
                group = sexo, 
                color = sexo), 
            stat="count")


# Gráfico de barras ####
# No gráfico de barras a altura das barras é proporcional ao número de registros

ggplot(data = base_exemplo) +
  geom_bar(aes(x = tosse))

# Adicionar preenchimento ao gráfico - argumento fill
ggplot(data = base_exemplo) +
  geom_bar(aes(x = tosse), 
           fill = "darkblue")

# Mudar espessura da barra - argumento width
ggplot(data = base_exemplo) +
  geom_bar(aes(x = tosse), 
           fill = "darkblue", 
           width = 0.6)

# Duas variáveis categóricas
ggplot(data = base_exemplo) +
  geom_bar(aes(x = tosse, 
               fill = sexo))

# Mudar a posição da barra - argumento position

# Barras proporcionais
ggplot(data = base_exemplo) +
  geom_bar(aes(x = tosse, 
               fill = sexo), 
           position = "fill")

# Barras laterais
ggplot(data = base_exemplo) +
  geom_bar(aes(x = tosse, 
               fill = sexo), 
           position = "dodge")

# Barras empilhadas
ggplot(data = base_exemplo) +
  geom_bar(aes(x = tosse, 
               fill = sexo), 
           position = "stack")

# Dados agregados - argumento stat
base_exemplo_idade <- base_exemplo %>%
  count(faixa_etaria)

ggplot(data = base_exemplo_idade) +
  geom_bar(aes(x = faixa_etaria))

ggplot(data = base_exemplo_idade) +
  geom_bar(aes(x = faixa_etaria, 
               y = n), 
           stat = "identity")

ggplot(data = base_exemplo_idade) +
  geom_bar(aes(y = faixa_etaria, 
               x = n), 
           stat = "identity")


# Para variáveis categóricas temos algumas opções: 
geom_bar
geom_col


# Gráfico de colunas ####

# No gráfico de colunas utilizando geom_col(), ambos os argumentos, y e x, são necessários
# Precisa agregar os dados

ggplot(data = base_exemplo) +
  geom_col(aes(x = tosse))

# Agrupar os dados 
base_exemplo3 <- base_exemplo %>% 
  count(tosse)

# Adicionar o argumento y 
ggplot(data = base_exemplo3) +
  geom_col(aes(x = tosse, y = n))

ggplot(data = base_exemplo3) +
  geom_col(aes(x = tosse, y = n), 
           fill = "darkgreen")



# Gráfico de densidade ####
ggplot(data = base_exemplo) +
  geom_density(aes(x = idade))

# Filtrar idade menor que 105
base_exemplo <- base_exemplo %>% 
  filter(idade <105)

ggplot(data = base_exemplo) +
  geom_density(aes(x = idade))

# Adicionar contorno e preenchimento
ggplot(data = base_exemplo) +
  geom_density(aes(x = idade), 
               color="darkblue", 
               fill="lightblue")

ggplot(data = base_exemplo) +
  geom_density(aes(x = idade, 
                   color= sexo, 
                   fill = sexo), 
               alpha = 0.5)

# FACETAS: FACET_WRAP ####
ggplot(data = base_exemplo) +
  geom_bar(aes(x = tosse), 
           fill = "darkblue") +
  facet_wrap(~sexo)

# Alterar o número de colunas
ggplot(data = base_exemplo) +
  geom_bar(aes(x = tosse), 
           fill = "darkred") +
  facet_wrap(~sexo, 
             ncol = 1)

# Alterar a escala
ggplot(data = base_exemplo) +
  geom_bar(aes(x = tosse), 
           fill = "darkred") +
  facet_wrap(~ sexo, 
             ncol = 1, 
             scales = "free_y")



#### Scatterplot - duas variáveis contínuas

# Dataset chamado iris é uma base de dados nativa do R

head(iris)


ggplot(data = iris) +
geom_point(aes(x=Sepal.Length, y=Sepal.Width))

#ou 

ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + 
  geom_point(color="orange")

  
##colorido
  
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + 
  geom_point(color="blue")


## mudar a forma das bolas

ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + 
  geom_point(color="magenta", shape=22)
