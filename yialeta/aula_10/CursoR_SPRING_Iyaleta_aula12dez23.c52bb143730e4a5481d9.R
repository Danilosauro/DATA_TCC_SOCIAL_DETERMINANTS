

# Curso Spring Iyaleta
# Introdução ao Software R: Análise de dados secundários em estudos epidemiológicos


## AULA 12/12/23 ##


### Verificar se todos estão com as bases de dados que será utilizada no trabalho prático


# Carregar pacote pacman (para instalar e carregar pacotes)
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



### Gramática dos gráficos - GGPLOT2

# gráficos naturalmente mais bonitos;
# fácil personalização (mais simples deixar o gráfico do jeito que você quer);
# a estrutura padronizada das funções deixa o aprendizado mais intuitivo;
# a diferença no código entre tipos diferentes de gráficos é muito pequena.
# gráficos são construídos camada por camada


# ESTRUTURA GERAL DE CONFIGURAÇÃO DE UM OBJETO GGPLOT2 ####

# Precisamos de três elementos essenciais: função + estética + geometria:


# 1) Função - ggplot()
# Observe que se rodarmos apenas a função ggplot(), obteremos um painel em branco.


ggplot(data = base_exemplo)

ggplot(base_exemplo)


#Apesar de termos passado os dados para a função, precisamos especificar como as observações
# serão mapeadas nos aspectos visuais do gráfico e quais formas geométricas serão utilizadas para isso.
# as camadas são unidas com um +; Não confundir com o %>%



# 2) Estética -  aes(x =, y = )

#estética dos objetos geométricos e estatísticos, como a posição, a cor, o tamanho, a forma e a transparência

class(base_exemplo$dtsint)

ggplot(data = base_exemplo, 
       aes(x = dtsint)) 


# 3) Geometria - geom_bar(), geom_histogram(), geom_point(), geom_line()
# formas geométricas que representarão os dados; point, col, histogram, line, bar, etc


ggplot(data = base_exemplo, 
       aes(x = dtsint)) +
  geom_histogram()

#ou

ggplot(data = base_exemplo) +
  geom_histogram(aes(x = dtsint))


# O QUE SÃO ESTÉTICAS ####

# Estética é diferente da aparência do gráfico, é como as variáveis são mapeadas no gráfico 
# A aparência do gráfico é fornecida pelos atributos da função
# A estética é como o sistema de coordenadas do gráfico - propriedades visuais
# Especificamos os dados fora de aes() 
# o Warning nos avisa sobre a exclusão das observações que possuem NA na variável 


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


### agora coloriu

ggplot(data = base_exemplo2) +
  geom_line(aes(x = dtsint, 
                y = casos), 
                color = "#fdbb84")




# Argumento fill dentro da função estética 
ggplot(data = base_exemplo2) +
  geom_line(aes(x = dtsint, 
                y = casos, 
                fill = "dtsint"))


# O argumento fill não serve para gráfico de linhas, 

# precisamos utilizar outro tipo de geometria
ggplot(data = base_exemplo) +
  geom_histogram(aes(x = dtsint, 
                     fill = "red"))

ggplot(data = base_exemplo) +
  geom_histogram(aes(x = dtsint), 
                 fill = "red")


# x, y, alpha, colour, fill, group, shape, size and stroke

# O QUE SÃO GEOMETRIAS ####

# Para variáveis quantitativas temos algumas opções: 

# Diagrama de caixas ou boxplots ####

# Boxplot é uma das maneiras mais simples de apresentar uma variável quantitativa

# Plotar somente uma variável numérica
ggplot(data = base_exemplo) +
  geom_boxplot(aes(y = idade),
               color="red")

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
               color = "violetred1", 
               fill = "moccasin")


# Para verificar o nome de todas as cores dos argumentos fill e color
colours()


# Adicionar transparência ao gráfico - argumento alpha 
ggplot(data = base_exemplo) +
  geom_boxplot(aes(x = sexo,
                   y = idade), 
               fill = "red", 
               alpha = 0.2)


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
            linewidth = 0.8,
            color = "red")



## gráfico para variáveis categóricas


# Para variáveis categóricas temos algumas opções: 
# geom_bar
# geom_col


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



####### Parei aqui  --- inicio da aula 12/12 ########


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




### Outros gráficos para variáveis quantitativas 

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



# ### FACETAS: FACET_WRAP ####

##  divide um gráfico em uma matriz de painéis. Cada painel mostra um subconjunto diferente dos dados


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



## Declarar as cores de diferentes formas ####
# Para obter a lista completa de cores do R
colors()

# Declarar a cor "deepskyblue2" para as caixas e "black" para o contorno

ggplot(data = base_exemplo) +
  geom_histogram(aes(x = dtsint),
                 fill  = "deepskyblue2",
                 color = "black",
                 stat  = "count")

# Declarar um código hexadecimal para colorir as caixas
ggplot(data = base_exemplo) +
  geom_histogram(aes(x = dtsint),
                 fill  = "#01665e",
                 color = "black",
                 stat  = "count")

# TÍTULOS E RÓTULOS #### 

ggplot(data = base_exemplo) + 
  geom_histogram(aes(x = dtsint), 
                 stat = "count") 

# Função labs() para títulos, subtítulos, caption e rótulos dos eixos
ggplot(data = base_exemplo) +
  geom_histogram(aes(x = dtsint),
                 stat = "count") +
  labs(title    = "Síndrome respiratória aguda grave",
       subtitle = "Curva epidêmica do Brasil em 2023",
       x = "Data de início dos sintomas",
       y = "Nº de hospitalizações", 
       caption = "Dados atualizados em 16/10/2023")


# Você não precisa informar todos os argumentos de labs()
ggplot(data = base_exemplo) +
  geom_histogram(aes(x = dtsint),
                 stat = "count") +
  labs(title    = "Síndrome respiratória aguda grave",
       x = "Data de início dos sintomas",
       y = "Nº de hospitalizações")


# Texto dinâmico para data de atualização
# Data de confecção do gráfico 

ggplot(data = base_exemplo) +
  geom_histogram(aes(x = d1_epi_sem_ano),
                 stat = "count") +
  labs(title    = "Síndrome respiratória aguda grave",
       x = "Data de início dos sintomas",
       y = "Nº de hospitalizações",
       caption = str_glue("Gráfico elaborado em {today()}"))


# Função labs() para nomear legenda

plot1 <-  ggplot(data = base_exemplo) +
  geom_histogram(aes(x = dtsint,
                     fill=sexo),
                 stat = "count") +
  labs(title    = "Síndrome respiratória aguda grave",
       x = "Data de início dos sintomas",
       y = "Nº de hospitalizações",
       caption = "Fonte: SIVEP-Gripe (16/010/2023)", 
       fill = "Sexo")



# Visualizar o objeto ggplot armazenado

plot1


# TEMAS COMPLETOS  ####
# Tema clássico
plot1 +
  theme_classic()

# Tema mínimo
plot1 +  theme_minimal()

# Tema leve
plot1 +  theme_void()

# Tema escuro
plot1 +  theme_dark()

#Tema cinza
plot1 +  theme_grey()

# Outros temas
plot1 +  theme_bw()

plot1 +  theme_linedraw()

plot1 +  theme_test()


# TEMAS AJUSTADOS MANUALMENTE  ####

# Omitir elementos do gráfico #### 

# element_blank() para retirar as linhas de grade secundárias
plot1+
  theme(panel.grid.minor = element_blank())

# element_blank() para retirar as linhas de grade principais
plot1+
  theme(panel.grid.major = element_blank())

# element_blank() para retirar todas as linhas de grade 
plot1+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

plot1+
  theme(
    panel.grid = element_blank())

# element_blank() para retirar todo o painel de fundo do gráfico
plot1+
  theme(panel.background = element_blank())

# Editar linhas  ####
# element_line() para definir linhas de grade principais em preto e em tamanho 1
plot1+
  theme(
    panel.grid.major = element_line(color = "black", size = 1),
    panel.grid.minor = element_blank())

# element_line() para definir linhas de grade secundárias em cinza e em tamanho 0.2
plot1+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(color = "grey", size = 0.2))

# element_line() para definir linhas de grade diferentes em x e y
plot1+
  theme(
    panel.grid.major.y = element_line(color = "grey", size = 0.2),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank())


# element_line() para definir linhas dos eixos e ticks  
plot1+
  theme(
    axis.line  = element_line(color = "black"), 
    axis.ticks = element_line(color = "black", size = 1))

# 7.3. Editar textos  ####
# Argumentos size, color e face de element_text() 
plot1+
  theme(
    plot.title = element_text(size = 20,           # título
                              color = "red"),      
    plot.subtitle = element_text(face = "italic"), # subtítulo
    plot.caption = element_text(family = "serif",  # caption 
                                color = "grey"))

# Argumentos hjust, size e family de element_text()
plot1+
  theme(
    axis.title = element_text(hjust = 1, 
                              size=8,
                              family = "mono"))

plot1+
  theme(
    axis.title = element_text(hjust = 0, 
                              size=8))

# Argumento angle de element_text()
plot1+
  theme(
    axis.text.x = element_text(angle=90))

plot1+
  theme(
    axis.text.x = element_text(angle=-90))

# Editar legenda com element_text()
plot1+
  theme(
    legend.text = element_text(hjust = 0.5),
    legend.title = element_text(hjust= 0.5))


# Elemento title para edição de todos os títulos
plot1+
  theme(
    title = element_text(size = 14, 
                         face = "bold"))

# Editar retângulos ####

# tamanho e tipo de linha da borda do painel
plot1+
  theme(panel.background = element_rect(fill = "#fef0d9", 
                                        colour = "black",
                                        linewidth = 1, 
                                        linetype = "dashed"))

# element_rect() para alterar cor de fundo da plotagem
plot1+
  theme(plot.background = element_rect(fill = "#fef0d9"))

# sem cor de fundo no painel e na legenda, com cor de fundo na plotagem  
plot1+
  theme(plot.background   = element_rect(fill = "#fef0d9"),
        panel.background  = element_blank(),
        legend.background = element_blank())

# element_rect() para cor de fundo da legenda
plot1+
  theme(legend.key = element_rect(fill="red"))


#  Legenda ####
# Reposicionamento de legenda no gráfico
plot1 +
  theme(legend.position = "bottom")

# Margens ####
# Editar margens
plot1 +
  theme(plot.margin = margin(t=20, # Superior (Top)
                             r=2,  # Direita  (Right)
                             b=2,  # Inferior (Bottom)
                             l=2)) # Esquerda (Left)




###  configuração do gráfico com vários ajustes feitos ao memso tempo


plot1 <-  ggplot(data = base_exemplo) +
  geom_histogram(aes(x = dtsint,
                     fill=sexo),
                 stat = "count") +
  labs(title    = "Síndrome respiratória aguda grave",
       x = "Data de início dos sintomas",
       y = "Nº de hospitalizações",
       caption = "Fonte: SIVEP-Gripe (16/010/2023)", 
       fill = "Sexo")  +
  theme_classic()+
  theme(legend.position = "bottom",legend.box = "vertical")+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))

plot1


## salvar a figura 


#  pdf
ggsave("plot1.pdf")
ggsave("plot1.pdf", width = 10, height = 7)


#  png
ggsave("plot1.png")
ggsave("plot1.png", width = 10, height = 7)
