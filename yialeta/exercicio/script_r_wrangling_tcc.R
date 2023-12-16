##### Prática



# selecione variáveis de interesse

# Exclua duas variáveis do banco

# mudar o nome de alguma variável ---escolha o nome que desejar

# frequência por unidade da federação de notificação (SG_UF_NOT)

# criar uma variável sexo do tipo fator usando o case_when

# Crie um banco de dados apenas com informações de pessoas do sexo feminino com idade entre 18 e 59 anos

# faça análise descritiva desse novo banco

# utilize o group_by por sexo e o summarise ccom a função median para calcular a mediana da idade por grupo


path = getwd() 
setwd(path)

pacman::p_load(rio,         # Importação e exportação de dados
               tidyverse,   # Manipulação e tratamento de dados     
               lubridate,   # Manipulação de variáveis do tipo data
               abjutils) 
## Ler de novo o dataset chamado de "base"
base <- import("base.xlsx") 

# selecione variáveis de interesse
base_selecionada <- base %>% 
  select(CS_SEXO, CS_RACA, idade) 

# Exclua duas variáveis do banco 
base_com_exclusao <- base %>% 
  select(-dtsint, -dtnasc) 

# mudar o nome de alguma variável ---escolha o nome que desejar 

base_selecionada <- base_selecionada %>% 
  rename('SEXO'='CS_SEXO','RACA'= 'CS_RACA','IDADE' = 'idade') 

# frequência por unidade da federação de notificação (SG_UF_NOT)

base_freq_uf <- base %>% 
  count(SG_UF_NOT, sort = T) 

# criar uma variável sexo do tipo fator usando o case_when 

base_selecionada <- base_selecionada %>%
  mutate(SEXO = case_when(SEXO == 'M' ~ 'MASCULINO', 
                          SEXO == 'F' ~ 'FEMININO', 
                          SEXO == 'I' ~ 'IGNORADO'), 
         SEXO = factor(SEXO))

# Crie um banco de dados apenas com informações de pessoas do sexo feminino com idade entre 18 e 59 anos 

db_filtrado <- base %>% 
  filter(CS_SEXO == 'F' & idade >= 18 & idade <=59) 

# faça análise descritiva desse novo banco 
glimpse(db_filtrado)
summary(db_filtrado)

#utilize o group_by por sexo e o summarise ccom a função median para calcular a mediana da idade por grupo 

grouped_base  <- base %>% 
  group_by(CS_SEXO) 

grouped_base <- grouped_base %>% 
  summarise(mediana = median(idade))

rm(grouped_base)
