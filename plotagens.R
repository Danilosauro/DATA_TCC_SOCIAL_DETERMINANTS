### plotagens inerentes aos estados aids

ggplot(piaui, aes(x = ano, y = QUANTIDADE_CASOS_AIDS, fill = UF)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = 'grey') +
  labs(title = "Quantidade de Casos de AIDS por ano no estado",
       x = "Ano",
       y = "Quantidade de Casos") +
  theme_minimal() + 
  theme(legend.position = "right")

### gráfico da quantidade de casos de aids por estado em cada ano

paleta_cores <- brewer.pal(n = 9, name = "Set1")
ggplot(painel_data, aes(x = ano, y = QUANTIDADE_CASOS_AIDS, fill = UF)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = paleta_cores) +
  labs(title = "Quantidade de Casos de AIDS por estado em Cada Ano",
       x = "Ano",
       y = "Quantidade de Casos") +
  theme_minimal() + 
  theme(legend.position = "right") 

ggsave("aids_estado_ano.jpg", width = 10, height = 7)

### gráfico da quantidade de casos de sifilis por estado em cada ano

paleta_cores <- brewer.pal(n = 9, name = "Set1") 
ggplot(painel_data, aes(x = ano, y = QUANTIDADE_CASOS_SIFILIS, fill = UF)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = paleta_cores) +
  labs(title = "Quantidade de Casos de SIFILIS por estado em Cada Ano",
       x = "Ano",
       y = "Quantidade de Casos") +
  theme_minimal() + 
  theme(legend.position = "right")

ggsave("sifilis_estado_ano.jpg", width = 10, height = 7) 


#### avaliações individualizadas por estado --- determinantes
ggplot(alagoas, aes(x = MEDIA_ANOS_ESTUDO, y = RENDA_PER_CAPITA)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relação entre Renda Per Capita e Escolaridade Média",
       x = "Escolaridade média",
       y = "Renda per capita") 


#### avaliações individualizadas por estado --- escolaridade ~ SIFILIS
ggplot(bahia, aes(x = MEDIA_ANOS_ESTUDO, y = QUANTIDADE_CASOS_SIFILIS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relação entre média de anos de estudo e casos de Sífilis adquirida",
       x = "Escolaridade média",
       y = "Notificações referentes a Sífilis adquirida")  


### avaliações invidualizadas por estado --- Renda per capita ~  SIFILIS 

ggplot(ceara, aes(x = RENDA_PER_CAPITA, y = QUANTIDADE_CASOS_SIFILIS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relação entre Renda per capita e casos de Sífilis adquirida",
       x = "Renda per capita",
       y = "Notificações referentes a Sífilis adquirida")

### avaliações invidualizadas por estado --- Esperança de vida ao nascer ~  SIFILIS

ggplot(maranhao, aes(x = ESPERANCA_VIDA_NASC, y = QUANTIDADE_CASOS_SIFILIS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relação entre esperança de vida ao nascer e casos de Sífilis adquirida",
       x = "Esperança de vida ao nascer",
       y = "Notificações referentes a Sífilis adquirida") 


#### avaliações individualizadas por estado --- escolaridade ~ AIDS
ggplot(alagoas, aes(x = MEDIA_ANOS_ESTUDO, y = QUANTIDADE_CASOS_AIDS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relação entre média de anos de estudo e casos de HIV-Aids",
       x = "Escolaridade média",
       y = "Notificações referentes a HIV_Aids")


#### avaliações individualizadas por estado --- renda per capita ~ AIDS
ggplot(alagoas, aes(x = RENDA_PER_CAPITA, y = QUANTIDADE_CASOS_AIDS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relação entre Renda per capita e casos de HIV_Aids",
       x = "Renda per capita",
       y = "Notificações referentes a HIV_Aids") 

#### avaliações individualizadas por estado --- renda per capita ~ AIDS
ggplot(alagoas, aes(x = RENDA_PER_CAPITA, y = QUANTIDADE_CASOS_AIDS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relação entre Renda per capita e casos de HIV_Aids",
       x = "Renda per capita",
       y = "Notificações referentes a HIV_Aids")

#### avaliações individualizadas por estado --- esperança de vida ao nascer ~ AIDS
ggplot(alagoas, aes(x = RENDA_PER_CAPITA, y = QUANTIDADE_CASOS_AIDS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relação entre Renda per capita e casos de HIV_Aids",
       x = "Renda per capita",
       y = "Notificações referentes a HIV_Aids")

### avaliações generalizadas painel_data 

ggplot(painel_data, aes(x = MEDIA_ANOS_ESTUDO, y = QUANTIDADE_CASOS_SIFILIS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relação entre média de anos de estudo e casos notificados de Sífilis adquirida",
       x = "Escolaridade média",
       y = "Notificações referentes a Sífilis adquirida") 


ggplot(painel_data, aes(x = RENDA_PER_CAPITA, y = QUANTIDADE_CASOS_SIFILIS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relação entre Renda per capita e casos notificados de Sífilis adquirida",
       x = "Renda per capita",
       y = "Notificações referentes a Sífilis adquirida") 

ggplot(painel_data, aes(x = ESPERANCA_VIDA_NASC, y = QUANTIDADE_CASOS_SIFILIS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relação entre Esperança de vida ao nascer e casos notificados de Sífilis adquirida",
       x = "Esperança de vida ao nascer",
       y = "Notificações referentes a Sífilis adquirida")




## tabelas
painel_data %>%
  tbl_summary(include = c(RENDA_PER_CAPITA, ESPERANCA_VIDA_NASC, MEDIA_ANOS_ESTUDO, QUANTIDADE_CASOS_AIDS, QUANTIDADE_CASOS_SIFILIS)) %>% 
  modify_header(label = "**Variable**")

painel_data %>%
  tbl_summary(UF) 

library(gt)

painel_data %>% gt()

# plots resíduos modelo generalizado (toda a região nordeste) 
library(ggplot2)
library(broom)

residuos_sifilis <- residuals(modelo_sifilis)

dados_residuos_sifilis <- data.frame(RENDA_PER_CAPITA = painel_data$RENDA_PER_CAPITA,
                                     Residuos = residuos_sifilis)

ggplot(dados_residuos_sifilis, aes(x = RENDA_PER_CAPITA, y = Residuos)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Dispersão entre Renda Per Capita e Resíduos da Sífilis",
       x = "Renda Per Capita",
       y = "Resíduos")


######## ------ PLOTAGENS DOS RESÍDUOS PELOS DETERMINANTES SOCIAIS -------- ###########

resultados <- list(
  Alagoas = resultados_por_estado$Alagoas,
  Bahia = resultados_por_estado$Bahia,
  Ceará = resultados_por_estado$Ceará,
  Maranhão = resultados_por_estado$Maranhão,
  Paraíba = resultados_por_estado$Paraíba,
  Pernambuco = resultados_por_estado$Pernambuco,
  Piauí = resultados_por_estado$Piauí,
  `Rio Grande do Norte` = resultados_por_estado$`Rio Grande do Norte`,
  Sergipe = resultados_por_estado$Sergipe
)


for (estado in names(resultados_por_estado)) {
  
  dados_aumentados <- augment(resultados_por_estado[[estado]])
  
  
  ggplot(dados_aumentados, aes(x = RENDA_PER_CAPITA, y = .resid)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, col = "red") +
    labs(title = paste("Dispersão entre Renda Per Capita e Resíduos -", estado),
         x = "Renda Per Capita",
         y = "Resíduos")
  
  ggsave(paste("/GRAFICOS/rpc_x_residuos_estado", estado, ".png", sep = ""), plot = last_plot())
  
  ggplot(dados_aumentados, aes(x= MEDIA_ANOS_ESTUDO, y= .resid)) + 
    geom_point()+
    geom_smooth(method = "lm", se = FALSE, col = "red") +
    labs(title = paste("Dispersão entre Escolaridade média e Resíduos -", estado),
         x = "Escolaridade média",
         y = "Resíduos")
  
  ggsave(paste("/GRAFICOS/escolaridade_media_x_residuos_estado", estado, ".png", sep = ""), plot = last_plot())
  
  ggplot(dados_aumentados, aes(x= ESPERANCA_VIDA_NASC, y= .resid)) + 
    geom_point()+
    geom_smooth(method = "lm", se = FALSE, col = "red") +
    labs(title = paste("Dispersão entre Esperança de vida ao nascer e Resíduos -", estado),
         x = "Esperança de vida ao nascer",
         y = "Resíduos")
  
  ggsave(paste("/GRAFICOS/esperanca_vida_nasc_x_residuos_estado", estado, ".png", sep = ""), plot = last_plot())
  
}