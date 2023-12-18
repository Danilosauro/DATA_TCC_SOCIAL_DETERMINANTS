### plotagens inerentes aos estados 

ggplot(piaui, aes(x = ano, y = QUANTIDADE_CASOS_AIDS, fill = UF)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = paleta_cores) +
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
ggplot(alagoas, aes(x = MEDIA_ANOS_ESTUDO, y = QUANTIDADE_CASOS_SIFILIS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relação entre média de anos de estudo e casos de Sífilis adquirida",
       x = "Escolaridade média",
       y = "Notificações referentes a Sífilis adquirida") 

### avaliações invidualizadas por estado --- Renda per capita ~  SIFILIS 

ggplot(alagoas, aes(x = RENDA_PER_CAPITA, y = QUANTIDADE_CASOS_SIFILIS)) +
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

## tabelas
painel_data %>%
  tbl_summary(include = c(RENDA_PER_CAPITA, ESPERANCA_VIDA_NASC, MEDIA_ANOS_ESTUDO, QUANTIDADE_CASOS_AIDS, QUANTIDADE_CASOS_SIFILIS)) %>% 
  modify_header(label = "**Variable**")

painel_data %>%
  tbl_summary(UF)
