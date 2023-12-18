## modelos linear  ~ glm 
model <- lm(QUANTIDADE_CASOS_AIDS ~ MEDIA_ANOS_ESTUDO, data = alagoas) 
summary(model)

# modelo linear múltiplo  ~ glm 
modelo_regressao_multipla_interacoes <- lm(QUANTIDADE_CASOS_AIDS ~  MEDIA_ANOS_ESTUDO  + RENDA_PER_CAPITA + ESPERANCA_VIDA_NASC, data = maranhao)
summary(modelo_regressao_multipla_interacoes) 

modelo_regressao_multipla_interacoes_all <- lm(QUANTIDADE_CASOS_SIFILIS ~ RENDA_PER_CAPITA * UF + MEDIA_ANOS_ESTUDO * UF + ESPERANCA_VIDA_NASC * UF, data = painel_data)
summary(modelo_regressao_multipla_interacoes_all) 


# Ajuste modelo de regressão por dados em painel por efeitos fixos

modelo_sifilis <- plm(QUANTIDADE_CASOS_SIFILIS ~ RENDA_PER_CAPITA + MEDIA_ANOS_ESTUDO + ESPERANCA_VIDA_NASC, data = painel_data, model="within")
summary(modelo_sifilis) 

summary(fixef(modelo_sifilis)) 


modelo_aids <- plm(QUANTIDADE_CASOS_AIDS ~ RENDA_PER_CAPITA + MEDIA_ANOS_ESTUDO + ESPERANCA_VIDA_NASC, data = painel_data, model = 'within') 
summary(modelo_aids) 

summary(fixef(modelo_sifilis))
