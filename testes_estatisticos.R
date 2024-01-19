## avaliações em painel


## modelos linear plm por estado 

lista_estados <- list(alagoas, bahia, ceara, maranhao, paraiba, pernambuco, piaui, rn, sergipe)

## aids 

generate_panel_data_model_aids <- function(estado){
  for(i in lista_estados){
    modelo_estado <- plm(QUANTIDADE_CASOS_AIDS ~ RENDA_PER_CAPITA + MEDIA_ANOS_ESTUDO + ESPERANCA_VIDA_NASC, data = estado, model = 'within')
  }
  return(summary(modelo_estado))
} 

generate_panel_data_model_aids(alagoas) 
generate_panel_data_model_aids(bahia)
generate_panel_data_model_aids(ceara) 
generate_panel_data_model_aids(maranhao)
generate_panel_data_model_aids(paraiba)
generate_panel_data__model_aids(piaui)
generate_panel_data__model_aids(pernambuco)
generate_panel_data_model_aids(rn)
generate_panel_data__model_aids(sergipe)

## sifilis 

generate_panel_data_model_sifilis <- function(estado){
  for(i in lista_estados){
    modelo_estado <- plm(QUANTIDADE_CASOS_SIFILIS ~ RENDA_PER_CAPITA + MEDIA_ANOS_ESTUDO + ESPERANCA_VIDA_NASC, data = estado, model = 'within')
  }
  return(summary(modelo_estado))
} 

generate_panel_data_model_sifilis(alagoas) 
generate_panel_data_model_sifilis(bahia)
generate_panel_data_model_sifilis(ceara) 
generate_panel_data_model_sifilis(maranhao)
generate_panel_data_model_sifilis(paraiba)
generate_panel_data_model_sifilis(piaui)
generate_panel_data_model_sifilis(pernambuco)
generate_panel_data_model_sifilis(rn)
generate_panel_data_model_sifilis(sergipe)


# Ajuste modelo de regressão por dados em painel por efeitos fixos para o dataset completo

modelo_sifilis <- plm(QUANTIDADE_CASOS_SIFILIS ~ RENDA_PER_CAPITA + MEDIA_ANOS_ESTUDO + ESPERANCA_VIDA_NASC, data = painel_data, model="within")
summary(modelo_sifilis) 

summary(fixef(modelo_sifilis)) 


modelo_aids <- plm(QUANTIDADE_CASOS_AIDS ~ RENDA_PER_CAPITA + MEDIA_ANOS_ESTUDO + ESPERANCA_VIDA_NASC, data = painel_data, model = 'within') 
summary(modelo_aids) 

summary(fixef(modelo_sifilis))