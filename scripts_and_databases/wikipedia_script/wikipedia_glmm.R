
rm(list=ls())
choose.dir()
setwd("C:\\Users\\joaov\\OneDrive\\Área de Trabalho\\Trabalho\\scripts_and_databases\\wikipedia_script")

#Accessing necessary packages
library(pageviews)
library(tidyverse)
library(ggpubr)
library(trend)
library(Kendall)
library(MuMIn)

#Create an object with Wikipedia's URLs
articles_url1 <-
  c(
    "Aliança_Climática_dos_Estados_Unidos",
    "Antropoceno",
    "Blockade_Australia",
    "Carbono_Social",
    "Causas_do_aquecimento_global",
    "Circulação_meridional_de_capotamento_do_Atlântico",
    "Climate_Change_Science_Program",
    "Comércio_de_eletricidade",
    "Comércio_de_emissões_de_carbono",
    "Crise_climática",
    "Curva_de_Keeling",
    "Declaração_de_Malé_sobre_a_Dimensão_Humana_das_Mudanças_Climáticas_Globais",
    "Desertificação",
    "EERE",
    "Efeito_Callendar",
    "Enchente_de_maré",
    "Escurecimento_global",
    "Esfriamento_global",
    "Estágios_isotópicos_de_oxigênio_marinho",
    "Joan_Feynman",
    "Financiamento_climático",
    "Financiamento_de_carbono",
    "Flygskam",
    "Fronteiras_planetárias",
    "Fuga_de_carbono",
    "Green_New_Deal",
    "Grupo_C40_de_Grandes_Cidades_para_a_Liderança_Climática",
    "Hipótese_de_Iris",
    "História_da_ciência_das_alterações_climáticas",
    "Ilha_de_calor",
    "Índice_de_aridez",
    "Injeção_de_aerossol_na_estratosfera",
    "Interrupção_da_circulação_termoalina",
    "Invasão_em_massa_de_ursos_polares_russos_em_2019",
    "Justiça_climática",
    "Mitigação_das_mudanças_climáticas",
    "Modelo_climático",
    "Modelo_de_Circulação_Geral",
    "Mudança_do_clima",
    "Mudança_global",
    "Mudanças_climáticas_e_agricultura",
    "Mudanças_climáticas_e_biodiversidade",
    "Mudanças_climáticas_e_crianças",
    "Mudanças_climáticas_e_género",
    "Mudanças_climáticas_em_2021",
    "Mulheres_e_mudanças_climáticas",
    "Na_Cidade_sem_Meu_Carro",
    "Observatório_Mauna_Loa",
    "Passivo_ambiental",
    "Política_ambiental_do_governo_de_Joe_Biden",
    "Programa_Homem_e_a_Biosfera",
    "Rarefação_da_Camada_de_Ozono",
    "Recuperação_verde",
    "Refugiado_climático",
    "Relatório_Stern",
    "Saída_dos_Estados_Unidos_do_Acordo_de_Paris",
    "Sindemia_global",
    "Sociedade_dos_2.000_watts",
    "Subida_do_nível_do_mar",
    "Svalbard_Global_Seed_Vault",
    "The_Climate_Book",
    "Tripla_crise_planetária",
    "Uunartoq_Qeqertaq",
    "Variação_solar",
    "Adaptação_às_alterações_climáticas",
    "Alfabetização_de_carbono",
    "Cenários_de_alterações_climáticas",
    "Ecoansiedade",
    "Educação_sobre_mudanças_climáticas",
    "Efeitos_das_alterações_climáticas",
    "Ética_climática",
    "Hora_do_Planeta",
    "Movimento_climático",
    "Vulnerabilidade_às_mudanças_climáticas",
    "Impactos_do_aquecimento_global_no_Brasil",
    "Primeiro_Relatório_de_Avaliação_Nacional_sobre_Mudanças_Climáticas",
    "Antônio_Rocha_Magalhães",
    "Carlos_Rittl",
    "Eduardo_Delgado_Assad",
    "Emilio_La_Rovere",
    "Fórum_Brasileiro_de_Mudança_do_Clima",
    "Lista_de_unidades_federativas_do_Brasil_por_emissões_de_dióxido_de_carbono",
    "Mercedes_Bustamante",
    "Modelo_Brasileiro_do_Sistema_Terrestre",
    "Negacionismo_climático_no_Brasil",
    "Carlos_Nobre_(cientista)",
    "Observatório_do_Clima",
    "Painel_Brasileiro_de_Mudanças_Climáticas",
    "Paulo_Artaxo",
    "Philip_Fearnside",
    "Rede_Clima",
    "Tércio_Ambrizzi",
    "Aquecimento_global",
    "Efeito_estufa",
    "Abandono_do_carvão",
    "Acidificação_oceânica",
    "Acordo_de_Paris_(2015)",
    "Acordo_Verde_Europeu",
    "Aquecimento_global_de_1,5_ºC",
    "Aquecimento_global_descontrolado",
    "Aquecimento_oceânico",
    "Boaty_McBoatface",
    "Breakthrough_Energy",
    "Causas_do_aquecimento_global",
    "Hipótese_da_arma_de_clatratos",
    "Consenso_científico_sobre_mudanças_climáticas",
    "Convenção-Quadro_das_Nações_Unidas_sobre_a_Mudança_do_Clima",
    "Cowspiracy",
    "Declínio_contemporâneo_da_biodiversidade_mundial",
    "Declínio_das_populações_de_peixes_marinhos",
    "Desflorestação",
    "Desoxigenação_oceânica",
    "Desperdício_de_alimentos",
    "Efeitos_regionais_do_aquecimento_global",
    "Equivalência_em_dióxido_de_carbono",
    "Estado_de_emergência_climática",
    "Extinction_Rebellion",
    "Forçamento_radiativo",
    "Fridays_for_Future",
    "Fundo_Verde_para_o_Clima",
    "Geleira_Okjökull",
    "Greve_climática_de_setembro_de_2019",
    "James_Hansen",
    "Iara_Cardoso",
    "Impacto_ambiental_da_pecuária",
    "Impactos_do_aquecimento_global_sobre_o_patrimônio_histórico_e_cultural",
    "Instituto_Potsdam_de_Pesquisas_sobre_o_Impacto_Climático",
    "Lista_de_países_por_emissões_de_dióxido_de_carbono",
    "Lista_de_países_por_reservas_de_carvão",
    "Mudança_Climática_e_Terra",
    "Mudanças_climáticas_e_biodiversidade",
    "Naomi_Oreskes",
    "Neutralidade_de_carbono",
    "Painel_Intergovernamental_sobre_Mudanças_Climáticas",
    "Poluição_atmosférica",
    "Potencial_de_aquecimento_global",
    "Recuo_dos_glaciares_desde_1850",
    "Registro_instrumental_de_temperaturas",
    "Relatório_Charney",
    "Suinocultura_e_sustentabilidade",
    "Thelma_Krug",
    "Turn_Down_the_Heat:_Why_a_4°C_Warmer_World_Must_be_Avoided",
    "Uma_Verdade_Inconveniente"
    )

#Inserting daily views in an empty object
for (i in articles_url1){
  pageviews1 <- article_pageviews(project = "pt.wikipedia",
                                article = paste0(articles_url1),
                                end = "2024051200",
                                granularity = "daily",
                                user_type = "user")
}

#Obtaining total daily views
daily_mean <- aggregate(x = pageviews1$views, 
          by = list(pageviews1$date), 
          FUN = sum) 

#Changing variable names
colnames(daily_mean)[colnames(daily_mean) %in% c("Group.1", "x")] <-
  c("date", "views")

#Writing a ".csv" document
daily_mean %>%
  write.csv("daily_mean.csv")

daily_mean <- read.csv("daily_mean.csv")

#Plotting daily view graphic
daily_mean %>%
  ggplot(aes(date, views))+
  geom_point()+
  geom_line(colour="red")+
  labs(title = "Daily views", x = "Date", y = "Views") +
  theme_bw()

#Saving the dates in an object and changing its property to "numeric"
date <- daily_mean$date 
daily_mean$date <- as.numeric(daily_mean$date)

#Mann-Kendall  
mannkendall <- MannKendall(daily_mean$views)
print(mannkendall) #See results

#Creating trend line
trend_line <- predict(loess(daily_mean$views ~ daily_mean$date))

#Plot with trend line
ggplot() +
  geom_point(aes(x = date, y = daily_mean$views), color = "gray") +
  geom_line(aes(x = date, y = trend_line), color = "red") +
  labs(x = "Date", y = "Views", title = "Trend Line Plot") +
  theme_minimal()

####Model News####

#Creating object with news data (2015 - 2019 = events_n3 / 2020 - 2024 = events_n4)
data_news <- read.csv("events_n3.csv") #Insert events_n3/events_n4 to create the model
data_news$news <- factor(data_news$news, levels = c(0,1), labels = c("Before", "During")) #transform news to factor with 2 levels

model_news <- lme4::glmer(views ~ news + (1|date), family = "poisson",
                     data = data_news) #run model
summary(model_news) #summary of the model
DHARMa::simulateResiduals(model_news, plot = TRUE) #check for normality of residues

r.squaredGLMM(model_news, pj2014 = T)#Maximum likelihoods

#Plotting graphic
plot_news <- ggplot(data_news, aes(x = news, y = views, fill = news)) +
  geom_violin(alpha = 0.6, adjust = 0.5, width = 0.5) +
  geom_jitter(width = 0.2, height = 0.2) +
  geom_boxplot(alpha = 0.8, width = 0.2) +
  labs(x = "News", y = "Views") +
  theme_minimal() +
  ggtitle("Views News (2015 - 2019)") +
  ylim (0,14000) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.title = element_blank(),
    legend.position = "none"
  )

####Model COP####

#Creating object with COPs data (2015 - 2019 = events_c3 / 2020 - 2024 = events_c4)
data_cops <- read.csv("events_c3.csv")
data_cops$cops <- factor(data_cops$cops, levels = c(0,1), labels = c("Before", "During"))

model_cops <- lme4::glmer(views ~ cops + (1|date), family = "poisson",
                          data = data_cops)
summary(model_cops)
DHARMa::simulateResiduals(model_cops, plot = TRUE)

r.squaredGLMM(model_cops, pj2014 = T)#Maximum likelihoods

plot_cops <- ggplot(data_cops, aes(x = cops, y = views, fill = cops)) +
  geom_violin(alpha = 0.6, adjust = 0.5, width = 0.5) +
  geom_jitter(width = 0.2, height = 0.2) +
  geom_boxplot(alpha = 0.8, width = 0.2) +
  labs(x = "COPs", y = "Views") +
  theme_minimal() +
  ggtitle("Views COPs (2015 - 2019)") +
  ylim (0,14000) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.title = element_blank(),
    legend.position = "none"
  )

####Model IPCCs####

#Creating object with IPCCs data (2015 - 2019 = events_i3 / 2020 - 2024 = events_i4)
data_ipccs <- read.csv("events_i3.csv") 
data_ipccs$ipccs <- factor(data_ipccs$ipccs, levels = c(0,1), labels = c("Before", "During"))

model_ipccs <- lme4::glmer(views ~ ipccs + (1|date), family = "poisson",
                           data = data_ipccs)

summary(model_ipccs)
DHARMa::simulateResiduals(model_ipccs, plot = TRUE)

r.squaredGLMM(model_ipccs, pj2014 = T)#Maximum likelihoods

plot_ipccs <- ggplot(data_ipccs, aes(x = ipccs, y = views, fill = ipccs)) +
  geom_violin(alpha = 0.6, adjust = 0.5, width = 0.5) +
  geom_jitter(width = 0.2, height = 0.2) +
  geom_boxplot(alpha = 0.8, width = 0.2) +
  labs(x = "IPCCs", y = "Views") +
  theme_minimal() +
  ggtitle("Views IPCCs (2015-2019)") +
  ylim (0,14000) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.title = element_blank(),
    legend.position = "none"
  )

#Plotting all graphs

ggarrange(plot_cops, plot_ipccs, plot_news,
          ncol = 3, nrow = 1, common.legend = T, legend = "none")
