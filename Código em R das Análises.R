# Limpando Plots, Console and Ambiente
rm(list = ls())
dev.off(dev.list()["RStudioGD"])
cat("\014")

# Carregando bibliotecas necessárias:
library('readr') # é usada para ler dados em R
library('readxl') # é usada para abrir o dataset em excel
library('tidyverse') # coleção abrangente de pacotes R projetados para manipulação e a análise de dados 
library('ggplot2') # é utilizada para criar gráficos estatísticos personalizáveis em R.
library('skimr') #  é usada para obter estatísticas descritivas sobre um conjunto de dados.
library('ggpubr') # é usada para estender as funcionalidades do ggplot2, oferecendo recursos adicionais para facilitar a criação de gráficos estatísticos e relatórios. 
library('ggcorrplot') # é usada para criar matrizes de correlação a partir de dados (no caso desse dataset) categóricos.
library('dplyr') # é usada para manipulação e transformação dos dados.
library('openintro') # fornece conjuntos de dados e funções relacionadas aos materiais didáticos do projeto OpenIntro
library('ggstatsplot') # utilizado para criar gráficos a partir de análises estatísticas.
library('Hmisc') # ajudar a simplificar o cálculo das correlações ponderadas.
library("PerformanceAnalytics") #para realizar o chart de correlação

options(scipen = 100, digits = 5)

####################

  ### Abertura de dataframes, normalização, inclusão de variáveis e exclusão da categoria "Brasil"

# Abrindo o dataframe Frequência de Utilização de Internet:
Internet <- read_xlsx("C:/Users/karol/Downloads/Referências TCC/Tabela Util_Net.xlsx")
Internet <- Internet[rowSums(is.na(Internet)) == 0,]
glimpse(Internet)

Internet %>%
  summary(Internet)

# Dividir todas as colunas numéricas por 100 para ajustar a escala
dn_Internet <- Internet %>%
  mutate(across(where(is.numeric), ~ . / 100))

# Verificar o resumo dos dados normalizados
summary(dn_Internet)

# Criando uma nova variável combinando Total_Freq_Util_Net e Total_Sexo
dn_Internet <- dn_Internet %>%
  mutate(vp = `Total_Freq_Util_Net` * Total_Sexo)

# Excluindo a categoria "Brasil"
dn_Internet <- dn_Internet %>% 
  filter(`Grandes Regiões` != "Brasil")


# Abrindo o dataframe Pessoas que Não Tem Amigos Próximos:
Sem_amgs_proximos <- read_xlsx("C:/Users/karol/Downloads/Referências TCC/Tabela N_Tem_Amigos_Proximos.xlsx")
Sem_amgs_proximos <- Sem_amgs_proximos[rowSums(is.na(Sem_amgs_proximos)) == 0,]
glimpse(Sem_amgs_proximos)

Sem_amgs_proximos %>%
  summary(Sem_amgs_proximos)

# Dividir todas as colunas numéricas por 100 para ajustar a escala
dn_Sem_amgs_proximos <- Sem_amgs_proximos %>%
  mutate(across(where(is.numeric), ~ . / 100))

# Criando uma nova variável combinando Total_Faixa_Idade e Total_Sexo
dn_Sem_amgs_proximos <- dn_Sem_amgs_proximos %>%
  mutate(vp = `Total_Faixa_Idade` * Total_Sexo)

# Excluindo a categoria "Brasil"
dn_Sem_amgs_proximos <- dn_Sem_amgs_proximos %>% 
  filter(
    `Grandes Regiões` != "Brasil",
    `Faixa_Idade` != "13 a 17 anos")


# Abrindo o dataframe Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia:
Preocupadas_com_coisas_comuns <- read_xlsx("C:/Users/karol/Downloads/Referências TCC/Tabela Pre_Coisas_Comuns.xlsx")
Preocupadas_com_coisas_comuns <- Preocupadas_com_coisas_comuns[rowSums(is.na(Preocupadas_com_coisas_comuns)) == 0,]
glimpse(Preocupadas_com_coisas_comuns)

Preocupadas_com_coisas_comuns %>%
  summary(Preocupadas_com_coisas_comuns)

# Dividir todas as colunas numéricas por 100 para ajustar a escala
dn_Preocupadas_com_coisas_comuns <- Preocupadas_com_coisas_comuns %>%
  mutate(across(where(is.numeric), ~ . / 100))

# Criando uma nova variável combinando Total_Faixa_Idade e Total_Sexo
dn_Preocupadas_com_coisas_comuns <- dn_Preocupadas_com_coisas_comuns %>%
  mutate(vp = `Total_Faixa_Idade` * Total_Sexo)

# Excluindo a categoria "Brasil"
dn_Preocupadas_com_coisas_comuns <- dn_Preocupadas_com_coisas_comuns %>% 
  filter(
    `Grandes Regiões` != "Brasil",
    `Faixa_Idade` != "13 a 17 anos")


# Abrindo o dataframe Pessoas que se Sentiram Tristes na maioria das vezes:
Sentiram_tristes <- read_xlsx("C:/Users/karol/Downloads/Referências TCC/Tabela Sentir_Triste.xlsx")
Sentiram_tristes <- Sentiram_tristes[rowSums(is.na(Sentiram_tristes)) == 0,]
glimpse(Sentiram_tristes)

Sentiram_tristes %>%
  summary(Sentiram_tristes)

# Dividir todas as colunas numéricas por 100 para ajustar a escala
dn_Sentiram_tristes <- Sentiram_tristes %>%
  mutate(across(where(is.numeric), ~ . / 100))

# Criando uma nova variável combinando Total_Faixa_Idade e Total_Sexo
dn_Sentiram_tristes <- dn_Sentiram_tristes %>%
  mutate(vp = `Total_Faixa_Idade` * Total_Sexo)

# Excluindo a categoria "Brasil"
dn_Sentiram_tristes <- dn_Sentiram_tristes %>% 
  filter(
    `Grandes Regiões` != "Brasil",
    `Faixa_Idade` != "13 a 17 anos")


# Abrindo o dataframe Pessoas que Sentiram que Ninguém se Preocupa com Elas:
Ninguem_se_preocupa_com_elas <- read_xlsx("C:/Users/karol/Downloads/Referências TCC/Tabela Ninguem_Pre_Eles.xlsx")
Ninguem_se_preocupa_com_elas <- Ninguem_se_preocupa_com_elas[rowSums(is.na(Ninguem_se_preocupa_com_elas)) == 0,]
glimpse(Ninguem_se_preocupa_com_elas)

Ninguem_se_preocupa_com_elas %>%
  summary(Ninguem_se_preocupa_com_elas)

# Dividir todas as colunas numéricas por 100 para ajustar a escala
dn_Ninguem_se_preocupa_com_elas <- Ninguem_se_preocupa_com_elas %>%
  mutate(across(where(is.numeric), ~ . / 100))

# Criando uma nova variável combinando Total_Faixa_Idade e Total_Sexo
dn_Ninguem_se_preocupa_com_elas <- dn_Ninguem_se_preocupa_com_elas %>%
  mutate(vp = `Total_Faixa_Idade` * Total_Sexo)

# Excluindo a categoria "Brasil"
dn_Ninguem_se_preocupa_com_elas <- dn_Ninguem_se_preocupa_com_elas %>% 
  filter(
    `Grandes Regiões` != "Brasil",
    `Faixa_Idade` != "13 a 17 anos")


# Abrindo o dataframe Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas:
Irritadas_Nervosas_MauHumoradas <- read_xlsx("C:/Users/karol/Downloads/Referências TCC/Tabela Sentir_INMH.xlsx")
Irritadas_Nervosas_MauHumoradas <- Irritadas_Nervosas_MauHumoradas[rowSums(is.na(Irritadas_Nervosas_MauHumoradas)) == 0,]
glimpse(Irritadas_Nervosas_MauHumoradas)

Irritadas_Nervosas_MauHumoradas %>%
  summary(Irritadas_Nervosas_MauHumoradas)

# Dividir todas as colunas numéricas por 100 para ajustar a escala
dn_Irritadas_Nervosas_MauHumoradas <- Irritadas_Nervosas_MauHumoradas %>%
  mutate(across(where(is.numeric), ~ . / 100))

# Criando uma nova variável combinando Total_Faixa_Idade e Total_Sexo
dn_Irritadas_Nervosas_MauHumoradas <- dn_Irritadas_Nervosas_MauHumoradas %>%
  mutate(vp = `Total_Faixa_Idade` * Total_Sexo)

# Excluindo a categoria "Brasil"
dn_Irritadas_Nervosas_MauHumoradas <- dn_Irritadas_Nervosas_MauHumoradas %>% 
  filter(
    `Grandes Regiões` != "Brasil",
    `Faixa_Idade` != "13 a 17 anos")


# Abrindo o dataframe Pessoas que Sentiram que a Vida Não Vale a Pena:
Vida_nao_vale_a_pena <- read_xlsx("C:/Users/karol/Downloads/Referências TCC/Tabela Vida_N_VP.xlsx")
Vida_nao_vale_a_pena <- Vida_nao_vale_a_pena[rowSums(is.na(Vida_nao_vale_a_pena)) == 0,]
glimpse(Vida_nao_vale_a_pena)

Vida_nao_vale_a_pena %>%
  summary(Vida_nao_vale_a_pena)

# Dividir todas as colunas numéricas por 100 para ajustar a escala
dn_Vida_nao_vale_a_pena <- Vida_nao_vale_a_pena %>%
  mutate(across(where(is.numeric), ~ . / 100))

# Criar uma nova variável combinando Total_Faixa_Idade e Total_Sexo
dn_Vida_nao_vale_a_pena <- dn_Vida_nao_vale_a_pena %>%
  mutate(vp = `Total_Faixa_Idade` * Total_Sexo)

# Excluindo a categoria "Brasil"
dn_Vida_nao_vale_a_pena <- dn_Vida_nao_vale_a_pena %>% 
  filter(
    `Grandes Regiões` != "Brasil",
    `Faixa_Idade` != "13 a 17 anos")

########################

  ##Boxplot para visualização dos datasets

#Frequência de Utilização de Internet
boxplot(dn_Internet$vp)

#Pessoas que Não Tem Amigos Próximos
boxplot(dn_Sem_amgs_proximos$vp)

#Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia
boxplot(dn_Preocupadas_com_coisas_comuns$vp)

#Pessoas que se Sentiram Tristes na maioria das vezes
boxplot(dn_Sentiram_tristes$vp)

#Pessoas que Sentiram que Ninguém se Preocupa com Elas
boxplot(dn_Ninguem_se_preocupa_com_elas$vp)

#Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas
boxplot(dn_Irritadas_Nervosas_MauHumoradas$vp)

#Pessoas que Sentiram que a Vida Não Vale a Pena
boxplot(dn_Vida_nao_vale_a_pena$vp)


#########################

  ### Gráfico de Pizza para visualização da Distribuição Proporcional de cada variável

## Frequência de Utilização de Internet | Grandes Regiões

# Agregando os dados por região
GR_agg0 <- dn_Internet %>%
  group_by(`Grandes Regiões`) %>%
  summarise(vp_media = mean(vp, na.rm = TRUE))

# Gráfico de pizza por Grandes Regiões
ggplot(GR_agg0, aes(x = "", y = vp_media, fill = `Grandes Regiões`)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  labs(title = "Distribuição Proporcional de Pessoas que Utilizam a Internet por Região") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remover textos desnecessários
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_text(aes(label = scales::percent(vp_media / sum(vp_media))), position = position_stack(vjust = 0.5))  # Adicionar rótulos percentuais

## Frequência de Utilização de Internet | Frequência Semanal

# Agregando os dados por Frequência Semanal
FNet_agg <- dn_Internet %>%
  group_by(`Freq_Util_Net`) %>%
  summarise(vp_media = mean(vp, na.rm = TRUE))

# Gráfico de pizza por Frequência Semanal (Não mostrar na dissertação, apenas escrever)
ggplot(FNet_agg, aes(x = "", y = vp_media, fill = `Freq_Util_Net`)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  labs(title = "Distribuição Proporcional de Pessoas que Utilizam a Internet por Frequência Semanal") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remover textos desnecessários
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_text(aes(label = scales::percent(vp_media / sum(vp_media))), position = position_stack(vjust = 0.5))  # Adicionar rótulos percentuais

## Frequência de Utilização de Internet | Sexo

# Agregando os dados por Sexo
Sexo_agg0 <- dn_Internet %>%
  group_by(`Sexo`) %>%
  summarise(vp_media = mean(vp, na.rm = TRUE))

# Gráfico de pizza por Sexo (Não mostrar na dissertação, apenas escrever)
ggplot(Sexo_agg0, aes(x = "", y = vp_media, fill = `Sexo`)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  labs(title = "Distribuição Proporcional de Pessoas que Utilizam a Internet por Sexo") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remover textos desnecessários
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_text(aes(label = scales::percent(vp_media / sum(vp_media))), position = position_stack(vjust = 0.5)) + # Adicionar rótulos percentuais
  scale_fill_manual(values = c("Homem" = "blue", "Mulher" = "pink"))  # Troca as cores


## Pessoas que Não Tem Amigos Próximos | Grandes Regiões

# Agregando os dados por região
GR_agg1 <- dn_Sem_amgs_proximos %>%
  group_by(`Grandes Regiões`) %>%
  summarise(vp_media = mean(vp, na.rm = TRUE))

# Gráfico de pizza por Grandes Regiões
ggplot(GR_agg1, aes(x = "", y = vp_media, fill = `Grandes Regiões`)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  labs(title = "Distribuição Proporcional de Pessoas que Não Tem Amigos Próximos por Região") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remover textos desnecessários
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_text(aes(label = scales::percent(vp_media / sum(vp_media))), position = position_stack(vjust = 0.5))  # Adicionar rótulos percentuais

## Pessoas que Não Tem Amigos Próximos | Faixa de Idade

# Agregando os dados por Faixa de Idade
FI_agg1 <- dn_Sem_amgs_proximos %>%
  group_by(`Faixa_Idade`) %>%
  summarise(vp_media = mean(vp, na.rm = TRUE))

# Gráfico de pizza por Faixa de Idade (Não mostrar na dissertação, apenas escrever)
ggplot(FI_agg1, aes(x = "", y = vp_media, fill = `Faixa_Idade`)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  labs(title = "Distribuição Proporcional de Pessoas que Não Tem Amigos Próximos por Faixa de Idade") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remover textos desnecessários
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_text(aes(label = scales::percent(vp_media / sum(vp_media))), position = position_stack(vjust = 0.5))  # Adicionar rótulos percentuais

## Pessoas que Não Tem Amigos Próximos | Sexo

# Agregando os dados por Sexo
Sexo_agg1 <- dn_Sem_amgs_proximos %>%
  group_by(`Sexo`) %>%
  summarise(vp_media = mean(vp, na.rm = TRUE))

# Gráfico de pizza por Sexo (Não mostrar na dissertação, apenas escrever)
ggplot(Sexo_agg1, aes(x = "", y = vp_media, fill = `Sexo`)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  labs(title = "Distribuição Proporcional de Pessoas que Não Tem Amigos Próximos por Sexo") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remover textos desnecessários
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_text(aes(label = scales::percent(vp_media / sum(vp_media))), position = position_stack(vjust = 0.5)) + # Adicionar rótulos percentuais
  scale_fill_manual(values = c("Homem" = "blue", "Mulher" = "pink"))  # Troca as cores


## Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia | Grandes Regiões

# Agregando os dados por região
GR_agg2 <- dn_Preocupadas_com_coisas_comuns %>%
  group_by(`Grandes Regiões`) %>%
  summarise(vp_media = mean(vp, na.rm = TRUE))

# Gráfico de pizza por Grandes Regiões
ggplot(GR_agg2, aes(x = "", y = vp_media, fill = `Grandes Regiões`)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  labs(title = "Distribuição Proporcional de Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia por Região") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remover textos desnecessários
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_text(aes(label = scales::percent(vp_media / sum(vp_media))), position = position_stack(vjust = 0.5))  # Adicionar rótulos percentuais

## Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia | Faixa de Idade

# Agregando os dados por Faixa de Idade
FI_agg2 <- dn_Preocupadas_com_coisas_comuns %>%
  group_by(`Faixa_Idade`) %>%
  summarise(vp_media = mean(vp, na.rm = TRUE))

# Gráfico de pizza por Faixa de Idade (Não mostrar na dissertação, apenas escrever)
ggplot(FI_agg2, aes(x = "", y = vp_media, fill = `Faixa_Idade`)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  labs(title = "Distribuição Proporcional de Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia por Faixa de Idade") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remover textos desnecessários
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_text(aes(label = scales::percent(vp_media / sum(vp_media))), position = position_stack(vjust = 0.5))  # Adicionar rótulos percentuais

## Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia | Sexo

# Agregando os dados por Sexo
Sexo_agg2 <- dn_Preocupadas_com_coisas_comuns %>%
  group_by(`Sexo`) %>%
  summarise(vp_media = mean(vp, na.rm = TRUE))

# Gráfico de pizza por Sexo (Não mostrar na dissertação, apenas escrever)
ggplot(Sexo_agg2, aes(x = "", y = vp_media, fill = `Sexo`)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  labs(title = "Distribuição Proporcional de Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia por Sexo") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remover textos desnecessários
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_text(aes(label = scales::percent(vp_media / sum(vp_media))), position = position_stack(vjust = 0.5)) + # Adicionar rótulos percentuais
  scale_fill_manual(values = c("Homem" = "blue", "Mulher" = "pink"))  # Troca as cores


## Pessoas que se Sentiram Tristes na maioria das vezes | Grandes Regiões

# Agregando os dados por região
GR_agg3 <- dn_Sentiram_tristes %>%
  group_by(`Grandes Regiões`) %>%
  summarise(vp_media = mean(vp, na.rm = TRUE))

# Gráfico de pizza por Grandes Regiões
ggplot(GR_agg3, aes(x = "", y = vp_media, fill = `Grandes Regiões`)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  labs(title = "Distribuição Proporcional de Pessoas que se Sentiram Tristes na maioria das vezes por Região") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remover textos desnecessários
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_text(aes(label = scales::percent(vp_media / sum(vp_media))), position = position_stack(vjust = 0.5))  # Adicionar rótulos percentuais

## Pessoas que se Sentiram Tristes na maioria das vezes | Faixa de Idade

# Agregando os dados por Faixa de Idade
FI_agg3 <- dn_Sentiram_tristes %>%
  group_by(`Faixa_Idade`) %>%
  summarise(vp_media = mean(vp, na.rm = TRUE))

# Gráfico de pizza por Faixa de Idade (Não mostrar na dissertação, apenas escrever)
ggplot(FI_agg3, aes(x = "", y = vp_media, fill = `Faixa_Idade`)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  labs(title = "Distribuição Proporcional de Pessoas que se Sentiram Tristes na maioria das vezes por Faixa de Idade") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remover textos desnecessários
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_text(aes(label = scales::percent(vp_media / sum(vp_media))), position = position_stack(vjust = 0.5))  # Adicionar rótulos percentuais

## Pessoas que se Sentiram Tristes na maioria das vezes | Sexo

# Agregando os dados por Sexo
Sexo_agg3 <- dn_Sentiram_tristes %>%
  group_by(`Sexo`) %>%
  summarise(vp_media = mean(vp, na.rm = TRUE))

# Gráfico de pizza por Sexo (Não mostrar na dissertação, apenas escrever)
ggplot(Sexo_agg3, aes(x = "", y = vp_media, fill = `Sexo`)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  labs(title = "Distribuição Proporcional de Pessoas que se Sentiram Tristes na maioria das vezes por Sexo") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remover textos desnecessários
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_text(aes(label = scales::percent(vp_media / sum(vp_media))), position = position_stack(vjust = 0.5)) + # Adicionar rótulos percentuais
  scale_fill_manual(values = c("Homem" = "blue", "Mulher" = "pink"))  # Troca as cores


## Pessoas que Sentiram que Ninguém se Preocupa com Elas | Grandes Regiões

# Agregando os dados por região
GR_agg4 <- dn_Ninguem_se_preocupa_com_elas %>%
  group_by(`Grandes Regiões`) %>%
  summarise(vp_media = mean(vp, na.rm = TRUE))

# Gráfico de pizza por Grandes Regiões
ggplot(GR_agg4, aes(x = "", y = vp_media, fill = `Grandes Regiões`)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  labs(title = "Distribuição Proporcional de Pessoas que Sentiram que Ninguém se Preocupa com Elas por Região") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remover textos desnecessários
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_text(aes(label = scales::percent(vp_media / sum(vp_media))), position = position_stack(vjust = 0.5))  # Adicionar rótulos percentuais

## Pessoas que Sentiram que Ninguém se Preocupa com Elas | Faixa de Idade

# Agregando os dados por Faixa de Idade
FI_agg4 <- dn_Ninguem_se_preocupa_com_elas %>%
  group_by(`Faixa_Idade`) %>%
  summarise(vp_media = mean(vp, na.rm = TRUE))

# Gráfico de pizza por Faixa de Idade (Não mostrar na dissertação, apenas escrever)
ggplot(FI_agg4, aes(x = "", y = vp_media, fill = `Faixa_Idade`)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  labs(title = "Distribuição Proporcional de Pessoas que Sentiram que Ninguém se Preocupa com Elas por Faixa de Idade") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remover textos desnecessários
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_text(aes(label = scales::percent(vp_media / sum(vp_media))), position = position_stack(vjust = 0.5))  # Adicionar rótulos percentuais

## Pessoas que Sentiram que Ninguém se Preocupa com Elas | Sexo

# Agregando os dados por Sexo
Sexo_agg4 <- dn_Ninguem_se_preocupa_com_elas %>%
  group_by(`Sexo`) %>%
  summarise(vp_media = mean(vp, na.rm = TRUE))

# Gráfico de pizza por Sexo (Não mostrar na dissertação, apenas escrever)
ggplot(Sexo_agg4, aes(x = "", y = vp_media, fill = `Sexo`)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  labs(title = "Distribuição Proporcional de Pessoas que Sentiram que Ninguém se Preocupa com Elas por Sexo") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remover textos desnecessários
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_text(aes(label = scales::percent(vp_media / sum(vp_media))), position = position_stack(vjust = 0.5)) + # Adicionar rótulos percentuais
  scale_fill_manual(values = c("Homem" = "blue", "Mulher" = "pink"))  # Troca as cores


## Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas | Grandes Regiões

# Agregando os dados por região
GR_agg5 <- dn_Irritadas_Nervosas_MauHumoradas %>%
  group_by(`Grandes Regiões`) %>%
  summarise(vp_media = mean(vp, na.rm = TRUE))

# Gráfico de pizza por Grandes Regiões
ggplot(GR_agg5, aes(x = "", y = vp_media, fill = `Grandes Regiões`)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  labs(title = "Distribuição Proporcional de Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas por Região") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remover textos desnecessários
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_text(aes(label = scales::percent(vp_media / sum(vp_media))), position = position_stack(vjust = 0.5))  # Adicionar rótulos percentuais

## Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas | Faixa de Idade

# Agregando os dados por Faixa de Idade
FI_agg5 <- dn_Irritadas_Nervosas_MauHumoradas %>%
  group_by(`Faixa_Idade`) %>%
  summarise(vp_media = mean(vp, na.rm = TRUE))

# Gráfico de pizza por Faixa de Idade (Não mostrar na dissertação, apenas escrever)
ggplot(FI_agg5, aes(x = "", y = vp_media, fill = `Faixa_Idade`)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  labs(title = "Distribuição Proporcional de Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas por Faixa de Idade") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remover textos desnecessários
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_text(aes(label = scales::percent(vp_media / sum(vp_media))), position = position_stack(vjust = 0.5))  # Adicionar rótulos percentuais

## Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas | Sexo

# Agregando os dados por Sexo
Sexo_agg5 <- dn_Irritadas_Nervosas_MauHumoradas %>%
  group_by(`Sexo`) %>%
  summarise(vp_media = mean(vp, na.rm = TRUE))

# Gráfico de pizza por Sexo (Não mostrar na dissertação, apenas escrever)
ggplot(Sexo_agg5, aes(x = "", y = vp_media, fill = `Sexo`)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  labs(title = "Distribuição Proporcional de Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas por Sexo") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remover textos desnecessários
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_text(aes(label = scales::percent(vp_media / sum(vp_media))), position = position_stack(vjust = 0.5)) + # Adicionar rótulos percentuais
  scale_fill_manual(values = c("Homem" = "blue", "Mulher" = "pink"))  # Troca as cores


## Pessoas que Sentiram que a Vida Não Vale a Pena | Grandes Regiões

# Agregando os dados por região
GR_agg6 <- dn_Vida_nao_vale_a_pena %>%
  group_by(`Grandes Regiões`) %>%
  summarise(vp_media = mean(vp, na.rm = TRUE))

# Gráfico de pizza por Grandes Regiões
ggplot(GR_agg6, aes(x = "", y = vp_media, fill = `Grandes Regiões`)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  labs(title = "Distribuição Proporcional de Pessoas que Sentiram que a Vida Não Vale a Pena por Região") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remover textos desnecessários
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_text(aes(label = scales::percent(vp_media / sum(vp_media))), position = position_stack(vjust = 0.5))  # Adicionar rótulos percentuais

## Pessoas que Sentiram que a Vida Não Vale a Pena | Faixa de Idade

# Agregando os dados por Faixa de Idade
FI_agg6 <- dn_Vida_nao_vale_a_pena %>%
  group_by(`Faixa_Idade`) %>%
  summarise(vp_media = mean(vp, na.rm = TRUE))

# Gráfico de pizza por Faixa de Idade (Não mostrar na dissertação, apenas escrever)
ggplot(FI_agg6, aes(x = "", y = vp_media, fill = `Faixa_Idade`)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  labs(title = "Distribuição Proporcional de Pessoas que Sentiram que a Vida Não Vale a Pena por Faixa de Idade") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remover textos desnecessários
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_text(aes(label = scales::percent(vp_media / sum(vp_media))), position = position_stack(vjust = 0.5))  # Adicionar rótulos percentuais

## Pessoas que Sentiram que a Vida Não Vale a Pena | Sexo

# Agregando os dados por Sexo
Sexo_agg6 <- dn_Vida_nao_vale_a_pena %>%
  group_by(`Sexo`) %>%
  summarise(vp_media = mean(vp, na.rm = TRUE))

# Gráfico de pizza por Sexo (Não mostrar na dissertação, apenas escrever)
ggplot(Sexo_agg6, aes(x = "", y = vp_media, fill = `Sexo`)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  labs(title = "Distribuição Proporcional de Pessoas que Sentiram que a Vida Não Vale a Pena por Sexo") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remover textos desnecessários
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_text(aes(label = scales::percent(vp_media / sum(vp_media))), position = position_stack(vjust = 0.5)) + # Adicionar rótulos percentuais
  scale_fill_manual(values = c("Homem" = "blue", "Mulher" = "pink"))  # Troca as cores

###################################

  ### Gráfico de Barras para visualização da Distribuição Proporcional de cada variável

## Frequência de Utilização de Internet | Grandes Regiões x Sexo
ggplot(dn_Internet, aes(x = `Grandes Regiões`, y = vp, fill = Sexo)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Proporção de Utilização da Internet por Região e Sexo",
       x = "Grandes Regiões",
       y = "Média de Utilização da Internet (%)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Homem" = "blue", "Mulher" = "pink"))

## Pessoas que Não Tem Amigos Próximos | Grandes Regiões x Sexo
ggplot(dn_Sem_amgs_proximos, aes(x = `Grandes Regiões`, y = vp, fill = Sexo)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Proporção de Pessoas que Não Tem Amigos Próximos por Região e Sexo",
       x = "Grandes Regiões",
       y = "Média de Pessoas que Não Tem Amigos Próximos (%)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Homem" = "blue", "Mulher" = "pink"))

## Pessoas que Não Tem Amigos Próximos | Grandes Regiões x Faixa de Idade
ggplot(dn_Sem_amgs_proximos, aes(x = `Grandes Regiões`, y = vp, fill = Faixa_Idade)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Média de Pessoas Sem Amigos Próximos por Região e Faixa de Idade",
       x = "Grandes Regiões",
       y = "Média de Pessoas Sem Amigos Próximos (%)") +
  theme_minimal()

## Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia | Grandes Regiões x Sexo
ggplot(dn_Preocupadas_com_coisas_comuns, aes(x = `Grandes Regiões`, y = vp, fill = Sexo)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Proporção de Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia por Região e Sexo",
       x = "Grandes Regiões",
       y = "Média de Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia (%)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Homem" = "blue", "Mulher" = "pink"))

## Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia | Grandes Regiões x Faixa de Idade
ggplot(dn_Preocupadas_com_coisas_comuns, aes(x = `Grandes Regiões`, y = vp, fill = Faixa_Idade)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Média de Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia por Região e Faixa de Idade",
       x = "Grandes Regiões",
       y = "Média de Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia (%)") +
  theme_minimal()

## Pessoas que se Sentiram Tristes na maioria das vezes | Grandes Regiões x Sexo
ggplot(dn_Sentiram_tristes, aes(x = `Grandes Regiões`, y = vp, fill = Sexo)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Proporção de Pessoas que se Sentiram Tristes na maioria das vezes por Região e Sexo",
       x = "Grandes Regiões",
       y = "Média de Pessoas que se Sentiram Tristes na maioria das vezes (%)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Homem" = "blue", "Mulher" = "pink"))

## Pessoas que se Sentiram Tristes na maioria das vezes | Grandes Regiões x Faixa de Idade
ggplot(dn_Sentiram_tristes, aes(x = `Grandes Regiões`, y = vp, fill = Faixa_Idade)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Média de Pessoas que se Sentiram Tristes na maioria das vezes por Região e Faixa de Idade",
       x = "Grandes Regiões",
       y = "Média de Pessoas que se Sentiram Tristes na maioria das vezes (%)") +
  theme_minimal()

## Pessoas que Sentiram que Ninguém se Preocupa com Elas | Grandes Regiões x Sexo
ggplot(dn_Ninguem_se_preocupa_com_elas, aes(x = `Grandes Regiões`, y = vp, fill = Sexo)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Proporção de Pessoas que Sentiram que Ninguém se Preocupa com Elas por Região e Sexo",
       x = "Grandes Regiões",
       y = "Média de Pessoas que Sentiram que Ninguém se Preocupa com Elas (%)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Homem" = "blue", "Mulher" = "pink"))

## Pessoas que Sentiram que Ninguém se Preocupa com Elas | Grandes Regiões x Faixa de Idade
ggplot(dn_Ninguem_se_preocupa_com_elas, aes(x = `Grandes Regiões`, y = vp, fill = Faixa_Idade)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Média de Pessoas que Sentiram que Ninguém se Preocupa com Elas por Região e Faixa de Idade",
       x = "Grandes Regiões",
       y = "Média de Pessoas que Sentiram que Ninguém se Preocupa com Elas (%)") +
  theme_minimal()

## Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas | Grandes Regiões x Sexo
ggplot(dn_Irritadas_Nervosas_MauHumoradas, aes(x = `Grandes Regiões`, y = vp, fill = Sexo)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Proporção de Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas por Região e Sexo",
       x = "Grandes Regiões",
       y = "Média de Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas (%)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Homem" = "blue", "Mulher" = "pink"))

## Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas | Grandes Regiões x Faixa de Idade
ggplot(dn_Irritadas_Nervosas_MauHumoradas, aes(x = `Grandes Regiões`, y = vp, fill = Faixa_Idade)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Média de Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas por Região e Faixa de Idade",
       x = "Grandes Regiões",
       y = "Média de Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas (%)") +
  theme_minimal()

## Pessoas que Sentiram que a Vida Não Vale a Pena | Grandes Regiões x Sexo
ggplot(dn_Vida_nao_vale_a_pena, aes(x = `Grandes Regiões`, y = vp, fill = Sexo)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Proporção de Pessoas que Sentiram que a Vida Não Vale a Pena por Região e Sexo",
       x = "Grandes Regiões",
       y = "Média de Pessoas que Sentiram que a Vida Não Vale a Pena (%)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Homem" = "blue", "Mulher" = "pink"))

## Pessoas que Sentiram que a Vida Não Vale a Pena | Grandes Regiões x Faixa de Idade
ggplot(dn_Vida_nao_vale_a_pena, aes(x = `Grandes Regiões`, y = vp, fill = Faixa_Idade)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Média de Pessoas que Sentiram que a Vida Não Vale a Pena por Região e Faixa de Idade",
       x = "Grandes Regiões",
       y = "Média de Pessoas que Sentiram que a Vida Não Vale a Pena (%)") +
  theme_minimal()

#####################################

  ### Média e mediana comum

# Frequência de Utilização de Internet
media_mediana_Internet <- dn_Internet %>%
  summarise(
    media = mean(`vp`, na.rm = TRUE),  # Cálculo da média
    mediana = median(`vp`, na.rm = TRUE),  # Cálculo da mediana
    sd = sd(`vp`, na.rm = TRUE),  # Cálculo do desvio padrão
    n = n(),  # Número de observações
    error = qt(0.975, df = n - 1) * (sd / sqrt(n)),  # Erro padrão (intervalo de confiança de 95%)
    lower_bound = media - error,  # Limite inferior do intervalo de confiança
    upper_bound = media + error   # Limite superior do intervalo de confiança
  )

# Pessoas que Não Tem Amigos Próximos
media_mediana_Sem_amgs_proximos <- dn_Sem_amgs_proximos %>%
  summarise(
    media = mean(`vp`, na.rm = TRUE),  # Cálculo da média
    mediana = median(`vp`, na.rm = TRUE),  # Cálculo da mediana
    sd = sd(`vp`, na.rm = TRUE),  # Cálculo do desvio padrão
    n = n(),  # Número de observações
    error = qt(0.975, df = n - 1) * (sd / sqrt(n)),  # Erro padrão (intervalo de confiança de 95%)
    lower_bound = media - error,  # Limite inferior do intervalo de confiança
    upper_bound = media + error   # Limite superior do intervalo de confiança
  )

# Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia
media_mediana_Preocupadas_com_coisas_comuns <- dn_Preocupadas_com_coisas_comuns %>%
  summarise(
    media = mean(`vp`, na.rm = TRUE),  # Cálculo da média
    mediana = median(`vp`, na.rm = TRUE),  # Cálculo da mediana
    sd = sd(`vp`, na.rm = TRUE),  # Cálculo do desvio padrão
    n = n(),  # Número de observações
    error = qt(0.975, df = n - 1) * (sd / sqrt(n)),  # Erro padrão (intervalo de confiança de 95%)
    lower_bound = media - error,  # Limite inferior do intervalo de confiança
    upper_bound = media + error   # Limite superior do intervalo de confiança
  )

# Pessoas que se Sentiram Tristes na maioria das vezes
media_mediana_Sentiram_tristes <- dn_Sentiram_tristes %>%
  summarise(
    media = mean(`vp`, na.rm = TRUE),  # Cálculo da média
    mediana = median(`vp`, na.rm = TRUE),  # Cálculo da mediana
    sd = sd(`vp`, na.rm = TRUE),  # Cálculo do desvio padrão
    n = n(),  # Número de observações
    error = qt(0.975, df = n - 1) * (sd / sqrt(n)),  # Erro padrão (intervalo de confiança de 95%)
    lower_bound = media - error,  # Limite inferior do intervalo de confiança
    upper_bound = media + error   # Limite superior do intervalo de confiança
  )

# Pessoas que Sentiram que Ninguém se Preocupa com Elas
media_mediana_Ninguem_se_preocupa_com_elas <- dn_Ninguem_se_preocupa_com_elas %>%
  summarise(
    media = mean(`vp`, na.rm = TRUE),  # Cálculo da média
    mediana = median(`vp`, na.rm = TRUE),  # Cálculo da mediana
    sd = sd(`vp`, na.rm = TRUE),  # Cálculo do desvio padrão
    n = n(),  # Número de observações
    error = qt(0.975, df = n - 1) * (sd / sqrt(n)),  # Erro padrão (intervalo de confiança de 95%)
    lower_bound = media - error,  # Limite inferior do intervalo de confiança
    upper_bound = media + error   # Limite superior do intervalo de confiança
  )

# Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas
media_mediana_Irritadas_Nervosas_MauHumoradas <- dn_Irritadas_Nervosas_MauHumoradas %>%
  summarise(
    media = mean(`vp`, na.rm = TRUE),  # Cálculo da média
    mediana = median(`vp`, na.rm = TRUE),  # Cálculo da mediana
    sd = sd(`vp`, na.rm = TRUE),  # Cálculo do desvio padrão
    n = n(),  # Número de observações
    error = qt(0.975, df = n - 1) * (sd / sqrt(n)),  # Erro padrão (intervalo de confiança de 95%)
    lower_bound = media - error,  # Limite inferior do intervalo de confiança
    upper_bound = media + error   # Limite superior do intervalo de confiança
  )

# Pessoas que Sentiram que a Vida Não Vale a Pena
media_mediana_Vida_nao_vale_a_pena <- dn_Vida_nao_vale_a_pena %>%
  summarise(
    media = mean(`vp`, na.rm = TRUE),  # Cálculo da média
    mediana = median(`vp`, na.rm = TRUE),  # Cálculo da mediana
    sd = sd(`vp`, na.rm = TRUE),  # Cálculo do desvio padrão
    n = n(),  # Número de observações
    error = qt(0.975, df = n - 1) * (sd / sqrt(n)),  # Erro padrão (intervalo de confiança de 95%)
    lower_bound = media - error,  # Limite inferior do intervalo de confiança
    upper_bound = media + error   # Limite superior do intervalo de confiança
  )

################################

  ### Shapiro Test

## Frequência de Utilização de Internet
shapiro.test(dn_Internet$vp)
# W = 0.55, p-value = 0.00000000069

ggqqplot(dn_Internet$vp, ylab = "Frequência de Utilização de Internet")

## Pessoas que Não Tem Amigos Próximos
shapiro.test(dn_n_amgs_prox$vp)
# W = 0.861, p-value = 0.008

ggqqplot(dn_n_amgs_prox$vp, ylab = "Pessoas que Não Tem Amigos Próximos")

## Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia
shapiro.test(dn_p_ccomuns$vp)
# W = 0.836, p-value = 0.0032

ggqqplot(dn_p_ccomuns$vp, ylab = "Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia")

## Pessoas que se Sentiram Tristes na maioria das vezes
shapiro.test(dn_s_triste$vp)
# W = 0.837, p-value = 0.0032

ggqqplot(dn_s_triste$vp, ylab = "Pessoas que se Sentiram Tristes na maioria das vezes")

## Pessoas que Sentiram que Ninguém se Preocupa com Elas
shapiro.test(dn_nin_pre_elas$vp)
# W = 0.795, p-value = 0.00072

ggqqplot(dn_nin_pre_elas$vp, ylab = "Pessoas que Sentiram que Ninguém se Preocupa com Elas")

## Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas
shapiro.test(dn_INMH$vp)
# W = 0.832, p-value = 0.0027

ggqqplot(dn_INMH$vp, ylab = "Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas")

## Pessoas que Sentiram que a Vida Não Vale a Pena
shapiro.test(dn_vida_n_vp$vp)
# W = 0.776, p-value = 0.00039

ggqqplot(dn_vida_n_vp$vp, ylab = "Pessoas que Sentiram que a Vida Não Vale a Pena")

###########################################

  ##### Correlações ###############################################################################

## Frequência de Utilização de Internet x Pessoas que Não Tem Amigos Próximos (Homens e Mulheres)

#Juntando dataframes
dados_combinados_0 <- dn_Internet %>%
  inner_join(dn_Sem_amgs_proximos, by = c("Grandes Regiões", "Sexo"))

#Correlação de Spearman
cor_spearman_dn_Internet_dn_Sem_amgs_proximos_test <- cor.test(dados_combinados_0$vp.x, 
                                                  dados_combinados_0$vp.y, 
                                                  method = "spearman")
print(cor_spearman_dn_Internet_dn_Sem_amgs_proximos_test)

ggplot(dados_combinados_0, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Utilização de Internet e Pessoas Sem Amigos Próximos",
       x = "Média de Utilização de Internet",
       y = "Proporção de Pessoas Sem Amigos Próximos") +
  theme_minimal()

## Frequência de Utilização de Internet x Pessoas que Não Tem Amigos Próximos (Homens)

# Filtrar para homens
dados_h0 <- dados_combinados_0 %>%
  filter(Sexo == "Homem")

#Correlação de Spearman
cor_spearman_h_dn_Internet_dn_Sem_amgs_proximos_test <- cor.test(dados_h0$vp.x, 
                                                  dados_h0$vp.y, 
                                                  method = "spearman")
print(cor_spearman_h_dn_Internet_dn_Sem_amgs_proximos_test)

ggplot(dados_h0, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Utilização de Internet e Pessoas Sem Amigos Próximos (Homens)",
       x = "Média de Utilização de Internet",
       y = "Proporção de Pessoas Sem Amigos Próximos") +
  theme_minimal()

## Frequência de Utilização de Internet x Pessoas que Não Tem Amigos Próximos (Mulheres)

# Filtrar para mulheres
dados_m0 <- dados_combinados_0 %>%
  filter(Sexo == "Mulher")

#Correlação de Spearman
cor_spearman_m_dn_Internet_dn_Sem_amgs_proximos_test <- cor.test(dados_m0$vp.x, 
                                                  dados_m0$vp.y, 
                                                  method = "spearman")
print(cor_spearman_m_dn_Internet_dn_Sem_amgs_proximos_test)

ggplot(dados_m0, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Utilização de Internet e Pessoas Sem Amigos Próximos (Mulheres)",
       x = "Média de Utilização de Internet",
       y = "Proporção de Pessoas Sem Amigos Próximos") +
  theme_minimal()

## Frequência de Utilização de Internet x Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia (Homens e Mulheres)

#Juntando dataframes
dados_combinados_1 <- dn_Internet %>%
  inner_join(dn_Preocupadas_com_coisas_comuns, by = c("Grandes Regiões", "Sexo"))

#Correlação de Spearman
cor_spearman_dn_Internet_dn_Preocupadas_com_coisas_comuns_test <- cor.test(dados_combinados_1$vp.x, 
                                                  dados_combinados_1$vp.y, 
                                                  method = "spearman")
print(cor_spearman_dn_Internet_dn_Preocupadas_com_coisas_comuns_test)

ggplot(dados_combinados_1, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Utilização de Internet e Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia",
       x = "Média de Utilização de Internet",
       y = "Proporção de Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia") +
  theme_minimal()

## Frequência de Utilização de Internet x Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia (Homens)

# Filtrar para homens
dados_h1 <- dados_combinados_1 %>%
  filter(Sexo == "Homem")

#Correlação de Spearman
cor_spearman_h_dn_Internet_dn_Preocupadas_com_coisas_comuns_test <- cor.test(dados_h1$vp.x, 
                                               dados_h1$vp.y, 
                                               method = "spearman")
print(cor_spearman_h_dn_Internet_dn_Preocupadas_com_coisas_comuns_test)

ggplot(dados_h1, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Utilização de Internet e Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia (Homens)",
       x = "Média de Utilização de Internet",
       y = "Proporção de Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia") +
  theme_minimal()

## Frequência de Utilização de Internet x Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia (Mulheres)

# Filtrar para mulheres
dados_m1 <- dados_combinados_1 %>%
  filter(Sexo == "Mulher")

#Correlação de Spearman
cor_spearman_m_dn_Internet_dn_Preocupadas_com_coisas_comuns_test <- cor.test(dados_m1$vp.x, 
                                                 dados_m1$vp.y, 
                                                 method = "spearman")
print(cor_spearman_m_dn_Internet_dn_Preocupadas_com_coisas_comuns_test)

ggplot(dados_m1, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Utilização de Internet e Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia (Mulheres)",
       x = "Média de Utilização de Internet",
       y = "Proporção de Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia") +
  theme_minimal()

## Frequência de Utilização de Internet x Pessoas que se Sentiram Tristes na maioria das vezes (Homens e Mulheres)

#Juntando dataframes
dados_combinados_2 <- dn_Internet %>%
  inner_join(dn_Sentiram_tristes, by = c("Grandes Regiões", "Sexo"))

#Correlação de Spearman
cor_spearman_dn_Internet_dn_Sentiram_tristes_test <- cor.test(dados_combinados_2$vp.x, 
                                               dados_combinados_2$vp.y, 
                                               method = "spearman")
print(cor_spearman_dn_Internet_dn_Sentiram_tristes_test)

ggplot(dados_combinados_2, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Utilização de Internet e Pessoas que se Sentiram Tristes na maioria das vezes",
       x = "Média de Utilização de Internet",
       y = "Proporção de Pessoas que se Sentiram Tristes na maioria das vezes") +
  theme_minimal()

## Frequência de Utilização de Internet x Pessoas que se Sentiram Tristes na maioria das vezes (Homens)

# Filtrar para homens
dados_h2 <- dados_combinados_2 %>%
  filter(Sexo == "Homem")

#Correlação de Spearman
cor_spearman_h_dn_Internet_dn_Sentiram_tristes_test <- cor.test(dados_h2$vp.x, 
                                                 dados_h2$vp.y, 
                                                 method = "spearman")
print(cor_spearman_h_dn_Internet_dn_Sentiram_tristes_test)

ggplot(dados_h2, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Utilização de Internet e Pessoas que se Sentiram Tristes na maioria das vezes (Homens)",
       x = "Média de Utilização de Internet",
       y = "Proporção de Pessoas que se Sentiram Tristes na maioria das vezes") +
  theme_minimal()

## Frequência de Utilização de Internet x Pessoas que se Sentiram Tristes na maioria das vezes (Mulheres)

# Filtrar para mulheres
dados_m2 <- dados_combinados_2 %>%
  filter(Sexo == "Mulher")

#Correlação de Spearman
cor_spearman_m_dn_Internet_dn_Sentiram_tristes_test <- cor.test(dados_m2$vp.x, 
                                                 dados_m2$vp.y, 
                                                 method = "spearman")
print(cor_spearman_m_dn_Internet_dn_Sentiram_tristes_test)

ggplot(dados_m2, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Utilização de Internet e Pessoas que se Sentiram Tristes na maioria das vezes (Mulheres)",
       x = "Média de Utilização de Internet",
       y = "Proporção de Pessoas que se Sentiram Tristes na maioria das vezes") +
  theme_minimal()

## Frequência de Utilização de Internet x Pessoas que Sentiram que Ninguém se Preocupa com Elas (Homens e Mulheres)

#Juntando dataframes
dados_combinados_3 <- dn_Internet %>%
  inner_join(dn_Ninguem_se_preocupa_com_elas, by = c("Grandes Regiões", "Sexo"))

#Correlação de Spearman
cor_spearman_dn_Internet_dn_Ninguem_se_preocupa_com_elas_test <- cor.test(dados_combinados_3$vp.x, 
                                              dados_combinados_3$vp.y, 
                                              method = "spearman")
print(cor_spearman_dn_Internet_dn_Ninguem_se_preocupa_com_elas_test)

ggplot(dados_combinados_3, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Utilização de Internet e Pessoas que Sentiram que Ninguém se Preocupa com Elas",
       x = "Média de Utilização de Internet",
       y = "Proporção de Pessoas que Sentiram que Ninguém se Preocupa com Elas") +
  theme_minimal()

## Frequência de Utilização de Internet x Pessoas que Sentiram que Ninguém se Preocupa com Elas (Homens)

# Filtrar para homens
dados_h3 <- dados_combinados_3 %>%
  filter(Sexo == "Homem")

#Correlação de Spearman
cor_spearman_h_dn_Internet_dn_Ninguem_se_preocupa_com_elas_test <- cor.test(dados_h3$vp.x, 
                                                dados_h3$vp.y, 
                                                method = "spearman")
print(cor_spearman_h_dn_Internet_dn_Ninguem_se_preocupa_com_elas_test)

ggplot(dados_h3, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Utilização de Internet e Pessoas que Sentiram que Ninguém se Preocupa com Elas (Homens)",
       x = "Média de Utilização de Internet",
       y = "Proporção de Pessoas que Sentiram que Ninguém se Preocupa com Elas") +
  theme_minimal()

## Frequência de Utilização de Internet x Pessoas que Sentiram que Ninguém se Preocupa com Elas (Mulheres)

# Filtrar para mulheres
dados_m3 <- dados_combinados_3 %>%
  filter(Sexo == "Mulher")

#Correlação de Spearman
cor_spearman_m_dn_Internet_dn_Ninguem_se_preocupa_com_elas_test <- cor.test(dados_m3$vp.x, 
                                                dados_m3$vp.y, 
                                                method = "spearman")
print(cor_spearman_m_dn_Internet_dn_Ninguem_se_preocupa_com_elas_test)

ggplot(dados_m3, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Utilização de Internet e Pessoas que Sentiram que Ninguém se Preocupa com Elas (Mulheres)",
       x = "Média de Utilização de Internet",
       y = "Proporção de Pessoas que Sentiram que Ninguém se Preocupa com Elas") +
  theme_minimal()

## Frequência de Utilização de Internet x Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas (Homens e Mulheres)

#Juntando dataframes
dados_combinados_4 <- dn_Internet %>%
  inner_join(dn_Irritadas_Nervosas_MauHumoradas, by = c("Grandes Regiões", "Sexo"))

#Correlação de Spearman
cor_spearman_dn_Internet_dn_Irritadas_Nervosas_MauHumoradas_test <- cor.test(dados_combinados_4$vp.x, 
                                               dados_combinados_4$vp.y, 
                                               method = "spearman")
print(cor_spearman_dn_Internet_dn_Irritadas_Nervosas_MauHumoradas_test)

ggplot(dados_combinados_4, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Utilização de Internet e Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas",
       x = "Média de Utilização de Internet",
       y = "Proporção de Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas") +
  theme_minimal()

## Frequência de Utilização de Internet x Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas (Homens)

# Filtrar para homens
dados_h4 <- dados_combinados_4 %>%
  filter(Sexo == "Homem")

#Correlação de Spearman
cor_spearman_h_dn_Internet_dn_Irritadas_Nervosas_MauHumoradas_test <- cor.test(dados_h4$vp.x, 
                                                 dados_h4$vp.y, 
                                                 method = "spearman")
print(cor_spearman_h_dn_Internet_dn_Irritadas_Nervosas_MauHumoradas_test)

ggplot(dados_h4, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Utilização de Internet e Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas (Homens)",
       x = "Média de Utilização de Internet",
       y = "Proporção de Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas") +
  theme_minimal()

## Frequência de Utilização de Internet x Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas (Mulheres)

# Filtrar para mulheres
dados_m4 <- dados_combinados_4 %>%
  filter(Sexo == "Mulher")

#Correlação de Spearman
cor_spearman_m_dn_Internet_dn_Irritadas_Nervosas_MauHumoradas_test <- cor.test(dados_m4$vp.x, 
                                                 dados_m4$vp.y, 
                                                 method = "spearman")
print(cor_spearman_m_dn_Internet_dn_Irritadas_Nervosas_MauHumoradas_test)

ggplot(dados_m4, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Utilização de Internet e Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas (Mulheres)",
       x = "Média de Utilização de Internet",
       y = "Proporção de Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas") +
  theme_minimal()

## Frequência de Utilização de Internet x Pessoas que Sentiram que a Vida Não Vale a Pena (Homens e Mulheres)

#Juntando dataframes
dados_combinados_5 <- dn_Internet %>%
  inner_join(dn_Vida_nao_vale_a_pena, by = c("Grandes Regiões", "Sexo"))

#Correlação de Spearman
cor_spearman_dn_Internet_dn_Vida_nao_vale_a_pena_test <- cor.test(dados_combinados_5$vp.x, 
                                           dados_combinados_5$vp.y, 
                                           method = "spearman")
print(cor_spearman_dn_Internet_dn_Vida_nao_vale_a_pena_test)

ggplot(dados_combinados_5, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Utilização de Internet e Pessoas que Sentiram que a Vida Não Vale a Pena",
       x = "Média de Utilização de Internet",
       y = "Proporção de Pessoas que Sentiram que a Vida Não Vale a Pena") +
  theme_minimal()

## Frequência de Utilização de Internet x Pessoas que Sentiram que a Vida Não Vale a Pena (Homens)

# Filtrar para homens
dados_h5 <- dados_combinados_5 %>%
  filter(Sexo == "Homem")

#Correlação de Spearman
cor_spearman_h_dn_Internet_dn_Vida_nao_vale_a_pena_test <- cor.test(dados_h5$vp.x, 
                                             dados_h5$vp.y, 
                                             method = "spearman")
print(cor_spearman_h_dn_Internet_dn_Vida_nao_vale_a_pena_test)

ggplot(dados_h5, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Utilização de Internet e Pessoas que Sentiram que a Vida Não Vale a Pena (Homens)",
       x = "Média de Utilização de Internet",
       y = "Proporção de Pessoas que Sentiram que a Vida Não Vale a Pena") +
  theme_minimal()

## Frequência de Utilização de Internet x Pessoas que Sentiram que a Vida Não Vale a Pena (Mulheres)

# Filtrar para mulheres
dados_m5 <- dados_combinados_5 %>%
  filter(Sexo == "Mulher")

#Correlação de Spearman
cor_spearman_m_dn_Internet_dn_Vida_nao_vale_a_pena_test <- cor.test(dados_m5$vp.x, 
                                             dados_m5$vp.y, 
                                             method = "spearman")
print(cor_spearman_m_dn_Internet_dn_Vida_nao_vale_a_pena_test)

ggplot(dados_m5, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Utilização de Internet e Pessoas que Sentiram que a Vida Não Vale a Pena (Mulheres)",
       x = "Média de Utilização de Internet",
       y = "Proporção de Pessoas que Sentiram que a Vida Não Vale a Pena") +
  theme_minimal()

## Pessoas que Não Tem Amigos Próximos x Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia (Homens e Mulheres)

#Correlação de Pearson
cor_pearson_dn_Sem_amgs_proximos_dn_Preocupadas_com_coisas_comuns_test <- cor.test(dn_Sem_amgs_proximos$vp, 
                                               dn_Preocupadas_com_coisas_comuns$vp, 
                                               method = "pearson")
print(cor_pearson_dn_Sem_amgs_proximos_dn_Preocupadas_com_coisas_comuns_test)

ggplot(data = NULL, aes(x = dn_Sem_amgs_proximos$vp , y = dn_Preocupadas_com_coisas_comuns$vp)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que Não Tem Amigos Próximos e Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia",
       x = "Média de Pessoas que Não Tem Amigos Próximos",
       y = "Proporção de Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia") +
  theme_minimal()

## Pessoas que Não Tem Amigos Próximos x Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia (Homens)

# Filtrar para homens
dados_h6 <- dn_Sem_amgs_proximos %>%
  inner_join(dn_Preocupadas_com_coisas_comuns, by = c("Grandes Regiões", "Sexo")) %>%
  filter(Sexo == "Homem")

#Correlação de Pearson
cor_pearson_h_dn_Sem_amgs_proximos_dn_Preocupadas_com_coisas_comuns_test <- cor.test(dados_h6$vp.x, 
                                                 dados_h6$vp.y, 
                                                 method = "pearson")
print(cor_pearson_h_dn_Sem_amgs_proximos_dn_Preocupadas_com_coisas_comuns_test)

ggplot(dados_h6, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que Não Tem Amigos Próximos e Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia (Homens)",
       x = "Média de Pessoas que Não Tem Amigos Próximos",
       y = "Proporção de Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia") +
  theme_minimal()

## Pessoas que Não Tem Amigos Próximos x Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia (Mulheres)

# Filtrar para mulheres
dados_m6 <- dn_Sem_amgs_proximos %>%
  inner_join(dn_Preocupadas_com_coisas_comuns, by = c("Grandes Regiões", "Sexo")) %>%
  filter(Sexo == "Mulher")

#Correlação de Pearson
cor_pearson_m_dn_Sem_amgs_proximos_dn_Preocupadas_com_coisas_comuns_test <- cor.test(dados_m6$vp.x, 
                                                    dados_m6$vp.y, 
                                                    method = "pearson")
print(cor_pearson_m_dn_Sem_amgs_proximos_dn_Preocupadas_com_coisas_comuns_test)

ggplot(dados_m6, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que Não Tem Amigos Próximos e Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia (Mulheres)",
       x = "Média de Pessoas que Não Tem Amigos Próximos",
       y = "Proporção de Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia") +
  theme_minimal()

## Pessoas que Não Tem Amigos Próximos x Pessoas que se Sentiram Tristes na maioria das vezes (Homens e Mulheres)

#Correlação de Spearman
cor_spearman_dn_Sem_amgs_proximos_dn_Sentiram_tristes_test <- cor.test(dn_Sem_amgs_proximos$vp, 
                                                  dn_Sentiram_tristes$vp, 
                                                  method = "spearman")
print(cor_spearman_dn_Sem_amgs_proximos_dn_Sentiram_tristes_test)

ggplot(data = NULL, aes(x = dn_Sem_amgs_proximos$vp , y = dn_Sentiram_tristes$vp)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que Não Tem Amigos Próximos e Pessoas que se Sentiram Tristes na maioria das vezes",
       x = "Média de Pessoas que Não Tem Amigos Próximos",
       y = "Proporção de Pessoas que se Sentiram Tristes na maioria das vezes") +
  theme_minimal()

## Pessoas que Não Tem Amigos Próximos x Pessoas que se Sentiram Tristes na maioria das vezes (Homens)

# Filtrar para homens
dados_h7 <- dn_Sem_amgs_proximos %>%
  inner_join(dn_Sentiram_tristes, by = c("Grandes Regiões", "Sexo")) %>%
  filter(Sexo == "Homem")

#Correlação de Spearman
cor_spearman_h_dn_Sem_amgs_proximos_dn_Sentiram_tristes_test <- cor.test(dados_h7$vp.x, 
                                                    dados_h7$vp.y, 
                                                    method = "spearman")
print(cor_spearman_h_dn_Sem_amgs_proximos_dn_Sentiram_tristes_test)

ggplot(dados_h7, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que Não Tem Amigos Próximos e Pessoas que se Sentiram Tristes na maioria das vezes (Homens)",
       x = "Média de Pessoas que Não Tem Amigos Próximos",
       y = "Proporção de Pessoas que se Sentiram Tristes na maioria das vezes") +
  theme_minimal()

## Pessoas que Não Tem Amigos Próximos x Pessoas que se Sentiram Tristes na maioria das vezes (Mulheres)

# Filtrar para mulheres
dados_m7 <- dn_Sem_amgs_proximos %>%
  inner_join(dn_Sentiram_tristes, by = c("Grandes Regiões", "Sexo")) %>%
  filter(Sexo == "Mulher")

#Correlação de Spearman
cor_spearman_m_dn_Sem_amgs_proximos_dn_Sentiram_tristes_test <- cor.test(dados_m7$vp.x, 
                                                    dados_m7$vp.y, 
                                                    method = "spearman")
print(cor_spearman_m_dn_Sem_amgs_proximos_dn_Sentiram_tristes_test)

ggplot(dados_m7, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que Não Tem Amigos Próximos e Pessoas que se Sentiram Tristes na maioria das vezes (Mulheres)",
       x = "Média de Pessoas que Não Tem Amigos Próximos",
       y = "Proporção de Pessoas que se Sentiram Tristes na maioria das vezes") +
  theme_minimal()

## Pessoas que Não Tem Amigos Próximos x Pessoas que Sentiram que Ninguém se Preocupa com Elas (Homens e Mulheres)

#Correlação de Spearman
cor_spearman_dn_Sem_amgs_proximos_dn_Ninguem_se_preocupa_com_elas_test <- cor.test(dn_Sem_amgs_proximos$vp, 
                                                  dn_Ninguem_se_preocupa_com_elas$vp, 
                                                  method = "spearman")
print(cor_spearman_dn_Sem_amgs_proximos_dn_Ninguem_se_preocupa_com_elas_test)

ggplot(data = NULL, aes(x = dn_Sem_amgs_proximos$vp , y = dn_Ninguem_se_preocupa_com_elas$vp)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que Não Tem Amigos Próximos e Pessoas que Sentiram que Ninguém se Preocupa com Elas",
       x = "Média de Pessoas que Não Tem Amigos Próximos",
       y = "Proporção de Pessoas que Sentiram que Ninguém se Preocupa com Elas") +
  theme_minimal()

## Pessoas que Não Tem Amigos Próximos x Pessoas que Sentiram que Ninguém se Preocupa com Elas (Homens)

# Filtrar para homens
dados_h8 <- dn_Sem_amgs_proximos %>%
  inner_join(dn_Ninguem_se_preocupa_com_elas, by = c("Grandes Regiões", "Sexo")) %>%
  filter(Sexo == "Homem")

#Correlação de Spearman
cor_spearman_h_dn_Sem_amgs_proximos_dn_Ninguem_se_preocupa_com_elas_test <- cor.test(dados_h8$vp.x, 
                                                    dados_h8$vp.y, 
                                                    method = "spearman")
print(cor_spearman_h_dn_Sem_amgs_proximos_dn_Ninguem_se_preocupa_com_elas_test)

ggplot(dados_h8, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que Não Tem Amigos Próximos e Pessoas que Sentiram que Ninguém se Preocupa com Elas (Homens)",
       x = "Média de Pessoas que Não Tem Amigos Próximos",
       y = "Proporção de Pessoas que Sentiram que Ninguém se Preocupa com Elas") +
  theme_minimal()

## Pessoas que Não Tem Amigos Próximos x Pessoas que Sentiram que Ninguém se Preocupa com Elas (Mulheres)

# Filtrar para mulheres
dados_m8 <- dn_Sem_amgs_proximos %>%
  inner_join(dn_Ninguem_se_preocupa_com_elas, by = c("Grandes Regiões", "Sexo")) %>%
  filter(Sexo == "Mulher")

#Correlação de Spearman
cor_spearman_m_dn_Sem_amgs_proximos_dn_Ninguem_se_preocupa_com_elas_test <- cor.test(dados_m8$vp.x, 
                                                    dados_m8$vp.y, 
                                                    method = "spearman")
print(cor_spearman_m_dn_Sem_amgs_proximos_dn_Ninguem_se_preocupa_com_elas_test)

ggplot(dados_m8, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que Não Tem Amigos Próximos e Pessoas que Sentiram que Ninguém se Preocupa com Elas (Mulheres)",
       x = "Média de Pessoas que Não Tem Amigos Próximos",
       y = "Proporção de Pessoas que Sentiram que Ninguém se Preocupa com Elas") +
  theme_minimal()

## Pessoas que Não Tem Amigos Próximos x Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas (Homens e Mulheres)

#Correlação de Spearman
cor_spearman_dn_Sem_amgs_proximos_dn_Irritadas_Nervosas_MauHumoradas_test <- cor.test(dn_Sem_amgs_proximos$vp, 
                                                   dn_Irritadas_Nervosas_MauHumoradas$vp, 
                                                   method = "spearman")
print(cor_spearman_dn_Sem_amgs_proximos_dn_Irritadas_Nervosas_MauHumoradas_test)

ggplot(data = NULL, aes(x = dn_Sem_amgs_proximos$vp , y = dn_Irritadas_Nervosas_MauHumoradas$vp)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que Não Tem Amigos Próximos e Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas",
       x = "Média de Pessoas que Não Tem Amigos Próximos",
       y = "Proporção de Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas") +
  theme_minimal()

## Pessoas que Não Tem Amigos Próximos x Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas (Homens)

# Filtrar para homens
dados_h9 <- dn_Sem_amgs_proximos %>%
  inner_join(dn_Irritadas_Nervosas_MauHumoradas, by = c("Grandes Regiões", "Sexo")) %>%
  filter(Sexo == "Homem")

#Correlação de Spearman
cor_spearman_h_dn_Sem_amgs_proximos_dn_Irritadas_Nervosas_MauHumoradas_test <- cor.test(dados_h9$vp.x, 
                                                     dados_h9$vp.y, 
                                                     method = "spearman")
print(cor_spearman_h_dn_Sem_amgs_proximos_dn_Irritadas_Nervosas_MauHumoradas_test)

ggplot(dados_h9, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que Não Tem Amigos Próximos e Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas (Homens)",
       x = "Média de Pessoas que Não Tem Amigos Próximos",
       y = "Proporção de Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas") +
  theme_minimal()

## Pessoas que Não Tem Amigos Próximos x Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas (Mulheres)

# Filtrar para mulheres
dados_m9 <- dn_Sem_amgs_proximos %>%
  inner_join(dn_Irritadas_Nervosas_MauHumoradas, by = c("Grandes Regiões", "Sexo")) %>%
  filter(Sexo == "Mulher")

#Correlação de Spearman
cor_spearman_m_dn_Sem_amgs_proximos_dn_Irritadas_Nervosas_MauHumoradas_test <- cor.test(dados_m9$vp.x, 
                                                     dados_m9$vp.y, 
                                                     method = "spearman")
print(cor_spearman_m_dn_Sem_amgs_proximos_dn_Irritadas_Nervosas_MauHumoradas_test)

ggplot(dados_m9, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que Não Tem Amigos Próximos e Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas (Mulheres)",
       x = "Média de Pessoas que Não Tem Amigos Próximos",
       y = "Proporção de Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas") +
  theme_minimal()

## Pessoas que Não Tem Amigos Próximos x Pessoas que Sentiram que a Vida Não Vale a Pena (Homens e Mulheres)

#Correlação de Spearman
cor_spearman_dn_Sem_amgs_proximos_dn_Vida_nao_vale_a_pena_test <- cor.test(dn_Sem_amgs_proximos$vp, 
                                               dn_Vida_nao_vale_a_pena$vp, 
                                               method = "spearman")
print(cor_spearman_dn_Sem_amgs_proximos_dn_Vida_nao_vale_a_pena_test)

ggplot(data = NULL, aes(x = dn_Sem_amgs_proximos$vp , y = dn_Vida_nao_vale_a_pena$vp)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que Não Tem Amigos Próximos e Pessoas que Sentiram que a Vida Não Vale a Pena",
       x = "Média de Pessoas que Não Tem Amigos Próximos",
       y = "Proporção de Pessoas que Sentiram que a Vida Não Vale a Pena") +
  theme_minimal()

## Pessoas que Não Tem Amigos Próximos x Pessoas que Sentiram que a Vida Não Vale a Pena (Homens)

# Filtrar para homens
dados_h10 <- dn_Sem_amgs_proximos %>%
  inner_join(dn_Vida_nao_vale_a_pena, by = c("Grandes Regiões", "Sexo")) %>%
  filter(Sexo == "Homem")

#Correlação de Spearman
cor_spearman_h_dn_Sem_amgs_proximos_dn_Vida_nao_vale_a_pena_test <- cor.test(dados_h10$vp.x, 
                                                 dados_h10$vp.y, 
                                                 method = "spearman")
print(cor_spearman_h_dn_Sem_amgs_proximos_dn_Vida_nao_vale_a_pena_test)

ggplot(dados_h10, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que Não Tem Amigos Próximos e Pessoas que Sentiram que a Vida Não Vale a Pena (Homens)",
       x = "Média de Pessoas que Não Tem Amigos Próximos",
       y = "Proporção de Pessoas que Sentiram que a Vida Não Vale a Pena") +
  theme_minimal()

## Pessoas que Não Tem Amigos Próximos x Pessoas que Sentiram que a Vida Não Vale a Pena (Mulheres)

# Filtrar para mulheres
dados_m10 <- dn_Sem_amgs_proximos %>%
  inner_join(dn_Vida_nao_vale_a_pena, by = c("Grandes Regiões", "Sexo")) %>%
  filter(Sexo == "Mulher")

#Correlação de Spearman
cor_spearman_m_dn_Sem_amgs_proximos_dn_Vida_nao_vale_a_pena_test <- cor.test(dados_m10$vp.x, 
                                                 dados_m10$vp.y, 
                                                 method = "spearman")
print(cor_spearman_m_dn_Sem_amgs_proximos_dn_Vida_nao_vale_a_pena_test)

ggplot(dados_m10, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que Não Tem Amigos Próximos e Pessoas que Sentiram que a Vida Não Vale a Pena (Mulheres)",
       x = "Média de Pessoas que Não Tem Amigos Próximos",
       y = "Proporção de Pessoas que Sentiram que a Vida Não Vale a Pena") +
  theme_minimal()

## Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia x Pessoas que se Sentiram Tristes na maioria das vezes (Homens e Mulheres)

#Correlação de Spearman
cor_spearman_dn_Preocupadas_com_coisas_comuns_dn_Sentiram_tristes_test <- cor.test(dn_Preocupadas_com_coisas_comuns$vp, 
                                                  dn_Sentiram_tristes$vp, 
                                                  method = "spearman")
print(cor_spearman_dn_Preocupadas_com_coisas_comuns_dn_Sentiram_tristes_test)

ggplot(data = NULL, aes(x = dn_Preocupadas_com_coisas_comuns$vp , y = dn_Sentiram_tristes$vp)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia e Pessoas que se Sentiram Tristes na maioria das vezes",
       x = "Média de Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia",
       y = "Proporção de Pessoas que se Sentiram Tristes na maioria das vezes") +
  theme_minimal()

## Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia x Pessoas que se Sentiram Tristes na maioria das vezes (Homens)

# Filtrar para homens
dados_h11 <- dn_Preocupadas_com_coisas_comuns %>%
  inner_join(dn_Sentiram_tristes, by = c("Grandes Regiões", "Sexo")) %>%
  filter(Sexo == "Homem")

#Correlação de Spearman
cor_spearman_h_dn_Preocupadas_com_coisas_comuns_dn_Sentiram_tristes_test <- cor.test(dados_h11$vp.x, 
                                                 dados_h11$vp.y, 
                                                    method = "spearman")
print(cor_spearman_h_dn_Preocupadas_com_coisas_comuns_dn_Sentiram_tristes_test)

ggplot(dados_h11, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia e Pessoas que se Sentiram Tristes na maioria das vezes (Homens)",
       x = "Média de Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia",
       y = "Proporção de Pessoas que se Sentiram Tristes na maioria das vezes") +
  theme_minimal()

## Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia x Pessoas que se Sentiram Tristes na maioria das vezes (Mulheres)

# Filtrar para mulheres
dados_m11 <- dn_Preocupadas_com_coisas_comuns %>%
  inner_join(dn_Sentiram_tristes, by = c("Grandes Regiões", "Sexo")) %>%
  filter(Sexo == "Mulher")

#Correlação de Spearman
cor_spearman_m_dn_Preocupadas_com_coisas_comuns_dn_Sentiram_tristes_test <- cor.test(dados_m11$vp.x, 
                                                 dados_m11$vp.y, 
                                                    method = "spearman")
print(cor_spearman_m_dn_Preocupadas_com_coisas_comuns_dn_Sentiram_tristes_test)

ggplot(dados_m11, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia e Pessoas que se Sentiram Tristes na maioria das vezes (Mulheres)",
       x = "Média de Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia",
       y = "Proporção de Pessoas que se Sentiram Tristes na maioria das vezes") +
  theme_minimal()

## Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia x Pessoas que Sentiram que Ninguém se Preocupa com Elas (Homens e Mulheres)

#Correlação de Spearman
cor_spearman_dn_Preocupadas_com_coisas_comuns_dn_Ninguem_se_preocupa_com_elas_test <- cor.test(dn_Preocupadas_com_coisas_comuns$vp, 
                                               dn_Ninguem_se_preocupa_com_elas$vp, 
                                               method = "spearman")
print(cor_spearman_dn_Preocupadas_com_coisas_comuns_dn_Ninguem_se_preocupa_com_elas_test)

ggplot(data = NULL, aes(x = dn_Preocupadas_com_coisas_comuns$vp , y = dn_Ninguem_se_preocupa_com_elas$vp)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia e Pessoas que Sentiram que Ninguém se Preocupa com Elas",
       x = "Média de Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia",
       y = "Proporção de Pessoas que Sentiram que Ninguém se Preocupa com Elas") +
  theme_minimal()

## Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia x Pessoas que Sentiram que Ninguém se Preocupa com Elas (Homens)

# Filtrar para homens
dados_h12 <- dn_Preocupadas_com_coisas_comuns %>%
  inner_join(dn_Ninguem_se_preocupa_com_elas, by = c("Grandes Regiões", "Sexo")) %>%
  filter(Sexo == "Homem")

#Correlação de Spearman
cor_spearman_h_dn_Preocupadas_com_coisas_comuns_dn_Ninguem_se_preocupa_com_elas_test <- cor.test(dados_h12$vp.x, 
                                                 dados_h12$vp.y, 
                                                 method = "spearman")
print(cor_spearman_h_dn_Preocupadas_com_coisas_comuns_dn_Ninguem_se_preocupa_com_elas_test)

ggplot(dados_h12, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia e Pessoas que Sentiram que Ninguém se Preocupa com Elas (Homens)",
       x = "Média de Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia",
       y = "Proporção de Pessoas que Sentiram que Ninguém se Preocupa com Elas") +
  theme_minimal()

## Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia x Pessoas que Sentiram que Ninguém se Preocupa com Elas (Mulheres)

# Filtrar para mulheres
dados_m12 <- dn_Preocupadas_com_coisas_comuns %>%
  inner_join(dn_Ninguem_se_preocupa_com_elas, by = c("Grandes Regiões", "Sexo")) %>%
  filter(Sexo == "Mulher")

#Correlação de Spearman
cor_spearman_m_dn_Preocupadas_com_coisas_comuns_dn_Ninguem_se_preocupa_com_elas_test <- cor.test(dados_m12$vp.x, 
                                                 dados_m12$vp.y, 
                                                 method = "spearman")
print(cor_spearman_m_dn_Preocupadas_com_coisas_comuns_dn_Ninguem_se_preocupa_com_elas_test)

ggplot(dados_m12, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia e Pessoas que Sentiram que Ninguém se Preocupa com Elas (Mulheres)",
       x = "Média de Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia",
       y = "Proporção de Pessoas que Sentiram que Ninguém se Preocupa com Elas") +
  theme_minimal()

## Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia x Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas (Homens e Mulheres)

#Correlação de Spearman
cor_spearman_dn_Preocupadas_com_coisas_comuns_dn_Irritadas_Nervosas_MauHumoradas_test <- cor.test(dn_Preocupadas_com_coisas_comuns$vp, 
                                                dn_Irritadas_Nervosas_MauHumoradas$vp, 
                                                method = "spearman")
print(cor_spearman_dn_Preocupadas_com_coisas_comuns_dn_Irritadas_Nervosas_MauHumoradas_test)

ggplot(data = NULL, aes(x = dn_Preocupadas_com_coisas_comuns$vp , y = dn_Irritadas_Nervosas_MauHumoradas$vp)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia e Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas",
       x = "Média de Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia",
       y = "Proporção de Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas") +
  theme_minimal()

## Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia x Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas (Homens)

# Filtrar para homens
dados_h13 <- dn_Preocupadas_com_coisas_comuns %>%
  inner_join(dn_Irritadas_Nervosas_MauHumoradas, by = c("Grandes Regiões", "Sexo")) %>%
  filter(Sexo == "Homem")

#Correlação de Spearman
cor_spearman_h_dn_Preocupadas_com_coisas_comuns_dn_Irritadas_Nervosas_MauHumoradas_test <- cor.test(dados_h13$vp.x, 
                                                  dados_h13$vp.y, 
                                                  method = "spearman")
print(cor_spearman_h_dn_Preocupadas_com_coisas_comuns_dn_Irritadas_Nervosas_MauHumoradas_test)

ggplot(dados_h13, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia e Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas (Homens)",
       x = "Média de Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia",
       y = "Proporção de Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas") +
  theme_minimal()

## Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia x Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas (Mulheres)

# Filtrar para mulheres
dados_m13 <- dn_Preocupadas_com_coisas_comuns %>%
  inner_join(dn_Irritadas_Nervosas_MauHumoradas, by = c("Grandes Regiões", "Sexo")) %>%
  filter(Sexo == "Mulher")

#Correlação de Spearman
cor_spearman_m_dn_Preocupadas_com_coisas_comuns_dn_Irritadas_Nervosas_MauHumoradas_test <- cor.test(dados_m13$vp.x, 
                                                  dados_m13$vp.y, 
                                                  method = "spearman")
print(cor_spearman_m_dn_Preocupadas_com_coisas_comuns_dn_Irritadas_Nervosas_MauHumoradas_test)

ggplot(dados_m13, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia e Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas (Mulheres)",
       x = "Média de Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia",
       y = "Proporção de Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas") +
  theme_minimal()

## Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia x Pessoas que Sentiram que a Vida Não Vale a Pena (Homens e Mulheres)

#Correlação de Spearman
cor_spearman_dn_Preocupadas_com_coisas_comuns_dn_Vida_nao_vale_a_pena_test <- cor.test(dn_Preocupadas_com_coisas_comuns$vp, 
                                            dn_Vida_nao_vale_a_pena$vp, 
                                            method = "spearman")
print(cor_spearman_dn_Preocupadas_com_coisas_comuns_dn_Vida_nao_vale_a_pena_test)

ggplot(data = NULL, aes(x = dn_Preocupadas_com_coisas_comuns$vp , y = dn_Vida_nao_vale_a_pena$vp)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia e Pessoas que Sentiram que a Vida Não Vale a Pena",
       x = "Média de Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia",
       y = "Proporção de Pessoas que Sentiram que a Vida Não Vale a Pena") +
  theme_minimal()

## Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia x Pessoas que Sentiram que a Vida Não Vale a Pena (Homens)

# Filtrar para homens
dados_h14 <- dn_Preocupadas_com_coisas_comuns %>%
  inner_join(dn_Vida_nao_vale_a_pena, by = c("Grandes Regiões", "Sexo")) %>%
  filter(Sexo == "Homem")

#Correlação de Spearman
cor_spearman_h_dn_Preocupadas_com_coisas_comuns_dn_Vida_nao_vale_a_pena_test <- cor.test(dados_h14$vp.x, 
                                              dados_h14$vp.y, 
                                              method = "spearman")
print(cor_spearman_h_dn_Preocupadas_com_coisas_comuns_dn_Vida_nao_vale_a_pena_test)

ggplot(dados_h14, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia e Pessoas que Sentiram que a Vida Não Vale a Pena (Homens)",
       x = "Média de Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia",
       y = "Proporção de Pessoas que Sentiram que a Vida Não Vale a Pena") +
  theme_minimal()

## Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia x Pessoas que Sentiram que a Vida Não Vale a Pena (Mulheres)

# Filtrar para mulheres
dados_m14 <- dn_Preocupadas_com_coisas_comuns %>%
  inner_join(dn_Vida_nao_vale_a_pena, by = c("Grandes Regiões", "Sexo")) %>%
  filter(Sexo == "Mulher")

#Correlação de Spearman
cor_spearman_m_dn_Preocupadas_com_coisas_comuns_dn_Vida_nao_vale_a_pena_test <- cor.test(dados_m14$vp.x, 
                                              dados_m14$vp.y, 
                                              method = "spearman")
print(cor_spearman_m_dn_Preocupadas_com_coisas_comuns_dn_Vida_nao_vale_a_pena_test)

ggplot(dados_m14, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia e Pessoas que Sentiram que a Vida Não Vale a Pena (Mulheres)",
       x = "Média de Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia",
       y = "Proporção de Pessoas que Sentiram que a Vida Não Vale a Pena") +
  theme_minimal()

## Pessoas que se Sentiram Tristes na maioria das vezes x Pessoas que Sentiram que Ninguém se Preocupa com Elas (Homens e Mulheres)

#Correlação de Spearman
cor_spearman_dn_Sentiram_tristes_dn_Ninguem_se_preocupa_com_elas_test <- cor.test(dn_Sentiram_tristes$vp, 
                                                dn_Ninguem_se_preocupa_com_elas$vp, 
                                                method = "spearman")
print(cor_spearman_dn_Sentiram_tristes_dn_Ninguem_se_preocupa_com_elas_test)

ggplot(data = NULL, aes(x = dn_Sentiram_tristes$vp , y = dn_Ninguem_se_preocupa_com_elas$vp)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que se Sentiram Tristes na maioria das vezes e Pessoas que Sentiram que Ninguém se Preocupa com Elas",
       x = "Média de Pessoas que se Sentiram Tristes na maioria das vezes",
       y = "Proporção de Pessoas que Sentiram que Ninguém se Preocupa com Elas") +
  theme_minimal()

## Pessoas que se Sentiram Tristes na maioria das vezes x Pessoas que Sentiram que Ninguém se Preocupa com Elas (Homens)

# Filtrar para homens
dados_h15 <- dn_Sentiram_tristes %>%
  inner_join(dn_Ninguem_se_preocupa_com_elas, by = c("Grandes Regiões", "Sexo")) %>%
  filter(Sexo == "Homem")

#Correlação de Spearman
cor_spearman_h_dn_Sentiram_tristes_dn_Ninguem_se_preocupa_com_elas_test <- cor.test(dados_h15$vp.x, 
                                                  dados_h15$vp.y, 
                                                  method = "spearman")
print(cor_spearman_h_dn_Sentiram_tristes_dn_Ninguem_se_preocupa_com_elas_test)

ggplot(dados_h15, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que se Sentiram Tristes na maioria das vezes e Pessoas que Sentiram que Ninguém se Preocupa com Elas (Homens)",
       x = "Média de Pessoas que se Sentiram Tristes na maioria das vezes",
       y = "Proporção de Pessoas que Sentiram que Ninguém se Preocupa com Elas") +
  theme_minimal()

## Pessoas que se Sentiram Tristes na maioria das vezes x Pessoas que Sentiram que Ninguém se Preocupa com Elas (Mulheres)

# Filtrar para mulheres
dados_m15 <- dn_Sentiram_tristes %>%
  inner_join(dn_Ninguem_se_preocupa_com_elas, by = c("Grandes Regiões", "Sexo")) %>%
  filter(Sexo == "Mulher")

#Correlação de Spearman
cor_spearman_m_dn_Sentiram_tristes_dn_Ninguem_se_preocupa_com_elas_test <- cor.test(dados_m15$vp.x, 
                                                  dados_m15$vp.y, 
                                                  method = "spearman")
print(cor_spearman_m_dn_Sentiram_tristes_dn_Ninguem_se_preocupa_com_elas_test)

ggplot(dados_m15, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que se Sentiram Tristes na maioria das vezes e Pessoas que Sentiram que Ninguém se Preocupa com Elas (Mulheres)",
       x = "Média de Pessoas que se Sentiram Tristes na maioria das vezes",
       y = "Proporção de Pessoas que Sentiram que Ninguém se Preocupa com Elas") +
  theme_minimal()

## Pessoas que se Sentiram Tristes na maioria das vezes x Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas (Homens e Mulheres)

#Correlação de Spearman
cor_spearman_dn_Sentiram_tristes_dn_Irritadas_Nervosas_MauHumoradas_test <- cor.test(dn_Sentiram_tristes$vp, 
                                               dn_Irritadas_Nervosas_MauHumoradas$vp, 
                                               method = "spearman")
print(cor_spearman_dn_Sentiram_tristes_dn_Irritadas_Nervosas_MauHumoradas_test)

ggplot(data = NULL, aes(x = dn_Sentiram_tristes$vp , y = dn_Irritadas_Nervosas_MauHumoradas$vp)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que se Sentiram Tristes na maioria das vezes e Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas",
       x = "Média de Pessoas que se Sentiram Tristes na maioria das vezes",
       y = "Proporção de Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas") +
  theme_minimal()

## Pessoas que se Sentiram Tristes na maioria das vezes x Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas (Homens)

# Filtrar para homens
dados_h16 <- dn_Sentiram_tristes %>%
  inner_join(dn_Irritadas_Nervosas_MauHumoradas, by = c("Grandes Regiões", "Sexo")) %>%
  filter(Sexo == "Homem")

#Correlação de Spearman
cor_spearman_h_dn_Sentiram_tristes_dn_Irritadas_Nervosas_MauHumoradas_test <- cor.test(dados_h16$vp.x, 
                                                 dados_h16$vp.y, 
                                                 method = "spearman")
print(cor_spearman_h_dn_Sentiram_tristes_dn_Irritadas_Nervosas_MauHumoradas_test)

ggplot(dados_h16, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que se Sentiram Tristes na maioria das vezes e Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas (Homens)",
       x = "Média de Pessoas que se Sentiram Tristes na maioria das vezes",
       y = "Proporção de Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas") +
  theme_minimal()

## Pessoas que se Sentiram Tristes na maioria das vezes x Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas (Mulheres)

# Filtrar para mulheres
dados_m16 <- dn_Sentiram_tristes %>%
  inner_join(dn_Irritadas_Nervosas_MauHumoradas, by = c("Grandes Regiões", "Sexo")) %>%
  filter(Sexo == "Mulher")

#Correlação de Spearman
cor_spearman_m_dn_Sentiram_tristes_dn_Irritadas_Nervosas_MauHumoradas_test <- cor.test(dados_m16$vp.x, 
                                                 dados_m16$vp.y, 
                                                 method = "spearman")
print(cor_spearman_m_dn_Sentiram_tristes_dn_Irritadas_Nervosas_MauHumoradas_test)

ggplot(dados_m16, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que se Sentiram Tristes na maioria das vezes e Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas (Mulheres)",
       x = "Média de Pessoas que se Sentiram Tristes na maioria das vezes",
       y = "Proporção de Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas") +
  theme_minimal()

## Pessoas que se Sentiram Tristes na maioria das vezes x Pessoas que Sentiram que a Vida Não Vale a Pena (Homens e Mulheres)

#Correlação de Spearman
cor_spearman_dn_Sentiram_tristes_dn_Vida_nao_vale_a_pena_test <- cor.test(dn_Sentiram_tristes$vp, 
                                           dn_Vida_nao_vale_a_pena$vp, 
                                           method = "spearman")
print(cor_spearman_dn_Sentiram_tristes_dn_Vida_nao_vale_a_pena_test)

ggplot(data = NULL, aes(x = dn_Sentiram_tristes$vp , y = dn_Vida_nao_vale_a_pena$vp)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que se Sentiram Tristes na maioria das vezes e Pessoas que Sentiram que a Vida Não Vale a Pena",
       x = "Média de Pessoas que se Sentiram Tristes na maioria das vezes",
       y = "Proporção de Pessoas que Sentiram que a Vida Não Vale a Pena") +
  theme_minimal()

## Pessoas que se Sentiram Tristes na maioria das vezes x Pessoas que Sentiram que a Vida Não Vale a Pena (Homens)

# Filtrar para homens
dados_h17 <- dn_Sentiram_tristes %>%
  inner_join(dn_Vida_nao_vale_a_pena, by = c("Grandes Regiões", "Sexo")) %>%
  filter(Sexo == "Homem")

#Correlação de Spearman
cor_spearman_h_dn_Sentiram_tristes_dn_Vida_nao_vale_a_pena_test <- cor.test(dados_h17$vp.x, 
                                             dados_h17$vp.y, 
                                             method = "spearman")
print(cor_spearman_h_dn_Sentiram_tristes_dn_Vida_nao_vale_a_pena_test)

ggplot(dados_h17, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que se Sentiram Tristes na maioria das vezes e Pessoas que Sentiram que a Vida Não Vale a Pena (Homens)",
       x = "Média de Pessoas que se Sentiram Tristes na maioria das vezes",
       y = "Proporção de Pessoas que Sentiram que a Vida Não Vale a Pena") +
  theme_minimal()

## Pessoas que se Sentiram Tristes na maioria das vezes x Pessoas que Sentiram que a Vida Não Vale a Pena (Mulheres)

# Filtrar para mulheres
dados_m17 <- dn_Sentiram_tristes %>%
  inner_join(dn_Vida_nao_vale_a_pena, by = c("Grandes Regiões", "Sexo")) %>%
  filter(Sexo == "Mulher")

#Correlação de Spearman
cor_spearman_m_dn_Sentiram_tristes_dn_Vida_nao_vale_a_pena_test <- cor.test(dados_m17$vp.x, 
                                             dados_m17$vp.y, 
                                             method = "spearman")
print(cor_spearman_m_dn_Sentiram_tristes_dn_Vida_nao_vale_a_pena_test)

ggplot(dados_m17, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que se Sentiram Tristes na maioria das vezes e Pessoas que Sentiram que a Vida Não Vale a Pena (Mulheres)",
       x = "Média de Pessoas que se Sentiram Tristes na maioria das vezes",
       y = "Proporção de Pessoas que Sentiram que a Vida Não Vale a Pena") +
  theme_minimal()

## Pessoas que Sentiram que Ninguém se Preocupa com Elas x Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas (Homens e Mulheres)

#Correlação de Spearman
cor_spearman_dn_Ninguem_se_preocupa_com_elas_dn_Irritadas_Nervosas_MauHumoradas_test <- cor.test(dn_Ninguem_se_preocupa_com_elas$vp, 
                                           dn_Irritadas_Nervosas_MauHumoradas$vp, 
                                           method = "spearman")
print(cor_spearman_dn_Ninguem_se_preocupa_com_elas_dn_Irritadas_Nervosas_MauHumoradas_test)

ggplot(data = NULL, aes(x = dn_Ninguem_se_preocupa_com_elas$vp , y = dn_Irritadas_Nervosas_MauHumoradas$vp)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que Sentiram que Ninguém se Preocupa com Elas e Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas",
       x = "Média de Pessoas que Sentiram que Ninguém se Preocupa com Elas",
       y = "Proporção de Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas") +
  theme_minimal()

## Pessoas que Sentiram que Ninguém se Preocupa com Elas x Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas (Homens)

# Filtrar para homens
dados_h18 <- dn_Ninguem_se_preocupa_com_elas %>%
  inner_join(dn_Irritadas_Nervosas_MauHumoradas, by = c("Grandes Regiões", "Sexo")) %>%
  filter(Sexo == "Homem")

#Correlação de Spearman
cor_spearman_h_dn_Ninguem_se_preocupa_com_elas_dn_Irritadas_Nervosas_MauHumoradas_test <- cor.test(dados_h18$vp.x, 
                                             dados_h18$vp.y, 
                                             method = "spearman")
print(cor_spearman_h_dn_Ninguem_se_preocupa_com_elas_dn_Irritadas_Nervosas_MauHumoradas_test)

ggplot(dados_h18, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que Sentiram que Ninguém se Preocupa com Elas e Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas (Homens)",
       x = "Média de Pessoas que Sentiram que Ninguém se Preocupa com Elas",
       y = "Proporção de Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas") +
  theme_minimal()

## Pessoas que Sentiram que Ninguém se Preocupa com Elas x Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas (Mulheres)

# Filtrar para mulheres
dados_m18 <- dn_Ninguem_se_preocupa_com_elas %>%
  inner_join(dn_Irritadas_Nervosas_MauHumoradas, by = c("Grandes Regiões", "Sexo")) %>%
  filter(Sexo == "Mulher")

#Correlação de Spearman
cor_spearman_m_dn_Ninguem_se_preocupa_com_elas_dn_Irritadas_Nervosas_MauHumoradas_test <- cor.test(dados_m18$vp.x, 
                                             dados_m18$vp.y, 
                                             method = "spearman")
print(cor_spearman_m_dn_Ninguem_se_preocupa_com_elas_dn_Irritadas_Nervosas_MauHumoradas_test)

ggplot(dados_m18, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que Sentiram que Ninguém se Preocupa com Elas e Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas (Mulheres)",
       x = "Média de Pessoas que Sentiram que Ninguém se Preocupa com Elas",
       y = "Proporção de Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas") +
  theme_minimal()

## Pessoas que Sentiram que Ninguém se Preocupa com Elas x Pessoas que Sentiram que a Vida Não Vale a Pena (Homens e Mulheres)

#Correlação de Spearman
cor_spearman_dn_Ninguem_se_preocupa_com_elas_dn_Vida_nao_vale_a_pena_test <- cor.test(dn_Ninguem_se_preocupa_com_elas$vp, 
                                            dn_Vida_nao_vale_a_pena$vp, 
                                            method = "spearman")
print(cor_spearman_dn_Ninguem_se_preocupa_com_elas_dn_Vida_nao_vale_a_pena_test)

ggplot(data = NULL, aes(x = dn_Ninguem_se_preocupa_com_elas$vp , y = dn_Vida_nao_vale_a_pena$vp)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que Sentiram que Ninguém se Preocupa com Elas e Pessoas que Sentiram que a Vida Não Vale a Pena",
       x = "Média de Pessoas que Sentiram que Ninguém se Preocupa com Elas",
       y = "Proporção de Pessoas que Sentiram que a Vida Não Vale a Pena") +
  theme_minimal()

## Pessoas que Sentiram que Ninguém se Preocupa com Elas x Pessoas que Sentiram que a Vida Não Vale a Pena (Homens)

# Filtrar para homens
dados_h19 <- dn_Ninguem_se_preocupa_com_elas %>%
  inner_join(dn_Vida_nao_vale_a_pena, by = c("Grandes Regiões", "Sexo")) %>%
  filter(Sexo == "Homem")

#Correlação de Spearman
cor_spearman_h_dn_Ninguem_se_preocupa_com_elas_dn_Vida_nao_vale_a_pena_test <- cor.test(dados_h19$vp.x, 
                                              dados_h19$vp.y, 
                                              method = "spearman")
print(cor_spearman_h_dn_Ninguem_se_preocupa_com_elas_dn_Vida_nao_vale_a_pena_test)

ggplot(dados_h19, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que Sentiram que Ninguém se Preocupa com Elas e Pessoas que Sentiram que a Vida Não Vale a Pena (Homens)",
       x = "Média de Pessoas que Sentiram que Ninguém se Preocupa com Elas",
       y = "Proporção de Pessoas que Sentiram que a Vida Não Vale a Pena") +
  theme_minimal()

## Pessoas que Sentiram que Ninguém se Preocupa com Elas x Pessoas que Sentiram que a Vida Não Vale a Pena (Mulheres)

# Filtrar para mulheres
dados_m19 <- dn_Ninguem_se_preocupa_com_elas %>%
  inner_join(dn_Vida_nao_vale_a_pena, by = c("Grandes Regiões", "Sexo")) %>%
  filter(Sexo == "Mulher")

#Correlação de Spearman
cor_spearman_m_dn_Ninguem_se_preocupa_com_elas_dn_Vida_nao_vale_a_pena_test <- cor.test(dados_m19$vp.x, 
                                              dados_m19$vp.y, 
                                              method = "spearman")
print(cor_spearman_m_dn_Ninguem_se_preocupa_com_elas_dn_Vida_nao_vale_a_pena_test)

ggplot(dados_m19, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que Sentiram que Ninguém se Preocupa com Elas e Pessoas que Sentiram que a Vida Não Vale a Pena (Mulheres)",
       x = "Média de Pessoas que Sentiram que Ninguém se Preocupa com Elas",
       y = "Proporção de Pessoas que Sentiram que a Vida Não Vale a Pena") +
  theme_minimal()

## Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas x Pessoas que Sentiram que a Vida Não Vale a Pena (Homens e Mulheres)

#Correlação de Spearman
cor_spearman_dn_Irritadas_Nervosas_MauHumoradas_dn_Vida_nao_vale_a_pena_test <- cor.test(dn_Irritadas_Nervosas_MauHumoradas$vp, 
                                               dn_Vida_nao_vale_a_pena$vp, 
                                               method = "spearman")
print(cor_spearman_dn_Irritadas_Nervosas_MauHumoradas_dn_Vida_nao_vale_a_pena_test)

ggplot(data = NULL, aes(x = dn_Irritadas_Nervosas_MauHumoradas$vp , y = dn_Vida_nao_vale_a_pena$vp)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas e Pessoas que Sentiram que a Vida Não Vale a Pena",
       x = "Média de Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas",
       y = "Proporção de Pessoas que Sentiram que a Vida Não Vale a Pena") +
  theme_minimal()

## Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas x Pessoas que Sentiram que a Vida Não Vale a Pena (Homens)

# Filtrar para homens
dados_h20 <- dn_Irritadas_Nervosas_MauHumoradas %>%
  inner_join(dn_Vida_nao_vale_a_pena, by = c("Grandes Regiões", "Sexo")) %>%
  filter(Sexo == "Homem")

#Correlação de Spearman
cor_spearman_h_dn_Irritadas_Nervosas_MauHumoradas_dn_Vida_nao_vale_a_pena_test <- cor.test(dados_h20$vp.x, 
                                                 dados_h20$vp.y, 
                                                 method = "spearman")
print(cor_spearman_h_dn_Irritadas_Nervosas_MauHumoradas_dn_Vida_nao_vale_a_pena_test)

ggplot(dados_h20, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas e Pessoas que Sentiram que a Vida Não Vale a Pena (Homens)",
       x = "Média de Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas",
       y = "Proporção de Pessoas que Sentiram que a Vida Não Vale a Pena") +
  theme_minimal()

## Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas x Pessoas que Sentiram que a Vida Não Vale a Pena (Mulheres)

# Filtrar para mulheres
dados_m20 <- dn_Irritadas_Nervosas_MauHumoradas %>%
  inner_join(dn_Vida_nao_vale_a_pena, by = c("Grandes Regiões", "Sexo")) %>%
  filter(Sexo == "Mulher")

#Correlação de Spearman
cor_spearman_m_dn_Irritadas_Nervosas_MauHumoradas_dn_Vida_nao_vale_a_pena_test <- cor.test(dados_m20$vp.x, 
                                                 dados_m20$vp.y, 
                                                 method = "spearman")
print(cor_spearman_m_dn_Irritadas_Nervosas_MauHumoradas_dn_Vida_nao_vale_a_pena_test)

ggplot(dados_m20, aes(x = vp.x, y = vp.y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dados
  geom_smooth(method = "lm", col = "red") +  # Adiciona a linha de regressão
  labs(title = "Relação entre Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas e Pessoas que Sentiram que a Vida Não Vale a Pena (Mulheres)",
       x = "Média de Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas",
       y = "Proporção de Pessoas que Sentiram que a Vida Não Vale a Pena") +
  theme_minimal()

##########################################

  ## T-test

# Homens e Mulheres para "Frequência de Utilização de Internet"
t_test_0 <- t.test(dn_Internet$vp ~ dn_Sem_amgs_proximos$`Grandes Regiões`)

print(t_test_0)

# Homens e Mulheres para "Pessoas que Não Tem Amigos Próximos"
t_test_n_amgs_prox <- t.test(Mdn_n_amgs_prox$Mdn_n_amgs_prox ~ Mdn_n_amgs_prox$Sexo)

print(t_test_n_amgs_prox)

# Homens e Mulheres para "Pessoas que se Sentiram Preocupados Com Coisas Comuns do Dia-a-Dia"
t_test_p_ccomuns <- t.test(Mdn_p_ccomuns$Mdn_p_ccomuns ~ Mdn_p_ccomuns$Sexo)

print(t_test_p_ccomuns)

# Homens e Mulheres para "Pessoas que se Sentiram Tristes na maioria das vezes"
t_test_s_triste <- t.test(Mdn_s_triste$Mdn_s_triste ~ Mdn_s_triste$Sexo)

print(t_test_s_triste)

# Homens e Mulheres para "Pessoas que Sentiram que Ninguém se Preocupa com Elas"
t_test_nin_pre_elas <- t.test(Mdn_nin_pre_elas$Mdn_nin_pre_elas ~ Mdn_nin_pre_elas$Sexo)

print(t_test_nin_pre_elas)

# Homens e Mulheres para "Pessoas que Sentiram Irritadas, Nervosas ou Mau-Humoradas"
t_test_INMH <- t.test(Mdn_INMH$Mdn_INMH ~ Mdn_INMH$Sexo)

print(t_test_INMH)

# Homens e Mulheres para "Pessoas que Sentiram que a Vida Não Vale a Pena"
t_test_vida_n_vp <- t.test(Mdn_vida_n_vp$Mdn_vida_n_vp ~ Mdn_vida_n_vp$Sexo)

print(t_test_vida_n_vp)

###############

  ## Matriz

# Juntando os datasets pelo que eles têm em comum
todos_dados <- dn_Internet %>%
  inner_join(dn_Sem_amgs_proximos, by = c("Grandes Regiões", "Sexo")) %>%
  inner_join(dn_Preocupadas_com_coisas_comuns, by = c("Grandes Regiões", "Sexo")) %>%
  inner_join(dn_Sentiram_tristes, by = c("Grandes Regiões", "Sexo")) %>%
  inner_join(dn_Ninguem_se_preocupa_com_elas, by = c("Grandes Regiões", "Sexo")) %>%
  inner_join(dn_Irritadas_Nervosas_MauHumoradas, by = c("Grandes Regiões", "Sexo")) %>%
  inner_join(dn_Vida_nao_vale_a_pena, by = c("Grandes Regiões", "Sexo"))

# Filtrando apenas as variáveis `vp`
todos_dados_vp <- todos_dados %>%
  select(contains("vp"))

colnames(todos_dados_vp) <- c("Internet_vp", "Sem_amgs_proximos_vp", "Preocupadas_com_coisas_comuns_vp", "Sentiram_tristes_vp", "Ninguem_se_preocupa_com_elas_vp", "Irritadas_Nervosas_MauHumoradas_vp", "Vida_nao_vale_a_pena_vp")
# Verifique se as colunas estão como esperado
print(names(todos_dados_vp))

chart.Correlation(todos_dados_vp, histogram=TRUE, pch=19)
