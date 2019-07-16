###### ANÁLISE AD-HOC INTERNAÇÕES HOSPITALARES ######

# CARREGAR PACOTES

library(data.table)
library(readr)
library(plyr)
library(dplyr)
library(tidyverse)
library(foreign)
library(reshape2)
library(car)
library(lubridate)
library(zoo)
library(DescTools)
library(gplots)
library(ggplot2)
library(ggthemes)
library(colourpicker)


#### ABRIR DATASETs ESTADOS ####

memory.size(max=10^12)

# SETAR DIRETÓRIO

setwd("G:/Economia/1_Base de Informações/1 - D-TISS [NOVA] - USAR ESTA AQUI/TISS - SET2017/HOSPITALAR")

# LISTAR OS ARQUIVOS E LER 

temp = list.files(pattern="*_DET.csv")
files = list2env(lapply(setNames(temp,make.names(gsub("*.csv$","",temp))),
                        read.csv),envir=.GlobalEnv)
rm(temp,files)

memory.size(max=10^12)
df_list <- sapply(.GlobalEnv, is.data.frame)
TISS_BR_HOSP <- do.call("rbind",mget(names(df_list)[df_list]))


#### SELECIONAR PROCEDIMENTOS ####

proced <- c("31102379",
            "41101227",
            "41101316",
            "41102010",
            "31102379",
            "31102360",
            "31102565",
            "30713137")

PROCED_SELECIONADOS <-  
  TISS_BR_HOSP %>%
  filter(CD_TUSS_PROCEDIMENTO %in% proced)

#### TRANSFORMAR A BASE ####

PROCED_SELEC_QTD <-
  PROCED_SELECIONADOS %>%
  ## EDITAR VARIÁVEIS DE DATA
  mutate(
    # Extrair Mês e Ano
    MES_EVENTO=paste(substr(DT_INICIO_EVENTO,4,5),substr(DT_INICIO_EVENTO,7,10),sep="-")
  ) %>%
  ## SELECIONAR VARIÁVEIS DE INTERESSE
  select(
    # Infos procedimento
    CD_TUSS_PROCEDIMENTO,
    # Datas
    MES_EVENTO,
    # Localização
    UF_PRESTADOR,
    # Quantidade Procedimentos
    QT_PROCEDIMENTO
  ) %>%
  dcast(.,formula=CD_TUSS_PROCEDIMENTO+UF_PRESTADOR~MES_EVENTO,
        fun.aggregate=sum,value.var="QT_PROCEDIMENTO")

PROCED_SELEC_VALOR <-
  PROCED_SELECIONADOS %>%
  ## EDITAR VARIÁVEIS DE DATA
  mutate(
    # Extrair Mês e Ano
    MES_EVENTO=paste(substr(DT_INICIO_EVENTO,4,5),substr(DT_INICIO_EVENTO,7,10),sep="-")
  ) %>%
  ## SELECIONAR VARIÁVEIS DE INTERESSE
  select(
    # Infos procedimento
    CD_TUSS_PROCEDIMENTO,
    # Datas
    MES_EVENTO,
    # Localização
    UF_PRESTADOR,
    # Valor Procedimentos
    VL_PROCEDIMENTO
  ) %>%
  dcast(.,formula=CD_TUSS_PROCEDIMENTO+UF_PRESTADOR~MES_EVENTO,
        fun.aggregate=sum,value.var="VL_PROCEDIMENTO")

#### EXPORTAR BASE ####

write.csv(PROCED_SELEC_QTD,file="G:/Economia/1_Base de Informações/1 - D-TISS [NOVA] - USAR ESTA AQUI/Pedido UHG/PROCED_SELEC_QTD2.csv")
    
write.csv(PROCED_SELEC_VALOR,file="G:/Economia/1_Base de Informações/1 - D-TISS [NOVA] - USAR ESTA AQUI/Pedido UHG/PROCED_SELEC_VALOR2.csv")




