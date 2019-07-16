#### PACOTES ####

library(gsubfn)
library(data.table)
library(plyr)
library(dplyr)
library(tidyverse)
library(reshape2)
library(foreign)
library(car)
library(lubridate)
library(zoo)
library(DescTools)
library(gplots)
library(ggplot2)
library(ggthemes)
library(colourpicker)


#### IMPORTAR AS BASES DE DADOS - SÃO PAULO ####

setwd("G:/Economia/1_Base de Informações/ANS - Info Consolidadas de Beneficiários/Histórico Bases/Bases/")

memory.size(max=10^12)
gc(TRUE)
ben201612_SP <- fread("ben201612_SP.csv")
ben201701_SP <- fread("ben201701_SP.csv")
ben201702_SP <- fread("ben201702_SP.csv")
ben201703_SP <- fread("ben201703_SP.csv")
ben201704_SP <- fread("ben201704_SP.csv")
ben201705_SP <- fread("ben201705_SP.csv")
ben201706_SP <- fread("ben201706_SP.csv")
ben201707_SP <- fread("ben201707_SP.csv")
ben201708_SP <- fread("ben201708_SP.csv")
ben201709_SP <- fread("ben201709_SP.csv")
ben201710_SP <- fread("ben201710_SP.csv")
ben201711_SP <- fread("ben201711_SP.csv")
ben201712_SP <- fread("ben201712_SP.csv")
ben201801_SP <- fread("ben201801_SP.csv")


memory.size(max=10^12)
gc(TRUE)
BENEF_SP1 <- do.call("rbind", list(ben201612_SP,
                                   ben201701_SP,ben201702_SP,ben201703_SP,
                                   ben201704_SP,ben201705_SP,ben201706_SP,
                                   ben201707_SP,ben201708_SP,ben201709_SP,
                                   ben201710_SP,ben201711_SP,ben201712_SP,
                                   ben201801_SP))
memory.size(max=10^12)
gc(TRUE)
rm(ben201612_SP,
   ben201701_SP,ben201702_SP,ben201703_SP,
   ben201704_SP,ben201705_SP,ben201706_SP,
   ben201707_SP,ben201708_SP,ben201709_SP,
   ben201710_SP,ben201711_SP,ben201712_SP,
   ben201801_SP)

memory.size(max=10^12)
gc(TRUE)
BENEF_SP_ATIVO1 <- 
  BENEF_SP1 %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ATIVO=sum(QT_BENEFICIARIO_ATIVO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ATIVO)

memory.size(max=10^12)
gc(TRUE)
BENEF_SP_ADERIDO1 <- 
  BENEF_SP1 %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ADERIDO=sum(QT_BENEFICIARIO_ADERIDO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ADERIDO)

memory.size(max=10^12)
gc(TRUE)
BENEF_SP_CANCELADO1 <- 
  BENEF_SP1 %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_CANCELADO=sum(QT_BENEFICIARIO_CANCELADO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_CANCELADO)

rm(BENEF_SP1)

memory.size(max=10^12)
gc(TRUE)
write.csv(BENEF_SP_ATIVO1,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_SP_ATIVO1.csv")
write.csv(BENEF_SP_ADERIDO1,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_SP_ADERIDO1.csv")
write.csv(BENEF_SP_CANCELADO1,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_SP_CANCELADO1.csv")

rm(BENEF_SP_ATIVO1,BENEF_SP_ADERIDO1,BENEF_SP_CANCELADO1)


memory.size(max=10^12)
gc(TRUE)
ben201802_SP <- fread("ben201802_SP.csv")
ben201803_SP <- fread("ben201803_SP.csv")
ben201804_SP <- fread("ben201804_SP.csv")
ben201805_SP <- fread("ben201805_SP.csv")
ben201806_SP <- fread("ben201806_SP.csv")
ben201807_SP <- fread("ben201807_SP.csv")
ben201808_SP <- fread("ben201808_SP.csv")
ben201809_SP <- fread("ben201809_SP.csv")
ben201810_SP <- fread("ben201810_SP.csv")
ben201811_SP <- fread("ben201811_SP.csv")
ben201812_SP <- fread("ben201812_SP.csv")
ben201901_SP <- fread("ben201901_SP.csv")
ben201902_SP <- fread("ben201902_SP.csv")
ben201903_SP <- fread("ben201903_SP.csv")



BENEF_SP2 <- do.call("rbind", list(ben201802_SP,ben201803_SP,
                                   ben201804_SP,ben201805_SP,ben201806_SP,
                                   ben201807_SP,ben201808_SP,ben201809_SP,
                                   ben201810_SP,ben201811_SP,ben201812_SP,
                                   ben201901_SP,ben201902_SP,ben201903_SP))
memory.size(max=10^12)
gc(TRUE)
rm(ben201802_SP,ben201803_SP,
   ben201804_SP,ben201805_SP,ben201806_SP,
   ben201807_SP,ben201808_SP,ben201809_SP,
   ben201810_SP,ben201811_SP,ben201812_SP,
   ben201901_SP,ben201902_SP,ben201903_SP)


memory.size(max=10^12)
gc(TRUE)
BENEF_SP_ATIVO2 <- 
  BENEF_SP2 %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ATIVO=sum(QT_BENEFICIARIO_ATIVO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ATIVO)

memory.size(max=10^12)
gc(TRUE)
BENEF_SP_ADERIDO2 <- 
  BENEF_SP2 %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ADERIDO=sum(QT_BENEFICIARIO_ADERIDO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ADERIDO)

memory.size(max=10^12)
gc(TRUE)
BENEF_SP_CANCELADO2 <- 
  BENEF_SP2 %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_CANCELADO=sum(QT_BENEFICIARIO_CANCELADO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_CANCELADO)

rm(BENEF_SP2)

memory.size(max=10^12)
gc(TRUE)
write.csv(BENEF_SP_ATIVO2,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_SP_ATIVO2.csv")
write.csv(BENEF_SP_ADERIDO2,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_SP_ADERIDO2.csv")
write.csv(BENEF_SP_CANCELADO2,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_SP_CANCELADO2.csv")

rm(BENEF_SP_ATIVO2,BENEF_SP_ADERIDO2,BENEF_SP_CANCELADO2)


#### IMPORTAR AS BASES DE DADOS - RIO DE JANEIRO ####

setwd("G:/Economia/1_Base de Informações/ANS - Info Consolidadas de Beneficiários/Histórico Bases/Bases/")

memory.size(max=10^12)
gc(TRUE)
ben201612_RJ <- fread("ben201612_RJ.csv")
ben201701_RJ <- fread("ben201701_RJ.csv")
ben201702_RJ <- fread("ben201702_RJ.csv")
ben201703_RJ <- fread("ben201703_RJ.csv")
ben201704_RJ <- fread("ben201704_RJ.csv")
ben201705_RJ <- fread("ben201705_RJ.csv")
ben201706_RJ <- fread("ben201706_RJ.csv")
ben201707_RJ <- fread("ben201707_RJ.csv")
ben201708_RJ <- fread("ben201708_RJ.csv")
ben201709_RJ <- fread("ben201709_RJ.csv")
ben201710_RJ <- fread("ben201710_RJ.csv")
ben201711_RJ <- fread("ben201711_RJ.csv")
ben201712_RJ <- fread("ben201712_RJ.csv")
ben201801_RJ <- fread("ben201801_RJ.csv")
ben201802_RJ <- fread("ben201802_RJ.csv")
ben201803_RJ <- fread("ben201803_RJ.csv")
ben201804_RJ <- fread("ben201804_RJ.csv")
ben201805_RJ <- fread("ben201805_RJ.csv")
ben201806_RJ <- fread("ben201806_RJ.csv")
ben201807_RJ <- fread("ben201807_RJ.csv")
ben201808_RJ <- fread("ben201808_RJ.csv")
ben201809_RJ <- fread("ben201809_RJ.csv")
ben201810_RJ <- fread("ben201810_RJ.csv")
ben201811_RJ <- fread("ben201811_RJ.csv")
ben201812_RJ <- fread("ben201812_RJ.csv")
ben201901_RJ <- fread("ben201901_RJ.csv")
ben201902_RJ <- fread("ben201902_RJ.csv")
ben201903_RJ <- fread("ben201903_RJ.csv")

memory.size(max=10^12)
gc(TRUE)
BENEF_RJ <- do.call("rbind", list(ben201612_RJ,
                                  ben201701_RJ,ben201702_RJ,ben201703_RJ,
                                  ben201704_RJ,ben201705_RJ,ben201706_RJ,
                                  ben201707_RJ,ben201708_RJ,ben201709_RJ,
                                  ben201710_RJ,ben201711_RJ,ben201712_RJ,
                                  ben201801_RJ,ben201802_RJ,ben201803_RJ,
                                  ben201804_RJ,ben201805_RJ,ben201806_RJ,
                                  ben201807_RJ,ben201808_RJ,ben201809_RJ,
                                  ben201810_RJ,ben201811_RJ,ben201812_RJ,
                                  ben201901_RJ,ben201902_RJ,ben201903_RJ))

rm(ben201612_RJ,
   ben201701_RJ,ben201702_RJ,ben201703_RJ,
   ben201704_RJ,ben201705_RJ,ben201706_RJ,
   ben201707_RJ,ben201708_RJ,ben201709_RJ,
   ben201710_RJ,ben201711_RJ,ben201712_RJ,
   ben201801_RJ,ben201802_RJ,ben201803_RJ,
   ben201804_RJ,ben201805_RJ,ben201806_RJ,
   ben201807_RJ,ben201808_RJ,ben201809_RJ,
   ben201810_RJ,ben201811_RJ,ben201812_RJ,
   ben201901_RJ,ben201902_RJ,ben201903_RJ)


BENEF_RJ_ATIVO <- 
  BENEF_RJ %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ATIVO=sum(QT_BENEFICIARIO_ATIVO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ATIVO)

BENEF_RJ_ADERIDO <- 
  BENEF_RJ %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ADERIDO=sum(QT_BENEFICIARIO_ADERIDO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ADERIDO)

BENEF_RJ_CANCELADO <- 
  BENEF_RJ %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_CANCELADO=sum(QT_BENEFICIARIO_CANCELADO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_CANCELADO)

rm(BENEF_RJ)

write.csv(BENEF_RJ_ATIVO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_RJ_ATIVO.csv")
write.csv(BENEF_RJ_ADERIDO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_RJ_ADERIDO.csv")
write.csv(BENEF_RJ_CANCELADO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_RJ_CANCELADO.csv")

rm(BENEF_RJ_ATIVO,BENEF_RJ_ADERIDO,BENEF_RJ_CANCELADO)

#### IMPORTAR AS BASES DE DADOS - MINAS GERAIS ####

setwd("G:/Economia/1_Base de Informações/ANS - Info Consolidadas de Beneficiários/Histórico Bases/Bases/")

memory.size(max=10^12)
gc(TRUE)
ben201612_MG <- fread("ben201612_MG.csv")
ben201701_MG <- fread("ben201701_MG.csv")
ben201702_MG <- fread("ben201702_MG.csv")
ben201703_MG <- fread("ben201703_MG.csv")
ben201704_MG <- fread("ben201704_MG.csv")
ben201705_MG <- fread("ben201705_MG.csv")
ben201706_MG <- fread("ben201706_MG.csv")
ben201707_MG <- fread("ben201707_MG.csv")
ben201708_MG <- fread("ben201708_MG.csv")
ben201709_MG <- fread("ben201709_MG.csv")
ben201710_MG <- fread("ben201710_MG.csv")
ben201711_MG <- fread("ben201711_MG.csv")
ben201712_MG <- fread("ben201712_MG.csv")
ben201801_MG <- fread("ben201801_MG.csv")
ben201802_MG <- fread("ben201802_MG.csv")
ben201803_MG <- fread("ben201803_MG.csv")
ben201804_MG <- fread("ben201804_MG.csv")
ben201805_MG <- fread("ben201805_MG.csv")
ben201806_MG <- fread("ben201806_MG.csv")
ben201807_MG <- fread("ben201807_MG.csv")
ben201808_MG <- fread("ben201808_MG.csv")
ben201809_MG <- fread("ben201809_MG.csv")
ben201810_MG <- fread("ben201810_MG.csv")
ben201811_MG <- fread("ben201811_MG.csv")
ben201812_MG <- fread("ben201812_MG.csv")
ben201901_MG <- fread("ben201901_MG.csv")
ben201902_MG <- fread("ben201902_MG.csv")
ben201903_MG <- fread("ben201903_MG.csv")

memory.size(max=10^12)
gc(TRUE)
BENEF_MG <- do.call("rbind", list(ben201612_MG,
                                  ben201701_MG,ben201702_MG,ben201703_MG,
                                  ben201704_MG,ben201705_MG,ben201706_MG,
                                  ben201707_MG,ben201708_MG,ben201709_MG,
                                  ben201710_MG,ben201711_MG,ben201712_MG,
                                  ben201801_MG,ben201802_MG,ben201803_MG,
                                  ben201804_MG,ben201805_MG,ben201806_MG,
                                  ben201807_MG,ben201808_MG,ben201809_MG,
                                  ben201810_MG,ben201811_MG,ben201812_MG,
                                  ben201901_MG,ben201902_MG,ben201903_MG))

rm(ben201612_MG,
   ben201701_MG,ben201702_MG,ben201703_MG,
   ben201704_MG,ben201705_MG,ben201706_MG,
   ben201707_MG,ben201708_MG,ben201709_MG,
   ben201710_MG,ben201711_MG,ben201712_MG,
   ben201801_MG,ben201802_MG,ben201803_MG,
   ben201804_MG,ben201805_MG,ben201806_MG,
   ben201807_MG,ben201808_MG,ben201809_MG,
   ben201810_MG,ben201811_MG,ben201812_MG,
   ben201901_MG,ben201902_MG,ben201903_MG)


BENEF_MG_ATIVO <- 
  BENEF_MG %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ATIVO=sum(QT_BENEFICIARIO_ATIVO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ATIVO)

BENEF_MG_ADERIDO <- 
  BENEF_MG %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ADERIDO=sum(QT_BENEFICIARIO_ADERIDO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ADERIDO)

BENEF_MG_CANCELADO <- 
  BENEF_MG %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_CANCELADO=sum(QT_BENEFICIARIO_CANCELADO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_CANCELADO)

rm(BENEF_MG)

write.csv(BENEF_MG_ATIVO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_MG_ATIVO.csv")
write.csv(BENEF_MG_ADERIDO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_MG_ADERIDO.csv")
write.csv(BENEF_MG_CANCELADO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_MG_CANCELADO.csv")

rm(BENEF_MG_ATIVO,BENEF_MG_ADERIDO,BENEF_MG_CANCELADO)

#### IMPORTAR AS BASES DE DADOS - PARANÁ ####

setwd("G:/Economia/1_Base de Informações/ANS - Info Consolidadas de Beneficiários/Histórico Bases/Bases/")

memory.size(max=10^12)
gc(TRUE)
ben201612_PR <- fread("ben201612_PR.csv")
ben201701_PR <- fread("ben201701_PR.csv")
ben201702_PR <- fread("ben201702_PR.csv")
ben201703_PR <- fread("ben201703_PR.csv")
ben201704_PR <- fread("ben201704_PR.csv")
ben201705_PR <- fread("ben201705_PR.csv")
ben201706_PR <- fread("ben201706_PR.csv")
ben201707_PR <- fread("ben201707_PR.csv")
ben201708_PR <- fread("ben201708_PR.csv")
ben201709_PR <- fread("ben201709_PR.csv")
ben201710_PR <- fread("ben201710_PR.csv")
ben201711_PR <- fread("ben201711_PR.csv")
ben201712_PR <- fread("ben201712_PR.csv")
ben201801_PR <- fread("ben201801_PR.csv")
ben201802_PR <- fread("ben201802_PR.csv")
ben201803_PR <- fread("ben201803_PR.csv")
ben201804_PR <- fread("ben201804_PR.csv")
ben201805_PR <- fread("ben201805_PR.csv")
ben201806_PR <- fread("ben201806_PR.csv")
ben201807_PR <- fread("ben201807_PR.csv")
ben201808_PR <- fread("ben201808_PR.csv")
ben201809_PR <- fread("ben201809_PR.csv")
ben201810_PR <- fread("ben201810_PR.csv")
ben201811_PR <- fread("ben201811_PR.csv")
ben201812_PR <- fread("ben201812_PR.csv")
ben201901_PR <- fread("ben201901_PR.csv")
ben201902_PR <- fread("ben201902_PR.csv")
ben201903_PR <- fread("ben201903_PR.csv")

memory.size(max=10^12)
gc(TRUE)
BENEF_PR <- do.call("rbind", list(ben201612_PR,
                                  ben201701_PR,ben201702_PR,ben201703_PR,
                                  ben201704_PR,ben201705_PR,ben201706_PR,
                                  ben201707_PR,ben201708_PR,ben201709_PR,
                                  ben201710_PR,ben201711_PR,ben201712_PR,
                                  ben201801_PR,ben201802_PR,ben201803_PR,
                                  ben201804_PR,ben201805_PR,ben201806_PR,
                                  ben201807_PR,ben201808_PR,ben201809_PR,
                                  ben201810_PR,ben201811_PR,ben201812_PR,
                                  ben201901_PR,ben201902_PR,ben201903_PR))

rm(ben201612_PR,
   ben201701_PR,ben201702_PR,ben201703_PR,
   ben201704_PR,ben201705_PR,ben201706_PR,
   ben201707_PR,ben201708_PR,ben201709_PR,
   ben201710_PR,ben201711_PR,ben201712_PR,
   ben201801_PR,ben201802_PR,ben201803_PR,
   ben201804_PR,ben201805_PR,ben201806_PR,
   ben201807_PR,ben201808_PR,ben201809_PR,
   ben201810_PR,ben201811_PR,ben201812_PR,
   ben201901_PR,ben201902_PR,ben201903_PR)


BENEF_PR_ATIVO <- 
  BENEF_PR %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ATIVO=sum(QT_BENEFICIARIO_ATIVO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ATIVO)

BENEF_PR_ADERIDO <- 
  BENEF_PR %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ADERIDO=sum(QT_BENEFICIARIO_ADERIDO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ADERIDO)

BENEF_PR_CANCELADO <- 
  BENEF_PR %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_CANCELADO=sum(QT_BENEFICIARIO_CANCELADO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_CANCELADO)

rm(BENEF_PR)

write.csv(BENEF_PR_ATIVO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_PR_ATIVO.csv")
write.csv(BENEF_PR_ADERIDO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_PR_ADERIDO.csv")
write.csv(BENEF_PR_CANCELADO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_PR_CANCELADO.csv")

rm(BENEF_PR_ATIVO,BENEF_PR_ADERIDO,BENEF_PR_CANCELADO)

#### IMPORTAR AS BASES DE DADOS - ACRE ####

setwd("G:/Economia/1_Base de Informações/ANS - Info Consolidadas de Beneficiários/Histórico Bases/Bases/")

memory.size(max=10^12)
gc(TRUE)
ben201612_AC <- fread("ben201612_AC.csv")
ben201701_AC <- fread("ben201701_AC.csv")
ben201702_AC <- fread("ben201702_AC.csv")
ben201703_AC <- fread("ben201703_AC.csv")
ben201704_AC <- fread("ben201704_AC.csv")
ben201705_AC <- fread("ben201705_AC.csv")
ben201706_AC <- fread("ben201706_AC.csv")
ben201707_AC <- fread("ben201707_AC.csv")
ben201708_AC <- fread("ben201708_AC.csv")
ben201709_AC <- fread("ben201709_AC.csv")
ben201710_AC <- fread("ben201710_AC.csv")
ben201711_AC <- fread("ben201711_AC.csv")
ben201712_AC <- fread("ben201712_AC.csv")
ben201801_AC <- fread("ben201801_AC.csv")
ben201802_AC <- fread("ben201802_AC.csv")
ben201803_AC <- fread("ben201803_AC.csv")
ben201804_AC <- fread("ben201804_AC.csv")
ben201805_AC <- fread("ben201805_AC.csv")
ben201806_AC <- fread("ben201806_AC.csv")
ben201807_AC <- fread("ben201807_AC.csv")
ben201808_AC <- fread("ben201808_AC.csv")
ben201809_AC <- fread("ben201809_AC.csv")
ben201810_AC <- fread("ben201810_AC.csv")
ben201811_AC <- fread("ben201811_AC.csv")
ben201812_AC <- fread("ben201812_AC.csv")
ben201901_AC <- fread("ben201901_AC.csv")
ben201902_AC <- fread("ben201902_AC.csv")
ben201903_AC <- fread("ben201903_AC.csv")

memory.size(max=10^12)
gc(TRUE)
BENEF_AC <- do.call("rbind", list(ben201612_AC,
                                  ben201701_AC,ben201702_AC,ben201703_AC,
                                  ben201704_AC,ben201705_AC,ben201706_AC,
                                  ben201707_AC,ben201708_AC,ben201709_AC,
                                  ben201710_AC,ben201711_AC,ben201712_AC,
                                  ben201801_AC,ben201802_AC,ben201803_AC,
                                  ben201804_AC,ben201805_AC,ben201806_AC,
                                  ben201807_AC,ben201808_AC,ben201809_AC,
                                  ben201810_AC,ben201811_AC,ben201812_AC,
                                  ben201901_AC,ben201902_AC,ben201903_AC))

rm(ben201612_AC,
   ben201701_AC,ben201702_AC,ben201703_AC,
   ben201704_AC,ben201705_AC,ben201706_AC,
   ben201707_AC,ben201708_AC,ben201709_AC,
   ben201710_AC,ben201711_AC,ben201712_AC,
   ben201801_AC,ben201802_AC,ben201803_AC,
   ben201804_AC,ben201805_AC,ben201806_AC,
   ben201807_AC,ben201808_AC,ben201809_AC,
   ben201810_AC,ben201811_AC,ben201812_AC,
   ben201901_AC,ben201902_AC,ben201903_AC)


BENEF_AC_ATIVO <- 
  BENEF_AC %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ATIVO=sum(QT_BENEFICIARIO_ATIVO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ATIVO)

BENEF_AC_ADERIDO <- 
  BENEF_AC %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ADERIDO=sum(QT_BENEFICIARIO_ADERIDO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ADERIDO)

BENEF_AC_CANCELADO <- 
  BENEF_AC %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_CANCELADO=sum(QT_BENEFICIARIO_CANCELADO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_CANCELADO)

rm(BENEF_AC)

write.csv(BENEF_AC_ATIVO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_AC_ATIVO.csv")
write.csv(BENEF_AC_ADERIDO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_AC_ADERIDO.csv")
write.csv(BENEF_AC_CANCELADO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_AC_CANCELADO.csv")

rm(BENEF_AC_ATIVO,BENEF_AC_ADERIDO,BENEF_AC_CANCELADO)

#### IMPORTAR AS BASES DE DADOS - AMAZONAS ####

setwd("G:/Economia/1_Base de Informações/ANS - Info Consolidadas de Beneficiários/Histórico Bases/Bases/")

memory.size(max=10^12)
gc(TRUE)
ben201612_AM <- fread("ben201612_AM.csv")
ben201701_AM <- fread("ben201701_AM.csv")
ben201702_AM <- fread("ben201702_AM.csv")
ben201703_AM <- fread("ben201703_AM.csv")
ben201704_AM <- fread("ben201704_AM.csv")
ben201705_AM <- fread("ben201705_AM.csv")
ben201706_AM <- fread("ben201706_AM.csv")
ben201707_AM <- fread("ben201707_AM.csv")
ben201708_AM <- fread("ben201708_AM.csv")
ben201709_AM <- fread("ben201709_AM.csv")
ben201710_AM <- fread("ben201710_AM.csv")
ben201711_AM <- fread("ben201711_AM.csv")
ben201712_AM <- fread("ben201712_AM.csv")
ben201801_AM <- fread("ben201801_AM.csv")
ben201802_AM <- fread("ben201802_AM.csv")
ben201803_AM <- fread("ben201803_AM.csv")
ben201804_AM <- fread("ben201804_AM.csv")
ben201805_AM <- fread("ben201805_AM.csv")
ben201806_AM <- fread("ben201806_AM.csv")
ben201807_AM <- fread("ben201807_AM.csv")
ben201808_AM <- fread("ben201808_AM.csv")
ben201809_AM <- fread("ben201809_AM.csv")
ben201810_AM <- fread("ben201810_AM.csv")
ben201811_AM <- fread("ben201811_AM.csv")
ben201812_AM <- fread("ben201812_AM.csv")
ben201901_AM <- fread("ben201901_AM.csv")
ben201902_AM <- fread("ben201902_AM.csv")
ben201903_AM <- fread("ben201903_AM.csv")

memory.size(max=10^12)
gc(TRUE)
BENEF_AM <- do.call("rbind", list(ben201612_AM,
                                  ben201701_AM,ben201702_AM,ben201703_AM,
                                  ben201704_AM,ben201705_AM,ben201706_AM,
                                  ben201707_AM,ben201708_AM,ben201709_AM,
                                  ben201710_AM,ben201711_AM,ben201712_AM,
                                  ben201801_AM,ben201802_AM,ben201803_AM,
                                  ben201804_AM,ben201805_AM,ben201806_AM,
                                  ben201807_AM,ben201808_AM,ben201809_AM,
                                  ben201810_AM,ben201811_AM,ben201812_AM,
                                  ben201901_AM,ben201902_AM,ben201903_AM))

rm(ben201612_AM,
   ben201701_AM,ben201702_AM,ben201703_AM,
   ben201704_AM,ben201705_AM,ben201706_AM,
   ben201707_AM,ben201708_AM,ben201709_AM,
   ben201710_AM,ben201711_AM,ben201712_AM,
   ben201801_AM,ben201802_AM,ben201803_AM,
   ben201804_AM,ben201805_AM,ben201806_AM,
   ben201807_AM,ben201808_AM,ben201809_AM,
   ben201810_AM,ben201811_AM,ben201812_AM,
   ben201901_AM,ben201902_AM,ben201903_AM)


BENEF_AM_ATIVO <- 
  BENEF_AM %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ATIVO=sum(QT_BENEFICIARIO_ATIVO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ATIVO)

BENEF_AM_ADERIDO <- 
  BENEF_AM %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ADERIDO=sum(QT_BENEFICIARIO_ADERIDO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ADERIDO)

BENEF_AM_CANCELADO <- 
  BENEF_AM %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_CANCELADO=sum(QT_BENEFICIARIO_CANCELADO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_CANCELADO)

rm(BENEF_AM)

write.csv(BENEF_AM_ATIVO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_AM_ATIVO.csv")
write.csv(BENEF_AM_ADERIDO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_AM_ADERIDO.csv")
write.csv(BENEF_AM_CANCELADO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_AM_CANCELADO.csv")

rm(BENEF_AM_ATIVO,BENEF_AM_ADERIDO,BENEF_AM_CANCELADO)

#### IMPORTAR AS BASES DE DADOS - AMAPÁ ####

setwd("G:/Economia/1_Base de Informações/ANS - Info Consolidadas de Beneficiários/Histórico Bases/Bases/")

memory.size(max=10^12)
gc(TRUE)
ben201612_AP <- fread("ben201612_AP.csv")
ben201701_AP <- fread("ben201701_AP.csv")
ben201702_AP <- fread("ben201702_AP.csv")
ben201703_AP <- fread("ben201703_AP.csv")
ben201704_AP <- fread("ben201704_AP.csv")
ben201705_AP <- fread("ben201705_AP.csv")
ben201706_AP <- fread("ben201706_AP.csv")
ben201707_AP <- fread("ben201707_AP.csv")
ben201708_AP <- fread("ben201708_AP.csv")
ben201709_AP <- fread("ben201709_AP.csv")
ben201710_AP <- fread("ben201710_AP.csv")
ben201711_AP <- fread("ben201711_AP.csv")
ben201712_AP <- fread("ben201712_AP.csv")
ben201801_AP <- fread("ben201801_AP.csv")
ben201802_AP <- fread("ben201802_AP.csv")
ben201803_AP <- fread("ben201803_AP.csv")
ben201804_AP <- fread("ben201804_AP.csv")
ben201805_AP <- fread("ben201805_AP.csv")
ben201806_AP <- fread("ben201806_AP.csv")
ben201807_AP <- fread("ben201807_AP.csv")
ben201808_AP <- fread("ben201808_AP.csv")
ben201809_AP <- fread("ben201809_AP.csv")
ben201810_AP <- fread("ben201810_AP.csv")
ben201811_AP <- fread("ben201811_AP.csv")
ben201812_AP <- fread("ben201812_AP.csv")
ben201901_AP <- fread("ben201901_AP.csv")
ben201902_AP <- fread("ben201902_AP.csv")
ben201903_AP <- fread("ben201903_AP.csv")

memory.size(max=10^12)
gc(TRUE)
BENEF_AP <- do.call("rbind", list(ben201612_AP,
                                  ben201701_AP,ben201702_AP,ben201703_AP,
                                  ben201704_AP,ben201705_AP,ben201706_AP,
                                  ben201707_AP,ben201708_AP,ben201709_AP,
                                  ben201710_AP,ben201711_AP,ben201712_AP,
                                  ben201801_AP,ben201802_AP,ben201803_AP,
                                  ben201804_AP,ben201805_AP,ben201806_AP,
                                  ben201807_AP,ben201808_AP,ben201809_AP,
                                  ben201810_AP,ben201811_AP,ben201812_AP,
                                  ben201901_AP,ben201902_AP,ben201903_AP))

rm(ben201612_AP,
   ben201701_AP,ben201702_AP,ben201703_AP,
   ben201704_AP,ben201705_AP,ben201706_AP,
   ben201707_AP,ben201708_AP,ben201709_AP,
   ben201710_AP,ben201711_AP,ben201712_AP,
   ben201801_AP,ben201802_AP,ben201803_AP,
   ben201804_AP,ben201805_AP,ben201806_AP,
   ben201807_AP,ben201808_AP,ben201809_AP,
   ben201810_AP,ben201811_AP,ben201812_AP,
   ben201901_AP,ben201902_AP,ben201903_AP)


BENEF_AP_ATIVO <- 
  BENEF_AP %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ATIVO=sum(QT_BENEFICIARIO_ATIVO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ATIVO)

BENEF_AP_ADERIDO <- 
  BENEF_AP %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ADERIDO=sum(QT_BENEFICIARIO_ADERIDO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ADERIDO)

BENEF_AP_CANCELADO <- 
  BENEF_AP %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_CANCELADO=sum(QT_BENEFICIARIO_CANCELADO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_CANCELADO)

rm(BENEF_AP)

write.csv(BENEF_AP_ATIVO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_AP_ATIVO.csv")
write.csv(BENEF_AP_ADERIDO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_AP_ADERIDO.csv")
write.csv(BENEF_AP_CANCELADO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_AP_CANCELADO.csv")

rm(BENEF_AP_ATIVO,BENEF_AP_ADERIDO,BENEF_AP_CANCELADO)

#### IMPORTAR AS BASES DE DADOS - ALAGOAS ####

setwd("G:/Economia/1_Base de Informações/ANS - Info Consolidadas de Beneficiários/Histórico Bases/Bases/")

memory.size(max=10^12)
gc(TRUE)
ben201612_AL <- fread("ben201612_AL.csv")
ben201701_AL <- fread("ben201701_AL.csv")
ben201702_AL <- fread("ben201702_AL.csv")
ben201703_AL <- fread("ben201703_AL.csv")
ben201704_AL <- fread("ben201704_AL.csv")
ben201705_AL <- fread("ben201705_AL.csv")
ben201706_AL <- fread("ben201706_AL.csv")
ben201707_AL <- fread("ben201707_AL.csv")
ben201708_AL <- fread("ben201708_AL.csv")
ben201709_AL <- fread("ben201709_AL.csv")
ben201710_AL <- fread("ben201710_AL.csv")
ben201711_AL <- fread("ben201711_AL.csv")
ben201712_AL <- fread("ben201712_AL.csv")
ben201801_AL <- fread("ben201801_AL.csv")
ben201802_AL <- fread("ben201802_AL.csv")
ben201803_AL <- fread("ben201803_AL.csv")
ben201804_AL <- fread("ben201804_AL.csv")
ben201805_AL <- fread("ben201805_AL.csv")
ben201806_AL <- fread("ben201806_AL.csv")
ben201807_AL <- fread("ben201807_AL.csv")
ben201808_AL <- fread("ben201808_AL.csv")
ben201809_AL <- fread("ben201809_AL.csv")
ben201810_AL <- fread("ben201810_AL.csv")
ben201811_AL <- fread("ben201811_AL.csv")
ben201812_AL <- fread("ben201812_AL.csv")
ben201901_AL <- fread("ben201901_AL.csv")
ben201902_AL <- fread("ben201902_AL.csv")
ben201903_AL <- fread("ben201903_AL.csv")

memory.size(max=10^12)
gc(TRUE)
BENEF_AL <- do.call("rbind", list(ben201612_AL,
                                  ben201701_AL,ben201702_AL,ben201703_AL,
                                  ben201704_AL,ben201705_AL,ben201706_AL,
                                  ben201707_AL,ben201708_AL,ben201709_AL,
                                  ben201710_AL,ben201711_AL,ben201712_AL,
                                  ben201801_AL,ben201802_AL,ben201803_AL,
                                  ben201804_AL,ben201805_AL,ben201806_AL,
                                  ben201807_AL,ben201808_AL,ben201809_AL,
                                  ben201810_AL,ben201811_AL,ben201812_AL,
                                  ben201901_AL,ben201902_AL,ben201903_AL))

rm(ben201612_AL,
   ben201701_AL,ben201702_AL,ben201703_AL,
   ben201704_AL,ben201705_AL,ben201706_AL,
   ben201707_AL,ben201708_AL,ben201709_AL,
   ben201710_AL,ben201711_AL,ben201712_AL,
   ben201801_AL,ben201802_AL,ben201803_AL,
   ben201804_AL,ben201805_AL,ben201806_AL,
   ben201807_AL,ben201808_AL,ben201809_AL,
   ben201810_AL,ben201811_AL,ben201812_AL,
   ben201901_AL,ben201902_AL,ben201903_AL)


BENEF_AL_ATIVO <- 
  BENEF_AL %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ATIVO=sum(QT_BENEFICIARIO_ATIVO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ATIVO)

BENEF_AL_ADERIDO <- 
  BENEF_AL %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ADERIDO=sum(QT_BENEFICIARIO_ADERIDO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ADERIDO)

BENEF_AL_CANCELADO <- 
  BENEF_AL %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_CANCELADO=sum(QT_BENEFICIARIO_CANCELADO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_CANCELADO)

rm(BENEF_AL)

write.csv(BENEF_AL_ATIVO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_AL_ATIVO.csv")
write.csv(BENEF_AL_ADERIDO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_AL_ADERIDO.csv")
write.csv(BENEF_AL_CANCELADO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_AL_CANCELADO.csv")

rm(BENEF_AL_ATIVO,BENEF_AL_ADERIDO,BENEF_AL_CANCELADO)

#### IMPORTAR AS BASES DE DADOS - BAHIA ####

setwd("G:/Economia/1_Base de Informações/ANS - Info Consolidadas de Beneficiários/Histórico Bases/Bases/")

memory.size(max=10^12)
gc(TRUE)
ben201612_BA <- fread("ben201612_BA.csv")
ben201701_BA <- fread("ben201701_BA.csv")
ben201702_BA <- fread("ben201702_BA.csv")
ben201703_BA <- fread("ben201703_BA.csv")
ben201704_BA <- fread("ben201704_BA.csv")
ben201705_BA <- fread("ben201705_BA.csv")
ben201706_BA <- fread("ben201706_BA.csv")
ben201707_BA <- fread("ben201707_BA.csv")
ben201708_BA <- fread("ben201708_BA.csv")
ben201709_BA <- fread("ben201709_BA.csv")
ben201710_BA <- fread("ben201710_BA.csv")
ben201711_BA <- fread("ben201711_BA.csv")
ben201712_BA <- fread("ben201712_BA.csv")
ben201801_BA <- fread("ben201801_BA.csv")
ben201802_BA <- fread("ben201802_BA.csv")
ben201803_BA <- fread("ben201803_BA.csv")
ben201804_BA <- fread("ben201804_BA.csv")
ben201805_BA <- fread("ben201805_BA.csv")
ben201806_BA <- fread("ben201806_BA.csv")
ben201807_BA <- fread("ben201807_BA.csv")
ben201808_BA <- fread("ben201808_BA.csv")
ben201809_BA <- fread("ben201809_BA.csv")
ben201810_BA <- fread("ben201810_BA.csv")
ben201811_BA <- fread("ben201811_BA.csv")
ben201812_BA <- fread("ben201812_BA.csv")
ben201901_BA <- fread("ben201901_BA.csv")
ben201902_BA <- fread("ben201902_BA.csv")
ben201903_BA <- fread("ben201903_BA.csv")

memory.size(max=10^12)
gc(TRUE)
BENEF_BA <- do.call("rbind", list(ben201612_BA,
                                  ben201701_BA,ben201702_BA,ben201703_BA,
                                  ben201704_BA,ben201705_BA,ben201706_BA,
                                  ben201707_BA,ben201708_BA,ben201709_BA,
                                  ben201710_BA,ben201711_BA,ben201712_BA,
                                  ben201801_BA,ben201802_BA,ben201803_BA,
                                  ben201804_BA,ben201805_BA,ben201806_BA,
                                  ben201807_BA,ben201808_BA,ben201809_BA,
                                  ben201810_BA,ben201811_BA,ben201812_BA,
                                  ben201901_BA,ben201902_BA,ben201903_BA))

rm(ben201612_BA,
   ben201701_BA,ben201702_BA,ben201703_BA,
   ben201704_BA,ben201705_BA,ben201706_BA,
   ben201707_BA,ben201708_BA,ben201709_BA,
   ben201710_BA,ben201711_BA,ben201712_BA,
   ben201801_BA,ben201802_BA,ben201803_BA,
   ben201804_BA,ben201805_BA,ben201806_BA,
   ben201807_BA,ben201808_BA,ben201809_BA,
   ben201810_BA,ben201811_BA,ben201812_BA,
   ben201901_BA,ben201902_BA,ben201903_BA)


BENEF_BA_ATIVO <- 
  BENEF_BA %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ATIVO=sum(QT_BENEFICIARIO_ATIVO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ATIVO)

BENEF_BA_ADERIDO <- 
  BENEF_BA %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ADERIDO=sum(QT_BENEFICIARIO_ADERIDO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ADERIDO)

BENEF_BA_CANCELADO <- 
  BENEF_BA %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_CANCELADO=sum(QT_BENEFICIARIO_CANCELADO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_CANCELADO)

rm(BENEF_BA)

write.csv(BENEF_BA_ATIVO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_BA_ATIVO.csv")
write.csv(BENEF_BA_ADERIDO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_BA_ADERIDO.csv")
write.csv(BENEF_BA_CANCELADO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_BA_CANCELADO.csv")

rm(BENEF_BA_ATIVO,BENEF_BA_ADERIDO,BENEF_BA_CANCELADO)

#### IMPORTAR AS BASES DE DADOS - CEARÁ ####

setwd("G:/Economia/1_Base de Informações/ANS - Info Consolidadas de Beneficiários/Histórico Bases/Bases/")

memory.size(max=10^12)
gc(TRUE)
ben201612_CE <- fread("ben201612_CE.csv")
ben201701_CE <- fread("ben201701_CE.csv")
ben201702_CE <- fread("ben201702_CE.csv")
ben201703_CE <- fread("ben201703_CE.csv")
ben201704_CE <- fread("ben201704_CE.csv")
ben201705_CE <- fread("ben201705_CE.csv")
ben201706_CE <- fread("ben201706_CE.csv")
ben201707_CE <- fread("ben201707_CE.csv")
ben201708_CE <- fread("ben201708_CE.csv")
ben201709_CE <- fread("ben201709_CE.csv")
ben201710_CE <- fread("ben201710_CE.csv")
ben201711_CE <- fread("ben201711_CE.csv")
ben201712_CE <- fread("ben201712_CE.csv")
ben201801_CE <- fread("ben201801_CE.csv")
ben201802_CE <- fread("ben201802_CE.csv")
ben201803_CE <- fread("ben201803_CE.csv")
ben201804_CE <- fread("ben201804_CE.csv")
ben201805_CE <- fread("ben201805_CE.csv")
ben201806_CE <- fread("ben201806_CE.csv")
ben201807_CE <- fread("ben201807_CE.csv")
ben201808_CE <- fread("ben201808_CE.csv")
ben201809_CE <- fread("ben201809_CE.csv")
ben201810_CE <- fread("ben201810_CE.csv")
ben201811_CE <- fread("ben201811_CE.csv")
ben201812_CE <- fread("ben201812_CE.csv")
ben201901_CE <- fread("ben201901_CE.csv")
ben201902_CE <- fread("ben201902_CE.csv")
ben201903_CE <- fread("ben201903_CE.csv")

memory.size(max=10^12)
gc(TRUE)
BENEF_CE <- do.call("rbind", list(ben201612_CE,
                                  ben201701_CE,ben201702_CE,ben201703_CE,
                                  ben201704_CE,ben201705_CE,ben201706_CE,
                                  ben201707_CE,ben201708_CE,ben201709_CE,
                                  ben201710_CE,ben201711_CE,ben201712_CE,
                                  ben201801_CE,ben201802_CE,ben201803_CE,
                                  ben201804_CE,ben201805_CE,ben201806_CE,
                                  ben201807_CE,ben201808_CE,ben201809_CE,
                                  ben201810_CE,ben201811_CE,ben201812_CE,
                                  ben201901_CE,ben201902_CE,ben201903_CE))

rm(ben201612_CE,
   ben201701_CE,ben201702_CE,ben201703_CE,
   ben201704_CE,ben201705_CE,ben201706_CE,
   ben201707_CE,ben201708_CE,ben201709_CE,
   ben201710_CE,ben201711_CE,ben201712_CE,
   ben201801_CE,ben201802_CE,ben201803_CE,
   ben201804_CE,ben201805_CE,ben201806_CE,
   ben201807_CE,ben201808_CE,ben201809_CE,
   ben201810_CE,ben201811_CE,ben201812_CE,
   ben201901_CE,ben201902_CE,ben201903_CE)


BENEF_CE_ATIVO <- 
  BENEF_CE %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ATIVO=sum(QT_BENEFICIARIO_ATIVO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ATIVO)

BENEF_CE_ADERIDO <- 
  BENEF_CE %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ADERIDO=sum(QT_BENEFICIARIO_ADERIDO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ADERIDO)

BENEF_CE_CANCELADO <- 
  BENEF_CE %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_CANCELADO=sum(QT_BENEFICIARIO_CANCELADO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_CANCELADO)

rm(BENEF_CE)

write.csv(BENEF_CE_ATIVO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_CE_ATIVO.csv")
write.csv(BENEF_CE_ADERIDO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_CE_ADERIDO.csv")
write.csv(BENEF_CE_CANCELADO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_CE_CANCELADO.csv")

rm(BENEF_CE_ATIVO,BENEF_CE_ADERIDO,BENEF_CE_CANCELADO)

#### IMPORTAR AS BASES DE DADOS - DISTRITO FEDERAL ####

setwd("G:/Economia/1_Base de Informações/ANS - Info Consolidadas de Beneficiários/Histórico Bases/Bases/")

memory.size(max=10^12)
gc(TRUE)
ben201612_DF <- fread("ben201612_DF.csv")
ben201701_DF <- fread("ben201701_DF.csv")
ben201702_DF <- fread("ben201702_DF.csv")
ben201703_DF <- fread("ben201703_DF.csv")
ben201704_DF <- fread("ben201704_DF.csv")
ben201705_DF <- fread("ben201705_DF.csv")
ben201706_DF <- fread("ben201706_DF.csv")
ben201707_DF <- fread("ben201707_DF.csv")
ben201708_DF <- fread("ben201708_DF.csv")
ben201709_DF <- fread("ben201709_DF.csv")
ben201710_DF <- fread("ben201710_DF.csv")
ben201711_DF <- fread("ben201711_DF.csv")
ben201712_DF <- fread("ben201712_DF.csv")
ben201801_DF <- fread("ben201801_DF.csv")
ben201802_DF <- fread("ben201802_DF.csv")
ben201803_DF <- fread("ben201803_DF.csv")
ben201804_DF <- fread("ben201804_DF.csv")
ben201805_DF <- fread("ben201805_DF.csv")
ben201806_DF <- fread("ben201806_DF.csv")
ben201807_DF <- fread("ben201807_DF.csv")
ben201808_DF <- fread("ben201808_DF.csv")
ben201809_DF <- fread("ben201809_DF.csv")
ben201810_DF <- fread("ben201810_DF.csv")
ben201811_DF <- fread("ben201811_DF.csv")
ben201812_DF <- fread("ben201812_DF.csv")
ben201901_DF <- fread("ben201901_DF.csv")
ben201902_DF <- fread("ben201902_DF.csv")
ben201903_DF <- fread("ben201903_DF.csv")

memory.size(max=10^12)
gc(TRUE)
BENEF_DF <- do.call("rbind", list(ben201612_DF,
                                  ben201701_DF,ben201702_DF,ben201703_DF,
                                  ben201704_DF,ben201705_DF,ben201706_DF,
                                  ben201707_DF,ben201708_DF,ben201709_DF,
                                  ben201710_DF,ben201711_DF,ben201712_DF,
                                  ben201801_DF,ben201802_DF,ben201803_DF,
                                  ben201804_DF,ben201805_DF,ben201806_DF,
                                  ben201807_DF,ben201808_DF,ben201809_DF,
                                  ben201810_DF,ben201811_DF,ben201812_DF,
                                  ben201901_DF,ben201902_DF,ben201903_DF))

rm(ben201612_DF,
   ben201701_DF,ben201702_DF,ben201703_DF,
   ben201704_DF,ben201705_DF,ben201706_DF,
   ben201707_DF,ben201708_DF,ben201709_DF,
   ben201710_DF,ben201711_DF,ben201712_DF,
   ben201801_DF,ben201802_DF,ben201803_DF,
   ben201804_DF,ben201805_DF,ben201806_DF,
   ben201807_DF,ben201808_DF,ben201809_DF,
   ben201810_DF,ben201811_DF,ben201812_DF,
   ben201901_DF,ben201902_DF,ben201903_DF)


BENEF_DF_ATIVO <- 
  BENEF_DF %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ATIVO=sum(QT_BENEFICIARIO_ATIVO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ATIVO)

BENEF_DF_ADERIDO <- 
  BENEF_DF %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ADERIDO=sum(QT_BENEFICIARIO_ADERIDO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ADERIDO)

BENEF_DF_CANCELADO <- 
  BENEF_DF %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_CANCELADO=sum(QT_BENEFICIARIO_CANCELADO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_CANCELADO)

rm(BENEF_DF)

write.csv(BENEF_DF_ATIVO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_DF_ATIVO.csv")
write.csv(BENEF_DF_ADERIDO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_DF_ADERIDO.csv")
write.csv(BENEF_DF_CANCELADO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_DF_CANCELADO.csv")

rm(BENEF_DF_ATIVO,BENEF_DF_ADERIDO,BENEF_DF_CANCELADO)

#### IMPORTAR AS BASES DE DADOS - ESPÍRITO SANTO ####

setwd("G:/Economia/1_Base de Informações/ANS - Info Consolidadas de Beneficiários/Histórico Bases/Bases/")

memory.size(max=10^12)
gc(TRUE)
ben201612_ES <- fread("ben201612_ES.csv")
ben201701_ES <- fread("ben201701_ES.csv")
ben201702_ES <- fread("ben201702_ES.csv")
ben201703_ES <- fread("ben201703_ES.csv")
ben201704_ES <- fread("ben201704_ES.csv")
ben201705_ES <- fread("ben201705_ES.csv")
ben201706_ES <- fread("ben201706_ES.csv")
ben201707_ES <- fread("ben201707_ES.csv")
ben201708_ES <- fread("ben201708_ES.csv")
ben201709_ES <- fread("ben201709_ES.csv")
ben201710_ES <- fread("ben201710_ES.csv")
ben201711_ES <- fread("ben201711_ES.csv")
ben201712_ES <- fread("ben201712_ES.csv")
ben201801_ES <- fread("ben201801_ES.csv")
ben201802_ES <- fread("ben201802_ES.csv")
ben201803_ES <- fread("ben201803_ES.csv")
ben201804_ES <- fread("ben201804_ES.csv")
ben201805_ES <- fread("ben201805_ES.csv")
ben201806_ES <- fread("ben201806_ES.csv")
ben201807_ES <- fread("ben201807_ES.csv")
ben201808_ES <- fread("ben201808_ES.csv")
ben201809_ES <- fread("ben201809_ES.csv")
ben201810_ES <- fread("ben201810_ES.csv")
ben201811_ES <- fread("ben201811_ES.csv")
ben201812_ES <- fread("ben201812_ES.csv")
ben201901_ES <- fread("ben201901_ES.csv")
ben201902_ES <- fread("ben201902_ES.csv")
ben201903_ES <- fread("ben201903_ES.csv")

memory.size(max=10^12)
gc(TRUE)
BENEF_ES <- do.call("rbind", list(ben201612_ES,
                                  ben201701_ES,ben201702_ES,ben201703_ES,
                                  ben201704_ES,ben201705_ES,ben201706_ES,
                                  ben201707_ES,ben201708_ES,ben201709_ES,
                                  ben201710_ES,ben201711_ES,ben201712_ES,
                                  ben201801_ES,ben201802_ES,ben201803_ES,
                                  ben201804_ES,ben201805_ES,ben201806_ES,
                                  ben201807_ES,ben201808_ES,ben201809_ES,
                                  ben201810_ES,ben201811_ES,ben201812_ES,
                                  ben201901_ES,ben201902_ES,ben201903_ES))

rm(ben201612_ES,
   ben201701_ES,ben201702_ES,ben201703_ES,
   ben201704_ES,ben201705_ES,ben201706_ES,
   ben201707_ES,ben201708_ES,ben201709_ES,
   ben201710_ES,ben201711_ES,ben201712_ES,
   ben201801_ES,ben201802_ES,ben201803_ES,
   ben201804_ES,ben201805_ES,ben201806_ES,
   ben201807_ES,ben201808_ES,ben201809_ES,
   ben201810_ES,ben201811_ES,ben201812_ES,
   ben201901_ES,ben201902_ES,ben201903_ES)


BENEF_ES_ATIVO <- 
  BENEF_ES %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ATIVO=sum(QT_BENEFICIARIO_ATIVO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ATIVO)

BENEF_ES_ADERIDO <- 
  BENEF_ES %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ADERIDO=sum(QT_BENEFICIARIO_ADERIDO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ADERIDO)

BENEF_ES_CANCELADO <- 
  BENEF_ES %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_CANCELADO=sum(QT_BENEFICIARIO_CANCELADO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_CANCELADO)

rm(BENEF_ES)

write.csv(BENEF_ES_ATIVO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_ES_ATIVO.csv")
write.csv(BENEF_ES_ADERIDO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_ES_ADERIDO.csv")
write.csv(BENEF_ES_CANCELADO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_ES_CANCELADO.csv")

rm(BENEF_ES_ATIVO,BENEF_ES_ADERIDO,BENEF_ES_CANCELADO)

#### IMPORTAR AS BASES DE DADOS - GOIÁS ####

setwd("G:/Economia/1_Base de Informações/ANS - Info Consolidadas de Beneficiários/Histórico Bases/Bases/")

memory.size(max=10^12)
gc(TRUE)
ben201612_GO <- fread("ben201612_GO.csv")
ben201701_GO <- fread("ben201701_GO.csv")
ben201702_GO <- fread("ben201702_GO.csv")
ben201703_GO <- fread("ben201703_GO.csv")
ben201704_GO <- fread("ben201704_GO.csv")
ben201705_GO <- fread("ben201705_GO.csv")
ben201706_GO <- fread("ben201706_GO.csv")
ben201707_GO <- fread("ben201707_GO.csv")
ben201708_GO <- fread("ben201708_GO.csv")
ben201709_GO <- fread("ben201709_GO.csv")
ben201710_GO <- fread("ben201710_GO.csv")
ben201711_GO <- fread("ben201711_GO.csv")
ben201712_GO <- fread("ben201712_GO.csv")
ben201801_GO <- fread("ben201801_GO.csv")
ben201802_GO <- fread("ben201802_GO.csv")
ben201803_GO <- fread("ben201803_GO.csv")
ben201804_GO <- fread("ben201804_GO.csv")
ben201805_GO <- fread("ben201805_GO.csv")
ben201806_GO <- fread("ben201806_GO.csv")
ben201807_GO <- fread("ben201807_GO.csv")
ben201808_GO <- fread("ben201808_GO.csv")
ben201809_GO <- fread("ben201809_GO.csv")
ben201810_GO <- fread("ben201810_GO.csv")
ben201811_GO <- fread("ben201811_GO.csv")
ben201812_GO <- fread("ben201812_GO.csv")
ben201901_GO <- fread("ben201901_GO.csv")
ben201902_GO <- fread("ben201902_GO.csv")
ben201903_GO <- fread("ben201903_GO.csv")

memory.size(max=10^12)
gc(TRUE)
BENEF_GO <- do.call("rbind", list(ben201612_GO,
                                  ben201701_GO,ben201702_GO,ben201703_GO,
                                  ben201704_GO,ben201705_GO,ben201706_GO,
                                  ben201707_GO,ben201708_GO,ben201709_GO,
                                  ben201710_GO,ben201711_GO,ben201712_GO,
                                  ben201801_GO,ben201802_GO,ben201803_GO,
                                  ben201804_GO,ben201805_GO,ben201806_GO,
                                  ben201807_GO,ben201808_GO,ben201809_GO,
                                  ben201810_GO,ben201811_GO,ben201812_GO,
                                  ben201901_GO,ben201902_GO,ben201903_GO))

rm(ben201612_GO,
   ben201701_GO,ben201702_GO,ben201703_GO,
   ben201704_GO,ben201705_GO,ben201706_GO,
   ben201707_GO,ben201708_GO,ben201709_GO,
   ben201710_GO,ben201711_GO,ben201712_GO,
   ben201801_GO,ben201802_GO,ben201803_GO,
   ben201804_GO,ben201805_GO,ben201806_GO,
   ben201807_GO,ben201808_GO,ben201809_GO,
   ben201810_GO,ben201811_GO,ben201812_GO,
   ben201901_GO,ben201902_GO,ben201903_GO)


BENEF_GO_ATIVO <- 
  BENEF_GO %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ATIVO=sum(QT_BENEFICIARIO_ATIVO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ATIVO)

BENEF_GO_ADERIDO <- 
  BENEF_GO %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ADERIDO=sum(QT_BENEFICIARIO_ADERIDO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ADERIDO)

BENEF_GO_CANCELADO <- 
  BENEF_GO %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_CANCELADO=sum(QT_BENEFICIARIO_CANCELADO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_CANCELADO)

rm(BENEF_GO)

write.csv(BENEF_GO_ATIVO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_GO_ATIVO.csv")
write.csv(BENEF_GO_ADERIDO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_GO_ADERIDO.csv")
write.csv(BENEF_GO_CANCELADO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_GO_CANCELADO.csv")

rm(BENEF_GO_ATIVO,BENEF_GO_ADERIDO,BENEF_GO_CANCELADO)

#### IMPORTAR AS BASES DE DADOS - MARANHÃO ####

setwd("G:/Economia/1_Base de Informações/ANS - Info Consolidadas de Beneficiários/Histórico Bases/Bases/")

memory.size(max=10^12)
gc(TRUE)
ben201612_MA <- fread("ben201612_MA.csv")
ben201701_MA <- fread("ben201701_MA.csv")
ben201702_MA <- fread("ben201702_MA.csv")
ben201703_MA <- fread("ben201703_MA.csv")
ben201704_MA <- fread("ben201704_MA.csv")
ben201705_MA <- fread("ben201705_MA.csv")
ben201706_MA <- fread("ben201706_MA.csv")
ben201707_MA <- fread("ben201707_MA.csv")
ben201708_MA <- fread("ben201708_MA.csv")
ben201709_MA <- fread("ben201709_MA.csv")
ben201710_MA <- fread("ben201710_MA.csv")
ben201711_MA <- fread("ben201711_MA.csv")
ben201712_MA <- fread("ben201712_MA.csv")
ben201801_MA <- fread("ben201801_MA.csv")
ben201802_MA <- fread("ben201802_MA.csv")
ben201803_MA <- fread("ben201803_MA.csv")
ben201804_MA <- fread("ben201804_MA.csv")
ben201805_MA <- fread("ben201805_MA.csv")
ben201806_MA <- fread("ben201806_MA.csv")
ben201807_MA <- fread("ben201807_MA.csv")
ben201808_MA <- fread("ben201808_MA.csv")
ben201809_MA <- fread("ben201809_MA.csv")
ben201810_MA <- fread("ben201810_MA.csv")
ben201811_MA <- fread("ben201811_MA.csv")
ben201812_MA <- fread("ben201812_MA.csv")
ben201901_MA <- fread("ben201901_MA.csv")
ben201902_MA <- fread("ben201902_MA.csv")
ben201903_MA <- fread("ben201903_MA.csv")

memory.size(max=10^12)
gc(TRUE)
BENEF_MA <- do.call("rbind", list(ben201612_MA,
                                  ben201701_MA,ben201702_MA,ben201703_MA,
                                  ben201704_MA,ben201705_MA,ben201706_MA,
                                  ben201707_MA,ben201708_MA,ben201709_MA,
                                  ben201710_MA,ben201711_MA,ben201712_MA,
                                  ben201801_MA,ben201802_MA,ben201803_MA,
                                  ben201804_MA,ben201805_MA,ben201806_MA,
                                  ben201807_MA,ben201808_MA,ben201809_MA,
                                  ben201810_MA,ben201811_MA,ben201812_MA,
                                  ben201901_MA,ben201902_MA,ben201903_MA))

rm(ben201612_MA,
   ben201701_MA,ben201702_MA,ben201703_MA,
   ben201704_MA,ben201705_MA,ben201706_MA,
   ben201707_MA,ben201708_MA,ben201709_MA,
   ben201710_MA,ben201711_MA,ben201712_MA,
   ben201801_MA,ben201802_MA,ben201803_MA,
   ben201804_MA,ben201805_MA,ben201806_MA,
   ben201807_MA,ben201808_MA,ben201809_MA,
   ben201810_MA,ben201811_MA,ben201812_MA,
   ben201901_MA,ben201902_MA,ben201903_MA)


BENEF_MA_ATIVO <- 
  BENEF_MA %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ATIVO=sum(QT_BENEFICIARIO_ATIVO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ATIVO)

BENEF_MA_ADERIDO <- 
  BENEF_MA %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ADERIDO=sum(QT_BENEFICIARIO_ADERIDO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ADERIDO)

BENEF_MA_CANCELADO <- 
  BENEF_MA %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_CANCELADO=sum(QT_BENEFICIARIO_CANCELADO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_CANCELADO)

rm(BENEF_MA)

write.csv(BENEF_MA_ATIVO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_MA_ATIVO.csv")
write.csv(BENEF_MA_ADERIDO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_MA_ADERIDO.csv")
write.csv(BENEF_MA_CANCELADO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_MA_CANCELADO.csv")

rm(BENEF_MA_ATIVO,BENEF_MA_ADERIDO,BENEF_MA_CANCELADO)

#### IMPORTAR AS BASES DE DADOS - MATO GROSSO ####

setwd("G:/Economia/1_Base de Informações/ANS - Info Consolidadas de Beneficiários/Histórico Bases/Bases/")

memory.size(max=10^12)
gc(TRUE)
ben201612_MT <- fread("ben201612_MT.csv")
ben201701_MT <- fread("ben201701_MT.csv")
ben201702_MT <- fread("ben201702_MT.csv")
ben201703_MT <- fread("ben201703_MT.csv")
ben201704_MT <- fread("ben201704_MT.csv")
ben201705_MT <- fread("ben201705_MT.csv")
ben201706_MT <- fread("ben201706_MT.csv")
ben201707_MT <- fread("ben201707_MT.csv")
ben201708_MT <- fread("ben201708_MT.csv")
ben201709_MT <- fread("ben201709_MT.csv")
ben201710_MT <- fread("ben201710_MT.csv")
ben201711_MT <- fread("ben201711_MT.csv")
ben201712_MT <- fread("ben201712_MT.csv")
ben201801_MT <- fread("ben201801_MT.csv")
ben201802_MT <- fread("ben201802_MT.csv")
ben201803_MT <- fread("ben201803_MT.csv")
ben201804_MT <- fread("ben201804_MT.csv")
ben201805_MT <- fread("ben201805_MT.csv")
ben201806_MT <- fread("ben201806_MT.csv")
ben201807_MT <- fread("ben201807_MT.csv")
ben201808_MT <- fread("ben201808_MT.csv")
ben201809_MT <- fread("ben201809_MT.csv")
ben201810_MT <- fread("ben201810_MT.csv")
ben201811_MT <- fread("ben201811_MT.csv")
ben201812_MT <- fread("ben201812_MT.csv")
ben201901_MT <- fread("ben201901_MT.csv")
ben201902_MT <- fread("ben201902_MT.csv")
ben201903_MT <- fread("ben201903_MT.csv")

memory.size(max=10^12)
gc(TRUE)
BENEF_MT <- do.call("rbind", list(ben201612_MT,
                                  ben201701_MT,ben201702_MT,ben201703_MT,
                                  ben201704_MT,ben201705_MT,ben201706_MT,
                                  ben201707_MT,ben201708_MT,ben201709_MT,
                                  ben201710_MT,ben201711_MT,ben201712_MT,
                                  ben201801_MT,ben201802_MT,ben201803_MT,
                                  ben201804_MT,ben201805_MT,ben201806_MT,
                                  ben201807_MT,ben201808_MT,ben201809_MT,
                                  ben201810_MT,ben201811_MT,ben201812_MT,
                                  ben201901_MT,ben201902_MT,ben201903_MT))

rm(ben201612_MT,
   ben201701_MT,ben201702_MT,ben201703_MT,
   ben201704_MT,ben201705_MT,ben201706_MT,
   ben201707_MT,ben201708_MT,ben201709_MT,
   ben201710_MT,ben201711_MT,ben201712_MT,
   ben201801_MT,ben201802_MT,ben201803_MT,
   ben201804_MT,ben201805_MT,ben201806_MT,
   ben201807_MT,ben201808_MT,ben201809_MT,
   ben201810_MT,ben201811_MT,ben201812_MT,
   ben201901_MT,ben201902_MT,ben201903_MT)


BENEF_MT_ATIVO <- 
  BENEF_MT %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ATIVO=sum(QT_BENEFICIARIO_ATIVO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ATIVO)

BENEF_MT_ADERIDO <- 
  BENEF_MT %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ADERIDO=sum(QT_BENEFICIARIO_ADERIDO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ADERIDO)

BENEF_MT_CANCELADO <- 
  BENEF_MT %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_CANCELADO=sum(QT_BENEFICIARIO_CANCELADO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_CANCELADO)

rm(BENEF_MT)

write.csv(BENEF_MT_ATIVO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_MT_ATIVO.csv")
write.csv(BENEF_MT_ADERIDO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_MT_ADERIDO.csv")
write.csv(BENEF_MT_CANCELADO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_MT_CANCELADO.csv")

rm(BENEF_MT_ATIVO,BENEF_MT_ADERIDO,BENEF_MT_CANCELADO)

#### IMPORTAR AS BASES DE DADOS - MATO GROSSO DO SUL ####

setwd("G:/Economia/1_Base de Informações/ANS - Info Consolidadas de Beneficiários/Histórico Bases/Bases/")

memory.size(max=10^12)
gc(TRUE)
ben201612_MS <- fread("ben201612_MS.csv")
ben201701_MS <- fread("ben201701_MS.csv")
ben201702_MS <- fread("ben201702_MS.csv")
ben201703_MS <- fread("ben201703_MS.csv")
ben201704_MS <- fread("ben201704_MS.csv")
ben201705_MS <- fread("ben201705_MS.csv")
ben201706_MS <- fread("ben201706_MS.csv")
ben201707_MS <- fread("ben201707_MS.csv")
ben201708_MS <- fread("ben201708_MS.csv")
ben201709_MS <- fread("ben201709_MS.csv")
ben201710_MS <- fread("ben201710_MS.csv")
ben201711_MS <- fread("ben201711_MS.csv")
ben201712_MS <- fread("ben201712_MS.csv")
ben201801_MS <- fread("ben201801_MS.csv")
ben201802_MS <- fread("ben201802_MS.csv")
ben201803_MS <- fread("ben201803_MS.csv")
ben201804_MS <- fread("ben201804_MS.csv")
ben201805_MS <- fread("ben201805_MS.csv")
ben201806_MS <- fread("ben201806_MS.csv")
ben201807_MS <- fread("ben201807_MS.csv")
ben201808_MS <- fread("ben201808_MS.csv")
ben201809_MS <- fread("ben201809_MS.csv")
ben201810_MS <- fread("ben201810_MS.csv")
ben201811_MS <- fread("ben201811_MS.csv")
ben201812_MS <- fread("ben201812_MS.csv")
ben201901_MS <- fread("ben201901_MS.csv")
ben201902_MS <- fread("ben201902_MS.csv")
ben201903_MS <- fread("ben201903_MS.csv")

memory.size(max=10^12)
gc(TRUE)
BENEF_MS <- do.call("rbind", list(ben201612_MS,
                                  ben201701_MS,ben201702_MS,ben201703_MS,
                                  ben201704_MS,ben201705_MS,ben201706_MS,
                                  ben201707_MS,ben201708_MS,ben201709_MS,
                                  ben201710_MS,ben201711_MS,ben201712_MS,
                                  ben201801_MS,ben201802_MS,ben201803_MS,
                                  ben201804_MS,ben201805_MS,ben201806_MS,
                                  ben201807_MS,ben201808_MS,ben201809_MS,
                                  ben201810_MS,ben201811_MS,ben201812_MS,
                                  ben201901_MS,ben201902_MS,ben201903_MS))

rm(ben201612_MS,
   ben201701_MS,ben201702_MS,ben201703_MS,
   ben201704_MS,ben201705_MS,ben201706_MS,
   ben201707_MS,ben201708_MS,ben201709_MS,
   ben201710_MS,ben201711_MS,ben201712_MS,
   ben201801_MS,ben201802_MS,ben201803_MS,
   ben201804_MS,ben201805_MS,ben201806_MS,
   ben201807_MS,ben201808_MS,ben201809_MS,
   ben201810_MS,ben201811_MS,ben201812_MS,
   ben201901_MS,ben201902_MS,ben201903_MS)


BENEF_MS_ATIVO <- 
  BENEF_MS %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ATIVO=sum(QT_BENEFICIARIO_ATIVO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ATIVO)

BENEF_MS_ADERIDO <- 
  BENEF_MS %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ADERIDO=sum(QT_BENEFICIARIO_ADERIDO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ADERIDO)

BENEF_MS_CANCELADO <- 
  BENEF_MS %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_CANCELADO=sum(QT_BENEFICIARIO_CANCELADO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_CANCELADO)

rm(BENEF_MS)

write.csv(BENEF_MS_ATIVO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_MS_ATIVO.csv")
write.csv(BENEF_MS_ADERIDO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_MS_ADERIDO.csv")
write.csv(BENEF_MS_CANCELADO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_MS_CANCELADO.csv")

rm(BENEF_MS_ATIVO,BENEF_MS_ADERIDO,BENEF_MS_CANCELADO)

#### IMPORTAR AS BASES DE DADOS - PARÁ ####

setwd("G:/Economia/1_Base de Informações/ANS - Info Consolidadas de Beneficiários/Histórico Bases/Bases/")

memory.size(max=10^12)
gc(TRUE)
ben201612_PA <- fread("ben201612_PA.csv")
ben201701_PA <- fread("ben201701_PA.csv")
ben201702_PA <- fread("ben201702_PA.csv")
ben201703_PA <- fread("ben201703_PA.csv")
ben201704_PA <- fread("ben201704_PA.csv")
ben201705_PA <- fread("ben201705_PA.csv")
ben201706_PA <- fread("ben201706_PA.csv")
ben201707_PA <- fread("ben201707_PA.csv")
ben201708_PA <- fread("ben201708_PA.csv")
ben201709_PA <- fread("ben201709_PA.csv")
ben201710_PA <- fread("ben201710_PA.csv")
ben201711_PA <- fread("ben201711_PA.csv")
ben201712_PA <- fread("ben201712_PA.csv")
ben201801_PA <- fread("ben201801_PA.csv")
ben201802_PA <- fread("ben201802_PA.csv")
ben201803_PA <- fread("ben201803_PA.csv")
ben201804_PA <- fread("ben201804_PA.csv")
ben201805_PA <- fread("ben201805_PA.csv")
ben201806_PA <- fread("ben201806_PA.csv")
ben201807_PA <- fread("ben201807_PA.csv")
ben201808_PA <- fread("ben201808_PA.csv")
ben201809_PA <- fread("ben201809_PA.csv")
ben201810_PA <- fread("ben201810_PA.csv")
ben201811_PA <- fread("ben201811_PA.csv")
ben201812_PA <- fread("ben201812_PA.csv")
ben201901_PA <- fread("ben201901_PA.csv")
ben201902_PA <- fread("ben201902_PA.csv")
ben201903_PA <- fread("ben201903_PA.csv")

memory.size(max=10^12)
gc(TRUE)
BENEF_PA <- do.call("rbind", list(ben201612_PA,
                                  ben201701_PA,ben201702_PA,ben201703_PA,
                                  ben201704_PA,ben201705_PA,ben201706_PA,
                                  ben201707_PA,ben201708_PA,ben201709_PA,
                                  ben201710_PA,ben201711_PA,ben201712_PA,
                                  ben201801_PA,ben201802_PA,ben201803_PA,
                                  ben201804_PA,ben201805_PA,ben201806_PA,
                                  ben201807_PA,ben201808_PA,ben201809_PA,
                                  ben201810_PA,ben201811_PA,ben201812_PA,
                                  ben201901_PA,ben201902_PA,ben201903_PA))

rm(ben201612_PA,
   ben201701_PA,ben201702_PA,ben201703_PA,
   ben201704_PA,ben201705_PA,ben201706_PA,
   ben201707_PA,ben201708_PA,ben201709_PA,
   ben201710_PA,ben201711_PA,ben201712_PA,
   ben201801_PA,ben201802_PA,ben201803_PA,
   ben201804_PA,ben201805_PA,ben201806_PA,
   ben201807_PA,ben201808_PA,ben201809_PA,
   ben201810_PA,ben201811_PA,ben201812_PA,
   ben201901_PA,ben201902_PA,ben201903_PA)


BENEF_PA_ATIVO <- 
  BENEF_PA %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ATIVO=sum(QT_BENEFICIARIO_ATIVO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ATIVO)

BENEF_PA_ADERIDO <- 
  BENEF_PA %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ADERIDO=sum(QT_BENEFICIARIO_ADERIDO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ADERIDO)

BENEF_PA_CANCELADO <- 
  BENEF_PA %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_CANCELADO=sum(QT_BENEFICIARIO_CANCELADO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_CANCELADO)

rm(BENEF_PA)

write.csv(BENEF_PA_ATIVO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_PA_ATIVO.csv")
write.csv(BENEF_PA_ADERIDO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_PA_ADERIDO.csv")
write.csv(BENEF_PA_CANCELADO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_PA_CANCELADO.csv")

rm(BENEF_PA_ATIVO,BENEF_PA_ADERIDO,BENEF_PA_CANCELADO)

#### IMPORTAR AS BASES DE DADOS - PERNAMBUCO ####

setwd("G:/Economia/1_Base de Informações/ANS - Info Consolidadas de Beneficiários/Histórico Bases/Bases/")

memory.size(max=10^12)
gc(TRUE)
ben201612_PE <- fread("ben201612_PE.csv")
ben201701_PE <- fread("ben201701_PE.csv")
ben201702_PE <- fread("ben201702_PE.csv")
ben201703_PE <- fread("ben201703_PE.csv")
ben201704_PE <- fread("ben201704_PE.csv")
ben201705_PE <- fread("ben201705_PE.csv")
ben201706_PE <- fread("ben201706_PE.csv")
ben201707_PE <- fread("ben201707_PE.csv")
ben201708_PE <- fread("ben201708_PE.csv")
ben201709_PE <- fread("ben201709_PE.csv")
ben201710_PE <- fread("ben201710_PE.csv")
ben201711_PE <- fread("ben201711_PE.csv")
ben201712_PE <- fread("ben201712_PE.csv")
ben201801_PE <- fread("ben201801_PE.csv")
ben201802_PE <- fread("ben201802_PE.csv")
ben201803_PE <- fread("ben201803_PE.csv")
ben201804_PE <- fread("ben201804_PE.csv")
ben201805_PE <- fread("ben201805_PE.csv")
ben201806_PE <- fread("ben201806_PE.csv")
ben201807_PE <- fread("ben201807_PE.csv")
ben201808_PE <- fread("ben201808_PE.csv")
ben201809_PE <- fread("ben201809_PE.csv")
ben201810_PE <- fread("ben201810_PE.csv")
ben201811_PE <- fread("ben201811_PE.csv")
ben201812_PE <- fread("ben201812_PE.csv")
ben201901_PE <- fread("ben201901_PE.csv")
ben201902_PE <- fread("ben201902_PE.csv")
ben201903_PE <- fread("ben201903_PE.csv")

memory.size(max=10^12)
gc(TRUE)
BENEF_PE <- do.call("rbind", list(ben201612_PE,
                                  ben201701_PE,ben201702_PE,ben201703_PE,
                                  ben201704_PE,ben201705_PE,ben201706_PE,
                                  ben201707_PE,ben201708_PE,ben201709_PE,
                                  ben201710_PE,ben201711_PE,ben201712_PE,
                                  ben201801_PE,ben201802_PE,ben201803_PE,
                                  ben201804_PE,ben201805_PE,ben201806_PE,
                                  ben201807_PE,ben201808_PE,ben201809_PE,
                                  ben201810_PE,ben201811_PE,ben201812_PE,
                                  ben201901_PE,ben201902_PE,ben201903_PE))

rm(ben201612_PE,
   ben201701_PE,ben201702_PE,ben201703_PE,
   ben201704_PE,ben201705_PE,ben201706_PE,
   ben201707_PE,ben201708_PE,ben201709_PE,
   ben201710_PE,ben201711_PE,ben201712_PE,
   ben201801_PE,ben201802_PE,ben201803_PE,
   ben201804_PE,ben201805_PE,ben201806_PE,
   ben201807_PE,ben201808_PE,ben201809_PE,
   ben201810_PE,ben201811_PE,ben201812_PE,
   ben201901_PE,ben201902_PE,ben201903_PE)


BENEF_PE_ATIVO <- 
  BENEF_PE %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ATIVO=sum(QT_BENEFICIARIO_ATIVO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ATIVO)

BENEF_PE_ADERIDO <- 
  BENEF_PE %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ADERIDO=sum(QT_BENEFICIARIO_ADERIDO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ADERIDO)

BENEF_PE_CANCELADO <- 
  BENEF_PE %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_CANCELADO=sum(QT_BENEFICIARIO_CANCELADO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_CANCELADO)

rm(BENEF_PE)

write.csv(BENEF_PE_ATIVO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_PE_ATIVO.csv")
write.csv(BENEF_PE_ADERIDO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_PE_ADERIDO.csv")
write.csv(BENEF_PE_CANCELADO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_PE_CANCELADO.csv")

rm(BENEF_PE_ATIVO,BENEF_PE_ADERIDO,BENEF_PE_CANCELADO)

#### IMPORTAR AS BASES DE DADOS - PIAUÍ ####

setwd("G:/Economia/1_Base de Informações/ANS - Info Consolidadas de Beneficiários/Histórico Bases/Bases/")

memory.size(max=10^12)
gc(TRUE)
ben201612_PI <- fread("ben201612_PI.csv")
ben201701_PI <- fread("ben201701_PI.csv")
ben201702_PI <- fread("ben201702_PI.csv")
ben201703_PI <- fread("ben201703_PI.csv")
ben201704_PI <- fread("ben201704_PI.csv")
ben201705_PI <- fread("ben201705_PI.csv")
ben201706_PI <- fread("ben201706_PI.csv")
ben201707_PI <- fread("ben201707_PI.csv")
ben201708_PI <- fread("ben201708_PI.csv")
ben201709_PI <- fread("ben201709_PI.csv")
ben201710_PI <- fread("ben201710_PI.csv")
ben201711_PI <- fread("ben201711_PI.csv")
ben201712_PI <- fread("ben201712_PI.csv")
ben201801_PI <- fread("ben201801_PI.csv")
ben201802_PI <- fread("ben201802_PI.csv")
ben201803_PI <- fread("ben201803_PI.csv")
ben201804_PI <- fread("ben201804_PI.csv")
ben201805_PI <- fread("ben201805_PI.csv")
ben201806_PI <- fread("ben201806_PI.csv")
ben201807_PI <- fread("ben201807_PI.csv")
ben201808_PI <- fread("ben201808_PI.csv")
ben201809_PI <- fread("ben201809_PI.csv")
ben201810_PI <- fread("ben201810_PI.csv")
ben201811_PI <- fread("ben201811_PI.csv")
ben201812_PI <- fread("ben201812_PI.csv")
ben201901_PI <- fread("ben201901_PI.csv")
ben201902_PI <- fread("ben201902_PI.csv")
ben201903_PI <- fread("ben201903_PI.csv")

memory.size(max=10^12)
gc(TRUE)
BENEF_PI <- do.call("rbind", list(ben201612_PI,
                                  ben201701_PI,ben201702_PI,ben201703_PI,
                                  ben201704_PI,ben201705_PI,ben201706_PI,
                                  ben201707_PI,ben201708_PI,ben201709_PI,
                                  ben201710_PI,ben201711_PI,ben201712_PI,
                                  ben201801_PI,ben201802_PI,ben201803_PI,
                                  ben201804_PI,ben201805_PI,ben201806_PI,
                                  ben201807_PI,ben201808_PI,ben201809_PI,
                                  ben201810_PI,ben201811_PI,ben201812_PI,
                                  ben201901_PI,ben201902_PI,ben201903_PI))

rm(ben201612_PI,
   ben201701_PI,ben201702_PI,ben201703_PI,
   ben201704_PI,ben201705_PI,ben201706_PI,
   ben201707_PI,ben201708_PI,ben201709_PI,
   ben201710_PI,ben201711_PI,ben201712_PI,
   ben201801_PI,ben201802_PI,ben201803_PI,
   ben201804_PI,ben201805_PI,ben201806_PI,
   ben201807_PI,ben201808_PI,ben201809_PI,
   ben201810_PI,ben201811_PI,ben201812_PI,
   ben201901_PI,ben201902_PI,ben201903_PI)


BENEF_PI_ATIVO <- 
  BENEF_PI %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ATIVO=sum(QT_BENEFICIARIO_ATIVO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ATIVO)

BENEF_PI_ADERIDO <- 
  BENEF_PI %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ADERIDO=sum(QT_BENEFICIARIO_ADERIDO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ADERIDO)

BENEF_PI_CANCELADO <- 
  BENEF_PI %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_CANCELADO=sum(QT_BENEFICIARIO_CANCELADO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_CANCELADO)

rm(BENEF_PI)

write.csv(BENEF_PI_ATIVO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_PI_ATIVO.csv")
write.csv(BENEF_PI_ADERIDO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_PI_ADERIDO.csv")
write.csv(BENEF_PI_CANCELADO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_PI_CANCELADO.csv")

rm(BENEF_PI_ATIVO,BENEF_PI_ADERIDO,BENEF_PI_CANCELADO)

#### IMPORTAR AS BASES DE DADOS - RIO GRANDE DO NORTE ####

setwd("G:/Economia/1_Base de Informações/ANS - Info Consolidadas de Beneficiários/Histórico Bases/Bases/")

memory.size(max=10^12)
gc(TRUE)
ben201612_RN <- fread("ben201612_RN.csv")
ben201701_RN <- fread("ben201701_RN.csv")
ben201702_RN <- fread("ben201702_RN.csv")
ben201703_RN <- fread("ben201703_RN.csv")
ben201704_RN <- fread("ben201704_RN.csv")
ben201705_RN <- fread("ben201705_RN.csv")
ben201706_RN <- fread("ben201706_RN.csv")
ben201707_RN <- fread("ben201707_RN.csv")
ben201708_RN <- fread("ben201708_RN.csv")
ben201709_RN <- fread("ben201709_RN.csv")
ben201710_RN <- fread("ben201710_RN.csv")
ben201711_RN <- fread("ben201711_RN.csv")
ben201712_RN <- fread("ben201712_RN.csv")
ben201801_RN <- fread("ben201801_RN.csv")
ben201802_RN <- fread("ben201802_RN.csv")
ben201803_RN <- fread("ben201803_RN.csv")
ben201804_RN <- fread("ben201804_RN.csv")
ben201805_RN <- fread("ben201805_RN.csv")
ben201806_RN <- fread("ben201806_RN.csv")
ben201807_RN <- fread("ben201807_RN.csv")
ben201808_RN <- fread("ben201808_RN.csv")
ben201809_RN <- fread("ben201809_RN.csv")
ben201810_RN <- fread("ben201810_RN.csv")
ben201811_RN <- fread("ben201811_RN.csv")
ben201812_RN <- fread("ben201812_RN.csv")
ben201901_RN <- fread("ben201901_RN.csv")
ben201902_RN <- fread("ben201902_RN.csv")
ben201903_RN <- fread("ben201903_RN.csv")

memory.size(max=10^12)
gc(TRUE)
BENEF_RN <- do.call("rbind", list(ben201612_RN,
                                  ben201701_RN,ben201702_RN,ben201703_RN,
                                  ben201704_RN,ben201705_RN,ben201706_RN,
                                  ben201707_RN,ben201708_RN,ben201709_RN,
                                  ben201710_RN,ben201711_RN,ben201712_RN,
                                  ben201801_RN,ben201802_RN,ben201803_RN,
                                  ben201804_RN,ben201805_RN,ben201806_RN,
                                  ben201807_RN,ben201808_RN,ben201809_RN,
                                  ben201810_RN,ben201811_RN,ben201812_RN,
                                  ben201901_RN,ben201902_RN,ben201903_RN))

rm(ben201612_RN,
   ben201701_RN,ben201702_RN,ben201703_RN,
   ben201704_RN,ben201705_RN,ben201706_RN,
   ben201707_RN,ben201708_RN,ben201709_RN,
   ben201710_RN,ben201711_RN,ben201712_RN,
   ben201801_RN,ben201802_RN,ben201803_RN,
   ben201804_RN,ben201805_RN,ben201806_RN,
   ben201807_RN,ben201808_RN,ben201809_RN,
   ben201810_RN,ben201811_RN,ben201812_RN,
   ben201901_RN,ben201902_RN,ben201903_RN)


BENEF_RN_ATIVO <- 
  BENEF_RN %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ATIVO=sum(QT_BENEFICIARIO_ATIVO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ATIVO)

BENEF_RN_ADERIDO <- 
  BENEF_RN %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ADERIDO=sum(QT_BENEFICIARIO_ADERIDO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ADERIDO)

BENEF_RN_CANCELADO <- 
  BENEF_RN %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_CANCELADO=sum(QT_BENEFICIARIO_CANCELADO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_CANCELADO)

rm(BENEF_RN)

write.csv(BENEF_RN_ATIVO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_RN_ATIVO.csv")
write.csv(BENEF_RN_ADERIDO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_RN_ADERIDO.csv")
write.csv(BENEF_RN_CANCELADO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_RN_CANCELADO.csv")

rm(BENEF_RN_ATIVO,BENEF_RN_ADERIDO,BENEF_RN_CANCELADO)

#### IMPORTAR AS BASES DE DADOS - RONDÔNIA ####

setwd("G:/Economia/1_Base de Informações/ANS - Info Consolidadas de Beneficiários/Histórico Bases/Bases/")

memory.size(max=10^12)
gc(TRUE)
ben201612_RO <- fread("ben201612_RO.csv")
ben201701_RO <- fread("ben201701_RO.csv")
ben201702_RO <- fread("ben201702_RO.csv")
ben201703_RO <- fread("ben201703_RO.csv")
ben201704_RO <- fread("ben201704_RO.csv")
ben201705_RO <- fread("ben201705_RO.csv")
ben201706_RO <- fread("ben201706_RO.csv")
ben201707_RO <- fread("ben201707_RO.csv")
ben201708_RO <- fread("ben201708_RO.csv")
ben201709_RO <- fread("ben201709_RO.csv")
ben201710_RO <- fread("ben201710_RO.csv")
ben201711_RO <- fread("ben201711_RO.csv")
ben201712_RO <- fread("ben201712_RO.csv")
ben201801_RO <- fread("ben201801_RO.csv")
ben201802_RO <- fread("ben201802_RO.csv")
ben201803_RO <- fread("ben201803_RO.csv")
ben201804_RO <- fread("ben201804_RO.csv")
ben201805_RO <- fread("ben201805_RO.csv")
ben201806_RO <- fread("ben201806_RO.csv")
ben201807_RO <- fread("ben201807_RO.csv")
ben201808_RO <- fread("ben201808_RO.csv")
ben201809_RO <- fread("ben201809_RO.csv")
ben201810_RO <- fread("ben201810_RO.csv")
ben201811_RO <- fread("ben201811_RO.csv")
ben201812_RO <- fread("ben201812_RO.csv")
ben201901_RO <- fread("ben201901_RO.csv")
ben201902_RO <- fread("ben201902_RO.csv")
ben201903_RO <- fread("ben201903_RO.csv")

memory.size(max=10^12)
gc(TRUE)
BENEF_RO <- do.call("rbind", list(ben201612_RO,
                                  ben201701_RO,ben201702_RO,ben201703_RO,
                                  ben201704_RO,ben201705_RO,ben201706_RO,
                                  ben201707_RO,ben201708_RO,ben201709_RO,
                                  ben201710_RO,ben201711_RO,ben201712_RO,
                                  ben201801_RO,ben201802_RO,ben201803_RO,
                                  ben201804_RO,ben201805_RO,ben201806_RO,
                                  ben201807_RO,ben201808_RO,ben201809_RO,
                                  ben201810_RO,ben201811_RO,ben201812_RO,
                                  ben201901_RO,ben201902_RO,ben201903_RO))

rm(ben201612_RO,
   ben201701_RO,ben201702_RO,ben201703_RO,
   ben201704_RO,ben201705_RO,ben201706_RO,
   ben201707_RO,ben201708_RO,ben201709_RO,
   ben201710_RO,ben201711_RO,ben201712_RO,
   ben201801_RO,ben201802_RO,ben201803_RO,
   ben201804_RO,ben201805_RO,ben201806_RO,
   ben201807_RO,ben201808_RO,ben201809_RO,
   ben201810_RO,ben201811_RO,ben201812_RO,
   ben201901_RO,ben201902_RO,ben201903_RO)


BENEF_RO_ATIVO <- 
  BENEF_RO %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ATIVO=sum(QT_BENEFICIARIO_ATIVO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ATIVO)

BENEF_RO_ADERIDO <- 
  BENEF_RO %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ADERIDO=sum(QT_BENEFICIARIO_ADERIDO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ADERIDO)

BENEF_RO_CANCELADO <- 
  BENEF_RO %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_CANCELADO=sum(QT_BENEFICIARIO_CANCELADO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_CANCELADO)

rm(BENEF_RO)

write.csv(BENEF_RO_ATIVO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_RO_ATIVO.csv")
write.csv(BENEF_RO_ADERIDO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_RO_ADERIDO.csv")
write.csv(BENEF_RO_CANCELADO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_RO_CANCELADO.csv")

rm(BENEF_RO_ATIVO,BENEF_RO_ADERIDO,BENEF_RO_CANCELADO)

#### IMPORTAR AS BASES DE DADOS - RORAIMA ####

setwd("G:/Economia/1_Base de Informações/ANS - Info Consolidadas de Beneficiários/Histórico Bases/Bases/")

memory.size(max=10^12)
gc(TRUE)
ben201612_RR <- fread("ben201612_RR.csv")
ben201701_RR <- fread("ben201701_RR.csv")
ben201702_RR <- fread("ben201702_RR.csv")
ben201703_RR <- fread("ben201703_RR.csv")
ben201704_RR <- fread("ben201704_RR.csv")
ben201705_RR <- fread("ben201705_RR.csv")
ben201706_RR <- fread("ben201706_RR.csv")
ben201707_RR <- fread("ben201707_RR.csv")
ben201708_RR <- fread("ben201708_RR.csv")
ben201709_RR <- fread("ben201709_RR.csv")
ben201710_RR <- fread("ben201710_RR.csv")
ben201711_RR <- fread("ben201711_RR.csv")
ben201712_RR <- fread("ben201712_RR.csv")
ben201801_RR <- fread("ben201801_RR.csv")
ben201802_RR <- fread("ben201802_RR.csv")
ben201803_RR <- fread("ben201803_RR.csv")
ben201804_RR <- fread("ben201804_RR.csv")
ben201805_RR <- fread("ben201805_RR.csv")
ben201806_RR <- fread("ben201806_RR.csv")
ben201807_RR <- fread("ben201807_RR.csv")
ben201808_RR <- fread("ben201808_RR.csv")
ben201809_RR <- fread("ben201809_RR.csv")
ben201810_RR <- fread("ben201810_RR.csv")
ben201811_RR <- fread("ben201811_RR.csv")
ben201812_RR <- fread("ben201812_RR.csv")
ben201901_RR <- fread("ben201901_RR.csv")
ben201902_RR <- fread("ben201902_RR.csv")
ben201903_RR <- fread("ben201903_RR.csv")

memory.size(max=10^12)
gc(TRUE)
BENEF_RR <- do.call("rbind", list(ben201612_RR,
                                  ben201701_RR,ben201702_RR,ben201703_RR,
                                  ben201704_RR,ben201705_RR,ben201706_RR,
                                  ben201707_RR,ben201708_RR,ben201709_RR,
                                  ben201710_RR,ben201711_RR,ben201712_RR,
                                  ben201801_RR,ben201802_RR,ben201803_RR,
                                  ben201804_RR,ben201805_RR,ben201806_RR,
                                  ben201807_RR,ben201808_RR,ben201809_RR,
                                  ben201810_RR,ben201811_RR,ben201812_RR,
                                  ben201901_RR,ben201902_RR,ben201903_RR))

rm(ben201612_RR,
   ben201701_RR,ben201702_RR,ben201703_RR,
   ben201704_RR,ben201705_RR,ben201706_RR,
   ben201707_RR,ben201708_RR,ben201709_RR,
   ben201710_RR,ben201711_RR,ben201712_RR,
   ben201801_RR,ben201802_RR,ben201803_RR,
   ben201804_RR,ben201805_RR,ben201806_RR,
   ben201807_RR,ben201808_RR,ben201809_RR,
   ben201810_RR,ben201811_RR,ben201812_RR,
   ben201901_RR,ben201902_RR,ben201903_RR)


BENEF_RR_ATIVO <- 
  BENEF_RR %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ATIVO=sum(QT_BENEFICIARIO_ATIVO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ATIVO)

BENEF_RR_ADERIDO <- 
  BENEF_RR %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ADERIDO=sum(QT_BENEFICIARIO_ADERIDO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ADERIDO)

BENEF_RR_CANCELADO <- 
  BENEF_RR %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_CANCELADO=sum(QT_BENEFICIARIO_CANCELADO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_CANCELADO)

rm(BENEF_RR)

write.csv(BENEF_RR_ATIVO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_RR_ATIVO.csv")
write.csv(BENEF_RR_ADERIDO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_RR_ADERIDO.csv")
write.csv(BENEF_RR_CANCELADO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_RR_CANCELADO.csv")

rm(BENEF_RR_ATIVO,BENEF_RR_ADERIDO,BENEF_RR_CANCELADO)

#### IMPORTAR AS BASES DE DADOS - RIO GRANDE DO SUL ####

setwd("G:/Economia/1_Base de Informações/ANS - Info Consolidadas de Beneficiários/Histórico Bases/Bases/")

memory.size(max=10^12)
gc(TRUE)
ben201612_RS <- fread("ben201612_RS.csv")
ben201701_RS <- fread("ben201701_RS.csv")
ben201702_RS <- fread("ben201702_RS.csv")
ben201703_RS <- fread("ben201703_RS.csv")
ben201704_RS <- fread("ben201704_RS.csv")
ben201705_RS <- fread("ben201705_RS.csv")
ben201706_RS <- fread("ben201706_RS.csv")
ben201707_RS <- fread("ben201707_RS.csv")
ben201708_RS <- fread("ben201708_RS.csv")
ben201709_RS <- fread("ben201709_RS.csv")
ben201710_RS <- fread("ben201710_RS.csv")
ben201711_RS <- fread("ben201711_RS.csv")
ben201712_RS <- fread("ben201712_RS.csv")
ben201801_RS <- fread("ben201801_RS.csv")
ben201802_RS <- fread("ben201802_RS.csv")
ben201803_RS <- fread("ben201803_RS.csv")
ben201804_RS <- fread("ben201804_RS.csv")
ben201805_RS <- fread("ben201805_RS.csv")
ben201806_RS <- fread("ben201806_RS.csv")
ben201807_RS <- fread("ben201807_RS.csv")
ben201808_RS <- fread("ben201808_RS.csv")
ben201809_RS <- fread("ben201809_RS.csv")
ben201810_RS <- fread("ben201810_RS.csv")
ben201811_RS <- fread("ben201811_RS.csv")
ben201812_RS <- fread("ben201812_RS.csv")
ben201901_RS <- fread("ben201901_RS.csv")
ben201902_RS <- fread("ben201902_RS.csv")
ben201903_RS <- fread("ben201903_RS.csv")

memory.size(max=10^12)
gc(TRUE)
BENEF_RS <- do.call("rbind", list(ben201612_RS,
                                  ben201701_RS,ben201702_RS,ben201703_RS,
                                  ben201704_RS,ben201705_RS,ben201706_RS,
                                  ben201707_RS,ben201708_RS,ben201709_RS,
                                  ben201710_RS,ben201711_RS,ben201712_RS,
                                  ben201801_RS,ben201802_RS,ben201803_RS,
                                  ben201804_RS,ben201805_RS,ben201806_RS,
                                  ben201807_RS,ben201808_RS,ben201809_RS,
                                  ben201810_RS,ben201811_RS,ben201812_RS,
                                  ben201901_RS,ben201902_RS,ben201903_RS))

rm(ben201612_RS,
   ben201701_RS,ben201702_RS,ben201703_RS,
   ben201704_RS,ben201705_RS,ben201706_RS,
   ben201707_RS,ben201708_RS,ben201709_RS,
   ben201710_RS,ben201711_RS,ben201712_RS,
   ben201801_RS,ben201802_RS,ben201803_RS,
   ben201804_RS,ben201805_RS,ben201806_RS,
   ben201807_RS,ben201808_RS,ben201809_RS,
   ben201810_RS,ben201811_RS,ben201812_RS,
   ben201901_RS,ben201902_RS,ben201903_RS)


BENEF_RS_ATIVO <- 
  BENEF_RS %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ATIVO=sum(QT_BENEFICIARIO_ATIVO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ATIVO)

BENEF_RS_ADERIDO <- 
  BENEF_RS %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ADERIDO=sum(QT_BENEFICIARIO_ADERIDO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ADERIDO)

BENEF_RS_CANCELADO <- 
  BENEF_RS %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_CANCELADO=sum(QT_BENEFICIARIO_CANCELADO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_CANCELADO)

rm(BENEF_RS)

write.csv(BENEF_RS_ATIVO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_RS_ATIVO.csv")
write.csv(BENEF_RS_ADERIDO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_RS_ADERIDO.csv")
write.csv(BENEF_RS_CANCELADO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_RS_CANCELADO.csv")

rm(BENEF_RS_ATIVO,BENEF_RS_ADERIDO,BENEF_RS_CANCELADO)

#### IMPORTAR AS BASES DE DADOS - SANTA CATARINA ####

setwd("G:/Economia/1_Base de Informações/ANS - Info Consolidadas de Beneficiários/Histórico Bases/Bases/")

memory.size(max=10^12)
gc(TRUE)
ben201612_SC <- fread("ben201612_SC.csv")
ben201701_SC <- fread("ben201701_SC.csv")
ben201702_SC <- fread("ben201702_SC.csv")
ben201703_SC <- fread("ben201703_SC.csv")
ben201704_SC <- fread("ben201704_SC.csv")
ben201705_SC <- fread("ben201705_SC.csv")
ben201706_SC <- fread("ben201706_SC.csv")
ben201707_SC <- fread("ben201707_SC.csv")
ben201708_SC <- fread("ben201708_SC.csv")
ben201709_SC <- fread("ben201709_SC.csv")
ben201710_SC <- fread("ben201710_SC.csv")
ben201711_SC <- fread("ben201711_SC.csv")
ben201712_SC <- fread("ben201712_SC.csv")
ben201801_SC <- fread("ben201801_SC.csv")
ben201802_SC <- fread("ben201802_SC.csv")
ben201803_SC <- fread("ben201803_SC.csv")
ben201804_SC <- fread("ben201804_SC.csv")
ben201805_SC <- fread("ben201805_SC.csv")
ben201806_SC <- fread("ben201806_SC.csv")
ben201807_SC <- fread("ben201807_SC.csv")
ben201808_SC <- fread("ben201808_SC.csv")
ben201809_SC <- fread("ben201809_SC.csv")
ben201810_SC <- fread("ben201810_SC.csv")
ben201811_SC <- fread("ben201811_SC.csv")
ben201812_SC <- fread("ben201812_SC.csv")
ben201901_SC <- fread("ben201901_SC.csv")
ben201902_SC <- fread("ben201902_SC.csv")
ben201903_SC <- fread("ben201903_SC.csv")

memory.size(max=10^12)
gc(TRUE)
BENEF_SC <- do.call("rbind", list(ben201612_SC,
                                  ben201701_SC,ben201702_SC,ben201703_SC,
                                  ben201704_SC,ben201705_SC,ben201706_SC,
                                  ben201707_SC,ben201708_SC,ben201709_SC,
                                  ben201710_SC,ben201711_SC,ben201712_SC,
                                  ben201801_SC,ben201802_SC,ben201803_SC,
                                  ben201804_SC,ben201805_SC,ben201806_SC,
                                  ben201807_SC,ben201808_SC,ben201809_SC,
                                  ben201810_SC,ben201811_SC,ben201812_SC,
                                  ben201901_SC,ben201902_SC,ben201903_SC))

rm(ben201612_SC,
   ben201701_SC,ben201702_SC,ben201703_SC,
   ben201704_SC,ben201705_SC,ben201706_SC,
   ben201707_SC,ben201708_SC,ben201709_SC,
   ben201710_SC,ben201711_SC,ben201712_SC,
   ben201801_SC,ben201802_SC,ben201803_SC,
   ben201804_SC,ben201805_SC,ben201806_SC,
   ben201807_SC,ben201808_SC,ben201809_SC,
   ben201810_SC,ben201811_SC,ben201812_SC,
   ben201901_SC,ben201902_SC,ben201903_SC)


BENEF_SC_ATIVO <- 
  BENEF_SC %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ATIVO=sum(QT_BENEFICIARIO_ATIVO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ATIVO)

BENEF_SC_ADERIDO <- 
  BENEF_SC %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ADERIDO=sum(QT_BENEFICIARIO_ADERIDO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ADERIDO)

BENEF_SC_CANCELADO <- 
  BENEF_SC %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_CANCELADO=sum(QT_BENEFICIARIO_CANCELADO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_CANCELADO)

rm(BENEF_SC)

write.csv(BENEF_SC_ATIVO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_SC_ATIVO.csv")
write.csv(BENEF_SC_ADERIDO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_SC_ADERIDO.csv")
write.csv(BENEF_SC_CANCELADO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_SC_CANCELADO.csv")

rm(BENEF_SC_ATIVO,BENEF_SC_ADERIDO,BENEF_SC_CANCELADO)

#### IMPORTAR AS BASES DE DADOS - SERGIPE ####

setwd("G:/Economia/1_Base de Informações/ANS - Info Consolidadas de Beneficiários/Histórico Bases/Bases/")

memory.size(max=10^12)
gc(TRUE)
ben201612_SE <- fread("ben201612_SE.csv")
ben201701_SE <- fread("ben201701_SE.csv")
ben201702_SE <- fread("ben201702_SE.csv")
ben201703_SE <- fread("ben201703_SE.csv")
ben201704_SE <- fread("ben201704_SE.csv")
ben201705_SE <- fread("ben201705_SE.csv")
ben201706_SE <- fread("ben201706_SE.csv")
ben201707_SE <- fread("ben201707_SE.csv")
ben201708_SE <- fread("ben201708_SE.csv")
ben201709_SE <- fread("ben201709_SE.csv")
ben201710_SE <- fread("ben201710_SE.csv")
ben201711_SE <- fread("ben201711_SE.csv")
ben201712_SE <- fread("ben201712_SE.csv")
ben201801_SE <- fread("ben201801_SE.csv")
ben201802_SE <- fread("ben201802_SE.csv")
ben201803_SE <- fread("ben201803_SE.csv")
ben201804_SE <- fread("ben201804_SE.csv")
ben201805_SE <- fread("ben201805_SE.csv")
ben201806_SE <- fread("ben201806_SE.csv")
ben201807_SE <- fread("ben201807_SE.csv")
ben201808_SE <- fread("ben201808_SE.csv")
ben201809_SE <- fread("ben201809_SE.csv")
ben201810_SE <- fread("ben201810_SE.csv")
ben201811_SE <- fread("ben201811_SE.csv")
ben201812_SE <- fread("ben201812_SE.csv")
ben201901_SE <- fread("ben201901_SE.csv")
ben201902_SE <- fread("ben201902_SE.csv")
ben201903_SE <- fread("ben201903_SE.csv")

memory.size(max=10^12)
gc(TRUE)
BENEF_SE <- do.call("rbind", list(ben201612_SE,
                                  ben201701_SE,ben201702_SE,ben201703_SE,
                                  ben201704_SE,ben201705_SE,ben201706_SE,
                                  ben201707_SE,ben201708_SE,ben201709_SE,
                                  ben201710_SE,ben201711_SE,ben201712_SE,
                                  ben201801_SE,ben201802_SE,ben201803_SE,
                                  ben201804_SE,ben201805_SE,ben201806_SE,
                                  ben201807_SE,ben201808_SE,ben201809_SE,
                                  ben201810_SE,ben201811_SE,ben201812_SE,
                                  ben201901_SE,ben201902_SE,ben201903_SE))

rm(ben201612_SE,
   ben201701_SE,ben201702_SE,ben201703_SE,
   ben201704_SE,ben201705_SE,ben201706_SE,
   ben201707_SE,ben201708_SE,ben201709_SE,
   ben201710_SE,ben201711_SE,ben201712_SE,
   ben201801_SE,ben201802_SE,ben201803_SE,
   ben201804_SE,ben201805_SE,ben201806_SE,
   ben201807_SE,ben201808_SE,ben201809_SE,
   ben201810_SE,ben201811_SE,ben201812_SE,
   ben201901_SE,ben201902_SE,ben201903_SE)


BENEF_SE_ATIVO <- 
  BENEF_SE %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ATIVO=sum(QT_BENEFICIARIO_ATIVO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ATIVO)

BENEF_SE_ADERIDO <- 
  BENEF_SE %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ADERIDO=sum(QT_BENEFICIARIO_ADERIDO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ADERIDO)

BENEF_SE_CANCELADO <- 
  BENEF_SE %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_CANCELADO=sum(QT_BENEFICIARIO_CANCELADO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_CANCELADO)

rm(BENEF_SE)

write.csv(BENEF_SE_ATIVO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_SE_ATIVO.csv")
write.csv(BENEF_SE_ADERIDO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_SE_ADERIDO.csv")
write.csv(BENEF_SE_CANCELADO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_SE_CANCELADO.csv")

rm(BENEF_SE_ATIVO,BENEF_SE_ADERIDO,BENEF_SE_CANCELADO)

#### IMPORTAR AS BASES DE DADOS - TOCANTINS ####

setwd("G:/Economia/1_Base de Informações/ANS - Info Consolidadas de Beneficiários/Histórico Bases/Bases/")

memory.size(max=10^12)
gc(TRUE)
ben201612_TO <- fread("ben201612_TO.csv")
ben201701_TO <- fread("ben201701_TO.csv")
ben201702_TO <- fread("ben201702_TO.csv")
ben201703_TO <- fread("ben201703_TO.csv")
ben201704_TO <- fread("ben201704_TO.csv")
ben201705_TO <- fread("ben201705_TO.csv")
ben201706_TO <- fread("ben201706_TO.csv")
ben201707_TO <- fread("ben201707_TO.csv")
ben201708_TO <- fread("ben201708_TO.csv")
ben201709_TO <- fread("ben201709_TO.csv")
ben201710_TO <- fread("ben201710_TO.csv")
ben201711_TO <- fread("ben201711_TO.csv")
ben201712_TO <- fread("ben201712_TO.csv")
ben201801_TO <- fread("ben201801_TO.csv")
ben201802_TO <- fread("ben201802_TO.csv")
ben201803_TO <- fread("ben201803_TO.csv")
ben201804_TO <- fread("ben201804_TO.csv")
ben201805_TO <- fread("ben201805_TO.csv")
ben201806_TO <- fread("ben201806_TO.csv")
ben201807_TO <- fread("ben201807_TO.csv")
ben201808_TO <- fread("ben201808_TO.csv")
ben201809_TO <- fread("ben201809_TO.csv")
ben201810_TO <- fread("ben201810_TO.csv")
ben201811_TO <- fread("ben201811_TO.csv")
ben201812_TO <- fread("ben201812_TO.csv")
ben201901_TO <- fread("ben201901_TO.csv")
ben201902_TO <- fread("ben201902_TO.csv")
ben201903_TO <- fread("ben201903_TO.csv")

memory.size(max=10^12)
gc(TRUE)
BENEF_TO <- do.call("rbind", list(ben201612_TO,
                                  ben201701_TO,ben201702_TO,ben201703_TO,
                                  ben201704_TO,ben201705_TO,ben201706_TO,
                                  ben201707_TO,ben201708_TO,ben201709_TO,
                                  ben201710_TO,ben201711_TO,ben201712_TO,
                                  ben201801_TO,ben201802_TO,ben201803_TO,
                                  ben201804_TO,ben201805_TO,ben201806_TO,
                                  ben201807_TO,ben201808_TO,ben201809_TO,
                                  ben201810_TO,ben201811_TO,ben201812_TO,
                                  ben201901_TO,ben201902_TO,ben201903_TO))

rm(ben201612_TO,
   ben201701_TO,ben201702_TO,ben201703_TO,
   ben201704_TO,ben201705_TO,ben201706_TO,
   ben201707_TO,ben201708_TO,ben201709_TO,
   ben201710_TO,ben201711_TO,ben201712_TO,
   ben201801_TO,ben201802_TO,ben201803_TO,
   ben201804_TO,ben201805_TO,ben201806_TO,
   ben201807_TO,ben201808_TO,ben201809_TO,
   ben201810_TO,ben201811_TO,ben201812_TO,
   ben201901_TO,ben201902_TO,ben201903_TO)


BENEF_TO_ATIVO <- 
  BENEF_TO %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ATIVO=sum(QT_BENEFICIARIO_ATIVO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ATIVO)

BENEF_TO_ADERIDO <- 
  BENEF_TO %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ADERIDO=sum(QT_BENEFICIARIO_ADERIDO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ADERIDO)

BENEF_TO_CANCELADO <- 
  BENEF_TO %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_CANCELADO=sum(QT_BENEFICIARIO_CANCELADO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_CANCELADO)

rm(BENEF_TO)

write.csv(BENEF_TO_ATIVO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_TO_ATIVO.csv")
write.csv(BENEF_TO_ADERIDO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_TO_ADERIDO.csv")
write.csv(BENEF_TO_CANCELADO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_TO_CANCELADO.csv")

rm(BENEF_TO_ATIVO,BENEF_TO_ADERIDO,BENEF_TO_CANCELADO)


#### IMPORTAR AS BASES DE DADOS - PARAÍBA ####

setwd("G:/Economia/1_Base de Informações/ANS - Info Consolidadas de Beneficiários/Histórico Bases/Bases/")

memory.size(max=10^12)
gc(TRUE)
ben201612_PB <- fread("ben201612_PB.csv")
ben201701_PB <- fread("ben201701_PB.csv")
ben201702_PB <- fread("ben201702_PB.csv")
ben201703_PB <- fread("ben201703_PB.csv")
ben201704_PB <- fread("ben201704_PB.csv")
ben201705_PB <- fread("ben201705_PB.csv")
ben201706_PB <- fread("ben201706_PB.csv")
ben201707_PB <- fread("ben201707_PB.csv")
ben201708_PB <- fread("ben201708_PB.csv")
ben201709_PB <- fread("ben201709_PB.csv")
ben201710_PB <- fread("ben201710_PB.csv")
ben201711_PB <- fread("ben201711_PB.csv")
ben201712_PB <- fread("ben201712_PB.csv")
ben201801_PB <- fread("ben201801_PB.csv")
ben201802_PB <- fread("ben201802_PB.csv")
ben201803_PB <- fread("ben201803_PB.csv")
ben201804_PB <- fread("ben201804_PB.csv")
ben201805_PB <- fread("ben201805_PB.csv")
ben201806_PB <- fread("ben201806_PB.csv")
ben201807_PB <- fread("ben201807_PB.csv")
ben201808_PB <- fread("ben201808_PB.csv")
ben201809_PB <- fread("ben201809_PB.csv")
ben201810_PB <- fread("ben201810_PB.csv")
ben201811_PB <- fread("ben201811_PB.csv")
ben201812_PB <- fread("ben201812_PB.csv")
ben201901_PB <- fread("ben201901_PB.csv")
ben201902_PB <- fread("ben201902_PB.csv")
ben201903_PB <- fread("ben201903_PB.csv")

memory.size(max=10^12)
gc(TRUE)
BENEF_PB <- do.call("rbind", list(ben201612_PB,
                                  ben201701_PB,ben201702_PB,ben201703_PB,
                                  ben201704_PB,ben201705_PB,ben201706_PB,
                                  ben201707_PB,ben201708_PB,ben201709_PB,
                                  ben201710_PB,ben201711_PB,ben201712_PB,
                                  ben201801_PB,ben201802_PB,ben201803_PB,
                                  ben201804_PB,ben201805_PB,ben201806_PB,
                                  ben201807_PB,ben201808_PB,ben201809_PB,
                                  ben201810_PB,ben201811_PB,ben201812_PB,
                                  ben201901_PB,ben201902_PB,ben201903_PB))

rm(ben201612_PB,
   ben201701_PB,ben201702_PB,ben201703_PB,
   ben201704_PB,ben201705_PB,ben201706_PB,
   ben201707_PB,ben201708_PB,ben201709_PB,
   ben201710_PB,ben201711_PB,ben201712_PB,
   ben201801_PB,ben201802_PB,ben201803_PB,
   ben201804_PB,ben201805_PB,ben201806_PB,
   ben201807_PB,ben201808_PB,ben201809_PB,
   ben201810_PB,ben201811_PB,ben201812_PB,
   ben201901_PB,ben201902_PB,ben201903_PB)


BENEF_PB_ATIVO <- 
  BENEF_PB %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ATIVO=sum(QT_BENEFICIARIO_ATIVO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ATIVO)

BENEF_PB_ADERIDO <- 
  BENEF_PB %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ADERIDO=sum(QT_BENEFICIARIO_ADERIDO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ADERIDO)

BENEF_PB_CANCELADO <- 
  BENEF_PB %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_CANCELADO=sum(QT_BENEFICIARIO_CANCELADO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_CANCELADO)

rm(BENEF_PB)

write.csv(BENEF_PB_ATIVO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_PB_ATIVO.csv")
write.csv(BENEF_PB_ADERIDO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_PB_ADERIDO.csv")
write.csv(BENEF_PB_CANCELADO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_PB_CANCELADO.csv")

rm(BENEF_PB_ATIVO,BENEF_PB_ADERIDO,BENEF_PB_CANCELADO)


#### IMPORTAR AS BASES DE DADOS - NÃO IDENTIFICADO ####

setwd("G:/Economia/1_Base de Informações/ANS - Info Consolidadas de Beneficiários/Histórico Bases/Bases/")

memory.size(max=10^12)
gc(TRUE)
ben201612_XX <- fread("ben201612_XX.csv")
ben201701_XX <- fread("ben201701_XX.csv")
ben201702_XX <- fread("ben201702_XX.csv")
ben201703_XX <- fread("ben201703_XX.csv")
ben201704_XX <- fread("ben201704_XX.csv")
ben201705_XX <- fread("ben201705_XX.csv")
ben201706_XX <- fread("ben201706_XX.csv")
ben201707_XX <- fread("ben201707_XX.csv")
ben201708_XX <- fread("ben201708_XX.csv")
ben201709_XX <- fread("ben201709_XX.csv")
ben201710_XX <- fread("ben201710_XX.csv")
ben201711_XX <- fread("ben201711_XX.csv")
ben201712_XX <- fread("ben201712_XX.csv")
ben201801_XX <- fread("ben201801_XX.csv")
ben201802_XX <- fread("ben201802_XX.csv")
ben201803_XX <- fread("ben201803_XX.csv")
ben201804_XX <- fread("ben201804_XX.csv")
ben201805_XX <- fread("ben201805_XX.csv")
ben201806_XX <- fread("ben201806_XX.csv")
ben201807_XX <- fread("ben201807_XX.csv")
ben201808_XX <- fread("ben201808_XX.csv")
ben201809_XX <- fread("ben201809_XX.csv")
ben201810_XX <- fread("ben201810_XX.csv")
ben201811_XX <- fread("ben201811_XX.csv")
ben201812_XX <- fread("ben201812_XX.csv")
ben201901_XX <- fread("ben201901_XX.csv")
ben201902_XX <- fread("ben201902_XX.csv")
ben201903_XX <- fread("ben201903_XX.csv")

memory.size(max=10^12)
gc(TRUE)
BENEF_XX <- do.call("rbind", list(ben201612_XX,
                                  ben201701_XX,ben201702_XX,ben201703_XX,
                                  ben201704_XX,ben201705_XX,ben201706_XX,
                                  ben201707_XX,ben201708_XX,ben201709_XX,
                                  ben201710_XX,ben201711_XX,ben201712_XX,
                                  ben201801_XX,ben201802_XX,ben201803_XX,
                                  ben201804_XX,ben201805_XX,ben201806_XX,
                                  ben201807_XX,ben201808_XX,ben201809_XX,
                                  ben201810_XX,ben201811_XX,ben201812_XX,
                                  ben201901_XX,ben201902_XX,ben201903_XX))

rm(ben201612_XX,
   ben201701_XX,ben201702_XX,ben201703_XX,
   ben201704_XX,ben201705_XX,ben201706_XX,
   ben201707_XX,ben201708_XX,ben201709_XX,
   ben201710_XX,ben201711_XX,ben201712_XX,
   ben201801_XX,ben201802_XX,ben201803_XX,
   ben201804_XX,ben201805_XX,ben201806_XX,
   ben201807_XX,ben201808_XX,ben201809_XX,
   ben201810_XX,ben201811_XX,ben201812_XX,
   ben201901_XX,ben201902_XX,ben201903_XX)


BENEF_XX_ATIVO <- 
  BENEF_XX %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ATIVO=sum(QT_BENEFICIARIO_ATIVO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ATIVO)

BENEF_XX_ADERIDO <- 
  BENEF_XX %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_ADERIDO=sum(QT_BENEFICIARIO_ADERIDO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_ADERIDO)

BENEF_XX_CANCELADO <- 
  BENEF_XX %>% 
  filter(TP_VIGENCIA_PLANO %in% 'P') %>%
  mutate(DT_VIGENCIA = `#ID_CMPT_MOVEL`) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO_CANCELADO=sum(QT_BENEFICIARIO_CANCELADO)) %>%
  spread(key=DT_VIGENCIA,value=QTD_BENEFICIARIO_CANCELADO)

rm(BENEF_XX)

write.csv(BENEF_XX_ATIVO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_XX_ATIVO.csv")
write.csv(BENEF_XX_ADERIDO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_XX_ADERIDO.csv")
write.csv(BENEF_XX_CANCELADO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_XX_CANCELADO.csv")

rm(BENEF_XX_ATIVO,BENEF_XX_ADERIDO,BENEF_XX_CANCELADO)


#### IMPORTAR AS BASES DE DADOS - NÃO IDENTIFICADO ####

setwd("F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/")

# LISTAR OS ARQUIVOS E LER 

temp = list.files(pattern="*ADERIDO")
files = list2env(lapply(setNames(temp,make.names(gsub("*.csv$","",temp))),
                        read.csv),envir=.GlobalEnv)
rm(temp,files)

# AGRUPAR AS BASES

BENEF_SP_ADERIDO <- 
  BENEF_SP_ADERIDO1 %>%
  inner_join(BENEF_SP_ADERIDO2 %>%
               select(CD_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,
                      X201802:X201903),
             c("CD_OPERADORA","DE_CONTRATACAO_PLANO","COBERTURA_ASSIST_PLAN","DE_FAIXA_ETARIA_REAJ"))


memory.size(max=10^12)
BENEF_BR_ADERIDO <- rbind(BENEF_AC_ADERIDO,BENEF_AL_ADERIDO,BENEF_AM_ADERIDO,BENEF_AP_ADERIDO, 
                          BENEF_BA_ADERIDO,BENEF_CE_ADERIDO,BENEF_DF_ADERIDO,BENEF_ES_ADERIDO, 
                          BENEF_GO_ADERIDO,BENEF_MA_ADERIDO,BENEF_MG_ADERIDO,BENEF_MS_ADERIDO, 
                          BENEF_MT_ADERIDO,BENEF_PA_ADERIDO,BENEF_PB_ADERIDO,BENEF_PE_ADERIDO,
                          BENEF_PI_ADERIDO,BENEF_PR_ADERIDO,BENEF_RJ_ADERIDO,BENEF_RN_ADERIDO, 
                          BENEF_RO_ADERIDO,BENEF_RR_ADERIDO,BENEF_RS_ADERIDO,BENEF_SC_ADERIDO,
                          BENEF_SE_ADERIDO,BENEF_SP_ADERIDO,BENEF_TO_ADERIDO,BENEF_XX_ADERIDO)

rm(BENEF_AC_ADERIDO,BENEF_AL_ADERIDO,BENEF_AM_ADERIDO,BENEF_AP_ADERIDO, 
   BENEF_BA_ADERIDO,BENEF_CE_ADERIDO,BENEF_DF_ADERIDO,BENEF_ES_ADERIDO, 
   BENEF_GO_ADERIDO,BENEF_MA_ADERIDO,BENEF_MG_ADERIDO,BENEF_MS_ADERIDO, 
   BENEF_MT_ADERIDO,BENEF_PA_ADERIDO,BENEF_PB_ADERIDO,BENEF_PE_ADERIDO,
   BENEF_PI_ADERIDO,BENEF_PR_ADERIDO,BENEF_RJ_ADERIDO,BENEF_RN_ADERIDO, 
   BENEF_RO_ADERIDO,BENEF_RR_ADERIDO,BENEF_RS_ADERIDO,BENEF_SC_ADERIDO,
   BENEF_SE_ADERIDO,BENEF_SP_ADERIDO,BENEF_SP_ADERIDO1,BENEF_SP_ADERIDO2,
   BENEF_TO_ADERIDO,BENEF_XX_ADERIDO)


#### IMPORTAR AS BASES DE DADOS - CANCELADOS ####

# LISTAR OS ARQUIVOS E LER 

temp = list.files(pattern="*CANCELADO")
files = list2env(lapply(setNames(temp,make.names(gsub("*.csv$","",temp))),
                        read.csv),envir=.GlobalEnv)
rm(temp,files)

# AGRUPAR AS BASES

BENEF_SP_CANCELADO <- 
  BENEF_SP_CANCELADO1 %>%
  inner_join(BENEF_SP_CANCELADO2 %>%
               select(CD_OPERADORA,DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,
                      X201802:X201903),
             c("CD_OPERADORA","DE_CONTRATACAO_PLANO","COBERTURA_ASSIST_PLAN","DE_FAIXA_ETARIA_REAJ"))

memory.size(max=10^12)
BENEF_BR_CANCELADO <- rbind(BENEF_AC_CANCELADO,BENEF_AL_CANCELADO,BENEF_AM_CANCELADO,BENEF_AP_CANCELADO, 
                            BENEF_BA_CANCELADO,BENEF_CE_CANCELADO,BENEF_DF_CANCELADO,BENEF_ES_CANCELADO, 
                            BENEF_GO_CANCELADO,BENEF_MA_CANCELADO,BENEF_MG_CANCELADO,BENEF_MS_CANCELADO, 
                            BENEF_MT_CANCELADO,BENEF_PA_CANCELADO,BENEF_PB_CANCELADO,BENEF_PE_CANCELADO,
                            BENEF_PI_CANCELADO,BENEF_PR_CANCELADO,BENEF_RJ_CANCELADO,BENEF_RN_CANCELADO, 
                            BENEF_RO_CANCELADO,BENEF_RR_CANCELADO,BENEF_RS_CANCELADO,BENEF_SC_CANCELADO,
                            BENEF_SE_CANCELADO,BENEF_SP_CANCELADO,BENEF_TO_CANCELADO,BENEF_XX_CANCELADO)

rm(BENEF_AC_CANCELADO,BENEF_AL_CANCELADO,BENEF_AM_CANCELADO,BENEF_AP_CANCELADO, 
   BENEF_BA_CANCELADO,BENEF_CE_CANCELADO,BENEF_DF_CANCELADO,BENEF_ES_CANCELADO, 
   BENEF_GO_CANCELADO,BENEF_MA_CANCELADO,BENEF_MG_CANCELADO,BENEF_MS_CANCELADO, 
   BENEF_MT_CANCELADO,BENEF_PA_CANCELADO,BENEF_PB_CANCELADO,BENEF_PE_CANCELADO,
   BENEF_PI_CANCELADO,BENEF_PR_CANCELADO,BENEF_RJ_CANCELADO,BENEF_RN_CANCELADO, 
   BENEF_RO_CANCELADO,BENEF_RR_CANCELADO,BENEF_RS_CANCELADO,BENEF_SC_CANCELADO,
   BENEF_SE_CANCELADO,BENEF_SP_CANCELADO,BENEF_SP_CANCELADO1,BENEF_SP_CANCELADO2,
   BENEF_TO_CANCELADO,BENEF_XX_CANCELADO)


#### IMPORTAR AS BASES DE DADOS - CANCELADOS ####


BENEF_BR_ADERIDO_NEW <-
  BENEF_BR_ADERIDO %>%
  gather(key=DT_VIGENCIA,value=QTD,X201612:X201903) %>%
  mutate(QTD = replace_na(QTD,0)) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,
           DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,
           DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO=sum(QTD))

BENEF_BR_CANCELADO_NEW <-
  BENEF_BR_CANCELADO %>%
  gather(key=DT_VIGENCIA,value=QTD,X201612:X201903) %>%
  mutate(QTD = replace_na(QTD,0)) %>%
  group_by(CD_OPERADORA,NM_RAZAO_SOCIAL,MODALIDADE_OPERADORA,
           DE_CONTRATACAO_PLANO,COBERTURA_ASSIST_PLAN,DE_FAIXA_ETARIA_REAJ,
           DT_VIGENCIA) %>%
  summarise(QTD_BENEFICIARIO=sum(QTD))



#### EXPORTAR BASES DE DADOS ####

write.csv(BENEF_BR_ADERIDO_NEW,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_BR_ADERIDO.csv")
write.csv(BENEF_BR_CANCELADO_NEW,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Cancelamentos e Adesões/BENEF_BR_CANCELADO.csv")






