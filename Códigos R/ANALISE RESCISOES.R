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


#### IMPORTAÇÃO DAS INFOS SOBRE AS OPERADORAS ####

dados_cadop <- read.csv("F:/2019/Economia/6_Projetos/Relatório Variação Despesa Assistencial/0. Dados/dados_cadop.csv",
                        sep=";",
                        header=TRUE)

#### IMPORTAÇÃO DAS INFOS SOBRE OS PLANOS DE SAÚDE ####

caract_planos <- read.csv("F:/2019/Economia/6_Projetos/Relatório Variação Despesa Assistencial/0. Dados/SIB ANS/Características dos Produtos de Saúde Suplementar.csv",
                          sep=";",header=TRUE)

caract_planos$CD_PLANO_RPS <- as.integer(as.numeric(as.character(caract_planos$Código.do.Plano)))


#### IMPORTAÇÃO DAS INFOS SOBRE OS PLANOS DE SAÚDE ####

dados_planos <- read.csv("F:/2019/Economia/6_Projetos/Relatório Variação Despesa Assistencial/0. Dados/SIB ANS/dados_de_planos_de_saude.csv",
                         sep=";",header=TRUE)
gc(TRUE)



#### FUNÇÕES UTILIZADAS ####

## construção de função que faz a contagem do tempo de rescisão dos contratos

contagem_tempo_rescisao <- 
  function(data_frame)
  { 
    data_frame %>%
      
      ## EDITAR VARIÁVEIS DE DATA
      mutate(
        # Datas de Nascimento
        DT_NASCIMENTO=as.Date(as.character(DT_NASCIMENTO),"%d/%m/%Y"),
        # Datas de Contratação
        DT_PRIMEIRA_CONTRATACAO=as.Date(as.character(DT_PRIMEIRA_CONTRATACAO),"%d/%m/%Y"),
        # Datas de Cancelamento
        DT_CANCELAMENTO=as.Date(as.character(DT_CANCELAMENTO),"%d/%m/%Y")
      ) %>%
      ## SELEÇÃO DOS PERÍODOS
      filter(DT_PRIMEIRA_CONTRATACAO>="2016-12-01" & DT_PRIMEIRA_CONTRATACAO<="2018-12-01") %>%
      
      ## SELEÇÃO DOS PLANOS PÓS-LEI E CRUZAMENTO PARA TRAZER DADOS DOS PLANOS
      filter(!is.na(CD_PLANO_RPS)) %>%
      inner_join(caract_planos,"CD_PLANO_RPS") %>%
      ## CRUZAMENTO PARA TRAZER DADOS DAS OPERADORAS
      mutate(REGISTRO=CD_OPERADORA) %>%
      left_join(dados_cadop,"REGISTRO") %>%
      ## RENOMEAR VARIÁVEIS IMPORTANTES
      rename(.,NOME_OPERADORA = RAZAO_SOCIAL) %>%
      rename(.,MODALIDADE_OPERADORA = MODALIDADE) %>%
      rename(.,COBERTURA = Cobertura) %>%
      rename(.,TIPO_CONTRATACAO = Tipo.Contratação) %>%
      
      ## EDITAR VARIÁVEIS DE IDADE
      mutate(
        # Idade durante Contratação
        AGE_CONTRATACAO=round(as.numeric((DT_PRIMEIRA_CONTRATACAO-DT_NASCIMENTO)/365)),
        # Idade durante Cancelamento
        AGE_CANCELAMENTO=round(as.numeric((DT_CANCELAMENTO-DT_NASCIMENTO)/365))
      ) %>%
      
      ## SELECIONAR VARIÁVEIS DE INTERESSE
      select(
        # Datas de Contratação
        DT_PRIMEIRA_CONTRATACAO,
        # Datas de Cancelamento
        DT_CANCELAMENTO,
        # Datas de Nascimento
        DT_NASCIMENTO,
        # Idade do Beneficiário
        AGE_CONTRATACAO,AGE_CANCELAMENTO,
        # Infos das Operadoras
        CD_OPERADORA,NOME_OPERADORA,MODALIDADE_OPERADORA,
        # Infos dos Planos
        CD_PLANO_RPS,COBERTURA,TIPO_CONTRATACAO,
        # Infos sobre Movimentações
        CD_BENE_MOTV_INCLUSAO,CD_BENE_MOTIV_CANCELAMENTO,ID_MOTIVO_MOVIMENTO
      ) %>%
      
      ## CRIAR DATA DE COMPETÊNCIA
      mutate(
        # Extrair meses
        DT_REF_MES=month(as.POSIXlt(DT_PRIMEIRA_CONTRATACAO,format="%d/%m/%Y")),
        # Extrair anos
        DT_REF_ANO=year(as.POSIXlt(DT_PRIMEIRA_CONTRATACAO,format="%d/%m/%Y"))
      ) %>%
      mutate(
        DT_REFERENCIA=paste(DT_REF_MES,DT_REF_ANO,sep="_")
      ) %>%
      
      ## EDITAR VARIÁVEL DE MODALIDADE
      mutate(MODALIDADE = ifelse(MODALIDADE_OPERADORA=="Cooperativa odontológica","Cooperativa Odontológica",
                                 ifelse(MODALIDADE_OPERADORA=="Seguradora Especializada em Saúde","Seguradora",
                                        ifelse(MODALIDADE_OPERADORA=="Medicina de Grupo","Medicina de Grupo",
                                               ifelse(MODALIDADE_OPERADORA=="Cooperativa Médica","Cooperativa Médica",
                                                      ifelse(MODALIDADE_OPERADORA=="Seguradora","Seguradora",
                                                             ifelse(MODALIDADE_OPERADORA=="Odontologia de Grupo","Odontologia de Grupo",
                                                                    ifelse(MODALIDADE_OPERADORA=="Filantropia","Filantropia",
                                                                           ifelse(MODALIDADE_OPERADORA=="Autogestão","Autogestão","NA")
                                                                    )))))))
      ) %>%
      
      ## REALIZAR CONTAGEM DO TEMPO DE RESCISAO DOS CONTRATOS
      mutate(TEMPO_RESCISAO = as.numeric(DT_CANCELAMENTO-DT_PRIMEIRA_CONTRATACAO)) %>%
      ## CLASSIFICAR CONTRATOS CONFORME TEMPO DE RESCISAO
      mutate(CLASSIF_RESCISAO = ifelse(TEMPO_RESCISAO<=30,"0 a 1 Mês",
                                       ifelse(TEMPO_RESCISAO>=31&TEMPO_RESCISAO<=60,"1 a 2 Meses",
                                              ifelse(TEMPO_RESCISAO>=61&TEMPO_RESCISAO<=90,"2 a 3 Meses",
                                                     ifelse(TEMPO_RESCISAO>=91&TEMPO_RESCISAO<=120,"3 a 4 Meses",
                                                            ifelse(TEMPO_RESCISAO>=121&TEMPO_RESCISAO<=150,"4 a 5 Meses",
                                                                   ifelse(TEMPO_RESCISAO>=151&TEMPO_RESCISAO<=180,"5 a 6 Meses",
                                                                          ifelse(TEMPO_RESCISAO>=181&TEMPO_RESCISAO<=210,"6 a 7 Meses",
                                                                                 ifelse(TEMPO_RESCISAO>=211&TEMPO_RESCISAO<=240,"7 a 8 Meses",
                                                                                        ifelse(TEMPO_RESCISAO>=241&TEMPO_RESCISAO<=270,"8 a 9 Meses",
                                                                                               ifelse(TEMPO_RESCISAO>=271&TEMPO_RESCISAO<=300,"9 a 10 Meses",
                                                                                                      ifelse(TEMPO_RESCISAO>=301&TEMPO_RESCISAO<=330,"10 a 11 Meses",
                                                                                                             ifelse(TEMPO_RESCISAO>=331&TEMPO_RESCISAO<=360,"11 a 12 Meses",
                                                                                                                    ifelse(TEMPO_RESCISAO>=361,"Acima de 12 Meses","NA")
                                                                                                             ))))))))))))
      )
  }

## função que agrupa as classificações dos tempos de rescisão

agrupar_tempo_rescisao <- 
  function(data_frame)
  {
    name <- deparse(substitute(data_frame))
    data_frame %>%
      group_by(DT_REFERENCIA,
               MODALIDADE,COBERTURA,TIPO_CONTRATACAO,
               AGE_CONTRATACAO,AGE_CANCELAMENTO,
               CD_BENE_MOTV_INCLUSAO,CD_BENE_MOTIV_CANCELAMENTO,ID_MOTIVO_MOVIMENTO,
               CLASSIF_RESCISAO) %>%
      summarise(QUANTIDADE_CONTRATOS=n()) %>%
      mutate(UF = substr(name,14,15))
  }


#### PERNAMBUCO ####

memory.limit(size=10^12)
INATIVO_PE <- fread("F:/2019/Economia/6_Projetos/Relatório Variação Despesa Assistencial/0. Dados/SIB ANS/Inativos/sib_201811_PE.csv")
gc(TRUE)

memory.limit(size=10^12)
INATIVO_EDIT_PE <- contagem_tempo_rescisao(INATIVO_PE)
rm(INATIVO_PE)
gc(TRUE)
memory.limit(size=10^12)
RESCISAO_PE <- agrupar_tempo_rescisao(INATIVO_EDIT_PE)
rm(INATIVO_EDIT_PE)
gc(TRUE)

write.csv(RESCISAO_PE,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Vinculos/RESCISAO_PE.csv")

#### ACRE ####

memory.limit(size=10^12)
INATIVO_AC <- fread("F:/2019/Economia/6_Projetos/Relatório Variação Despesa Assistencial/0. Dados/SIB ANS/Inativos/sib_201811_AC.csv")
gc(TRUE)

memory.limit(size=10^12)
INATIVO_EDIT_AC <- contagem_tempo_rescisao(INATIVO_AC)
rm(INATIVO_AC)
gc(TRUE)
memory.limit(size=10^12)
RESCISAO_AC <- agrupar_tempo_rescisao(INATIVO_EDIT_AC)
rm(INATIVO_EDIT_AC)
gc(TRUE)

write.csv(RESCISAO_AC,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Vinculos/RESCISAO_AC.csv")

#### SÃO PAULO ####

memory.limit(size=10^12)
INATIVO_SP <- fread("F:/2019/Economia/6_Projetos/Relatório Variação Despesa Assistencial/0. Dados/SIB ANS/Inativos/sib_201811_SP.csv")
gc(TRUE)

memory.limit(size=10^12)
INATIVO_EDIT_SP <- contagem_tempo_rescisao(INATIVO_SP)
rm(INATIVO_SP)
gc(TRUE)
memory.limit(size=10^12)
RESCISAO_SP <- agrupar_tempo_rescisao(INATIVO_EDIT_SP)
rm(INATIVO_EDIT_SP)
gc(TRUE)

write.csv(RESCISAO_SP,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Vinculos/RESCISAO_SP.csv")

#### RIO DE JANEIRO ####

memory.limit(size=10^12)
INATIVO_RJ <- fread("F:/2019/Economia/6_Projetos/Relatório Variação Despesa Assistencial/0. Dados/SIB ANS/Inativos/sib_201811_RJ.csv")
gc(TRUE)

memory.limit(size=10^12)
INATIVO_EDIT_RJ <- contagem_tempo_rescisao(INATIVO_RJ)
rm(INATIVO_RJ)
gc(TRUE)
memory.limit(size=10^12)
RESCISAO_RJ <- agrupar_tempo_rescisao(INATIVO_EDIT_RJ)
rm(INATIVO_EDIT_RJ)
gc(TRUE)

write.csv(RESCISAO_RJ,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Vinculos/RESCISAO_RJ.csv")

#### MINAS GERAIS ####

memory.limit(size=10^12)
INATIVO_MG <- fread("F:/2019/Economia/6_Projetos/Relatório Variação Despesa Assistencial/0. Dados/SIB ANS/Inativos/sib_201811_MG.csv")
gc(TRUE)

memory.limit(size=10^12)
INATIVO_EDIT_MG <- contagem_tempo_rescisao(INATIVO_MG)
rm(INATIVO_MG)
gc(TRUE)
memory.limit(size=10^12)
RESCISAO_MG <- agrupar_tempo_rescisao(INATIVO_EDIT_MG)
rm(INATIVO_EDIT_MG)
gc(TRUE)

write.csv(RESCISAO_MG,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Vinculos/RESCISAO_MG.csv")

#### PARANÁ ####

memory.limit(size=10^12)
INATIVO_PR <- fread("F:/2019/Economia/6_Projetos/Relatório Variação Despesa Assistencial/0. Dados/SIB ANS/Inativos/sib_201811_PR.csv")
gc(TRUE)

memory.limit(size=10^12)
INATIVO_EDIT_PR <- contagem_tempo_rescisao(INATIVO_PR)
rm(INATIVO_PR)
gc(TRUE)
memory.limit(size=10^12)
RESCISAO_PR <- agrupar_tempo_rescisao(INATIVO_EDIT_PR)
rm(INATIVO_EDIT_PR)
gc(TRUE)

write.csv(RESCISAO_PR,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Vinculos/RESCISAO_PR.csv")

#### AMAZONAS ####

memory.limit(size=10^12)
INATIVO_AM <- fread("F:/2019/Economia/6_Projetos/Relatório Variação Despesa Assistencial/0. Dados/SIB ANS/Inativos/sib_201811_AM.csv")
gc(TRUE)

memory.limit(size=10^12)
INATIVO_EDIT_AM <- contagem_tempo_rescisao(INATIVO_AM)
rm(INATIVO_AM)
gc(TRUE)
memory.limit(size=10^12)
RESCISAO_AM <- agrupar_tempo_rescisao(INATIVO_EDIT_AM)
rm(INATIVO_EDIT_AM)
gc(TRUE)

write.csv(RESCISAO_AM,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Vinculos/RESCISAO_AM.csv")

#### AMAPÁ ####

memory.limit(size=10^12)
INATIVO_AP <- fread("F:/2019/Economia/6_Projetos/Relatório Variação Despesa Assistencial/0. Dados/SIB ANS/Inativos/sib_201811_AP.csv")
gc(TRUE)

memory.limit(size=10^12)
INATIVO_EDIT_AP <- contagem_tempo_rescisao(INATIVO_AP)
rm(INATIVO_AP)
gc(TRUE)
memory.limit(size=10^12)
RESCISAO_AP <- agrupar_tempo_rescisao(INATIVO_EDIT_AP)
rm(INATIVO_EDIT_AP)
gc(TRUE)

write.csv(RESCISAO_AP,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Vinculos/RESCISAO_AP.csv")

#### AMAPÁ ####

memory.limit(size=10^12)
INATIVO_AP <- fread("F:/2019/Economia/6_Projetos/Relatório Variação Despesa Assistencial/0. Dados/SIB ANS/Inativos/sib_201811_AP.csv")
gc(TRUE)

memory.limit(size=10^12)
INATIVO_EDIT_AP <- contagem_tempo_rescisao(INATIVO_AP)
rm(INATIVO_AP)
gc(TRUE)
memory.limit(size=10^12)
RESCISAO_AP <- agrupar_tempo_rescisao(INATIVO_EDIT_AP)
rm(INATIVO_EDIT_AP)
gc(TRUE)

write.csv(RESCISAO_AP,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Vinculos/RESCISAO_AP.csv")

#### BAHIA ####

memory.limit(size=10^12)
INATIVO_BA <- fread("F:/2019/Economia/6_Projetos/Relatório Variação Despesa Assistencial/0. Dados/SIB ANS/Inativos/sib_201811_BA.csv")
gc(TRUE)

memory.limit(size=10^12)
INATIVO_EDIT_BA <- contagem_tempo_rescisao(INATIVO_BA)
rm(INATIVO_BA)
gc(TRUE)
memory.limit(size=10^12)
RESCISAO_BA <- agrupar_tempo_rescisao(INATIVO_EDIT_BA)
rm(INATIVO_EDIT_BA)
gc(TRUE)

write.csv(RESCISAO_BA,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Vinculos/RESCISAO_BA.csv")

#### CEARÁ ####

memory.limit(size=10^12)
INATIVO_CE <- fread("F:/2019/Economia/6_Projetos/Relatório Variação Despesa Assistencial/0. Dados/SIB ANS/Inativos/sib_201811_CE.csv")
gc(TRUE)

memory.limit(size=10^12)
INATIVO_EDIT_CE <- contagem_tempo_rescisao(INATIVO_CE)
rm(INATIVO_CE)
gc(TRUE)
memory.limit(size=10^12)
RESCISAO_CE <- agrupar_tempo_rescisao(INATIVO_EDIT_CE)
rm(INATIVO_EDIT_CE)
gc(TRUE)

write.csv(RESCISAO_CE,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Vinculos/RESCISAO_CE.csv")

#### DISTRITO FEDERAL ####

memory.limit(size=10^12)
INATIVO_DF <- fread("F:/2019/Economia/6_Projetos/Relatório Variação Despesa Assistencial/0. Dados/SIB ANS/Inativos/sib_201811_DF.csv")
gc(TRUE)

memory.limit(size=10^12)
INATIVO_EDIT_DF <- contagem_tempo_rescisao(INATIVO_DF)
rm(INATIVO_DF)
gc(TRUE)
memory.limit(size=10^12)
RESCISAO_DF <- agrupar_tempo_rescisao(INATIVO_EDIT_DF)
rm(INATIVO_EDIT_DF)
gc(TRUE)

write.csv(RESCISAO_DF,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Vinculos/RESCISAO_DF.csv")

#### GOIÁS ####

memory.limit(size=10^12)
INATIVO_GO <- fread("F:/2019/Economia/6_Projetos/Relatório Variação Despesa Assistencial/0. Dados/SIB ANS/Inativos/sib_201811_GO.csv")
gc(TRUE)

memory.limit(size=10^12)
INATIVO_EDIT_GO <- contagem_tempo_rescisao(INATIVO_GO)
rm(INATIVO_GO)
gc(TRUE)
memory.limit(size=10^12)
RESCISAO_GO <- agrupar_tempo_rescisao(INATIVO_EDIT_GO)
rm(INATIVO_EDIT_GO)
gc(TRUE)

write.csv(RESCISAO_GO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Vinculos/RESCISAO_GO.csv")

#### ESPÍRITO SANTO ####

memory.limit(size=10^12)
INATIVO_ES <- fread("F:/2019/Economia/6_Projetos/Relatório Variação Despesa Assistencial/0. Dados/SIB ANS/Inativos/sib_201811_ES.csv")
gc(TRUE)

memory.limit(size=10^12)
INATIVO_EDIT_ES <- contagem_tempo_rescisao(INATIVO_ES)
rm(INATIVO_ES)
gc(TRUE)
memory.limit(size=10^12)
RESCISAO_ES <- agrupar_tempo_rescisao(INATIVO_EDIT_ES)
rm(INATIVO_EDIT_ES)
gc(TRUE)

write.csv(RESCISAO_ES,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Vinculos/RESCISAO_ES.csv")

#### MARANHÃO ####

memory.limit(size=10^12)
INATIVO_MA <- fread("F:/2019/Economia/6_Projetos/Relatório Variação Despesa Assistencial/0. Dados/SIB ANS/Inativos/sib_201811_MA.csv")
gc(TRUE)

memory.limit(size=10^12)
INATIVO_EDIT_MA <- contagem_tempo_rescisao(INATIVO_MA)
rm(INATIVO_MA)
gc(TRUE)
memory.limit(size=10^12)
RESCISAO_MA <- agrupar_tempo_rescisao(INATIVO_EDIT_MA)
rm(INATIVO_EDIT_MA)
gc(TRUE)

write.csv(RESCISAO_MA,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Vinculos/RESCISAO_MA.csv")

#### MATO GROSSO ####

memory.limit(size=10^12)
INATIVO_MT <- fread("F:/2019/Economia/6_Projetos/Relatório Variação Despesa Assistencial/0. Dados/SIB ANS/Inativos/sib_201811_MT.csv")
gc(TRUE)

memory.limit(size=10^12)
INATIVO_EDIT_MT <- contagem_tempo_rescisao(INATIVO_MT)
rm(INATIVO_MT)
gc(TRUE)
memory.limit(size=10^12)
RESCISAO_MT <- agrupar_tempo_rescisao(INATIVO_EDIT_MT)
rm(INATIVO_EDIT_MT)
gc(TRUE)

write.csv(RESCISAO_MT,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Vinculos/RESCISAO_MT.csv")

#### MATO GROSSO DO SUL ####

memory.limit(size=10^12)
INATIVO_MS <- fread("F:/2019/Economia/6_Projetos/Relatório Variação Despesa Assistencial/0. Dados/SIB ANS/Inativos/sib_201811_MS.csv")
gc(TRUE)

memory.limit(size=10^12)
INATIVO_EDIT_MS <- contagem_tempo_rescisao(INATIVO_MS)
rm(INATIVO_MS)
gc(TRUE)
memory.limit(size=10^12)
RESCISAO_MS <- agrupar_tempo_rescisao(INATIVO_EDIT_MS)
rm(INATIVO_EDIT_MS)
gc(TRUE)

write.csv(RESCISAO_MS,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Vinculos/RESCISAO_MS.csv")

#### PARÁ ####

memory.limit(size=10^12)
INATIVO_PA <- fread("F:/2019/Economia/6_Projetos/Relatório Variação Despesa Assistencial/0. Dados/SIB ANS/Inativos/sib_201811_PA.csv")
gc(TRUE)

memory.limit(size=10^12)
INATIVO_EDIT_PA <- contagem_tempo_rescisao(INATIVO_PA)
rm(INATIVO_PA)
gc(TRUE)
memory.limit(size=10^12)
RESCISAO_PA <- agrupar_tempo_rescisao(INATIVO_EDIT_PA)
rm(INATIVO_EDIT_PA)
gc(TRUE)

write.csv(RESCISAO_PA,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Vinculos/RESCISAO_PA.csv")

#### PARAÍBA ####

memory.limit(size=10^12)
INATIVO_PB <- fread("F:/2019/Economia/6_Projetos/Relatório Variação Despesa Assistencial/0. Dados/SIB ANS/Inativos/sib_201811_PB.csv")
gc(TRUE)

memory.limit(size=10^12)
INATIVO_EDIT_PB <- contagem_tempo_rescisao(INATIVO_PB)
rm(INATIVO_PB)
gc(TRUE)
memory.limit(size=10^12)
RESCISAO_PB <- agrupar_tempo_rescisao(INATIVO_EDIT_PB)
rm(INATIVO_EDIT_PB)
gc(TRUE)

write.csv(RESCISAO_PB,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Vinculos/RESCISAO_PB.csv")

#### PIAUÍ ####

memory.limit(size=10^12)
INATIVO_PI <- fread("F:/2019/Economia/6_Projetos/Relatório Variação Despesa Assistencial/0. Dados/SIB ANS/Inativos/sib_201811_PI.csv")
gc(TRUE)

memory.limit(size=10^12)
INATIVO_EDIT_PI <- contagem_tempo_rescisao(INATIVO_PI)
rm(INATIVO_PI)
gc(TRUE)
memory.limit(size=10^12)
RESCISAO_PI <- agrupar_tempo_rescisao(INATIVO_EDIT_PI)
rm(INATIVO_EDIT_PI)
gc(TRUE)

write.csv(RESCISAO_PI,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Vinculos/RESCISAO_PI.csv")

#### RIO GRANDE DO NORTE ####

memory.limit(size=10^12)
INATIVO_RN <- fread("F:/2019/Economia/6_Projetos/Relatório Variação Despesa Assistencial/0. Dados/SIB ANS/Inativos/sib_201811_RN.csv")
gc(TRUE)

memory.limit(size=10^12)
INATIVO_EDIT_RN <- contagem_tempo_rescisao(INATIVO_RN)
rm(INATIVO_RN)
gc(TRUE)
memory.limit(size=10^12)
RESCISAO_RN <- agrupar_tempo_rescisao(INATIVO_EDIT_RN)
rm(INATIVO_EDIT_RN)
gc(TRUE)

write.csv(RESCISAO_RN,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Vinculos/RESCISAO_RN.csv")

#### RIO GRANDE DO SUL ####

memory.limit(size=10^12)
INATIVO_RS <- fread("F:/2019/Economia/6_Projetos/Relatório Variação Despesa Assistencial/0. Dados/SIB ANS/Inativos/sib_201811_RS.csv")
gc(TRUE)

memory.limit(size=10^12)
INATIVO_EDIT_RS <- contagem_tempo_rescisao(INATIVO_RS)
rm(INATIVO_RS)
gc(TRUE)
memory.limit(size=10^12)
RESCISAO_RS <- agrupar_tempo_rescisao(INATIVO_EDIT_RS)
rm(INATIVO_EDIT_RS)
gc(TRUE)

write.csv(RESCISAO_RS,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Vinculos/RESCISAO_RS.csv")

#### RONDÔNIA ####

memory.limit(size=10^12)
INATIVO_RO <- fread("F:/2019/Economia/6_Projetos/Relatório Variação Despesa Assistencial/0. Dados/SIB ANS/Inativos/sib_201811_RO.csv")
gc(TRUE)

memory.limit(size=10^12)
INATIVO_EDIT_RO <- contagem_tempo_rescisao(INATIVO_RO)
rm(INATIVO_RO)
gc(TRUE)
memory.limit(size=10^12)
RESCISAO_RO <- agrupar_tempo_rescisao(INATIVO_EDIT_RO)
rm(INATIVO_EDIT_RO)
gc(TRUE)

write.csv(RESCISAO_RO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Vinculos/RESCISAO_RO.csv")

#### RORAIMA ####

memory.limit(size=10^12)
INATIVO_RR <- fread("F:/2019/Economia/6_Projetos/Relatório Variação Despesa Assistencial/0. Dados/SIB ANS/Inativos/sib_201811_RR.csv")
gc(TRUE)

memory.limit(size=10^12)
INATIVO_EDIT_RR <- contagem_tempo_rescisao(INATIVO_RR)
rm(INATIVO_RR)
gc(TRUE)
memory.limit(size=10^12)
RESCISAO_RR <- agrupar_tempo_rescisao(INATIVO_EDIT_RR)
rm(INATIVO_EDIT_RR)
gc(TRUE)

write.csv(RESCISAO_RR,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Vinculos/RESCISAO_RR.csv")

#### SANTA CATARINA ####

memory.limit(size=10^12)
INATIVO_SC <- fread("F:/2019/Economia/6_Projetos/Relatório Variação Despesa Assistencial/0. Dados/SIB ANS/Inativos/sib_201811_SC.csv")
gc(TRUE)

memory.limit(size=10^12)
INATIVO_EDIT_SC <- contagem_tempo_rescisao(INATIVO_SC)
rm(INATIVO_SC)
gc(TRUE)
memory.limit(size=10^12)
RESCISAO_SC <- agrupar_tempo_rescisao(INATIVO_EDIT_SC)
rm(INATIVO_EDIT_SC)
gc(TRUE)

write.csv(RESCISAO_SC,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Vinculos/RESCISAO_SC.csv")

#### SERGIPE ####

memory.limit(size=10^12)
INATIVO_SE <- fread("F:/2019/Economia/6_Projetos/Relatório Variação Despesa Assistencial/0. Dados/SIB ANS/Inativos/sib_201811_SE.csv")
gc(TRUE)

memory.limit(size=10^12)
INATIVO_EDIT_SE <- contagem_tempo_rescisao(INATIVO_SE)
rm(INATIVO_SE)
gc(TRUE)
memory.limit(size=10^12)
RESCISAO_SE <- agrupar_tempo_rescisao(INATIVO_EDIT_SE)
rm(INATIVO_EDIT_SE)
gc(TRUE)

write.csv(RESCISAO_SE,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Vinculos/RESCISAO_SE.csv")

#### TOCANTINS ####

memory.limit(size=10^12)
INATIVO_TO <- fread("F:/2019/Economia/6_Projetos/Relatório Variação Despesa Assistencial/0. Dados/SIB ANS/Inativos/sib_201811_TO.csv")
gc(TRUE)

memory.limit(size=10^12)
INATIVO_EDIT_TO <- contagem_tempo_rescisao(INATIVO_TO)
rm(INATIVO_TO)
gc(TRUE)
memory.limit(size=10^12)
RESCISAO_TO <- agrupar_tempo_rescisao(INATIVO_EDIT_TO)
rm(INATIVO_EDIT_TO)
gc(TRUE)

write.csv(RESCISAO_TO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Vinculos/RESCISAO_TO.csv")

#### ALAGOAS ####

memory.limit(size=10^12)
INATIVO_AL <- fread("F:/2019/Economia/6_Projetos/Relatório Variação Despesa Assistencial/0. Dados/SIB ANS/Inativos/sib_201811_AL.csv")
gc(TRUE)

memory.limit(size=10^12)
INATIVO_EDIT_AL <- contagem_tempo_rescisao(INATIVO_AL)
rm(INATIVO_AL)
gc(TRUE)
memory.limit(size=10^12)
RESCISAO_AL <- agrupar_tempo_rescisao(INATIVO_EDIT_AL)
rm(INATIVO_EDIT_AL)
gc(TRUE)

write.csv(RESCISAO_AL,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Vinculos/RESCISAO_AL.csv")


#### CONSTRUIR BASE NACIONAL ####

memory.size(max=10^12)
BASE_NACIONAL_RESCISAO <- rbind(RESCISAO_SE,RESCISAO_TO,RESCISAO_PR,RESCISAO_RN,
                                RESCISAO_MA,RESCISAO_PE,RESCISAO_MS,RESCISAO_SP,
                                RESCISAO_AC,RESCISAO_SC,RESCISAO_RS,RESCISAO_MT,
                                RESCISAO_GO,RESCISAO_RJ,RESCISAO_MG,RESCISAO_AM,
                                RESCISAO_ES,RESCISAO_AP,RESCISAO_CE,RESCISAO_RR,
                                RESCISAO_PA,RESCISAO_PI,RESCISAO_DF,RESCISAO_RO,
                                RESCISAO_BA,RESCISAO_AL,RESCISAO_PB)

rm(RESCISAO_SE,RESCISAO_TO,RESCISAO_PR,RESCISAO_RN,
   RESCISAO_MA,RESCISAO_PE,RESCISAO_MS,RESCISAO_SP,
   RESCISAO_AC,RESCISAO_SC,RESCISAO_RS,RESCISAO_MT,
   RESCISAO_GO,RESCISAO_RJ,RESCISAO_MG,RESCISAO_AM,
   RESCISAO_ES,RESCISAO_AP,RESCISAO_CE,RESCISAO_RR,
   RESCISAO_PA,RESCISAO_PI,RESCISAO_DF,RESCISAO_RO,
   RESCISAO_BA,RESCISAO_AL,RESCISAO_PB)

write.csv(BASE_NACIONAL_RESCISAO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Vinculos/BASE_NACIONAL_RESCISAO.csv")

gc(TRUE)

BASE_BR_RESCISAO <-
  BASE_NACIONAL_RESCISAO %>%
  ## CLASSIFICAR CONTRATOS CONFORME TEMPO DE RESCISAO
  mutate(CLASS_RESCISAO = ifelse(CLASSIF_RESCISAO=="0 a 1 Mês","0 a 01 Mês",
                                 ifelse(CLASSIF_RESCISAO=="1 a 2 Meses","01 a 02 Meses",
                                        ifelse(CLASSIF_RESCISAO=="2 a 3 Meses","02 a 03 Meses",
                                               ifelse(CLASSIF_RESCISAO=="3 a 4 Meses","03 a 04 Meses",
                                                      ifelse(CLASSIF_RESCISAO=="4 a 5 Meses","04 a 05 Meses",
                                                             ifelse(CLASSIF_RESCISAO=="5 a 6 Meses","05 a 06 Meses",
                                                                    ifelse(CLASSIF_RESCISAO=="6 a 7 Meses","06 a 07 Meses",
                                                                           ifelse(CLASSIF_RESCISAO=="7 a 8 Meses","07 a 08 Meses",
                                                                                  ifelse(CLASSIF_RESCISAO=="8 a 9 Meses","08 a 09 Meses",
                                                                                         ifelse(CLASSIF_RESCISAO=="9 a 10 Meses","09 a 10 Meses",
                                                                                                ifelse(CLASSIF_RESCISAO=="10 a 11 Meses","10 a 11 Meses",
                                                                                                       ifelse(CLASSIF_RESCISAO=="11 a 12 Meses","11 a 12 Meses",
                                                                                                              ifelse(CLASSIF_RESCISAO=="Acima de 12 Meses","Acima de 12 Meses","NA")
                                                                                                       ))))))))))))
  ) %>%
  ## CLASSIFICAR FAIXA-ETÁRIA REAJUSTE
  mutate(CLASS_AGE_CONTRATACAO = ifelse(AGE_CONTRATACAO>=0&AGE_CONTRATACAO<=18,"0 a 18 Anos",
                                        ifelse(AGE_CONTRATACAO>=19&AGE_CONTRATACAO<=23,"19 a 23 Anos",
                                               ifelse(AGE_CONTRATACAO>=24&AGE_CONTRATACAO<=28,"24 a 28 Anos",
                                                      ifelse(AGE_CONTRATACAO>=29&AGE_CONTRATACAO<=33,"29 a 33 Anos",
                                                             ifelse(AGE_CONTRATACAO>=34&AGE_CONTRATACAO<=38,"34 a 38 Anos",
                                                                    ifelse(AGE_CONTRATACAO>=39&AGE_CONTRATACAO<=43,"39 a 43 Anos",
                                                                           ifelse(AGE_CONTRATACAO>=44&AGE_CONTRATACAO<=48,"44 a 48 Anos",
                                                                                  ifelse(AGE_CONTRATACAO>=49&AGE_CONTRATACAO<=53,"49 a 53 Anos",
                                                                                         ifelse(AGE_CONTRATACAO>=54&AGE_CONTRATACAO<=58,"54 a 59 Anos",
                                                                                                ifelse(AGE_CONTRATACAO>=59,"59 Anos ou mais","NA")
                                                                                         )))))))))
  ) %>%
  mutate(CLASS_AGE_CANCELAMENTO = ifelse(AGE_CANCELAMENTO>=0&AGE_CANCELAMENTO<=18,"0 a 18 Anos",
                                         ifelse(AGE_CANCELAMENTO>=19&AGE_CANCELAMENTO<=23,"19 a 23 Anos",
                                                ifelse(AGE_CANCELAMENTO>=24&AGE_CANCELAMENTO<=28,"24 a 28 Anos",
                                                       ifelse(AGE_CANCELAMENTO>=29&AGE_CANCELAMENTO<=33,"29 a 33 Anos",
                                                              ifelse(AGE_CANCELAMENTO>=34&AGE_CANCELAMENTO<=38,"34 a 38 Anos",
                                                                     ifelse(AGE_CANCELAMENTO>=39&AGE_CANCELAMENTO<=43,"39 a 43 Anos",
                                                                            ifelse(AGE_CANCELAMENTO>=44&AGE_CANCELAMENTO<=48,"44 a 48 Anos",
                                                                                   ifelse(AGE_CANCELAMENTO>=49&AGE_CANCELAMENTO<=53,"49 a 53 Anos",
                                                                                          ifelse(AGE_CANCELAMENTO>=54&AGE_CANCELAMENTO<=58,"54 a 59 Anos",
                                                                                                 ifelse(AGE_CANCELAMENTO>=59,"59 Anos ou mais","NA")
                                                                                          )))))))))
  ) %>%
  ## CLASSIFICAR TIPO DE CANCELAMENTO
  mutate(CLASS_MOTIVO_CANCELAMENTO=ifelse(CD_BENE_MOTIV_CANCELAMENTO==41,"Iniciativa do Beneficiário",
                                          ifelse(CD_BENE_MOTIV_CANCELAMENTO==42,"Desligamento da Empresa",
                                                 ifelse(CD_BENE_MOTIV_CANCELAMENTO==43,"Inadimplência",
                                                        ifelse(CD_BENE_MOTIV_CANCELAMENTO==44,"Óbito do Beneficiário",
                                                               ifelse(CD_BENE_MOTIV_CANCELAMENTO==45,"Transferência de Carteira",
                                                                      ifelse(CD_BENE_MOTIV_CANCELAMENTO==46,"Inclusão Indevida",
                                                                             ifelse(CD_BENE_MOTIV_CANCELAMENTO==47,"Fraude",
                                                                                    ifelse(CD_BENE_MOTIV_CANCELAMENTO==47,"Portabilidade de Carência",
                                                                                           ifelse(CD_BENE_MOTIV_CANCELAMENTO==72,"Realizados pela ANS",
                                                                                                  ifelse(CD_BENE_MOTIV_CANCELAMENTO==73,"Transferência Automática de Beneficiários","NA")
                                                                                           )))))))))
  ) %>%
  group_by(DT_REFERENCIA,
           MODALIDADE,COBERTURA,TIPO_CONTRATACAO,
           CLASS_AGE_CONTRATACAO,CLASS_AGE_CANCELAMENTO,
           CLASS_MOTIVO_CANCELAMENTO,
           CLASS_RESCISAO) %>%
  summarise(QTD_CONTRATOS=sum(QUANTIDADE_CONTRATOS))

write.csv(BASE_BR_RESCISAO,"F:/2019/Economia/6_Projetos/Rescisão de Contratos/2. Simulação/Dados Vinculos/BASE_BR_RESCISAO.csv")


#### GRÁFICOS ####

# gráfico 1 - Rescisões x Motivo do Cancelamento

BASE_BR_RESCISAO %>%
  ## transformacao -------------------------------------------------------------
  filter(COBERTURA=="MÉDICO-HOSPITALAR") %>%
  filter(TIPO_CONTRATACAO=="COLETIVO EMPRESARIAL"|TIPO_CONTRATACAO=="COLETIVO POR ADESÃO") %>%
  group_by(CLASS_MOTIVO_CANCELAMENTO,TIPO_CONTRATACAO) %>%
  summarise(QTD_CONTRATOS=sum(QTD_CONTRATOS)) %>%
  ## grafico ------------------------------------------------------------------
  ggplot(aes(x=reorder(CLASS_MOTIVO_CANCELAMENTO,-QTD_CONTRATOS),
           y=QTD_CONTRATOS)) +
  geom_bar(colour="white",fill="cornsilk2",
           stat="identity",alpha=.5) +
  geom_text(aes(y=QTD_CONTRATOS,label=QTD_CONTRATOS),
            size=3) +
  ## edição visual ------------------------------------------------------------
  theme_hc() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        text=element_text(family="Helvetica Neue"),
        axis.text.x=element_text(angle=70,hjust=1)) +
  labs(fill='Tipo de Contratação') +
  facet_wrap(~TIPO_CONTRATACAO,scales="free")




# gráfico 2 - Rescisões x Tempo de Contrato

BASE_BR_RESCISAO %>%
  ## transformacao -------------------------------------------------------------
filter(COBERTURA=="MÉDICO-HOSPITALAR") %>%
  filter(TIPO_CONTRATACAO=="COLETIVO EMPRESARIAL"|TIPO_CONTRATACAO=="COLETIVO POR ADESÃO") %>%
  group_by(CLASS_RESCISAO,TIPO_CONTRATACAO) %>%
  summarise(QTD_CONTRATOS=sum(QTD_CONTRATOS)) %>%
  ## grafico ------------------------------------------------------------------
ggplot(aes(x=CLASS_RESCISAO,y=QTD_CONTRATOS,fill=TIPO_CONTRATACAO)) +
  geom_bar(fill="white",stat="identity",alpha=.5) +
  geom_text(aes(y=QTD_CONTRATOS,label=QTD_CONTRATOS),
            size=3,position=position_dodge(.5)) +
  ## edição visual ------------------------------------------------------------
theme_hc() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        text=element_text(family="Helvetica Neue"),
        axis.text.x=element_text(angle=70,hjust=1)) +
  labs(x='Tempo de Contrato', 
       y='Quantidade de Cancelamentos',
       fill='Tipo de Contratação')


BASE_BR_RESCISAO %>%
  ## transformacao -------------------------------------------------------------
filter(COBERTURA=="MÉDICO-HOSPITALAR") %>%
  filter(TIPO_CONTRATACAO=="COLETIVO EMPRESARIAL"|TIPO_CONTRATACAO=="COLETIVO POR ADESÃO") %>%
  group_by(CLASS_AGE_CANCELAMENTO,CLASS_MOTIVO_CANCELAMENTO,TIPO_CONTRATACAO) %>%
  summarise(QTD_CONTRATOS=sum(QTD_CONTRATOS)) %>%
  ## grafico ------------------------------------------------------------------
ggplot(aes(x=CLASS_AGE_CANCELAMENTO,y=QTD_CONTRATOS,fill=TIPO_CONTRATACAO)) +
  geom_bar(colour="white",stat="identity",alpha=.5) +
  geom_text(aes(y=QTD_CONTRATOS,label=QTD_CONTRATOS),
            size=3,vjust=-0.5,position=position_dodge(.5)) +
  ## edição visual ------------------------------------------------------------
theme_hc() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        text=element_text(family="Helvetica Neue"),
        axis.text.x=element_text(angle=70,hjust=1)) +
  labs(title="Planos Médico-Hospitalares") +
  facet_wrap(~CLASS_MOTIVO_CANCELAMENTO,scales="free")



