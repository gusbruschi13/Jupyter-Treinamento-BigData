
library(devtools)
library(janitor)
library(esaj)
library(abjutils)
library(httr)
library(XML)
library(xml2)
library(rvest)
library(dplyr)
library(tibble)
library(rlang)
library(stringr)
library(purrr)
library(zoo)
library(readr)
library(gplots)
library(ggplot2)
library(ggthemes)
library(colourpicker)
library(extrafont)
font_import()
loadfonts(device="win") 
fonts()

#### LISTAR ASSUNTOS E CAMARAS ####

assuntos <- esaj::cjsg_table("subjects")
camaras <- esaj::cjsg_table("courts")


#### ABRIR BASES TOTAL ####

cadop_ativas <-
  read.csv("G:/Economia/1_Base de Informa��es/7_ Dados Judiciais/Relatorio_cadop.csv",
           header=TRUE,sep=";")

cadop_canceladas <-
  read.csv("G:/Economia/1_Base de Informa��es/7_ Dados Judiciais/Relatorio_cadop_canceladas.csv",
           header=TRUE,sep=";")

cadop_todas <- rbind(cadop_ativas,
                     cadop_canceladas)

indicadores_qualidade_ANS <-
  read.csv("G:/Economia/1_Base de Informa��es/7_ Dados Judiciais/Indicadores_Qualidade_ANS.csv",
           header=TRUE,sep=";")

names_pt_lista <-
  read.csv("G:/Economia/1_Base de Informa��es/7_ Dados Judiciais/names_pt_lista.csv",
           header=TRUE,sep=";")


setwd("U:/2018/Judicializa��o da Sa�de/Dados/TJSP/DOCUMENTOS/Parse/")

d_cposg_2007_2017 <- 
  list.files(pattern = ".rds") %>%
  purrr::map(readRDS) %>% bind_rows()

df_tjsp_TOTAL <-
  readRDS("U:/2018/Judicializa��o da Sa�de/Dados/TJSP/BASES COMPLETAS/df_tjsp_TOTAL.rds")

df_tjsp_TOTAL$id <- df_tjsp_TOTAL$id_lawsuit

df_tjsp_TOTAL$date_registration_M <- 
  as.Date(as.yearmon(paste(df_tjsp_TOTAL$date_registration_YEAR,
                           df_tjsp_TOTAL$date_registration_MONTH),"%Y %m"),
          format="%d/%m/%Y")


#### ELABORA��O DOS GR�FICOS ####

#### 1. Gr�fico - Quantidade de Processos por Ano ####

# separar processos conforme data de registro e plotar o gr�fico

grafico_1 <-
  ## separar dados necess�rios ------------------------------------------------
  df_tjsp_TOTAL %>%
  dplyr::distinct(id,date_registration_YEAR) %>%
  group_by(date_registration_YEAR) %>% summarise(count=n()) %>%
  ## grafico ------------------------------------------------------------------
  ggplot(aes(x=date_registration_YEAR,
             y=count)) +
  geom_col(fill="steelblue2",colour="steelblue2") +
  geom_text(aes(y=count,label=count),size=4,
            vjust=-0.5,position=position_dodge(.9)) +
  scale_x_continuous(breaks=c(2007,2008,2009,2010,
                              2011,2012,2013,2014,
                              2015,2016,2017)) +
  scale_y_continuous(breaks=c(0,6000,12000,18000,24000)) +
  ## edi��o visual ------------------------------------------------------------
  theme_hc(13) +
  theme(axis.title.x=element_blank(),
        text=element_text(family="Helvetica Neue")) +
  labs(y='Quantidade de Processos')
  
grafico_1

## salvar gr�fico ------------------------------------------------------------

ggsave("F:/2018/Economia/5_Boletim ABRAMGE/13 Edi��o/0. Dados e Gr�ficos/Cap�tulo Especial - Judicializa��o/Apresenta��es/2� Apresenta��o/Gr�ficos/grafico_1.png",
       plot=grafico_1,
       width=16,height=12,
       units="cm",dpi=700)

#### 2. Gr�fico - Processos por Classe % ####

# criar tabela com processos por classe e situa��o

data_classe_situacao <-
  d_cposg_2007_2017 %>% 
  dplyr::select(id,data) %>% 
  tidyr::unnest(data) %>%
  dplyr::filter(data=='Classe'|
                data=='Situa��o') %>%
  dplyr::inner_join(.,df_tjsp_TOTAL,by=c('id')) %>%
  dplyr::select(id,data,value,date_registration_YEAR) %>%
  dplyr::distinct(id,data,value,date_registration_YEAR)

# separar processos conforme data de registro e plotar o gr�fico

grafico_2 <-
  ## separar dados necess�rios ------------------------------------------------
  data_classe_situacao %>% 
  dplyr::filter(data=='Classe') %>%
  group_by(value) %>%
  summarise(n=n()) %>% mutate(freq=(n/sum(n))*100) %>%
  arrange(desc(freq)) %>%
  ## grafico ------------------------------------------------------------------
  ggplot(aes(x=reorder(value,-freq),y=freq)) +
  geom_col(colour='tan1',fill='tan1') +
  geom_text(aes(y=freq,label=round(freq,3)),
            size=3.5,position=position_dodge(.5)) +
  coord_flip() +
  ## edi��o visual ------------------------------------------------------------
  theme_minimal(12) +
  theme(axis.title.y=element_blank(),
        text=element_text(family="Helvetica Neue")) +
  labs(y='%')

grafico_2

## salvar gr�fico ------------------------------------------------------------

ggsave("F:/2018/Economia/5_Boletim ABRAMGE/13 Edi��o/0. Dados e Gr�ficos/Cap�tulo Especial - Judicializa��o/Apresenta��es/2� Apresenta��o/Gr�ficos/grafico_2.png",
       plot=grafico_2,
       width=20,height=12,
       units="cm",dpi=700)


#### 3. Gr�fico - Processos por Situa��o % ####

# separar processos conforme data de registro e plotar o gr�fico

grafico_3 <-
  ## separar dados necess�rios ------------------------------------------------
  data_classe_situacao %>% 
  dplyr::filter(data=='Situa��o') %>%
  group_by(value) %>%
  summarise(n=n()) %>% mutate(freq=(n/sum(n))*100) %>%
  arrange(desc(freq)) %>%
  ## grafico ------------------------------------------------------------------
  ggplot(aes(x=reorder(value,-freq),y=freq)) +
  geom_col(colour='seagreen2',fill='seagreen2') +
  geom_text(aes(y=freq,label=round(freq,2)),
            size=3.5,position=position_dodge(.5)) +
  coord_flip() +
  ## edi��o visual ------------------------------------------------------------
  theme_minimal(12) +
  theme(axis.title.y=element_blank(),
        text=element_text(family="Helvetica Neue")) +
  labs(y='%')

grafico_3

## salvar gr�fico ------------------------------------------------------------

ggsave("F:/2018/Economia/5_Boletim ABRAMGE/13 Edi��o/0. Dados e Gr�ficos/Cap�tulo Especial - Judicializa��o/Apresenta��es/2� Apresenta��o/Gr�ficos/grafico_3.png",
       plot=grafico_3,
       width=20,height=12,
       units="cm",dpi=700)


#### 4. Gr�fico - Porcentagem de Decis�es em Apela��es ####

# criar tabela com processos por classe e situa��o

negaram <- stringr::regex('negara?m|nega-se|negam-se', ignore_case = TRUE)
parcial <- stringr::regex('parcial', ignore_case = TRUE)
deram <- stringr::regex('deram|mantiv|d�-se|nul|conhec', ignore_case = TRUE)
extinto <- stringr::regex('extin', ignore_case = TRUE)

tipos_decisao <- function(decisoes) # criar fun��o que classifica decis�es
  {
  dplyr::case_when(
    stringr::str_detect(decisoes, negaram) ~ 'negado',
    stringr::str_detect(decisoes, parcial) ~ 'parcial',
    stringr::str_detect(decisoes, deram) ~ 'provido',
    stringr::str_detect(decisoes, extinto) ~ 'extinto',
    TRUE ~ "outros")
  }

apelacoes <-
  data_classe_situacao %>%
  dplyr::filter(data=='Classe') %>%
  dplyr::filter(value=='Apela��o')

partes_apelacoes <-
  d_cposg_2007_2017 %>% 
  dplyr::select(id,parts) %>% 
  tidyr::unnest(parts) %>% 
  dplyr::filter(part == 'Apelado') %>%
  dplyr::inner_join(.,apelacoes,by=c('id')) %>%
  dplyr::select(id,part,data,value) %>%
  dplyr::distinct(id,part,data,value) %>%
  dplyr::inner_join(.,df_tjsp_TOTAL,by=c('id')) %>%
  dplyr::select(id,part,court,date_registration_YEAR,data,value) %>%
  dplyr::distinct(id,part,court,date_registration_YEAR,data,value) %>%
  dplyr::mutate(num_court = readr::parse_number(court),
                num_court = str_pad(num_court,2,pad="0"))

d_decisoes_apelacoes <-
  d_cposg_2007_2017 %>% 
  dplyr::select(id, decisions) %>% 
  tidyr::unnest(decisions) %>% 
  dplyr::inner_join(partes_apelacoes, "id") %>% 
  dplyr::arrange(desc(date_registration_YEAR)) %>%  
  dplyr::group_by(id) %>%
  dplyr::slice(1) %>% 
  dplyr::ungroup()

decisoes_partes_apelacoes <-
  d_decisoes_apelacoes %>% 
  dplyr::mutate(tipo_decisao = tipos_decisao(decision)) %>% 
  dplyr::select(id,date_registration_YEAR,
                num_court,decision,tipo_decisao)

prop_negados <- function(x) # fun��o propor��o de negados
{
  sum(x == "negado") / length(x)
}

# cruzar os nomes dos p�los ativos e passivos e classificar os nomes das empresas (REGEX OPERADORAS)

apelados_teste <- 
  d_cposg_2007_2017 %>% 
  dplyr::select(id,parts) %>% 
  tidyr::unnest(parts) %>% 
  dplyr::filter(part=='Apelado') %>% dplyr::filter(role=='Apelado') %>%
  dplyr::inner_join(decisoes_partes_apelacoes, "id") %>%
  dplyr::group_by(id) %>%
  dplyr::slice(1) %>% 
  dplyr::ungroup() %>%
  rename(apelado=name) %>%
  dplyr::select(id,apelado,part,role,num_court,date_registration_YEAR,decision,tipo_decisao) %>%
  dplyr::distinct(id,apelado,part,role,num_court,date_registration_YEAR,decision,tipo_decisao) %>%
  dplyr::mutate(ident_company_apelado=regex_list_OPS(apelado))

apelantes_teste <- 
  d_cposg_2007_2017 %>% 
  dplyr::select(id,parts) %>% 
  tidyr::unnest(parts) %>% 
  dplyr::filter(part=='Apelante') %>% dplyr::filter(role=='Apelante') %>%
  dplyr::inner_join(decisoes_partes_apelacoes, "id") %>%
  dplyr::group_by(id) %>%
  dplyr::slice(1) %>% 
  dplyr::ungroup() %>%
  rename(apelante=name) %>%
  dplyr::select(id,apelante,part,role,num_court,date_registration_YEAR,decision,tipo_decisao) %>%
  dplyr::distinct(id,apelante,part,role,num_court,date_registration_YEAR,decision,tipo_decisao) %>%
  dplyr::mutate(ident_company_apelante=regex_list_OPS(apelante))

apelados_apelantes_ident <- 
  apelados_teste %>% 
  dplyr::inner_join(apelantes_teste, "id") %>%
  dplyr::group_by(id) %>% dplyr::slice(1) %>% dplyr::ungroup() %>%
  # criar coluna com p�los ativo e passivo
  rename(num_court=num_court.x,
         date_registration_YEAR=date_registration_YEAR.x,
         decision=decision.x,tipo_decisao=tipo_decisao.y) %>%
  dplyr::select(id,apelante,apelado,
                ident_company_apelante,ident_company_apelado,
                num_court,date_registration_YEAR,decision,tipo_decisao) %>%
  dplyr::distinct(id,apelante,apelado,
                  ident_company_apelante,ident_company_apelado,
                  num_court,date_registration_YEAR,decision,tipo_decisao) %>%
  # encontrar comarcas e relatores
  dplyr::inner_join(df_tjsp_TOTAL,"id") %>%
  dplyr::group_by(id) %>% dplyr::slice(1) %>% dplyr::ungroup() %>%
  rename(date_registration_YEAR=date_registration_YEAR.x) %>%
  dplyr::select(id,apelante,apelado,
                ident_company_apelante,ident_company_apelado,
                num_court,district,rapporteur,date_registration_M,date_registration_YEAR,
                summary_clean,decision,tipo_decisao) %>%
  dplyr::distinct(id,apelante,apelado,
                  ident_company_apelante,ident_company_apelado,
                  num_court,district,rapporteur,date_registration_M,date_registration_YEAR,
                  summary_clean,decision,tipo_decisao) %>%
  # editar empresas importantes
  dplyr::mutate(nome_empresa_apelado=ifelse(ident_company_apelado=='AMIL ASSIST�NCIA M�DICA INTERNACIONAL S.A.'|
                                            ident_company_apelado=='AMIL SA�DE LTDA.'|
                                            ident_company_apelado=='AMICO SA�DE LTDA'|
                                            ident_company_apelado=='AMESP SISTEMA DE SA�DE LTDA',
                                            'AMIL (SOMA DO GRUPO)',
                                            ifelse(ident_company_apelado=='INTERM�DICA SISTEMA DE SAUDE LTDA'|
                                                   ident_company_apelado=='NOTRE DAME INTERM�DICA SA�DE S.A.'|
                                                   ident_company_apelado=='SANTAM�LIA SA�DE S.A.',
                                                   'INTERM�DICA (SOMA DO GRUPO)',
                                                   ident_company_apelado)))


# separar processos conforme data de registro e plotar o gr�fico

tabela_1 <-
  decisoes_partes_apelacoes %>%
  ## transformacao -------------------------------------------------------------
  dplyr::filter(!is.na(num_court)) %>%
  ## calcular propor��o de negados
  dplyr::mutate(num = 
                  num_court %>% 
                  forcats::fct_reorder(tipo_decisao,
                                       prop_negados) %>% 
                  forcats::fct_rev()) %>%
  dplyr::mutate(tipo_decisao=
                  forcats::fct_infreq(tipo_decisao) %>%
                  forcats::fct_rev()) %>%
  ## tabela --------------------------------------------------------------------
  group_by(num_court,tipo_decisao) %>% summarise(n=n()) %>%
  reshape2::dcast(.,num_court~tipo_decisao,value.var="n")

tabela_1[is.na(tabela_1)] <- 0
tabela_1$total <- tabela_1$extinto+tabela_1$parcial+tabela_1$outros+tabela_1$provido+tabela_1$negado

## exportar tabela ------------------------------------------------------------

write.csv(tabela_1,
          "F:/2018/Economia/5_Boletim ABRAMGE/13 Edi��o/0. Dados e Gr�ficos/Cap�tulo Especial - Judicializa��o/Apresenta��es/2� Apresenta��o/Gr�ficos/tabela_1.csv")


# separar processos conforme data de registro e plotar o gr�fico

grafico_4 <-
  apelados_apelantes_ident %>%
  ## transformacao -------------------------------------------------------------
  dplyr::filter(!is.na(num_court)) %>%
  ## calcular propor��o de negados
  dplyr::mutate(num = 
                  num_court %>% 
                  forcats::fct_reorder(tipo_decisao,
                                       prop_negados) %>% 
                  forcats::fct_rev()) %>%
  dplyr::mutate(tipo_decisao=
                  forcats::fct_infreq(tipo_decisao) %>%
                  forcats::fct_rev()) %>%
  group_by(num_court) %>% summarise(count=n()) %>%
  ## grafico --------------------------------------------------------------------
  ggplot(aes(x=reorder(num_court,-count),
           y=count)) +
  geom_col(fill="firebrick2",colour="white",
           size=1,alpha=.7,width=.7) +
  geom_hline(yintercept=c(1400,2800,4200,5600),linetype=2,
             size=.7,alpha=.4) +
  geom_text(aes(y=count,label=count),
            size=3,position=position_dodge(.5)) +
  scale_y_continuous(breaks=c(0,1400,2800,4200,5600)) +
  coord_flip() +
  ## edi��o visual ------------------------------------------------------------
  theme_minimal(13) +
  theme(text=element_text(family="Helvetica Neue")) +
  labs(x="C�mara",
       y='Quantidade de Apela��es')

grafico_4

## salvar gr�fico ------------------------------------------------------------

ggsave("G:/Economia/1_Base de Informa��es/7_ Dados Judiciais/Cap�tulo Especial - Judicializa��o/Apresenta��es/2� Apresenta��o/Gr�ficos/grafico_4.png",
       plot=grafico_4,
       width=24,height=16,
       units="cm",dpi=700)


#### 5. Gr�fico - Porcentagem de Decis�es em Apela��es ####

# separar processos conforme data de registro e plotar o gr�fico

grafico_5 <-
  apelados_apelantes_ident %>%
  ## transformacao -------------------------------------------------------------
  dplyr::filter(!is.na(num_court)) %>%
  filter(num_court=='07'|num_court=='05'|
           num_court=='03'|num_court=='09'|
           num_court=='08'|num_court=='02'|
           num_court=='06'|num_court=='10'|
           num_court=='01'|num_court=='04') %>%
  ## calcular propor��o de negados
  dplyr::mutate(num = 
                  num_court %>% 
                  forcats::fct_reorder(tipo_decisao,
                                       prop_negados) %>% 
                  forcats::fct_rev()) %>%
  dplyr::mutate(tipo_decisao=
                  forcats::fct_infreq(tipo_decisao) %>%
                  forcats::fct_rev()) %>%
  ## transformacao -------------------------------------------------------------
  dplyr::filter(!tipo_decisao=='extinto') %>%
  group_by(num_court,tipo_decisao) %>%
  summarise(count=n()) %>% mutate(freq=(count/sum(count))*100) %>%
  ## grafico --------------------------------------------------------------------
  ggplot(aes(x=num_court,y=freq)) +
  geom_col(aes(fill=tipo_decisao),
           colour='white',position='fill',
           size=.2,alpha=.8,width=.7) +
  geom_hline(yintercept=c(0,0.25,0.5,0.75,1),
             linetype=2, size=.7,alpha=.4) +
  geom_text(aes(x=num_court,y=freq,
                label=format(round(freq,1),nsmall=1),
                group=tipo_decisao),
            size=3,position=position_fill(0.5)) +
  scale_y_continuous(labels=scales::percent) + 
  scale_fill_manual(values=cptcity::cpt(pal="jjg_cbac_div_cbacSpectral05",n=4)) +
  coord_flip() +
  ## edi��o visual ------------------------------------------------------------
  theme_hc(13) +
  theme(legend.position="bottom",
        text=element_text(family="Helvetica Neue")) +
  labs(x='C�maras', 
       y='Propor��es de Processos por Tipos de Decis�es',
       fill='Decis�o')

grafico_5

## salvar gr�fico ------------------------------------------------------------

ggsave("G:/Economia/1_Base de Informa��es/7_ Dados Judiciais/Cap�tulo Especial - Judicializa��o/Apresenta��es/2� Apresenta��o/Gr�ficos/grafico_5_v2.png",
       plot=grafico_5,
       width=24,height=16,
       units="cm",dpi=700)


#### 6. Gr�fico - Porcentagens de Apelantes em 2� Inst�ncia ####

grafico_6 <-
  apelados_apelantes_ident %>%
  ## selecionar principais c�maras ----------------------------------------------
  dplyr::filter(!is.na(num_court)) %>%
  filter(num_court=='07'|num_court=='05'|
           num_court=='03'|num_court=='09'|
           num_court=='08'|num_court=='02'|
           num_court=='06'|num_court=='10'|
           num_court=='01'|num_court=='04') %>%
  ## transformacao 1 ------------------------------------------------------------
  group_by(date_registration_YEAR) %>% 
  summarise(beneficiarios_apelantes=sum(ident_company_apelante=='OUTROS'&
                                        !ident_company_apelado=='OUTROS'),
            operadoras_apelantes=sum(!ident_company_apelante=='OUTROS'&
                                     ident_company_apelado=='OUTROS')) %>%
  ## transformacao 2 ------------------------------------------------------------
  reshape2::melt(.,id.vars=c("date_registration_YEAR"),
               variable.name="apelante", 
               value.name="count") %>%
  ## grafico ------------------------------------------------------------------
  ggplot(aes(x=date_registration_YEAR,y=count)) +
  geom_col(aes(fill=apelante),
           alpha=.7,width=.7) +
  geom_text(aes(x=date_registration_YEAR,y=count,
                label=count,group=apelante),
            size=3,position=position_stack(vjust=.8)) +
  scale_x_continuous(breaks=c(2007,2008,2009,2010,
                              2011,2012,2013,2014,
                              2015,2016,2017)) +
  scale_y_continuous(breaks=c(0,2000,4000,6000,8000,10000)) +
  scale_fill_manual(labels=c("Pessoas F�sicas","Operadoras/Hospitais"),
                    values=c("cyan3","red3")) +
  ## edi��o visual ------------------------------------------------------------
  theme_hc(13) +
  theme(axis.title.x=element_blank(),
        text=element_text(family="Helvetica Neue")) +
  labs(y='Quantidade de Processos',
       fill='Apelantes em 2� Inst�ncia')

grafico_6

## salvar gr�fico ------------------------------------------------------------

ggsave("G:/Economia/1_Base de Informa��es/7_ Dados Judiciais/Cap�tulo Especial - Judicializa��o/Apresenta��es/2� Apresenta��o/Gr�ficos/grafico_6.png",
       plot=grafico_6,
       width=16,height=12,
       units="cm",dpi=700)


#### 7. Gr�fico - Desempenho das Operadoras Apelantes por C�mara ####

grafico_7 <-
  apelados_apelantes_ident %>%
  ## selecionar principais c�maras ----------------------------------------------
  dplyr::filter(!is.na(num_court)) %>%
  filter(num_court=='07'|num_court=='05'|
           num_court=='03'|num_court=='09'|
           num_court=='08'|num_court=='02'|
           num_court=='06'|num_court=='10'|
           num_court=='01'|num_court=='04') %>%
  ## calcular propor��o de negados
  dplyr::mutate(num = 
                  num_court %>% 
                  forcats::fct_reorder(tipo_decisao,
                                       prop_negados) %>% 
                  forcats::fct_rev()) %>%
  dplyr::mutate(tipo_decisao=
                  forcats::fct_infreq(tipo_decisao) %>%
                  forcats::fct_rev()) %>%
  ## transformacao -------------------------------------------------------------
  dplyr::filter(!ident_company_apelante=='OUTROS') %>% dplyr::filter(ident_company_apelado=='OUTROS') %>%
  dplyr::filter(tipo_decisao=='provido'|tipo_decisao=='negado') %>%
  group_by(num_court,tipo_decisao) %>% summarise(count=n()) %>%
  ## grafico -------------------------------------------------------------------
  ggplot(aes(x=reorder(num_court,-count),
           y=count,
           fill=tipo_decisao)) +
  geom_bar(colour='white',stat="identity",
           size=.2,alpha=.5,width=.7) +
  geom_hline(yintercept=c(0,700,1400,2100,2800,3500),
             linetype=2,size=.7,alpha=.4) +
  geom_text(aes(x=num_court,y=count,
                label=count,group=tipo_decisao),
            size=4,position=position_stack(vjust=.5)) +
  scale_y_continuous(breaks=c(0,700,1400,2100,2800,3500)) +
  jcolors::scale_fill_jcolors(palette="pal5") +
  coord_flip() +
  ## edi��o visual ------------------------------------------------------------
  theme_hc(13) +
  theme(text=element_text(family="Helvetica Neue")) +
  labs(x='C�maras', 
       y='Quantidade de Processos por Tipos de Decis�es',
       fill='Decis�o')

grafico_7

## salvar gr�fico ------------------------------------------------------------

ggsave("G:/Economia/1_Base de Informa��es/7_ Dados Judiciais/Cap�tulo Especial - Judicializa��o/Apresenta��es/2� Apresenta��o/Gr�ficos/grafico_7.png",
       plot=grafico_7,
       width=24,height=16,
       units="cm",dpi=700)


#### 8. Gr�fico - Desempenho dos Benefici�rios Apelantes por C�mara ####

grafico_8 <-
  apelados_apelantes_ident %>%
  ## selecionar principais c�maras ----------------------------------------------
  dplyr::filter(!is.na(num_court)) %>%
  filter(num_court=='07'|num_court=='05'|
           num_court=='03'|num_court=='09'|
           num_court=='08'|num_court=='02'|
           num_court=='06'|num_court=='10'|
           num_court=='01'|num_court=='04') %>%
  ## calcular propor��o de negados
  dplyr::mutate(num = 
                  num_court %>% 
                  forcats::fct_reorder(tipo_decisao,
                                       prop_negados) %>% 
                  forcats::fct_rev()) %>%
  dplyr::mutate(tipo_decisao=
                  forcats::fct_infreq(tipo_decisao) %>%
                  forcats::fct_rev()) %>%
  ## transformacao -------------------------------------------------------------
  dplyr::filter(ident_company_apelante=='OUTROS') %>% dplyr::filter(!ident_company_apelado=='OUTROS') %>%
  dplyr::filter(tipo_decisao=='provido'|tipo_decisao=='negado') %>%
  group_by(num_court,tipo_decisao) %>% summarise(count=n()) %>%
  ## grafico -------------------------------------------------------------------
  ggplot(aes(x=reorder(num_court,-count),
           y=count,
           fill=tipo_decisao)) +
  geom_bar(colour='white',stat="identity",
           size=.2,alpha=.5,width=.7) +
  geom_hline(yintercept=c(0,300,600,900,1200,1500),
             linetype=2,size=.7,alpha=.4) +
  geom_text(aes(x=num_court,y=count,
                label=count,group=tipo_decisao),
            size=4,position=position_stack(vjust=.5)) +
  scale_y_continuous(breaks=c(0,300,600,900,1200,1500)) +
  jcolors::scale_fill_jcolors(palette="pal5") +
  coord_flip() +
  ## edi��o visual ------------------------------------------------------------
  theme_hc(13) +
  theme(text=element_text(family="Helvetica Neue")) +
  labs(x='C�maras', 
       y='Quantidade de Processos por Tipos de Decis�es',
       fill='Decis�o')

grafico_8


## salvar gr�fico ------------------------------------------------------------

ggsave("G:/Economia/1_Base de Informa��es/7_ Dados Judiciais/Cap�tulo Especial - Judicializa��o/Apresenta��es/2� Apresenta��o/Gr�ficos/grafico_8.png",
       plot=grafico_8,
       width=24,height=16,
       units="cm",dpi=700)


#### 9. Gr�fico - Valores das A��es ####

# separar os valores das a��es

valor_acao <-
  d_cposg_2007_2017 %>% 
  dplyr::select(id,data) %>% 
  tidyr::unnest(data) %>% 
  dplyr::filter(data == 'Valor da a��o')

valor_acao$value <-
  as.numeric(gsub(",",".",
                  gsub("\\.","",valor_acao$value)))

decisoes_valor_acao <-
  d_cposg_2007_2017 %>% 
  dplyr::select(id, decisions) %>% 
  tidyr::unnest(decisions) %>% 
  dplyr::inner_join(valor_acao, "id") %>% 
  dplyr::arrange(desc(value)) %>%  
  dplyr::group_by(id) %>%
  dplyr::slice(1) %>% 
  dplyr::ungroup()%>%
  dplyr::inner_join(df_tjsp_TOTAL,"id") %>% 
  dplyr::mutate(tipo_decisao = tipos_decisao(decision)) %>% 
  dplyr::select(id,date_registration_M,value,tipo_decisao) %>%
  dplyr::distinct(id,date_registration_M,value,tipo_decisao)


grafico_9 <-
  apelados_apelantes_ident %>%
  ## selecionar principais c�maras ----------------------------------------------
  dplyr::filter(!is.na(num_court)) %>%
  filter(num_court=='07'|num_court=='05'|
           num_court=='03'|num_court=='09'|
           num_court=='08'|num_court=='02'|
           num_court=='06'|num_court=='10'|
           num_court=='01'|num_court=='04') %>%
  ## calcular propor��o de negados
  dplyr::mutate(num = 
                  num_court %>% 
                  forcats::fct_reorder(tipo_decisao,
                                       prop_negados) %>% 
                  forcats::fct_rev()) %>%
  dplyr::mutate(tipo_decisao=
                  forcats::fct_infreq(tipo_decisao) %>%
                  forcats::fct_rev()) %>%
  ## transformacao -------------------------------------------------------------
  dplyr::filter(!ident_company_apelante=='OUTROS') %>% dplyr::filter(ident_company_apelado=='OUTROS') %>%  
  dplyr::inner_join(decisoes_valor_acao,"id") %>%
  dplyr::group_by(id) %>% dplyr::slice(1) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::filter(tipo_decisao.x=='negado'|tipo_decisao.x=='provido') %>%
  group_by(num_court,tipo_decisao.x) %>%
  summarise(valor_acao_total=sum(value)/10^6) %>%
  ## grafico --------------------------------------------------------------------
  ggplot(aes(x=reorder(num_court,-valor_acao_total),
             y=valor_acao_total,fill=tipo_decisao.x)) +
  geom_col(colour='white',stat="identity",
           size=.2,alpha=.5,width=.7) +
  geom_hline(yintercept=c(0,30,60,90),
            linetype=2,size=.7,alpha=.4) +
  geom_text(aes(x=num_court,y=valor_acao_total,
                label=format(round(valor_acao_total,1),nsmall=1),
                group=tipo_decisao.x),
            size=4,position=position_stack(vjust=.5)) +
  scale_y_continuous(breaks=c(0,30,60,90)) +
  scale_fill_manual(values=c('lightblue2','olivedrab2')) +
  coord_flip() +
  ## edi��o visual ------------------------------------------------------------
  theme_hc(13) +
  theme(text=element_text(family="Helvetica Neue")) +
  labs(x='C�maras', 
     y='Valor da A��o Judicial (Em Milh�es R$)',
     fill='Decis�o')

grafico_9


## salvar gr�fico ------------------------------------------------------------

ggsave("G:/Economia/1_Base de Informa��es/7_ Dados Judiciais/Cap�tulo Especial - Judicializa��o/Apresenta��es/2� Apresenta��o/Gr�ficos/grafico_9.png",
       plot=grafico_9,
       width=24,height=16,
       units="cm",dpi=700)


#### 10. Gr�fico - Valores das A��es ####

grafico_10 <-
  apelados_apelantes_ident %>%
  ## selecionar principais c�maras ----------------------------------------------
  dplyr::filter(!is.na(num_court)) %>%
  filter(num_court=='07'|num_court=='05'|
           num_court=='03'|num_court=='09'|
           num_court=='08'|num_court=='02'|
           num_court=='06'|num_court=='10'|
           num_court=='01'|num_court=='04') %>%
  ## calcular propor��o de negados
  dplyr::mutate(num = 
                  num_court %>% 
                  forcats::fct_reorder(tipo_decisao,
                                       prop_negados) %>% 
                  forcats::fct_rev()) %>%
  dplyr::mutate(tipo_decisao=
                  forcats::fct_infreq(tipo_decisao) %>%
                  forcats::fct_rev()) %>%
  ## transformacao -------------------------------------------------------------
  dplyr::filter(ident_company_apelante=='OUTROS') %>% dplyr::filter(!ident_company_apelado=='OUTROS') %>%  
  dplyr::inner_join(decisoes_valor_acao,"id") %>%
  dplyr::group_by(id) %>% dplyr::slice(1) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::filter(tipo_decisao.x=='negado'|tipo_decisao.x=='provido') %>%
  group_by(num_court,tipo_decisao.x) %>%
  summarise(valor_acao_total=sum(value)/10^6) %>%
  ## grafico --------------------------------------------------------------------
  ggplot(aes(x=reorder(num_court,-valor_acao_total),
           y=valor_acao_total,fill=tipo_decisao.x)) +
  geom_col(colour='white',stat="identity",
           size=.2,alpha=.5,width=.7) +
  geom_hline(yintercept=c(0,40,80,120),
             linetype=2,size=.7,alpha=.4) +
  geom_text(aes(x=num_court,y=valor_acao_total,
                label=format(round(valor_acao_total,1),nsmall=1),
                group=tipo_decisao.x),
            size=4,position=position_stack(vjust=.5)) +
  scale_y_continuous(breaks=c(0,40,80,120)) +
  scale_fill_manual(values=c('lightblue2','olivedrab2')) +
  coord_flip() +
  ## edi��o visual ------------------------------------------------------------
  theme_hc(13) +
  theme(text=element_text(family="Helvetica Neue")) +
  labs(x='C�maras', 
       y='Valor da A��o Judicial (Em Milh�es R$)',
       fill='Decis�o')

grafico_10

## salvar gr�fico ------------------------------------------------------------

ggsave("G:/Economia/1_Base de Informa��es/7_ Dados Judiciais/Cap�tulo Especial - Judicializa��o/Apresenta��es/2� Apresenta��o/Gr�ficos/grafico_10.png",
       plot=grafico_10,
       width=24,height=16,
       units="cm",dpi=700)




#### 11. Gr�fico - Mapa Munic�pios ####

library(maps) #mapas simples, eixos, escala, cidades 
library(mapdata) #base de dados WorldHires e rios
library(rworldmap) #outra base de dados de mapas do mundo
library(maptools) #Ler ESRI shapefiles 
library(mapproj) #Proje��es e grids
library(ggmap) #Gmaps, OSM + mapas baseados em ggplot2
library(rgdal)
library(raster) # Para baixar o pol�gono
library(rvest) # Para importar a base de dados
library(stringr) # Para manipula��o dos dados
library(viridis) # Para selecionar uma bonita paleta de cores
library(tmap) # Para plotar o mapa
library(abjMaps)

prestadores_cnes <-
  read.csv("G:/Economia/1_Base de Informa��es/7_ Dados Judiciais/Prestadores_CNES.csv",
           header=TRUE,sep=";")

dados_socioeconomicos_municipios_sp <-
  read.csv("G:/Economia/1_Base de Informa��es/7_ Dados Judiciais/dados_socioeconomicos_municipios_sp.csv",
           header=TRUE,sep=";")

prestadores_cnes_SP <-
  prestadores_cnes %>%
  dplyr::filter(SG_UF=="SP")

dados_socioeconomicos_municipios_sp$city <- 
  str_to_lower(dados_socioeconomicos_municipios_sp$city)
dados_socioeconomicos_municipios_sp$district <- 
  str_to_lower(dados_socioeconomicos_municipios_sp$district)

litigation_district <-
  apelados_apelantes_ident %>% 
  dplyr::mutate(district=str_to_lower(district)) %>%
  dplyr::inner_join(dados_socioeconomicos_municipios_sp,
                    c("district","date_registration_YEAR")) %>%
  dplyr::mutate(comarca=str_to_lower(district),
                media_benef_comarca=as.numeric(as.character(media_benef_comarca)),
                idh_comarca=as.numeric(as.character(idh_comarca)),
                renda_pc=as.numeric(as.character(renda_pc_comarca)))


litigation_district$comarca <- iconv(litigation_district$comarca,
                                     from="UTF-8",to="ASCII//TRANSLIT")
  
# extrair mapa

d_sf$sf$comarca$comarca <- str_to_lower(str_replace_all(d_sf$sf$comarca$comarca,"\\.","\\ "))
litigation_district$comarca <- str_replace_all(litigation_district$comarca,"\\.","\\ ")

map_comarcas <- d_sf$sf$comarca

litigation_district %>%
  dplyr::filter(idh_comarca>0.8) %>%
  group_by(comarca,date_registration_YEAR) %>% summarise(count=n()) %>% 
  dplyr::inner_join(d_sf$sf$comarca,"comarca") %>%
  dplyr::filter(date_registration_YEAR=='2011'|
                date_registration_YEAR=='2013'|
                date_registration_YEAR=='2015'|
                date_registration_YEAR=='2017') %>%
  ggplot() +
  geom_sf(aes(geometry=geometry,
              fill=count),
          colour='cornsilk2') +
  scale_fill_gradientn(colors=cptcity::cpt(pal="cb_seq_Reds_09",n=100)) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  labs(title='Comarcas',
       fill='�ndice de Litig�ncia') +
  facet_wrap(~date_registration_YEAR)


pointsToLabel1 <-
  c("S�o Paulo","Campinas","Leme","Guarulhos","S�o Caetano do Sul",
    "Ribeir�o Preto","Araraquara","Buri","S�o Jos� do Rio Pardo","Salto")
pointsToLabel2 <-
  c("2013","2017")

litigation_district %>%
  dplyr::filter(date_registration_YEAR=='2013'|
                date_registration_YEAR=='2017') %>%
  ggplot(aes(x=renda_pc,y=count)) +
  geom_point(aes(fill=IDH),
             colour='cornsilk1',shape=21,size=2,stroke=1) +
  geom_smooth(mapping=aes(linetype="r2"),
              method="lm",se=FALSE,
              color="red",size=0.7) +
  ggrepel::geom_text_repel(aes(label=district),
                           data=filter(litigation_district,
                                       district %in% pointsToLabel1,
                                       date_registration_YEAR %in% pointsToLabel2),
                           color="black",size=2.5,force=5) +
  scale_fill_gradientn(colors=cptcity::cpt(pal="cb_seq_Blues_09",n=20)) +
  theme_minimal(12) + facet_wrap(~date_registration_YEAR)



#### 12. Gr�fico - S�rie Temporal Decis�es C�maras ####

grafico_12 <- 
  apelados_apelantes_ident %>%
  ## selecionar principais c�maras ----------------------------------------------
  dplyr::filter(!is.na(num_court)) %>%
  filter(num_court=='07'|num_court=='05'|
           num_court=='03'|num_court=='09'|
           num_court=='08'|num_court=='02'|
           num_court=='06'|num_court=='10'|
           num_court=='01'|num_court=='04') %>%
  filter(date_registration_YEAR>=2012) %>%
  ## calcular propor��o de negados
  dplyr::mutate(num = 
                  num_court %>% 
                  forcats::fct_reorder(tipo_decisao,
                                       prop_negados) %>% 
                  forcats::fct_rev()) %>%
  dplyr::mutate(tipo_decisao=
                  forcats::fct_infreq(tipo_decisao) %>%
                  forcats::fct_rev()) %>%
  ## transformacao -------------------------------------------------------------
  dplyr::filter(!ident_company_apelante=='OUTROS') %>% dplyr::filter(ident_company_apelado=='OUTROS') %>%
  dplyr::filter(tipo_decisao=='provido'|tipo_decisao=='negado') %>%
  group_by(num_court,date_registration_YEAR,tipo_decisao) %>% summarise(count=n()) %>%
  mutate(freq=(count/sum(count))*100) %>%
  ## grafico -------------------------------------------------------------------
  ggplot(aes(x=date_registration_YEAR,
           y=freq,colour=tipo_decisao)) +
  geom_line(size=.7) + geom_point(size=1.3,stroke=1) +
  geom_hline(yintercept=50,
             linetype=2,size=.7,alpha=.4) +
  geom_text(aes(x=date_registration_YEAR,
                y=freq,
                label=format(round(freq,1),nsmall=1),
                group=tipo_decisao),size=3,vjust=-0.4) +
  scale_colour_manual(values=c("green4","magenta4")) +
  ## edi��o visual ------------------------------------------------------------
  theme_minimal(11) +
  theme(text=element_text(family="Helvetica Neue")) +
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),
        legend.position="bottom") +
  labs(x='Anos', 
       y='Propor��es de Processos por Tipos de Decis�es',
       colour='Decis�o') +
  facet_wrap(~num_court,ncol=5,nrow=2)

grafico_12

## salvar gr�fico ------------------------------------------------------------

ggsave("G:/Economia/1_Base de Informa��es/7_ Dados Judiciais/Cap�tulo Especial - Judicializa��o/Apresenta��es/2� Apresenta��o/Gr�ficos/grafico_12.png",
       plot=grafico_12,
       width=24,height=14,
       units="cm",dpi=700)


#### 13. Gr�fico - S�rie Temporal Decis�es C�maras ####

grafico_13 <- 
  apelados_apelantes_ident %>%
  ## selecionar principais c�maras ----------------------------------------------
  dplyr::filter(!is.na(num_court)) %>%
  filter(num_court=='07'|num_court=='05'|
           num_court=='03'|num_court=='09'|
           num_court=='08'|num_court=='02'|
           num_court=='06'|num_court=='10'|
           num_court=='01'|num_court=='04') %>%
  filter(date_registration_YEAR>=2012) %>%
  ## calcular propor��o de negados
  dplyr::mutate(num = 
                  num_court %>% 
                  forcats::fct_reorder(tipo_decisao,
                                       prop_negados) %>% 
                  forcats::fct_rev()) %>%
  dplyr::mutate(tipo_decisao=
                  forcats::fct_infreq(tipo_decisao) %>%
                  forcats::fct_rev()) %>%
  ## transformacao -------------------------------------------------------------
  dplyr::filter(ident_company_apelante=='OUTROS') %>% dplyr::filter(!ident_company_apelado=='OUTROS') %>%
  dplyr::filter(tipo_decisao=='provido'|tipo_decisao=='negado') %>%
  group_by(num_court,date_registration_YEAR,tipo_decisao) %>% summarise(count=n()) %>%
  mutate(freq=(count/sum(count))*100) %>%
  ## grafico -------------------------------------------------------------------
  ggplot(aes(x=date_registration_YEAR,
           y=freq,colour=tipo_decisao)) +
  geom_line(size=.7) + geom_point(size=1.3,stroke=1) +
  geom_hline(yintercept=50,
             linetype=2,size=.7,alpha=.4) +
  geom_text(aes(x=date_registration_YEAR,
                y=freq,
                label=format(round(freq,1),nsmall=1),
                group=tipo_decisao),size=3,vjust=-0.4) +
  scale_colour_manual(values=c("green4","magenta4")) +
  ## edi��o visual ------------------------------------------------------------
  theme_minimal(11) +
  theme(text=element_text(family="Helvetica Neue")) +
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),
        legend.position="bottom") +
  labs(x='Anos', 
       y='Propor��es de Processos por Tipos de Decis�es',
       colour='Decis�o') +
  facet_wrap(~num_court,ncol=5,nrow=2)

grafico_13

## salvar gr�fico ------------------------------------------------------------

ggsave("G:/Economia/1_Base de Informa��es/7_ Dados Judiciais/Cap�tulo Especial - Judicializa��o/Apresenta��es/2� Apresenta��o/Gr�ficos/grafico_13.png",
       plot=grafico_13,
       width=24,height=14,
       units="cm",dpi=700)
