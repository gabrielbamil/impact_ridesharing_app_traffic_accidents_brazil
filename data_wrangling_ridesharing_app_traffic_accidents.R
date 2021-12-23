
###### Monografia 2
###### Aluno: Gabriel Amil Bastos
###### UFES 2020
###### Script de Leitura e Tratamento dos dados

#### Pacotes
{
install.packages("rvest")
install.packages("tidyr")
install.packages("tibble")
install.packages("openxlsx")
install.packages("dplyr")
install.packages("stringr")
install.packages("abjutils")
install.packages("reshape2")
install.packages("tictoc")

library(rvest)
library(tidyr)
library(tibble)
library(openxlsx)
library(dplyr)
library(stringr)
library(abjutils)
library(reshape2)
library(tictoc)
  
}


###### Entrada Uber e códigos de municípios - Pesquisa Manual 
{
setwd("C:/Users/gabri/Google Drive/Arquivos Mono/Dados/")
Munic_uber <- read.xlsx("Municípios Uber.xlsx", sheet = "dados")

# Join de dados de população para URL
Munic_pop_filtro <- read.xlsx("População/tabela6579.xlsx",startRow = 4)
Munic_pop_filtro <- Munic_pop_filtro[,c(2,6)]
Munic_pop_filtro <- Munic_pop_filtro %>% 
                    rename(cod_munic = Cód.)
Munic_uber <- left_join(Munic_uber,Munic_pop_filtro, by=c('cod_munic'))

## preparando chave de ligação entre Munic_uber e Dados de Frota
Munic_uber$munic_merge <- rm_accent(str_to_lower(paste(gsub(" ","",Munic_uber$munic),Munic_uber$cod_UF,sep = "_")))
Munic_uber$munic_merge <- ifelse(Munic_uber$munic_merge=="embudasartes_sp", "embu_sp", Munic_uber$munic_merge)
}
###### Frota Municipal - CONATRAN
{
#set the working directory from which the files will be read from
setwd("C:/Users/gabri/Google Drive/Arquivos Mono/Dados/Frota Municípios Resumido/")

#create a list of the files from your target directory
file_list <- list.files(path="C:/Users/gabri/Google Drive/Arquivos Mono/Dados/Frota Municípios Resumido/")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable

frota_munic<- list()
#had to specify columns to get rid of the total column
for (i in 1:length(file_list)){
  frota_munic[[i]] <- read.xlsx(file_list[i])
  frota_munic[[i]] <- subset(frota_munic[[i]], !(is.na(frota_munic[[i]]$X2)))
  frota_munic[[i]] <- frota_munic[[i]] %>%
    setNames(frota_munic[[i]][1,]) %>%
    slice(-1)
  frota_munic[[i]]$Ano <- paste("20",str_sub(gsub(".xlsx", "", file_list[i]),-2),sep = "") #clean the data as needed, in this case I am creating a new column that indicates which file each row of data came from
  frota_munic[[i]]$munic_merge <- rm_accent(str_to_lower(paste(gsub(" ", "", frota_munic[[i]]$MUNICIPIO),frota_munic[[i]]$UF,sep = "_")))
  frota_munic[[i]] <- inner_join(frota_munic[[i]], Munic_uber, by=c("munic_merge"))
}
dfvf_munic_frota <- do.call("rbind",frota_munic)
dfvf_munic_frota$cod_munic <- as.character(dfvf_munic_frota$cod_munic)
dfvf_munic_frota$munic_ibge_6 <- as.character(dfvf_munic_frota$munic_ibge_6)
}
###### PIB Municipal e Valor Adicionado Bruto - PIB Munic IBGE
{
setwd("C:/Users/gabri/Google Drive/Arquivos Mono/Dados/")
Munic_pib_pc <- read.csv("PIB_munic/tabela5938.csv",sep = ";",header = FALSE,encoding = "UTF-8")
Munic_pib_pc <- Munic_pib_pc[-c(1,2,50134:50146),-2]

Munic_pib_pc <- Munic_pib_pc %>% 
  setNames(c("cod_munic","Ano","percet_vl_brut_agropec","percet_vl_brut_ind","percet_vl_brut_serv","percet_vl_brut_admpublic","pib_preco_corrent")) %>%
  slice(-1) %>%
  mutate(pib_preco_corrent = as.character(pib_preco_corrent),
         cod_munic = as.character(cod_munic),
         Ano = as.character(Ano),
         percet_vl_brut_agropec = as.character(percet_vl_brut_agropec),
         percet_vl_brut_ind = as.character(percet_vl_brut_ind),
         percet_vl_brut_serv = as.character(percet_vl_brut_serv),
         percet_vl_brut_admpublic = as.character(percet_vl_brut_admpublic))
}
###### População Extração Final - Estimapop
{
Munic_pop <- read.csv("População/tabela6579_reshape.csv",sep = ";",header = FALSE,encoding = "UTF-8")
Munic_pop <- Munic_pop[-c(1,2,3,55704:55784),-2]
Munic_pop <- Munic_pop %>% 
  setNames(c("cod_munic","Ano","pop"))%>%
  mutate(pop = as.character(pop),
         cod_munic = as.character(cod_munic),
         Ano = as.character(Ano))

Munic_pop_censo <- read.xlsx("População/tabela_censo_reshape.xlsx", startRow = 4)
Munic_pop_censo <- Munic_pop_censo[-c(1,2,3,5569),-2]
Munic_pop_censo <- Munic_pop_censo %>% 
  setNames(c("cod_munic","Ano","pop")) %>%
  mutate(pop = as.character(pop),
         cod_munic = as.character(cod_munic),
         Ano = as.character(Ano))


Munic_pop <- Munic_pop %>% bind_rows(Munic_pop_censo)
}
###### Índices de Preços - INPC/IPCA
{
igpm <- read.csv("Índice de Preços/IGPM.csv",sep = ";",header = FALSE,encoding = "UTF-8")
igpm <- igpm[-1,-3]
igpm <- igpm %>%
          setNames(c("Ano","igpm")) %>%
          mutate(Ano = as.character(Ano),
                 igpm = as.character(igpm))

igpm$mes <-  substr(igpm$Ano,6,7)
igpm$Ano <-  substr(igpm$Ano,1,4)
igpm <- subset(igpm,mes=="12" & Ano!="2019" & Ano!="2018")
igpm$mes <- NULL

ipca <- read.csv("Índice de Preços/IPCA.csv",sep = ";",header = FALSE,encoding = "UTF-8")
ipca <- ipca[-1,-3]    
ipca <- ipca %>%
  setNames(c("Ano","ipca")) %>%
  mutate(Ano = as.character(Ano),
         ipca = as.character(ipca))

ipca$mes <-  substr(ipca$Ano,6,7)
ipca$Ano <-  substr(ipca$Ano,1,4)
ipca <- subset(ipca,mes=="12" & Ano!="2019" & Ano!="2018")
ipca$mes <- NULL


}
###### Salário Médio - RAIS
{
rais_sm <- read.csv("Salário Médio RAIS/consulta29228348.csv",sep = ";",header = FALSE,encoding = "UTF-7")
rais_sm <- rais_sm[-c(1,2,5573:5584),-11]
rais_sm <- rais_sm %>%
  setNames(c("munic_ibge_6","2017","2016","2015","2014","2013","2012","2011","2010","2009"))

rais_sm <- melt(data = rais_sm, id = c("munic_ibge_6"))
rais_sm <- rais_sm %>%
  setNames(c("munic_ibge_6","Ano","salario_medio")) %>%
  mutate(Ano = as.character(Ano),
         munic_ibge_6 = as.character(munic_ibge_6),
         salario_medio = as.character(salario_medio))

}
###### Número de Leitos Hospitalares
{
  setwd("C:/Users/gabri/Google Drive/Arquivos Mono/Dados/Leitos/")
  
  file_list_leitos <- list.files(path="C:/Users/gabri/Google Drive/Arquivos Mono/Dados/Leitos/")
  leitos_datasus<- list()
  
  for (i in 1:length(file_list_leitos)){
    leitos_datasus[[i]] <- read.csv(file_list_leitos[i],sep = ";",header = FALSE,na.strings = TRUE,skip = 3)
    leitos_datasus[[i]] <- subset(leitos_datasus[[i]], leitos_datasus[[i]]$V2!="")
    names(leitos_datasus[[i]]) <- unlist(leitos_datasus[[i]][1,])
    leitos_datasus[[i]] <- leitos_datasus[[i]][-1,]
    
    leitos_datasus[[i]] <- melt(leitos_datasus[[i]], id = c("Município"))
    leitos_datasus[[i]] <- leitos_datasus[[i]] %>%
      setNames(c("munic_ibge_6","Mês",gsub(".csv","",file_list_leitos[i])))
    
    leitos_datasus[[i]]$munic_ibge_6 <- substr(leitos_datasus[[i]]$munic_ibge_6,1,6)
    leitos_datasus[[i]]$Ano <- substr(leitos_datasus[[i]]$Mês,1,4)
    leitos_datasus[[i]]$Mês <- substr(leitos_datasus[[i]]$Mês,6,8)
    
    #leitos_datasus[[i]] <- subset(leitos_datasus[[i]],leitos_datasus[[i]]$Mês=="Dez")
    
    leitos_datasus[[i]][,3] <- ifelse(leitos_datasus[[i]][,3]=="-",0,leitos_datasus[[i]][,3])
    
    leitos_datasus[[i]] <- leitos_datasus[[i]] %>%
      left_join(Munic_uber[c("munic_ibge_6","munic")],by = c("munic_ibge_6"))
    
    leitos_datasus[[i]]$munic_ibge_6 <- as.character(leitos_datasus[[i]]$munic_ibge_6)
    leitos_datasus[[i]]$Ano <- as.character(leitos_datasus[[i]]$Ano)
    #leitos_datasus[[i]]$Mês <- NULL
    leitos_datasus[[i]] <- subset(leitos_datasus[[i]],!(is.na(leitos_datasus[[i]]$munic)))
    leitos_datasus[[i]]$munic <- NULL
    vargroup_leitos <- c(gsub(".csv","",file_list_leitos))
  }
  
  leitos_datasus_ <- leitos_datasus %>%
    bind_rows()
  
  for (i in 1:length(vargroup_leitos)){
    var <- vargroup_leitos[i]
    leitos_datasus_[,c(var)] <- as.numeric(sub(",",".",leitos_datasus_[,c(var)], fixed = TRUE))
  }
  
  leitos_datasus_[is.na(leitos_datasus_)] = 0
  
  leitos_datasus_$total_leitos <- leitos_datasus_[,c(vargroup_leitos[1])]+leitos_datasus_[,c(vargroup_leitos[2])]+leitos_datasus_[,c(vargroup_leitos[3])]+leitos_datasus_[,c(vargroup_leitos[4])]+leitos_datasus_[,c(vargroup_leitos[5])]+leitos_datasus_[,c(vargroup_leitos[6])]+leitos_datasus_[,c(vargroup_leitos[7])]+leitos_datasus_[,c(vargroup_leitos[8])]+leitos_datasus_[,c(vargroup_leitos[9])]+leitos_datasus_[,c(vargroup_leitos[10])]
  vargroup_leitos <- c("total_leitos",vargroup_leitos)
  
  leitos_datasus_ <- leitos_datasus_ %>%
    group_by(Ano,munic_ibge_6,Mês) %>%
    summarise_at(vargroup_leitos, sum, na.rm=TRUE) 
  
  leitos_datasus_ <- leitos_datasus_ %>%
    group_by(Ano,munic_ibge_6) %>%
    summarise_at(vargroup_leitos, mean, na.rm=TRUE)
  
  
}
###### Médicos
{
  setwd("C:/Users/gabri/Google Drive/Arquivos Mono/Dados/Médicos/")
  
  file_list_medicos <- list.files(path="C:/Users/gabri/Google Drive/Arquivos Mono/Dados/Médicos/")
  medicos_datasus<- list()
  
  for (i in 1:length(file_list_medicos)){
    medicos_datasus[[i]] <- read.csv(file_list_medicos[i],sep = ";",header = FALSE,na.strings = TRUE,skip = 3)
    medicos_datasus[[i]] <- subset(medicos_datasus[[i]], medicos_datasus[[i]]$V2!="")
    names(medicos_datasus[[i]]) <- unlist(medicos_datasus[[i]][1,])
    medicos_datasus[[i]] <- medicos_datasus[[i]][-1,]
    
    medicos_datasus[[i]] <- melt(medicos_datasus[[i]], id = c("Município"))
    medicos_datasus[[i]] <- medicos_datasus[[i]] %>%
      setNames(c("munic_ibge_6","Mês",gsub(".csv","",file_list_medicos[i])))
    
    medicos_datasus[[i]]$munic_ibge_6 <- substr(medicos_datasus[[i]]$munic_ibge_6,1,6)
    medicos_datasus[[i]]$Ano <- substr(medicos_datasus[[i]]$Mês,1,4)
    medicos_datasus[[i]]$Mês <- substr(medicos_datasus[[i]]$Mês,6,8)
    
    #medicos_datasus[[i]] <- subset(medicos_datasus[[i]],medicos_datasus[[i]]$Mês=="Dez")
    
    medicos_datasus[[i]][,3] <- ifelse(medicos_datasus[[i]][,3]=="-",0,medicos_datasus[[i]][,3])
    
    medicos_datasus[[i]] <- medicos_datasus[[i]] %>%
      left_join(Munic_uber[c("munic_ibge_6","munic")],by = c("munic_ibge_6"))
    
    medicos_datasus[[i]]$munic_ibge_6 <- as.character(medicos_datasus[[i]]$munic_ibge_6)
    medicos_datasus[[i]]$Ano <- as.character(medicos_datasus[[i]]$Ano)
    medicos_datasus[[i]]$Mês <- NULL
    medicos_datasus[[i]] <- subset(medicos_datasus[[i]],!(is.na(medicos_datasus[[i]]$munic)))
    medicos_datasus[[i]]$munic <- NULL
    vargroup_medicos <- c(gsub(".csv","",file_list_medicos))
    
  }
  
  medicos_datasus_ <- medicos_datasus %>%
    bind_rows()
  
  for (i in 1:length(vargroup_medicos)){
    var <- vargroup_medicos[i]
    medicos_datasus_[,c(var)] <- as.numeric(sub(",",".",medicos_datasus_[,c(var)], fixed = TRUE))
  }
  
  medicos_datasus_ <- medicos_datasus_ %>%
    group_by(Ano,munic_ibge_6) %>%
    summarise_at(vargroup_medicos, mean, na.rm=TRUE)
  
  
}


##### Dados Mortalidade DATASUS Residência
{
setwd("C:/Users/gabri/Google Drive/Arquivos Mono/Dados/Mortalidade DATASUS/")
  
file_list_mort <- list.files(path="C:/Users/gabri/Google Drive/Arquivos Mono/Dados/Mortalidade DATASUS/")
mort_datasus<- list()
  
for (i in 1:length(file_list_mort)){
  mort_datasus[[i]] <- read.csv(file_list_mort[i],sep = ";",header = FALSE,na.strings = TRUE,skip = 3)
  mort_datasus[[i]] <- subset(mort_datasus[[i]], mort_datasus[[i]]$V2!="")
  mort_datasus[[i]] <- mort_datasus[[i]][-c(5598,5599),-c(12)]
  mort_datasus[[i]] <- mort_datasus[[i]] %>%
  setNames(c("munic_ibge_6","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018")) %>%
    slice(-1)
  mort_datasus[[i]] <- melt(mort_datasus[[i]], id = c("munic_ibge_6"))
  mort_datasus[[i]] <- mort_datasus[[i]] %>%
    setNames(c("munic_ibge_6","Ano",gsub(".csv","",file_list_mort[i])))
  
  mort_datasus[[i]]$munic_ibge_6 <- substr(mort_datasus[[i]]$munic_ibge_6,1,6)
  
  mort_datasus[[i]][,3] <- ifelse(mort_datasus[[i]][,3]=="-",0,mort_datasus[[i]][,3])
}
mort_datasus_ <- do.call("cbind",mort_datasus)
mort_datasus_ <- mort_datasus_[,-c(4,5,7,8,10,11,13,14,16,17,19,20)]
mort_datasus_ <- mort_datasus_ %>%
  setNames(c("munic_ibge_6","Ano","res_mort_transp_20a29","res_mort_transp_30a39","res_mort_transp_40mais","res_mort_transp_fem","res_mort_transp_masc","res_mort_transp_menor20","res_mort_transp_total")) %>%
  mutate(Ano = as.character(Ano),
         munic_ibge_6 = as.character(munic_ibge_6))

}
##### Dados Mortalidade DATASUS Ocorrência
{
  setwd("C:/Users/gabri/Google Drive/Arquivos Mono/Dados/Mortalidade DATASUS Ocorrência/")
  
  file_list_mort_ocor <- list.files(path="C:/Users/gabri/Google Drive/Arquivos Mono/Dados/Mortalidade DATASUS Ocorrência/")
  mort_datasus_ocor<- list()
  
  for (i in 1:length(file_list_mort_ocor)){
    mort_datasus_ocor[[i]] <- read.csv(file_list_mort_ocor[i],sep = ";",header = FALSE,na.strings = TRUE,skip = 3)
    mort_datasus_ocor[[i]] <- subset(mort_datasus_ocor[[i]], mort_datasus_ocor[[i]]$V2!="")
    mort_datasus_ocor[[i]] <- mort_datasus_ocor[[i]][-c(5598,5599),-c(12)]
    mort_datasus_ocor[[i]] <- mort_datasus_ocor[[i]] %>%
      setNames(c("munic_ibge_6","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018")) %>%
      slice(-1)
    mort_datasus_ocor[[i]] <- melt(mort_datasus_ocor[[i]], id = c("munic_ibge_6"))
    mort_datasus_ocor[[i]] <- mort_datasus_ocor[[i]] %>%
      setNames(c("munic_ibge_6","Ano",gsub(".csv","",file_list_mort_ocor[i])))
    
    mort_datasus_ocor[[i]]$munic_ibge_6 <- substr(mort_datasus_ocor[[i]]$munic_ibge_6,1,6)
    
    mort_datasus_ocor[[i]][,3] <- ifelse(mort_datasus_ocor[[i]][,3]=="-",0,mort_datasus_ocor[[i]][,3])
    mort_datasus_ocor[[i]] <- mort_datasus_ocor[[i]] %>%
      left_join(Munic_uber[c("munic_ibge_6","munic")],by = c("munic_ibge_6"))
    
    mort_datasus_ocor[[i]] <- subset(mort_datasus_ocor[[i]],!(is.na(mort_datasus_ocor[[i]]$munic)))
  }
  mort_datasus_ocor_ <- do.call("cbind",mort_datasus_ocor)
  mort_datasus_ocor_ <- mort_datasus_ocor_[c("munic_ibge_6","Ano","mort_transp_20a29","mort_transp_30a39","mort_transp_40mais","mort_transp_fem","mort_transp_masc","mort_transp_menor20","mort_transp_total")]
  mort_datasus_ocor_ <- mort_datasus_ocor_ %>%
    setNames(c("munic_ibge_6","Ano","ocor_mort_transp_20a29","ocor_mort_transp_30a39","ocor_mort_transp_40mais","ocor_mort_transp_fem","ocor_mort_transp_masc","ocor_mort_transp_menor20","ocor_mort_transp_total")) %>%
    mutate(Ano = as.character(Ano),
           munic_ibge_6 = as.character(munic_ibge_6))
  
}


##### Dados Morbidade DATASUS Residência - Internações
{
  setwd("C:/Users/gabri/Google Drive/Arquivos Mono/Dados/Morbidade Internações Residência/")
  
  file_list_morb_res <- list.files(path="C:/Users/gabri/Google Drive/Arquivos Mono/Dados/Morbidade Internações Residência/")
  morb_datasus_res<- list()
  
  for (i in 1:length(file_list_morb_res)){
    morb_datasus_res[[i]] <- read.csv(file_list_morb_res[i],sep = ";",header = FALSE,na.strings = TRUE,skip = 3)
    morb_datasus_res[[i]] <- subset(morb_datasus_res[[i]], morb_datasus_res[[i]]$V2!="")
    morb_datasus_res[[i]] <- morb_datasus_res[[i]][-c(5598,5599),-c(2,12)]
    morb_datasus_res[[i]] <- morb_datasus_res[[i]] %>%
      setNames(c("munic_ibge_6","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018")) %>%
      slice(-1)
    morb_datasus_res[[i]] <- melt(morb_datasus_res[[i]], id = c("munic_ibge_6"))
    morb_datasus_res[[i]] <- morb_datasus_res[[i]] %>%
      setNames(c("munic_ibge_6","Ano",gsub(".csv","",file_list_morb_res[i])))
    
    morb_datasus_res[[i]]$munic_ibge_6 <- substr(morb_datasus_res[[i]]$munic_ibge_6,1,6)
    
    morb_datasus_res[[i]][,3] <- ifelse(morb_datasus_res[[i]][,3]=="-",0,morb_datasus_res[[i]][,3])
    morb_datasus_res[[i]] <- morb_datasus_res[[i]] %>%
      left_join(Munic_uber[c("munic_ibge_6","munic")],by = c("munic_ibge_6"))
    
    morb_datasus_res[[i]] <- subset(morb_datasus_res[[i]],!(is.na(morb_datasus_res[[i]]$munic)))
  }
  morb_datasus_res_ <- do.call("cbind",morb_datasus_res)
  morb_datasus_res_ <- morb_datasus_res_[c("munic_ibge_6","Ano","res_internacoes_20a29","res_internacoes_30a39","res_internacoes_40mais","res_internacoes_fem","res_internacoes_masc","res_internacoes_menor20","res_internacoes_total")]
  morb_datasus_res_ <- morb_datasus_res_ %>%
    setNames(c("munic_ibge_6","Ano","res_internacoes_transp_20a29","res_internacoes_transp_30a39","res_internacoes_transp_40mais","res_internacoes_transp_fem","res_internacoes_transp_masc","res_internacoes_transp_menor20","res_internacoes_transp_total")) %>%
    mutate(Ano = as.character(Ano),
           munic_ibge_6 = as.character(munic_ibge_6))
  
}
##### Dados Morbidade DATASUS Ocorrência - Internações
{
  setwd("C:/Users/gabri/Google Drive/Arquivos Mono/Dados/Morbidade Internações Ocorrência/")
  
  file_list_morb_ocor <- list.files(path="C:/Users/gabri/Google Drive/Arquivos Mono/Dados/Morbidade Internações Ocorrência/")
  morb_datasus_ocor<- list()
  
  for (i in 1:length(file_list_morb_ocor)){
    morb_datasus_ocor[[i]] <- read.csv(file_list_morb_ocor[i],sep = ";",header = FALSE,na.strings = TRUE,skip = 3)
    morb_datasus_ocor[[i]] <- subset(morb_datasus_ocor[[i]], morb_datasus_ocor[[i]]$V2!="")
    morb_datasus_ocor[[i]] <- morb_datasus_ocor[[i]][-c(5598,5599),-c(2,12)]
    morb_datasus_ocor[[i]] <- morb_datasus_ocor[[i]] %>%
      setNames(c("munic_ibge_6","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018")) %>%
      slice(-1)
    morb_datasus_ocor[[i]] <- melt(morb_datasus_ocor[[i]], id = c("munic_ibge_6"))
    morb_datasus_ocor[[i]] <- morb_datasus_ocor[[i]] %>%
      setNames(c("munic_ibge_6","Ano",gsub(".csv","",file_list_morb_ocor[i])))
    
    morb_datasus_ocor[[i]]$munic_ibge_6 <- substr(morb_datasus_ocor[[i]]$munic_ibge_6,1,6)
    
    morb_datasus_ocor[[i]][,3] <- ifelse(morb_datasus_ocor[[i]][,3]=="-",0,morb_datasus_ocor[[i]][,3])
    morb_datasus_ocor[[i]] <- morb_datasus_ocor[[i]] %>%
      left_join(Munic_uber[c("munic_ibge_6","munic")],by = c("munic_ibge_6"))
    
    morb_datasus_ocor[[i]] <- subset(morb_datasus_ocor[[i]],!(is.na(morb_datasus_ocor[[i]]$munic)))
    
    morb_datasus_ocor[[i]]$munic <- as.character(morb_datasus_ocor[[i]]$munic)
    morb_datasus_ocor[[i]]$munic_ibge_6 <- as.character(morb_datasus_ocor[[i]]$munic_ibge_6)
    morb_datasus_ocor[[i]]$Ano <- as.character(morb_datasus_ocor[[i]]$Ano)
  }
  
  morb_datasus_ocor_ <- morb_datasus_ocor %>%
    bind_rows() %>%
    mutate(
      ocorr_internacoes_20a29 = as.numeric(sub(",",".",ocorr_internacoes_20a29, fixed = TRUE)),
      ocorr_internacoes_30a39 = as.numeric(sub(",",".",ocorr_internacoes_30a39, fixed = TRUE)),
      ocorr_internacoes_40mais = as.numeric(sub(",",".",ocorr_internacoes_40mais, fixed = TRUE)),
      ocorr_internacoes_fem = as.numeric(sub(",",".",ocorr_internacoes_fem, fixed = TRUE)),
      ocorr_internacoes_masc = as.numeric(sub(",",".",ocorr_internacoes_masc, fixed = TRUE)),
      ocorr_internacoes_menor20 = as.numeric(sub(",",".",ocorr_internacoes_menor20, fixed = TRUE)),
      ocorr_internacoes_total = as.numeric(sub(",",".",ocorr_internacoes_total, fixed = TRUE)))
  
  vargroup <- c("ocorr_internacoes_20a29","ocorr_internacoes_30a39","ocorr_internacoes_40mais","ocorr_internacoes_fem","ocorr_internacoes_masc","ocorr_internacoes_menor20","ocorr_internacoes_total")
  
  morb_datasus_ocor_ <- morb_datasus_ocor_ %>%
    group_by(Ano,munic,munic_ibge_6) %>%
    summarise_at(vargroup, sum, na.rm=TRUE)
  
  morb_datasus_ocor_ <- morb_datasus_ocor_[c("munic_ibge_6","Ano","ocorr_internacoes_20a29","ocorr_internacoes_30a39","ocorr_internacoes_40mais","ocorr_internacoes_fem","ocorr_internacoes_masc","ocorr_internacoes_menor20","ocorr_internacoes_total")]
  morb_datasus_ocor_ <- morb_datasus_ocor_ %>%
    setNames(c("munic_ibge_6","Ano","ocor_internacoes_transp_20a29","ocor_internacoes_transp_30a39","ocor_internacoes_transp_40mais","ocor_internacoes_transp_fem","ocor_internacoes_transp_masc","ocor_internacoes_transp_menor20","ocor_internacoes_transp_total")) %>%
    mutate(Ano = as.character(Ano),
           munic_ibge_6 = as.character(munic_ibge_6))
  
}

##### Dados Morbidade DATASUS Residência - Valor Internações Total
{
  setwd("C:/Users/gabri/Google Drive/Arquivos Mono/Dados/Valor Total Gastos Morbidade - Residência/")
  
  file_list_vlmorb_res <- list.files(path="C:/Users/gabri/Google Drive/Arquivos Mono/Dados/Valor Total Gastos Morbidade - Residência/")
  vlmorb_datasus_res<- list()
  
  for (i in 1:length(file_list_vlmorb_res)){
    vlmorb_datasus_res[[i]] <- read.csv(file_list_vlmorb_res[i],sep = ";",header = FALSE,na.strings = TRUE,skip = 3)
    vlmorb_datasus_res[[i]] <- subset(vlmorb_datasus_res[[i]], vlmorb_datasus_res[[i]]$V2!="")
    vlmorb_datasus_res[[i]] <- vlmorb_datasus_res[[i]][-c(5598,5599),-c(2,12)]
    vlmorb_datasus_res[[i]] <- vlmorb_datasus_res[[i]] %>%
      setNames(c("munic_ibge_6","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018")) %>%
      slice(-1)
    vlmorb_datasus_res[[i]] <- melt(vlmorb_datasus_res[[i]], id = c("munic_ibge_6"))
    vlmorb_datasus_res[[i]] <- vlmorb_datasus_res[[i]] %>%
      setNames(c("munic_ibge_6","Ano",gsub(".csv","",file_list_vlmorb_res[i])))
    
    vlmorb_datasus_res[[i]]$munic_ibge_6 <- substr(vlmorb_datasus_res[[i]]$munic_ibge_6,1,6)
    
    vlmorb_datasus_res[[i]][,3] <- ifelse(vlmorb_datasus_res[[i]][,3]=="-",0,vlmorb_datasus_res[[i]][,3])
    vlmorb_datasus_res[[i]] <- vlmorb_datasus_res[[i]] %>%
      left_join(Munic_uber[c("munic_ibge_6","munic")],by = c("munic_ibge_6"))
    
    vlmorb_datasus_res[[i]] <- subset(vlmorb_datasus_res[[i]],!(is.na(vlmorb_datasus_res[[i]]$munic)))
  }
  vlmorb_datasus_res_ <- do.call("cbind",vlmorb_datasus_res)
  vlmorb_datasus_res_ <- vlmorb_datasus_res_[c("munic_ibge_6","Ano","resid_valor_20a29","resid_valor_30a39","resid_valor_40mais","resid_valor_fem","resid_valor_masc","resid_valor_menor20","resid_valor_total")]
  vlmorb_datasus_res_ <- vlmorb_datasus_res_ %>%
    setNames(c("munic_ibge_6","Ano","res_valor_20a29","res_valor_30a39","res_valor_40mais","res_valor_fem","res_valor_masc","res_valor_menor20","res_valor_total")) %>%
    mutate(Ano = as.character(Ano),
           munic_ibge_6 = as.character(munic_ibge_6))
  
}
##### Dados Morbidade DATASUS Ocorrência - Valor Internações Total
{
  setwd("C:/Users/gabri/Google Drive/Arquivos Mono/Dados/Valor Total Gastos Morbidade - Ocorrência/")
  
  file_list_vlmorb_ocor <- list.files(path="C:/Users/gabri/Google Drive/Arquivos Mono/Dados/Valor Total Gastos Morbidade - Ocorrência/")
  vlmorb_datasus_ocor<- list()
  
  for (i in 1:length(file_list_vlmorb_ocor)){
    vlmorb_datasus_ocor[[i]] <- read.csv(file_list_vlmorb_ocor[i],sep = ";",header = FALSE,na.strings = TRUE,skip = 3)
    vlmorb_datasus_ocor[[i]] <- subset(vlmorb_datasus_ocor[[i]], vlmorb_datasus_ocor[[i]]$V2!="")
    vlmorb_datasus_ocor[[i]] <- vlmorb_datasus_ocor[[i]][-c(5598,5599),-c(2,12)]
    vlmorb_datasus_ocor[[i]] <- vlmorb_datasus_ocor[[i]] %>%
      setNames(c("munic_ibge_6","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018")) %>%
      slice(-1)
    vlmorb_datasus_ocor[[i]] <- melt(vlmorb_datasus_ocor[[i]], id = c("munic_ibge_6"))
    vlmorb_datasus_ocor[[i]] <- vlmorb_datasus_ocor[[i]] %>%
      setNames(c("munic_ibge_6","Ano",gsub(".csv","",file_list_vlmorb_ocor[i])))
    
    vlmorb_datasus_ocor[[i]]$munic_ibge_6 <- substr(vlmorb_datasus_ocor[[i]]$munic_ibge_6,1,6)
    
    vlmorb_datasus_ocor[[i]][,3] <- ifelse(vlmorb_datasus_ocor[[i]][,3]=="-",0,vlmorb_datasus_ocor[[i]][,3])
    vlmorb_datasus_ocor[[i]] <- vlmorb_datasus_ocor[[i]] %>%
      left_join(Munic_uber[c("munic_ibge_6","munic")],by = c("munic_ibge_6"))
    
    vlmorb_datasus_ocor[[i]] <- subset(vlmorb_datasus_ocor[[i]],!(is.na(vlmorb_datasus_ocor[[i]]$munic)))
    
    vlmorb_datasus_ocor[[i]]$munic <- as.character(vlmorb_datasus_ocor[[i]]$munic)
    vlmorb_datasus_ocor[[i]]$munic_ibge_6 <- as.character(vlmorb_datasus_ocor[[i]]$munic_ibge_6)
    vlmorb_datasus_ocor[[i]]$Ano <- as.character(vlmorb_datasus_ocor[[i]]$Ano)
  }
  
  vlmorb_datasus_ocor_ <- vlmorb_datasus_ocor %>%
    bind_rows() %>%
    mutate(
      ocorr_valor_20a29 = as.numeric(sub(",",".",ocorr_valor_20a29, fixed = TRUE)),
      ocorr_valor_30a39 = as.numeric(sub(",",".",ocorr_valor_30a39, fixed = TRUE)),
      ocorr_valor_40mais = as.numeric(sub(",",".",ocorr_valor_40mais, fixed = TRUE)),
      ocorr_valor_fem = as.numeric(sub(",",".",ocorr_valor_fem, fixed = TRUE)),
      ocorr_valor_masc = as.numeric(sub(",",".",ocorr_valor_masc, fixed = TRUE)),
      ocorr_valor_menor20 = as.numeric(sub(",",".",ocorr_valor_menor20, fixed = TRUE)),
      ocorr_valor_total = as.numeric(sub(",",".",ocorr_valor_total, fixed = TRUE)))
  
  vargroup <- c("ocorr_valor_20a29","ocorr_valor_30a39","ocorr_valor_40mais","ocorr_valor_fem","ocorr_valor_masc","ocorr_valor_menor20","ocorr_valor_total")
  
  vlmorb_datasus_ocor_ <- vlmorb_datasus_ocor_ %>%
    group_by(Ano,munic,munic_ibge_6) %>%
    summarise_at(vargroup, sum, na.rm=TRUE)
  
  vlmorb_datasus_ocor_ <- vlmorb_datasus_ocor_[c("munic_ibge_6","Ano","ocorr_valor_20a29","ocorr_valor_30a39","ocorr_valor_40mais","ocorr_valor_fem","ocorr_valor_masc","ocorr_valor_menor20","ocorr_valor_total")]
  vlmorb_datasus_ocor_ <- vlmorb_datasus_ocor_ %>%
    setNames(c("munic_ibge_6","Ano","ocor_valor_20a29","ocor_valor_30a39","ocor_valor_40mais","ocor_valor_fem","ocor_valor_masc","ocor_valor_menor20","ocor_valor_total")) %>%
    mutate(Ano = as.character(Ano),
           munic_ibge_6 = as.character(munic_ibge_6))
  
}

##### Dados Morbidade DATASUS Residência - Valor Internações Serviços Hospitalares
{
  setwd("C:/Users/gabri/Google Drive/Arquivos Mono/Dados/Valor Gastos SH Morbidade - Residência/")
  
  file_list_vl_morbsh_res <- list.files(path="C:/Users/gabri/Google Drive/Arquivos Mono/Dados/Valor Gastos SH Morbidade - Residência/")
  vl_morbsh_datasus_res<- list()
  
  for (i in 1:length(file_list_vl_morbsh_res)){
    vl_morbsh_datasus_res[[i]] <- read.csv(file_list_vl_morbsh_res[i],sep = ";",header = FALSE,na.strings = TRUE,skip = 3)
    vl_morbsh_datasus_res[[i]] <- subset(vl_morbsh_datasus_res[[i]], vl_morbsh_datasus_res[[i]]$V2!="")
    vl_morbsh_datasus_res[[i]] <- vl_morbsh_datasus_res[[i]][-c(5598,5599),-c(2,12)]
    vl_morbsh_datasus_res[[i]] <- vl_morbsh_datasus_res[[i]] %>%
      setNames(c("munic_ibge_6","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018")) %>%
      slice(-1)
    vl_morbsh_datasus_res[[i]] <- melt(vl_morbsh_datasus_res[[i]], id = c("munic_ibge_6"))
    vl_morbsh_datasus_res[[i]] <- vl_morbsh_datasus_res[[i]] %>%
      setNames(c("munic_ibge_6","Ano",gsub(".csv","",file_list_vl_morbsh_res[i])))
    
    vl_morbsh_datasus_res[[i]]$munic_ibge_6 <- substr(vl_morbsh_datasus_res[[i]]$munic_ibge_6,1,6)
    
    vl_morbsh_datasus_res[[i]][,3] <- ifelse(vl_morbsh_datasus_res[[i]][,3]=="-",0,vl_morbsh_datasus_res[[i]][,3])
    vl_morbsh_datasus_res[[i]] <- vl_morbsh_datasus_res[[i]] %>%
      left_join(Munic_uber[c("munic_ibge_6","munic")],by = c("munic_ibge_6"))
    
    vl_morbsh_datasus_res[[i]] <- subset(vl_morbsh_datasus_res[[i]],!(is.na(vl_morbsh_datasus_res[[i]]$munic)))
  }
  vl_morbsh_datasus_res_ <- do.call("cbind",vl_morbsh_datasus_res)
  vl_morbsh_datasus_res_ <- vl_morbsh_datasus_res_[c("munic_ibge_6","Ano","resid_valor_sh_20a29","resid_valor_sh_30a39","resid_valor_sh_40mais","resid_valor_sh_fem","resid_valor_sh_masc","resid_valor_sh_menor20","resid_valor_sh_total")]
  vl_morbsh_datasus_res_ <- vl_morbsh_datasus_res_ %>%
    setNames(c("munic_ibge_6","Ano","res_valor_sh_20a29","res_valor_sh_30a39","res_valor_sh_40mais","res_valor_sh_fem","res_valor_sh_masc","res_valor_sh_menor20","res_valor_sh_total")) %>%
    mutate(Ano = as.character(Ano),
           munic_ibge_6 = as.character(munic_ibge_6))
  
}
##### Dados Morbidade DATASUS Ocorrência - Valor Internações Serviços Hospitalares
{
  setwd("C:/Users/gabri/Google Drive/Arquivos Mono/Dados/Valor Gastos SH Morbidade - Ocorrência/")
  
  file_list_vlmorbsh_ocor <- list.files(path="C:/Users/gabri/Google Drive/Arquivos Mono/Dados/Valor Gastos SH Morbidade - Ocorrência/")
  vlmorbsh_datasus_ocor<- list()
  
  for (i in 1:length(file_list_vlmorbsh_ocor)){
    vlmorbsh_datasus_ocor[[i]] <- read.csv(file_list_vlmorbsh_ocor[i],sep = ";",header = FALSE,na.strings = TRUE,skip = 3)
    vlmorbsh_datasus_ocor[[i]] <- subset(vlmorbsh_datasus_ocor[[i]], vlmorbsh_datasus_ocor[[i]]$V2!="")
    vlmorbsh_datasus_ocor[[i]] <- vlmorbsh_datasus_ocor[[i]][-c(5598,5599),-c(2,12)]
    vlmorbsh_datasus_ocor[[i]] <- vlmorbsh_datasus_ocor[[i]] %>%
      setNames(c("munic_ibge_6","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018")) %>%
      slice(-1)
    vlmorbsh_datasus_ocor[[i]] <- melt(vlmorbsh_datasus_ocor[[i]], id = c("munic_ibge_6"))
    vlmorbsh_datasus_ocor[[i]] <- vlmorbsh_datasus_ocor[[i]] %>%
      setNames(c("munic_ibge_6","Ano",gsub(".csv","",file_list_vlmorbsh_ocor[i])))
    
    vlmorbsh_datasus_ocor[[i]]$munic_ibge_6 <- substr(vlmorbsh_datasus_ocor[[i]]$munic_ibge_6,1,6)
    
    vlmorbsh_datasus_ocor[[i]][,3] <- ifelse(vlmorbsh_datasus_ocor[[i]][,3]=="-",0,vlmorbsh_datasus_ocor[[i]][,3])
    vlmorbsh_datasus_ocor[[i]] <- vlmorbsh_datasus_ocor[[i]] %>%
      left_join(Munic_uber[c("munic_ibge_6","munic")],by = c("munic_ibge_6"))
    
    vlmorbsh_datasus_ocor[[i]] <- subset(vlmorbsh_datasus_ocor[[i]],!(is.na(vlmorbsh_datasus_ocor[[i]]$munic)))
    
    vlmorbsh_datasus_ocor[[i]]$munic <- as.character(vlmorbsh_datasus_ocor[[i]]$munic)
    vlmorbsh_datasus_ocor[[i]]$munic_ibge_6 <- as.character(vlmorbsh_datasus_ocor[[i]]$munic_ibge_6)
    vlmorbsh_datasus_ocor[[i]]$Ano <- as.character(vlmorbsh_datasus_ocor[[i]]$Ano)
  }
  
  vlmorbsh_datasus_ocor_ <- vlmorbsh_datasus_ocor %>%
    bind_rows() %>%
    mutate(
      ocorr_valor_sh_20a29 = as.numeric(sub(",",".",ocorr_valor_sh_20a29, fixed = TRUE)),
      ocorr_valor_sh_30a39 = as.numeric(sub(",",".",ocorr_valor_sh_30a39, fixed = TRUE)),
      ocorr_valor_sh_40mais = as.numeric(sub(",",".",ocorr_valor_sh_40mais, fixed = TRUE)),
      ocorr_valor_sh_fem = as.numeric(sub(",",".",ocorr_valor_sh_fem, fixed = TRUE)),
      ocorr_valor_sh_masc = as.numeric(sub(",",".",ocorr_valor_sh_masc, fixed = TRUE)),
      ocorr_valor_sh_menor20 = as.numeric(sub(",",".",ocorr_valor_sh_menor20, fixed = TRUE)),
      ocorr_valor_sh_total = as.numeric(sub(",",".",ocorr_valor_sh_total, fixed = TRUE)))
  
  vargroup <- c("ocorr_valor_sh_20a29","ocorr_valor_sh_30a39","ocorr_valor_sh_40mais","ocorr_valor_sh_fem","ocorr_valor_sh_masc","ocorr_valor_sh_menor20","ocorr_valor_sh_total")
  
  vlmorbsh_datasus_ocor_ <- vlmorbsh_datasus_ocor_ %>%
    group_by(Ano,munic,munic_ibge_6) %>%
    summarise_at(vargroup, sum, na.rm=TRUE)
  
  vlmorbsh_datasus_ocor_ <- vlmorbsh_datasus_ocor_[c("munic_ibge_6","Ano","ocorr_valor_sh_20a29","ocorr_valor_sh_30a39","ocorr_valor_sh_40mais","ocorr_valor_sh_fem","ocorr_valor_sh_masc","ocorr_valor_sh_menor20","ocorr_valor_sh_total")]
  vlmorbsh_datasus_ocor_ <- vlmorbsh_datasus_ocor_ %>%
    setNames(c("munic_ibge_6","Ano","ocor_valor_sh_20a29","ocor_valor_sh_30a39","ocor_valor_sh_40mais","ocor_valor_sh_fem","ocor_valor_sh_masc","ocor_valor_sh_menor20","ocor_valor_sh_total")) %>%
    mutate(Ano = as.character(Ano),
           munic_ibge_6 = as.character(munic_ibge_6))
  
}


setwd("C:/Users/gabri/Google Drive/Arquivos Mono/Dados/")  

###### Join e preparação final dos dados
{
df_modelo <- dfvf_munic_frota %>%
                left_join(Munic_pop, by=c("Ano","cod_munic")) %>%
                left_join(Munic_pib_pc, by=c("Ano","cod_munic")) %>%
                left_join(igpm, by=c("Ano")) %>%
                left_join(ipca, by=c("Ano")) %>%
                left_join(rais_sm, by = c("munic_ibge_6","Ano")) %>%
                left_join(mort_datasus_, by = c("munic_ibge_6","Ano")) %>%
                left_join(mort_datasus_ocor_, by = c("munic_ibge_6","Ano")) %>%
                left_join(morb_datasus_ocor_, by = c("munic_ibge_6","Ano")) %>%  
                left_join(morb_datasus_res_, by = c("munic_ibge_6","Ano")) %>%
                left_join(vlmorb_datasus_ocor_, by = c("munic_ibge_6","Ano")) %>%
                left_join(vlmorb_datasus_res_, by = c("munic_ibge_6","Ano")) %>%
                left_join(vlmorbsh_datasus_ocor_, by = c("munic_ibge_6","Ano")) %>%
                left_join(vl_morbsh_datasus_res_ , by = c("munic_ibge_6","Ano")) %>%
                left_join(leitos_datasus_ , by = c("munic_ibge_6","Ano"))%>%
                left_join(medicos_datasus_ , by = c("munic_ibge_6","Ano"))
    
###Exclusão de variáveis
{
df_modelo$UF <- NULL
df_modelo$MUNICIPIO <- NULL
#df_modelo$AUTOMOVEL <- NULL
df_modelo$BONDE <- NULL
#df_modelo$CAMINHAO <- NULL
df_modelo$`CAMINHAO TRATOR` <- NULL
#df_modelo$CAMINHONETE <- NULL
df_modelo$CAMIONETA <- NULL
df_modelo$`CHASSI PLATAF` <- NULL
df_modelo$CICLOMOTOR <- NULL
#df_modelo$`MICRO-ONIBUS` <- NULL
#df_modelo$MOTOCICLETA <- NULL
#df_modelo$MOTONETA <- NULL
#df_modelo$ONIBUS <- NULL
df_modelo$QUADRICICLO <- NULL
df_modelo$REBOQUE <- NULL
df_modelo$`SEMI-REBOQUE` <- NULL
df_modelo$`SIDE-CAR` <- NULL
df_modelo$OUTROS <- NULL
df_modelo$`TRATOR ESTEI` <- NULL
df_modelo$`TRATOR RODAS` <- NULL
df_modelo$TRICICLO <- NULL
df_modelo$UTILITARIO <- NULL
df_modelo$munic_merge <- NULL
#df_modelo$regiao <- NULL
#df_modelo$uf <- NULL
df_modelo$cod_UF <- NULL
df_modelo$munic_ibge_6 <- NULL
#df_modelo$munic <- NULL
df_modelo$munic_link <- NULL
df_modelo$Fonte <- NULL
df_modelo$`2014` <- NULL
}

df_modelo <- df_modelo %>%
  rename(total_frota = TOTAL,
         carro = AUTOMOVEL,
         motocicleta = MOTOCICLETA,
         motoneta = MOTONETA,
         onibus = ONIBUS,
         microonibus = `MICRO-ONIBUS`,
         caminhonete = CAMINHONETE,
         caminhao = CAMINHAO)

df_modelo$mês_ano_Entrada_num <- as.numeric(substr(as.character(as.Date.numeric(df_modelo$mês_ano_Entrada.Uber,origin = "1900-01-01")),6,7))
df_modelo$mês_ano_Entrada.Uber <- months.Date(as.Date.numeric(df_modelo$mês_ano_Entrada.Uber,origin = "1900-01-01"))
df_modelo$capital <- replace_na(df_modelo$capital,0)
df_modelo$ano_Entrada_Uber_nv <- ifelse(df_modelo$mês_ano_Entrada_num>=11,as.character(as.numeric(df_modelo$ano_Entrada_Uber)+1),df_modelo$ano_Entrada_Uber)
df_modelo$tratamento <- ifelse(df_modelo$Ano>=df_modelo$ano_Entrada_Uber_nv,1,0)
df_modelo$tratamento_dec <- ifelse(as.numeric(df_modelo$Ano) == as.numeric(df_modelo$ano_Entrada_Uber),(13/12)-(df_modelo$mês_ano_Entrada_num/12),ifelse(as.numeric(df_modelo$Ano)>as.numeric(df_modelo$ano_Entrada_Uber),1,0))

#### Rename ordenação e formato variável
{
df_modelo <- df_modelo[,c("Ano",	"cod_regiao",	"regiao",	"cod_uf",	"uf",	"cod_munic",	"munic",	"capital",	"ano_Entrada_Uber","ano_Entrada_Uber_nv","mês_ano_Entrada_num",	"mês_ano_Entrada.Uber",	"tratamento","tratamento_dec",	"pop",	"pib_preco_corrent","percet_vl_brut_agropec","percet_vl_brut_ind","percet_vl_brut_serv","percet_vl_brut_admpublic","salario_medio",	"total_frota",	"carro",	"motocicleta",	"motoneta", "onibus", "caminhao","caminhonete","microonibus","igpm","ipca", vargroup_leitos[1],vargroup_leitos[2],vargroup_leitos[3],vargroup_leitos[4],vargroup_leitos[5],vargroup_leitos[6],vargroup_leitos[7],vargroup_leitos[8],vargroup_leitos[9],vargroup_leitos[10],vargroup_leitos[11],"total_medicos","res_mort_transp_20a29","res_mort_transp_30a39","res_mort_transp_40mais","res_mort_transp_fem","res_mort_transp_masc","res_mort_transp_menor20","res_mort_transp_total","ocor_mort_transp_20a29","ocor_mort_transp_30a39","ocor_mort_transp_40mais","ocor_mort_transp_fem","ocor_mort_transp_masc","ocor_mort_transp_menor20","ocor_mort_transp_total","ocor_internacoes_transp_20a29",	"ocor_internacoes_transp_30a39",	"ocor_internacoes_transp_40mais",	"ocor_internacoes_transp_fem",	"ocor_internacoes_transp_masc",	"ocor_internacoes_transp_menor20",	"ocor_internacoes_transp_total",	"res_internacoes_transp_20a29",	"res_internacoes_transp_30a39",	"res_internacoes_transp_40mais",	"res_internacoes_transp_fem",	"res_internacoes_transp_masc",	"res_internacoes_transp_menor20",	"res_internacoes_transp_total",	"ocor_valor_20a29",	"ocor_valor_30a39",	"ocor_valor_40mais",	"ocor_valor_fem",	"ocor_valor_masc",	"ocor_valor_menor20",	"ocor_valor_total",	"res_valor_20a29",	"res_valor_30a39",	"res_valor_40mais",	"res_valor_fem",	"res_valor_masc",	"res_valor_menor20",	"res_valor_total",	"ocor_valor_sh_20a29",	"ocor_valor_sh_30a39",	"ocor_valor_sh_40mais",	"ocor_valor_sh_fem",	"ocor_valor_sh_masc",	"ocor_valor_sh_menor20",	"ocor_valor_sh_total",	"res_valor_sh_20a29",	"res_valor_sh_30a39",	"res_valor_sh_40mais",	"res_valor_sh_fem",	"res_valor_sh_masc",	"res_valor_sh_menor20",	"res_valor_sh_total")]

df_modelo[is.na(df_modelo)] <- 0  
  
df_modelo <- df_modelo %>%
  mutate(cod_regiao = as.character(cod_regiao),
         cod_uf = as.character(cod_uf),
         capital = as.character(capital),
         ano_Entrada_Uber = as.character(ano_Entrada_Uber),
         tratamento = as.character(tratamento),
         pop = as.integer(pop),
         pib_preco_corrent = as.integer(pib_preco_corrent),
         total_frota = as.integer(total_frota),
         carro = as.integer(carro),
         motocicleta = as.integer(motocicleta),
         motoneta = as.integer(motoneta),
         onibus = as.integer(onibus),
         caminhao = as.integer(caminhao),
         caminhonete = as.integer(caminhonete),
         microonibus = as.integer(microonibus),
         percet_vl_brut_agropec = as.numeric(sub(",",".",percet_vl_brut_agropec, fixed = TRUE)),
         percet_vl_brut_ind = as.numeric(sub(",",".",percet_vl_brut_ind, fixed = TRUE)),
         percet_vl_brut_serv = as.numeric(sub(",",".",percet_vl_brut_serv, fixed = TRUE)),
         percet_vl_brut_admpublic = as.numeric(sub(",",".",percet_vl_brut_admpublic, fixed = TRUE)),
         salario_medio = as.numeric(sub(",",".",salario_medio, fixed = TRUE)),
         igpm = as.numeric(sub(",",".",igpm, fixed = TRUE)),
         ipca = as.numeric(sub(",",".",ipca, fixed = TRUE)),
         
         res_mort_transp_20a29 = as.numeric(sub(",",".",res_mort_transp_20a29, fixed = TRUE)),
         res_mort_transp_30a39 = as.numeric(sub(",",".",res_mort_transp_30a39, fixed = TRUE)),
         res_mort_transp_40mais = as.numeric(sub(",",".",res_mort_transp_40mais, fixed = TRUE)),
         res_mort_transp_fem = as.numeric(sub(",",".",res_mort_transp_fem, fixed = TRUE)),
         res_mort_transp_masc = as.numeric(sub(",",".",res_mort_transp_masc, fixed = TRUE)),
         res_mort_transp_menor20 = as.numeric(sub(",",".",res_mort_transp_menor20, fixed = TRUE)),
         res_mort_transp_total = as.numeric(sub(",",".",res_mort_transp_total, fixed = TRUE)),
         ocor_mort_transp_20a29 = as.numeric(sub(",",".",ocor_mort_transp_20a29, fixed = TRUE)),
         ocor_mort_transp_30a39 = as.numeric(sub(",",".",ocor_mort_transp_30a39, fixed = TRUE)),
         ocor_mort_transp_40mais = as.numeric(sub(",",".",ocor_mort_transp_40mais, fixed = TRUE)),
         ocor_mort_transp_fem = as.numeric(sub(",",".",ocor_mort_transp_fem, fixed = TRUE)),
         ocor_mort_transp_masc = as.numeric(sub(",",".",ocor_mort_transp_masc, fixed = TRUE)),
         ocor_mort_transp_menor20 = as.numeric(sub(",",".",ocor_mort_transp_menor20, fixed = TRUE)),
         ocor_mort_transp_total = as.numeric(sub(",",".",ocor_mort_transp_total, fixed = TRUE)),
         
         res_internacoes_transp_20a29 = as.numeric(sub(",",".",res_internacoes_transp_20a29, fixed = TRUE)),
         res_internacoes_transp_30a39 = as.numeric(sub(",",".",res_internacoes_transp_30a39, fixed = TRUE)),
         res_internacoes_transp_40mais = as.numeric(sub(",",".",res_internacoes_transp_40mais, fixed = TRUE)),
         res_internacoes_transp_fem = as.numeric(sub(",",".",res_internacoes_transp_fem, fixed = TRUE)),
         res_internacoes_transp_masc = as.numeric(sub(",",".",res_internacoes_transp_masc, fixed = TRUE)),
         res_internacoes_transp_menor20 = as.numeric(sub(",",".",res_internacoes_transp_menor20, fixed = TRUE)),
         res_internacoes_transp_total = as.numeric(sub(",",".",res_internacoes_transp_total, fixed = TRUE)),
         ocor_internacoes_transp_20a29 = as.numeric(sub(",",".",ocor_internacoes_transp_20a29, fixed = TRUE)),
         ocor_internacoes_transp_30a39 = as.numeric(sub(",",".",ocor_internacoes_transp_30a39, fixed = TRUE)),
         ocor_internacoes_transp_40mais = as.numeric(sub(",",".",ocor_internacoes_transp_40mais, fixed = TRUE)),
         ocor_internacoes_transp_fem = as.numeric(sub(",",".",ocor_internacoes_transp_fem, fixed = TRUE)),
         ocor_internacoes_transp_masc = as.numeric(sub(",",".",ocor_internacoes_transp_masc, fixed = TRUE)),
         ocor_internacoes_transp_menor20 = as.numeric(sub(",",".",ocor_internacoes_transp_menor20, fixed = TRUE)),
         ocor_internacoes_transp_total = as.numeric(sub(",",".",ocor_internacoes_transp_total, fixed = TRUE)),
         
         res_valor_20a29 = as.numeric(sub(",",".",res_valor_20a29, fixed = TRUE)),
         res_valor_30a39 = as.numeric(sub(",",".",res_valor_30a39, fixed = TRUE)),
         res_valor_40mais = as.numeric(sub(",",".",res_valor_40mais, fixed = TRUE)),
         res_valor_fem = as.numeric(sub(",",".",res_valor_fem, fixed = TRUE)),
         res_valor_masc = as.numeric(sub(",",".",res_valor_masc, fixed = TRUE)),
         res_valor_menor20 = as.numeric(sub(",",".",res_valor_menor20, fixed = TRUE)),
         res_valor_total = as.numeric(sub(",",".",res_valor_total, fixed = TRUE)),
         ocor_valor_20a29 = as.numeric(sub(",",".",ocor_valor_20a29, fixed = TRUE)),
         ocor_valor_30a39 = as.numeric(sub(",",".",ocor_valor_30a39, fixed = TRUE)),
         ocor_valor_40mais = as.numeric(sub(",",".",ocor_valor_40mais, fixed = TRUE)),
         ocor_valor_fem = as.numeric(sub(",",".",ocor_valor_fem, fixed = TRUE)),
         ocor_valor_masc = as.numeric(sub(",",".",ocor_valor_masc, fixed = TRUE)),
         ocor_valor_menor20 = as.numeric(sub(",",".",ocor_valor_menor20, fixed = TRUE)),
         ocor_valor_total = as.numeric(sub(",",".",ocor_valor_total, fixed = TRUE)),
         
         res_valor_sh_20a29 = as.numeric(sub(",",".",res_valor_sh_20a29, fixed = TRUE)),
         res_valor_sh_30a39 = as.numeric(sub(",",".",res_valor_sh_30a39, fixed = TRUE)),
         res_valor_sh_40mais = as.numeric(sub(",",".",res_valor_sh_40mais, fixed = TRUE)),
         res_valor_sh_fem = as.numeric(sub(",",".",res_valor_sh_fem, fixed = TRUE)),
         res_valor_sh_masc = as.numeric(sub(",",".",res_valor_sh_masc, fixed = TRUE)),
         res_valor_sh_menor20 = as.numeric(sub(",",".",res_valor_sh_menor20, fixed = TRUE)),
         res_valor_sh_total = as.numeric(sub(",",".",res_valor_sh_total, fixed = TRUE)),
         ocor_valor_sh_20a29 = as.numeric(sub(",",".",ocor_valor_sh_20a29, fixed = TRUE)),
         ocor_valor_sh_30a39 = as.numeric(sub(",",".",ocor_valor_sh_30a39, fixed = TRUE)),
         ocor_valor_sh_40mais = as.numeric(sub(",",".",ocor_valor_sh_40mais, fixed = TRUE)),
         ocor_valor_sh_fem = as.numeric(sub(",",".",ocor_valor_sh_fem, fixed = TRUE)),
         ocor_valor_sh_masc = as.numeric(sub(",",".",ocor_valor_sh_masc, fixed = TRUE)),
         ocor_valor_sh_menor20 = as.numeric(sub(",",".",ocor_valor_sh_menor20, fixed = TRUE)),
         ocor_valor_sh_total = as.numeric(sub(",",".",ocor_valor_sh_total, fixed = TRUE)),
         
         
         
         )
}

### alterando variáveis
### colocando por 100K de hab
{
df_modelo <- df_modelo %>%
  mutate(total_frota = (total_frota/pop)*100000,
         carro = (carro/pop)*100000,
         motocicleta = (motocicleta/pop)*100000,
         motoneta = (motoneta/pop)*100000,
         onibus = (onibus/pop)*100000,
         caminhao = (caminhao/pop)*100000,
         caminhonete = (caminhonete/pop)*100000,
         microonibus = (microonibus/pop)*100000,
         
         res_mort_transp_20a29 = (res_mort_transp_20a29/pop)*100000,
         res_mort_transp_30a39 = (res_mort_transp_30a39/pop)*100000,
         res_mort_transp_40mais = (res_mort_transp_40mais/pop)*100000,
         res_mort_transp_fem = (res_mort_transp_fem/pop)*100000,
         res_mort_transp_masc = (res_mort_transp_masc/pop)*100000,
         res_mort_transp_menor20 = (res_mort_transp_menor20/pop)*100000,
         res_mort_transp_total = (res_mort_transp_total/pop)*100000,
         ocor_mort_transp_20a29 = (ocor_mort_transp_20a29/pop)*100000,
         ocor_mort_transp_30a39 = (ocor_mort_transp_30a39/pop)*100000,
         ocor_mort_transp_40mais = (ocor_mort_transp_40mais/pop)*100000,
         ocor_mort_transp_fem = (ocor_mort_transp_fem/pop)*100000,
         ocor_mort_transp_masc = (ocor_mort_transp_masc/pop)*100000,
         ocor_mort_transp_menor20 = (ocor_mort_transp_menor20/pop)*100000,
         ocor_mort_transp_total = (ocor_mort_transp_total/pop)*100000,

        res_internacoes_transp_20a29 = (res_internacoes_transp_20a29/pop)*100000,
        res_internacoes_transp_30a39 = (res_internacoes_transp_30a39/pop)*100000,
        res_internacoes_transp_40mais = (res_internacoes_transp_40mais/pop)*100000,
        res_internacoes_transp_fem = (res_internacoes_transp_fem/pop)*100000,
        res_internacoes_transp_masc = (res_internacoes_transp_masc/pop)*100000,
        res_internacoes_transp_menor20 = (res_internacoes_transp_menor20/pop)*100000,
        res_internacoes_transp_total = (res_internacoes_transp_total/pop)*100000,
        ocor_internacoes_transp_20a29 = (ocor_internacoes_transp_20a29/pop)*100000,
        ocor_internacoes_transp_30a39 = (ocor_internacoes_transp_30a39/pop)*100000,
        ocor_internacoes_transp_40mais = (ocor_internacoes_transp_40mais/pop)*100000,
        ocor_internacoes_transp_fem = (ocor_internacoes_transp_fem/pop)*100000,
        ocor_internacoes_transp_masc = (ocor_internacoes_transp_masc/pop)*100000,
        ocor_internacoes_transp_menor20 = (ocor_internacoes_transp_menor20/pop)*100000,
        ocor_internacoes_transp_total = (ocor_internacoes_transp_total/pop)*100000,
        
        res_valor_20a29 = (res_valor_20a29/pop)*100000,
        res_valor_30a39 = (res_valor_30a39/pop)*100000,
        res_valor_40mais = (res_valor_40mais/pop)*100000,
        res_valor_fem = (res_valor_fem/pop)*100000,
        res_valor_masc = (res_valor_masc/pop)*100000,
        res_valor_menor20 = (res_valor_menor20/pop)*100000,
        res_valor_total = (res_valor_total/pop)*100000,
        ocor_valor_20a29 = (ocor_valor_20a29/pop)*100000,
        ocor_valor_30a39 = (ocor_valor_30a39/pop)*100000,
        ocor_valor_40mais = (ocor_valor_40mais/pop)*100000,
        ocor_valor_fem = (ocor_valor_fem/pop)*100000,
        ocor_valor_masc = (ocor_valor_masc/pop)*100000,
        ocor_valor_menor20 = (ocor_valor_menor20/pop)*100000,
        ocor_valor_total = (ocor_valor_total/pop)*100000,
        
        res_valor_sh_20a29 = (res_valor_sh_20a29/pop)*100000,
        res_valor_sh_30a39 = (res_valor_sh_30a39/pop)*100000,
        res_valor_sh_40mais = (res_valor_sh_40mais/pop)*100000,
        res_valor_sh_fem = (res_valor_sh_fem/pop)*100000,
        res_valor_sh_masc = (res_valor_sh_masc/pop)*100000,
        res_valor_sh_menor20 = (res_valor_sh_menor20/pop)*100000,
        res_valor_sh_total = (res_valor_sh_total/pop)*100000,
        ocor_valor_sh_20a29 = (ocor_valor_sh_20a29/pop)*100000,
        ocor_valor_sh_30a39 = (ocor_valor_sh_30a39/pop)*100000,
        ocor_valor_sh_40mais = (ocor_valor_sh_40mais/pop)*100000,
        ocor_valor_sh_fem = (ocor_valor_sh_fem/pop)*100000,
        ocor_valor_sh_masc = (ocor_valor_sh_masc/pop)*100000,
        ocor_valor_sh_menor20 = (ocor_valor_sh_menor20/pop)*100000,
        ocor_valor_sh_total = (ocor_valor_sh_total/pop)*100000,
        total_medicos = (total_medicos/pop)*100000)

  
  for (i in 1:length(vargroup_leitos)){
    var <- vargroup_leitos[i]
    df_modelo[,c(var)] <-(df_modelo[,c(var)]/df_modelo$pop)*100000
  }
  
  
#### Deflacionando valores 
  
lista_valores <- c("ocor_valor_20a29",	"ocor_valor_30a39",	"ocor_valor_40mais",	"ocor_valor_fem",	"ocor_valor_masc",	"ocor_valor_menor20",	"ocor_valor_total",	"res_valor_20a29",	"res_valor_30a39",	"res_valor_40mais",	"res_valor_fem",	"res_valor_masc",	"res_valor_menor20",	"res_valor_total",	"ocor_valor_sh_20a29",	"ocor_valor_sh_30a39",	"ocor_valor_sh_40mais",	"ocor_valor_sh_fem",	"ocor_valor_sh_masc",	"ocor_valor_sh_menor20",	"ocor_valor_sh_total",	"res_valor_sh_20a29",	"res_valor_sh_30a39",	"res_valor_sh_40mais",	"res_valor_sh_fem",	"res_valor_sh_masc",	"res_valor_sh_menor20",	"res_valor_sh_total")

for (i in 1:length(lista_valores)){
  var <- lista_valores[[i]]
  df_modelo[,c(var)] <- (df_modelo[,c(var)]/df_modelo$igpm)*as.numeric(sub(",",".",igpm[length(igpm$Ano),2],fixed = TRUE))
}

  df_modelo$salario_medio_def <- (df_modelo$salario_medio/df_modelo$ipca)*as.numeric(sub(",",".",ipca[length(ipca$Ano),2],fixed = TRUE))
  df_modelo$pib_real_milh <- (df_modelo$pib_preco_corrent/df_modelo$igpm)*as.numeric(sub(",",".",igpm[length(igpm$Ano),2],fixed = TRUE))/1000
  
  df_modelo$nv_tecno <- (100-df_modelo$percet_vl_brut_agropec)
  df_modelo$pib_perc_def <- (((df_modelo$pib_preco_corrent*1000)/df_modelo$pop)/df_modelo$igpm)*as.numeric(sub(",",".",igpm[length(igpm$Ano),2],fixed = TRUE))

}
  
  
tratamento_munic <- df_modelo[c("cod_munic","tratamento_dec")] %>%
  group_by(cod_munic) %>%
  summarise(tratamento_dec = sum(tratamento_dec))
  
tratamento_munic$tratamento_amostra <- ifelse(tratamento_munic$tratamento_dec==0,0,1)
tratamento_munic$tratamento_dec <- NULL

df_modelo <- df_modelo %>%
  left_join(tratamento_munic)

}

########## Exportando DF ###########
write.xlsx(df_modelo,"0. Saída Programa/df_modelo_V2.xlsx")