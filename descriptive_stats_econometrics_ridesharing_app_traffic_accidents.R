###### Monografia 2
###### Aluno: Gabriel Amil Bastos
###### UFES 2020
###### Script de Estatísticas básicas e Modelagem

#### Pacotes
{
    install.packages("readxl")
    install.packages("tidyverse", dependecies = TRUE)
    install.packages("stargazer", dependencies = TRUE)
    install.packages("car")
    install.packages("plm", dependencies = TRUE)
    install.packages("lmtest")
    install.packages("dplyr")
    install.packages("sandwich")
    install.packages("ggpubr", dependecies = TRUE)
    install.packages("sjPlot", dependencies = TRUE)
    install.packages("pracma", dependencies = TRUE)
  
  library(readxl)
  library(tidyverse)
  library(stargazer)
  library(car)
  library(plm)
  library(lmtest)
  library(dplyr)
  library(sandwich)
  library(ggpubr)
  library(sjPlot)
  library(pracma)
  }
rm(list=ls())

setwd("C:/Users/gabri/Google Drive/Arquivos Mono/Dados/0. Saída Programa")


df_uber <- as.data.frame(read_xlsx("df_modelo_V2.xlsx"))

###### Estatística Descritiva
{
  stargazer(subset(df_uber[c("total_frota",	"carro",	"motocicleta",	"total_leitos",	"total_medicos",
                             "ocor_mort_transp_total",	"ocor_internacoes_transp_total","ocor_valor_total",	"ocor_valor_sh_total",
                             "pib_real_milh",	"pop",	"salario_medio_def","pib_perc_def",	"percet_vl_brut_agropec",
                             "percet_vl_brut_ind",	"percet_vl_brut_serv",	"percet_vl_brut_admpublic")],df_uber$Ano=="2017"),
            covariate.labels = c("Frota total (100 mil hab.)",	"Frota de carros (100 mil hab.)",	"Frota de motocicletas (100 mil hab.)",	
                                 "Total de leitos (100 mil hab.)","Total de Médicos (100 mil hab.)","Óbitos (100 mil hab.)",
                                 "Internações (100 mil hab.)","Valor das Internações (100 mil hab.)","Valor dos Serviços Hospitalares (100 mil hab.)",
                                 "PIB (R$ milhões)","População",	"Salário médio (R$)","PIB per capita (R$)","VA Agricultura (%)",
                                 "VA Indústria (%)","VA Serviços (%)",	"VA Administração Pública (%)"),
            summary.stat = c("n","mean", "min", "max","sd"), type="text", title="Estatísticas Descritivas", digits=2,decimal.mark = ",",
            digit.separator = ".",style = "jpam", out="01_Descritivas.html")#, out="01_Descritivas.html"
}

###### Teste t amostra
{
  variables=c("total_frota",	"carro",	"motocicleta",	"total_leitos",	"total_medicos",
              "ocor_mort_transp_total",	"ocor_internacoes_transp_total","ocor_valor_total",	"ocor_valor_sh_total",
              "pib_real_milh",	"pop",	"salario_medio_def","pib_perc_def",	"percet_vl_brut_agropec",
              "percet_vl_brut_ind",	"percet_vl_brut_serv",	"percet_vl_brut_admpublic")
  
  labels=c("Frota total (100 mil hab.)",	"Frota de carros (100 mil hab.)",	"Frota de motocicletas (100 mil hab.)",	
           "Total de leitos (100 mil hab.)","Total de Médicos (100 mil hab.)","Óbitos (100 mil hab.)",
           "Internações (100 mil hab.)","Valor das Internações (100 mil hab.)","Valor dos Serviços Hospitalares (100 mil hab.)",
           "PIB (R$ milhões)","População",	"Salário médio (R$)","PIB per capita (R$)","VA Agricultura (%)",
           "VA Indústria (%)","VA Serviços (%)",	"VA Administração Pública (%)")
  
  for (j in variables) {
    assign(paste("ttest_",j,sep=""),t.test(get(j)~tratamento_amostra,data=df_uber,df_uber$Ano=="2013",na.rm = TRUE))
    
  }
  table_ttest=c("Variável","Tratados","Controle","t","p-valor")
  
  for (j in c(1:length(variables))) {
    table_ttest=rbind(table_ttest,
                      c(labels[j],round(get(paste("ttest_",variables[j],sep = ""))$estimate[2],2),round(get(paste("ttest_",variables[j],sep = ""))$estimate[1],2),round(get(paste("ttest_",variables[j],sep = ""))$statistic,2),round(get(paste("ttest_",variables[j],sep = ""))$p.value,2)))
  }
  table_ttest
  write.table(table_ttest,file="02_teste_t.txt",row.names = FALSE, col.names = FALSE,
              quote = FALSE, sep=";") #Transforma tabela em arquivo txt para abrir no Word.
  
  rm(list = ls()[-1])
  
}

###### Gráficos verificação - Tendências paralelas
{
df_uber$Ano=as.numeric(df_uber$Ano)  
df_uber = df_uber%>%
    filter(Ano<=2013)


    
  gvar=c("res_mort_transp_total",	"ocor_mort_transp_total",
         "ocor_internacoes_transp_total","res_internacoes_transp_total",
         "ocor_valor_total","res_valor_total")
  
  glabel=c("Óbitos - Acidentes de transporte (Resid. - 100 mil hab.)","Óbitos - Acidentes de transporte (Ocorr. - 100 mil hab.)",
           "Internações - Acidentes de transporte (Intern. - 100 mil hab.)",	"Internações - Acidentes de transporte (Resid. - 100 mil hab.)",
           "Vlr Internações - Acidentes de transporte (Intern. - 100 mil hab.)",	"Vlr Internações - Acidentes de transporte (Resid. - 100 mil hab.)")
  
  glinha=df_uber%>%
    group_by(tratamento_amostra,Ano)%>%
    summarise_at(gvar,mean,na.rm=TRUE)
  glinha$tratados="Tratados"
  for(i in c(1:nrow(glinha))){
    if(glinha$tratamento_amostra[i]==0){
      glinha$tratados[i]="Controle"
    }
  }
  
  for(i in c(1:length(gvar))){
    assign(gvar[i],ggplot(data=glinha, aes(x=Ano,y=get(gvar[i]),group=tratados))+
             scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))+
             geom_point(aes(shape = tratados)) +
             geom_line()+
             labs(x="Year",y=glabel[i],linetype="",shape="")+
             theme(legend.position = "bottom"))
    ggsave(paste("g_",i,gvar[i],".jpeg",sep = ""),dpi = "retina")
    
  }
  
}


###### Regressões

rm(list = ls())
library(readxl); library(stargazer); library(car);library(plm);library(lmtest);library(car);library(tidyverse);library(pracma);library(sandwich)

setwd("C:/Users/gabri/Google Drive/Arquivos Mono/Dados/0. Saída Programa")


df_uber <- as.data.frame(read_xlsx("df_modelo_V2.xlsx"))

df_uber = pdata.frame(df_uber , index=c("cod_munic","Ano"))
pdim(df_uber)


setwd("C:/Users/gabri/Google Drive/Arquivos Mono/Dados/0. Saída Programa/Saídas Estatística/Modelos/")

###### Ocorrência e internação
{
#### Mortalidade
{
### Totais, faixa etária e sexo
{
mort_ocor_reg1=plm(log(ocor_mort_transp_total+1)~tratamento, data=df_uber)
vcov.mort_ocor_reg1<- vcovHC(mort_ocor_reg1,method ="arellano",type = "sss",cluster = "group")
#coeftest(mort_ocor_reg1,vcov.mort_ocor_reg1)

mort_ocor_reg2=plm(log(ocor_mort_transp_total+1)~tratamento, data=df_uber,model="within",effect="twoways")
vcov.mort_ocor_reg2<- vcovHC(mort_ocor_reg2,method ="arellano",type = "sss",cluster = "group")
#coeftest(mort_ocor_reg2,vcov.mort_ocor_reg2)

mort_ocor_reg3= plm(log(ocor_mort_transp_total+1)~tratamento+log(total_frota)+log(total_leitos)+log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
vcov.mort_ocor_reg3= vcovHC(mort_ocor_reg3,method ="arellano",type = "sss",cluster = "group")
#coeftest(mort_ocor_reg3,vcov.mort_ocor_reg3)

mort_ocor_19= plm(log(ocor_mort_transp_menor20+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
vcov.mort_ocor_19= vcovHC(mort_ocor_19,method ="arellano",type = "sss",cluster = "group")
#coeftest(mort_ocor_19,vcov.mort_ocor_19)

mort_ocor_20= plm(log(ocor_mort_transp_20a29+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
vcov.mort_ocor_20= vcovHC(mort_ocor_20,method ="arellano",type = "sss",cluster = "group")
#coeftest(mort_ocor_20,vcov.mort_ocor_20)

mort_ocor_30= plm(log(ocor_mort_transp_30a39+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
vcov.mort_ocor_30= vcovHC(mort_ocor_30,method ="arellano",type = "sss",cluster = "group")
#coeftest(mort_ocor_30,vcov.mort_ocor_30)

mort_ocor_40= plm(log(ocor_mort_transp_40mais+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
vcov.mort_ocor_40= vcovHC(mort_ocor_40,method ="arellano",type = "sss",cluster = "group")
#coeftest(mort_ocor_40,vcov.mort_ocor_40)

mort_ocor_fem= plm(log(ocor_mort_transp_fem+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
vcov.mort_ocor_fem= vcovHC(mort_ocor_fem,method ="arellano",type = "sss",cluster = "group")
#coeftest(mort_ocor_fem,vcov.mort_ocor_fem)

mort_ocor_masc= plm(log(ocor_mort_transp_masc+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
vcov.mort_ocor_masc= vcovHC(mort_ocor_masc,method ="arellano",type = "sss",cluster = "group")
#coeftest(mort_ocor_masc,vcov.mort_ocor_masc)


stargazer(mort_ocor_reg1,mort_ocor_reg2,mort_ocor_reg3,mort_ocor_19,mort_ocor_20,mort_ocor_30,mort_ocor_40,mort_ocor_fem,mort_ocor_masc,
          keep=c("tratamento","total_frota","total_leitos","total_medicos","salario_medio_def","nv_tecno"),
          dep.var.labels = c("Total","<20","20-29","30-39",">40","Fem.","Masc."),
          covariate.labels=c("Uber","Frota","Leitos","Médicos","Salário","VA Urbano"),
          se=list(sqrt(diag(vcov.mort_ocor_reg1)),sqrt(diag(vcov.mort_ocor_reg2)),sqrt(diag(vcov.mort_ocor_reg3)),
                  sqrt(diag(vcov.mort_ocor_19)),sqrt(diag(vcov.mort_ocor_20)),sqrt(diag(vcov.mort_ocor_30)),
                  sqrt(diag(vcov.mort_ocor_40)),sqrt(diag(vcov.mort_ocor_fem)),sqrt(diag(vcov.mort_ocor_masc))),
          add.lines = list(c("Efeitos Fixos","Não","Sim","Sim","Sim","Sim","Sim","Sim","Sim","Sim"),
                           c("Tendências","Não","Não","Sim","Sim","Sim","Sim","Sim","Sim","Sim")),
          title = c("Efeitos do Uber Sobre Óbitos por 100 mil Habitantes (Local de Ocorrência)"),
          notes.label = "Notas", notes = c("Desvios padrões agrupados por municípios. Variável dependente separada por grupos etários e sexo. Controles: frota de veículos por mil habitantes, leitos hospitalares por mil habitantes, médicos por mil habitantes, salário médio, percentual valor adicionado urbano (indústria, comércio, serviços e administração pública)."),
          type="text", decimal.mark = "," , digit.separator = ".", style ="apsr" ,keep.stat=c("n", "rsq"),df=FALSE, digits=3, out = "03_reg_mortalidade_1.html")#, out = "03_reg_mortalidade_1.html"
}
### Heterogeneidade por capitais, regiões e estados
{
mort_ocor_capital_reg3= plm(log(ocor_mort_transp_total+1)~tratamento*capital+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
vcov.mort_ocor_capital_reg3= vcovHC(mort_ocor_capital_reg3,method ="arellano",type = "sss",cluster = "group")
coeftest(mort_ocor_capital_reg3,vcov.mort_ocor_capital_reg3)

mort_ocor_regiao_reg3= plm(log(ocor_mort_transp_total+1)~tratamento*regiao+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
vcov.mort_ocor_regiao_reg3= vcovHC(mort_ocor_regiao_reg3,method ="arellano",type = "sss",cluster = "group")
coeftest(mort_ocor_regiao_reg3,vcov.mort_ocor_regiao_reg3)

stargazer(mort_ocor_capital_reg3,mort_ocor_regiao_reg3,keep=c("tratamento1"),
          dep.var.labels=c("Óbitos-Residência","Óbitos-Ocorrência"),
          covariate.labels=c("Uber","Uber x Capital","Uber x NE","Uber x N","Uber x SE","Uber x S"),
          se=list(sqrt(diag(vcov.mort_res_capital_reg3)),sqrt(diag(vcov.mort_res_regiao_reg3)),sqrt(diag(vcov.mort_ocor_capital_reg3)),sqrt(diag(vcov.mort_ocor_regiao_reg3))),
          title = c("Variável Dependente: Óbitos por 100 mil Habitantes"),
          notes.label = "Notas", notes = c("Desvios padrões agrupados por municípios.",
                                           "Controles: frota de veículos por mil habitantes, leitos hospitalares por mil habitantes, médicos por mil habitantes, salário médio, percentual valor adicionado urbano (indústria, comércio, serviços e administração pública), efeitos fixos para municípios e anos, tendências lineares por municípios."),
          type="text", decimal.mark = "," , digit.separator = ".", style = "jpam",keep.stat=c("n", "rsq"),
          model.names = FALSE, model.numbers = TRUE, df=FALSE, digits=3,out="06_reg_mortalidade_regiao_1.html")
}
}
#### Internações
{
### Totais, faixa etária e sexo
{
internacoes_ocor_reg1=plm(log(ocor_internacoes_transp_total+1)~tratamento, data=df_uber)
vcov.internacoes_ocor_reg1<- vcovHC(internacoes_ocor_reg1,method ="arellano",type = "sss",cluster = "group")
#coeftest(internacoes_ocor_reg1,vcov.internacoes_ocor_reg1)

internacoes_ocor_reg2=plm(log(ocor_internacoes_transp_total+1)~tratamento, data=df_uber,model="within",effect="twoways")
vcov.internacoes_ocor_reg2<- vcovHC(internacoes_ocor_reg2,method ="arellano",type = "sss",cluster = "group")
#coeftest(internacoes_ocor_reg2,vcov.internacoes_ocor_reg2)

internacoes_ocor_reg3= plm(log(ocor_internacoes_transp_total+1)~tratamento+log(total_frota)+log(total_leitos)+log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
vcov.internacoes_ocor_reg3= vcovHC(internacoes_ocor_reg3,method ="arellano",type = "sss",cluster = "group")
#coeftest(internacoes_ocor_reg3,vcov.internacoes_ocor_reg3)

internacoes_ocor_19= plm(log(ocor_internacoes_transp_menor20+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
vcov.internacoes_ocor_19= vcovHC(internacoes_ocor_19,method ="arellano",type = "sss",cluster = "group")
#coeftest(internacoes_ocor_19,vcov.internacoes_ocor_19)

internacoes_ocor_20= plm(log(ocor_internacoes_transp_20a29+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
vcov.internacoes_ocor_20= vcovHC(internacoes_ocor_20,method ="arellano",type = "sss",cluster = "group")
#coeftest(internacoes_ocor_20,vcov.internacoes_ocor_20)

internacoes_ocor_30= plm(log(ocor_internacoes_transp_30a39+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
vcov.internacoes_ocor_30= vcovHC(internacoes_ocor_30,method ="arellano",type = "sss",cluster = "group")
#coeftest(internacoes_ocor_30,vcov.internacoes_ocor_30)

internacoes_ocor_40= plm(log(ocor_internacoes_transp_40mais+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
vcov.internacoes_ocor_40= vcovHC(internacoes_ocor_40,method ="arellano",type = "sss",cluster = "group")
#coeftest(internacoes_ocor_40,vcov.internacoes_ocor_40)

internacoes_ocor_fem= plm(log(ocor_internacoes_transp_fem+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
vcov.internacoes_ocor_fem= vcovHC(internacoes_ocor_fem,method ="arellano",type = "sss",cluster = "group")
#coeftest(internacoes_ocor_fem,vcov.internacoes_ocor_fem)

internacoes_ocor_masc= plm(log(ocor_internacoes_transp_masc+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
vcov.internacoes_ocor_masc= vcovHC(internacoes_ocor_masc,method ="arellano",type = "sss",cluster = "group")
#coeftest(internacoes_ocor_masc,vcov.internacoes_ocor_masc)

stargazer(internacoes_ocor_reg1,internacoes_ocor_reg2,internacoes_ocor_reg3,internacoes_ocor_19,internacoes_ocor_20,internacoes_ocor_30,internacoes_ocor_40,internacoes_ocor_fem,internacoes_ocor_masc,
          keep=c("tratamento","total_frota","total_leitos","total_medicos","salario_medio_def","nv_tecno"),
  dep.var.labels=c("Total","<20","20-29","30-39",">40","Fem.","Masc."),
  covariate.labels=c("Uber","Frota","Leitos","Médicos","Salário","VA Urbano"),
  se=list(sqrt(diag(vcov.internacoes_ocor_reg1)),sqrt(diag(vcov.internacoes_ocor_reg2)),sqrt(diag(vcov.internacoes_ocor_reg3)),
          sqrt(diag(vcov.internacoes_ocor_19)),sqrt(diag(vcov.internacoes_ocor_20)),sqrt(diag(vcov.internacoes_ocor_30)),
          sqrt(diag(vcov.internacoes_ocor_40)),sqrt(diag(vcov.internacoes_ocor_fem)),sqrt(diag(vcov.internacoes_ocor_masc))),
  add.lines = list(c("Efeitos Fixos","Não","Sim","Sim","Sim","Sim","Sim","Sim","Sim","Sim"),c("Tendências","Não","Não","Sim","Sim","Sim","Sim","Sim","Sim","Sim")),
  title = c("Efeitos do Uber Sobre Internações por 100 mil Habitantes (Local de Internação)"),
  notes.label = "Notas", notes = c("Desvios padrões agrupados por municípios. Variável dependente separada por grupos etários e sexo. Controles: frota de veículos por mil habitantes, leitos hospitalares por mil habitantes, médicos por mil habitantes, salário médio, percentual valor adicionado urbano (indústria, comércio, serviços e administração pública)."),
  type="text", decimal.mark = "," , digit.separator = ".", style = "apsr",keep.stat=c("n", "rsq"),
  model.names = FALSE, model.numbers = TRUE, df=FALSE, digits=3, out="04_reg_internacoes_total_1.html")
  }
### Heterogeneidade por capitais, regiões e estados
{

internacoes_ocor_capital_reg3= plm(log(ocor_internacoes_transp_total+1)~tratamento*capital+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
vcov.internacoes_ocor_capital_reg3= vcovHC(internacoes_ocor_capital_reg3,method ="arellano",type = "sss",cluster = "group")
coeftest(internacoes_ocor_capital_reg3,vcov.internacoes_ocor_capital_reg3)

internacoes_ocor_regiao_reg3= plm(log(ocor_internacoes_transp_total+1)~tratamento*regiao+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
vcov.internacoes_ocor_regiao_reg3= vcovHC(internacoes_ocor_regiao_reg3,method ="arellano",type = "sss",cluster = "group")
coeftest(internacoes_ocor_regiao_reg3,vcov.internacoes_ocor_regiao_reg3)

stargazer(internacoes_res_capital_reg3,internacoes_res_regiao_reg3,internacoes_ocor_capital_reg3,internacoes_ocor_regiao_reg3,keep=c("tratamento1"),
  dep.var.labels=c("Internações - Residência","Internações - Local de Internação"),
  covariate.labels=c("Uber","Uber x Capital","Uber x NE","Uber x N","Uber x SE","Uber x S"),
  se=list(sqrt(diag(vcov.internacoes_res_capital_reg3)),sqrt(diag(vcov.internacoes_res_regiao_reg3)),sqrt(diag(vcov.internacoes_ocor_capital_reg3)),sqrt(diag(vcov.internacoes_ocor_regiao_reg3))),
  title = c("Variável Dependente: Internação por 100 mil Habitantes"),
  notes.label = "Notas", notes = c("Desvios padrões agrupados por municípios.",
   "Controles: frota de veículos por mil habitantes, leitos hospitalares por mil habitantes, médicos por mil habitantes, salário médio, percentual valor adicionado urbano (indústria, comércio, serviços e administração pública), efeitos fixos para municípios e anos, tendências lineares por municípios."),
  type="text", decimal.mark = "," , digit.separator = ".", style = "jpam",keep.stat=c("n", "rsq"),
  model.names = FALSE, model.numbers = TRUE, df=FALSE, digits=3,out="10_reg_internacoes_regiao_1.html")
  }
}
#### Valor Total de Internação
{
### Totais, faixa etária e sexo
{
valor_ocor_reg1=plm(log(ocor_valor_total+1)~tratamento, data=df_uber)
vcov.valor_ocor_reg1<- vcovHC(valor_ocor_reg1,method ="arellano",type = "sss",cluster = "group")
#coeftest(valor_ocor_reg1,vcov.valor_ocor_reg1)

valor_ocor_reg2=plm(log(ocor_valor_total+1)~tratamento, data=df_uber,model="within",effect="twoways")
vcov.valor_ocor_reg2<- vcovHC(valor_ocor_reg2,method ="arellano",type = "sss",cluster = "group")
#coeftest(valor_ocor_reg2,vcov.valor_ocor_reg2)

valor_ocor_reg3= plm(log(ocor_valor_total+1)~tratamento+log(total_frota)+log(total_leitos)+log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
vcov.valor_ocor_reg3= vcovHC(valor_ocor_reg3,method ="arellano",type = "sss",cluster = "group")
#coeftest(valor_ocor_reg3,vcov.valor_ocor_reg3)

valor_ocor_19= plm(log(ocor_valor_menor20+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
vcov.valor_ocor_19= vcovHC(valor_ocor_19,method ="arellano",type = "sss",cluster = "group")
#coeftest(valor_ocor_19,vcov.valor_ocor_19)

valor_ocor_20= plm(log(ocor_valor_20a29+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
vcov.valor_ocor_20= vcovHC(valor_ocor_20,method ="arellano",type = "sss",cluster = "group")
#coeftest(valor_ocor_20,vcov.valor_ocor_20)

valor_ocor_30= plm(log(ocor_valor_30a39+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
vcov.valor_ocor_30= vcovHC(valor_ocor_30,method ="arellano",type = "sss",cluster = "group")
#coeftest(valor_ocor_30,vcov.valor_ocor_30)

valor_ocor_40= plm(log(ocor_valor_40mais+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
vcov.valor_ocor_40= vcovHC(valor_ocor_40,method ="arellano",type = "sss",cluster = "group")
#coeftest(valor_ocor_40,vcov.valor_ocor_40)

valor_ocor_fem= plm(log(ocor_valor_fem+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
vcov.valor_ocor_fem= vcovHC(valor_ocor_fem,method ="arellano",type = "sss",cluster = "group")
#coeftest(valor_ocor_fem,vcov.valor_ocor_fem)

valor_ocor_masc= plm(log(ocor_valor_masc+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
vcov.valor_ocor_masc= vcovHC(valor_ocor_masc,method ="arellano",type = "sss",cluster = "group")
#coeftest(valor_ocor_masc,vcov.valor_ocor_masc)

stargazer(valor_ocor_reg1,valor_ocor_reg2,valor_ocor_reg3,valor_ocor_19,valor_ocor_20,valor_ocor_30,valor_ocor_40,valor_ocor_fem,valor_ocor_masc,
          keep=c("tratamento","total_frota","total_leitos","total_medicos","salario_medio_def","nv_tecno"),
          dep.var.labels=c("Total","<20","20-29","30-39",">40","Fem.","Masc."),
          covariate.labels=c("Uber","Frota","Leitos","Médicos","Salário","VA Urbano"),
          se=list(sqrt(diag(vcov.valor_ocor_reg1)),sqrt(diag(vcov.valor_ocor_reg2)),sqrt(diag(vcov.valor_ocor_reg3)),
                  sqrt(diag(vcov.valor_ocor_19)),sqrt(diag(vcov.valor_ocor_20)),sqrt(diag(vcov.valor_ocor_30)),
                  sqrt(diag(vcov.valor_ocor_40)),sqrt(diag(vcov.valor_ocor_fem)),sqrt(diag(vcov.valor_ocor_masc))),
          add.lines = list(c("Efeitos Fixos","Não","Sim","Sim","Sim","Sim","Sim","Sim","Sim","Sim"),
                           c("Tendências","Não","Não","Sim","Sim","Sim","Sim","Sim","Sim","Sim")),
          title = c("Efeitos do Uber Sobre Valor Total de Internações por 100 mil Habitantes (Local de Internação)"),
          notes.label = "Notas", notes = c("Desvios padrões agrupados por municípios. Variável dependente separada por grupos etários e sexo. Controles: frota de veículos por mil habitantes, leitos hospitalares por mil habitantes, médicos por mil habitantes, salário médio, percentual valor adicionado urbano (indústria, comércio, serviços e administração pública)."),
          type="text", decimal.mark = "," , digit.separator = ".", style = "apsr",keep.stat=c("n", "rsq"),
          model.names = FALSE, model.numbers = TRUE, df=FALSE, digits=3, out="5_reg_valor_total_1.html")
}
### Heterogeneidade por capitais, regiões e estados
{
mort_ocor_capital_reg3= plm(log(ocor_mort_transp_total+1)~tratamento*capital+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
vcov.mort_ocor_capital_reg3= vcovHC(mort_ocor_capital_reg3,method ="arellano",type = "sss",cluster = "group")
#coeftest(mort_ocor_capital_reg3,vcov.mort_ocor_capital_reg3)

mort_ocor_regiao_reg3= plm(log(ocor_mort_transp_total+1)~tratamento*regiao+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
vcov.mort_ocor_regiao_reg3= vcovHC(mort_ocor_regiao_reg3,method ="arellano",type = "sss",cluster = "group")
#coeftest(mort_ocor_regiao_reg3,vcov.mort_ocor_regiao_reg3)

internacoes_ocor_capital_reg3= plm(log(ocor_internacoes_transp_total+1)~tratamento*capital+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
vcov.internacoes_ocor_capital_reg3= vcovHC(internacoes_ocor_capital_reg3,method ="arellano",type = "sss",cluster = "group")
#coeftest(internacoes_ocor_capital_reg3,vcov.internacoes_ocor_capital_reg3)

internacoes_ocor_regiao_reg3= plm(log(ocor_internacoes_transp_total+1)~tratamento*regiao+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
vcov.internacoes_ocor_regiao_reg3= vcovHC(internacoes_ocor_regiao_reg3,method ="arellano",type = "sss",cluster = "group")
#coeftest(internacoes_ocor_regiao_reg3,vcov.internacoes_ocor_regiao_reg3)

valor_ocor_capital_reg3= plm(log(ocor_valor_total+1)~tratamento*capital+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
vcov.valor_ocor_capital_reg3= vcovHC(valor_ocor_capital_reg3,method ="arellano",type = "sss",cluster = "group")
#coeftest(valor_ocor_capital_reg3,vcov.valor_ocor_capital_reg3)

valor_ocor_regiao_reg3= plm(log(ocor_valor_total+1)~tratamento*regiao+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
vcov.valor_ocor_regiao_reg3= vcovHC(valor_ocor_regiao_reg3,method ="arellano",type = "sss",cluster = "group")
#coeftest(valor_ocor_regiao_reg3,vcov.valor_ocor_regiao_reg3)


stargazer(mort_ocor_capital_reg3,mort_ocor_regiao_reg3,internacoes_ocor_capital_reg3,internacoes_ocor_regiao_reg3,valor_ocor_capital_reg3,valor_ocor_regiao_reg3,
          keep=c("tratamento1"),dep.var.labels=c("Óbitos","Internações","Valor Internações "),
          covariate.labels=c("Uber","Uber x Capital","Uber x NE","Uber x N","Uber x SE","Uber x S"),
          se=list(sqrt(diag(vcov.mort_ocor_capital_reg3)),sqrt(diag(vcov.mort_ocor_regiao_reg3)),
                  sqrt(diag(vcov.internacoes_ocor_capital_reg3)),sqrt(diag(vcov.internacoes_ocor_regiao_reg3)),
                  sqrt(diag(vcov.valor_ocor_capital_reg3)),sqrt(diag(vcov.valor_ocor_regiao_reg3))),
          title = c("Efeitos do Uber por Capitais e Regiões"),notes.label = "Notas",
          notes = c("Desvios padrões agrupados por municípios. Variável dependente separada por grupos etários e sexo. Controles: frota de veículos por mil habitantes, leitos hospitalares por mil habitantes, médicos por mil habitantes, salário médio, percentual valor adicionado urbano (indústria, comércio, serviços e administração pública)."),
          type="text", decimal.mark = "," , digit.separator = ".", style = "apsr",keep.stat=c("n", "rsq"),
          model.names = FALSE, model.numbers = TRUE, df=FALSE, digits=3,out="6_reg_valor_regiao_1.html")
}
}
}

#### Local de Residência
{
#### Mortalidade
{
  ### Totais, faixa etária e sexo
  {
    mort_res_reg1=plm(log(res_mort_transp_total+1)~tratamento, data=df_uber)
    vcov.mort_res_reg1<- vcovHC(mort_res_reg1,method ="arellano",type = "sss",cluster = "group")
    #coeftest(mort_res_reg1,vcov.mort_res_reg1)
    
    mort_res_reg2=plm(log(res_mort_transp_total+1)~tratamento, data=df_uber,model="within",effect="twoways")
    vcov.mort_res_reg2<- vcovHC(mort_res_reg2,method ="arellano",type = "sss",cluster = "group")
    #coeftest(mort_res_reg2,vcov.mort_res_reg2)
    
    mort_res_reg3= plm(log(res_mort_transp_total+1)~tratamento+log(total_frota)+log(total_leitos)+log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
    vcov.mort_res_reg3= vcovHC(mort_res_reg3,method ="arellano",type = "sss",cluster = "group")
    #coeftest(mort_res_reg3,vcov.mort_res_reg3)
    
    mort_res_19= plm(log(res_mort_transp_menor20+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
    vcov.mort_res_19= vcovHC(mort_res_19,method ="arellano",type = "sss",cluster = "group")
    #coeftest(mort_res_19,vcov.mort_res_19)
    
    mort_res_20= plm(log(res_mort_transp_20a29+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
    vcov.mort_res_20= vcovHC(mort_res_20,method ="arellano",type = "sss",cluster = "group")
    #coeftest(mort_res_20,vcov.mort_res_20)
    
    mort_res_30= plm(log(res_mort_transp_30a39+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
    vcov.mort_res_30= vcovHC(mort_res_30,method ="arellano",type = "sss",cluster = "group")
    #coeftest(mort_res_30,vcov.mort_res_30)
    
    mort_res_40= plm(log(res_mort_transp_40mais+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
    vcov.mort_res_40= vcovHC(mort_res_40,method ="arellano",type = "sss",cluster = "group")
    #coeftest(mort_res_40,vcov.mort_res_40)
    
    mort_res_fem= plm(log(res_mort_transp_fem+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
    vcov.mort_res_fem= vcovHC(mort_res_fem,method ="arellano",type = "sss",cluster = "group")
    #coeftest(mort_res_fem,vcov.mort_res_fem)
    
    mort_res_masc= plm(log(res_mort_transp_masc+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
    vcov.mort_res_masc= vcovHC(mort_res_masc,method ="arellano",type = "sss",cluster = "group")
    #coeftest(mort_res_masc,vcov.mort_res_masc)
    
    
    stargazer(mort_res_reg1,mort_res_reg2,mort_res_reg3,mort_res_19,mort_res_20,mort_res_30,mort_res_40,mort_res_fem,mort_res_masc,
              keep=c("tratamento","total_frota","total_leitos","total_medicos","salario_medio_def","nv_tecno"),
              dep.var.labels = c("Total","<20","20-29","30-39",">40","Fem.","Masc."),
              covariate.labels=c("Uber","Frota","Leitos","Médicos","Salário","VA Urbano"),
              se=list(sqrt(diag(vcov.mort_res_reg1)),sqrt(diag(vcov.mort_res_reg2)),sqrt(diag(vcov.mort_res_reg3)),
                      sqrt(diag(vcov.mort_res_19)),sqrt(diag(vcov.mort_res_20)),sqrt(diag(vcov.mort_res_30)),
                      sqrt(diag(vcov.mort_res_40)),sqrt(diag(vcov.mort_res_fem)),sqrt(diag(vcov.mort_res_masc))),
              add.lines = list(c("Efeitos Fixos","Não","Sim","Sim","Sim","Sim","Sim","Sim","Sim","Sim"),
                               c("Tendências","Não","Não","Sim","Sim","Sim","Sim","Sim","Sim","Sim")),
              title = c("Efeitos do Uber Sobre Óbitos por 100 mil Habitantes (Local de Residência)"),
              notes.label = "Notas", notes = c("Desvios padrões agrupados por municípios. Variável dependente separada por grupos etários e sexo. Controles: frota de veículos por mil habitantes, leitos hospitalares por mil habitantes, médicos por mil habitantes, salário médio, percentual valor adicionado urbano (indústria, comércio, serviços e administração pública)."),
              type="text", decimal.mark = "," , digit.separator = ".", style ="apsr" ,keep.stat=c("n", "rsq"),df=FALSE, digits=3, out = "03_reg_mortalidade_1_res.html")#, out = "03_reg_mortalidade_1.html"
  }
  ### Heterogeneidade por capitais, regiões e estados
  {
    mort_res_capital_reg3= plm(log(res_mort_transp_total+1)~tratamento*capital+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
    vcov.mort_res_capital_reg3= vcovHC(mort_res_capital_reg3,method ="arellano",type = "sss",cluster = "group")
    coeftest(mort_res_capital_reg3,vcov.mort_res_capital_reg3)
    
    mort_res_regiao_reg3= plm(log(res_mort_transp_total+1)~tratamento*regiao+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
    vcov.mort_res_regiao_reg3= vcovHC(mort_res_regiao_reg3,method ="arellano",type = "sss",cluster = "group")
    coeftest(mort_res_regiao_reg3,vcov.mort_res_regiao_reg3)
    
    stargazer(mort_res_capital_reg3,mort_res_regiao_reg3,keep=c("tratamento1"),
              dep.var.labels=c("Óbitos-Residência","Óbitos-reência"),
              covariate.labels=c("Uber","Uber x Capital","Uber x NE","Uber x N","Uber x SE","Uber x S"),
              se=list(sqrt(diag(vcov.mort_res_capital_reg3)),sqrt(diag(vcov.mort_res_regiao_reg3)),sqrt(diag(vcov.mort_res_capital_reg3)),sqrt(diag(vcov.mort_res_regiao_reg3))),
              title = c("Variável Dependente: Óbitos por 100 mil Habitantes"),
              notes.label = "Notas", notes = c("Desvios padrões agrupados por municípios.",
                                               "Controles: frota de veículos por mil habitantes, leitos hospitalares por mil habitantes, médicos por mil habitantes, salário médio, percentual valor adicionado urbano (indústria, comércio, serviços e administração pública), efeitos fixos para municípios e anos, tendências lineares por municípios."),
              type="text", decimal.mark = "," , digit.separator = ".", style = "jpam",keep.stat=c("n", "rsq"),
              model.names = FALSE, model.numbers = TRUE, df=FALSE, digits=3,out="06_reg_mortalidade_regiao_1_res.html")
  }
}
#### Internações
{
  ### Totais, faixa etária e sexo
  {
    internacoes_res_reg1=plm(log(res_internacoes_transp_total+1)~tratamento, data=df_uber)
    vcov.internacoes_res_reg1<- vcovHC(internacoes_res_reg1,method ="arellano",type = "sss",cluster = "group")
    #coeftest(internacoes_res_reg1,vcov.internacoes_res_reg1)
    
    internacoes_res_reg2=plm(log(res_internacoes_transp_total+1)~tratamento, data=df_uber,model="within",effect="twoways")
    vcov.internacoes_res_reg2<- vcovHC(internacoes_res_reg2,method ="arellano",type = "sss",cluster = "group")
    #coeftest(internacoes_res_reg2,vcov.internacoes_res_reg2)
    
    internacoes_res_reg3= plm(log(res_internacoes_transp_total+1)~tratamento+log(total_frota)+log(total_leitos)+log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
    vcov.internacoes_res_reg3= vcovHC(internacoes_res_reg3,method ="arellano",type = "sss",cluster = "group")
    #coeftest(internacoes_res_reg3,vcov.internacoes_res_reg3)
    
    internacoes_res_19= plm(log(res_internacoes_transp_menor20+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
    vcov.internacoes_res_19= vcovHC(internacoes_res_19,method ="arellano",type = "sss",cluster = "group")
    #coeftest(internacoes_res_19,vcov.internacoes_res_19)
    
    internacoes_res_20= plm(log(res_internacoes_transp_20a29+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
    vcov.internacoes_res_20= vcovHC(internacoes_res_20,method ="arellano",type = "sss",cluster = "group")
    #coeftest(internacoes_res_20,vcov.internacoes_res_20)
    
    internacoes_res_30= plm(log(res_internacoes_transp_30a39+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
    vcov.internacoes_res_30= vcovHC(internacoes_res_30,method ="arellano",type = "sss",cluster = "group")
    #coeftest(internacoes_res_30,vcov.internacoes_res_30)
    
    internacoes_res_40= plm(log(res_internacoes_transp_40mais+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
    vcov.internacoes_res_40= vcovHC(internacoes_res_40,method ="arellano",type = "sss",cluster = "group")
    #coeftest(internacoes_res_40,vcov.internacoes_res_40)
    
    internacoes_res_fem= plm(log(res_internacoes_transp_fem+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
    vcov.internacoes_res_fem= vcovHC(internacoes_res_fem,method ="arellano",type = "sss",cluster = "group")
    #coeftest(internacoes_res_fem,vcov.internacoes_res_fem)
    
    internacoes_res_masc= plm(log(res_internacoes_transp_masc+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
    vcov.internacoes_res_masc= vcovHC(internacoes_res_masc,method ="arellano",type = "sss",cluster = "group")
    #coeftest(internacoes_res_masc,vcov.internacoes_res_masc)
    
    stargazer(internacoes_res_reg1,internacoes_res_reg2,internacoes_res_reg3,internacoes_res_19,internacoes_res_20,internacoes_res_30,internacoes_res_40,internacoes_res_fem,internacoes_res_masc,
              keep=c("tratamento","total_frota","total_leitos","total_medicos","salario_medio_def","nv_tecno"),
              dep.var.labels=c("Total","<20","20-29","30-39",">40","Fem.","Masc."),
              covariate.labels=c("Uber","Frota","Leitos","Médicos","Salário","VA Urbano"),
              se=list(sqrt(diag(vcov.internacoes_res_reg1)),sqrt(diag(vcov.internacoes_res_reg2)),sqrt(diag(vcov.internacoes_res_reg3)),
                      sqrt(diag(vcov.internacoes_res_19)),sqrt(diag(vcov.internacoes_res_20)),sqrt(diag(vcov.internacoes_res_30)),
                      sqrt(diag(vcov.internacoes_res_40)),sqrt(diag(vcov.internacoes_res_fem)),sqrt(diag(vcov.internacoes_res_masc))),
              add.lines = list(c("Efeitos Fixos","Não","Sim","Sim","Sim","Sim","Sim","Sim","Sim","Sim"),c("Tendências","Não","Não","Sim","Sim","Sim","Sim","Sim","Sim","Sim")),
              title = c("Efeitos do Uber Sobre Internações por 100 mil Habitantes (Local de Residência)"),
              notes.label = "Notas", notes = c("Desvios padrões agrupados por municípios. Variável dependente separada por grupos etários e sexo. Controles: frota de veículos por mil habitantes, leitos hospitalares por mil habitantes, médicos por mil habitantes, salário médio, percentual valor adicionado urbano (indústria, comércio, serviços e administração pública)."),
              type="text", decimal.mark = "," , digit.separator = ".", style = "apsr",keep.stat=c("n", "rsq"),
              model.names = FALSE, model.numbers = TRUE, df=FALSE, digits=3, out="04_reg_internacoes_total_1_res.html")
  }
  ### Heterogeneidade por capitais, regiões e estados
  {
    
    internacoes_res_capital_reg3= plm(log(res_internacoes_transp_total+1)~tratamento*capital+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
    vcov.internacoes_res_capital_reg3= vcovHC(internacoes_res_capital_reg3,method ="arellano",type = "sss",cluster = "group")
    coeftest(internacoes_res_capital_reg3,vcov.internacoes_res_capital_reg3)
    
    internacoes_res_regiao_reg3= plm(log(res_internacoes_transp_total+1)~tratamento*regiao+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
    vcov.internacoes_res_regiao_reg3= vcovHC(internacoes_res_regiao_reg3,method ="arellano",type = "sss",cluster = "group")
    coeftest(internacoes_res_regiao_reg3,vcov.internacoes_res_regiao_reg3)
    
    stargazer(internacoes_res_capital_reg3,internacoes_res_regiao_reg3,internacoes_res_capital_reg3,internacoes_res_regiao_reg3,keep=c("tratamento1"),
              dep.var.labels=c("Internações - Residência","Internações - Local de Internação"),
              covariate.labels=c("Uber","Uber x Capital","Uber x NE","Uber x N","Uber x SE","Uber x S"),
              se=list(sqrt(diag(vcov.internacoes_res_capital_reg3)),sqrt(diag(vcov.internacoes_res_regiao_reg3)),sqrt(diag(vcov.internacoes_res_capital_reg3)),sqrt(diag(vcov.internacoes_res_regiao_reg3))),
              title = c("Variável Dependente: Internação por 100 mil Habitantes"),
              notes.label = "Notas", notes = c("Desvios padrões agrupados por municípios.",
                                               "Controles: frota de veículos por mil habitantes, leitos hospitalares por mil habitantes, médicos por mil habitantes, salário médio, percentual valor adicionado urbano (indústria, comércio, serviços e administração pública), efeitos fixos para municípios e anos, tendências lineares por municípios."),
              type="text", decimal.mark = "," , digit.separator = ".", style = "jpam",keep.stat=c("n", "rsq"),
              model.names = FALSE, model.numbers = TRUE, df=FALSE, digits=3,out="10_reg_internacoes_regiao_1_res.html")
  }
}
#### Valor Total de Internação
{
  ### Totais, faixa etária e sexo
  {
    valor_res_reg1=plm(log(res_valor_total+1)~tratamento, data=df_uber)
    vcov.valor_res_reg1<- vcovHC(valor_res_reg1,method ="arellano",type = "sss",cluster = "group")
    #coeftest(valor_res_reg1,vcov.valor_res_reg1)
    
    valor_res_reg2=plm(log(res_valor_total+1)~tratamento, data=df_uber,model="within",effect="twoways")
    vcov.valor_res_reg2<- vcovHC(valor_res_reg2,method ="arellano",type = "sss",cluster = "group")
    #coeftest(valor_res_reg2,vcov.valor_res_reg2)
    
    valor_res_reg3= plm(log(res_valor_total+1)~tratamento+log(total_frota)+log(total_leitos)+log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
    vcov.valor_res_reg3= vcovHC(valor_res_reg3,method ="arellano",type = "sss",cluster = "group")
    #coeftest(valor_res_reg3,vcov.valor_res_reg3)
    
    valor_res_19= plm(log(res_valor_menor20+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
    vcov.valor_res_19= vcovHC(valor_res_19,method ="arellano",type = "sss",cluster = "group")
    #coeftest(valor_res_19,vcov.valor_res_19)
    
    valor_res_20= plm(log(res_valor_20a29+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
    vcov.valor_res_20= vcovHC(valor_res_20,method ="arellano",type = "sss",cluster = "group")
    #coeftest(valor_res_20,vcov.valor_res_20)
    
    valor_res_30= plm(log(res_valor_30a39+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
    vcov.valor_res_30= vcovHC(valor_res_30,method ="arellano",type = "sss",cluster = "group")
    #coeftest(valor_res_30,vcov.valor_res_30)
    
    valor_res_40= plm(log(res_valor_40mais+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
    vcov.valor_res_40= vcovHC(valor_res_40,method ="arellano",type = "sss",cluster = "group")
    #coeftest(valor_res_40,vcov.valor_res_40)
    
    valor_res_fem= plm(log(res_valor_fem+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
    vcov.valor_res_fem= vcovHC(valor_res_fem,method ="arellano",type = "sss",cluster = "group")
    #coeftest(valor_res_fem,vcov.valor_res_fem)
    
    valor_res_masc= plm(log(res_valor_masc+1)~tratamento+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
    vcov.valor_res_masc= vcovHC(valor_res_masc,method ="arellano",type = "sss",cluster = "group")
    #coeftest(valor_res_masc,vcov.valor_res_masc)
    
    stargazer(valor_res_reg1,valor_res_reg2,valor_res_reg3,valor_res_19,valor_res_20,valor_res_30,valor_res_40,valor_res_fem,valor_res_masc,
              keep=c("tratamento","total_frota","total_leitos","total_medicos","salario_medio_def","nv_tecno"),
              dep.var.labels=c("Total","<20","20-29","30-39",">40","Fem.","Masc."),
              covariate.labels=c("Uber","Frota","Leitos","Médicos","Salário","VA Urbano"),
              se=list(sqrt(diag(vcov.valor_res_reg1)),sqrt(diag(vcov.valor_res_reg2)),sqrt(diag(vcov.valor_res_reg3)),
                      sqrt(diag(vcov.valor_res_19)),sqrt(diag(vcov.valor_res_20)),sqrt(diag(vcov.valor_res_30)),
                      sqrt(diag(vcov.valor_res_40)),sqrt(diag(vcov.valor_res_fem)),sqrt(diag(vcov.valor_res_masc))),
              add.lines = list(c("Efeitos Fixos","Não","Sim","Sim","Sim","Sim","Sim","Sim","Sim","Sim"),
                               c("Tendências","Não","Não","Sim","Sim","Sim","Sim","Sim","Sim","Sim")),
              title = c("Efeitos do Uber Sobre Valor Total de Internações por 100 mil Habitantes (Local de Residência)"),
              notes.label = "Notas", notes = c("Desvios padrões agrupados por municípios. Variável dependente separada por grupos etários e sexo. Controles: frota de veículos por mil habitantes, leitos hospitalares por mil habitantes, médicos por mil habitantes, salário médio, percentual valor adicionado urbano (indústria, comércio, serviços e administração pública)."),
              type="text", decimal.mark = "," , digit.separator = ".", style = "apsr",keep.stat=c("n", "rsq"),
              model.names = FALSE, model.numbers = TRUE, df=FALSE, digits=3, out="5_reg_valor_total_1_res.html")
  }
  ### Heterogeneidade por capitais, regiões e estados
  {
    mort_res_capital_reg3= plm(log(res_mort_transp_total+1)~tratamento*capital+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
    vcov.mort_res_capital_reg3= vcovHC(mort_res_capital_reg3,method ="arellano",type = "sss",cluster = "group")
    #coeftest(mort_res_capital_reg3,vcov.mort_res_capital_reg3)
    
    mort_res_regiao_reg3= plm(log(res_mort_transp_total+1)~tratamento*regiao+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
    vcov.mort_res_regiao_reg3= vcovHC(mort_res_regiao_reg3,method ="arellano",type = "sss",cluster = "group")
    #coeftest(mort_res_regiao_reg3,vcov.mort_res_regiao_reg3)
    
    internacoes_res_capital_reg3= plm(log(res_internacoes_transp_total+1)~tratamento*capital+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
    vcov.internacoes_res_capital_reg3= vcovHC(internacoes_res_capital_reg3,method ="arellano",type = "sss",cluster = "group")
    #coeftest(internacoes_res_capital_reg3,vcov.internacoes_res_capital_reg3)
    
    internacoes_res_regiao_reg3= plm(log(res_internacoes_transp_total+1)~tratamento*regiao+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
    vcov.internacoes_res_regiao_reg3= vcovHC(internacoes_res_regiao_reg3,method ="arellano",type = "sss",cluster = "group")
    #coeftest(internacoes_res_regiao_reg3,vcov.internacoes_res_regiao_reg3)
    
    valor_res_capital_reg3= plm(log(res_valor_total+1)~tratamento*capital+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
    vcov.valor_res_capital_reg3= vcovHC(valor_res_capital_reg3,method ="arellano",type = "sss",cluster = "group")
    #coeftest(valor_res_capital_reg3,vcov.valor_res_capital_reg3)
    
    valor_res_regiao_reg3= plm(log(res_valor_total+1)~tratamento*regiao+ log(total_frota)+ log(total_leitos)+ log(total_medicos)+log(salario_medio_def)+nv_tecno+cod_munic*as.numeric(Ano), data=df_uber, model="within",effect="twoways")
    vcov.valor_res_regiao_reg3= vcovHC(valor_res_regiao_reg3,method ="arellano",type = "sss",cluster = "group")
    #coeftest(valor_res_regiao_reg3,vcov.valor_res_regiao_reg3)
    
    
    stargazer(mort_res_capital_reg3,mort_res_regiao_reg3,internacoes_res_capital_reg3,internacoes_res_regiao_reg3,valor_res_capital_reg3,valor_res_regiao_reg3,
              keep=c("tratamento1"),dep.var.labels=c("Óbitos","Internações","Valor Internações "),
              covariate.labels=c("Uber","Uber x Capital","Uber x NE","Uber x N","Uber x SE","Uber x S"),
              se=list(sqrt(diag(vcov.mort_res_capital_reg3)),sqrt(diag(vcov.mort_res_regiao_reg3)),
                      sqrt(diag(vcov.internacoes_res_capital_reg3)),sqrt(diag(vcov.internacoes_res_regiao_reg3)),
                      sqrt(diag(vcov.valor_res_capital_reg3)),sqrt(diag(vcov.valor_res_regiao_reg3))),
              title = c("Efeitos do Uber por Capitais e Regiões - Local de Residência"),notes.label = "Notas",
              notes = c("Desvios padrões agrupados por municípios. Variável dependente separada por grupos etários e sexo. Controles: frota de veículos por mil habitantes, leitos hospitalares por mil habitantes, médicos por mil habitantes, salário médio, percentual valor adicionado urbano (indústria, comércio, serviços e administração pública)."),
              type="text", decimal.mark = "," , digit.separator = ".", style = "apsr",keep.stat=c("n", "rsq"),
              model.names = FALSE, model.numbers = TRUE, df=FALSE, digits=3,out="6_reg_valor_regiao_1_res.html")
  }
}
}


