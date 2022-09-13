#Packages
{
library(rio)
library(dplyr)
library(tidyr)
library(tidyverse)
library(textclean)
library(glue)
library(DT)
library(ggthemr)
library(ggplot2)
library(gganimate)
library(plotly)
library(MicroDatosEs)
#ggthemr("dust")
}

#EXTRAER ZIPS
for (ii in 05:19){
    if (ii < 10) {
        ii <- as.character(ii)
        ii <- paste0("0" , {ii})
    }
    zip::unzip(zipfile= glue("./dades/zips/datos_1t" , {ii} , ".zip") , exdir = glue("./dades/zips/datos_1t" , {ii} , "/"))
    zip::unzip(zipfile= glue("./dades/zips/datos_2t" , {ii} , ".zip") , exdir = glue("./dades/zips/datos_2t" , {ii} , "/"))
    zip::unzip(zipfile= glue("./dades/zips/datos_3t" , {ii} , ".zip") , exdir = glue("./dades/zips/datos_3t" , {ii} , "/"))
    zip::unzip(zipfile= glue("./dades/zips/datos_4t" , {ii} , ".zip") , exdir = glue("./dades/zips/datos_4t" , {ii} , "/"))
}

#JUNTAR LAS EPAS Y EXPORTAR EN "archivo_bueno.csv"
for (ii in 05:19){
    if (ii < 10) {
        ii <- as.character(ii)
        ii <- paste0("0" , {ii})
    }
    epa1 <- epa2005(glue("./dades/zips/datos_1t" , {ii} , "/md_EPA_20" , {ii} , "T1.txt"))
    epa2 <- epa2005(glue("./dades/zips/datos_2t" , {ii} , "/md_EPA_20" , {ii} , "T2.txt"))
    epa3 <- epa2005(glue("./dades/zips/datos_3t" , {ii} , "/md_EPA_20" , {ii} , "T3.txt"))
    epa4 <- epa2005(glue("./dades/zips/datos_4t" , {ii} , "/md_EPA_20" , {ii} , "T4.txt"))
    epa <- bind_rows(epa1 , epa2 , epa3 , epa4)
    assign(glue("epa" , {ii}) , epa)
}
df <- bind_rows(epa05,epa06,epa07,epa08,epa09,epa10,epa11,epa12,epa13,epa14,epa15,epa16,epa17,epa18,epa19)
df <- df %>% select(CICLO,CCAA,EDAD,RELPP,SEXO,NAC,EXREGNA,NFORMA,CURSR,OCUP,ACT,SITU,SP,DUCON1,DUCON2,DUCON3,BUSCA,DESEA,FOBACT,ASALA,DISP,DTANT,CSE,FACTOREL)
la_seq <- seq(from = 133, to = 189, by = 4)
df <- df %>% filter(CICLO %in% la_seq) %>%
    mutate("FACTOREL" = as.numeric(FACTOREL)/100)
rio::export(x = df , file = "./archivo_bueno2.csv")

#OTRO
df <- import("./archivo_bueno2.csv")
df <- df %>% select(CICLO,CCAA,NIVEL,EDAD5,RELPP1,SEXO1,NAC1,EXREGNA1,NFORMA,CURSR,OCUP1,ACT1,SITU,SP,DUCON1,DUCON2,DUCON3,BUSCA,DESEA,FOBACT,ASALA,DISP,DTANT,CSE,FACTOREL)
rio::export(x = df , file = "./archivo_bueno2.csv")
