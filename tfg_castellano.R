# Packages + tidy
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
library(knitr)
library(data.table)
library(kableExtra)
library(gt)
library(gridExtra)
library(RColorBrewer)
library(fmsb)
library(radarchart)
options(scipen=999 , digits = 6)
df <- import("./archivo_bueno2.csv")
df <- df %>% mutate(SEXO1 = gsub(x = SEXO1 , pattern = 1 , replacement = "Hombre")) %>%
  mutate(SEXO1 = gsub(x = SEXO1 , pattern = 6 , replacement = "Mujer"))
}

# 1. Tabla mayores
{
aa <- df %>% filter(CICLO == 189) %>%
    mutate(ANO = 2019) %>%
    group_by(SEXO1 , NIVEL , ANO) %>%
    summarise("personas" = sum(FACTOREL , na.rm = FALSE)) %>%
    ungroup()
newRow1 <- data.frame(SEXO1 = c("Mujer" , "Mujer" , "Hombre" , "Hombre") , NIVEL = c(1 , 2 , 1 , 2) , ANO = as.numeric(c("2003" , "2003" , "2003" , "2003")) , personas = as.numeric(c("17579800" , "3071700" , "16595700" , "3276300")))
newRow2 <- data.frame(SEXO1 = c("Mujer" , "Mujer" , "Hombre" , "Hombre") , NIVEL = c(1 , 2 , 1 , 2) , ANO = as.numeric(c("1978" , "1978" , "1978" , "1978")) , personas = as.numeric(c("13489900" , "4871500" , "12522300" , "5122200")))
aa <- bind_rows(aa , newRow1 , newRow2) %>%
    pivot_wider(names_from = SEXO1 , values_from = personas)
aa <- aa %>% mutate(Total = Hombre + Mujer) %>%
    pivot_longer(names_to = "SEXO1" , values_to = "personas" , cols = 3:5)
x <- as.numeric(c(1978 , 2003 , 2019))
for (ii in x) {
    zz <- aa %>% filter(NIVEL == 1) %>%
    pivot_wider(names_from = SEXO1 , values_from = personas) %>%
    arrange(-desc(ANO)) %>%
    select(ANO , Total , Hombre , Mujer) %>%
    filter(ANO == ii) %>%
    mutate(Total = Total/1000 , Hombre = Hombre/1000 , Mujer = Mujer/1000)

    assign(x = paste0("zz" , {ii}) , value = zz)
}
aa1 <- aa %>% filter(NIVEL == 1) %>%
    pivot_wider(names_from = SEXO1 , values_from = personas) %>%
    mutate(ANO = (as.character(ANO))) %>%
    select(ANO , Total , Hombre , Mujer) %>%
    arrange(ANO) %>%
    mutate(Total = Total/1000 , Hombre = Hombre/1000 , Mujer = Mujer/1000)
dif19.03 <- data.frame(ANO = "Diferencia 2019 - 2003" , Total = zz2019$Total - zz2003$Total , Hombre = zz2019$Hombre - zz2003$Hombre , Mujer = zz2019$Mujer - zz2003$Mujer)
dif_por19.03 <- data.frame(ANO = "Diferencia % 2019 - 2003" , Total = (zz2019$Total - zz2003$Total)/zz2003$Total , Hombre = (zz2019$Hombre - zz2003$Hombre)/zz2003$Hombre , Mujer = (zz2019$Mujer - zz2003$Mujer)/zz2003$Mujer)
dif19.78 <- data.frame(ANO = "Diferencia 2019 - 1978" , Total = zz2019$Total - zz1978$Total , Hombre = zz2019$Hombre - zz1978$Hombre , Mujer = zz2019$Mujer - zz1978$Mujer)
dif_por19.78 <- data.frame(ANO = "Diferencia % 2019 - 1978" , Total = (zz2019$Total - zz1978$Total)/zz1978$Total , Hombre = (zz2019$Hombre - zz1978$Hombre)/zz1978$Hombre , Mujer = (zz2019$Mujer - zz1978$Mujer)/zz1978$Mujer)
aa1 <- bind_rows(aa1 , dif19.78 , dif_por19.78 , dif19.03 , dif_por19.03)

options(digits = 6)

p <- aa1 %>% gt() %>%
    tab_header(title = md("**GRÁFICO 1: EVOLUCIÓN DE LA POBLACIÓN DE 16 AÑOS O MÁS EN ESPAÑA, 1978, 2003 Y 2019** *(En miles)*")) %>%
    cols_label(ANO = "Año" , Total = "Ambos sexos" , Hombre = "Hombres" , Mujer = "Mujeres") %>%
    fmt_percent(columns = c("Total" , "Hombre" , "Mujer") , rows = c(5 , 7)) %>%
    tab_source_note(source_note = md("***Fuente:** INE, EPA*<br>*Elaboración propia*")) %>%
    tab_style(style = cell_text(color = "firebrick", size = "large", align = "center"), locations = cells_title()) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_title()) %>%
    tab_style(style = cell_text(color = "firebrick", size = "normal", align = "center" , weight = "bolder"), locations = cells_column_labels(columns = c("ANO" , "Total" , "Hombre" , "Mujer"))) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_column_labels(columns = c("ANO" , "Total" , "Hombre" , "Mujer"))) %>%
    tab_style(style = cell_text(weight = "bold"), locations = cells_data(columns = c("ANO" , "Total" , "Hombre" , "Mujer"), rows = c(5 , 7))) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_data(columns = "ANO")) %>%
    tab_style(style = cell_text(color = "firebrick" , size = "normal" , align = "center" , weight = "bolder"), locations = cells_data(columns = "ANO"))

p
gtsave(p , filename = "1. Tabla mayores.png" , path = "./Exports/" , useragent = TRUE)
}

# 2. Tabla menores
{
aa <- df %>% filter(CICLO == 189) %>%
    mutate(ANO = 2019) %>%
    group_by(SEXO1 , NIVEL , ANO) %>%
    summarise("personas" = sum(FACTOREL , na.rm = FALSE)) %>%
    ungroup()
newRow1 <- data.frame(SEXO1 = c("Mujer" , "Mujer" , "Hombre" , "Hombre") , NIVEL = c(1 , 2 , 1 , 2) , ANO = as.numeric(c("2003" , "2003" , "2003" , "2003")) , personas = as.numeric(c("17579800" , "3071700" , "16595700" , "3276300")))
newRow2 <- data.frame(SEXO1 = c("Mujer" , "Mujer" , "Hombre" , "Hombre") , NIVEL = c(1 , 2 , 1 , 2) , ANO = as.numeric(c("1978" , "1978" , "1978" , "1978")) , personas = as.numeric(c("13489900" , "4871500" , "12522300" , "5122200")))
aa <- bind_rows(aa , newRow1 , newRow2) %>%
    pivot_wider(names_from = SEXO1 , values_from = personas)
aa <- aa %>% mutate(Total = Hombre + Mujer) %>%
    pivot_longer(names_to = "SEXO1" , values_to = "personas" , cols = 3:5)
x <- as.numeric(c(1978 , 2003 , 2019))
for (ii in x) {
    zz <- aa %>% filter(NIVEL == 2) %>%
    pivot_wider(names_from = SEXO1 , values_from = personas) %>%
    arrange(-desc(ANO)) %>%
    select(ANO , Total , Hombre , Mujer) %>%
    filter(ANO == ii) %>%
    mutate(Total = Total/1000 , Hombre = Hombre/1000 , Mujer = Mujer/1000)

    assign(x = paste0("zz" , {ii}) , value = zz)
}
aa1 <- aa %>% filter(NIVEL == 2) %>%
    pivot_wider(names_from = SEXO1 , values_from = personas) %>%
    mutate(ANO = (as.character(ANO))) %>%
    select(ANO , Total , Hombre , Mujer) %>%
    arrange(ANO) %>%
    mutate(Total = Total/1000 , Hombre = Hombre/1000 , Mujer = Mujer/1000)
dif19.03 <- data.frame(ANO = "Diferencia 2019 - 2003" , Total = zz2019$Total - zz2003$Total , Hombre = zz2019$Hombre - zz2003$Hombre , Mujer = zz2019$Mujer - zz2003$Mujer)
dif_por19.03 <- data.frame(ANO = "Diferencia % 2019 - 2003" , Total = (zz2019$Total - zz2003$Total)/zz2003$Total , Hombre = (zz2019$Hombre - zz2003$Hombre)/zz2003$Hombre , Mujer = (zz2019$Mujer - zz2003$Mujer)/zz2003$Mujer)
dif19.78 <- data.frame(ANO = "Diferencia 2019 - 1978" , Total = zz2019$Total - zz1978$Total , Hombre = zz2019$Hombre - zz1978$Hombre , Mujer = zz2019$Mujer - zz1978$Mujer)
dif_por19.78 <- data.frame(ANO = "Diferencia % 2019 - 1978" , Total = (zz2019$Total - zz1978$Total)/zz1978$Total , Hombre = (zz2019$Hombre - zz1978$Hombre)/zz1978$Hombre , Mujer = (zz2019$Mujer - zz1978$Mujer)/zz1978$Mujer)
aa1 <- bind_rows(aa1 , dif19.78 , dif_por19.78 , dif19.03 , dif_por19.03)

p <- aa1 %>% gt() %>%
    tab_header(title = md("**GRÁFICO 2: EVOLUCIÓN DE LA POBLACIÓN MENOR DE 16 AÑOS EN ESPAÑA, 1978, 2003 Y 2019** *(En miles)*")) %>%
    cols_label(ANO = "Año" , Total = "Ambos sexos" , Hombre = "Hombres" , Mujer = "Mujeres") %>%
    fmt_percent(columns = c("Total" , "Hombre" , "Mujer") , rows = c(5 , 7) ) %>%
    tab_source_note(source_note = md("***Fuente:** INE, EPA*<br>*Elaboración propia*")) %>%
    tab_style(style = cell_text(color = "firebrick", size = "large", align = "center"), locations = cells_title()) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_title()) %>%
    tab_style(style = cell_text(color = "firebrick", size = "normal", align = "center" , weight = "bolder"), locations = cells_column_labels(columns = c("ANO" , "Total" , "Hombre" , "Mujer"))) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_column_labels(columns = c("ANO" , "Total" , "Hombre" , "Mujer"))) %>%
    tab_style(style = cell_text(weight = "bold"), locations = cells_data(columns = c("ANO" , "Total" , "Hombre" , "Mujer"), rows = c(5 , 7))) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_data(columns = "ANO")) %>%
    tab_style(style = cell_text(color = "firebrick" , size = "normal" , align = "center" , weight = "bolder"), locations = cells_data(columns = "ANO"))

p
gtsave(p , filename = "2. Tabla menores.png" , path = "./Exports/")
}

# 3. Distribución población
{
y <- c("Hombre" , "Mujer")

for (jj in y) {
  ii <- 133

  zz1 <- df %>% filter(CICLO == ii , SEXO1 == jj , NIVEL == 1) %>%
    summarise("personas" = sum(FACTOREL , na.rm = FALSE)) %>%
    as.numeric()
  aa <- df %>% filter(CICLO == ii , SEXO1 == jj , NIVEL == 1) %>%
    group_by(EDAD5 , CICLO , SEXO1) %>%
    summarise("personas" = ((sum(FACTOREL , na.rm = FALSE))*100)/zz1) %>%
    ungroup()

  x <- c(153 , 173 , 189)

  for (ii in x) {
    zz1 <- df %>% filter(CICLO == ii , SEXO1 == jj , NIVEL == 1) %>%
      summarise("personas" = sum(FACTOREL , na.rm = FALSE)) %>%
      as.numeric()
    aa1 <- df %>% filter(CICLO == ii , SEXO1 == jj , NIVEL == 1) %>%
      group_by(EDAD5 , CICLO , SEXO1) %>%
      summarise("personas" = ((sum(FACTOREL , na.rm = FALSE))*100)/zz1) %>%
      ungroup()

    aa <- bind_rows(aa , aa1)
    }
  assign(x = paste0("aa_" , jj) , value = aa)
}

aa_def <- bind_rows(aa_Hombre , aa_Mujer)
aa_def <- aa_def %>% mutate(Año = gsub(x = CICLO , pattern = 133 , replacement = "2005")) %>%
  mutate(Año = gsub(x = Año , pattern = 153 , replacement = "2010")) %>%
  mutate(Año = gsub(x = Año , pattern = 173 , replacement = "2015")) %>%
  mutate(Año = gsub(x = Año , pattern = 189 , replacement = "2019")) %>%
  mutate(Edad = EDAD5 , Porcentaje = personas , Sexo = SEXO1) %>%
  select(Año , Edad , Porcentaje , Sexo)

newRow1 <- data.frame(Año = as.character(c(1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978)) , Edad = c(16 , 20 , 25 , 30 , 35 , 40 , 45 , 50 , 55 , 60 , 65) , Porcentaje = c(9.8 , 10.7 , 9.8 , 9.5 , 8.2 , 8.8 , 9.2 , 8.4 , 7 , 5.5 , 13.1) , Sexo = c("Hombre" , "Hombre" , "Hombre" , "Hombre" , "Hombre" , "Hombre" , "Hombre" , "Hombre" , "Hombre" , "Hombre" , "Hombre"))

newRow2 <- data.frame(Año = as.character(c(1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978)) , Edad = c(16 , 20 , 25 , 30 , 35 , 40 , 45 , 50 , 55 , 60 , 65) , Porcentaje = c(8.9 , 9.9 , 9.1 , 8.7 , 7.5 , 8.4 , 8.7 , 8.1 , 7.1 , 6.2 , 17.4) , Sexo = c("Mujer" , "Mujer" , "Mujer" , "Mujer" , "Mujer" , "Mujer" , "Mujer" , "Mujer" , "Mujer" , "Mujer" , "Mujer"))

newRow3 <- data.frame(Año = as.character(c(2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003)) , Edad = c(16 , 20 , 25 , 30 , 35 , 40 , 45 , 50 , 55 , 60 , 65) , Porcentaje = c(5.8 , 8.9 , 10.6 , 10.6 , 10.2 , 9.1 , 8 , 7.3 , 6.7 , 5.5 , 17.3) , Sexo = c("Hombre" , "Hombre" , "Hombre" , "Hombre" , "Hombre" , "Hombre" , "Hombre" , "Hombre" , "Hombre" , "Hombre" , "Hombre"))

newRow4 <- data.frame(Año = as.character(c(2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003)) , Edad = c(16 , 20 , 25 , 30 , 35 , 40 , 45 , 50 , 55 , 60 , 65) , Porcentaje = c(5.1 , 8 , 9.5 , 9.6 , 9.4 , 8.6 , 7.7 , 7 , 6.7 , 5.7 , 22.7) , Sexo = c("Mujer" , "Mujer" , "Mujer" , "Mujer" , "Mujer" , "Mujer" , "Mujer" , "Mujer" , "Mujer" , "Mujer" , "Mujer"))

aa_def <- bind_rows(aa_def , newRow1 , newRow2 , newRow3 , newRow4)

options(digits = 3)

p <- ggplot(data = aa_def , mapping = aes(x = Edad , y = Porcentaje , colour = Año)) +
  geom_line() +
  labs(title = "GRÁFICO 3: DISTRIBUCIÓN DE LA POBLACIÓN EN ESPAÑA SEGÚN EDAD" , caption = "Fuente: INE, EPA. Elaboración propia" , x = NULL , y = NULL) +
  theme(plot.caption = element_text(face = "italic", colour = "gray24") ,
    plot.title = element_text(size = 16 , face = "bold", colour = "firebrick") ,
    axis.text.x = element_text(angle = 45, hjust = 1) ,
    panel.background = element_rect(fill = "slategray", colour = "snow4")) +
  facet_wrap(vars(Sexo)) +
  scale_x_continuous(breaks = seq(15 , 65 , 10) , labels = c("15-19" , "25-29" , "35-39" , "45-49" , "55-59" , "65 y más")) +
  scale_y_continuous(breaks = seq(5 , 25 , 2.5) , labels = c("5%" , "7.5%" , "10%" , "12.5%" , "15%" , "17.5%" , "20%" , "22.5%" , "25%")) +
  labs(colour = NULL) +
  scale_color_brewer(palette = "YlOrRd")

ggplotly(p , tooltip = c("Año" ,  "Porcentaje" , "Edad"))
ggsave(filename = "./Exports/3.1 Distribución población.png" , plot = p , width = 8.5 , height = 6)

p <- ggplot(data = aa_def , mapping = aes(x = Edad , y = Porcentaje , colour = Año)) +
  geom_line() +
  labs(title = "GRÁFICO 3: DISTRIBUCIÓN DE LA POBLACIÓN EN ESPAÑA SEGÚN EDAD" , caption = "Fuente: INE, EPA. Elaboración propia" , x = NULL , y = NULL) +
  theme(plot.caption = element_text(face = "italic", colour = "gray24") ,
    plot.title = element_text(size = 16 , face = "bold", colour = "firebrick") ,
    axis.text.x = element_text(angle = 45, hjust = 1) ,
    panel.background = element_rect(fill = "gray80", colour = "snow4")) +
  facet_wrap(vars(Sexo)) +
  scale_x_continuous(breaks = seq(15 , 65 , 10) , labels = c("15-19" , "25-29" , "35-39" , "45-49" , "55-59" , "65 y más")) +
  scale_y_continuous(breaks = seq(5 , 25 , 2.5) , labels = c("5%" , "7.5%" , "10%" , "12.5%" , "15%" , "17.5%" , "20%" , "22.5%" , "25%")) +
  labs(colour = NULL) +
  scale_color_brewer(palette = "YlOrRd")

ggplotly(p , tooltip = c("Año" ,  "Porcentaje" , "Edad"))
ggsave(filename = "./Exports/3.2 Distribución población.png" , plot = p , width = 8.5 , height = 6)
}

# 4. Distribución por nivel de estudios (spider - radar chart) (no se guarda)
{
zz <- df %>% filter(CICLO == 189 , NIVEL == 1) %>%
  summarise(total = sum(FACTOREL , na.rm = FALSE))
zz <- as.numeric(zz$total)

aa <- df %>% filter(CICLO %in% c(189) , NIVEL == 1) %>%
  group_by(NFORMA , CICLO) %>%
  summarise(Porcentaje = sum(FACTOREL , na.rm = FALSE)*100/zz) %>%
  ungroup() %>%
  select("Formación" = NFORMA , "2019" = Porcentaje) %>%
  mutate(Formación = gsub(pattern = "AN" , replacement = "Analfabetos" , x = Formación)) %>%
  mutate(Formación = gsub(pattern = "P1" , replacement = "Sin educación" , x = Formación)) %>%
  mutate(Formación = gsub(pattern = "P2" , replacement = "Primaria" , x = Formación)) %>%
  mutate(Formación = gsub(pattern = "S1" , replacement = "Secundaria primera etapa" , x = Formación)) %>%
  mutate(Formación = gsub(pattern = "SG" , replacement = "Secundaria segunda etapa" , x = Formación)) %>%
  mutate(Formación = gsub(pattern = "SP" , replacement = "Formación profesional" , x = Formación)) %>%
  mutate(Formación = gsub(pattern = "SU" , replacement = "Educación superior" , x = Formación)) %>%
  add_column("2003" = c(3 , 9 , 26 , 24 , 13 , 12 , 13)) %>%
  add_column("1978" = c(9 , 15 , 56 , 8 , 6 , 1 , 5)) %>%
  select(1 , 4:2)

chartJSRadar(scores = aa , maxScale = 60 , showToolTipLabel = TRUE)
}

# 5. Tasa de actividad por sexo y grupos quinquenales
{
  ii <- 133

  zz1 <- df %>% filter(CICLO == ii , NIVEL == 1) %>%
  group_by(CICLO , EDAD5 , SEXO1) %>%
  summarise("Total" = sum(FACTOREL , na.rm = FALSE)) %>%
  ungroup()

  aa <- df %>% filter(AOI == 3 | AOI == 4 | AOI == 5 | AOI == 6) %>%
  filter(CICLO == ii , NIVEL == 1) %>%
  group_by(CICLO , EDAD5 , SEXO1) %>%
  summarise("Porcentaje" = sum(FACTOREL , na.rm = FALSE)) %>%
  ungroup() %>%
  mutate(Porcentaje = Porcentaje*100/zz1$Total)

  x <- c(153 , 173 , 189)

  for (ii in x) {
    zz1 <- df %>% filter(CICLO == ii , NIVEL == 1) %>%
  group_by(CICLO , EDAD5 , SEXO1) %>%
  summarise("Total" = sum(FACTOREL , na.rm = FALSE)) %>%
  ungroup()
    aa1 <- df %>% filter(AOI == 3 | AOI == 4 | AOI == 5 | AOI == 6) %>%
  filter(CICLO == ii , NIVEL == 1) %>%
  group_by(CICLO , EDAD5 , SEXO1) %>%
  summarise("Porcentaje" = sum(FACTOREL , na.rm = FALSE)) %>%
  ungroup() %>%
  mutate(Porcentaje = Porcentaje*100/zz1$Total)

    aa <- bind_rows(aa , aa1)
    }

aa <- aa %>% mutate(Año = gsub(x = CICLO , pattern = 133 , replacement = "2005")) %>%
  mutate(Año = gsub(x = Año , pattern = 153 , replacement = "2010")) %>%
  mutate(Año = gsub(x = Año , pattern = 173 , replacement = "2015")) %>%
  mutate(Año = gsub(x = Año , pattern = 189 , replacement = "2019")) %>%
  mutate(Edad = EDAD5 , Sexo = SEXO1) %>%
  select(Año , Edad , Porcentaje , Sexo)

newRow1 <- data.frame(Año = as.character(c(1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978)) , Edad = c(16,16,20,20,25,25,30,30,35,35,40,40,45,45,50,50,55,55,60,60,65,65) , Porcentaje = c(57,45,63,54,94,37,98,28,98,26,97,28,96,27,93,27.5,87,26,69,20,16,9) , Sexo = c("Hombre" , "Mujer" , "Hombre" , "Mujer" , "Hombre" , "Mujer" , "Hombre" , "Mujer" , "Hombre" , "Mujer" , "Hombre" , "Mujer" , "Hombre" , "Mujer" , "Hombre" , "Mujer" , "Hombre" , "Mujer" , "Hombre" , "Mujer" , "Hombre" , "Mujer"))
newRow2 <- data.frame(Año = as.character(c(2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003)) , Edad = c(16,16,20,20,25,25,30,30,35,35,40,40,45,45,50,50,55,55,60,60,65,65) , Porcentaje = c(31,19,68,58,90,80,95,75,95.5,70,95,67,93.5,61.5,89,47,76,33.5,49,19,3.2,2) , Sexo = c("Hombre" , "Mujer" , "Hombre" , "Mujer" , "Hombre" , "Mujer" , "Hombre" , "Mujer" , "Hombre" , "Mujer" , "Hombre" , "Mujer" , "Hombre" , "Mujer" , "Hombre" , "Mujer" , "Hombre" , "Mujer" , "Hombre" , "Mujer" , "Hombre" , "Mujer"))

aa_def <- bind_rows(aa , newRow1 , newRow2)
#aa_def <- aa_def %>% filter(Año %in% c("1978" , "2003" , "2019"))

p <- ggplot(data = aa_def , mapping = aes(x = Edad , y = Porcentaje , colour = Año)) +
  geom_line() +
  labs(title = "GRÁFICO 7: TASA DE ACTIVIDAD POR SEXO Y GRUPOS QUINQUENALES DE EDAD EN ESPAÑA" , caption = "Fuente: INE, EPA. Elaboración propia" , x = NULL , y = NULL) +
  theme(plot.caption = element_text(face = "italic", colour = "gray24") ,
    plot.title = element_text(size = 16 , face = "bold", colour = "firebrick") ,
    axis.text.x = element_text(angle = 45, hjust = 1) ,
    panel.background = element_rect(fill = "slategray", colour = "snow4")) +
  facet_wrap(vars(Sexo)) +
  scale_x_continuous(breaks = seq(15 , 65 , 10) , labels = c("15-19" , "25-29" , "35-39" , "45-49" , "55-59" , "65 y más")) +
  scale_y_continuous(breaks = seq(0 , 100 , 10) , labels = c("0%" , "10%" , "20%" , "30%" , "40%" , "50%" , "60%" , "70%" , "80%" , "90%" , "100%")) +
  labs(colour = NULL) +
  scale_color_brewer(palette = "YlOrRd")

ggplotly(p , tooltip = c("Año" ,  "Porcentaje" , "Edad"))
ggsave(filename = "./Exports/5.1 Tasa de actividad por sexo y grupos quinquenales.png" , plot = p , width = 9.5 , height = 6)

p <- ggplot(data = aa_def , mapping = aes(x = Edad , y = Porcentaje , colour = Año)) +
  geom_line() +
  labs(title = "GRÁFICO 7: TASA DE ACTIVIDAD POR SEXO Y GRUPOS QUINQUENALES DE EDAD EN ESPAÑA" , caption = "Fuente: INE, EPA. Elaboración propia" , x = NULL , y = NULL) +
  theme(plot.caption = element_text(face = "italic", colour = "gray24") ,
    plot.title = element_text(size = 16 , face = "bold", colour = "firebrick") ,
    axis.text.x = element_text(angle = 45, hjust = 1) ,
    panel.background = element_rect(fill = "gray80", colour = "snow4")) +
  facet_wrap(vars(Sexo)) +
  scale_x_continuous(breaks = seq(15 , 65 , 10) , labels = c("15-19" , "25-29" , "35-39" , "45-49" , "55-59" , "65 y más")) +
  scale_y_continuous(breaks = seq(0 , 100 , 10) , labels = c("0%" , "10%" , "20%" , "30%" , "40%" , "50%" , "60%" , "70%" , "80%" , "90%" , "100%")) +
  labs(colour = NULL) +
  scale_color_brewer(palette = "YlOrRd")

ggplotly(p , tooltip = c("Año" ,  "Porcentaje" , "Edad"))
ggsave(filename = "./Exports/5.2 Tasa de actividad por sexo y grupos quinquenales.png" , plot = p , width = 9.5 , height = 6)
}

# 6. Porcentaje de jóvenes que se declaran estudiantes (parece estar mal porque no es normal que en 2003 porcentaje alto y 2005 porcentaje bajo. Además, no tenemos los datos de edad sin agrupar porque esos datos valen dinero)
{
ii <- 133

  zz1 <- df %>% filter(CICLO == ii , NIVEL == 1 , EDAD5 %in% c(16 , 20 , 25 , 30)) %>%
  group_by(CICLO , EDAD5) %>%
  summarise("Total" = sum(FACTOREL , na.rm = FALSE)) %>%
  ungroup()

  aa <- df %>% filter(CURSR %in% c(1 , 2) | CURSNR %in% c(1 , 2)) %>%
  filter(CICLO == ii , NIVEL == 1 , EDAD5 %in% c(16 , 20 , 25 , 30)) %>%
  group_by(CICLO , EDAD5) %>%
  summarise("Porcentaje" = sum(FACTOREL , na.rm = FALSE)) %>%
  ungroup() %>%
  mutate(Porcentaje = Porcentaje*100/zz1$Total)

  x <- c(153 , 173 , 189)

  for (ii in x) {
    zz1 <- df %>% filter(CICLO == ii , NIVEL == 1 , EDAD5 %in% c(16 , 20 , 25 , 30)) %>%
  group_by(CICLO , EDAD5) %>%
  summarise("Total" = sum(FACTOREL , na.rm = FALSE)) %>%
  ungroup()
    aa1 <- df %>% filter(CURSR %in% c(1 , 2) | CURSNR %in% c(1 , 2)) %>%
  filter(CICLO == ii , NIVEL == 1 , EDAD5 %in% c(16 , 20 , 25 , 30)) %>%
  group_by(CICLO , EDAD5) %>%
  summarise("Porcentaje" = sum(FACTOREL , na.rm = FALSE)) %>%
  ungroup() %>%
  mutate(Porcentaje = Porcentaje*100/zz1$Total)

    aa <- bind_rows(aa , aa1)
    }

aa <- aa %>% mutate(Año = gsub(x = CICLO , pattern = 133 , replacement = "2005")) %>%
  mutate(Año = gsub(x = Año , pattern = 153 , replacement = "2010")) %>%
  mutate(Año = gsub(x = Año , pattern = 173 , replacement = "2015")) %>%
  mutate(Año = gsub(x = Año , pattern = 189 , replacement = "2019")) %>%
  mutate(Edad = EDAD5) %>%
  select(Año , Edad , Porcentaje)

newRow1 <- data.frame(Año = as.character(c(1978 , 1978 , 1978 , 1978)) , Edad = c(16 , 20 , 25 , 30) , Porcentaje = c(55 , 25 , 6 , 1))
newRow2 <- data.frame(Año = as.character(c(2003 , 2003 , 2003 , 2003)) , Edad = c(16 , 20 , 25 , 30) , Porcentaje = c(91 , 56 , 26 , 7.5))
aa_def <- bind_rows(aa , newRow1 , newRow2)
#aa_def <- aa_def %>% filter(Año %in% c("1978" , "2003" , "2019"))

p <- ggplot(data = aa_def , mapping = aes(x = Edad , y = Porcentaje , colour = Año)) +
  geom_line() +
  labs(title = "GRÁFICO 8: PORCENTAJE DE JÓVENES QUE SE DECLARA ESTUDIANTE EN ESPAÑA" , caption = "Fuente: INE, EPA. Elaboración propia" , x = NULL , y = NULL) +
  theme(plot.caption = element_text(face = "italic", colour = "gray24") ,
    plot.title = element_text(size = 16 , face = "bold", colour = "firebrick") ,
    axis.text.x = element_text(angle = 45, hjust = 1) ,
    panel.background = element_rect(fill = "slategray", colour = "snow4")) +
  scale_x_continuous(breaks = seq(0 , 30 , 5)) +
  scale_y_continuous(breaks = seq(0 , 100 , 10) , labels = c("0%" , "10%" , "20%" , "30%" , "40%" , "50%" , "60%" , "70%" , "80%" , "90%" , "100%")) +
  labs(colour = NULL) +
  scale_color_brewer(palette = "YlOrRd")

ggplotly(p , tooltip = c("Año" ,  "Porcentaje" , "Edad"))
ggsave(filename = "./Exports/6.1 Porcentaje de jóvenes que se declaran estudiantes (mal).png" , plot = p , width = 8.5 , height = 6)

p <- ggplot(data = aa_def , mapping = aes(x = Edad , y = Porcentaje , colour = Año)) +
  geom_line() +
  labs(title = "GRÁFICO 8: PORCENTAJE DE JÓVENES QUE SE DECLARA ESTUDIANTE EN ESPAÑA" , caption = "Fuente: INE, EPA. Elaboración propia" , x = NULL , y = NULL) +
  theme(plot.caption = element_text(face = "italic", colour = "gray24") ,
    plot.title = element_text(size = 16 , face = "bold", colour = "firebrick") ,
    axis.text.x = element_text(angle = 45, hjust = 1) ,
    panel.background = element_rect(fill = "gray80", colour = "snow4")) +
  scale_x_continuous(breaks = seq(0 , 30 , 5)) +
  scale_y_continuous(breaks = seq(0 , 100 , 10) , labels = c("0%" , "10%" , "20%" , "30%" , "40%" , "50%" , "60%" , "70%" , "80%" , "90%" , "100%")) +
  labs(colour = NULL) +
  scale_color_brewer(palette = "YlOrRd")

ggplotly(p , tooltip = c("Año" ,  "Porcentaje" , "Edad"))
ggsave(filename = "./Exports/6.2 Porcentaje de jóvenes que se declaran estudiantes (mal).png" , plot = p , width = 8.5 , height = 6)
}

# 7. Grandes magnitudes de la actividad, el empleo y el paro en españa.
{

  #Ambos sexos
  {
aa <- df %>% filter(CICLO == 189 , NIVEL == 1) %>%
  mutate(ANO = 2019) %>%
  group_by(AOI , ANO , SEXO1) %>%
  summarise(Total = sum(FACTOREL , na.rm = FALSE)) %>%
  ungroup()

aa1 <- aa %>% filter(AOI == 3 | AOI == 4) %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  mutate(Situación = "Ocupados")

aa2 <- aa %>% filter(AOI == 5 | AOI == 6) %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  mutate(Situación = "Parados")

aa3 <- aa %>% filter(AOI == 3 | AOI == 4 |AOI == 5 | AOI == 6) %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  mutate(Situación = "Activos")

aa4 <- aa %>% filter(AOI == 7 | AOI == 8 | AOI == 9) %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  mutate(Situación = "Inactivos")

aa5 <- aa %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  mutate(Situación = "Total mayores de 16 años")

newRow1 <- data.frame(ANO = 1978 , Total = c(12401600 , 877600 , 13279200 , 12733000 , 26012200) , Situación = c("Ocupados" , "Parados" , "Activos" , "Inactivos" , "Total mayores de 16 años"))
newRow2 <- data.frame(ANO = 2003 , Total = c(16666000 , 2460400 , 19126400 , 15049100 , 34175500) , Situación = c("Ocupados" , "Parados" , "Activos" , "Inactivos" , "Total mayores de 16 años"))

aa_tot <- bind_rows(aa1 , aa2 , aa3 , aa4 , aa5 , newRow1 , newRow2)
aa_tot <- aa_tot %>% pivot_wider(names_from = ANO , names_prefix = "ano" , values_from = Total) %>%
  mutate(dif19.03 = ano2019-ano2003 , dif19.78 = ano2019-ano1978) %>%
  select(Situación , ano1978 , ano2003 , ano2019 , dif19.03 , dif19.78) %>%
  add_column(Sexo = "Ambos sexos" , .before = 1)
}

  #Hombre
  {
aa1 <- aa %>% filter(SEXO1 == "Hombre") %>%
  filter(AOI == 3 | AOI == 4) %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  mutate(Situación = "Ocupados")

aa2 <- aa %>% filter(SEXO1 == "Hombre") %>%
  filter(AOI == 5 | AOI == 6) %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  mutate(Situación = "Parados")

aa3 <- aa %>% filter(SEXO1 == "Hombre") %>%
  filter(AOI == 3 | AOI == 4 |AOI == 5 | AOI == 6) %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  mutate(Situación = "Activos")

aa4 <- aa %>% filter(SEXO1 == "Hombre") %>%
  filter(AOI == 7 | AOI == 8 | AOI == 9) %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  mutate(Situación = "Inactivos")

aa5 <- aa %>%
  filter(SEXO1 == "Hombre") %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  mutate(Situación = "Total mayores de 16 años")

zz <- data.frame(Sexo = "Tasas" , Situación = c("Tasa de actividad - Varones" , "Tasa de ocupación - Varones" , "Tasa de paro - Varones" , "Tasa de paro absoluta - Varones") , ano1978 = c(0.759 , 0.712 , 0.063 , 0.048) , ano2003 =  c(0.681 , 0.619 , 0.09 , 0.061) , ano2019 = c(aa3$Total/aa5$Total , aa1$Total/aa5$Total , aa2$Total/aa3$Total , aa2$Total/aa5$Total))

newRow1 <- data.frame(ANO = 1978 , Total = c(8912000 , 598600 , 9510600 , 3011700 , 12522300) , Situación = c("Ocupados" , "Parados" , "Activos" , "Inactivos" , "Total mayores de 16 años"))
newRow2 <- data.frame(ANO = 2003 , Total = c(10278600 , 1019500 , 11298100 , 5297600 , 16595700) , Situación = c("Ocupados" , "Parados" , "Activos" , "Inactivos" , "Total mayores de 16 años"))

aa_Hombre <- bind_rows(aa1 , aa2 , aa3 , aa4 , aa5 , newRow1 , newRow2)
aa_Hombre <- aa_Hombre %>% pivot_wider(names_from = ANO , names_prefix = "ano" , values_from = Total) %>%
  mutate(dif19.03 = ano2019-ano2003 , dif19.78 = ano2019-ano1978) %>%
  select(Situación , ano1978 , ano2003 , ano2019 , dif19.03 , dif19.78) %>%
  add_column(Sexo = "Hombre" , .before = 1)
}

  #Mujer
  {aa1 <- aa %>% filter(SEXO1 == "Mujer") %>%
  filter(AOI == 3 | AOI == 4) %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  mutate(Situación = "Ocupados")

aa2 <- aa %>% filter(SEXO1 == "Mujer") %>%
  filter(AOI == 5 | AOI == 6) %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  mutate(Situación = "Parados")

aa3 <- aa %>% filter(SEXO1 == "Mujer") %>%
  filter(AOI == 3 | AOI == 4 |AOI == 5 | AOI == 6) %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  mutate(Situación = "Activos")

aa4 <- aa %>% filter(SEXO1 == "Mujer") %>%
  filter(AOI == 7 | AOI == 8 | AOI == 9) %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  mutate(Situación = "Inactivos")

aa5 <- aa %>%
  filter(SEXO1 == "Mujer") %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  mutate(Situación = "Total mayores de 16 años")

zz1 <- data.frame(Sexo = "Tasas" , Situación = c("Tasa de actividad - Mujeres" , "Tasa de ocupación - Mujeres" , "Tasa de paro - Mujeres" , "Tasa de paro absoluta - Mujeres") , ano1978 = c(0.279 , 0.259 , 0.074 , 0.021) , ano2003 =  c(0.445 , 0.363 , 0.184 , 0.082) , ano2019 = c(aa3$Total/aa5$Total , aa1$Total/aa5$Total , aa2$Total/aa3$Total , aa2$Total/aa5$Total))

newRow1 <- data.frame(ANO = 1978 , Total = c(3489600 , 279000 , 3768600 , 9721300 , 13489900) , Situación = c("Ocupados" , "Parados" , "Activos" , "Inactivos" , "Total mayores de 16 años"))
newRow2 <- data.frame(ANO = 2003 , Total = c(6387400 , 1440900 , 7828300 , 9751500 , 17579800) , Situación = c("Ocupados" , "Parados" , "Activos" , "Inactivos" , "Total mayores de 16 años"))

aa_Mujer <- bind_rows(aa1 , aa2 , aa3 , aa4 , aa5 , newRow1 , newRow2)
aa_Mujer <- aa_Mujer %>% pivot_wider(names_from = ANO , names_prefix = "ano" , values_from = Total) %>%
  mutate(dif19.03 = ano2019-ano2003 , dif19.78 = ano2019-ano1978) %>%
  select(Situación , ano1978 , ano2003 , ano2019 , dif19.03 , dif19.78) %>%
  add_column(Sexo = "Mujer" , .before = 1)
}

  #7.1 Tasas
  {
zz <- bind_rows(zz , zz1)
zz <- zz %>% arrange(Situación) %>% mutate(dif19.03 = ano2019 - ano2003 , dif19.78 = ano2019 - ano1978) %>%
  group_by(Sexo)

options(digits = 2)

p <- zz %>% gt() %>%
    tab_header(title = md("**GRÁFICO 6: LAS GRANDES TASAS DE LA ACTIVIDAD, EL EMPLEO Y EL PARO EN ESPAÑA, 1978, 2003 Y 2019**")) %>%
    cols_label(Situación = "" , ano1978 = "1978" , ano2003 = "2003" , ano2019 = "2019" , dif19.03 = md("Diferencia<br>2019 - 2003") , dif19.78 = md("Diferencia<br>2019 - 1978")) %>%
    fmt_percent(columns = c("ano1978" , "ano2003" , "ano2019" , "dif19.03" , "dif19.78")) %>%
    tab_source_note(source_note = md("***Fuente:** INE, EPA*<br>*Elaboración propia*")) %>%
    tab_style(style = cell_text(color = "firebrick", size = "large", align = "center"), locations = cells_title()) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_title()) %>%
    tab_style(style = cell_text(color = "firebrick", size = "normal", align = "center" , weight = "bolder"), locations = cells_column_labels(columns = c("Situación" , "ano1978" , "ano2003" , "ano2019" , "dif19.03" , "dif19.78"))) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_column_labels(columns = c("Situación" , "ano1978" , "ano2003" , "ano2019" , "dif19.03" , "dif19.78"))) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_data(columns = c("Situación"))) %>%
    tab_style(style = cell_text(color = "firebrick" , style = "italic"), locations = cells_data(columns = c("Situación")))
p

gtsave(p , filename = "7.1 Las grandes tasas de la actividad, el empleo y el paro.png" , path = "./Exports/")
  }

aa <- bind_rows(aa_tot , aa_Hombre , aa_Mujer)
aa <- aa %>% mutate(ano1978 = ano1978/1000 , ano2003 = ano2003/1000 , ano2019 = ano2019/1000 , dif19.03 = dif19.03/1000 , dif19.78 = dif19.78/1000) %>%
  group_by(Sexo)

options(digits = 4)

p <- aa %>% gt() %>%
    tab_header(title = md("**GRÁFICO 5: LAS GRANDES MAGNITUDES DE LA ACTIVIDAD, EL EMPLEO Y EL PARO EN ESPAÑA, 1978, 2003 Y 2019**")) %>%
    cols_label(Situación = "" , ano1978 = "1978" , ano2003 = "2003" , ano2019 = "2019" , dif19.03 = md("Diferencia<br>2019 - 2003") , dif19.78 = md("Diferencia<br>2019 - 1978")) %>%
    tab_source_note(source_note = md("***Fuente:** INE, EPA*<br>*Elaboración propia*")) %>%
    tab_style(style = cell_text(color = "firebrick", size = "large", align = "center"), locations = cells_title()) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_title()) %>%
    tab_style(style = cell_text(color = "firebrick", size = "normal", align = "center" , weight = "bolder"), locations = cells_column_labels(columns = c("Situación" , "ano1978" , "ano2003" , "ano2019" , "dif19.03" , "dif19.78"))) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_column_labels(columns = c("Situación" , "ano1978" , "ano2003" , "ano2019" , "dif19.03" , "dif19.78"))) %>%
    tab_style(style = cell_text(weight = "bold"), locations = cells_data(columns = c("Situación" , "ano1978" , "ano2003" , "ano2019" , "dif19.03" , "dif19.78"), rows = c(5 , 10 , 15))) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_data(columns = c("Situación"))) %>%
    tab_style(style = cell_text(color = "firebrick" , style = "italic"), locations = cells_data(columns = c("Situación")))

p
gtsave(p , filename = "7.2 Las grandes magnitudes de la actividad, el empleo y el paro.png" , path = "./Exports/")
}

# 8. Distribución del empleo por sector de actividad
{
aa <- df %>% filter(CICLO == 189) %>%
  mutate(ANO = 2019) %>%
  group_by(ACT1 , ANO) %>%
  summarise(Total = sum(FACTOREL , na.rm = FALSE)) %>%
  ungroup()

aa_tot <- aa %>% group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  ungroup() %>%
  select(ANO , Total) %>%
  add_row(ANO = c(1978 , 2003) , Total = c(12400500 , 16666100) , .before = 1) %>%
  pivot_wider(names_prefix = "ano" , names_from = ANO , values_from = Total) %>%
  mutate(por1978 = ano1978/ano1978, por2003 = ano2003/ano2003, por2019 = ano2019/ano2019 , Sector = "Total") %>%
  select(7 , 1:6)


aa1 <- aa %>% filter(ACT1 == 0) %>%
  select(ANO , Total) %>%
  add_row(ANO = c(1978 , 2003) , Total = c(2472100 , 934400) , .before = 1) %>%
  pivot_wider(names_prefix = "ano" , names_from = ANO , values_from = Total) %>%
  mutate(por1978 = ano1978/aa_tot$ano1978, por2003 = ano2003/aa_tot$ano2003, por2019 = ano2019/aa_tot$ano2019 , Sector = "Agricultura") %>%
  select(7 , 1:6)

aa2 <- aa %>% filter(ACT1 == 1 | ACT1 == 2 | ACT1 == 3) %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  ungroup() %>%
  select(ANO , Total) %>%
  add_row(ANO = c(1978 , 2003) , Total = c(3425000 , 3127900) , .before = 1) %>%
  pivot_wider(names_prefix = "ano" , names_from = ANO , values_from = Total) %>%
  mutate(por1978 = ano1978/aa_tot$ano1978, por2003 = ano2003/aa_tot$ano2003, por2019 = ano2019/aa_tot$ano2019 , Sector = "Industria") %>%
  select(7 , 1:6)

aa3 <- aa %>% filter(ACT1 == 4) %>%
  select(ANO , Total) %>%
  add_row(ANO = c(1978 , 2003) , Total = c(1234500 , 2010000) , .before = 1) %>%
  pivot_wider(names_prefix = "ano" , names_from = ANO , values_from = Total) %>%
  mutate(por1978 = ano1978/aa_tot$ano1978, por2003 = ano2003/aa_tot$ano2003, por2019 = ano2019/aa_tot$ano2019 , Sector = "Construcción") %>%
  select(7 , 1:6)

aa4 <- aa %>% filter(ACT1 == 5 | ACT1 == 6) %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  ungroup() %>%
  select(ANO , Total) %>%
  add_row(ANO = c(1978 , 2003) , Total = c(2956200 , 4689400) , .before = 1) %>%
  pivot_wider(names_prefix = "ano" , names_from = ANO , values_from = Total) %>%
  mutate(por1978 = ano1978/aa_tot$ano1978, por2003 = ano2003/aa_tot$ano2003, por2019 = ano2019/aa_tot$ano2019 , Sector = "Comercio, hostelería, transporte") %>%
  select(7 , 1:6)

aa5 <- aa %>% filter(ACT1 == 7) %>%
  select(ANO , Total) %>%
  add_row(ANO = c(1978 , 2003) , Total = c(415800 , 1724000) , .before = 1) %>%
  pivot_wider(names_prefix = "ano" , names_from = ANO , values_from = Total) %>%
  mutate(por1978 = ano1978/aa_tot$ano1978, por2003 = ano2003/aa_tot$ano2003, por2019 = ano2019/aa_tot$ano2019 , Sector = "Serv. financieros, serv. empresas") %>%
  select(7 , 1:6)

aa6 <- aa %>% filter(ACT1 == 8) %>%
  select(ANO , Total) %>%
  add_row(ANO = c(1978 , 2003) , Total = c(1131900 , 3047100) , .before = 1) %>%
  pivot_wider(names_prefix = "ano" , names_from = ANO , values_from = Total) %>%
  mutate(por1978 = ano1978/aa_tot$ano1978, por2003 = ano2003/aa_tot$ano2003, por2019 = ano2019/aa_tot$ano2019 , Sector = "Serv. públicos") %>%
  select(7 , 1:6)

aa7 <- aa %>% filter(ACT1 == 9) %>%
  select(ANO , Total) %>%
  add_row(ANO = c(1978 , 2003) , Total = c(765000 , 1133300) , .before = 1) %>%
  pivot_wider(names_prefix = "ano" , names_from = ANO , values_from = Total) %>%
  mutate(por1978 = ano1978/aa_tot$ano1978, por2003 = ano2003/aa_tot$ano2003, por2019 = ano2019/aa_tot$ano2019 , Sector = "Otros servicios") %>%
  select(7 , 1:6)

aa <- bind_rows(aa1 , aa2 , aa3 , aa4 , aa5 , aa6 , aa7 , aa_tot)
aa <- aa %>% mutate(ano1978 = ano1978/1000 , ano2003 = ano2003/1000 , ano2019 = ano2019/1000 , por1978 = por1978 , por2003 = por2003 , por2019 = por2019)

p <- aa %>% gt() %>%
    tab_header(title = md("**DISTRIBUCIÓN DEL EMPLEO POR SECTORES DE ACTIVIDAD EN ESPAÑA, 1978, 2003 Y 2019**")) %>%
    cols_label(Sector = "Sectores de actividad" , ano1978 = "1978" , ano2003 = "2003" , ano2019 = "2019" , por1978 = "1978" , por2003 = "2003" , por2019 = "2019") %>%
    fmt_percent(columns = c("por1978" , "por2003" , "por2019")) %>%
    tab_spanner(label = "Miles de personas", columns = c("ano1978" , "ano2003" , "ano2019"), gather = TRUE) %>%
    tab_spanner(label = "Porcentajes del total", columns = c("por1978" , "por2003" , "por2019"), gather = TRUE) %>%
    tab_source_note(source_note = md("***Fuente:** INE, Epa*<br>*Elaboración propia*")) %>%
    tab_style(style = cell_text(color = "firebrick", size = "large", align = "center"), locations = cells_title()) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_column_spanners(spanners = c("Miles de personas" , "Porcentajes del total"))) %>%
    tab_style(style = cell_text(color = "firebrick", size = "large", align = "center" , weight = "bolder"), locations = cells_column_spanners(spanners = c("Miles de personas" , "Porcentajes del total"))) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_title()) %>%
    tab_style(style = cell_text(color = "firebrick", size = "normal", align = "center" , weight = "bolder"), locations = cells_column_labels(columns = c("Sector" , "ano1978" , "ano2003" , "ano2019" , "por1978" , "por2003" , "por2019"))) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_column_labels(columns = c("Sector" , "ano1978" , "ano2003" , "ano2019" , "por1978" , "por2003" , "por2019"))) %>%
    tab_style(style = cell_text(weight = "bold"), locations = cells_data(columns = c("ano1978" , "ano2003" , "ano2019" , "por1978" , "por2003" , "por2019"), rows = 8)) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_data(columns = c("Sector"))) %>%
    tab_style(style = cell_text(color = "firebrick" , style = "italic"), locations = cells_data(columns = c("Sector")))

p
gtsave(p , filename = "8. Distribución del empleo por sector de actividad.png" , path = "./Exports/")
}

# 9. Distribución del empleo por tipos de ocupaciones en España, 1978, 2003 y 2019. (falta por hacer)
{
aa <- df %>% filter(CICLO == 189) %>%
  mutate(ANO = 2019) %>%
  group_by(ACT1 , ANO) %>%
  summarise(Total = sum(FACTOREL , na.rm = FALSE)) %>%
  ungroup()

aa_tot <- aa %>% group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  ungroup() %>%
  select(ANO , Total) %>%
  add_row(ANO = c(1978 , 2003) , Total = c(12400500 , 16666100) , .before = 1) %>%
  pivot_wider(names_prefix = "ano" , names_from = ANO , values_from = Total) %>%
  mutate(por1978 = ano1978/ano1978, por2003 = ano2003/ano2003, por2019 = ano2019/ano2019 , Sector = "Total") %>%
  select(7 , 1:6)


aa1 <- aa %>% filter(ACT1 == 0) %>%
  select(ANO , Total) %>%
  add_row(ANO = c(1978 , 2003) , Total = c(2472100 , 934400) , .before = 1) %>%
  pivot_wider(names_prefix = "ano" , names_from = ANO , values_from = Total) %>%
  mutate(por1978 = ano1978/aa_tot$ano1978, por2003 = ano2003/aa_tot$ano2003, por2019 = ano2019/aa_tot$ano2019 , Sector = "Agricultura") %>%
  select(7 , 1:6)

aa2 <- aa %>% filter(ACT1 == 1 | ACT1 == 2 | ACT1 == 3) %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  ungroup() %>%
  select(ANO , Total) %>%
  add_row(ANO = c(1978 , 2003) , Total = c(3425000 , 3127900) , .before = 1) %>%
  pivot_wider(names_prefix = "ano" , names_from = ANO , values_from = Total) %>%
  mutate(por1978 = ano1978/aa_tot$ano1978, por2003 = ano2003/aa_tot$ano2003, por2019 = ano2019/aa_tot$ano2019 , Sector = "Industria") %>%
  select(7 , 1:6)

aa3 <- aa %>% filter(ACT1 == 4) %>%
  select(ANO , Total) %>%
  add_row(ANO = c(1978 , 2003) , Total = c(1234500 , 2010000) , .before = 1) %>%
  pivot_wider(names_prefix = "ano" , names_from = ANO , values_from = Total) %>%
  mutate(por1978 = ano1978/aa_tot$ano1978, por2003 = ano2003/aa_tot$ano2003, por2019 = ano2019/aa_tot$ano2019 , Sector = "Construcción") %>%
  select(7 , 1:6)

aa4 <- aa %>% filter(ACT1 == 5 | ACT1 == 6) %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  ungroup() %>%
  select(ANO , Total) %>%
  add_row(ANO = c(1978 , 2003) , Total = c(2956200 , 4689400) , .before = 1) %>%
  pivot_wider(names_prefix = "ano" , names_from = ANO , values_from = Total) %>%
  mutate(por1978 = ano1978/aa_tot$ano1978, por2003 = ano2003/aa_tot$ano2003, por2019 = ano2019/aa_tot$ano2019 , Sector = "Comercio, hostelería, transporte") %>%
  select(7 , 1:6)

aa5 <- aa %>% filter(ACT1 == 7) %>%
  select(ANO , Total) %>%
  add_row(ANO = c(1978 , 2003) , Total = c(415800 , 1724000) , .before = 1) %>%
  pivot_wider(names_prefix = "ano" , names_from = ANO , values_from = Total) %>%
  mutate(por1978 = ano1978/aa_tot$ano1978, por2003 = ano2003/aa_tot$ano2003, por2019 = ano2019/aa_tot$ano2019 , Sector = "Serv. financieros, serv. empresas") %>%
  select(7 , 1:6)

aa6 <- aa %>% filter(ACT1 == 8) %>%
  select(ANO , Total) %>%
  add_row(ANO = c(1978 , 2003) , Total = c(1131900 , 3047100) , .before = 1) %>%
  pivot_wider(names_prefix = "ano" , names_from = ANO , values_from = Total) %>%
  mutate(por1978 = ano1978/aa_tot$ano1978, por2003 = ano2003/aa_tot$ano2003, por2019 = ano2019/aa_tot$ano2019 , Sector = "Serv. públicos") %>%
  select(7 , 1:6)

aa7 <- aa %>% filter(ACT1 == 9) %>%
  select(ANO , Total) %>%
  add_row(ANO = c(1978 , 2003) , Total = c(765000 , 1133300) , .before = 1) %>%
  pivot_wider(names_prefix = "ano" , names_from = ANO , values_from = Total) %>%
  mutate(por1978 = ano1978/aa_tot$ano1978, por2003 = ano2003/aa_tot$ano2003, por2019 = ano2019/aa_tot$ano2019 , Sector = "Otros servicios") %>%
  select(7 , 1:6)

aa <- bind_rows(aa1 , aa2 , aa3 , aa4 , aa5 , aa6 , aa7 , aa_tot)
aa <- aa %>% mutate(ano1978 = ano1978/1000 , ano2003 = ano2003/1000 , ano2019 = ano2019/1000 , por1978 = por1978 , por2003 = por2003 , por2019 = por2019)

p <- aa %>% gt() %>%
    tab_header(title = md("**DISTRIBUCIÓN DEL EMPLEO POR SECTORES DE ACTIVIDAD EN ESPAÑA, 1978, 2003 Y 2019**")) %>%
    cols_label(Sector = "Sectores de actividad" , ano1978 = "1978" , ano2003 = "2003" , ano2019 = "2019" , por1978 = "1978" , por2003 = "2003" , por2019 = "2019") %>%
    fmt_percent(columns = c("por1978" , "por2003" , "por2019")) %>%
    tab_spanner(label = "Miles de personas", columns = c("ano1978" , "ano2003" , "ano2019"), gather = TRUE) %>%
    tab_spanner(label = "Porcentajes del total", columns = c("por1978" , "por2003" , "por2019"), gather = TRUE) %>%
    tab_source_note(source_note = md("***Fuente:** INE, Epa*<br>*Elaboración propia*")) %>%
    tab_style(style = cell_text(color = "firebrick", size = "large", align = "center"), locations = cells_title()) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_column_spanners(spanners = c("Miles de personas" , "Porcentajes del total"))) %>%
    tab_style(style = cell_text(color = "firebrick", size = "large", align = "center" , weight = "bolder"), locations = cells_column_spanners(spanners = c("Miles de personas" , "Porcentajes del total"))) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_title()) %>%
    tab_style(style = cell_text(color = "firebrick", size = "normal", align = "center" , weight = "bolder"), locations = cells_column_labels(columns = c("Sector" , "ano1978" , "ano2003" , "ano2019" , "por1978" , "por2003" , "por2019"))) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_column_labels(columns = c("Sector" , "ano1978" , "ano2003" , "ano2019" , "por1978" , "por2003" , "por2019"))) %>%
    tab_style(style = cell_text(weight = "bold"), locations = cells_data(columns = c("ano1978" , "ano2003" , "ano2019" , "por1978" , "por2003" , "por2019"), rows = 8)) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_data(columns = c("Sector"))) %>%
    tab_style(style = cell_text(color = "firebrick" , style = "italic"), locations = cells_data(columns = c("Sector")))

p
#gtsave(p , filename = "9. Distribución del empleo por sector de actividad.png" , path = "./Exports/")
}

# 10. Distribución del empleo por situación profesional
{
aa <- df %>% filter(CICLO == 189) %>%
  mutate(ANO = 2019) %>%
  group_by(SITU , ANO) %>%
  summarise(Total = sum(FACTOREL , na.rm = FALSE)) %>%
  ungroup() %>%
  filter(!SITU == is.na(SITU))

aa_tot <- aa %>% group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  ungroup() %>%
  select(ANO , Total) %>%
  add_row(ANO = c(1978 , 2003) , Total = c(12381800 , 16665900) , .before = 1) %>%
  pivot_wider(names_prefix = "ano" , names_from = ANO , values_from = Total) %>%
  mutate(por1978 = ano1978/ano1978, por2003 = ano2003/ano2003, por2019 = ano2019/ano2019 , Sector = "Total") %>%
  select(7 , 1:6)

aa1 <- aa %>% filter(SITU == 1) %>%
  select(ANO , Total) %>%
  add_row(ANO = c(1978 , 2003) , Total = c(427400 , 914800) , .before = 1) %>%
  pivot_wider(names_prefix = "ano" , names_from = ANO , values_from = Total) %>%
  mutate(por1978 = ano1978/aa_tot$ano1978, por2003 = ano2003/aa_tot$ano2003, por2019 = ano2019/aa_tot$ano2019 , Sector = "Empleadores") %>%
  select(7 , 1:6)

aa3 <- aa %>% filter(SITU == 3) %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  ungroup() %>%
  select(ANO , Total) %>%
  add_row(ANO = c(1978 , 2003) , Total = c(2140100 , 1888800) , .before = 1) %>%
  pivot_wider(names_prefix = "ano" , names_from = ANO , values_from = Total) %>%
  mutate(por1978 = ano1978/aa_tot$ano1978, por2003 = ano2003/aa_tot$ano2003, por2019 = ano2019/aa_tot$ano2019 , Sector = "Autónomos o empresario sin asalariados") %>%
  select(7 , 1:6)

aa6 <- aa %>% filter(SITU == 6) %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  ungroup() %>%
  select(ANO , Total) %>%
  add_row(ANO = c(1978 , 2003) , Total = c(1039400 , 272100) , .before = 1) %>%
  pivot_wider(names_prefix = "ano" , names_from = ANO , values_from = Total) %>%
  mutate(por1978 = ano1978/aa_tot$ano1978, por2003 = ano2003/aa_tot$ano2003, por2019 = ano2019/aa_tot$ano2019 , Sector = "Ayuda en empresa o negocio familiar") %>%
  select(7 , 1:6)

aa7 <- aa %>% filter(SITU == 7) %>%
  select(ANO , Total) %>%
  add_row(ANO = c(1978 , 2003) , Total = c(1484900 , 2716200) , .before = 1) %>%
  pivot_wider(names_prefix = "ano" , names_from = ANO , values_from = Total) %>%
  mutate(por1978 = ano1978/aa_tot$ano1978, por2003 = ano2003/aa_tot$ano2003, por2019 = ano2019/aa_tot$ano2019 , Sector = "Asalariado sector público") %>%
  select(7 , 1:6)

aa8 <- aa %>% filter(SITU == 8) %>%
  select(ANO , Total) %>%
  add_row(ANO = c(1978 , 2003) , Total = c(7277900 , 10858100) , .before = 1) %>%
  pivot_wider(names_prefix = "ano" , names_from = ANO , values_from = Total) %>%
  mutate(por1978 = ano1978/aa_tot$ano1978, por2003 = ano2003/aa_tot$ano2003, por2019 = ano2019/aa_tot$ano2019 , Sector = "Asalariado sector privado") %>%
  select(7 , 1:6)

aa9 <- aa %>% filter(SITU == 9 | SITU == 5) %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  select(ANO , Total) %>%
  add_row(ANO = c(1978 , 2003) , Total = c(12100 , 15900) , .before = 1) %>%
  pivot_wider(names_prefix = "ano" , names_from = ANO , values_from = Total) %>%
  mutate(por1978 = ano1978/aa_tot$ano1978, por2003 = ano2003/aa_tot$ano2003, por2019 = ano2019/aa_tot$ano2019 , Sector = "Miembro de cooperativa u otra situación") %>%
  select(7 , 1:6)

aa1 <- bind_rows(aa1 , aa3 , aa6 , aa7 , aa8 , aa9 , aa_tot)
aa1 <- aa1 %>% mutate(ano1978 = ano1978/1000 , ano2003 = ano2003/1000 , ano2019 = ano2019/1000 , por1978 = por1978 , por2003 = por2003 , por2019 = por2019)

options(digits = 4)

p <- aa1 %>% gt() %>%
    tab_header(title = md("**DISTRIBUCIÓN DEL EMPLEO POR SITUACIÓN PROFESIONAL EN ESPAÑA, 1978, 2003 Y 2019**")) %>%
    cols_label(Sector = "Situación profesional" , ano1978 = "1978" , ano2003 = "2003" , ano2019 = "2019" , por1978 = "1978" , por2003 = "2003" , por2019 = "2019") %>%
    fmt_percent(columns = c("por1978" , "por2003" , "por2019")) %>%
    tab_spanner(label = "Miles de personas", columns = c("ano1978" , "ano2003" , "ano2019"), gather = TRUE) %>%
    tab_spanner(label = "Porcentajes del total", columns = c("por1978" , "por2003" , "por2019"), gather = TRUE) %>%
    tab_source_note(source_note = md("***Fuente:** INE, Epa*<br>*Elaboración propia*")) %>%
    tab_style(style = cell_text(color = "firebrick", size = "large", align = "center"), locations = cells_title()) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_column_spanners(spanners = c("Miles de personas" , "Porcentajes del total"))) %>%
    tab_style(style = cell_text(color = "firebrick", size = "large", align = "center" , weight = "bolder"), locations = cells_column_spanners(spanners = c("Miles de personas" , "Porcentajes del total"))) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_title()) %>%
    tab_style(style = cell_text(color = "firebrick", size = "normal", align = "center" , weight = "bolder"), locations = cells_column_labels(columns = c("Sector" , "ano1978" , "ano2003" , "ano2019" , "por1978" , "por2003" , "por2019"))) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_column_labels(columns = c("Sector" , "ano1978" , "ano2003" , "ano2019" , "por1978" , "por2003" , "por2019"))) %>%
    tab_style(style = cell_text(weight = "bold"), locations = cells_data(columns = c("ano1978" , "ano2003" , "ano2019" , "por1978" , "por2003" , "por2019"), rows = 7)) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_data(columns = c("Sector"))) %>%
    tab_style(style = cell_text(color = "firebrick" , style = "italic"), locations = cells_data(columns = c("Sector")))

p
gtsave(p , filename = "10. Distribución del empleo por situación profesional.png" , path = "./Exports/")
}

# 11. Distribución del empleo según la relación con la persona de referencia
{
aa <- df %>% filter(CICLO == 189) %>%
  filter(AOI == 5 | AOI == 6) %>%
  mutate(ANO = 2019) %>%
  group_by(RELPP1 , SEXO1 , ANO) %>%
  summarise(Total = sum(FACTOREL , na.rm = FALSE)) %>%
  ungroup() %>%
  mutate(RELPP1 = gsub(pattern = 4 , replacement = 4 , x = RELPP1)) %>%
  mutate(RELPP1 = gsub(pattern = 5 , replacement = 4 , x = RELPP1)) %>%
  mutate(RELPP1 = gsub(pattern = 6 , replacement = 4 , x = RELPP1)) %>%
  mutate(RELPP1 = gsub(pattern = 7 , replacement = 4 , x = RELPP1)) %>%
  mutate(RELPP1 = gsub(pattern = 8 , replacement = 4 , x = RELPP1)) %>%
  mutate(RELPP1 = gsub(pattern = 9 , replacement = 4 , x = RELPP1)) %>%
  group_by(ANO , SEXO1 , RELPP1) %>%
  summarise("Total" = sum(Total , na.rm = FALSE)) %>%
  ungroup() %>%
  arrange(RELPP1)

newRow1 <- data.frame(ANO = c(1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978) , RELPP1 = as.character(c(1 , 1 , 2 , 2 , 3 , 3 , 4 , 4)) , Total = c(259200 , 8300 , 400 , 40700 , 295300 , 211800 , 43900 , 18200) , SEXO1 = c("Hombre" , "Mujer" , "Hombre" , "Mujer" , "Hombre" , "Mujer" , "Hombre" , "Mujer"))
newRow2 <- data.frame(ANO = c(2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003) , RELPP1 = as.character(c(1 , 1 , 2 , 2 , 3 , 3 , 4 , 4)) , Total = c(389500 , 185800 , 40700 , 675200 , 529600 , 525300 , 59700 , 54600) , SEXO1 = c("Hombre" , "Mujer" , "Hombre" , "Mujer" , "Hombre" , "Mujer" , "Hombre" , "Mujer"))

aa <- bind_rows(aa , newRow1 , newRow2)
aa <- aa %>% mutate(SEXO1 = gsub(pattern = "Hombre" , replacement = "hombre" , x = SEXO1)) %>%
  mutate(SEXO1 = gsub(pattern = "Mujer" , replacement = "mujer" , x = SEXO1)) %>%
  mutate(RELPP1 = gsub(pattern = "1" , replacement = "Persona referencia" , x = RELPP1)) %>%
  mutate(RELPP1 = gsub(pattern = "2" , replacement = "Cónyuge" , x = RELPP1)) %>%
  mutate(RELPP1 = gsub(pattern = "3" , replacement = "Hijo" , x = RELPP1)) %>%
  mutate(RELPP1 = gsub(pattern = "4" , replacement = "Otras situaciones" , x = RELPP1)) %>%
  unite(RELPP1, RELPP1:SEXO1, sep = " ") %>%
  pivot_wider(names_from = ANO , values_from = Total , names_prefix = "ano")
newRow3 <- data.frame(RELPP1 = "Total" , ano2003 = sum(aa$ano2003 , na.rm = FALSE) , ano1978 = sum(aa$ano1978 , na.rm = FALSE) , ano2019 = sum(aa$ano2019 , na.rm = FALSE))
aa <- bind_rows(aa , newRow3)
aa <- aa %>% mutate(por1978 = ano1978/newRow3$ano1978 , por2003 = ano2003/newRow3$ano2003 , por2019 = ano2019/newRow3$ano2019) %>%
  mutate(ano1978 = ano1978/1000 , ano2003 = ano2003/1000 , ano2019 = ano2019/1000 , por1978 = por1978 , por2003 = por2003 , por2019 = por2019) %>%
  select(RELPP1 , ano1978 , ano2003 , ano2019 , por1978 , por2003 , por2019)

options(digits = 4)

p <- aa %>% gt() %>%
    tab_header(title = md("**DISTRIBUCIÓN DEL EMPLEO SEGÚN LA RELACIÓN CON LA PERSONA DE REFERENCIA EN ESPAÑA, 1978, 2003 Y 2019**")) %>%
    cols_label(RELPP1 = "Relación con la persona de referencia y sexo" , ano1978 = "1978" , ano2003 = "2003" , ano2019 = "2019" , por1978 = "1978" , por2003 = "2003" , por2019 = "2019") %>%
    fmt_percent(columns = c("por1978" , "por2003" , "por2019")) %>%
    tab_spanner(label = "Miles de personas", columns = c("ano1978" , "ano2003" , "ano2019"), gather = TRUE) %>%
    tab_spanner(label = "Porcentajes del total", columns = c("por1978" , "por2003" , "por2019"), gather = TRUE) %>%
    tab_source_note(source_note = md("***Fuente:** INE, Epa*<br>*Elaboración propia*")) %>%
    tab_style(style = cell_text(color = "firebrick", size = "large", align = "center"), locations = cells_title()) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_column_spanners(spanners = c("Miles de personas" , "Porcentajes del total"))) %>%
    tab_style(style = cell_text(color = "firebrick", size = "large", align = "center" , weight = "bolder"), locations = cells_column_spanners(spanners = c("Miles de personas" , "Porcentajes del total"))) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_title()) %>%
    tab_style(style = cell_text(color = "firebrick", size = "normal", align = "center" , weight = "bolder"), locations = cells_column_labels(columns = c("RELPP1" , "ano1978" , "ano2003" , "ano2019" , "por1978" , "por2003" , "por2019"))) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_column_labels(columns = c("RELPP1" , "ano1978" , "ano2003" , "ano2019" , "por1978" , "por2003" , "por2019"))) %>%
    tab_style(style = cell_text(weight = "bold"), locations = cells_data(columns = c("ano1978" , "ano2003" , "ano2019" , "por1978" , "por2003" , "por2019"), rows = 9)) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_data(columns = c("RELPP1"))) %>%
    tab_style(style = cell_text(color = "firebrick" , style = "italic"), locations = cells_data(columns = c("RELPP1")))

p
gtsave(p , filename = "11. Distribución del empleo según la relación con la persona de referencia.png" , path = "./Exports/")
}

# 12. Tasa de paro según la persona de referencia
{
  ii <- 133

zz1 <- df %>% filter(CICLO == ii , NIVEL == 1) %>%
  filter(AOI == 3 | AOI == 4 | AOI == 5 | AOI == 6) %>%
  mutate(RELPP1 = gsub(pattern = 4 , replacement = 4 , x = RELPP1)) %>%
  mutate(RELPP1 = gsub(pattern = 5 , replacement = 4 , x = RELPP1)) %>%
  mutate(RELPP1 = gsub(pattern = 6 , replacement = 4 , x = RELPP1)) %>%
  mutate(RELPP1 = gsub(pattern = 7 , replacement = 4 , x = RELPP1)) %>%
  mutate(RELPP1 = gsub(pattern = 8 , replacement = 4 , x = RELPP1)) %>%
  mutate(RELPP1 = gsub(pattern = 9 , replacement = 4 , x = RELPP1)) %>%
  group_by(CICLO , SEXO1 , RELPP1) %>%
  summarise("Total" = sum(FACTOREL , na.rm = FALSE)) %>%
  ungroup() %>%
  arrange(RELPP1)

aa <- df %>% filter(CICLO == ii , NIVEL == 1) %>%
  filter(AOI == 5 | AOI == 6) %>%
  mutate(RELPP1 = gsub(pattern = 4 , replacement = 4 , x = RELPP1)) %>%
  mutate(RELPP1 = gsub(pattern = 5 , replacement = 4 , x = RELPP1)) %>%
  mutate(RELPP1 = gsub(pattern = 6 , replacement = 4 , x = RELPP1)) %>%
  mutate(RELPP1 = gsub(pattern = 7 , replacement = 4 , x = RELPP1)) %>%
  mutate(RELPP1 = gsub(pattern = 8 , replacement = 4 , x = RELPP1)) %>%
  mutate(RELPP1 = gsub(pattern = 9 , replacement = 4 , x = RELPP1)) %>%
  group_by(CICLO , SEXO1 , RELPP1) %>%
  summarise("Porcentaje" = sum(FACTOREL , na.rm = FALSE)) %>%
  ungroup() %>%
  arrange(RELPP1) %>%
  mutate(Porcentaje = Porcentaje*100/zz1$Total)

x <- c(153 , 173 , 189)

for (ii in x) {
    zz1 <- df %>% filter(CICLO == ii , NIVEL == 1) %>%
  filter(AOI == 3 | AOI == 4 | AOI == 5 | AOI == 6) %>%
  mutate(RELPP1 = gsub(pattern = 4 , replacement = 4 , x = RELPP1)) %>%
  mutate(RELPP1 = gsub(pattern = 5 , replacement = 4 , x = RELPP1)) %>%
  mutate(RELPP1 = gsub(pattern = 6 , replacement = 4 , x = RELPP1)) %>%
  mutate(RELPP1 = gsub(pattern = 7 , replacement = 4 , x = RELPP1)) %>%
  mutate(RELPP1 = gsub(pattern = 8 , replacement = 4 , x = RELPP1)) %>%
  mutate(RELPP1 = gsub(pattern = 9 , replacement = 4 , x = RELPP1)) %>%
  group_by(CICLO , SEXO1 , RELPP1) %>%
  summarise("Total" = sum(FACTOREL , na.rm = FALSE)) %>%
  ungroup() %>%
  arrange(RELPP1)
   aa1 <- df %>% filter(CICLO == ii , NIVEL == 1) %>%
  filter(AOI == 5 | AOI == 6) %>%
  mutate(RELPP1 = gsub(pattern = 4 , replacement = 4 , x = RELPP1)) %>%
  mutate(RELPP1 = gsub(pattern = 5 , replacement = 4 , x = RELPP1)) %>%
  mutate(RELPP1 = gsub(pattern = 6 , replacement = 4 , x = RELPP1)) %>%
  mutate(RELPP1 = gsub(pattern = 7 , replacement = 4 , x = RELPP1)) %>%
  mutate(RELPP1 = gsub(pattern = 8 , replacement = 4 , x = RELPP1)) %>%
  mutate(RELPP1 = gsub(pattern = 9 , replacement = 4 , x = RELPP1)) %>%
  group_by(CICLO , SEXO1 , RELPP1) %>%
  summarise("Porcentaje" = sum(FACTOREL , na.rm = FALSE)) %>%
  ungroup() %>%
  arrange(RELPP1) %>%
  mutate(Porcentaje = Porcentaje*100/zz1$Total)

    aa <- bind_rows(aa , aa1)
    }

aa <- aa %>% mutate(Año = gsub(x = CICLO , pattern = 133 , replacement = "2005")) %>%
  mutate(Año = gsub(x = Año , pattern = 153 , replacement = "2010")) %>%
  mutate(Año = gsub(x = Año , pattern = 173 , replacement = "2015")) %>%
  mutate(Año = gsub(x = Año , pattern = 189 , replacement = "2019")) %>%
  mutate(Relacion = RELPP1 , Sexo = SEXO1) %>%
  select(Año , Relacion , Sexo , Porcentaje)

newRow1 <- data.frame(Año = as.character(c(1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978)) , Relacion = as.character(c(1 , 1 , 2 , 2 , 3 , 3 , 4 , 4)) , Porcentaje = c(3.9 , 2.2 , 8.4 , 2.7 , 13.2 , 14.1 , 6.7 , 4.2) , Sexo = c("Hombre" , "Mujer" , "Hombre" , "Mujer" , "Hombre" , "Mujer" , "Hombre" , "Mujer"))
newRow2 <- data.frame(Año = as.character(c(2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003)) , Relacion = as.character(c(1 , 1 , 2 , 2 , 3 , 3 , 4 , 4)) , Porcentaje = c(5.8 , 13.4 , 6 , 17.3 , 16.2 , 23.2 , 12.3 , 17.9) , Sexo = c("Hombre" , "Mujer" , "Hombre" , "Mujer" , "Hombre" , "Mujer" , "Hombre" , "Mujer"))

aa_def <- bind_rows(aa , newRow1 , newRow2)
aa_def <- aa_def %>% mutate(Sexo = gsub(pattern = "Hombre" , replacement = "hombre" , x = Sexo)) %>%
  mutate(Sexo = gsub(pattern = "Mujer" , replacement = "mujer" , x = Sexo)) %>%
  mutate(Relacion = gsub(pattern = "1" , replacement = "Persona referencia" , x = Relacion)) %>%
  mutate(Relacion = gsub(pattern = "2" , replacement = "Cónyuge" , x = Relacion)) %>%
  mutate(Relacion = gsub(pattern = "3" , replacement = "Hijo" , x = Relacion)) %>%
  mutate(Relacion = gsub(pattern = "4" , replacement = "Otras situaciones" , x = Relacion)) %>%
  unite(Relacion, Relacion:Sexo, sep = "
 ")
aa_def <- aa_def %>% filter(Año %in% c("1978" , "2003" , "2019"))

p <- ggplot(data = aa_def , mapping = aes(x = Relacion , y = Porcentaje , fill = Año)) +
  geom_col(position = "dodge") +
  labs(title = "TASA DE PARO SEGÚN LA RELACIÓN CON LA PERSONA DE REFERENCIA, ESPAÑA, 1978, 2003 Y 2019" , caption = "Fuente: INE, EPA. Elaboración propia" , x = NULL , y = NULL) +
  theme(plot.caption = element_text(face = "italic", colour = "gray24") ,
    plot.title = element_text(size = 14 , face = "bold", colour = "firebrick") ,
    panel.background = element_rect(fill = "slategray", colour = "snow4")) +
  scale_x_discrete() +
  scale_y_continuous(breaks = seq(0 , 30 , 5) , labels = c("0%" , "5%" , "10%" , "15%" , "20%" , "25%" , "30%")) +
  scale_fill_brewer(palette = "Reds")
options(digits = 2)
ggplotly(p , tooltip = c("Año" ,  "Porcentaje"))
ggsave(filename = "./Exports/12.1 Tasa de paro según la persona de referencia.png" , plot = p , width = 10.5 , height = 6)

p <- ggplot(data = aa_def , mapping = aes(x = Relacion , y = Porcentaje , fill = Año)) +
  geom_col(position = "dodge") +
  labs(title = "TASA DE PARO SEGÚN LA RELACIÓN CON LA PERSONA DE REFERENCIA, ESPAÑA, 1978, 2003 Y 2019" , caption = "Fuente: INE, EPA. Elaboración propia" , x = NULL , y = NULL) +
  theme(plot.caption = element_text(face = "italic", colour = "gray24") ,
    plot.title = element_text(size = 14 , face = "bold", colour = "firebrick") ,
    panel.background = element_rect(fill = "gray80", colour = "snow4")) +
  scale_x_discrete() +
  scale_y_continuous(breaks = seq(0 , 30 , 5) , labels = c("0%" , "5%" , "10%" , "15%" , "20%" , "25%" , "30%")) +
  scale_fill_brewer(palette = "Reds")
options(digits = 2)
ggplotly(p , tooltip = c("Año" ,  "Porcentaje"))
ggsave(filename = "./Exports/12.2 Tasa de paro según la persona de referencia.png" , plot = p , width = 10.5 , height = 6)
}

# 13. Tasa de paro por sexo y comunidad autónoma de residencia
{
  ii <- 189

zz1 <- df %>% filter(CICLO == ii , NIVEL == 1) %>%
  filter(AOI == 3 | AOI == 4 | AOI == 5 | AOI == 6) %>%
  group_by(CICLO , SEXO1 , CCAA) %>%
  summarise("Total" = sum(FACTOREL , na.rm = FALSE)) %>%
  ungroup()

aa <- df %>% filter(CICLO == ii , NIVEL == 1) %>%
  filter(AOI == 5 | AOI == 6) %>%
  group_by(CICLO , SEXO1 , CCAA) %>%
  summarise("Porcentaje" = sum(FACTOREL , na.rm = FALSE)) %>%
  ungroup() %>%
  mutate(Porcentaje = Porcentaje*100/zz1$Total)

x <- c(153 , 173 , 133)

  for (ii in x) {
    zz1 <- df %>% filter(CICLO == ii , NIVEL == 1) %>%
  filter(AOI == 3 | AOI == 4 | AOI == 5 | AOI == 6) %>%
  group_by(CICLO , SEXO1 , CCAA) %>%
  summarise("Total" = sum(FACTOREL , na.rm = FALSE)) %>%
  ungroup()
   aa1 <- df %>% filter(CICLO == ii , NIVEL == 1) %>%
  filter(AOI == 5 | AOI == 6) %>%
  group_by(CICLO , SEXO1 , CCAA) %>%
  summarise("Porcentaje" = sum(FACTOREL , na.rm = FALSE)) %>%
  ungroup() %>%
  mutate(Porcentaje = Porcentaje*100/zz1$Total)

    aa <- bind_rows(aa , aa1)
    }

aa <- aa %>% mutate(Año = gsub(x = CICLO , pattern = 133 , replacement = "2005")) %>%
  mutate(Año = gsub(x = Año , pattern = 153 , replacement = "2010")) %>%
  mutate(Año = gsub(x = Año , pattern = 173 , replacement = "2015")) %>%
  mutate(Año = gsub(x = Año , pattern = 189 , replacement = "2019")) %>%
  select(Año , CCAA , SEXO1 , Porcentaje)

newRow1 <- data.frame(Año = "1978" , CCAA = c(1:17,51,52) , Porcentaje = c(11.9 , 3.5 , 4.3 , 4.2 , 9.2 , 5.3 , 3.9 , 5 , 5.8 , 4.1 , 9.8 , 3.2 , 7.3 , 4.1 , 2.8 , 5.2 , 2.2 , 0 , 0) , SEXO1 = "Hombre")
newRow2 <- data.frame(Año = "1978" , CCAA = c(1:17,51,52) , Porcentaje = c(12.3 , 7.2 , 5.1 , 4.2 , 10.2 , 3.8 , 7.4 , 8.8 , 6.4 , 5.3 , 10 , 2.3 , 9.1 , 9.9 , 7 , 11.8 , 2.8 , 0 , 0) , SEXO1 = "Mujer")
newRow3 <- data.frame(Año = "2003" , CCAA = c(1:17,51,52) , Porcentaje = c(3.7 , 4.6 , 9 , 7.5 , 10.5 , 8.6 , 6.9 , 7 , 7.4 , 8.3 , 15.4 , 9.3 , 6.5 , 7.4 , 3.7 , 6.8 , 4.1 , 0 , 0) , SEXO1 = "Hombre")
newRow4 <- data.frame(Año = "2003" , CCAA = c(1:17,51,52) , Porcentaje = c(30.1 , 12.4 , 19.5 , 11.8 , 17.6 , 18.5 , 21.7 , 21.9 , 12.6 , 16.9 , 32.7 , 19.8 , 12.6 , 15.8 , 8.4 , 14.4 , 12.9 , 0 , 0) , SEXO1 = "Mujer")

aa_def <- bind_rows(aa , newRow1 , newRow2 , newRow3 , newRow4)
aa_def <- aa_def %>% mutate(CCAA = gsub(pattern = 10 , replacement = "Comunitat Valenciana" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 11 , replacement = "Extremadura" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 12 , replacement = "Galicia" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 13 , replacement = "Madrid, Comunidad de" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 14 , replacement = "Murcia, Región de" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 15 , replacement = "Navarra, Comunidad Foral de" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 16 , replacement = "País Vasco" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 17 , replacement = "Rioja, La" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 51 , replacement = "Ceuta" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 52 , replacement = "Melilla" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 1 , replacement = "Andalucía" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 2 , replacement = "Aragón" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 3 , replacement = "Asturias, Principado de" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 4 , replacement = "Balears, Illes" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 5 , replacement = "Canarias" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 6 , replacement = "Cantabria" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 7 , replacement = "Castilla y León" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 8 , replacement = "Castilla-La Mancha" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 9 , replacement = "Cataluña" , x = CCAA))

aa_def <- aa_def %>% filter(Año %in% c("1978" , "2003" , "2019")) %>%
  pivot_wider(names_from = Año , names_prefix = "ano" , values_from = Porcentaje)

aa_Hombre <- aa_def %>% filter(SEXO1 == "Hombre") %>%
  arrange(ano2019) %>%
  pivot_longer(cols = 3:5 , names_to = "Año" , values_to = "Porcentaje") %>%
  mutate(CCAA = forcats::as_factor(CCAA)) %>%
  select(CCAA , Año , Porcentaje.hombre = Porcentaje)

aa_Mujer <- aa_def %>% filter(SEXO1 == "Mujer") %>%
  pivot_longer(cols = 3:5 , names_to = "Año" , values_to = "Porcentaje") %>%
  mutate(CCAA = forcats::as_factor(CCAA)) %>%
  select(CCAA , Año , Porcentaje.mujer = Porcentaje)

aa_def <- left_join(aa_Hombre , aa_Mujer)
aa_def <- aa_def %>% pivot_longer(cols = 3:4 , names_to = "Sexo" , values_to = "Porcentaje") %>%
  mutate(Año = gsub(x = Año , pattern = "ano" , replacement = "")) %>%
  mutate(Sexo = gsub(x = Sexo , pattern = "Porcentaje.hombre" , replacement = "Hombre")) %>%
  mutate(Sexo = gsub(x = Sexo , pattern = "Porcentaje.mujer" , replacement = "Mujer")) %>%
  mutate(CCAA = forcats::as_factor((CCAA)))

p <- ggplot(data = aa_def , mapping = aes(x = CCAA , y = Porcentaje , fill = Año)) +
  geom_col(position = "dodge") +
  facet_grid(cols = vars(Sexo)) +
  labs(title = "TASA DE PARO POR SEXO Y COMUNIDAD AUTÓNOMA DE RESIDENCIA, ESPAÑA, 1978, 2003 Y 2019" , caption = "Fuente: INE, EPA. Elaboración propia" , x = NULL , y = NULL) +
  theme(plot.caption = element_text(face = "italic", colour = "gray24") ,
    plot.title = element_text(size = 14 , face = "bold", colour = "firebrick") ,
    panel.background = element_rect(fill = "slategray", colour = "snow4") ,
    axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete() +
  scale_y_continuous(breaks = seq(0 , 40 , 2)) +
  scale_fill_brewer(palette = "Reds") +
  scale_color_brewer(palette = "Blues")
options(digits = 2)
ggplotly(p , tooltip = c("Año" ,  "Porcentaje" , "CCAA"))
ggsave(filename = "./Exports/13.1 Tasa de paro por sexo y comunidad autónoma de residencia.png" , plot = p , width = 16 , height = 5)

p <- ggplot(data = aa_def , mapping = aes(x = CCAA , y = Porcentaje , fill = Año)) +
  geom_col(position = "dodge") +
  facet_grid(cols = vars(Sexo)) +
  labs(title = "TASA DE PARO POR SEXO Y COMUNIDAD AUTÓNOMA DE RESIDENCIA, ESPAÑA, 1978, 2003 Y 2019" , caption = "Fuente: INE, EPA. Elaboración propia" , x = NULL , y = NULL) +
  theme(plot.caption = element_text(face = "italic", colour = "gray24") ,
    plot.title = element_text(size = 14 , face = "bold", colour = "firebrick") ,
    panel.background = element_rect(fill = "gray80", colour = "snow4") ,
    axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete() +
  scale_y_continuous(breaks = seq(0 , 40 , 2)) +
  scale_fill_brewer(palette = "Reds") +
  scale_color_brewer(palette = "Blues")
options(digits = 2)
ggplotly(p , tooltip = c("Año" ,  "Porcentaje" , "CCAA"))
ggsave(filename = "./Exports/13.2 Tasa de paro por sexo y comunidad autónoma de residencia.png" , plot = p , width = 16 , height = 5)
}

# Distribución de los hogares españoles según tipo, 1978, 2003 y 2019. (falta)



# Distribución de los hogares españoles según la actividad, 1978, 2003 y 2019. (falta)



# Tasa de paro total y tasa de incidencia del paro de los hogares activos españoles, 1978, 2003 y 2019. (falta)




