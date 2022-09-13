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
df <- df %>% mutate(SEXO1 = gsub(x = SEXO1 , pattern = 1 , replacement = "Home")) %>%
  mutate(SEXO1 = gsub(x = SEXO1 , pattern = 6 , replacement = "Dona"))
}

# 1. Taula majors
{
aa <- df %>% filter(CICLO == 189) %>%
    mutate(ANO = 2019) %>%
    group_by(SEXO1 , NIVEL , ANO) %>%
    summarise("personas" = sum(FACTOREL , na.rm = FALSE)) %>%
    ungroup()
newRow1 <- data.frame(SEXO1 = c("Dona" , "Dona" , "Home" , "Home") , NIVEL = c(1 , 2 , 1 , 2) , ANO = as.numeric(c("2003" , "2003" , "2003" , "2003")) , personas = as.numeric(c("17579800" , "3071700" , "16595700" , "3276300")))
newRow2 <- data.frame(SEXO1 = c("Dona" , "Dona" , "Home" , "Home") , NIVEL = c(1 , 2 , 1 , 2) , ANO = as.numeric(c("1978" , "1978" , "1978" , "1978")) , personas = as.numeric(c("13489900" , "4871500" , "12522300" , "5122200")))
aa <- bind_rows(aa , newRow1 , newRow2) %>%
    pivot_wider(names_from = SEXO1 , values_from = personas)
aa <- aa %>% mutate(Total = Home + Dona) %>%
    pivot_longer(names_to = "SEXO1" , values_to = "personas" , cols = 3:5)
x <- as.numeric(c(1978 , 2003 , 2019))
for (ii in x) {
    zz <- aa %>% filter(NIVEL == 1) %>%
    pivot_wider(names_from = SEXO1 , values_from = personas) %>%
    arrange(-desc(ANO)) %>%
    select(ANO , Total , Home , Dona) %>%
    filter(ANO == ii) %>%
    mutate(Total = Total/1000 , Home = Home/1000 , Dona = Dona/1000)

    assign(x = paste0("zz" , {ii}) , value = zz)
}
aa1 <- aa %>% filter(NIVEL == 1) %>%
    pivot_wider(names_from = SEXO1 , values_from = personas) %>%
    mutate(ANO = (as.character(ANO))) %>%
    select(ANO , Total , Home , Dona) %>%
    arrange(ANO) %>%
    mutate(Total = Total/1000 , Home = Home/1000 , Dona = Dona/1000)
dif19.03 <- data.frame(ANO = "Diferència 2019 - 2003" , Total = zz2019$Total - zz2003$Total , Home = zz2019$Home - zz2003$Home , Dona = zz2019$Dona - zz2003$Dona)
dif_por19.03 <- data.frame(ANO = "Diferència % 2019 - 2003" , Total = (zz2019$Total - zz2003$Total)/zz2003$Total , Home = (zz2019$Home - zz2003$Home)/zz2003$Home , Dona = (zz2019$Dona - zz2003$Dona)/zz2003$Dona)
dif19.78 <- data.frame(ANO = "Diferència 2019 - 1978" , Total = zz2019$Total - zz1978$Total , Home = zz2019$Home - zz1978$Home , Dona = zz2019$Dona - zz1978$Dona)
dif_por19.78 <- data.frame(ANO = "Diferència % 2019 - 1978" , Total = (zz2019$Total - zz1978$Total)/zz1978$Total , Home = (zz2019$Home - zz1978$Home)/zz1978$Home , Dona = (zz2019$Dona - zz1978$Dona)/zz1978$Dona)
aa1 <- bind_rows(aa1 , dif19.78 , dif_por19.78 , dif19.03 , dif_por19.03)

options(digits = 6)

p <- aa1 %>% gt() %>%
    tab_header(title = md("**QUADRE 1: EVOLUCIÓ DE LA POBLACIÓ DE 16 ANYS O MÉS EN ESPANYA, 1978, 2003 Y 2019** *(En milers)*")) %>%
    cols_label(ANO = "Any" , Total = "Ambdós sexes" , Home = "Homes" , Dona = "Dones") %>%
    fmt_percent(columns = c("Total" , "Home" , "Dona") , rows = c(5 , 7)) %>%
    tab_source_note(source_note = md("***Font:** INE, EPA*<br>*Elaboració pròpia*")) %>%
    tab_style(style = cell_text(color = "firebrick", size = "large", align = "center"), locations = cells_title()) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_title()) %>%
    tab_style(style = cell_text(color = "firebrick", size = "normal", align = "center" , weight = "bolder"), locations = cells_column_labels(columns = c("ANO" , "Total" , "Home" , "Dona"))) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_column_labels(columns = c("ANO" , "Total" , "Home" , "Dona"))) %>%
    tab_style(style = cell_text(weight = "bold"), locations = cells_data(columns = c("ANO" , "Total" , "Home" , "Dona"), rows = c(5 , 7))) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_data(columns = "ANO")) %>%
    tab_style(style = cell_text(color = "firebrick" , size = "normal" , align = "center" , weight = "bolder"), locations = cells_data(columns = "ANO"))

p
gtsave(p , filename = "1. Taula majors.png" , path = "./ExportsVal/" , useragent = TRUE)
}

# 2. Taula menors
{
aa <- df %>% filter(CICLO == 189) %>%
    mutate(ANO = 2019) %>%
    group_by(SEXO1 , NIVEL , ANO) %>%
    summarise("personas" = sum(FACTOREL , na.rm = FALSE)) %>%
    ungroup()
newRow1 <- data.frame(SEXO1 = c("Dona" , "Dona" , "Home" , "Home") , NIVEL = c(1 , 2 , 1 , 2) , ANO = as.numeric(c("2003" , "2003" , "2003" , "2003")) , personas = as.numeric(c("17579800" , "3071700" , "16595700" , "3276300")))
newRow2 <- data.frame(SEXO1 = c("Dona" , "Dona" , "Home" , "Home") , NIVEL = c(1 , 2 , 1 , 2) , ANO = as.numeric(c("1978" , "1978" , "1978" , "1978")) , personas = as.numeric(c("13489900" , "4871500" , "12522300" , "5122200")))
aa <- bind_rows(aa , newRow1 , newRow2) %>%
    pivot_wider(names_from = SEXO1 , values_from = personas)
aa <- aa %>% mutate(Total = Home + Dona) %>%
    pivot_longer(names_to = "SEXO1" , values_to = "personas" , cols = 3:5)
x <- as.numeric(c(1978 , 2003 , 2019))
for (ii in x) {
    zz <- aa %>% filter(NIVEL == 2) %>%
    pivot_wider(names_from = SEXO1 , values_from = personas) %>%
    arrange(-desc(ANO)) %>%
    select(ANO , Total , Home , Dona) %>%
    filter(ANO == ii) %>%
    mutate(Total = Total/1000 , Home = Home/1000 , Dona = Dona/1000)

    assign(x = paste0("zz" , {ii}) , value = zz)
}
aa1 <- aa %>% filter(NIVEL == 2) %>%
    pivot_wider(names_from = SEXO1 , values_from = personas) %>%
    mutate(ANO = (as.character(ANO))) %>%
    select(ANO , Total , Home , Dona) %>%
    arrange(ANO) %>%
    mutate(Total = Total/1000 , Home = Home/1000 , Dona = Dona/1000)
dif19.03 <- data.frame(ANO = "Diferència 2019 - 2003" , Total = zz2019$Total - zz2003$Total , Home = zz2019$Home - zz2003$Home , Dona = zz2019$Dona - zz2003$Dona)
dif_por19.03 <- data.frame(ANO = "Diferència % 2019 - 2003" , Total = (zz2019$Total - zz2003$Total)/zz2003$Total , Home = (zz2019$Home - zz2003$Home)/zz2003$Home , Dona = (zz2019$Dona - zz2003$Dona)/zz2003$Dona)
dif19.78 <- data.frame(ANO = "Diferència 2019 - 1978" , Total = zz2019$Total - zz1978$Total , Home = zz2019$Home - zz1978$Home , Dona = zz2019$Dona - zz1978$Dona)
dif_por19.78 <- data.frame(ANO = "Diferència % 2019 - 1978" , Total = (zz2019$Total - zz1978$Total)/zz1978$Total , Home = (zz2019$Home - zz1978$Home)/zz1978$Home , Dona = (zz2019$Dona - zz1978$Dona)/zz1978$Dona)
aa1 <- bind_rows(aa1 , dif19.78 , dif_por19.78 , dif19.03 , dif_por19.03)

p <- aa1 %>% gt() %>%
    tab_header(title = md("**QUADRE 2: EVOLUCIÓ DE LA POBLACIÓ MENOR DE 16 ANYS EN ESPANYA, 1978, 2003 Y 2019** *(En milers)*")) %>%
    cols_label(ANO = "Any" , Total = "Ambdós sexos" , Home = "Homes" , Dona = "Dones") %>%
    fmt_percent(columns = c("Total" , "Home" , "Dona") , rows = c(5 , 7) ) %>%
    tab_source_note(source_note = md("***Font:** INE, EPA*<br>*Elaboración propia*")) %>%
    tab_style(style = cell_text(color = "firebrick", size = "large", align = "center"), locations = cells_title()) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_title()) %>%
    tab_style(style = cell_text(color = "firebrick", size = "normal", align = "center" , weight = "bolder"), locations = cells_column_labels(columns = c("ANO" , "Total" , "Home" , "Dona"))) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_column_labels(columns = c("ANO" , "Total" , "Home" , "Dona"))) %>%
    tab_style(style = cell_text(weight = "bold"), locations = cells_data(columns = c("ANO" , "Total" , "Home" , "Dona"), rows = c(5 , 7))) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_data(columns = "ANO")) %>%
    tab_style(style = cell_text(color = "firebrick" , size = "normal" , align = "center" , weight = "bolder"), locations = cells_data(columns = "ANO"))

p
gtsave(p , filename = "2. Taula menors.png" , path = "./ExportsVal/")
}

# 3. Distribución población
{
y <- c("Home" , "Dona")

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

aa_def <- bind_rows(aa_Home , aa_Dona)
aa_def <- aa_def %>% mutate(Any = gsub(x = CICLO , pattern = 133 , replacement = "2005")) %>%
  mutate(Any = gsub(x = Any , pattern = 153 , replacement = "2010")) %>%
  mutate(Any = gsub(x = Any , pattern = 173 , replacement = "2015")) %>%
  mutate(Any = gsub(x = Any , pattern = 189 , replacement = "2019")) %>%
  mutate(Edat = EDAD5 , Percentatge = personas , Sexo = SEXO1) %>%
  select(Any , Edat , Percentatge , Sexo)

newRow1 <- data.frame(Any = as.character(c(1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978)) , Edat = c(16 , 20 , 25 , 30 , 35 , 40 , 45 , 50 , 55 , 60 , 65) , Percentatge = c(9.8 , 10.7 , 9.8 , 9.5 , 8.2 , 8.8 , 9.2 , 8.4 , 7 , 5.5 , 13.1) , Sexo = c("Home" , "Home" , "Home" , "Home" , "Home" , "Home" , "Home" , "Home" , "Home" , "Home" , "Home"))

newRow2 <- data.frame(Any = as.character(c(1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978)) , Edat = c(16 , 20 , 25 , 30 , 35 , 40 , 45 , 50 , 55 , 60 , 65) , Percentatge = c(8.9 , 9.9 , 9.1 , 8.7 , 7.5 , 8.4 , 8.7 , 8.1 , 7.1 , 6.2 , 17.4) , Sexo = c("Dona" , "Dona" , "Dona" , "Dona" , "Dona" , "Dona" , "Dona" , "Dona" , "Dona" , "Dona" , "Dona"))

newRow3 <- data.frame(Any = as.character(c(2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003)) , Edat = c(16 , 20 , 25 , 30 , 35 , 40 , 45 , 50 , 55 , 60 , 65) , Percentatge = c(5.8 , 8.9 , 10.6 , 10.6 , 10.2 , 9.1 , 8 , 7.3 , 6.7 , 5.5 , 17.3) , Sexo = c("Home" , "Home" , "Home" , "Home" , "Home" , "Home" , "Home" , "Home" , "Home" , "Home" , "Home"))

newRow4 <- data.frame(Any = as.character(c(2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003)) , Edat = c(16 , 20 , 25 , 30 , 35 , 40 , 45 , 50 , 55 , 60 , 65) , Percentatge = c(5.1 , 8 , 9.5 , 9.6 , 9.4 , 8.6 , 7.7 , 7 , 6.7 , 5.7 , 22.7) , Sexo = c("Dona" , "Dona" , "Dona" , "Dona" , "Dona" , "Dona" , "Dona" , "Dona" , "Dona" , "Dona" , "Dona"))

aa_def <- bind_rows(aa_def , newRow1 , newRow2 , newRow3 , newRow4)

options(digits = 3)

p <- ggplot(data = aa_def , mapping = aes(x = Edat , y = Percentatge , colour = Any)) +
  geom_line() +
  labs(title = "GRÀFIC 1: DISTRIBUCIÓ DE LA POBLACIÓ EN ESPANYA SEGONS EDAT" , caption = "Font: INE, EPA. Elaboració pròpia" , x = NULL , y = NULL) +
  theme(plot.caption = element_text(face = "italic", colour = "gray24") ,
    plot.title = element_text(size = 16 , face = "bold", colour = "firebrick") ,
    axis.text.x = element_text(angle = 45, hjust = 1) ,
    panel.background = element_rect(fill = "slategray", colour = "snow4")) +
  facet_wrap(vars(Sexo)) +
  scale_x_continuous(breaks = seq(15 , 65 , 10) , labels = c("15-19" , "25-29" , "35-39" , "45-49" , "55-59" , "65 y más")) +
  scale_y_continuous(breaks = seq(5 , 25 , 2.5) , labels = c("5%" , "7.5%" , "10%" , "12.5%" , "15%" , "17.5%" , "20%" , "22.5%" , "25%")) +
  labs(colour = NULL) +
  scale_color_brewer(palette = "YlOrRd")

ggplotly(p , tooltip = c("Any" ,  "Percentatge" , "Edat"))
ggsave(filename = "./ExportsVal/3.1 Distribució població.png" , plot = p , width = 8.5 , height = 6)

p <- ggplot(data = aa_def , mapping = aes(x = Edat , y = Percentatge , colour = Any)) +
  geom_line() +
  labs(title = "GRÀFIC 1: DISTRIBUCIÓ DE LA POBLACIÓ EN ESPANYA SEGONS EDAT" , caption = "Font: INE, EPA. Elaboració pròpia" , x = NULL , y = NULL) +
  theme(plot.caption = element_text(face = "italic", colour = "gray24") ,
    plot.title = element_text(size = 16 , face = "bold", colour = "firebrick") ,
    axis.text.x = element_text(angle = 45, hjust = 1) ,
    panel.background = element_rect(fill = "gray80", colour = "snow4")) +
  facet_wrap(vars(Sexo)) +
  scale_x_continuous(breaks = seq(15 , 65 , 10) , labels = c("15-19" , "25-29" , "35-39" , "45-49" , "55-59" , "65 y más")) +
  scale_y_continuous(breaks = seq(5 , 25 , 2.5) , labels = c("5%" , "7.5%" , "10%" , "12.5%" , "15%" , "17.5%" , "20%" , "22.5%" , "25%")) +
  labs(colour = NULL) +
  scale_color_brewer(palette = "YlOrRd")

ggplotly(p , tooltip = c("Any" ,  "Percentatge" , "Edat"))
ggsave(filename = "./ExportsVal/3.2 Distribució població.png" , plot = p , width = 8.5 , height = 6)
}

# 4. Distribución por nivel de estudios (spider - radar chart) (no se guarda)
{
zz <- df %>% filter(CICLO == 189 , NIVEL == 1) %>%
  summarise(total = sum(FACTOREL , na.rm = FALSE))
zz <- as.numeric(zz$total)

aa <- df %>% filter(CICLO %in% c(189) , NIVEL == 1) %>%
  group_by(NFORMA , CICLO) %>%
  summarise(Percentatge = sum(FACTOREL , na.rm = FALSE)*100/zz) %>%
  ungroup() %>%
  select("Formación" = NFORMA , "2019" = Percentatge) %>%
  mutate(Formación = gsub(pattern = "AN" , replacement = "Analfabets" , x = Formación)) %>%
  mutate(Formación = gsub(pattern = "P1" , replacement = "Sense educació" , x = Formación)) %>%
  mutate(Formación = gsub(pattern = "P2" , replacement = "Primària" , x = Formación)) %>%
  mutate(Formación = gsub(pattern = "S1" , replacement = "Secondària primera etapa" , x = Formación)) %>%
  mutate(Formación = gsub(pattern = "SG" , replacement = "Secondària segona etapa" , x = Formación)) %>%
  mutate(Formación = gsub(pattern = "SP" , replacement = "Formació professional" , x = Formación)) %>%
  mutate(Formación = gsub(pattern = "SU" , replacement = "Educació superior" , x = Formación)) %>%
  add_column("2003" = c(3 , 9 , 26 , 24 , 13 , 12 , 13)) %>%
  add_column("1978" = c(9 , 15 , 56 , 8 , 6 , 1 , 5)) %>%
  select(1 , 4:2)

chartJSRadar(scores = aa , maxScale = 60 , showToolTipLabel = TRUE)
}

# 5. Taxa de actividad por sexo y grupos quinquenales
{
  ii <- 133

  zz1 <- df %>% filter(CICLO == ii , NIVEL == 1) %>%
  group_by(CICLO , EDAD5 , SEXO1) %>%
  summarise("Total" = sum(FACTOREL , na.rm = FALSE)) %>%
  ungroup()

  aa <- df %>% filter(AOI == 3 | AOI == 4 | AOI == 5 | AOI == 6) %>%
  filter(CICLO == ii , NIVEL == 1) %>%
  group_by(CICLO , EDAD5 , SEXO1) %>%
  summarise("Percentatge" = sum(FACTOREL , na.rm = FALSE)) %>%
  ungroup() %>%
  mutate(Percentatge = Percentatge*100/zz1$Total)

  x <- c(153 , 173 , 189)

  for (ii in x) {
    zz1 <- df %>% filter(CICLO == ii , NIVEL == 1) %>%
  group_by(CICLO , EDAD5 , SEXO1) %>%
  summarise("Total" = sum(FACTOREL , na.rm = FALSE)) %>%
  ungroup()
    aa1 <- df %>% filter(AOI == 3 | AOI == 4 | AOI == 5 | AOI == 6) %>%
  filter(CICLO == ii , NIVEL == 1) %>%
  group_by(CICLO , EDAD5 , SEXO1) %>%
  summarise("Percentatge" = sum(FACTOREL , na.rm = FALSE)) %>%
  ungroup() %>%
  mutate(Percentatge = Percentatge*100/zz1$Total)

    aa <- bind_rows(aa , aa1)
    }

aa <- aa %>% mutate(Any = gsub(x = CICLO , pattern = 133 , replacement = "2005")) %>%
  mutate(Any = gsub(x = Any , pattern = 153 , replacement = "2010")) %>%
  mutate(Any = gsub(x = Any , pattern = 173 , replacement = "2015")) %>%
  mutate(Any = gsub(x = Any , pattern = 189 , replacement = "2019")) %>%
  mutate(Edat = EDAD5 , Sexo = SEXO1) %>%
  select(Any , Edat , Percentatge , Sexo)

newRow1 <- data.frame(Any = as.character(c(1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978)) , Edat = c(16,16,20,20,25,25,30,30,35,35,40,40,45,45,50,50,55,55,60,60,65,65) , Percentatge = c(57,45,63,54,94,37,98,28,98,26,97,28,96,27,93,27.5,87,26,69,20,16,9) , Sexo = c("Home" , "Dona" , "Home" , "Dona" , "Home" , "Dona" , "Home" , "Dona" , "Home" , "Dona" , "Home" , "Dona" , "Home" , "Dona" , "Home" , "Dona" , "Home" , "Dona" , "Home" , "Dona" , "Home" , "Dona"))
newRow2 <- data.frame(Any = as.character(c(2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003)) , Edat = c(16,16,20,20,25,25,30,30,35,35,40,40,45,45,50,50,55,55,60,60,65,65) , Percentatge = c(31,19,68,58,90,80,95,75,95.5,70,95,67,93.5,61.5,89,47,76,33.5,49,19,3.2,2) , Sexo = c("Home" , "Dona" , "Home" , "Dona" , "Home" , "Dona" , "Home" , "Dona" , "Home" , "Dona" , "Home" , "Dona" , "Home" , "Dona" , "Home" , "Dona" , "Home" , "Dona" , "Home" , "Dona" , "Home" , "Dona"))

aa_def <- bind_rows(aa , newRow1 , newRow2)
#aa_def <- aa_def %>% filter(Any %in% c("1978" , "2003" , "2019"))

p <- ggplot(data = aa_def , mapping = aes(x = Edat , y = Percentatge , colour = Any)) +
  geom_line() +
  labs(title = "GRÀFIC 3: TAXA D'ACTIVIDAD PER SEXE I GRUPS QUINQUENALS D'EDAT EN ESPANYA" , caption = "Font: INE, EPA. Elaboració pròpia" , x = NULL , y = NULL) +
  theme(plot.caption = element_text(face = "italic", colour = "gray24") ,
    plot.title = element_text(size = 16 , face = "bold", colour = "firebrick") ,
    axis.text.x = element_text(angle = 45, hjust = 1) ,
    panel.background = element_rect(fill = "slategray", colour = "snow4")) +
  facet_wrap(vars(Sexo)) +
  scale_x_continuous(breaks = seq(15 , 65 , 10) , labels = c("15-19" , "25-29" , "35-39" , "45-49" , "55-59" , "65 y más")) +
  scale_y_continuous(breaks = seq(0 , 100 , 10) , labels = c("0%" , "10%" , "20%" , "30%" , "40%" , "50%" , "60%" , "70%" , "80%" , "90%" , "100%")) +
  labs(colour = NULL) +
  scale_color_brewer(palette = "YlOrRd")

ggplotly(p , tooltip = c("Any" ,  "Percentatge" , "Edat"))
ggsave(filename = "./ExportsVal/5.1 Taxa d'activitad per sexe i grups quinquenals.png" , plot = p , width = 10 , height = 6)

p <- ggplot(data = aa_def , mapping = aes(x = Edat , y = Percentatge , colour = Any)) +
  geom_line() +
  labs(title = "GRÀFIC 3: TAXA D'ACTIVIDAD PER SEXE I GRUPS QUINQUENALS D'EDAT EN ESPANYA" , caption = "Font: INE, EPA. Elaboració pròpia" , x = NULL , y = NULL) +
  theme(plot.caption = element_text(face = "italic", colour = "gray24") ,
    plot.title = element_text(size = 16 , face = "bold", colour = "firebrick") ,
    axis.text.x = element_text(angle = 45, hjust = 1) ,
    panel.background = element_rect(fill = "gray80", colour = "snow4")) +
  facet_wrap(vars(Sexo)) +
  scale_x_continuous(breaks = seq(15 , 65 , 10) , labels = c("15-19" , "25-29" , "35-39" , "45-49" , "55-59" , "65 y más")) +
  scale_y_continuous(breaks = seq(0 , 100 , 10) , labels = c("0%" , "10%" , "20%" , "30%" , "40%" , "50%" , "60%" , "70%" , "80%" , "90%" , "100%")) +
  labs(colour = NULL) +
  scale_color_brewer(palette = "YlOrRd")

ggplotly(p , tooltip = c("Any" ,  "Percentatge" , "Edat"))
ggsave(filename = "./ExportsVal/5.2 Taxa d'activitad per sexe i grups quinquenals.png" , plot = p , width = 10 , height = 6)
}

# 6. Percentatge de jóvenes que se declaran estudiantes (parece estar mal porque no es normal que en 2003 Percentatge alto y 2005 Percentatge bajo. Además, no tenemos los datos de Edat sin agrupar porque esos datos valen dinero)
{
ii <- 133

  zz1 <- df %>% filter(CICLO == ii , NIVEL == 1 , EDAD5 %in% c(16 , 20 , 25 , 30)) %>%
  group_by(CICLO , EDAD5) %>%
  summarise("Total" = sum(FACTOREL , na.rm = FALSE)) %>%
  ungroup()

  aa <- df %>% filter(CURSR %in% c(1 , 2) | CURSNR %in% c(1 , 2)) %>%
  filter(CICLO == ii , NIVEL == 1 , EDAD5 %in% c(16 , 20 , 25 , 30)) %>%
  group_by(CICLO , EDAD5) %>%
  summarise("Percentatge" = sum(FACTOREL , na.rm = FALSE)) %>%
  ungroup() %>%
  mutate(Percentatge = Percentatge*100/zz1$Total)

  x <- c(153 , 173 , 189)

  for (ii in x) {
    zz1 <- df %>% filter(CICLO == ii , NIVEL == 1 , EDAD5 %in% c(16 , 20 , 25 , 30)) %>%
  group_by(CICLO , EDAD5) %>%
  summarise("Total" = sum(FACTOREL , na.rm = FALSE)) %>%
  ungroup()
    aa1 <- df %>% filter(CURSR %in% c(1 , 2) | CURSNR %in% c(1 , 2)) %>%
  filter(CICLO == ii , NIVEL == 1 , EDAD5 %in% c(16 , 20 , 25 , 30)) %>%
  group_by(CICLO , EDAD5) %>%
  summarise("Percentatge" = sum(FACTOREL , na.rm = FALSE)) %>%
  ungroup() %>%
  mutate(Percentatge = Percentatge*100/zz1$Total)

    aa <- bind_rows(aa , aa1)
    }

aa <- aa %>% mutate(Any = gsub(x = CICLO , pattern = 133 , replacement = "2005")) %>%
  mutate(Any = gsub(x = Any , pattern = 153 , replacement = "2010")) %>%
  mutate(Any = gsub(x = Any , pattern = 173 , replacement = "2015")) %>%
  mutate(Any = gsub(x = Any , pattern = 189 , replacement = "2019")) %>%
  mutate(Edat = EDAD5) %>%
  select(Any , Edat , Percentatge)

newRow1 <- data.frame(Any = as.character(c(1978 , 1978 , 1978 , 1978)) , Edat = c(16 , 20 , 25 , 30) , Percentatge = c(55 , 25 , 6 , 1))
newRow2 <- data.frame(Any = as.character(c(2003 , 2003 , 2003 , 2003)) , Edat = c(16 , 20 , 25 , 30) , Percentatge = c(91 , 56 , 26 , 7.5))
aa_def <- bind_rows(aa , newRow1 , newRow2)
#aa_def <- aa_def %>% filter(Any %in% c("1978" , "2003" , "2019"))

p <- ggplot(data = aa_def , mapping = aes(x = Edat , y = Percentatge , colour = Any)) +
  geom_line() +
  labs(title = "GRÀFIC 4: PERCENTATGE DE JOVES QUE ES DECLAREN ESTUDIANTS EN ESPANYA" , caption = "Font: INE, EPA. Elaboració pròpia" , x = NULL , y = NULL) +
  theme(plot.caption = element_text(face = "italic", colour = "gray24") ,
    plot.title = element_text(size = 16 , face = "bold", colour = "firebrick") ,
    axis.text.x = element_text(angle = 45, hjust = 1) ,
    panel.background = element_rect(fill = "slategray", colour = "snow4")) +
  scale_x_continuous(breaks = seq(0 , 30 , 5)) +
  scale_y_continuous(breaks = seq(0 , 100 , 10) , labels = c("0%" , "10%" , "20%" , "30%" , "40%" , "50%" , "60%" , "70%" , "80%" , "90%" , "100%")) +
  labs(colour = NULL) +
  scale_color_brewer(palette = "YlOrRd")

ggplotly(p , tooltip = c("Any" ,  "Percentatge" , "Edat"))
ggsave(filename = "./ExportsVal/6.1 Percentatge de jóvenes que se declaran estudiantes (mal).png" , plot = p , width = 10 , height = 6)

p <- ggplot(data = aa_def , mapping = aes(x = Edat , y = Percentatge , colour = Any)) +
  geom_line() +
  labs(title = "GRÀFIC 4: PERCENTATGE DE JOVES QUE ES DECLAREN ESTUDIANTS EN ESPANYA" , caption = "Font: INE, EPA. Elaboració pròpia" , x = NULL , y = NULL) +
  theme(plot.caption = element_text(face = "italic", colour = "gray24") ,
    plot.title = element_text(size = 16 , face = "bold", colour = "firebrick") ,
    axis.text.x = element_text(angle = 45, hjust = 1) ,
    panel.background = element_rect(fill = "gray80", colour = "snow4")) +
  scale_x_continuous(breaks = seq(0 , 30 , 5)) +
  scale_y_continuous(breaks = seq(0 , 100 , 10) , labels = c("0%" , "10%" , "20%" , "30%" , "40%" , "50%" , "60%" , "70%" , "80%" , "90%" , "100%")) +
  labs(colour = NULL) +
  scale_color_brewer(palette = "YlOrRd")

ggplotly(p , tooltip = c("Any" ,  "Percentatge" , "Edat"))
ggsave(filename = "./ExportsVal/6.2 Percentatge de jóvenes que se declaran estudiantes (mal).png" , plot = p , width = 10 , height = 6)
}

# 7. Grandes magnitudes de la actividad, el empleo y el paro en españa.
{

  #Varons
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
  mutate(Situación = "Aturats")

aa3 <- aa %>% filter(AOI == 3 | AOI == 4 |AOI == 5 | AOI == 6) %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  mutate(Situación = "Actius")

aa4 <- aa %>% filter(AOI == 7 | AOI == 8 | AOI == 9) %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  mutate(Situación = "Inactius")

aa5 <- aa %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  mutate(Situación = "Total majors de 16 anys")

newRow1 <- data.frame(ANO = 1978 , Total = c(12401600 , 877600 , 13279200 , 12733000 , 26012200) , Situación = c("Ocupados" , "Aturats" , "Actius" , "Inactius" , "Total majors de 16 anys"))
newRow2 <- data.frame(ANO = 2003 , Total = c(16666000 , 2460400 , 19126400 , 15049100 , 34175500) , Situación = c("Ocupados" , "Aturats" , "Actius" , "Inactius" , "Total majors de 16 anys"))

aa_tot <- bind_rows(aa1 , aa2 , aa3 , aa4 , aa5 , newRow1 , newRow2)
aa_tot <- aa_tot %>% pivot_wider(names_from = ANO , names_prefix = "ano" , values_from = Total) %>%
  mutate(dif19.03 = ano2019-ano2003 , dif19.78 = ano2019-ano1978) %>%
  select(Situación , ano1978 , ano2003 , ano2019 , dif19.03 , dif19.78) %>%
  add_column(Sexo = "Ambdós sexes" , .before = 1)
}

  #Home
  {
aa1 <- aa %>% filter(SEXO1 == "Home") %>%
  filter(AOI == 3 | AOI == 4) %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  mutate(Situación = "Ocupats")

aa2 <- aa %>% filter(SEXO1 == "Home") %>%
  filter(AOI == 5 | AOI == 6) %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  mutate(Situación = "Aturats")

aa3 <- aa %>% filter(SEXO1 == "Home") %>%
  filter(AOI == 3 | AOI == 4 |AOI == 5 | AOI == 6) %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  mutate(Situación = "Actius")

aa4 <- aa %>% filter(SEXO1 == "Home") %>%
  filter(AOI == 7 | AOI == 8 | AOI == 9) %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  mutate(Situación = "Inactius")

aa5 <- aa %>%
  filter(SEXO1 == "Home") %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  mutate(Situación = "Total majors de 16 anys")

zz <- data.frame(Sexo = "Taxes" , Situación = c("Taxa d'activitat - Varons" , "Taxa d'ocupació - Varons" , "Taxa d'atur - Varons" , "Taxa d'atur absoluta - Varons") , ano1978 = c(0.759 , 0.712 , 0.063 , 0.048) , ano2003 =  c(0.681 , 0.619 , 0.09 , 0.061) , ano2019 = c(aa3$Total/aa5$Total , aa1$Total/aa5$Total , aa2$Total/aa3$Total , aa2$Total/aa5$Total))

newRow1 <- data.frame(ANO = 1978 , Total = c(8912000 , 598600 , 9510600 , 3011700 , 12522300) , Situación = c("Ocupats" , "Aturats" , "Actius" , "Inactius" , "Total majors de 16 anys"))
newRow2 <- data.frame(ANO = 2003 , Total = c(10278600 , 1019500 , 11298100 , 5297600 , 16595700) , Situación = c("Ocupats" , "Aturats" , "Actius" , "Inactius" , "Total majors de 16 anys"))

aa_Home <- bind_rows(aa1 , aa2 , aa3 , aa4 , aa5 , newRow1 , newRow2)
aa_Home <- aa_Home %>% pivot_wider(names_from = ANO , names_prefix = "ano" , values_from = Total) %>%
  mutate(dif19.03 = ano2019-ano2003 , dif19.78 = ano2019-ano1978) %>%
  select(Situación , ano1978 , ano2003 , ano2019 , dif19.03 , dif19.78) %>%
  add_column(Sexo = "Home" , .before = 1)
}

  #Dona
  {aa1 <- aa %>% filter(SEXO1 == "Dona") %>%
  filter(AOI == 3 | AOI == 4) %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  mutate(Situación = "Ocupats")

aa2 <- aa %>% filter(SEXO1 == "Dona") %>%
  filter(AOI == 5 | AOI == 6) %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  mutate(Situación = "Aturats")

aa3 <- aa %>% filter(SEXO1 == "Dona") %>%
  filter(AOI == 3 | AOI == 4 |AOI == 5 | AOI == 6) %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  mutate(Situación = "Actius")

aa4 <- aa %>% filter(SEXO1 == "Dona") %>%
  filter(AOI == 7 | AOI == 8 | AOI == 9) %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  mutate(Situación = "Inactius")

aa5 <- aa %>%
  filter(SEXO1 == "Dona") %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  mutate(Situación = "Total majors de 16 anys")

zz1 <- data.frame(Sexo = "Taxes" , Situación = c("Taxa de actividad - Dones" , "Taxa de ocupación - Dones" , "Taxa de paro - Dones" , "Taxa de paro absoluta - Dones") , ano1978 = c(0.279 , 0.259 , 0.074 , 0.021) , ano2003 =  c(0.445 , 0.363 , 0.184 , 0.082) , ano2019 = c(aa3$Total/aa5$Total , aa1$Total/aa5$Total , aa2$Total/aa3$Total , aa2$Total/aa5$Total))

newRow1 <- data.frame(ANO = 1978 , Total = c(3489600 , 279000 , 3768600 , 9721300 , 13489900) , Situación = c("Ocupats" , "Aturats" , "Actius" , "Inactius" , "Total majors de 16 anys"))
newRow2 <- data.frame(ANO = 2003 , Total = c(6387400 , 1440900 , 7828300 , 9751500 , 17579800) , Situación = c("Ocupats" , "Aturats" , "Actius" , "Inactius" , "Total majors de 16 anys"))

aa_Dona <- bind_rows(aa1 , aa2 , aa3 , aa4 , aa5 , newRow1 , newRow2)
aa_Dona <- aa_Dona %>% pivot_wider(names_from = ANO , names_prefix = "ano" , values_from = Total) %>%
  mutate(dif19.03 = ano2019-ano2003 , dif19.78 = ano2019-ano1978) %>%
  select(Situación , ano1978 , ano2003 , ano2019 , dif19.03 , dif19.78) %>%
  add_column(Sexo = "Dona" , .before = 1)
}

  #7.1 Taxes
  {
zz <- bind_rows(zz , zz1)
zz <- zz %>% arrange(Situación) %>% mutate(dif19.03 = ano2019 - ano2003 , dif19.78 = ano2019 - ano1978) %>%
  group_by(Sexo)

options(digits = 2)

p <- zz %>% gt() %>%
    tab_header(title = md("**QUADRE 4: LES GRANS TAXES DE L'ACTIVIDAD, L'OCUPACIÓ I L'ATUR EN ESPANYA, 1978, 2003 Y 2019**")) %>%
    cols_label(Situación = "" , ano1978 = "1978" , ano2003 = "2003" , ano2019 = "2019" , dif19.03 = md("Diferència<br>2019 - 2003") , dif19.78 = md("Diferència<br>2019 - 1978")) %>%
    fmt_percent(columns = c("ano1978" , "ano2003" , "ano2019" , "dif19.03" , "dif19.78")) %>%
    tab_source_note(source_note = md("***Font:** INE, EPA*<br>*Elaboració pròpia*")) %>%
    tab_style(style = cell_text(color = "firebrick", size = "large", align = "center"), locations = cells_title()) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_title()) %>%
    tab_style(style = cell_text(color = "firebrick", size = "normal", align = "center" , weight = "bolder"), locations = cells_column_labels(columns = c("Situación" , "ano1978" , "ano2003" , "ano2019" , "dif19.03" , "dif19.78"))) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_column_labels(columns = c("Situación" , "ano1978" , "ano2003" , "ano2019" , "dif19.03" , "dif19.78"))) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_data(columns = c("Situación"))) %>%
    tab_style(style = cell_text(color = "firebrick" , style = "italic"), locations = cells_data(columns = c("Situación")))
p

gtsave(p , filename = "7.1 Las grandes Taxes de la actividad, el empleo y el paro.png" , path = "./ExportsVal/")
  }

aa <- bind_rows(aa_tot , aa_Home , aa_Dona)
aa <- aa %>% mutate(ano1978 = ano1978/1000 , ano2003 = ano2003/1000 , ano2019 = ano2019/1000 , dif19.03 = dif19.03/1000 , dif19.78 = dif19.78/1000) %>%
  group_by(Sexo)

options(digits = 4)

p <- aa %>% gt() %>%
    tab_header(title = md("**QUADRE 3: LES GRANS MAGNITUDS DE L'ACTIVITAT, L'OCUPACIÓ I L'ATUR EN ESPANYA, 1978, 2003 Y 2019**")) %>%
    cols_label(Situación = "" , ano1978 = "1978" , ano2003 = "2003" , ano2019 = "2019" , dif19.03 = md("Diferència<br>2019 - 2003") , dif19.78 = md("Diferència<br>2019 - 1978")) %>%
    tab_source_note(source_note = md("***Font:** INE, EPA*<br>*Elaboració pròpia*")) %>%
    tab_style(style = cell_text(color = "firebrick", size = "large", align = "center"), locations = cells_title()) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_title()) %>%
    tab_style(style = cell_text(color = "firebrick", size = "normal", align = "center" , weight = "bolder"), locations = cells_column_labels(columns = c("Situación" , "ano1978" , "ano2003" , "ano2019" , "dif19.03" , "dif19.78"))) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_column_labels(columns = c("Situación" , "ano1978" , "ano2003" , "ano2019" , "dif19.03" , "dif19.78"))) %>%
    tab_style(style = cell_text(weight = "bold"), locations = cells_data(columns = c("Situación" , "ano1978" , "ano2003" , "ano2019" , "dif19.03" , "dif19.78"), rows = c(5 , 10 , 15))) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_data(columns = c("Situación"))) %>%
    tab_style(style = cell_text(color = "firebrick" , style = "italic"), locations = cells_data(columns = c("Situación")))

p
gtsave(p , filename = "7.2 Las grandes magnitudes de la actividad, el empleo y el paro.png" , path = "./ExportsVal/")
}

# 8. Distribució del empleo por sector de actividad
{
aa <- df %>% filter(CICLO == 189) %>%
  filter(AOI == 3 | AOI == 4) %>%
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
  mutate(por1978 = ano1978/aa_tot$ano1978, por2003 = ano2003/aa_tot$ano2003, por2019 = ano2019/aa_tot$ano2019 , Sector = "Indústria") %>%
  select(7 , 1:6)

aa3 <- aa %>% filter(ACT1 == 4) %>%
  select(ANO , Total) %>%
  add_row(ANO = c(1978 , 2003) , Total = c(1234500 , 2010000) , .before = 1) %>%
  pivot_wider(names_prefix = "ano" , names_from = ANO , values_from = Total) %>%
  mutate(por1978 = ano1978/aa_tot$ano1978, por2003 = ano2003/aa_tot$ano2003, por2019 = ano2019/aa_tot$ano2019 , Sector = "Construcció") %>%
  select(7 , 1:6)

aa4 <- aa %>% filter(ACT1 == 5 | ACT1 == 6) %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  ungroup() %>%
  select(ANO , Total) %>%
  add_row(ANO = c(1978 , 2003) , Total = c(2956200 , 4689400) , .before = 1) %>%
  pivot_wider(names_prefix = "ano" , names_from = ANO , values_from = Total) %>%
  mutate(por1978 = ano1978/aa_tot$ano1978, por2003 = ano2003/aa_tot$ano2003, por2019 = ano2019/aa_tot$ano2019 , Sector = "Comerç, hostaleria, transport") %>%
  select(7 , 1:6)

aa5 <- aa %>% filter(ACT1 == 7) %>%
  select(ANO , Total) %>%
  add_row(ANO = c(1978 , 2003) , Total = c(415800 , 1724000) , .before = 1) %>%
  pivot_wider(names_prefix = "ano" , names_from = ANO , values_from = Total) %>%
  mutate(por1978 = ano1978/aa_tot$ano1978, por2003 = ano2003/aa_tot$ano2003, por2019 = ano2019/aa_tot$ano2019 , Sector = "Serv. financers, serv. empreses") %>%
  select(7 , 1:6)

aa6 <- aa %>% filter(ACT1 == 8) %>%
  select(ANO , Total) %>%
  add_row(ANO = c(1978 , 2003) , Total = c(1131900 , 3047100) , .before = 1) %>%
  pivot_wider(names_prefix = "ano" , names_from = ANO , values_from = Total) %>%
  mutate(por1978 = ano1978/aa_tot$ano1978, por2003 = ano2003/aa_tot$ano2003, por2019 = ano2019/aa_tot$ano2019 , Sector = "Serv. públics") %>%
  select(7 , 1:6)

aa7 <- aa %>% filter(ACT1 == 9) %>%
  select(ANO , Total) %>%
  add_row(ANO = c(1978 , 2003) , Total = c(765000 , 1133300) , .before = 1) %>%
  pivot_wider(names_prefix = "ano" , names_from = ANO , values_from = Total) %>%
  mutate(por1978 = ano1978/aa_tot$ano1978, por2003 = ano2003/aa_tot$ano2003, por2019 = ano2019/aa_tot$ano2019 , Sector = "Altres serveis") %>%
  select(7 , 1:6)

aa <- bind_rows(aa1 , aa2 , aa3 , aa4 , aa5 , aa6 , aa7 , aa_tot)
aa <- aa %>% mutate(ano1978 = ano1978/1000 , ano2003 = ano2003/1000 , ano2019 = ano2019/1000 , por1978 = por1978 , por2003 = por2003 , por2019 = por2019)

p <- aa %>% gt() %>%
    tab_header(title = md("**QUADRE 5: DISPERSIÓ DE L'OCUPACIÓ PER SECTORS D'ACTIVITAT EN ESPANYA, 1978, 2003 I 2019**")) %>%
    cols_label(Sector = "Sectors d'activitat" , ano1978 = "1978" , ano2003 = "2003" , ano2019 = "2019" , por1978 = "1978" , por2003 = "2003" , por2019 = "2019") %>%
    fmt_percent(columns = c("por1978" , "por2003" , "por2019")) %>%
    tab_spanner(label = "Milers de persones", columns = c("ano1978" , "ano2003" , "ano2019"), gather = TRUE) %>%
    tab_spanner(label = "Percentatges del total", columns = c("por1978" , "por2003" , "por2019"), gather = TRUE) %>%
    tab_source_note(source_note = md("***Font:** INE, EPA*<br>*Elaboració pròpia*")) %>%
    tab_style(style = cell_text(color = "firebrick", size = "large", align = "center"), locations = cells_title()) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_column_spanners(spanners = c("Milers de persones" , "Percentatges del total"))) %>%
    tab_style(style = cell_text(color = "firebrick", size = "large", align = "center" , weight = "bolder"), locations = cells_column_spanners(spanners = c("Milers de persones" , "Percentatges del total"))) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_title()) %>%
    tab_style(style = cell_text(color = "firebrick", size = "normal", align = "center" , weight = "bolder"), locations = cells_column_labels(columns = c("Sector" , "ano1978" , "ano2003" , "ano2019" , "por1978" , "por2003" , "por2019"))) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_column_labels(columns = c("Sector" , "ano1978" , "ano2003" , "ano2019" , "por1978" , "por2003" , "por2019"))) %>%
    tab_style(style = cell_text(weight = "bold"), locations = cells_data(columns = c("ano1978" , "ano2003" , "ano2019" , "por1978" , "por2003" , "por2019"), rows = 8)) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_data(columns = c("Sector"))) %>%
    tab_style(style = cell_text(color = "firebrick" , style = "italic"), locations = cells_data(columns = c("Sector")))

p
gtsave(p , filename = "8. Distribució de l'ocupació por sector de actividad.png" , path = "./ExportsVal/")
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


#gtsave(p , filename = "9. Distribución del empleo por sector de actividad.png" , path = "./ExportsVal/")
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
  mutate(por1978 = ano1978/aa_tot$ano1978, por2003 = ano2003/aa_tot$ano2003, por2019 = ano2019/aa_tot$ano2019 , Sector = "Empleadors") %>%
  select(7 , 1:6)

aa3 <- aa %>% filter(SITU == 3) %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  ungroup() %>%
  select(ANO , Total) %>%
  add_row(ANO = c(1978 , 2003) , Total = c(2140100 , 1888800) , .before = 1) %>%
  pivot_wider(names_prefix = "ano" , names_from = ANO , values_from = Total) %>%
  mutate(por1978 = ano1978/aa_tot$ano1978, por2003 = ano2003/aa_tot$ano2003, por2019 = ano2019/aa_tot$ano2019 , Sector = "Autònoms o empresari sense assalariats") %>%
  select(7 , 1:6)

aa6 <- aa %>% filter(SITU == 6) %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  ungroup() %>%
  select(ANO , Total) %>%
  add_row(ANO = c(1978 , 2003) , Total = c(1039400 , 272100) , .before = 1) %>%
  pivot_wider(names_prefix = "ano" , names_from = ANO , values_from = Total) %>%
  mutate(por1978 = ano1978/aa_tot$ano1978, por2003 = ano2003/aa_tot$ano2003, por2019 = ano2019/aa_tot$ano2019 , Sector = "Ajuda en empresa o negoci familiar") %>%
  select(7 , 1:6)

aa7 <- aa %>% filter(SITU == 7) %>%
  select(ANO , Total) %>%
  add_row(ANO = c(1978 , 2003) , Total = c(1484900 , 2716200) , .before = 1) %>%
  pivot_wider(names_prefix = "ano" , names_from = ANO , values_from = Total) %>%
  mutate(por1978 = ano1978/aa_tot$ano1978, por2003 = ano2003/aa_tot$ano2003, por2019 = ano2019/aa_tot$ano2019 , Sector = "Assalariat sector públic") %>%
  select(7 , 1:6)

aa8 <- aa %>% filter(SITU == 8) %>%
  select(ANO , Total) %>%
  add_row(ANO = c(1978 , 2003) , Total = c(7277900 , 10858100) , .before = 1) %>%
  pivot_wider(names_prefix = "ano" , names_from = ANO , values_from = Total) %>%
  mutate(por1978 = ano1978/aa_tot$ano1978, por2003 = ano2003/aa_tot$ano2003, por2019 = ano2019/aa_tot$ano2019 , Sector = "Assalariado sector privat") %>%
  select(7 , 1:6)

aa9 <- aa %>% filter(SITU == 9 | SITU == 5) %>%
  group_by(ANO) %>%
  summarise(Total = sum(Total , na.rm = FALSE)) %>%
  select(ANO , Total) %>%
  add_row(ANO = c(1978 , 2003) , Total = c(12100 , 15900) , .before = 1) %>%
  pivot_wider(names_prefix = "ano" , names_from = ANO , values_from = Total) %>%
  mutate(por1978 = ano1978/aa_tot$ano1978, por2003 = ano2003/aa_tot$ano2003, por2019 = ano2019/aa_tot$ano2019 , Sector = "Membre de cooperativa o altra situació") %>%
  select(7 , 1:6)

aa1 <- bind_rows(aa1 , aa3 , aa6 , aa7 , aa8 , aa9 , aa_tot)
aa1 <- aa1 %>% mutate(ano1978 = ano1978/1000 , ano2003 = ano2003/1000 , ano2019 = ano2019/1000 , por1978 = por1978 , por2003 = por2003 , por2019 = por2019)

options(digits = 4)

p <- aa1 %>% gt() %>%
    tab_header(title = md("**QUADRE 6: DISTRIBUCIÓ DE L'OCUPACIÓ PER SITUACIÓ PROFESSIONAL EN ESPANYA, 1978, 2003 Y 2019**")) %>%
    cols_label(Sector = "Situació professional" , ano1978 = "1978" , ano2003 = "2003" , ano2019 = "2019" , por1978 = "1978" , por2003 = "2003" , por2019 = "2019") %>%
    fmt_percent(columns = c("por1978" , "por2003" , "por2019")) %>%
    tab_spanner(label = "Milers de persones", columns = c("ano1978" , "ano2003" , "ano2019"), gather = TRUE) %>%
    tab_spanner(label = "Percentatges del total", columns = c("por1978" , "por2003" , "por2019"), gather = TRUE) %>%
    tab_source_note(source_note = md("***Font:** INE, EPA*<br>*Elaboració pròpia*")) %>%
    tab_style(style = cell_text(color = "firebrick", size = "large", align = "center"), locations = cells_title()) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_column_spanners(spanners = c("Milers de persones" , "Percentatges del total"))) %>%
    tab_style(style = cell_text(color = "firebrick", size = "large", align = "center" , weight = "bolder"), locations = cells_column_spanners(spanners = c("Milers de persones" , "Percentatges del total"))) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_title()) %>%
    tab_style(style = cell_text(color = "firebrick", size = "normal", align = "center" , weight = "bolder"), locations = cells_column_labels(columns = c("Sector" , "ano1978" , "ano2003" , "ano2019" , "por1978" , "por2003" , "por2019"))) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_column_labels(columns = c("Sector" , "ano1978" , "ano2003" , "ano2019" , "por1978" , "por2003" , "por2019"))) %>%
    tab_style(style = cell_text(weight = "bold"), locations = cells_data(columns = c("ano1978" , "ano2003" , "ano2019" , "por1978" , "por2003" , "por2019"), rows = 7)) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_data(columns = c("Sector"))) %>%
    tab_style(style = cell_text(color = "firebrick" , style = "italic"), locations = cells_data(columns = c("Sector")))

p
gtsave(p , filename = "10. Distribución del empleo por situación profesional.png" , path = "./ExportsVal/")
}

# 11. Distribución del paro según la relación con la persona de referencia
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

newRow1 <- data.frame(ANO = c(1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978) , RELPP1 = as.character(c(1 , 1 , 2 , 2 , 3 , 3 , 4 , 4)) , Total = c(259200 , 8300 , 400 , 40700 , 295300 , 211800 , 43900 , 18200) , SEXO1 = c("Home" , "Dona" , "Home" , "Dona" , "Home" , "Dona" , "Home" , "Dona"))
newRow2 <- data.frame(ANO = c(2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003) , RELPP1 = as.character(c(1 , 1 , 2 , 2 , 3 , 3 , 4 , 4)) , Total = c(389500 , 185800 , 40700 , 675200 , 529600 , 525300 , 59700 , 54600) , SEXO1 = c("Home" , "Dona" , "Home" , "Dona" , "Home" , "Dona" , "Home" , "Dona"))

aa <- bind_rows(aa , newRow1 , newRow2)
aa <- aa %>% mutate(SEXO1 = gsub(pattern = "Home" , replacement = "Home" , x = SEXO1)) %>%
  mutate(SEXO1 = gsub(pattern = "Dona" , replacement = "Dona" , x = SEXO1)) %>%
  mutate(RELPP1 = gsub(pattern = "1" , replacement = "Persona referència" , x = RELPP1)) %>%
  mutate(RELPP1 = gsub(pattern = "2" , replacement = "Cònjuge" , x = RELPP1)) %>%
  mutate(RELPP1 = gsub(pattern = "3" , replacement = "Fill" , x = RELPP1)) %>%
  mutate(RELPP1 = gsub(pattern = "4" , replacement = "Altres situacions" , x = RELPP1)) %>%
  unite(RELPP1, RELPP1:SEXO1, sep = " ") %>%
  pivot_wider(names_from = ANO , values_from = Total , names_prefix = "ano")
newRow3 <- data.frame(RELPP1 = "Total" , ano2003 = sum(aa$ano2003 , na.rm = FALSE) , ano1978 = sum(aa$ano1978 , na.rm = FALSE) , ano2019 = sum(aa$ano2019 , na.rm = FALSE))
aa <- bind_rows(aa , newRow3)
aa <- aa %>% mutate(por1978 = ano1978/newRow3$ano1978 , por2003 = ano2003/newRow3$ano2003 , por2019 = ano2019/newRow3$ano2019) %>%
  mutate(ano1978 = ano1978/1000 , ano2003 = ano2003/1000 , ano2019 = ano2019/1000 , por1978 = por1978 , por2003 = por2003 , por2019 = por2019) %>%
  select(RELPP1 , ano1978 , ano2003 , ano2019 , por1978 , por2003 , por2019)

options(digits = 4)

p <- aa %>% gt() %>%
    tab_header(title = md("**QUADRE 7: DISTRIBUCIÓ DE L'ATUR SEGONS LA RELACIÓ AMB LA PERSONA DE REFERÈNCIA EN ESPANYA, 1978, 2003 Y 2019**")) %>%
    cols_label(RELPP1 = "Relació amb la persona de referència i sexe" , ano1978 = "1978" , ano2003 = "2003" , ano2019 = "2019" , por1978 = "1978" , por2003 = "2003" , por2019 = "2019") %>%
    fmt_percent(columns = c("por1978" , "por2003" , "por2019")) %>%
    tab_spanner(label = "Milers de persones", columns = c("ano1978" , "ano2003" , "ano2019"), gather = TRUE) %>%
    tab_spanner(label = "Percentatges del total", columns = c("por1978" , "por2003" , "por2019"), gather = TRUE) %>%
    tab_source_note(source_note = md("***Font:** INE, EPA*<br>*Elaboració pròpia*")) %>%
    tab_style(style = cell_text(color = "firebrick", size = "large", align = "center"), locations = cells_title()) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_column_spanners(spanners = c("Milers de persones" , "Percentatges del total"))) %>%
    tab_style(style = cell_text(color = "firebrick", size = "large", align = "center" , weight = "bolder"), locations = cells_column_spanners(spanners = c("Milers de persones" , "Percentatges del total"))) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_title()) %>%
    tab_style(style = cell_text(color = "firebrick", size = "normal", align = "center" , weight = "bolder"), locations = cells_column_labels(columns = c("RELPP1" , "ano1978" , "ano2003" , "ano2019" , "por1978" , "por2003" , "por2019"))) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_column_labels(columns = c("RELPP1" , "ano1978" , "ano2003" , "ano2019" , "por1978" , "por2003" , "por2019"))) %>%
    tab_style(style = cell_text(weight = "bold"), locations = cells_data(columns = c("ano1978" , "ano2003" , "ano2019" , "por1978" , "por2003" , "por2019"), rows = 9)) %>%
    tab_style(style = cell_fill(color = "gray95"), locations = cells_data(columns = c("RELPP1"))) %>%
    tab_style(style = cell_text(color = "firebrick" , style = "italic"), locations = cells_data(columns = c("RELPP1")))

p
gtsave(p , filename = "11. Distribución del paro según la relación con la persona de referencia.png" , path = "./ExportsVal/")
}

# 12. Taxa de paro según la persona de referencia
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
  summarise("Percentatge" = sum(FACTOREL , na.rm = FALSE)) %>%
  ungroup() %>%
  arrange(RELPP1) %>%
  mutate(Percentatge = Percentatge*100/zz1$Total)

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
  summarise("Percentatge" = sum(FACTOREL , na.rm = FALSE)) %>%
  ungroup() %>%
  arrange(RELPP1) %>%
  mutate(Percentatge = Percentatge*100/zz1$Total)

    aa <- bind_rows(aa , aa1)
    }

aa <- aa %>% mutate(Any = gsub(x = CICLO , pattern = 133 , replacement = "2005")) %>%
  mutate(Any = gsub(x = Any , pattern = 153 , replacement = "2010")) %>%
  mutate(Any = gsub(x = Any , pattern = 173 , replacement = "2015")) %>%
  mutate(Any = gsub(x = Any , pattern = 189 , replacement = "2019")) %>%
  mutate(Relacion = RELPP1 , Sexo = SEXO1) %>%
  select(Any , Relacion , Sexo , Percentatge)

newRow1 <- data.frame(Any = as.character(c(1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978 , 1978)) , Relacion = as.character(c(1 , 1 , 2 , 2 , 3 , 3 , 4 , 4)) , Percentatge = c(3.9 , 2.2 , 8.4 , 2.7 , 13.2 , 14.1 , 6.7 , 4.2) , Sexo = c("Home" , "Dona" , "Home" , "Dona" , "Home" , "Dona" , "Home" , "Dona"))
newRow2 <- data.frame(Any = as.character(c(2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003 , 2003)) , Relacion = as.character(c(1 , 1 , 2 , 2 , 3 , 3 , 4 , 4)) , Percentatge = c(5.8 , 13.4 , 6 , 17.3 , 16.2 , 23.2 , 12.3 , 17.9) , Sexo = c("Home" , "Dona" , "Home" , "Dona" , "Home" , "Dona" , "Home" , "Dona"))

aa_def <- bind_rows(aa , newRow1 , newRow2)
aa_def <- aa_def %>% mutate(Sexo = gsub(pattern = "Home" , replacement = "Home" , x = Sexo)) %>%
  mutate(Sexo = gsub(pattern = "Dona" , replacement = "Dona" , x = Sexo)) %>%
  mutate(Relacion = gsub(pattern = "1" , replacement = "Persona referència" , x = Relacion)) %>%
  mutate(Relacion = gsub(pattern = "2" , replacement = "Cònjuge" , x = Relacion)) %>%
  mutate(Relacion = gsub(pattern = "3" , replacement = "Fill" , x = Relacion)) %>%
  mutate(Relacion = gsub(pattern = "4" , replacement = "Altres situacions" , x = Relacion)) %>%
  unite(Relacion, Relacion:Sexo, sep = "
 ")
aa_def <- aa_def %>% filter(Any %in% c("1978" , "2003" , "2019"))

p <- ggplot(data = aa_def , mapping = aes(x = Relacion , y = Percentatge , fill = Any)) +
  geom_col(position = "dodge") +
  labs(title = "GRÀFIC 5: TAXA D'ATUR SEGONS LA RELACIÓ AMB LA PERSONA DE REFERÈNCIA EN ESPAÑA, 1978, 2003 Y 2019" , caption = "Font: INE, EPA. Elaboració pròpia" , x = NULL , y = NULL) +
  theme(plot.caption = element_text(face = "italic", colour = "gray24") ,
    plot.title = element_text(size = 14 , face = "bold", colour = "firebrick") ,
    panel.background = element_rect(fill = "slategray", colour = "snow4")) +
  scale_x_discrete() +
  scale_y_continuous(breaks = seq(0 , 30 , 5) , labels = c("0%" , "5%" , "10%" , "15%" , "20%" , "25%" , "30%")) +
  scale_fill_brewer(palette = "Reds")
options(digits = 2)
ggplotly(p , tooltip = c("Any" ,  "Percentatge"))
ggsave(filename = "./ExportsVal/12.1 Taxa de paro según la persona de referencia.png" , plot = p , width = 10.5 , height = 6)

p <- ggplot(data = aa_def , mapping = aes(x = Relacion , y = Percentatge , fill = Any)) +
  geom_col(position = "dodge") +
  labs(title = "GRÀFIC 5: TAXA D'ATUR SEGONS LA RELACIÓ AMB LA PERSONA DE REFERÈNCIA EN ESPAÑA, 1978, 2003 Y 2019" , caption = "Font: INE, EPA. Elaboració pròpia" , x = NULL , y = NULL) +
  theme(plot.caption = element_text(face = "italic", colour = "gray24") ,
    plot.title = element_text(size = 14 , face = "bold", colour = "firebrick") ,
    panel.background = element_rect(fill = "gray80", colour = "snow4")) +
  scale_x_discrete() +
  scale_y_continuous(breaks = seq(0 , 30 , 5) , labels = c("0%" , "5%" , "10%" , "15%" , "20%" , "25%" , "30%")) +
  scale_fill_brewer(palette = "Reds")
options(digits = 2)
ggplotly(p , tooltip = c("Any" ,  "Percentatge"))
ggsave(filename = "./ExportsVal/12.2 Taxa de paro según la persona de referencia.png" , plot = p , width = 10.5 , height = 6)
}

# 13. Taxa de paro por sexo y comunidad autónoma de residencia
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
  summarise("Percentatge" = sum(FACTOREL , na.rm = FALSE)) %>%
  ungroup() %>%
  mutate(Percentatge = Percentatge*100/zz1$Total)

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
  summarise("Percentatge" = sum(FACTOREL , na.rm = FALSE)) %>%
  ungroup() %>%
  mutate(Percentatge = Percentatge*100/zz1$Total)

    aa <- bind_rows(aa , aa1)
    }

aa <- aa %>% mutate(Any = gsub(x = CICLO , pattern = 133 , replacement = "2005")) %>%
  mutate(Any = gsub(x = Any , pattern = 153 , replacement = "2010")) %>%
  mutate(Any = gsub(x = Any , pattern = 173 , replacement = "2015")) %>%
  mutate(Any = gsub(x = Any , pattern = 189 , replacement = "2019")) %>%
  select(Any , CCAA , SEXO1 , Percentatge)

newRow1 <- data.frame(Any = "1978" , CCAA = c(1:17,51,52) , Percentatge = c(11.9 , 3.5 , 4.3 , 4.2 , 9.2 , 5.3 , 3.9 , 5 , 5.8 , 4.1 , 9.8 , 3.2 , 7.3 , 4.1 , 2.8 , 5.2 , 2.2 , 0 , 0) , SEXO1 = "Home")
newRow2 <- data.frame(Any = "1978" , CCAA = c(1:17,51,52) , Percentatge = c(12.3 , 7.2 , 5.1 , 4.2 , 10.2 , 3.8 , 7.4 , 8.8 , 6.4 , 5.3 , 10 , 2.3 , 9.1 , 9.9 , 7 , 11.8 , 2.8 , 0 , 0) , SEXO1 = "Dona")
newRow3 <- data.frame(Any = "2003" , CCAA = c(1:17,51,52) , Percentatge = c(3.7 , 4.6 , 9 , 7.5 , 10.5 , 8.6 , 6.9 , 7 , 7.4 , 8.3 , 15.4 , 9.3 , 6.5 , 7.4 , 3.7 , 6.8 , 4.1 , 0 , 0) , SEXO1 = "Home")
newRow4 <- data.frame(Any = "2003" , CCAA = c(1:17,51,52) , Percentatge = c(30.1 , 12.4 , 19.5 , 11.8 , 17.6 , 18.5 , 21.7 , 21.9 , 12.6 , 16.9 , 32.7 , 19.8 , 12.6 , 15.8 , 8.4 , 14.4 , 12.9 , 0 , 0) , SEXO1 = "Dona")

aa_def <- bind_rows(aa , newRow1 , newRow2 , newRow3 , newRow4)
aa_def <- aa_def %>% mutate(CCAA = gsub(pattern = 10 , replacement = "Comunitat Valenciana" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 11 , replacement = "Extremadura" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 12 , replacement = "Galícia" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 13 , replacement = "Madrid, Comunidad de" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 14 , replacement = "Murcia, Regió de" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 15 , replacement = "Navarra" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 16 , replacement = "País Vasc" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 17 , replacement = "Rioja, La" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 51 , replacement = "Ceuta" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 52 , replacement = "Melilla" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 1 , replacement = "Andalusia" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 2 , replacement = "Aragó" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 3 , replacement = "Asturies, Principat d'" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 4 , replacement = "Balears, Illes" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 5 , replacement = "Canaries" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 6 , replacement = "Cantàbria" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 7 , replacement = "Castella i Lleó" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 8 , replacement = "Castella-La Manxa" , x = CCAA)) %>%
  mutate(CCAA = gsub(pattern = 9 , replacement = "Catalunya" , x = CCAA))

aa_def <- aa_def %>% filter(Any %in% c("1978" , "2003" , "2019")) %>%
  pivot_wider(names_from = Any , names_prefix = "ano" , values_from = Percentatge)

aa_Home <- aa_def %>% filter(SEXO1 == "Home") %>%
  arrange(ano2019) %>%
  pivot_longer(cols = 3:5 , names_to = "Any" , values_to = "Percentatge") %>%
  mutate(CCAA = forcats::as_factor(CCAA)) %>%
  select(CCAA , Any , Percentatge.Home = Percentatge)

aa_Dona <- aa_def %>% filter(SEXO1 == "Dona") %>%
  pivot_longer(cols = 3:5 , names_to = "Any" , values_to = "Percentatge") %>%
  mutate(CCAA = forcats::as_factor(CCAA)) %>%
  select(CCAA , Any , Percentatge.Dona = Percentatge)

aa_def <- left_join(aa_Home , aa_Dona)
aa_def <- aa_def %>% pivot_longer(cols = 3:4 , names_to = "Sexo" , values_to = "Percentatge") %>%
  mutate(Any = gsub(x = Any , pattern = "ano" , replacement = "")) %>%
  mutate(Sexo = gsub(x = Sexo , pattern = "Percentatge.Home" , replacement = "Home")) %>%
  mutate(Sexo = gsub(x = Sexo , pattern = "Percentatge.Dona" , replacement = "Dona")) %>%
  mutate(CCAA = forcats::as_factor((CCAA)))

p <- ggplot(data = aa_def , mapping = aes(x = CCAA , y = Percentatge , fill = Any)) +
  geom_col(position = "dodge") +
  facet_grid(rows = vars(Sexo)) +
  labs(title = "GRÀFIC 6: TAXA D'ATUR PER SEXE I COMUNITAT AUTÒNOMA DE RESIDÈNCIA EN ESPANYA EN 1978, 2003 Y 2019" , caption = "Font: INE, EPA. Elaboració pròpia" , x = NULL , y = NULL) +
  theme(plot.caption = element_text(face = "italic", colour = "gray24") ,
    plot.title = element_text(size = 14 , face = "bold", colour = "firebrick") ,
    panel.background = element_rect(fill = "slategray", colour = "snow4") ,
    axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete() +
  scale_y_continuous(breaks = seq(0 , 40 , 2)) +
  scale_fill_brewer(palette = "Reds") +
  scale_color_brewer(palette = "Blues")
options(digits = 2)
ggplotly(p , tooltip = c("Any" ,  "Percentatge" , "CCAA"))
ggsave(filename = "./ExportsVal/13.1 Taxa de paro por sexo y comunidad autónoma de residencia.png" , plot = p , width = 11.5 , height = 8.5)

p <- ggplot(data = aa_def , mapping = aes(x = CCAA , y = Percentatge , fill = Any)) +
  geom_col(position = "dodge") +
  facet_grid(rows = vars(Sexo)) +
  labs(title = "GRÀFIC 6: TAXA D'ATUR PER SEXE I COMUNITAT AUTÒNOMA DE RESIDÈNCIA EN ESPANYA EN 1978, 2003 Y 2019" , caption = "Font: INE, EPA. Elaboració pròpia" , x = NULL , y = NULL) +
  theme(plot.caption = element_text(face = "italic", colour = "gray24") ,
    plot.title = element_text(size = 14 , face = "bold", colour = "firebrick") ,
    panel.background = element_rect(fill = "gray80", colour = "snow4") ,
    axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete() +
  scale_y_continuous(breaks = seq(0 , 40 , 2)) +
  scale_fill_brewer(palette = "Reds") +
  scale_color_brewer(palette = "Blues")
options(digits = 2)
ggplotly(p , tooltip = c("Any" ,  "Percentatge" , "CCAA"))
ggsave(filename = "./ExportsVal/13.2 Taxa de paro por sexo y comunidad autónoma de residencia.png" , plot = p , width = 11.5 , height = 8.5)
}

# Distribución de los hogares españoles según tipo, 1978, 2003 y 2019. (falta)



# Distribución de los hogares españoles según la actividad, 1978, 2003 y 2019. (falta)



# Taxa de paro total y Taxa de incidencia del paro de los hogares Actius españoles, 1978, 2003 y 2019. (falta)




