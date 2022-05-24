require(foreign)
require(haven)
require(dplyr)
require(magrittr)
require(ggplot2)
require(scales)
require(reshape2)
require(labelled)
require(tidyverse)
require(data.table)
require(writexl)


setwd("")

# Task 3 - COMPARAÇÃO CONFIANÇA NAS INSTITUIÇÕES BRASILEIRAS AO LONGO DO TEMPO, 
# VS AMÉRICA LATINA:

wvs <- readRDS("WVS_Trend_v2_0.rds")

## Criar base apenas com as variáveis para confiança (sem a ONU):
wvstrust <- wvs[,c("E069_11", "E069_07", "E069_12", "E069_06", "E069_08",
                   "E069_04", "E069_17", "S020", "COW_ALPHA", "S002")]

## Filtrar NS e NA:
wvstrust <- filter(wvstrust,  E069_11 != "-1")
wvstrust <- filter(wvstrust,  E069_11 != "-2")
wvstrust <- filter(wvstrust,  E069_11 != "-4")
wvstrust <- filter(wvstrust,  E069_11 != "-5")
wvstrust <- filter(wvstrust,  E069_07 != "-1")
wvstrust <- filter(wvstrust,  E069_07 != "-2")
wvstrust <- filter(wvstrust,  E069_07 != "-4")
wvstrust <- filter(wvstrust,  E069_07 != "-5")
wvstrust <- filter(wvstrust,  E069_12 != "-1")
wvstrust <- filter(wvstrust,  E069_12 != "-2")
wvstrust <- filter(wvstrust,  E069_12 != "-5")
wvstrust <- filter(wvstrust,  E069_12 != "-4")
wvstrust <- filter(wvstrust,  E069_06 != "-1")
wvstrust <- filter(wvstrust,  E069_06 != "-2")
wvstrust <- filter(wvstrust,  E069_06 != "-4")
wvstrust <- filter(wvstrust,  E069_06 != "-5")
wvstrust <- filter(wvstrust,  E069_08 != "-1")
wvstrust <- filter(wvstrust,  E069_08 != "-2")
wvstrust <- filter(wvstrust,  E069_08 != "-4")
wvstrust <- filter(wvstrust,  E069_08 != "-5")
wvstrust <- filter(wvstrust,  E069_04 != "-1")
wvstrust <- filter(wvstrust,  E069_04 != "-2")
wvstrust <- filter(wvstrust,  E069_04 != "-4")
wvstrust <- filter(wvstrust,  E069_04 != "-5")
wvstrust <- filter(wvstrust,  E069_17 != "-1")
wvstrust <- filter(wvstrust,  E069_17 != "-2")
wvstrust <- filter(wvstrust,  E069_17 != "-4")
wvstrust <- filter(wvstrust,  E069_17 != "-5")

## Filtrar onda:
wvstrust <- filter(wvstrust,  S002 != "1")
wvstrust <- filter(wvstrust,  S002 != "2")
wvstrust <- filter(wvstrust,  S002 != "4")

# Recodificar para valor maior representar mais confiança:
wvstrust <- remove_labels(wvstrust)
wvstrust <- wvstrust %>% mutate(E069_11 = recode(E069_11, "1" = "4", "2" = "3", "3" = "2", "4" = "1"))
wvstrust <- wvstrust %>% mutate(E069_07 = recode(E069_07, "1" = "4", "2" = "3", "3" = "2", "4" = "1"))
wvstrust <- wvstrust %>% mutate(E069_12 = recode(E069_12, "1" = "4", "2" = "3", "3" = "2", "4" = "1"))
wvstrust <- wvstrust %>% mutate(E069_06 = recode(E069_06, "1" = "4", "2" = "3", "3" = "2", "4" = "1"))
wvstrust <- wvstrust %>% mutate(E069_08 = recode(E069_08, "1" = "4", "2" = "3", "3" = "2", "4" = "1"))
wvstrust <- wvstrust %>% mutate(E069_04 = recode(E069_04, "1" = "4", "2" = "3", "3" = "2", "4" = "1"))
wvstrust <- wvstrust %>% mutate(E069_17 = recode(E069_17, "1" = "4", "2" = "3", "3" = "2", "4" = "1"))

## Transformar em numérico:
wvstrust$COW_ALPHA <- as.factor(as.character(wvstrust$COW_ALPHA))
wvstrust$E069_11 <- as.numeric(as.character(wvstrust$E069_11))
wvstrust$E069_07 <- as.numeric(as.character(wvstrust$E069_07))
wvstrust$E069_12 <- as.numeric(as.character(wvstrust$E069_12))
wvstrust$E069_06 <- as.numeric(as.character(wvstrust$E069_06))
wvstrust$E069_08 <- as.numeric(as.character(wvstrust$E069_08))
wvstrust$E069_04 <- as.numeric(as.character(wvstrust$E069_04))
wvstrust$E069_17 <- as.numeric(as.character(wvstrust$E069_17))

## Criar nova variável que é a soma das confianças (excluindo a confiança na ONU):
wvstrust$index <- (wvstrust$E069_11 + wvstrust$E069_07 + wvstrust$E069_12 + wvstrust$E069_06 +
                     wvstrust$E069_08 + wvstrust$E069_04 + wvstrust$E069_17)/7

# Selecionar países LATAM:
countries <- c("ARG", "CHL", "BRA", "COL", "MEX", "PER", "URU")
wvslatamtrust <- filter(wvstrust, COW_ALPHA %in% countries)

# Calcular médias por onda e por país (LATAM):
avg2 <- wvslatamtrust %>%
  group_by(S002, COW_ALPHA) %>%
  summarise(a_sum=sum(index),
            a_mean=(mean(index)))

# Juntar países LATAM (sem o BR) e calcular média de confiança por onda:
avglatam <- filter(avg2, COW_ALPHA != 'BRA')
avglatam <- avglatam %>%
  group_by(S002) %>%
  summarise(a_sum=sum(a_sum),
            a_mean=(mean(a_mean)))

avglatam <- avglatam %>%
  mutate(region = "LATAM")

# Calcular média de confiança por onda no Brasil:
avgbr <- filter(avg2, COW_ALPHA == 'BRA')
avgbr <- avgbr %>%
  group_by(S002) %>%
  summarise(a_sum=sum(a_sum),
            a_mean=(mean(a_mean)))

avgbr <- avgbr %>%
  mutate(region = "BRA")

# Juntar BR e LATAM:
avgcomp <- full_join(avglatam, avgbr)
avgcomp$region <- as.factor(as.character(avgcomp$region))
avgcomp$S002 <- as.factor(as.numeric(avgcomp$S002))

# Plotar gráfico:
p2 <- ggplot(avgcomp, aes(x = S002, y = a_mean, color = region, group = region))+
  geom_point()+
  geom_line(size=0.8, lineend="round")+
  labs(x='WVS Wave', y = 'Average Trust Score', title = 'Index of institutional trust over time, Brazil vs Latin America',
       caption  = "NOTE: number 1 represents 'no trust' and number 4 'a lot of trust'.")+
  scale_x_discrete(expand = c(0.05, 0.05), labels=c("3\n(1995-1998)", "5\n(2005-2009)", "6\n(2010-2014)", "7\n(2017-2020)"))+
  scale_colour_manual(name = "", values=c("#7A2939","#1F038B"),
                      labels=c("BRA" = "Brazil","LATAM" = "Latin America"))+
  theme_minimal()

# Exportar gráfico em 300dpi
tiff("avgtrustbrtime.tiff", units="in", width=10, height=7, res=300)
p2+theme(plot.caption = element_text(hjust = 0, face= "italic"))
dev.off()
