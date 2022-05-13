### Graficos

# Librarys
library(readxl)
library(tidyverse)
library(ggrepel)
library(ggthemes)
library(scales)
library(ggdark)
library(gganimate)
library(gifski)
library(png)
library(av)
library(magrittr)

fundos_mm <-  readxl::read_excel('Resultados/fundo de ações.xlsx') %>% 
  dplyr::rename("Fundo" = "DENOM_SOCIAL")

fundos_mm$Fundo <- gsub('FUNDO DE INVESTIMENTO EM COTAS DE FUNDOS DE INVESTIMENTO EM AÇÕES','FIC FIA',fundos_mm$Fundo)
fundos_mm$Fundo <- gsub('FUNDO DE INVESTIMENTO EM AÇÕES', 'FIA', fundos_mm$Fundo)
fundos_mm$Fundo <- gsub('FUNDO DE INVESTIMENTO DE AÇÕES', 'FIA', fundos_mm$Fundo)
fundos_mm$Fundo <- gsub('FUNDO DE INVESTIMENTO EM COTAS DE FIA', 'FIC FIA', fundos_mm$Fundo)
fundos_mm$Fundo <- gsub('FUNDO DE INVESTIMENTO EM COTAS DE FUNDOS DE INVESTIMENTO DE AÇÕES', 'FIC FIA', fundos_mm$Fundo)
fundos_mm$Fundo <- gsub('FUNDO DE INVESTIMENTO DE ACOES', 'FIA', fundos_mm$Fundo)
fundos_mm$Fundo <- gsub('FUNDO DE INVESTIMENTO EM COTAS DE FUNDOS DE INVESTIMENTO EM ACOES', 'FIC FIA', fundos_mm$Fundo)
fundos_mm$Fundo <- gsub('INSTITUCIONAL', 'INST', fundos_mm$Fundo)
fundos_mm$Fundo <- gsub('FUNDO DE INVESTIMENTO EM COTAS DE FUNDOS DE INVESTIMENTO', 'FIC FI',fundos_mm$Fundo)
fundos_mm$Fundo <- gsub('FUNDO DE INVESTIMENTO EM COTAS DE FUNDOS EM INVESTIMENTO DE AÇÕES', 'FIC FIA', fundos_mm$Fundo)
fundos_mm$Fundo <- gsub('FDO DE INVEST EM COTAS DE FDO DE INVEST','FIC FI', fundos_mm$Fundo)

fundos_mm$Fundo <- gsub('FUNDO DE INVESTIMENTO EM  COTAS DE FUNDOS DE INVESTIMENTO DE AÇÕES','FIC FIA', fundos_mm$Fundo)

fundos_mm$Fundo <- gsub('FUNDO DE INVESTIMENTO AÇÕES', 'FIA',fundos_mm$Fundo)

fundos_mm$Fundo <- gsub('FUNDO DE INVESTIMENTOS EM AÇÕES','FIA', fundos_mm$Fundo)
fundos_mm$Fundo <- gsub('FUNDO DE INVESTIMENTO EM QUOTAS DE FUNDOS DE INVESTIMENTO DE AÇÕES', 'FIC FIA', fundos_mm$Fundo)

fundos_mm$Fundo <- gsub('FI EM COTAS DE FUNDOS DE INVESTIMENTO DE ACOES','FIC FIA',fundos_mm$Fundo)

fundos_mm$Fundo <- gsub('FI EM COTAS DE FUNDOS DE INVESTIMENTO DE ACOES','FIC FIA',fundos_mm$Fundo)

fundos_mm$Fundo <- gsub('FIQ DE FUNDOS DE INVESTIMENTO DE AÇÕES', 'FIC FIA',fundos_mm$Fundo)
fundos_mm$Fundo <- gsub('FI EM COTAS DE FI DE AÇÕES','FIC FIA',fundos_mm$Fundo)
fundos_mm$Fundo <- gsub('FUNDO DE INVESTIMENTO','FI',fundos_mm$Fundo)
fundos_mm$Fundo <- gsub('IBOVESPA','IBOV',fundos_mm$Fundo)


fundos_mm <- fundos_mm %>% 
  dplyr::rename('Tracking Error' = tracking_error, 
                'Alfa' = Retorno_real)
# 0,5 - 2
media <- fundos_mm$IR %>% mean() 

fundos_mm_filtros <- fundos_mm %>%  
  dplyr::filter(IR > media) 

fundos_mm_filtros$Alfa %>% max()
fundos_mm_filtros$Alfa %>% min()
fundos_mm_filtros$`Tracking Error`%>% max()
fundos_mm_filtros$`Tracking Error` %>% min()

fundos_mm_filtros %>%  
  
  ggplot2::ggplot(ggplot2::aes(x = `IR`, y = Mdd)) +
  
  ggdark::dark_theme_classic() +
  
  ggplot2::geom_point(ggplot2::aes(size =  Alfa,
                                   color = `Tracking Error`)) + 
  scale_colour_gradient(low = 'red', 
                        high = 'blue',
                        limits = c(0.015,
                                   0.20),
                        labels = percent) +
  
  # geom_line(data =  data.frame(x = c(0,2),
  #                              y = c(0, 0.04)),
  #           aes(x = x, y = y),
  #           color = 'green', size = 0.20) +
  
  geom_text_repel(aes(label = Fundo),
                  size = 3.2, 
                  color = 'white', 
                  box.padding = 1,
                  max.overlaps = 55) +
  # labs(title = 'Fundos com vol entre 0,5% - 2%', caption = "Período: 29-03-2019 até 31-03-2022") +
  scale_size(range = c(2, 8), 
             limits = c(0.012, 0.25),
             breaks = c(0.01, 0.05, 0.1, 0.15, 0.20, 0.25),
             labels = percent) +
  
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 16),
        title = element_text(size = 12),
        axis.title.x = element_text(color = 'white'),
        axis.title.y = element_text(color = 'white')) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "IR > 0.44")



fundos_mm_1 <- fundos_mm %>% 
  dplyr::filter(IR <= media)


fundos_mm_1$Alfa %>% max()
fundos_mm_1$Alfa %>% min()
fundos_mm_1$`Tracking Error`%>% max()
fundos_mm_1$`Tracking Error` %>% min()


fundos_mm_1 %>%  
  
  ggplot2::ggplot(ggplot2::aes(x = `IR`, y = Mdd)) +
  
  ggdark::dark_theme_classic() +
  
  ggplot2::geom_point(ggplot2::aes(size =  Alfa,
                                   color = `Tracking Error`)) + 
  scale_colour_gradient(low = 'red', 
                        high = 'blue',
                        limits = c(0.02,
                                   0.25),
                        labels = percent) +

  
  # geom_line(data =  data.frame(x = c(0,2),
  #                              y = c(0, 0.04)),
  #           aes(x = x, y = y),
  #           color = 'green', size = 0.20) +
  
  geom_text_repel(aes(label = Fundo),
                  size = 3, 
                  color = 'white', 
                  box.padding = 1,
                  max.overlaps = 65) +
  # labs(title = 'Fundos com vol entre 0,5% - 2%', caption = "Período: 29-03-2019 até 31-03-2022") +
  scale_size(range = c(2, 8), 
             limits = c(0, 0.1),
             breaks = c(0.0, 0.025, 0.05, 0.075, 0.1),
             labels = percent) +
  
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 16),
        title = element_text(size = 12),
        axis.title.x = element_text(color = 'white'),
        axis.title.y = element_text(color = 'white')) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "IR < 0.44")


