
# Calculo do estudo -------------------------------------------------------

# Bibliotecas
library(readxl)
library(tidyquant)
library(magrittr)
library(tidyr)

#Funcao - calcula historico de cotacoes dos ativos -----------------------
historico_cotacoes <- function(tickers, inicio = lubridate::today(), fim = lubridate::today()) {
  tickers %>%
    purrr::map_dfr(~{
      df <- quantmod::getSymbols(.x, from = inicio, to = fim,
                                 source = 'Yahoo', # fonte: API gratuita do Yahoo
                                 auto.assign = FALSE)
      df <- df[,c(4, 6)]
      # 4 - preço de fechamento
      # 6 - preço ajustado
      names(df) <- c('P_fechamento', 'P_ajustado')
      data.frame(timestamp = zoo::index(df), # convertendo em coluna as datas que são indices de linha
                 zoo::coredata(df),
                 symbol = .x,
                 stringsAsFactors = FALSE)
    }) %>%
    dplyr::mutate(symbol = sub('\\.SA$', '', symbol)) %>% # retirando o .SA dos tickers
    dplyr::select(Data = timestamp, Codigo = symbol, P_fechamento, P_ajustado)
}

# Retorno IBOV ------------------------------------------------------------
benchmark <- historico_cotacoes('^BVSP',
                                inicio = as.Date("2017-03-31"), fim = as.Date("2022-04-01")) %>% na.omit()
# Retorno IBOV ------------------------------------------------------------
Ibov_Serie <- retorno <- benchmark %>% 
  dplyr::mutate(
    retorno = P_ajustado/dplyr::lag(P_ajustado, 1)-1
  ) %>% 
  dplyr::mutate(retorno = tidyr::replace_na(retorno, 0)
  )

dados_fundos_estudo <- qs::qread("Dados/Fundos_dados.rds") %>% 
  dplyr::select(CNPJ_FUNDO, DT_COMPTC, VL_QUOTA, DENOM_SOCIAL) %>% 
  dplyr::distinct() 

Ibov_Serie <- Ibov_Serie %>% 
  dplyr:: mutate(DT_COMPTC = as.Date(Data),
                 DENOM_SOCIAL = 'Ibov') %>% 
  dplyr::select(-1) %>% 
  dplyr::mutate(janelas = dplyr::if_else(DT_COMPTC >= "2017-03-31" & DT_COMPTC <= "2018-03-30", "Primeira janela", "0"),
                janelas = dplyr::if_else(DT_COMPTC >= "2018-04-02" & DT_COMPTC <= "2019-03-29", "Segunda Janela", janelas),
                janelas = dplyr::if_else(DT_COMPTC >= "2019-04-01" & DT_COMPTC <= "2020-03-31", "Terceira janela", janelas), 
                janelas = dplyr::if_else(DT_COMPTC >= "2020-04-01" & DT_COMPTC <= "2021-03-31", "Quarta janela", janelas), 
                janelas = dplyr::if_else(DT_COMPTC >= "2021-04-01" & DT_COMPTC <= "2022-03-31", "Quinta janela", janelas)) %>% 
  dplyr::rename('VL_QUOTA' = 'retorno') # nm da base do retorno do DI. Caso seja diferente, renomear


# Crinado um df temp para fazer join com o cdi e respeitar
# o historico de cada fundo ------------------------------------------------------
df_temp <- dados_fundos_estudo %>% 
  dplyr::group_by(DENOM_SOCIAL) %>% 
  dplyr::group_split() %>% 
  purrr::pluck(1)

juntar_dfs <- dplyr::left_join(Ibov_Serie, df_temp, by = 'DT_COMPTC') 

# CDI Index 
ibov_index <- tibble::tibble(VL_QUOTA = juntar_dfs$VL_QUOTA.x, 
                            DT_COMPTC = juntar_dfs$DT_COMPTC,
                            DENOM_SOCIAL =  juntar_dfs$DENOM_SOCIAL.x, 
                            janelas = juntar_dfs$janelas)
# Agrupando, quebrando em grupos, reduzindo dimensao e selecionando o que eh imporatnte
funds_list <- dados_fundos_estudo %>% 
  dplyr::group_by(DENOM_SOCIAL) %>% 
  dplyr::group_split() %>% 
  purrr::map(., ~dplyr::left_join(x = ibov_index, y = ., by = 'DT_COMPTC')) %>% 
  purrr::reduce(dplyr::bind_rows) %>% 
  dplyr::select(c(VL_QUOTA.y, DT_COMPTC, DENOM_SOCIAL.y, janelas))

# Alterando nome da coluna
names(funds_list) <- c('VL_QUOTA', 'DT_COMPTC', 'DENOM_SOCIAL','janelas')

# Setando formatacao dos numeros
options(scipen = 999)

# Retorno por janelas e por fundo
returns_matrix <- funds_list %>% 
  na.omit() %>% 
  dplyr::group_by(DENOM_SOCIAL, janelas) %>% 
  tidyquant::tq_transmute(select = 'VL_QUOTA',
                          mutate_fun = periodReturn, 
                          period = 'daily',
                          col_rename = 'Returns')
# CDI INDEX
ibov_index <- ibov_index %>% 
  dplyr::rename("Returns" = 'VL_QUOTA')

returns_matrix <- rbind(returns_matrix, ibov_index)

# Indicadores
annual_return <- returns_matrix %>% 
  dplyr::group_by(DENOM_SOCIAL, janelas) %>% 
  dplyr::summarise(acumulado = cumprod(1+Returns)-1) %>% 
  dplyr::filter(dplyr::row_number() == dplyr::n())

tracking_error_anual <- returns_matrix %>% 
  dplyr::group_by(DENOM_SOCIAL) %>% 
  dplyr::group_split() %>% 
  purrr::map(., ~dplyr::left_join(x = ibov_index, y = ., by = 'DT_COMPTC')) %>% 
  purrr::reduce(dplyr::bind_rows) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(diferencas = abs(Returns.y) - abs(Returns.x)) %>% 
  dplyr::group_by(DENOM_SOCIAL.y, janelas.x) %>% 
  dplyr::summarise(
    tracking_error = sd(diferencas)*sqrt(252)) %>% 
  dplyr::ungroup() %>% 
  dplyr::rename(
    "DENOM_SOCIAL"= "DENOM_SOCIAL.y",
    "janelas" = "janelas.x"
  )


retorno_real <- annual_return %>% 
  dplyr::group_by(DENOM_SOCIAL) %>% 
  dplyr::group_split() %>% 
  purrr::map(., ~dplyr::left_join(x = ibov_index, y = ., by = 'janelas')) %>% 
  purrr::reduce(dplyr::bind_rows) %>% 
  dplyr::group_by(janelas, DENOM_SOCIAL.y) %>% 
  dplyr::summarise(Ibov = cumprod(Returns+1) -1, Retorno_real = acumulado - Ibov, Retorno = acumulado) %>% 
  dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
  dplyr::rename('DENOM_SOCIAL' = 'DENOM_SOCIAL.y') %>% 
  dplyr::ungroup()


IR <- dplyr::left_join(retorno_real, tracking_error_anual, by = c('DENOM_SOCIAL', 'janelas')) %>% 
  dplyr::group_by(DENOM_SOCIAL, janelas) %>% 
  dplyr::summarise(IR = Retorno_real/tracking_error,  tracking_error = tracking_error, Retorno_real = Retorno_real) %>% 
  dplyr::mutate(Data = dplyr::case_when(janelas == 'Primeira janela' ~ "2017-03-31/2018-03-30",
                                        janelas =='Segunda Janela' ~  "2018-04-02/2019-03-29",
                                        janelas == 'Terceira janela' ~  "2019-04-01/2020-03-31", 
                                        janelas == "Quarta janela" ~ "2020-04-01/2021-03-31", 
                                        janelas == "Quinta janela" ~ "2021-04-01/2022-03-31"))
Mdd <- returns_matrix %>% 
  dplyr::group_by(DENOM_SOCIAL) %>% 
  dplyr::summarise(Mdd_sem_janela = maxDrawdown(Returns)) %>% 
  dplyr::ungroup()

df_final <- dplyr::left_join(Mdd, IR, by = "DENOM_SOCIAL")
# Novos Filtros -----------------------------------------------------------
df_final <- df_final %>%
  dplyr::group_by(
    DENOM_SOCIAL
  ) %>%
  dplyr::summarise(IR = mean(IR),
                   tracking_error = mean(tracking_error),
                   Retorno_real = mean(Retorno_real),
                   Mdd = unique(Mdd_sem_janela)) %>%
  dplyr::ungroup() %>% 
  dplyr::filter(
    IR >= 0, 
    tracking_error >= 0.005
  )

# Exportando --------------------------------------------------------------
rio::export(df_final,
            glue::glue("Resultados/Estudofia1_{format(data_inicio, '%d-%m-%Y')}_{format(data_final, '%d-%m-%Y')}.xlsx"))

