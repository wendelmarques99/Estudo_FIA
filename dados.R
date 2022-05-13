# Dados -------------------------------------------------------------------

# Biblioteca --------------------------------------------------------------
library(dplyr)
library(jsonlite)
library(qs)

# Funcao API Okanebox -  Cotas de fundos
getfund <- function(cnpj, first_date, last_date){
  
  URL <- glue::glue("https://www.okanebox.com.br/api/fundoinvestimento/hist/{cnpj}/{first_date}/{last_date}/")
  
  outputdata <- URL %>% 
    jsonlite::fromJSON()
  
  return(outputdata)
  
}
# Datas
data_inicio <- as.Date("2017-03-31")
data_final <- as.Date("2022-03-31")

data_inicio_format <- "20170331"
data_final_format <- "20220331"

# Base com informacoes cadastrais dos fundos - fonte: http://dados.cvm.gov.br/dados/FI/CAD/DADOS/cad_fi.csv
cadastro <- data.table::fread("http://dados.cvm.gov.br/dados/FI/CAD/DADOS/cad_fi.csv")

cadastro <- cadastro %>% 
  dplyr::filter(SIT == "EM FUNCIONAMENTO NORMAL",
                CLASSE == "Fundo de Ações",
                CONDOM == "Aberto",
                FUNDO_EXCLUSIVO == "N",
                VL_PATRIM_LIQ >= 50000000, 
                DT_INI_ATIV <= data_inicio) 

# Limpando CNPJ para ficar do estilo da API
cadastro$CNPJ_FUNDO <- gsub(pattern = "\\.", replacement = "", x = cadastro$CNPJ_FUNDO)  

cadastro$CNPJ_FUNDO <-  gsub(pattern = "\\/", "", cadastro$CNPJ_FUNDO) 

cadastro$CNPJ_FUNDO <- gsub(pattern = "-", "", cadastro$CNPJ_FUNDO) 

# Rodando a funcao para todos os fundos que passaram nos filtros
fundos_dados <- purrr::map_dfr(.x = unique(cadastro$CNPJ_FUNDO),
                               ~getfund(cnpj = .x, 
                                        first_date = data_inicio_format,
                                        data_final_format)) %>% 
  dplyr::mutate(DT_COMPTC = as.Date(DT_COMPTC))

# Novo filtro. pegar a ultima data. Para saber o numero de cotista nela.
fundos_cnpj_NRCOTISTA_filtro <- fundos_dados %>% 
  dplyr::filter(DT_COMPTC == dplyr::last(fundos_dados$DT_COMPTC)) %>% 
  dplyr::filter(NR_COTST > 100) %>% 
  dplyr::select(CNPJ_FUNDO)

# pegando os filtros que tem mais de 100 cotistas na bd "fundos dados"
fundos_dados <- fundos_dados %>% 
  dplyr::filter(CNPJ_FUNDO %in% fundos_cnpj_NRCOTISTA_filtro$CNPJ_FUNDO)

# Juntando os dois Dfs
fundos_dados <- dplyr::left_join(x = fundos_dados , y =  cadastro, by = "CNPJ_FUNDO")

# Save df como rds
qsave(fundos_dados, "Dados/Fundos_dados.rds")
