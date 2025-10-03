#' Calcula indicadores financeiros de liquidez e endividamento
#'
#' Esta funcao recebe um data frame resultante da agregacao de contas contabeis por categoria
#' (geralmente produzido pela funcao \code{processar_balanco}) e calcula indicadores classicos de
#' liquidez e endividamento com base nas categorias padronizadas: \code{ACO}, \code{ACF}, \code{PCO},
#' \code{PCF}, \code{ANC}, \code{PNC} e \code{PL}.
#'
#' @param df Um \code{data.frame} com colunas representando anos (valores numericos) e uma coluna chamada
#' \code{Categoria}, contendo os rotulos padronizados das contas contabeis agregadas.
#'
#' @return Um \code{data.frame} onde cada linha representa um indicador financeiro e cada coluna subsequente
#' representa o valor do indicador para um determinado ano.
#'
#' @details As categorias devem seguir os seguintes significados:
#' \itemize{
#'   \item \code{ACO}: Ativo Circulante Operacional
#'   \item \code{ACF}: Ativo Circulante Financeiro
#'   \item \code{PCO}: Passivo Circulante Operacional
#'   \item \code{PCF}: Passivo Circulante Financeiro
#'   \item \code{ANC}: Ativo Nao Circulante
#'   \item \code{PNC}: Passivo Nao Circulante
#'   \item \code{PL}: Patrimonio Liquido
#' }
#'
#' Indicadores calculados:
#' \itemize{
#'   \item \strong{Liquidez Corrente:} (ACO + ACF) / (PCO + PCF)
#'   \item \strong{Liquidez Seca:} ACO / (PCO + PCF)
#'   \item \strong{Liquidez Imediata:} ACF / (PCO + PCF)
#'   \item \strong{Endividamento Geral:} (PCO + PCF + PNC) / (ACO + ACF + ANC)
#'   \item \strong{Composicao do Endividamento:} (PCO + PCF) / (PCO + PCF + PNC)
#'   \item \strong{Imobilizacao do PL:} ANC / PL
#' }
#'
#' @examples
#' # Criando um data frame de balanço com todas as categorias essenciais
#' df <- data.frame(
#'   Categoria = c("ACO", "ACF", "PCO", "PCF", "ANC", "PNC", "PL"),
#'   X2022 = c(1000, 500, 600, 400, 2000, 1500, 2000),
#'   X2023 = c(1200, 600, 700, 500, 2200, 1600, 2300)
#' )
#'
#' # Calculando os indicadores
#' indicadores(df)
#'
#' @export
indicadores <- function(df) {

  # Garantir que todas as categorias essenciais estao presentes
  categorias_necessarias <- c("ACO", "ACF", "PCO", "PCF", "ANC", "PNC", "PL")
  categorias_existentes <- df$Categoria

  if (!all(categorias_necessarias %in% categorias_existentes)) {
    stop("Erro: Algumas categorias essenciais nao foram encontradas no data frame.")
  }

  # Criar um novo data frame apenas com os valores (removendo a coluna 'Categoria')
  df_valores <- df %>% dplyr::select(-Categoria)
  anos <- colnames(df_valores)

  # Criar um novo data frame para armazenar os indicadores
  df_indicadores <- data.frame(Indicador = c(
    "Liquidez Corrente", "Liquidez Seca", "Liquidez Imediata",
    "Endividamento Geral", "Composicao do Endividamento", "Imobilizacao do PL"
  ))

  # Calcular os indicadores para cada ano (coluna)
  for (ano in anos) {
    df_indicadores[[ano]] <- c(
      # Indicadores de Liquidez
      (dplyr::first(df_valores[df$Categoria == "ACO", ano]) + dplyr::first(df_valores[df$Categoria == "ACF", ano])) /
        (dplyr::first(df_valores[df$Categoria == "PCO", ano]) + dplyr::first(df_valores[df$Categoria == "PCF", ano])),

      (dplyr::first(df_valores[df$Categoria == "ACO", ano])) /
        (dplyr::first(df_valores[df$Categoria == "PCO", ano]) + dplyr::first(df_valores[df$Categoria == "PCF", ano])),

      (dplyr::first(df_valores[df$Categoria == "ACF", ano])) /
        (dplyr::first(df_valores[df$Categoria == "PCO", ano]) + dplyr::first(df_valores[df$Categoria == "PCF", ano])),

      # Indicadores de Endividamento
      (dplyr::first(df_valores[df$Categoria == "PCO", ano]) + dplyr::first(df_valores[df$Categoria == "PCF", ano]) + dplyr::first(df_valores[df$Categoria == "PNC", ano])) /
        (dplyr::first(df_valores[df$Categoria == "ACO", ano]) + dplyr::first(df_valores[df$Categoria == "ACF", ano]) + dplyr::first(df_valores[df$Categoria == "ANC", ano])),

      (dplyr::first(df_valores[df$Categoria == "PCO", ano]) + dplyr::first(df_valores[df$Categoria == "PCF", ano])) /
        (dplyr::first(df_valores[df$Categoria == "PCO", ano]) + dplyr::first(df_valores[df$Categoria == "PCF", ano]) + dplyr::first(df_valores[df$Categoria == "PNC", ano])),

      dplyr::first(df_valores[df$Categoria == "ANC", ano]) / dplyr::first(df_valores[df$Categoria == "PL", ano])
    )
  }

  return(df_indicadores)
}

