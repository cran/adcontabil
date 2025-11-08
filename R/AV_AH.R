#' Calcular Análise Vertical e Horizontal com projeção
#'
#' Esta função realiza a Análise Vertical (AV) e Análise Horizontal (AH) com base em dados contábeis de empresas, podendo ser aplicada tanto a dados agregados quanto a dados detalhados (ex: Balanço Patrimonial individualizado). Além disso, gera uma projeção para o ano seguinte, assumindo um crescimento de 5% nos valores.
#'
#' @param df Um data frame contendo os dados contábeis. Deve conter colunas com valores numéricos para diferentes anos, além de uma coluna identificadora da natureza das contas (por exemplo, \code{"Categoria"} ou \code{"Conta"}).
#' @param tipo Um parâmetro do tipo \code{character} que indica a estrutura do data frame. Deve ser \code{"agregado"} quando os dados estão organizados por categorias (ex: ACO, ANC, PL etc.), ou outro valor (ex: \code{"detalhado"}) quando as contas individuais estão identificadas por uma coluna chamada \code{"Conta"}.
#'
#' @details
#' A Análise Vertical (AV) expressa cada item patrimonial como uma proporção do total do ativo ou passivo correspondente no mesmo ano.
#'
#' A Análise Horizontal (AH) compara a evolução dos valores ao longo dos anos, em relação ao primeiro ano da base de dados (ano base).
#'
#' A função ainda projeta valores para o ano seguinte com base em um crescimento linear de 5% sobre os valores do último ano disponível.
#'
#' @return Uma lista com dois data frames:
#' \describe{
#'   \item{\code{AV_AH}}{Data frame contendo os valores originais, os resultados da Análise Vertical (com sufixo \code{_AV}) e da Análise Horizontal (com sufixo \code{_AH}).}
#'   \item{\code{Projecao}}{Data frame contendo a projeção de valores para o ano seguinte, com base em um crescimento de 5\%.}
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate summarise select rename_with across filter relocate left_join
#' @importFrom dplyr where all_of ends_with everything
#' @importFrom data.table :=
#' @examples
#' # Criando dados agregados de exemplo
#' df <- data.frame(
#'   Conta = c("Ativo Circulante", "Passivo Circulante"),
#'   X2022 = c(1000, 800),
#'   X2023 = c(1200, 900)
#' )
#'
#' # Padronizando o balanço
#' resultado <- padronizar_balanco(df)
#'
#' # Calculando análise vertical e horizontal para dados agregados
#' av_ah <- calcular_AV_AH(resultado$agregado, tipo = "agregado")
#'
#' # Visualizando partes do resultado
#' head(av_ah$AV_AH)
#' head(av_ah$Projecao)
#' @export
calcular_AV_AH <- function(df, tipo) {
  `%>%` <- magrittr::`%>%`

  # Definir se estamos lidando com df_agregado ou BP_SLCE_P
  nome_coluna_categoria <- ifelse(tipo == "agregado", "categorias_bp", "Conta")

  # Garantir que os valores são numéricos
  df <- df %>%
    dplyr::mutate(dplyr::across(where(is.character), as.character)) %>%  # Garantir que texto é texto
    dplyr::mutate(dplyr::across(where(is.numeric), as.numeric))  # Garantir que números são números

  # Identificar os anos (colunas numéricas de interesse)
  if (tipo == "agregado") {
    anos <- colnames(df)[-1]  # Todas exceto a primeira (Categoria)
  } else {
    anos <- colnames(df)[2:(ncol(df) - 1)]  # Exclui a primeira (Conta) e a última (Categoria)
  }

  # Determinar Ativo Total e Passivo Total para cada ano
  if (tipo == "agregado") {
    df_totais <- df %>%
      dplyr::filter(categorias_bp %in% c("ACO", "ACF", "ANC")) %>%
      dplyr::summarise(across(dplyr::all_of(anos), sum, na.rm = TRUE)) %>%
      dplyr::rename_with(~ paste0(.x, "_Ativo_Total"))

    df_totais_passivo <- df %>%
      dplyr::filter(categorias_bp %in% c("PCO", "PCF", "PNC", "PL")) %>%
      dplyr::summarise(dplyr::across(all_of(anos), sum, na.rm = TRUE)) %>%
      dplyr::rename_with(~ paste0(.x, "_Passivo_Total"))
  } else {
    df_totais <- df %>%
      dplyr::filter(Conta == "ativo total") %>%
      dplyr::select(dplyr::all_of(anos)) %>%
      dplyr::rename_with(~ paste0(.x, "_Ativo_Total"))

    df_totais_passivo <- df %>%
      dplyr::filter(Conta == "passivo total") %>%
      dplyr::select(dplyr::all_of(anos)) %>%
      dplyr::rename_with(~ paste0(.x, "_Passivo_Total"))
  }

  # Criar Análise Vertical (AV)
  df_av <- df %>%
    dplyr::left_join(df_totais, by = character()) %>%
    dplyr::left_join(df_totais_passivo, by = character()) %>%
    dplyr::mutate(across(dplyr::all_of(anos),
                         ~ ifelse(categorias_bp %in% c("ACO", "ACF", "ANC"),
                                  . / get(paste0(dplyr::cur_column(), "_Ativo_Total")),
                                  . / get(paste0(dplyr::cur_column(), "_Passivo_Total"))),
                         .names = "{.col}_AV")) %>%
    dplyr::select(-ends_with("_Ativo_Total"), -dplyr::ends_with("_Passivo_Total"))  # Remover colunas auxiliares

  # Definir o ano base para Análise Horizontal
  ano_base <- anos[1]

  # Criar Análise Horizontal (AH)
  df_ah <- df_av %>%
    dplyr::mutate(across(all_of(anos), ~ . / get(ano_base), .names = "{.col}_AH"))

  # Adicionar linha para representar o ano seguinte (previsto)
  df_ano_seguinte <- df_ah %>%
    dplyr::select(dplyr::all_of(anos)) %>%
    dplyr::mutate(dplyr::across(everything(), ~ . * 1.05)) %>%  # Simulação de crescimento de 5%
    dplyr::mutate(!!nome_coluna_categoria := paste0("Ano Seguinte_", df[[nome_coluna_categoria]])) %>%
    dplyr::relocate(!!nome_coluna_categoria, .before = 1)  # Move a coluna para a primeira posição

  # Retornar ambos os data.frames em uma lista
  return(list(
    AV_AH = df_ah,
    Projecao = df_ano_seguinte
  ))
}
