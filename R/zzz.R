#' Converte texto contabil brasileiro para numero negativo quando necessario
#'
#' Esta funcao transforma strings representando valores contabeis no formato brasileiro
#' (com virgula decimal, ponto como separador de milhar e uso de parenteses para indicar valor negativo)
#' em valores numericos padrao do R.
#'
#' @param vetor Vetor de caracteres contendo os valores a serem convertidos.
#'
#' @return Vetor numerico com os valores convertidos.
#'
#' @keywords internal
#'
conv_br_numeric <- function(vetor) {
  vetor <- gsub("\\(", "-", vetor)      # troca "(" por "-"
  vetor <- gsub("\\)", "", vetor)       # remove ")"
  vetor <- gsub("\\.", "", vetor)       # remove ponto (milhar)
  vetor <- gsub(",", ".", vetor)        # troca virgula decimal por ponto
  as.numeric(vetor)                 # converte para numerico
}


#' Classifica contas contabeis de acordo com categorias predefinidas
#'
#' A funcao recebe o nome de uma conta e retorna sua categoria contabil conforme a lista `categorias`.
#'
#' @param conta Um vetor de caracteres com o(s) nome(s) da(s) conta(s) a classificar.
#'
#' @return Um vetor de caracteres com a categoria correspondente ou \code{NA} se nao classificada.
#'
#' @keywords internal
#'
classificar_conta <- function(conta) {
  for (cat in names(categorias)) {
    if (conta %in% categorias[[cat]]) {
      return(cat)
    }
  }
  return(NA)  # Se a conta nao se encaixar, retorna NA
}



#' Lista de categorias contabeis utilizadas internamente
#'
#' Este objeto lista, em formato de `list`, as categorias contabeis utilizadas
#' para classificacao de contas em funcoes internas do pacote.
#'
#' As chaves representam os grupos (por exemplo, ACF = Ativo Circulante Financeiro),
#' e os vetores associados contem os nomes das contas que pertencem a cada grupo.
#'
#' @format Uma lista nomeada com vetores de caracteres.
#'
#' @keywords internal
#'
categorias <- list(
  ACF = c("caixa e equivalentes de caixa", "aplicacoes financeiras", "titulos e valores mobiliarios"),
  ACO = c("contas a receber de clientes", "adiantamento a fornecedores", "estoques",
          "tributos a recuperar", "outros ativos circulantes"),
  PCF = c("emprestimos e financiamentos", "debentures", "operacoes de credito"),
  PCO = c("fornecedores", "obrigacoes trabalhistas e previdenciarias",
          "tributos a pagar", "adiantamento de clientes", "outras obrigacoes circulantes"),
  ANC = c("ativo nao circulante"),
  PNC = c("passivo nao circulante"),
  PL  = c("patrimonio liquido", "capital social", "reservas de lucros",
          "prejuizos acumulados", "ajustes de avaliacao patrimonial")
)

#' Normaliza texto removendo acentos e cedilha
#'
#' Esta funcao converte o texto para minusculas, normaliza para Unicode NFC
#' e remove todos os acentos, cedilhas e marcas diacriticas, retornando
#' apenas caracteres ASCII basicos.
#'
#' @param x Vetor de caracteres a ser normalizado.
#' @return Vetor de caracteres normalizado.
#' @examples
#' normalizar_texto(c("Ção", "Ótimo", "maçã", "PÃO"))
#' # [1] "cao"   "otimo" "maca" "pao"
#' @export
normalizar_texto <- function(x) {
  x |>
    stringi::stri_trans_nfc() |> # normaliza Unicode
    tolower() |>                 # converte para minusculas
    stringi::stri_trans_general("NFD; [:Nonspacing Mark:] Remove; NFC")
}

# Declara variáveis globais para evitar NOTEs no R CMD check
utils::globalVariables(c("Categoria", "Conta"))
