#' @file mod_causalidade.R
#' @description Módulo para análise de causalidade dos acidentes da PRF 2024.
#' Identifica as principais causas  e tipos de acidentes para fins preventivos(educativos) ou planejamento de ações de segurança
#'
#' @title Aula Prática Disciplina Probabilidade e Estatística para Análise de Dados - Unidade I Aula 4
#' @name Manipulação e análise de dados com R
#'
#' @author Cristina de Almeida
#' @version 1.0
#' @date Fevereiro 2026

# --- CONFIGURAÇÃO INICIAL ---
options(radian.enabled = TRUE) # Configurar para usar R no VS Code
options(OutDec = ",", big.mark = ".") # Exibir "," como separador decimal e "." como separador de milhar
options("repos" = c(CRAN = "https://cloud.r-project.org/")) # Definir o repositório padrão do CRAN para instalação de pacotes

#' @description Função para identificar as principais causas e tipos de acidentes
#' @param df_causalidade Data frame contendo os dados tratados no 'data_preparation.R'
#' @return Data frame com as métricas de causalidade calculadas

analisar_causalidade <- function(df_causalidade) {
  cat("************************************************************************\n")
  cat("MÓDULO CAUSALIDADE - Iniciando análises de causalidade dos acidentes da PRF 2024...\n")
  cat("\n")

  # Buscar os tipo únicos de condição meteorológica
  valores_unicos_meteo <- unique(df_causalidade$condicao_metereologica)

  cat("========================================================================\n")
  cat("Tipo de condição meteorológica encontradas no data frame:\n")
  print(valores_unicos_meteo)
  cat("\n")

  # Validar se df possui as colunas necessárias para análise de causalidade
  colunas_necessarias <- c("causa_acidente", "tipo_acidente", "condicao_metereologica")
  if (!all(colunas_necessarias %in% colnames(df_causalidade))) {
    stop("Erro: Colunas de causalidade não encontradas no data frame.")
  }

  # --- 1. CALCULO DA MODA DA CAUSA DE ACIDENTE ---
  df_causalidade_causa <- df_causalidade |>
    group_by(causa_acidente) |>
    summarise(
      frequencia_absoluta = dplyr::n(),
      .groups = "drop"
    ) |>
    mutate(
      frequencia_relativa = frequencia_absoluta / sum(frequencia_absoluta) * 100
    ) |>
    arrange(desc(frequencia_absoluta))

  cat("========================================================================\n")
  cat("A MODA da causa de acidente é:", df_causalidade_causa$causa_acidente[1],", com frequência absoluta de", df_causalidade_causa$frequencia_absoluta[1],
    ". Que representa", round(df_causalidade_causa$frequencia_relativa[1], 2),"% dos acidentes.\n")
  cat("\n")

  cat("As 5 causas mais frequentes de acidentes são:\n")
  print(as_tibble(head(df_causalidade_causa, 5)))
  cat("\n")

  # --- 2. CALCULO DA MODA DO TIPO DE ACIDENTE ---
  df_causalidade_tipo <- df_causalidade |>
    group_by(tipo_acidente) |>
    summarise(
      frequencia_absoluta = dplyr::n(),
      .groups = "drop"
    ) |>
    mutate(
      frequencia_relativa = frequencia_absoluta / sum(frequencia_absoluta) * 100
    ) |>
    arrange(desc(frequencia_absoluta))

  cat("A MODA do tipo de acidente é:", df_causalidade_tipo$tipo_acidente[1],", com frequência absoluta de", df_causalidade_tipo$frequencia_absoluta[1],
      ". Que representa", round(df_causalidade_tipo$frequencia_relativa[1], 2),"% dos acidentes.\n")
  cat("\n")

  cat("Os 5 tipos de acidentes mais frequentes são:\n")
  print(as_tibble(head(df_causalidade_tipo, 5)))
  cat("\n")

  # --- 3. CALCULO PROBABILIDADE POR CONDIÇÃO METEOROLÓGICA ---

  total_acidentes <- nrow(df_causalidade)

  df_causalidade_meteo <- df_causalidade |>
    group_by(condicao_metereologica) |>
    summarise(
      frequencia_absoluta = dplyr::n(),
      .groups = "drop"
    ) |>
    mutate(
      probabilidade = frequencia_absoluta / total_acidentes * 100
    ) |>
    arrange(desc(frequencia_absoluta))

  # Calcular probabilidade individual para CADA condição meteorológica
  # Percorre todas as condições encontradas no dataset e exibe a probabilidade
  cat("========================================================================\n")
  cat("Probabilidade por condição meteorológica:\n")
  cat("\n")

  # Iterar sobre cada condição e exibir sua probabilidade individualmente
  for (i in seq_len(nrow(df_causalidade_meteo))) {
    cat("  P(", df_causalidade_meteo$condicao_metereologica[i], ") = ",
        format(df_causalidade_meteo$frequencia_absoluta[i], big.mark = "."),
        " acidentes -> ",
        round(df_causalidade_meteo$probabilidade[i], 2), "%\n", sep = "")
  }
  cat("\n")

  # Calcular a probabilidade combinada de condições claras: "Céu Claro" + "Sol"
  prob_condicoes_claras <- sum(df_causalidade_meteo$probabilidade[
      df_causalidade_meteo$condicao_metereologica %in% c("Céu Claro", "Sol")])

  cat("  P(Condições Claras) = P(Céu Claro) + P(Sol) = ", round(prob_condicoes_claras, 2), "%\n", sep = "")

  # Calcular a probabilidade combinada de condições adversas: (todas excluindo "Céu Claro" + "Sol")
  condicoes_adversas <- c("Chuva", "Nublado", "Garoa/Chuvisco", "Nevoeiro/Neblina", "Vento", "Granizo", "Neve")

  prob_adversas <- sum(df_causalidade_meteo$probabilidade[
      df_causalidade_meteo$condicao_metereologica %in% condicoes_adversas])

  cat("  P(Condições Adversas) = P(Chuva) + P(Nublado) + P(Neblina)... = ", round(prob_adversas, 2), "%\n", sep = "")

  # Calcular probabilidade que não seja P(Condições Claras)  e  P(Condições Adversas), existe o valor ignorado no dataset
  prob_ignoradas <- 100 - (prob_condicoes_claras + prob_adversas)
  cat("  P(Condições Ignoradas) = 100% - P(Condições Claras) - P(Condições Adversas) = ", round(prob_ignoradas, 2), "%\n", sep = "")
  cat("\n")

  cat("Fim das análises de causalidade.\n")

  # Retornar os data frames para uso análises gráficas
  return(list(
    causa_acidente = df_causalidade_causa,
    tipo_acidente = df_causalidade_tipo,
    condicao_metereologica = df_causalidade_meteo
  ))
}

# log
message("Módulo 'mod_causalidade.R' carregado com sucesso.")

# Fim do arquivo mod_causalidade.R
