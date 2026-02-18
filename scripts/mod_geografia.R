#' @file mod_geografia.R
#' @description Módulo para análises os pontos críticos de acidentes da PRF 2024
#' Identifica os municípios com maior número de acidentes para fins preventivos ou planejamento de ações de segurança viária.
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

#' @description Função para identificar os municípios com maior número de acidentes
#' @param df_geografia Data frame contendo os dados tratados no 'data_preparation.R'
#' @return Data frame métricas da geografia dos acidentes calculadas

analisar_geografia <- function(df_geografia) {
  cat("************************************************************************\n")
  cat("MÓDULO GEOGRAFIA - Iniciando análises geográfica dos acidentes da PRF 2024...\n")
  cat("\n")

  # Validar se df possui as colunas necessárias para análise de geografia
  colunas_necessarias <- c("municipio", "uf")
  if (!all(colunas_necessarias %in% colnames(df_geografia))) {
    stop("Erro: Colunas geográficas não encontradas no data frame.")
  }

  # --- 1. CALCULO UF COM MAIOR NÚMERO DE ACIDENTES ---
  total_geral <- nrow(df_geografia)

  df_geografia_uf <- df_geografia |>
    group_by(uf) |>
    summarise(
      total_acidentes = dplyr::n(),
      .groups = "drop"
    ) |>
    arrange(desc(total_acidentes)) |>
    mutate(
      # Criar um ranking para identificar UFs mais críticos
      probabilidade = total_acidentes / total_geral * 100,

      ranking = row_number()
    )

  cat("========================================================================\n")
  cat("A UF com maior número de acidentes é:", df_geografia_uf$uf[1],", com um total de", df_geografia_uf$total_acidentes[1],
    "acidentes. Que representa", round(df_geografia_uf$probabilidade[1], 2), "% dos acidentes da PRF 2024.\n")
  cat("\n")

  cat("As 5 primeiras UFs com maior número de acidentes:\n")
  df_print_uf <- df_geografia_uf |>
    head(5) |>
    select(ranking, uf, total_acidentes, probabilidade)

  print(as_tibble(df_print_uf))
  cat("\n")

  # --- 2. CALCULO MUNICÍPIO COM MAIOR NÚMERO DE ACIDENTES ---
  df_geografia_municipio <- df_geografia |>
    group_by(municipio, uf) |>
    summarise(
      total_acidentes = dplyr::n(),
      .groups = "drop"
    ) |>
    arrange(desc(total_acidentes)) |>
    mutate(
      probabilidade = total_acidentes / total_geral * 100,
      ranking = row_number()
    )
  cat("========================================================================\n")
  cat("O município com maior número de acidentes é:", df_geografia_municipio$municipio[1], "(", df_geografia_municipio$uf[1], ")", ", com um total de", df_geografia_municipio$total_acidentes[1],
    "acidentes. Que representa", round(df_geografia_municipio$probabilidade[1], 2), "% dos acidentes da PRF 2024.\n")
  cat("\n")

  cat("As 5 primeiros municípios com maior número de acidentes:\n")
  df_print_municipio <- df_geografia_municipio |>
    head(5) |>
    select(ranking, municipio, uf, total_acidentes, probabilidade)
  print(as_tibble(df_print_municipio))
  cat("\n")

  cat("Fim das análises geográficas.\n")

  # Retornar o data frame para uso análises gráficas
  return(df_geografia_uf)
}

# log
message("Módulo 'mod_geografia.R' carregado com sucesso.")

# Fim do arquivo mod_geografia.R
