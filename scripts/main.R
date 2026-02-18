#' @file main.R
#' @description Script principal para executar a análise exploratória e probabilidade dos acidentes da PRF 2024
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

# Carregar pacotes necessários
library(testthat)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr) # replace_na() para lidar com valores ausentes

# --- IMPORTAÇÃO DOS MÓDULOS DE ANÁLISE ---
# Carregar o módulo de preparação dos dados
source("scripts/data_preparation.R")

# Carregar o módulo de análise de causalidade
source("scripts/mod_causalidade.R")

# Carregar o módulo de análise geográfica
source("scripts/mod_geografia.R")

# Carregar o módulo de análise de severidade
source("scripts/mod_severidade.R")

# Carregar o módulo de análise de temporalidade
source("scripts/mod_temporal.R")

# --- EXECUÇÃO PRINCIPAL ---

# Validar se o arquivo de dados existe antes de tentar carregar
if (!exists("df_detran_prf2024")) {
  stop("ERRO: O data frame 'df_detran_prf2024' não foi criado. Verifique o módulo data_preparation.R.")
}
# 1. Executar as análises exploratórias e de probabilidade
analises_causalidade <- analisar_causalidade(df_detran_prf2024)
analises_geografia <- analisar_geografia(df_detran_prf2024)
analises_severidade <- analisar_severidade(df_detran_prf2024)
analises_temporalidade <- analisar_temporal(df_detran_prf2024)

# --- GERAÇÃO DOS GRÁFICOS DAS ANÁLISES ---
cat("************************************************************************\n")
cat("Iniciando geração dos gráficos...\n")
cat("\n")

# Carregar o módulo de Tema e Cores para os gráficos
source("scripts/mod_grafico_tema.R")

# Carrega o módulo de geração de gráficos para causalidade
source("scripts/mod_causalidade_grafico.R")
grafico_causalidade <- plot_causalidade(analises_causalidade$condicao_metereologica)
print(grafico_causalidade)
salvar_grafico(grafico_causalidade, "grafico_causalidade.png")
cat("Gráficos de causalidade gerados com sucesso!\n")

# Carrega o módulo de geração de gráficos para geografia
source("scripts/mod_geografia_grafico.R")
grafico_geografia <- plot_geografia(analises_geografia)
print(grafico_geografia)
salvar_grafico(grafico_geografia, "grafico_geografia.png")
cat("Gráficos de geografia gerados com sucesso!\n")

# Carrega o módulo de geração de gráficos para severidade
source("scripts/mod_severidade_grafico.R")
grafico_severidade <- plot_severidade(analises_severidade)
print(grafico_severidade)
salvar_grafico(grafico_severidade, "grafico_severidade.png")
cat("Gráficos de severidade gerados com sucesso!\n")

# Carrega o módulo de geração de gráficos para temporalidade
source("scripts/mod_temporal_grafico.R")
grafico_temporalidade <- plot_temporal(analises_temporalidade)
print(grafico_temporalidade)
salvar_grafico(grafico_temporalidade, "grafico_temporalidade.png")

cat("\n")
cat("Todos os gráficos foram salvos na pasta 'graficos/'.\n")
cat("\n")


cat("*===============================  FIM  =================================*\n")
cat("Análises exploratórias e de probabilidade dos acidentes da PRF 2024 concluídas com sucesso!\n")
cat("\n")


# Fim do arquivo main.R
