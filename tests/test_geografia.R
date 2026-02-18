#' @file test_geografia.R
#' @description  Testes unitários para validar as funções do módulo mod_geografia.R, garantindo a precisão e confiabilidade das análises.
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

MODO_TESTE <- TRUE

# --- CARREGAMENTO DO MÓDULO DE PREPARAÇÃO DE DADOS ---
library(testthat)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)

if (file.exists("scripts/mod_geografia.R")) {
  source("scripts/mod_geografia.R")
} else if (file.exists("../scripts/mod_geografia.R")) {
  source("../scripts/mod_geografia.R")
} else {
  stop("ERRO: O arquivo 'mod_geografia.R' não foi encontrado. Verifique o caminho e o nome do arquivo.")
}

cat("************************************************************************\n")
cat("INICIANDO TESTES UNITÁRIOS - Módulo de Geografia\n")
cat("\n")

# Ciclo de Testes para a função analisar_geografia()
test_that("analisar_geografia retorna estrutura correta", {

    # Criar Mock de dados para teste
    df_teste <- data.frame(
        municipio = c("Belo Horizonte", "Contagem", "Betim", "Curitiba", "Londrina", "Maringá", "Boa Vista"),
        uf = c("MG", "PR", "RR", "PR", "PR", "PR", "RR"),
        stringsAsFactors = FALSE
    )

    # Executar a função de análise geográfica
    resultado_geografia <- analisar_geografia(df_teste)

    # Teste 1: Verificar se o retorno é um data.frame
    expect_s3_class(resultado_geografia, "data.frame")

    # Teste 2: Verificar se as colunas esperadas estão presentes
    expect_true("uf" %in% colnames(resultado_geografia))
    expect_true("total_acidentes" %in% colnames(resultado_geografia))
    expect_true("ranking" %in% colnames(resultado_geografia))
    expect_true("probabilidade" %in% colnames(resultado_geografia))
})

# Ciclo de Testes para Verificar Ranking de UFs por total de acidentes
test_that("Ranking de UFs é calculado corretamente", {
    # Criar Mock de dados para teste
    # MG: 3 acidentes, PR: 2 acidentes, RR: 1 acidente (Ranking esperado: MG 1º, PR 2º, RR 3º)
    df_teste <- data.frame(
        municipio = c(
            rep("MG", 3), # 3 Acidentes (Será 1º)
            rep("PR", 2), # 2 Acidentes (Será 2º)
            rep("RR", 1)  # 1 Acidente  (Será 3º)
    ),
    uf = c(rep("MG", 3), rep("PR", 2), rep("RR", 1)),
    stringsAsFactors = FALSE
    )

    # Executar a função de análise geográfica
    resultado_geografia <- analisar_geografia(df_teste)

    # Teste 1: Verificar se o ranking respeitou a quantidade de linhas (MG 1, PR 2, RR 3)
    expect_equal(resultado_geografia$ranking[resultado_geografia$uf == "MG"], 1)
    expect_equal(resultado_geografia$ranking[resultado_geografia$uf == "PR"], 2)
    expect_equal(resultado_geografia$ranking[resultado_geografia$uf == "RR"], 3)
})

# Ciclo de Testes para Calcula de total de acidentes por UF
test_that("Soma de acidentes por UF é calculada corretamente", {
    # Criar Mock de dados para teste (MG: 3 acidentes, PR: 2 acidentes, RR: 1 acidente, Total Geral: 6 acidentes)
    df_teste <- data.frame(
        municipio = c(
            rep("MG", 3),
            rep("PR", 2),
            rep("RR", 1)
        ),
        uf = c(rep("MG", 3), rep("PR", 2), rep("RR", 1)), # Adicionamos UF pois o group_by usa ela
        stringsAsFactors = FALSE
    )

    # Executar a função de análise geográfica
    resultado_geografia <- analisar_geografia(df_teste)

    # Teste 1: Verificar se a soma absoluta de MG bate (3)
    total_bh <- resultado_geografia$total_acidentes[resultado_geografia$uf == "MG"]
    expect_equal(total_bh, 3)

    # Teste 2: Verifica se a soma absoluta de PR bate (2)
    total_contagem <- resultado_geografia$total_acidentes[resultado_geografia$uf == "PR"]
    expect_equal(total_contagem, 2)

    # Teste 3: Verificar se a soma absoluta de RR bate (1)
    total_rr <- resultado_geografia$total_acidentes[resultado_geografia$uf == "RR"]
    expect_equal(total_rr, 1)

    # Teste 4: Verificar se a soma total de acidentes bate com o número de linhas do data frame (6)
    expect_equal(sum(resultado_geografia$total_acidentes), nrow(df_teste))
})

cat("TESTES UNITÁRIOS FINALIZADOS - Módulo de Geografia\n")

# Fim do arquivo test_geografia.R
