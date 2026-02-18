#' @file mod_temporal_grafico.R
#' @description Módulo para criação de gráficos relacionados à análise de temporalidade dos acidentes da PRF 2024, utilizando ggplot2.
#' para o Módulo: mod_temporal.R
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
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)

plot_temporal <- function(df_temporal_prob) {

    if (missing(df_temporal_prob) || !is.data.frame(df_temporal_prob)) {
        stop("Erro: O argumento 'df_temporal_prob' é obrigatório e deve ser um data frame.")
    }

    # 2. Preparação dos Dados (Agrupamento)
    dados_plot <- df_temporal_prob |>
        group_by(eh_fim_semana) |>
        summarise(
            total_acidentes = sum(frequencia),
            probabilidade_total = sum(probabilidade),
            .groups = "drop"
        ) |>
        mutate(
            # Ordenar do maior para o menor
            eh_fim_semana = reorder(eh_fim_semana, desc(total_acidentes))
        )

    # 3. Geração do Gráfico
    ggplot(dados_plot, aes(x = eh_fim_semana, y = total_acidentes)) +

        # Barras
        geom_col(fill = cores_personalizadas["verde"], width = 0.6) +

        # Rótulo Principal: Quantidade Absoluta
        geom_text(aes(label = total_acidentes),
                  vjust = -0.5,
                  color = "#2c3e50",
                  fontface = "bold",
                  size = 4) +

        # Rótulo Secundário: Porcentagem (Abaixo do número)
        geom_text(aes(label = paste0("(", round(probabilidade_total, 1), "%)")),
                  vjust = 1.5, # Joga para dentro da barra (topo)
                  color = "white", #
                  fontface = "bold",
                  size = 3.5) +

        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +

        labs(
            title = "Acidentes: Dia Útil vs Fim de Semana",
            subtitle = "Comparativo de ocorrências por tipo de dia (PRF 2024)",
            x = NULL,
            y = "Total de Registros"
        ) +

        theme_personalizado()
}

# Fim arquivo mod_temporal_grafico.R
