#' @file mod_causalidade_grafico.R
#' @description Módulo para criação de gráficos relacionados à análise de condições meteorológicas dos acidentes da PRF 2024, utilizando ggplot2.
#' para o Módulo: mod_causalidade.R
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

plot_causalidade <- function(df_causalidade_meteo) {

    if (missing(df_causalidade_meteo) || !is.data.frame(df_causalidade_meteo)) {
        stop("Erro: O argumento 'df_causalidade_meteo' é obrigatório e deve ser um data frame.")
    }

    dados_plot <- df_causalidade_meteo |>
        arrange(desc(frequencia_absoluta)) |>
        head(10) |>
            mutate(
                condicao_metereologica = reorder(condicao_metereologica, frequencia_absoluta)
            )

    ggplot(dados_plot, aes(x = condicao_metereologica, y = frequencia_absoluta)) +

        # Cores
        geom_col(fill = cores_personalizadas["azul"], width = 0.7) +

        # Rótulos de porcentagem acima das barras
        geom_text(aes(label = paste0(round(probabilidade, 2), "%")),
                  vjust = -0.5,
                  color = cores_personalizadas["cinza_escuro"],
                  fontface = "bold",
                  size = 3.5) +

        # Ajustar os limites do eixo y para dar espaço aos rótulos
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +

        # Títulos e rótulos
        labs(
            title = "Acidentes por Condição Meteorológica",
            subtitle = "Distribuição percentual (PRF 2024)",
            x = "Condição Climática",
            y = "Total de Acidentes"
        ) +

        # Tema personalizado para os gráficos
        theme_personalizado()
}

# Fim do módulo de gráficos de causalidade
