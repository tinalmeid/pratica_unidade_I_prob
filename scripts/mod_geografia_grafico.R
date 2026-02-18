#' @file mod_geografia_grafico.R
#' @description Módulo para criação de gráficos relacionados à análise de geografia dos acidentes da PRF 2024, utilizando ggplot2.
#' para o Módulo: mod_geografia.R
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

plot_geografia <- function(df_geografia) {

    if (missing(df_geografia) || !is.data.frame(df_geografia)) {
        stop("Erro: O argumento 'df_geografia' é obrigatório e deve ser um data frame.")
    }

    # Preparar os dados para o gráfico
    dados_plot <- df_geografia |>
        arrange(desc(total_acidentes)) |>
        head(5) |>
        mutate(
            # Criar uma nova coluna que combina município e UF para o eixo x
            uf = paste(uf),
            uf = reorder(uf, desc(total_acidentes))
        )

    # Criar o gráfico de barras
    ggplot(dados_plot, aes(x = uf, y = total_acidentes)) +

        # Barras
        geom_col(fill = cores_personalizadas["vermelho"], width = 0.7) +

        # Rótulos  acima das barras
        geom_text(aes(label =total_acidentes),
                  vjust = -0.5,
                  color = cores_personalizadas["cinza_escuro"],
                  fontface = "bold",
                  size = 3.5) +

        # Ajustar os limites do eixo y para dar espaço aos rótulos
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +

        # Títulos e rótulos
        labs(
            title = "UFs com Maior Número de Acidentes da PRF 2024",
            subtitle = "Top 5 UFs (PRF 2024)",
            y = "Total de Acidentes",
            x = NULL
        ) +

        # Tema personalizado para os gráficos
        theme_personalizado()

}

# Fim do módulo de gráficos para geografia
