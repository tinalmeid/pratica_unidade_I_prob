#' @file mod_severidade_grafico.R
#' @description Módulo para criação de gráficos relacionados à análise de severidade dos acidentes da PRF 2024, utilizando ggplot2.
#' para o Módulo: mod_severidade.R
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

plot_severidade <- function(df_severidade) {

    if (missing(df_severidade) || !is.data.frame(df_severidade)) {
        stop("Erro: O argumento 'df_severidade' é obrigatório e deve ser um data frame.")
    }

    # 2. Preparação dos Dados - O valor da média nacional (para referência na linha do gráfico)
    media_nacional <- unique(df_severidade$referencia_media_nacional)[1]

    dados_plot <- df_severidade |>
        arrange(desc(media_feridos)) |>
        head(10) |>
        mutate(
            uf = reorder(uf, desc(media_feridos))
        )

    # 3. Geração do Gráfico
    ggplot(dados_plot, aes(x = uf, y = media_feridos)) +

        # Barras
        geom_col(fill = cores_personalizadas["amarelo"], width = 0.7) +

        # Linha da Média Nacional (Vermelho)
        geom_hline(yintercept = media_nacional, linetype = "dashed", color = cores_personalizadas["vermelho"], linewidth = 1) +

        # Rótulos nas Barras
        geom_text(aes(label = round(media_feridos, 2)),
                  vjust = -0.5,
                  color = cores_personalizadas["cinza_escuro"],
                  fontface = "bold",
                  size = 3.5) +

        # Rótulo da Linha (Anotação)
        annotate("text",
                 x = length(dados_plot$uf), # Posiciona na última barra
                 y = media_nacional + 0.05, # Um pouco acima da linha
                 label = paste("Média Nacional:", round(media_nacional, 2)),
                 color = cores_personalizadas["vermelho"],
                 fontface = "bold",
                 hjust = 1, # Alinha à direita
                 vjust = 0) +

        scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +

        labs(
            title = "Severidade por Estado (Top 10)",
            subtitle = "UFs com maior média de feridos por acidente",
            x = "Unidade Federativa (UF)",
            y = "Média de Feridos"
        ) +

        # Aplicar o tema personalizado
        theme_personalizado()
}

# Fim do módulo de gráficos para severidade

