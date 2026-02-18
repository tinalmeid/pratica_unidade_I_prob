#' @file mod_grafico_tema.R
#' @description Módulo para criação de gráficos temáticos utilizando ggplot2, com foco na análise dos acidentes da PRF 2024.
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

# Função auxiliar para ser independente (Quebrar texto de Títulos e Eixos para evitar sobreposição)
quebrar_texto <- function(texto, width) {
    # aplicar a quebra de linha do R
    sapply(texto, function(t) {
        # Quebrar o texto usando strwrap e colapsar com \n
        paste(strwrap(t, width = width), collapse = "\n")
    })
}

# Função salvar gráficos em formato PNG (300 dpi)
salvar_grafico <- function(plot, nome_arquivo) {

    # Definir Caminho e nome do arquivo para salvar o gráfico
    pasta_graficos <- "graficos"
    caminho_completo <- file.path(pasta_graficos, nome_arquivo)

    # Garante que a pasta existe antes de salvar o gráfico
    if (!dir.exists(pasta_graficos)) {
        dir.create(pasta_graficos)
        cat(paste("Pasta '", pasta_graficos, "' criada para salvar os gráficos.\n", sep = ""))
    }

    # Limpa o arquivo se já existir para evitar sobreposição de gráficos
    if (file.exists(caminho_completo)) {
        tryCatch({
            file.remove(caminho_completo)
            cat(paste("Arquivo '", caminho_completo, "' existente removido para evitar sobreposição.\n", sep = ""))
        }, error = function(e) {
            cat(paste("ERRO: Ao tentar remover o arquivo '", caminho_completo, "': ", e$message, "\n", sep = ""))
        })
    }

    # Salvar o gráfico em PNG com resolução de 300 dpi
    ggsave(
        filename = caminho_completo,
        plot = plot,
        width = 12, # Largura em polegadas
        height = 7, # Altura em polegadas
        units = "in",
        dpi = 300 # Resolução
    )

    cat(paste("Gráfico salvo com sucesso em:", caminho_completo, "\n"))
}

# --- TEMA PERSONALIZADO PARA GRÁFICOS ---
theme_personalizado <- function() {
    theme_minimal(base_size = 14) +
        theme(
            # Textos e títulos
            text = element_text(color = "#2c3e50" ),

            # Títulos e legendas
            plot.title = element_text(face = "bold", size = 16, hjust = 0),
            plot.subtitle = element_text(size = 12, margin = margin(b = 15), color = "#7f8c8d"),
            plot.caption = element_text(size = 10, margin = margin(t = 10), color = "#95a5a6"),

            # Legendas
            legend.position = "top",        # Legenda no topo
            legend.justification = "left",  # Alinhada à esquerda
            legend.title = element_blank(), # Remover título da legenda
            legend.text = element_text(size = 10, color = "#34495e"),

            # Linhas de grade
            panel.grid.major.y = element_line(linetype = "dashed", color = "#ecf0f1"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),

            # Eixos
            axis.title = element_text(face = "bold", size = 12),
            axis.text.y = element_text(color = "#34495e"),

            # Eixo X inclinados 45 graus
            axis.text.x = element_text(color = "#34495e", angle = 45, hjust = 1)
        )
}

# --- Paleta de cores personalizada para os gráficos ---
cores_personalizadas <- c(
    azul = "#2980b9",
    vermelho = "#e74c3c",
    cinza = "#bdc3c7",
    cinza_escuro = "#2c3e50",
    verde = "#27ae60",
    amarelo = "#f1c40f",
    branco = "#ffffff"
)

# Fim do arquivo mod_grafico_tema.R
