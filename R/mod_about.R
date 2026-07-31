mod_about_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      bslib::card_header("Ajuda"),
      shiny::tags$ol(
        shiny::tags$li(
          "Na aba Dados, importe as tabelas. A identificação das colunas é automática ",
          "e pode ser revisada no Diagnóstico."
        ),
        shiny::tags$li("Na aba Configuração, escolha modelos, leads, meses e períodos."),
        shiny::tags$li("Execute e acompanhe o progresso."),
        shiny::tags$li("Consulte e exporte as tabelas em Resultados."),
        shiny::tags$li("Explore as métricas na aba Gráficos.")
      ),
      shiny::div(
        class = "help-note",
        shiny::strong("Importante: "),
        "PARX testa subconjuntos das covariáveis. Com 10 variáveis há 1.024 combinações ",
        "por série, lead e mês; execuções extensas podem levar tempo."
      )
    ),
    bslib::card(
      bslib::card_header("Como citar"),
      shiny::h5("Artigo principal sobre uso do ISPaL"),
      shiny::p(
        class = "citation",
        "Lappicy, T., & Lima, C. H. (2023). Enhancing monthly streamflow ",
        "forecasting for Brazilian hydropower plants through climate index integration ",
        "with stochastic methods. RBRH, 28, e48. ",
        shiny::a(
          "https://doi.org/10.1590/2318-0331.282320230118",
          href = "https://doi.org/10.1590/2318-0331.282320230118",
          target = "_blank"
        )
      ),
      shiny::h5("Artigo comparativo entre o ISPaL e outros modelos"),
      shiny::p(
        class = "citation",
        "Treistman, F., Penna, D. D. J., Khenayfis, L. D. S., Cavalcante, N. B. R., ",
        "Souza Filho, F. D. A. D., Rocha, R. V., Estácio, A. B., Rolim, L. Z. R., ",
        "Pontes Filho, J. D. A., Porto, V. C., Guimarães, S. O., Pessanha, J. F. M., ",
        "Almeida, V. A., Chan, P. D. S. C., Lappicy, T., Lima, C. H. R., ",
        "Detzel, D. H. M., & Bessa, M. R. (2023). A framework to evaluate and compare ",
        "synthetic streamflow scenario generation models. RBRH, 28, e43. ",
        shiny::a(
          "https://doi.org/10.1590/2318-0331.282320230115",
          href = "https://doi.org/10.1590/2318-0331.282320230115",
          target = "_blank"
        )
      )
    )
  )
}
