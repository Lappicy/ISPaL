#' Run the ISPaL dashboard
#'
#' Starts the self-contained ISPaL Shiny dashboard installed from GitHub.
#'
#' @param launch.browser Whether to open the dashboard in the default browser.
#' @param ... Additional arguments passed to [shiny::runApp()]. For example,
#'   `port = 3838`.
#'
#' @return The return value of [shiny::runApp()] (normally returned invisibly
#'   when the application stops).
#' @export
runISPaL <- function(launch.browser = interactive(), ...) {
  app_dir <- system.file("app", package = "ISPaLShiny")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop(
      "O painel não foi encontrado. Reinstale o pacote ISPaLShiny a partir do GitHub.",
      call. = FALSE
    )
  }

  shiny::runApp(appDir = app_dir, launch.browser = launch.browser, ...)
}
