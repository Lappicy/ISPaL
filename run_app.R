runISPaL <- function(launch.browser = interactive(), ...) {
  app_dir <- normalizePath(
    if (file.exists("app.R")) "." else "ISPaL_Shiny",
    mustWork = TRUE
  )
  shiny::runApp(appDir = app_dir, launch.browser = launch.browser, ...)
}
