app_dir <- normalizePath(
  if (file.exists("app.R")) "." else "ISPaL_Shiny",
  mustWork = TRUE
)
shiny::runApp(app_dir, launch.browser = TRUE)

