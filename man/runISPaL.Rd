\name{runISPaL}
\alias{runISPaL}
\title{Run the ISPaL dashboard}
\usage{
runISPaL(launch.browser = interactive(), ...)
}
\arguments{
  \item{launch.browser}{Whether to open the dashboard in the default browser.}
  \item{...}{Additional arguments passed to \code{shiny::runApp()}, such as
  \code{port = 3838}.}
}
\value{
The return value of \code{shiny::runApp()}, normally returned invisibly when
the application stops.
}
\description{
Starts the self-contained ISPaL Shiny dashboard installed from GitHub.
}
