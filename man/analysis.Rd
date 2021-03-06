% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis.R
\name{analysis}
\alias{analysis}
\title{Run Statistical Analysis of Monthly Background Checks of Gun Purchase}
\usage{
analysis(debug = FALSE, verbose = FALSE)
}
\arguments{
\item{debug}{Optional boolean switch to indicate whether interim data is displayed;
default is \sQuote{FALSE}}

\item{verbose}{Optional boolean switch to indicate whether verbose operation is
desired, default is \sQuote{FALSE}}
}
\value{
A \code{data.frame} is returned, contained all different prepared columns.
}
\description{
Run Statistical Analysis of Monthly Background Checks of Gun Purchase
}
\examples{
\dontrun{
  gs <- analysis()
  gunsales::plot_gunsales(gs)
  gunsales::ggplot_gunsales(gs)
  all.equal(gs, gunsales::analysis())
}
}
\seealso{
The NY Times article presenting this analsysi undertaken by the \code{gunsales} package is
at \url{http://www.nytimes.com/interactive/2015/12/10/us/gun-sales-terrorism-obama-restrictions.html?}
}
\author{
Gregor Aisch and Josh Keller wrote the (initial) R code; Dirk Eddelbuettel created
and maintains the \code{gunsales} package as well as this package.
}
