\name{agg.chart.ME}
\alias{agg.chart.ME}
\title{Aggregate Maximum Excursion Charts}
\usage{
  agg.chart.ME(Portfolio, Symbols, type = c("MAE", "MFE"),
    scale = c("cash", "percent", "tick"))
}
\arguments{
  \item{Portfolio}{the portfolio name (string)}

  \item{Symbols}{the symbols for which to plot}

  \item{type}{MAE for Maximum Adverse Excursion or MFE for
  Maximum Favorable Excursion}

  \item{scale}{scale of P&L (cash, tick, percent)}
}
\value{
  plots all the trades among the symbols in one chart.
  Useful with equal risk order sizing.
}
\description{
  Aggregate Maximum Excursion Charts
}

