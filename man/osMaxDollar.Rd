\name{osMaxDollar}
\alias{osMaxDollar}
\title{Order Size: Max Dollar}
\usage{
  osMaxDollar(data, timestamp, orderqty, ordertype,
    orderside, portfolio, symbol, prefer = "Open",
    tradeSize, maxSize, integerQty = TRUE, ...)
}
\arguments{
  \item{tradeSize}{the dollar value to transact (use
  negative number to sell short)}

  \item{maxSize}{the dollar limit to the position (use
  negative number for short side)}

  \item{integerQty}{a boolean whether or not to truncate to
  the nearest integer of contracts/shares/etc.}
}
\value{
  a quantity to order
}
\description{
  An order sizing function that limits position size based
  on dollar value of the position, rather than quantity of
  shares.
}

