\name{pieBarViz}
\alias{pieBarViz}
\title{Pie Bar Vizualisation}
\usage{
pieBar(Dataset,Stacked = FALSE )
}
\description{

}
\examples{

  chart2 <-
    read.table(text = "Name No_Mutation ATM ATR BRCA2 NBN PALB2  PMS2 POLD1 PRSS1 RAD51D SLX4 XRCC2
               'Aggressive Cases' 85 2 1 2 2 1 1 1 1 1 1 1",
               header = TRUE)
  newData <- melt(chart2, id.var = "Name")

  pieBar(newData)

}
