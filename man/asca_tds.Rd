% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/asca_tds.R
\name{asca_tds}
\alias{asca_tds}
\title{Compute ASCA on Temporal Dominant Sensation (TDS) data.}
\usage{
asca_tds(
  formula,
  data,
  timecol,
  attributes,
  loadings.time.structure = "long",
  time.quantization = NULL,
  ...
)
}
\arguments{
\item{formula}{An object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted. The formula will be applied for each atttribute at each time interval defined by the timecol column.}

\item{data}{A data frame, or object coercible by as.data.frame to a data frame, containing the variables in the model. It must be in long format and must contain a column for Temporal Dominant Sensation binary data, a column reporting the time values, and a column that defines the attributes analyzed.}

\item{timecol}{A string containing the name of the column indicating the time intervals of the TDS dataset.}

\item{attributes}{A string containing the name of the column indicating the attributes of the TDS.}

\item{loadings.time.structure}{A string that specifies whether the estimation of the ASCA decomposition has to be done putting each time unit in the loading values or in the scores values. Standard is "long", for multiple loadings values for each combination of time and attributes. If it is specified "short", the score values will be defined as the combination of ach level of the factor and each unit of time, and the loadings will be the attributes overall values.}

\item{time.quantization}{A single number reporting the number of time units contained in the new intervals used to quantize the time column.}

\item{...}{Optional parameters}
}
\value{
A list of objects containing the results of ASCA decomposition of a structured dataset.
}
\description{
Compute ASCA on Temporal Dominant Sensation (TDS) data.
}
\examples{
\dontrun{
asca_tds(CATA~(sample+assessor)^2, data = tempR::bars \%>\%
gather( time, CATA, 5:455)  \%>\% mutate(time = str_extract(time, "\\\\d+\\\\.\\\\d")),
timecol = "time", attributes = "attribute")
# To quantize the time units in larger intervals, it is possible to
 # specify the number of time unit contained in the new intervals
 # in time.quantization
asca_tds(CATA~(sample+assessor)^2, data = tempR::bars \%>\%
gather( time, CATA, 5:455) \%>\% mutate(time = str_extract(time, "\\\\d+\\\\.\\\\d")),
timecol = "time", attributes = "attribute",
time.quantization = 2)

# To estimate Score values for each unit of time for each level
# of the factors included it is necessary to specify
# loadings.time.structure == "short"

asca_tds(CATA~(sample+assessor)^2, data = tempR::bars \%>\%
gather( time, CATA, 5:455) \%>\% mutate(time = str_extract(time, "\\\\d+\\\\.\\\\d")),
timecol = "time", attributes = "attribute",
loadings.time.structure = "short")

}
}
