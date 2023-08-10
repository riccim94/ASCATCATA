\name{tcatasca}
\alias{tcatasca}
\title{Application of ANOVA simultaneous Component Analysis on TCATA data}
\usage{
tcatasca(formula, data, timecol, attributes, time.quantization, ...)
}
\description{
Apply Anova Simultaneous component analysis (ASCA) on temporal-Check-All-That-Apply(TCATA).

}

\arguments{

\item{data}{A data frame, or object coercible by as.data.frame to a data frame, containing the variables in the model. It must be in long format and must contain a column for Temporal Check All That Apply binary data, a column reporting the time values, and a column that defines the attributes analyzed.}

\item{formula}{An object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted. The formula will be applied for each atttribut at each time interval defined by the timecol column}

\item{timecol}{A string containing the name of the column indicating the time intervals of the TCATA dataset.}

\item{attributes}{A string contining the name of the column indicating the attributes of the TCATA.}

\item{time.quantization}{A single number reporting the number of time units contained in the new intervals used to quantize the time column.}

\item{...}{Optional parameters}

}

\details{
The resulting matrics of effects are directly affected by the formula defined in the function.
}

\value{
A list of PCA objects estimates for all the decomposing factors spcified by the formula object. The scores of the
}

\examples{
\dontrun{
data <- tempR::ojtcata

#The dataset is put in long format, specifying the column of time and the column
#of the attribute type

data.long <- gather(data, time, value, 5:25)

tcatasca( value~(samp+cons)^2 , data = data.long)

}

}



