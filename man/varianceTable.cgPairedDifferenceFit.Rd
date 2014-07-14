\name{varianceTable.cgPairedDifferenceFit}
\docType{methods}
\alias{varianceTable.cgPairedDifferenceFit}
\alias{varianceTable,cgPairedDifferenceFit-method}
\alias{cgPairedDifferenceVarianceTable-class}
\alias{cgPairedDifferenceVarianceTable}
\title{
  Compute Variances from a cgPairedDifferenceFit object
}
\description{
  Create a table of variance component estimates of the data in a 
  \code{\link{cgPairedDifferenceFit}} object.
}
\usage{
\S4method{varianceTable}{cgPairedDifferenceFit}(fit, display = "print", \dots)
}
\arguments{
  \item{fit}{
    A \code{cgPairedDifferenceFit} object, typically created by 
    \code{\link{fit.cgPairedDifferenceData}}.
  }
\item{display }{One of three valid values:
    
    \describe{
      \item{\code{"print"}}{
	The default value; It calls a \code{print} method for the
	created\cr \code{cgPairedDifferenceVarianceTable} object, which is
	a formatted text output of the table.
      }
      \item{\code{"none"}}{
	Supresses any printing. Useful, for example, when just assignment
	of the resulting object is desired.
      }
      \item{\code{"show"}}{
	Calls the default \code{\link{showDefault}} method, which
	will just print out the cgPairedDifferenceVatianceTable object components.
      }
    }
  }
  \item{\dots }{
    Additional arguments. Currently none are valid. 
  }
}
\details{
  The returned table contains variance component estimates for the
  classical least squares fit. There is no analogous decomposition of
  variance component estimates calculated for the resistant & robust
  fit. 
}
\value{
  Creates an object of class  \code{cgPairedDifferenceVarianceTable}, with the
  following slots:
  \describe{
    \item{\code{contents}}{The table of variance component
      estimates. There are two, the "within experimental unit" variance
      and the "between experimental unit" variance.  See below for the
      data frame structure of the table. The label portion "experimental
      unit" will be replaced by the \code{expunitname} component of the
      \code{settings} slot of the \code{\link{cgPairedDifferenceFit}}
      \code{fit} object, if previously specified.
    }
    \item{\code{efficiency}}{A table of efficiency
      estimates, derived from the variance component estimates. The
      goal is to quantify the reduced number of experimental units
      needed since a paired difference design was employed, instead of a
      an unpaired design. See below for the data frame structure of the table.
    }
    \item{\code{settings}}{A list of settings carried from the
      \code{\link{cgPairedDifferenceFit}} \code{fit} object. These are used
      for the \code{\link{print.cgPairedDifferenceVarianceTable}} method,
      invoked for example when \code{display="print"}.
    }
  }

  The data frame structure of the variance components table from the
  classical least squares fit is provided in the \code{contents}
  slot. The data frame consists of \code{row.names} based on the
  \code{expunitname} component of the \code{settings} slot in the
  \code{\link{cgPairedDifferenceFit}} \code{fit} object. The first row
  is for the "within" component, and the second is for the "between"
  component. The "total" variance is in the third row of the table, the
  sum of the between and within variance components. The
  first column of the table is the variance components estimates, and
  the third column is the square root of the variance components,
  labeled \code{Spread(StdDev)}. In the second column is the \code{Percent}
  calculation of the two variance components relative to the total sum variance. 

  The data frame structure of the efficiency table
  from the classical least squares fit is
  provided in the \code{efficiency}
  slot. There are four rows and one column. All values are derived from
  the variance components estimates in the \code{contents}
  slot described above. The first row of \code{Relative Efficiency}
  comes from dividing the total variance by the between experimental
  unit variance component. The second row expresses the estimated gain in
  sensitivity by using a paired difference design and analysis over
  using a unpaired design and analysis. This is equal to the within
  experimental unit variance component divided by the total variance,
  and is expressed here as \code{Percent Reduction}. The third row is
  the number of experimental units based on the input data set paired
  structure. The last row contains the estimated number of unpaired
  design experimental units that would have been needed for the same
  sensitivity. The label portion "experimental unit" in these last
  two row names will be replaced by the \code{expunitname} component of the
  \code{settings} slot of the \code{\link{cgPairedDifferenceFit}}
  \code{fit} object if previously specified.
}
\author{
  Bill Pikounis [aut, cre, cph], John Oleynick [aut], Eva Ye [ctb]
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments. 
}

\examples{
data(anorexiaFT)
anorexiaFT.data <- prepareCGPairedDifferenceData(anorexiaFT, format="groupcolumns",
                                                 analysisname="Anorexia FT",
                                                 endptname="Weight",
                                                 endptunits="lbs",
                                                 expunitname="Patient",
                                                 digits=1, logscale=TRUE)
anorexiaFT.fit <- fit(anorexiaFT.data)

varianceTable(anorexiaFT.fit)

}
\concept{variance components}
