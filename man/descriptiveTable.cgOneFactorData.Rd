\name{descriptiveTable.cgOneFactorData}
\docType{methods}
\alias{descriptiveTable.cgOneFactorData}
\alias{descriptiveTable,cgOneFactorData-method}
\alias{cgOneFactorDescriptiveTable-class}
\alias{cgOneFactorDescriptiveTable}
\title{
  Compute Descriptive Summary Statistics of Groups in a cgOneFactorData object
}
\description{
  Create a table of quantiles and other summary statistics of the data in a 
  \code{\link{cgOneFactorData}} object.
}
\usage{
\S4method{descriptiveTable}{cgOneFactorData}(data, display = "print", \dots)
}
\arguments{
  \item{data}{
    A \code{cgOneFactorData} object, typically created by 
    \code{\link{prepareCGOneFactorData}}.
  }
\item{display }{One of three valid values:
    
    \describe{
      \item{\code{"print"}}{
	The default value; It calls a \code{print} method for the
	created \code{descriptiveTable} object, which is
	a formatted text output of the table.
      }
      \item{\code{"none"}}{
	Supresses any printing. Useful, for example, when just assignment
	of the resulting object is desired.
      }
      \item{\code{"show"}}{
	Calls the default \code{\link{showDefault}} method, which
	will just print out the cgOneFactorDescriptiveTable object components.
      }
    }
  }
  \item{\dots }{
    Additional arguments. Currently only one is valid: 
    \describe{
      \item{\code{logscale}}{
	A \code{logical} value, indicating whether or not the geometric means and
	standard errors should be included in the summary.
	If \code{logscale} is  not
	specified, its value is taken from the \code{cgOneFactorData} object,
	which \code{\link{prepareCGOneFactorData}} sets from its \code{logscale} argument.
      }
    }
  }
}
\details{
  The returned table contains quantiles, means, sample sizes, and
  estimates of variability for each group. If censored data are present,
  the estimated quantiles accomodate that with the Kaplan-Meier
  method, following Gentleman and Crowley (1991) .
  The number of censored / incomplete and number of complete observations
  are also included when censored data is present in any of the groups.
  If the \code{logscale} option is specified (either explicitly, or implicitly
  from the \code{cgOneFactorData} object), then the geometric mean and
  geometric standard error for each group are also included. See the Value section
  below for details.
}
\value{
  Creates an object of class  \code{cgOneFactorDescriptiveTable}, with the
  following slots:
  \describe{
    \item{\code{contents}}{The table of descriptive summary statistics
      for each group. See below for the data frame structure
      of the table.
    }
    \item{\code{settings}}{A list of settings carried from the
      \code{\link{cgOneFactorData}} \code{data} object. These are used
      for the \code{\link{print.cgOneFactorDescriptiveTable}} method,
      invoked for example when \code{display="print"}.
    }
  }

  The data frame structure of the descriptive table in a \code{contents}
  slot consists of \code{row.names} that specify the group, and these columns:
  
  \describe{
    \item{\code{n}}{The sample size of the group.
    }
    \item{\code{Min}}{The minimum value of the group.
    }
    \item{\code{25\%ile}}{The 25th percentile of the group, estimated
      with the \code{\link{quantile}} function.
    }
    \item{\code{Median}}{The median value of the group.
    }
    \item{\code{75\%ile}}{The 75th percentile of the group, estimated
      with the \code{\link{quantile}} function.
    }
    \item{\code{Max}}{The maximum value of the group.
    }
    \item{\code{Mean}}{The arithmetic mean value of the group.
    }
    \item{\code{StdDev}}{The standard deviation value of the group.
    }
    \item{\code{StdErr}}{The standard error value of the group.
    }
  }

  If \code{logscale=TRUE}, then two additional columns are added:
  \describe{
    \item{\code{GeoMean}}{The geometric mean value of the group.
    }
    \item{\code{SEGeoMean}}{The estimated standard error associated
      withthe geometric mean. This is calculated with the Delta Method,
      and will particularly lose accuracy in its useful approximation once the standard
      error in the log scale exceeds 0.50. A warning message is
      issued when this occurs.
    }
  }

  If censored data are present in the \code{cgOneFactorData} object,
  then two more columns are added:
  \describe{
    \item{\code{ncensored}}{The number of censored / incomplete observations.
    }
    \item{\code{ncomplete}}{The number of complete observations.
    }
  }
  These two \code{ncensored} and \code{ncomplete} quantities will add up
  to \code{n} above and be placed
  adjacent to it.

  The presence of censored observations will convert columns such as the
  \code{Min} and \code{Max} to character values, with the appropriate ">"
  and "<" symbols for right-censoring and left-censoring, respectively.

  For censored data, Kaplan-Meier estimates are used for the quantiles, as 
  proposed by Gentleman and Crowley (1991).  The \pkg{survreg}::\code{\link{survfit}}
  conventions are followed for interpolation of these quantiles.
  With enough censored data
  observations in a group, certain quantiles may not be estimable. If
  any censored observations are present, the mean, geometric mean,
  and associated standard errors will not be
  calculated. The <NA> character representation is used.
}
\references{
Gentleman, R.C. and Crowley, J. (1991).  "Graphical Methods for Censored
Data", \emph{Journal of the American Statistical Association}, Volume
86, 678-683.
}

\author{
  Bill Pikounis, John Oleynick, Eva Ye
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments. 
}

\examples{
data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")

descriptiveTable(canine.data)

## Remove the geometric mean and standard error columns
descriptiveTable(canine.data, logscale=FALSE)

## Censored Data
data(gmcsfcens)
gmcsfcens.data <- prepareCGOneFactorData(gmcsfcens, format="groupcolumns",
                                         analysisname="cytokine",
                                         endptname="GM-CSF (pg/ml)",
                                         logscale=TRUE)
descriptiveTable(gmcsfcens.data)
}
\concept{exploratory}
\concept{descriptive}
