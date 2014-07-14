\name{descriptiveTable.cgPairedDifferenceData}
\docType{methods}
\alias{descriptiveTable.cgPairedDifferenceData}
\alias{descriptiveTable,cgPairedDifferenceData-method}
\alias{cgPairedDifferenceDescriptiveTable-class}
\alias{cgPairedDifferenceDescriptiveTable}
\title{
  Compute Descriptive Summary Statistics of Groups in a cgPairedDifferenceData object
}
\description{
  Create a table of quantiles and other summary statistics of the data in a 
  \code{\link{cgPairedDifferenceData}} object.
}
\usage{
\S4method{descriptiveTable}{cgPairedDifferenceData}(data, display = "print", \dots)
}
\arguments{
  \item{data}{
    A \code{cgPairedDifferenceData} object, typically created by 
    \code{\link{prepareCGPairedDifferenceData}}.
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
	will just print out the cgPairedDifferenceData object components.
      }
    }
  }
  \item{\dots }{
    Additional arguments. Currently only one is valid: 
    \describe{
      \item{\code{logscale}}{
	A \code{logical} value, indicating whether or not the geometric
	means, their
	standard errors, and ratio differences should be included in the summary.
	If \code{logscale} is not
	specified, its value is taken from the \code{cgPairedDifferenceData} object,
	which \code{\link{prepareCGPairedDifferenceData}} sets from its \code{logscale} argument.
      }
    }
  }
}
\details{
  The returned table contains quantiles, means, sample sizes, and
  estimates of variability for each group, and also for the paired
  differences.
  It also presents the same
  summary measures for the paired differences from the groups.
  If the \code{logscale} option is specified (either explicitly, or implicitly
  from the \code{cgPairedDifferenceData} object), then the geometric mean and
  geometric standard error for each of the two groups are included. Also
  included are summary measures of the ratio and percent forms of the paired
  differences.  See the Value section  below for details.
}
\value{
  Creates an object of class  \code{cgPairedDifferenceDescriptiveTable}, with the
  following slots:
  \describe{
    \item{\code{contents}}{The table of descriptive summary statistics
      for each group, and also for paired differences. See below for the data frame structure
      of the table.
    }
    \item{\code{settings}}{A list of settings carried from the
      \code{\link{cgPairedDifferenceData}} \code{data} object. These are used
      for the \code{\link{print.cgPairedDifferenceDescriptiveTable}} method,
      invoked for example when \code{display="print"}.
    }
  }

  The data frame structure of the descriptive table in a \code{contents}
  slot consists of \code{row.names} that specify the group or paired
  difference, and these columns:
  
  \describe{
    \item{\code{n}}{The sample size.
    }
    \item{\code{Min}}{The minimum value.
    }
    \item{\code{25\%ile}}{The 25th percentile, estimated
      with the \code{\link{quantile}} function.
    }
    \item{\code{Median}}{The median value.
    }
    \item{\code{75\%ile}}{The 75th percentile, estimated
      with the \code{\link{quantile}} function.
    }
    \item{\code{Max}}{The maximum value.
    }
    \item{\code{Mean}}{The arithmetic mean value.
    }
    \item{\code{StdDev}}{The standard deviation value.
    }
    \item{\code{StdErr}}{The standard error value.
    }
  }

  If \code{logscale=TRUE}, then two additional columns are added:
  \describe{
    \item{\code{GeoMean}}{The geometric mean value of the group.
    }
    \item{\code{SEGeoMean}}{The estimated standard error associated
      with the geometric mean. This is calculated with the Delta Method,
      and will particularly lose accuracy in its useful approximation once the standard
      error in the log scale exceeds 0.50. A warning message is
      issued when this occurs.
    }
  }
  The third row of simple difference summaries
  has \code{GeoMean} and \code{SEGeoMean} are set to <NA>.
  
  Fourth and fifth rows are also added with summaries of the paired
  ratio differences and percent differences. The \code{StdDev} and 
  \code{StdErr} values are set to <NA> in these two rows.
  The \code{GeoMean} and \code{SEGeoMean} values are calculated via the
  the Delta Method, with the same caveats described above.
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

descriptiveTable(anorexiaFT.data)

## Remove the geometric mean and standard error columns,
## and the Ratio / Percent Rows, since they are no longer applicable.

descriptiveTable(anorexiaFT.data, logscale=FALSE)


}
\concept{exploratory}
\concept{descriptive}
