\name{correlationTable.cgPairedDifferenceData}
\docType{methods}
\alias{correlationTable.cgPairedDifferenceData}
\alias{correlationTable,cgPairedDifferenceData-method}
\alias{cgPairedDifferenceCorrelationTable-class}
\alias{cgPairedDifferenceCorrelationTable}
\title{
  Compute Correlations from a cgPairedDifferenceData object
}
\description{
  Create a table of correlations of the data in a 
  \code{\link{cgPairedDifferenceData}} object.
}
\usage{
\S4method{correlationTable}{cgPairedDifferenceData}(data, display = "print", \dots)
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
	created \code{correlationTable} object, which is
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
	A \code{logical} value, indicating whether the correlation of
	the log transformed data should be calculated.
	If \code{logscale} is not
	specified, its value is taken from the \code{cgPairedDifferenceData} object,
	which \code{\link{prepareCGPairedDifferenceData}} sets from its \code{logscale} argument.
      }
    }
  }
}
\details{
  The returned table contains correlations between the paired samples. The
  Pearson and Spearman methods are applied with the
  \code{\link{cor.test}} function from the core \pkg{stats} package.  
  If the \code{logscale} option is specified (either explicitly, or implicitly
  from the \code{cgPairedDifferenceData} object), then the Pearson
  calculation on the log transformed data is added.
}
\value{
  Creates an object of class  \code{cgPairedDifferenceCorrelationTable}, with the
  following slots:
  \describe{
    \item{\code{contents}}{The table of correlations for the
      paired differences. See below for the
      data frame structure
      of the table.
    }
    \item{\code{settings}}{A list of settings carried from the
      \code{\link{cgPairedDifferenceData}} \code{data} object. These are used
      for the \code{\link{print.cgPairedDifferenceCorrelationTable}} method,
      invoked for example when \code{display="print"}.
    }
  }

  The data frame structure of the correlation table in a \code{contents}
  slot consists of \code{row.names} that specify the correlation method:
  \code{Pearson}, and \code{Spearman} if original
  (i.e. \code{logscale=FALSE}),
  and \code{Pearson Original}, \code{Pearson Log}, and \code{Spearman}
  if \code{logscale=TRUE}. The header label for the column of calculated
  correlations is \code{correlation}.
}
\author{
  Bill Pikounis, John Oleynick, Eva Ye
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

correlationTable(anorexiaFT.data)

## Show only correlations computed on original scale
correlationTable(anorexiaFT.data, logscale=FALSE)


}
\concept{exploratory}
\concept{correlation}
