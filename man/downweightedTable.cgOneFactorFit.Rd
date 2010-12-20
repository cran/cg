\name{downweightedTable.cgOneFactorFit}
\docType{methods}
\alias{downweightedTable.cgOneFactorFit}
\alias{downweightedTable,cgOneFactorFit-method}
\alias{cgOneFactorDownweightedTable-class}
\alias{cgOneFactorDownweightedTable}
\title{
  Create a table of downweighted observations from a Resistant & Robust fit.
}
\description{
  Create a table of downweighted observations based on a rrfit object
  within a cgOneFactorFit object. A cgOneFactorDownweightedTable
  class object is created.  
}
\usage{
\S4method{downweightedTable}{cgOneFactorFit}(fit, cutoffwt, display="print", \dots)
}

\arguments{
  \item{fit }{
   A fit object of class \code{\link{cgOneFactorFit}}.  
  }
  \item{cutoffwt }{
    It has no default and must be specified as a numeric between 0 and 1
    exclusive. It is a threshold on which observations to identify. All
    observations that exceed this specified value will be
    identified. For example, a \code{cutoffwt=0.90} will yield
    those observations that were downweighted by at least 10\%. 
  }
  \item{display}{One of three valid values:
	
    \describe{
      \item{\code{"print"}}{
	The default value, it calls a \code{print} method for the
	created \cr\code{cgOneFactorDownweightedTable} object, which is
	a formatted text output of the table(s).
      }
      \item{\code{"none"}}{
	Supresses any printing. Useful, for example, when just assignment
	of the resulting object is desired.
      }
      \item{\code{"show"}}{
	Calls the default \code{\link{showDefault}} method, which
	will just print out the \code{cgOneFactorDownweightedTable} components.
      }
    }
  }
  \item{\dots }{
    Additional arguments. None are currently defined for this method.
  }
}
\details{
  If no observations meet the cutoff criteria, a text message of the 
  \code{cgOneFactorDownweightedTable} content emptiness is output
  instead.

  The reported weights are in the scale of the observation, not the
  sum of squared errors representation for the likelihood. Thus they are
  derived from the square root of the \code{$w} component from
  a MASS::\code{rlm} fit object.
}
\value{
  An object of class \code{cgOneFactorDownweightedTable}, with the
  following slots:

  \describe{
    \item{\code{contents}}{A data frame where each row is an observation
      from the fitted data set that meets the cutoff criteria, and these columns:
      \describe{
	\item{\code{group}}{The group identified from the fitted data.}
	\item{\code{endpoint}}{The observed response value.}
	\item{\code{weight}}{The weight associated to the observation from
	  the resistant / robust fit.}
	\item{\code{pct down-weighted}}{An expression of the weight in
	  terms of percent reduction from the maximum of 1.}
      }
	  
      If no observations meet the cutoff criteria,
      the \code{contents} slot is set to \code{NULL}. 
	  
    }
    \item{\code{cutoffwt}}{Taken from the specified \code{cutoffwt} argument
      value.}
    \item{\code{settings}}{A list of settings carried from the
      \code{\link{cgOneFactorFit}} object, and the addition
      of the specified \code{cutoffwt} argument in the method call above. These are used
      for the \cr\code{\link{print.cgOneFactorDownweightedTable}} method,
      invoked for example when
      \code{display="print"}.
    }
  }

}
\seealso{
  \code{\link{cgOneFactorFit}}, MASS::\code{\link{rlm}} 
}
\references{
  Venables, W. N. and Ripley, B. D. (2002), 
  \emph{Modern Applied Statistics with S.}  Fourth edition. Springer.
}
\author{
  Bill Pikounis and John Oleynick
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
canine.fit <- fit(canine.data)

canine.dwtable <- downweightedTable(canine.fit, cutoff=0.95)

downweightedTable(canine.fit, cutoff=0.75) ## No observation
                                           ## downweighted at least 25\%

}



