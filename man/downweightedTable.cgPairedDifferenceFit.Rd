\name{downweightedTable.cgPairedDifferenceFit}
\docType{methods}
\alias{downweightedTable.cgPairedDifferenceFit}
\alias{downweightedTable.cgPairedDifferenceFit}
\alias{downweightedTable,cgPairedDifferenceFit-method}
\alias{cgPairedDifferenceDownweightedTable-class}
\alias{cgPairedDifferenceDownweightedTable}
\title{
  Create a table of downweighted observations from a Resistant & Robust fit.
}
\description{
  Create a table of downweighted observations based on a rrfit object
  within a cgPairedDifferenceFit object. A cgPairedDifferenceDownweightedTable
  class object is created.  
}
\usage{
\S4method{downweightedTable}{cgPairedDifferenceFit}(fit, cutoffwt, display = "print",\dots)
}

\arguments{
  \item{fit}{
   A fit object of class \code{\link{cgPairedDifferenceFit}}.
  }
  \item{cutoffwt}{
    It has no default and must be specified as a numeric between 0 and 1
    exclusive. It is a threshold. All
    observations that fall \emph{beneath} the threshold will be
    identified. For example, a \code{cutoffwt=0.90} will yield
    those observations that were downweighted by at least 10\%. 
 }
  \item{display}{One of three valid values:
	
    \describe{
      \item{\code{"print"}}{
	The default value; It calls a \code{print} method for the
	created \cr\code{cgPairedDifferenceDownweightedTable} object, which is
	a formatted text output of the table(s).
      }
      \item{\code{"none"}}{
	Supresses any printing. Useful, for example, when just assignment
	of the resulting object is desired.
      }
      \item{\code{"show"}}{
	Calls the default \code{\link{showDefault}} method, which
	will just print out the \code{cgPairedDifferenceDownweightedTable} components.
      }
    }
  }
  \item{\dots }{
    Additional arguments. None are currently defined for this method.
  }
}
\details{
  If no observations meet the cutoff criteria, a text message of the 
  \code{cgPairedDifferenceDownweightedTable} content emptiness is output
  instead.

  The reported weights are in the scale of the observation, not the
  sum of squared errors representation for the likelihood. Thus they are
  derived from the square root of the \code{$w} component from
  a \pkg{MASS}::\code{rlm} fit object.
}
\value{
  An object of class \code{cgPairedDifferenceDownweightedTable}, with the
  following slots:

  \describe{
    \item{\code{contents}}{A data frame where each row is an observation
      from the fitted data set that meets the cutoff criteria, and these columns:
      \describe{
	\item{\code{expunit}}{The experimental unit name identified from the fitted data.}
	\item{\code{grp1}}{The observed response value from group 1.}
	\item{\code{grp2}}{The observed response value from group 2.}
	\item{\code{weight}}{The weight associated to the observation from
	  the resistant / robust fit.}
	\item{\code{pct down-weighted}}{An expression of the weight in
	  terms of percent reduction from the maximum of 1.}
	\item{\code{Simple Diff}}{The difference of observed response value between 
          the two groups.}
	\item{\code{Ratio Diff}}{The percent difference of observed response value 
          between the two groups. NOTE this only occurs when
	  \code{logscale=TRUE} from the
	\code{\link{cgPairedDifferenceFit}} object \code{settings} in the \code{fit} argument.}
	\item{\code{Pct Diff}}{The percent difference of observed response value 
          between the two groups. NOTE this only occurs when
	  \code{logscale=TRUE} from the
	\code{\link{cgPairedDifferenceFit}} object \code{settings} in the \code{fit} argument.}
      }
	  
      If no observations meet the cutoff criteria,
      the \code{contents} slot is set to \code{NULL}. 
	  
    }
    \item{\code{messages}}{A message when the \code{contents} slot is set to \code{NULL}.
    }
    \item{\code{settings}}{A list of settings carried from the
      \code{\link{cgPairedDifferenceFit}} object. These are used for the 
        \cr\code{\link{print.cgPairedDifferenceDownweightedTable}} method,
      invoked for example when \code{display="print"}.
    }
  }
}
\seealso{
  \code{\link{cgPairedDifferenceFit}}, \pkg{MASS}::\code{\link{rlm}} 
}
\references{
  Venables, W. N. and Ripley, B. D. (2002), 
  \emph{Modern Applied Statistics with S.}  Fourth edition. Springer.
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
                                                 digits=1,
                                                 logscale=TRUE)
anorexiaFT.fit <- fit(anorexiaFT.data)

downweightedTable(anorexiaFT.fit, cutoffwt=0.25) ## No observation

downweightedTable(anorexiaFT.fit, cutoffwt=0.75) ## downweighted at least 25\%
                                           
}
