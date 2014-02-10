\name{samplesizeTable.cgPairedDifferenceFit}
\alias{samplesizeTable.cgPairedDifferenceFit}
\docType{methods}
\alias{samplesizeTable.cgPairedDifferenceFit}
\alias{samplesizeTable,cgPairedDifferenceFit-method}
\alias{cgPairedDifferenceSampleSizeTable-class}
\alias{cgPairedDifferenceSampleSizeTable}
\title{
  Estimate Sample Sizes based on a cgPairedDifferenceFit object
}
\description{
  Estimate the sample size that would be required to detect a specified 
  difference in a paired difference data study.  The estimate is based on the
  variability that was observed in a previous paired difference data study.
  A cgPairedDifferenceSampleSizeTable class object is created. }
\usage{
\S4method{samplesizeTable}{cgPairedDifferenceFit}(fit, direction, mmdvec,
 power = 0.80, alpha = 0.05, nmax = 1000, display = "print", \dots)
}
\arguments{
  \item{fit }{
    A \code{\link{cgPairedDifferenceFit}} object from a previous paired difference data study.    
  }
  \item{direction }{
    A \code{character} value indicating whether the sample size should be estimated
    to detect an \code{"increase"} or a \code{"decrease"}.  This only effects
    the sample size estimates if the previous study in \code{fit} was 
    analyzed on the log scale, in which case the differences in \code{mmdvec}
    are relative differences instead of absolute differences.
    For detecting relative changes, the sample size
    required to detect a relative increase of 25\% is not the same as the sample
    size to detect a relative decrease of 25\%, for example.  But for detecting
    absolute changes, the sample size required
    to detect an absolute increase of 25 is the same as the sample size
    to detect an absolute decrease of 25.
  }
  \item{mmdvec }{
    A \code{numeric} \code{vector} specifying the minimum meaningful differences
    to be detected in the future study.  If the previous study
    in \code{fit} was analyzed on the log scale, then the values in \code{mmdvec}
    are assumed to be relative percentage increases or decreases, depending
    on the value of \code{direction}.  If the previous study in
    \code{fit} was not analyzed on the log scale, then the values in \code{mmdvec}
    are assumed to be absolute increases or decreases, depending on 
    the value of \code{direction}. Each value in \code{mmdvec} needs to
    be positive.
  }
  \item{power }{
    The power for the future study, set by default to be \code{0.80}.
    This is equivalent
    to \eqn{1 - \beta}, where \eqn{\beta} is the probability of
    committing a Type II error: accepting the null hypothesis of
    no difference when a difference truly exists.
  }
  \item{alpha }{
    The significance level or alpha for the future study, set by default
    as \code{0.05}.
  }
  \item{nmax }{
    The maximum number of subjects per group.  If more subjects are estimated
    to be required, than the exact number required is not reported, only the
    fact that more than the maximum number would be required. This is in
    place to prevent long and likely unnecessary calculations.
  }
  \item{display }{
    One of three valid values:
    
    \describe{
      \item{\code{"print"}}{
	The default value; It calls a \code{print} method for the
	created \cr\code{cgPairedDifferenceSamplesizeTable} object, which is
	a formatted text output of the table(s).
      }
      \item{\code{"none"}}{
	Supresses any printing. Useful, for example, when just assignment
	of the resulting object is desired.
      }
      \item{\code{"show"}}{
	Calls the default \code{\link{showDefault}} method, which
	will just print out the \code{cgPairedDifferenceSamplesizeTable} components.
      }
    }
  }
  \item{\dots }{
    Additional arguments. Only one is currently valid:
  
    \describe{
      \item{\code{correction} }{
	Other than the \code{NULL} value, only the \code{"df"} value can
	be specified. This \code{"df"} values provides a
	degrees of freedom correction for using 
	variance estimates based on different degrees of freedom. See
	details below.
      }
    }
  }
}

\details{
  Here, the estimated sample size actually refers to the number of
  experimental units. Hence the number of observations will always be
  twice the number of experimental units, due to the paired structure. 
    
  This sample size method only works for the classical least squares
  fitted model, since there is no analogous decomposition of total
  variance into  between-experimental unit and within-experimental unit variance
  components. Sample sizes are estimated for detecting a minimum
  difference with the classical least squares t-test / F-test.

  The \code{correction = "df"} argument specifies a method that Fleiss
  (1986, pages 129-130) attributes to Cochran and Cox (1957) and Fisher.
  The correction decreases the relative efficiency that is calculated
  from accounting for correlated paired observations, relative to the unpaired two group
  design. The adjustment accounts for the different degrees of freedom
  used for the variance components in the paired design
  (between-experimental unit, within-experimettal unit, total variability.)

  Since the correction reduces the relative efficiency, and the
  noncentrality parameter is also reduced. The correction
  is a multiplicative factor bounded below
  by 0.833 and approaches 1 as the number of experimental units
  increments from the minimum of \code{n=2}. The reduction in the
  noncentrality parameter increases the computed sample size.
}
\value{  
  Creates an object of class \code{cgPairedDifferenceSampleSizeTable}, with the
  following slots:
  
  \describe{
    \item{\code{ols.sstable}}{A matrix with the estimated experimental
      unit sample sizes based on the
      classical model variance estimates. The matrix has 3 columns and
      one row for each element of the \code{mmdvec} \code{vector}.
      The first column specifies the minimum meaningful difference (\code{"mmd"}).
      The second column gives the number of experimental units
      (\code{"n"}) required,
      possibly truncated at \code{nmax}.
      The third column gives the total number of
      observations (\code{"N"}), also possibly truncated at
      \code{nmax}. Since this for the paired groups design, \code{N = n
	* 2} will always hold.}
    \item{\code{settings}}{A list of properties mostly carried as-is from the
      \code{data} argument object of class
      \code{\link{cgPairedDifferenceData}}, with the following additional members:
      \describe{
	\item{\code{sigmaest}}{A list with 1 member, \code{ols}, containing the
	  estimated spread (sigma, standard deviation) variance
	  estimates from
	  the classical model of \code{fit}. This list component is a
	  vector of length 3, providing the within-experimental unit,
	  between experimental unit, and total variability estimates. }
	\item{\code{planningname}}{A \code{character} describing the study
	  or purpose of the sample size analysis.  Taken from the 
	  \code{settings$analysisname} of the \code{fit} object. }
	\item{\code{direction}}{A saved copy of the \code{direction} argument. }
	\item{\code{alpha}}{A saved copy of the \code{alpha} argument. }
	\item{\code{power}}{A saved copy of the \code{power} argument. }
	\item{\code{nmax}}{A saved copy of the \code{nmax} argument. }
      }
    }
  }
}
\author{
  Bill Pikounis, John Oleynick, and Eva Ye
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments. 
}
\references{
Fleiss, J. L. (1986). \emph{The Design and Analysis of Clinical
  Experiments},  pages 129 - 130. New York: Wiley.

Cochran, W. G. and Cox, G. M. (1957), \emph{Experimental Designs.} Second
edition. Wiley. 

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

## Recall the interest is in increased weight for the anorexia FT
## (family treatment) group of patients
samplesizeTable(anorexiaFT.fit, direction="increasing",
                mmdvec=c(5, 10, 15, 20))

## and with the adjustment on the noncentrality parameter
samplesizeTable(anorexiaFT.fit, direction="increasing",
                mmdvec=c(5, 10, 15, 20), correction="df")

}
\concept{sample size}

