\name{samplesizeTable.cgOneFactorFit}
\docType{methods}
\alias{samplesizeTable.cgOneFactorFit}
\alias{samplesizeTable,cgOneFactorFit-method}
\alias{cgOneFactorSampleSizeTable-class}
\alias{cgOneFactorSampleSizeTable}
\title{
  Estimate Sample Sizes based on a cgOneFactorFit object
}
\description{
  Estimate the sample size that would be required to detect a specified 
  difference in a one factor study.  The estimate is based on the
  variability that was observed in a previous one factor study.
  A cgOneFactorSampleSizeTable class object is created. 
}
\usage{
\S4method{samplesizeTable}{cgOneFactorFit}(fit, direction, mmdvec,
 power = 0.80, alpha = 0.05, nmax = 1000, display = "print", \dots)
}
\arguments{
  \item{fit }{
    A \code{\link{cgOneFactorFit}} object from a previous one factor study.    
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
    no difference when differences truly exist.
  }
  \item{alpha }{
    The significance level or alpha for the future study, set by default
    as \code{0.05}.
  }
  \item{nmax }{
    The maximum number of subjects per group.  If more subjects are estimated
    to be required, then the exact number required is not reported, only the
    fact that more than the maximum number would be required. This is in
    place to prevent long and likely unnecessary calculations.
  }
  \item{display }{
    One of three valid values:
    
    \describe{
      \item{\code{"print"}}{
	The default value; It calls a \code{print} method for the
	created \cr\code{cgOneFactorSamplesizeTable} object, which is
	a formatted text output of the table(s).
      }
      \item{\code{"none"}}{
	Supresses any printing. Useful, for example, when just assignment
	of the resulting object is desired.
      }
      \item{\code{"show"}}{
	Calls the default \code{\link{showDefault}} method, which
	will just print out the \code{cgOneFactorSamplesizeTable} components.
      }
    }
  }
  \item{\dots }{
    Additional arguments.

    \describe{
      \item{\code{ngrps} }{
	The number of groups that will be in the future one factor
	study. The default is \code{ngrps = 2}.
      }
      \item{\code{model}}{  
	A \code{character} value indicating which variability estimate
	in
	\code{fit} should be
	used to estimate the sample size: the robust model (\code{"rronly"}),
	the classical model (\code{"olsonly"}), or both (\code{"both"}).  If an estimate
	is requested for a model that was not fit, then no sample sizes are returned
	for that model but an error is not reported (e.g. if only the classical model
	was fitted but "both" are requested, only the classical model estimates
	will be returned):

	\describe{
	  \item{\code{"both"}}{Sample Size tables based on both the ordinary 
	    classical least squares and resistant & robust fits are
	    performed. This is the default when both fits are present in
	    the \code{cgOneFactorFit} object specified in the \code{fit}
	    argument. If the resistant & robust fit is not available,
	    this value is not relevant.
	  }
	  \item{\code{"olsonly"}}{Only a sample size table based on the ordinary 
	    classical least squares \code{olsfit} fit is calculated.
	  }
	  \item{\code{"rronly"}}{Only a sample size table based on the resistant and
	    robust \code{rrfit} fit is calculated.
	  }
	}
      }
    }
  }
}

\details{
  This sample size method does not work for fitted models that allowed unequal
  variances or censored observations.

  Sample sizes are estimated for detecting a minimum difference with a global
  F test. The algorithm is detailed in Fleiss (1986), Appendix A. When
  there are more than 2 groups, the lower bound of possible
  noncentrality parameter values is calculated from assuming only two of
  the \code{ngrps} number of groups differ by the \code{mmdvec/2} amount
  from the "grand mean" while the rest of the groups are equal to the grand
  mean.
  
  For detecting an absolute difference, the sample size is the
  smallest group size n for which\cr \code{1 - pf (qf (1 - alpha, numdf, dendf), numdf,
  dendf, ncp)} exceeds \code{power},\cr where \code{ncp = (n * mmdvec ^ 2) / (2 *
  sigamest ^ 2)}, and \code{sigmaest} is the residual mean square error from the model in
  \code{fit}.  For detecting a relative difference, the calculations are
  the same except\cr \code{ncp = (n * (log (sign * mmdvec / 100 + 1) ) ^ 2) / (2
    * sigmaest ^ 2)}, where\cr \code{sign = -1} if \code{direction="decreasing"},
  and \code{sign = 1} if \code{direction = "increasing"}.
}
\value{
  Creates an object of class \code{cgOneFactorSampleSizeTable}, with the
  following slots:
  
  \describe{
    \item{\code{ols.sstable}}{A matrix with the estimated sample sizes based on the
      classical model variance estimates, or \code{NULL}.   The matrix has 3 columns and
      one row for each element of the \code{mmdvec} \code{vector}.
      The first column specifies the minimum meaningful difference (\code{"mmd"}).
      The second column gives the number of subjects required for each 
      group (\code{"n"}), possibly truncated at \code{nmax}.
      The third column gives the total number of
      subjects required (\code{"N"}), also truncated if
      \code{nmax} is truncated.}
    \item{\code{rr.sstable}}{A matrix with the estimated sample sizes based on the
      robust model variance estimates, or else \code{NULL} if
      \code{model="olsonly"} was specified. See the \code{ols.sstable}
      slot description above for the analogous layout of the matrix.
    }
    \item{\code{settings}}{A list of properties mostly carried as-is from the
      \code{data} argument object of class
      \code{\link{cgOneFactorData}}, with the following additional members:
      \describe{
	\item{\code{sigmaest}}{A list with 2 members, \code{ols}, containing the
	  estimated spread (sigma, standard deviation) from
	  the classical model of \code{fit}, 
	  and \code{rr},
	  containing the estimated spread (sigma, standard deviation)
	  from the robust model of
	  \code{fit}, or
	  \code{NULL} if the robust model was not fit.}
	\item{\code{planningname}}{A \code{character} describing the study
	  or purpose of the sample size analysis.  Taken from the 
	  \code{settings$analysisname} of the \code{fit} object. }
	\item{\code{ngrps}}{A saved copy of the \code{ngrps} argument. }
	\item{\code{direction}}{A saved copy of the \code{direction} argument. }
	\item{\code{alpha}}{A saved copy of the \code{alpha} argument. }
	\item{\code{power}}{A saved copy of the \code{power} argument. }
	\item{\code{nmax}}{A saved copy of the \code{nmax} argument. }
      }
    }
  }
}
\author{
  Bill Pikounis [aut, cre, cph], John Oleynick [aut], Eva Ye [ctb]
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments. 
}
\references{
Fleiss, J.L. (1986). \emph{The Design and Analysis of Clinical
  Experiments}, Appendix A, pages 371 - 376. New York: Wiley.
}
\examples{
data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")
canine.fit <- fit(canine.data)

canine.samplesize <- samplesizeTable(canine.fit, direction="increasing", 
                                     mmdvec=c(10, 25, 50, 75, 100))

samplesizeTable(canine.fit, direction="decreasing", 
                mmdvec=c(25, 50, 75), model="olsonly")
}
\concept{sample size}