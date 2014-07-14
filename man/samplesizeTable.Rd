\name{samplesizeTable}
\docType{methods}
\alias{samplesizeTable}
\title{
  Estimate Required Sample Sizes
}
\description{
  Estimate the sample size  required to detect a specified 
  difference in a future study.  The estimate is based on the
  variability in a data fit with the \pkg{cg} package. 
}
\usage{
samplesizeTable(fit, direction, mmdvec, power = 0.80,
 alpha = 0.05, nmax = 1000, display = "print", \dots)
}
\arguments{
  \item{fit }{
    An object created by calling a 
    \code{\link{fit}} method from the \pkg{cg} package.
    \cr There is currently one class of objects that are currently available:
    \cr\code{\link{cgOneFactorFit}}, which is prepared by the 
    \cr\code{\link{fit.cgOneFactorData}} method. 
  }  
  \item{direction }{
    A \code{character} indicating whether the sample size should be estimated
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
    to be required, than the exact number required is not reported, only the
    fact that more than the maximum number would be required. This is in
    place to prevent long and likely unnecessary calculations.
  }
  \item{display }{
One of three valid values:
    
    \describe{
      \item{\code{"print"}}{
	The default value; It calls a \code{print} method for the
	created \code{samplesizeTable} object, which is
	a formatted text output of the table(s).
      }
      \item{\code{"none"}}{
	Supresses any printing. Useful, for example, when just assignment
	of the resulting object is desired.
      }
      \item{\code{"show"}}{
	Calls the default \code{\link{showDefault}} method, which
	will just print out the \code{samplesizeTable} components.
      }
    }
  }
  \item{\dots }{
    Additional arguments, depending on the specific method written for
    the object.  See the method-specific documentation for additional
    details.
  }
}
%% \details{
%% }
\value{
  A method-specific \code{SampleSizeTable} object is returned.
  See the specific methods for discussion of return values. 
}

\author{
  Bill Pikounis [aut, cre, cph], John Oleynick [aut], Eva Ye [ctb]
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments. 
}
\seealso{
  \code{\link{samplesizeTable.cgOneFactorFit}}
}
\examples{
#### One Factor data

data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")
canine.fit <- fit(canine.data)

canine.samplesize <- samplesizeTable(canine.fit, direction="increasing", 
                                     mmdvec=c(10, 25, 50, 75, 100))

samplesizeGraph(canine.samplesize)


}
\concept{sample size}
