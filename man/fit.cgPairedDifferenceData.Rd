\name{fit.cgPairedDifferenceData}
\docType{methods}
\alias{fit.cgPairedDifferenceData}
\alias{fit,cgPairedDifferenceData-method}
\alias{cgPairedDifferenceFit-class}
\alias{cgPairedDifferenceFit}
\title{
  Fit models to a cgPairedDifferenceData object
}
\description{
  Fits a paired difference model based
  on the \code{cgPairedDifferenceData} object. The
  created object is designed for paired samples
  collected data, and is of class \code{\link{cgPairedDifferenceFit}}.
}
\usage{
\S4method{fit}{cgPairedDifferenceData}(data, type="rr", \dots)

}
\arguments{
  \item{data}{
    A data object of class \code{\link{cgPairedDifferenceData}}.  
  }
  \item{type }{Type of model to fit, represented by a character
    value. The default value is \code{"rr"}. The two current
    possibilities are:

    \describe{
      \item{\code{"ols"}}{Only fit an ordinary, classical least squares
	model with the base \code{\link{lm}} linear model function. 
      }
      \item{\code{"rr"}}{Fit a Resistant / Robust model based on M- and
	S-estimation, using the \code{\link{rlm}} function from the
	\pkg{MASS} package (Venables and Ripley, 2002).
	This is the default character value for the
	\code{type} argument. See the \code{\dots} argument and
	Details below for what options are available when
	\code{\link{rlm}} is used in this wrapper method. 
      }
    }
  }
  \item{\dots }{
    Additional arguments, both \emph{optional}, that are allowed to be specified dependent on
    the choice of the \code{type} argument. Otherwise they have no
    effect on the fit:
    \describe{
    \item{\code{maxIter}}{
      If \code{type="rr"} 
      then \code{maxIter} can be specified as a numeric postive
      integer. The default value of \code{maxIter} is \code{100}. 
      This gets passed to the \code{maxit} argument in the
      \code{\link{rlm}} method. 
    }
  }
  }
}
\details{
  In the current version of the \pkg{cg} package, most default settings
  for \code{\link{rlm}} are kept for the
  \code{fit.cgPairedDifferenceData} method wrapper call  when
  \code{type="rr"}, with no capability to choose
  another value for an arguments such as \code{psi}, \code{scale.est},
  and \code{k2}. The \code{method} argument is set to \code{"MM"}.
}
\value{
  Creates an object of class  \code{cgPairedDifferenceFit}, with the
  following slots:
  \describe{
    \item{\code{olsfit}}{The contents of a \code{\link{lm}} fit to the
      data. This is always populated with an \code{\link{lm}} object
      no matter the choice of the \code{type} argument, such as \code{code="rr"}.
    }
    \item{\code{rrfit}}{The contents of a \code{\link{rlm}} fit to the
      data, housed as a \code{rrfit} class object.
      If \code{type="rr"} is not selected, then this is set
      to a simple character value of \code{"No fit was selected."}.
    }
    \item{\code{settings}}{A list of properties carried as-is from the
      \code{data} argument object of class
      \code{\link{cgPairedDifferenceData}}.
        In particular,
	if \code{zeroscore} is specified as a non-\code{NULL} number in
	the \code{\link{cgPairedDifferenceData}} object in the \code{data}
	argument, then a score value near zero was derived to replace
	all zeroes for subsequent
	log-scale analyses. Alternatively, if \code{addconstant} is specified 
	as a non-\code{NULL} number in the
	\code{\link{cgPairedDifferenceData}} object in the \code{data}
	argument, then a value was added to shift up all observations for subsequent
	log-scale analyses. 
      }
    }
}
\references{
  Venables, W. N. and Ripley, B. D. (2002), 
  \emph{Modern Applied Statistics with S.}  Fourth edition. Springer.
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
anorexiaFT.fit <- fit(data=anorexiaFT.data, type="rr")					  

}
\keyword{models}
\concept{resistant}
\concept{robust}
\concept{least squares}
