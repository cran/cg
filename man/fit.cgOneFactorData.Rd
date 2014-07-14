\name{fit.cgOneFactorData}
\docType{methods}
\alias{fit.cgOneFactorData}
\alias{fit,cgOneFactorData-method}
\alias{cgOneFactorFit-class}
\alias{cgOneFactorFit}
\title{
  Fit models to a cgOneFactorData object
}
\description{
  Fits a one-factor model based on the \code{cgOneFactorData} object. The
  created object is designed for one-factor / one-way / unpaired samples
  collected data, and is of class \code{\link{cgOneFactorFit}}.
}
\usage{
\S4method{fit}{cgOneFactorData}(data, type="rr", \dots)

}
\arguments{
  \item{data}{
    A data object of class \code{\link{cgOneFactorData}}.  
  }
  \item{type }{Type of model to fit, represented by a character
    value. The default value is \code{"rr"}. The four current
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
      \item{\code{"aft"}}{Fit an accelerated failure time model, using the
	\code{\link{survreg}} function from the
	\pkg{survival} package. If the \code{data} object has censored
	data and a slot state of \code{has.censored=TRUE}, then
	\code{type="aft"} will be set. See the \code{\dots} argument and
	Details below for what options are used when
	\code{\link{survreg}} is used in this wrapper method; in
	particular, the \code{robust} argument. 
      }
      \item{\code{"uv"}}{Fit an unequal variances model, with a simple
	wrapper method around the \code{\link{gls}} from the \pkg{nlme} package.
	No optional arguments are passed down to \code{\link{gls}}
	through the \code{\dots} argument.
      }
    }
  }
  \item{\dots }{
    Additional arguments, both \emph{optional}, that are allowed to be specified dependent on
    the choice of the \code{type} argument. Otherwise they have no
    effect on the fit:
    \describe{
    \item{\code{maxIter}}{
      If \code{type="rr"} or \code{type="aft"},
      then \code{maxIter} can be specified as a numeric postive
      integer. The default value of \code{maxIter} is \code{100}. For
      \code{"rr"}, this gets passed to the \code{maxit} argument in the
      \code{\link{rlm}} method. For \code{"aft"}, this gets
      passed to the \code{maxiter} argument in the \code{\link{survreg}} function.
    }
    \item{\code{sandaft}}{
      If \code{type="aft"}, then \code{sandaft} is passed to the
      \code{robust} argument of the \code{\link{survreg}} function. The
      default value of \code{sandaft} is \code{TRUE} when
      \code{type="aft"}, which applies the Huber-type (1967)
      sandwich estimator to the
      variance-covariance matrix of the group estimates.
    }
  }
  }
}
\details{
  In the current version of the \pkg{cg} package, most default settings
  for \code{\link{rlm}} are kept for the
  \code{fit.cgOneFactorData} method wrapper call  when
  \code{type="rr"}, with no capability to choose
  another value for an arguments such as \code{psi}, \code{scale.est},
  and \code{k2}. The \code{method} argument is set to \code{"MM"}.

  Analogously most \code{survreg} default settings are kept for the
  \code{fit.cgOneFactorData} method wrapper call when
  \code{type="aft"}, with no capability to modify the
  arguments. Most notably the \code{dist} argument is set to
  \code{"lognormal"} or \code{"gaussian"}, depending on
  whether a log scale analysis request is evident in the
  \code{\link{cgOneFactorData}} object or not, respectively.
}
\value{
  Creates an object of class  \code{cgOneFactorFit}, with the
  following slots:
  \describe{
    \item{\code{olsfit}}{The contents of a \code{\link{lm}} fit to the
      data. This is always populated with an \code{\link{lm}} object
      no matter the choice of the
      \code{type} argument, even though it is certainly inappropriate in
      the \code{type="aft"} case.
    }
    \item{\code{rrfit}}{The contents of a \code{\link{rlm}} fit to the
      data, housed as a \code{rrfit} class object.
      If \code{type="rr"} is not selected, then this is set
      to a simple character value of \code{"No fit was selected."}.
    }
    \item{\code{aftfit}}{The contents of a \code{\link{survreg}} fit to the
      data, with some annotations, to be a \code{aftfit} class object.
      If \code{type="aft"} is not selected, then this is set
      to a simple character value of \code{"No fit was selected."}.
    }
    \item{\code{uvfit}}{The contents of a \code{\link{gls}} fit to the
      data, housed as a \code{uvfit} class object.
      If \code{type="uv"} is not selected, then this is set
      to a simple character value of \code{"No fit was selected."}.
    }
    \item{\code{settings}}{A list of properties carried as-is from the
      \code{data} argument object of class
      \code{\link{cgOneFactorData}}.
        In particular,
	if \code{zeroscore} is specified as a non-\code{NULL} number in
	the \code{\link{cgOneFactorData}} object in the \code{data}
	argument, then a score value near zero was derived to replace all zeroes for subsequent
	log-scale analyses. Alternatively, if \code{addconstant} is specified 
	as a non-\code{NULL} number in the
	\code{\link{cgOneFactorData}} object in the \code{data}
	argument, then a value was added to shift up all observations for subsequent
	log-scale analyses. 
      }
    }
}
\references{
  Huber, P. J. (1967),
  "The Behavior of Maximum Likelihood Estimates Under
  Nonstandard Conditions",
  \emph{Proceedings of the Fifth Berkeley Symposium on Mathematical
    Statistics and Probability}, Volume 1, 221-233.
  
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
data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")
									  
canine.fit <- fit(data=canine.data, type="rr")


data(gmcsfcens)
gmcsfcens.data <- prepareCGOneFactorData(gmcsfcens, format="groupcolumns",
                                         analysisname="cytokine",
                                         endptname="GM-CSF (pg/ml)",
                                         logscale=TRUE)

gmcsfcens.fit <- fit(gmcsfcens.data, type="aft")
}
\keyword{models}
\concept{resistant}
\concept{robust}
\concept{least squares}
\concept{censored}
\concept{accelerated failure time}
\concept{unequal variance}
