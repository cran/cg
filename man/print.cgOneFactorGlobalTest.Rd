\name{print.cgOneFactorGlobalTest}
\docType{methods}
\alias{print,cgOneFactorGlobalTest-method}
\alias{print.cgOneFactorGlobalTest}
\title{
  Print One Factor Global F-test object wiht some format options 
}
\description{
  Print a \code{cgOneFactorGlobalTest} object, which contains global
  F-test p-value information taken from a \code{cgOneFactorFit}
  object.
}
\usage{
\S4method{print}{cgOneFactorGlobalTest}(x, title = NULL, endptname = NULL, ...)
}
\arguments{
  \item{x }{
    An \code{\link{cgOneFactorGlobalTest}} object,
    typically created by \code{\link{globalTest.cgOneFactorFit}}.
  }
  \item{title }{
    The title printed out with the p-value.
    If \code{NULL}, it is set to be "Group Test P-value of"
    the \code{analysisname} value in the \code{settings} slot of the 
    \code{\link{cgOneFactorGlobalTest}} object.
  }
  \item{endptname }{
    The endpoint name, printed out with the p-value. 
    If \code{NULL}, it is set to 
    the \code{endptname} value in the \code{settings} slot of the 
    \code{\link{cgOneFactorGlobalTest}} object.
  }
  \item{\dots }{
    Additional arguments. Only one is currently valid: 
    \describe{
      \item{\code{model} }{For \code{cgOneFactorGlobalTest}
	objects that have p-values derived from
	classical least squares \code{\link{lm}} or resistant & robust
	\code{\link{rlm}} fits, the following argument values are possible:
	
	\describe{
	  \item{\code{"both"}}{Both the ordinary 
	    classical least squares and resistant & robust p-values are
	    printed.
	    This is the default when both fits are present in
	    the \code{cgOneFactorGlobalTest} object specified in the \code{x}
	    argument.
	  }
	  \item{\code{"olsonly"}}{Only the ordinary 
	    classical least squares p-value is printed.
	  }
	  \item{\code{"rronly"}}{Only the resistant &
	    robust approximated p-value is printed.
	  }
	}
      }
    }
    For other possible \code{cgOneFactorGlobalTest} p-value
    components such as
    accelerated failure time or unequal variance models, the \code{model}
    argument is not relevant, and the single p-value will just be printed
    for these model types.
  }
  
}
\details{
  The smallest actual p-value that will be printed is \code{0.001}. Anything
  less than \code{0.001} will be displayed as \code{< 0.001}. If you
  need more digits, see the \code{\link{cgOneFactorGlobalTest}} object.

  The notion of a global F test, or equivalently, of \eqn{R^2},
  for resistant & robust linear models is
  murky, as no clear theoretical analogue to the ordinary classical
  least squares approach exists. See \code{\link{cgOneFactorGlobalTest}}
  for details, and regard the output p-value here as ad-hoc.

  The object is printed using a mix of \code{cat} and \code{print}
  calls. See \code{\link{cgOneFactorGlobalTest}}
  for details of the \code{*.gpval} and other object slots.
}
\value{
  \code{print.cgOneFactorGlobalTest} returns
  \code{\link{invisible}}. The main purpose is the side
  effect of printing to the current output connection, which is
  typically the console.
}
\author{
  Bill Pikounis and John Oleynick
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments. 
}
\seealso{
  \code{\link{cgOneFactorGlobalTest}}
}
\examples{
data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")
canine.fit <- fit(canine.data)

canine.global <- globalTest(canine.fit)

print(canine.global)


}

