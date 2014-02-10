\name{globalTest.cgOneFactorFit}
\docType{methods}
\alias{globalTest.cgOneFactorFit}
\alias{globalTest,cgOneFactorFit-method}
\alias{cgOneFactorGlobalTest-class}
\alias{cgOneFactorGlobalTest}
\title{
  Perform a global Test of significance with cgOneFactorFit object
}
\description{
  Performs a global test based on the cgOneFactorFit
  object, to assess whether there are any significant differences
  amongst levels of the factor, i.e. amongst the groups. A
  cgOneFactorGlobalTest class object is created.  
}
\usage{
\S4method{globalTest}{cgOneFactorFit}(fit, display="print", ...)

}
\arguments{
  \item{fit }{
    A fit object of class \code{\link{cgOneFactorFit}}.  
  }
    \item{display}{One of three valid values:
    
    \describe{
      \item{\code{"print"}}{
	The default value; It calls a \code{print} method for the
	created \cr \code{globalTest.cgOneFactorFit} object, which is
	formatted text output of the test p-values.
      }
      \item{\code{"none"}}{
	Supresses any printing. Useful, for example, when just assignment
	of the resulting object is desired.

      }
      \item{\code{"show"}}{
	Calls the default \code{\link{showDefault}} method, which
	will just print out the \code{globalTest.cgOneFactorFit} components.
      }
    }
  }
  \item{\dots }{
    Additional arguments. Only one is currently valid: 
    \describe{
      \item{\code{model} }{For \code{cgOneFactorFit} objects that have 
	classical least squares \code{lm()} or resistant & robust
	\code{rlm()} fits, the following argument values are possible:
	
	\describe{
	  \item{\code{"both"}}{Global tests on both the ordinary 
	    classical least squares and resistant robust fits are
	    performed. This is the default when both fits are present in
	    the \code{cgOneFactorFit} object specified in the \code{fit}
	    argument.
	  }
	  \item{\code{"olsonly"}}{Only a global test on the ordinary 
	    classical least squares \code{olsfit} fit is performed.
	  }
	  \item{\code{"rronly"}}{Only a global test on the resistant and
	    robust \code{rrfit} fit is performed.
	  }
	}
      }
    }
   For other possible \code{cgOneFactorFit} fit components such as
   accelerated failure time or unequal variance models, the \code{model}
   argument is not relevant, and the appropriate global test will be
   detected and performed for these model types.
  }
}
\details{
  The notion of a global F test, or equivalently, of \eqn{R^2},
  for resistant & robust linear models is
  murky, as no clear theoretical analogue to the ordinary classical
  least squares approach exists. The approach taken here is ad-hoc,
  which is essentially to re-fit a linear model with \code{lm()} and weights
  from the resistant & robust fit. This ad-hoc approach is taken when
  there are 3 or more groups.

  If there are only 2 groups, then the \code{\link{comparisonsTable.cgOneFactorFit}}
  method is used with the \code{rlm()} model component.
}
\value{
  Creates an object of class  \code{cgOneFactorGlobalTest}, with the
  following slots:
  \describe{
    \item{\code{ols.gpval}}{The p-value of a global F test applied
      to the \code{olsfit} component of the \code{\link{cgOneFactorFit}}
      object, unless \code{model="rronly"} is specified. Will not be appropriate in
      the case where a valid \code{aftfit} component is present in the
      \code{\link{cgOneFactorFit}} object.
    }
    \item{\code{rr.gpval}}{The p-value of an ad-hoc global test applied
      to the \code{rrfit} component of the \code{\link{cgOneFactorFit}}
      object, if a valid resistant & robust fit object is present.
      See the Details section
      above. If \code{rrfit} is a simple character value of
      \code{"No fit was selected."}, or \code{model="olsonly"} was
      specified, then the value is \code{NULL}.
    }
    \item{\code{aft.gpval}}{The p-value of a global chi-square test applied
      to the \code{aftfit} component of the \code{\link{cgOneFactorFit}}
      object if a valid accelerated failure time fit object is present.
      If \code{aftfit} is a simple character value of
      \code{"No fit was selected."}, then the value is \code{NULL}.
    }
    \item{\code{uv.gpval}}{The p-value of a global F test applied
      to the \code{uvfit} component of the \code{\link{cgOneFactorFit}}
      object if a valid unequal variances fit object is present.
    }
  }
}
\author{
  Bill Pikounis, John Oleynick, Eva Ye
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

canine.globalTest <- globalTest(canine.fit)

globalTest(canine.fit, model="both")

globalTest(canine.fit, model="olsonly")

globalTest(canine.fit, model="rronly")


data(gmcsfcens)
gmcsfcens.data <- prepareCGOneFactorData(gmcsfcens, format="groupcolumns",
                                         analysisname="cytokine",
                                         endptname="GM-CSF (pg/ml)",
                                         logscale=TRUE)

gmcsfcens.fit <- fit(gmcsfcens.data, type="aft")

globalTest(gmcsfcens.fit)

}
\concept{global test}