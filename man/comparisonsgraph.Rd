\name{comparisonsgraph}

\alias{comparisonsgraph}
\title{
   Create an graph of comparisons 
}
\description{
  Creates a graph to see comparisons based on group estimates and
  variance-covariance matrix
}
\usage{
comparisonsgraph(compstable, difftype, analysisname = "",
                 endptname = "", alpha = 0.05, digits = NULL,
                 titlestamp = TRUE, explanation = TRUE,
                 wraplength = 20, cex.comps = 0.7,
                 ticklabels = NULL, ...) 
}
\arguments{
  \item{compstable }{
    A data frame object of form like that created by the \code{\link{comparisons}}
    function. 
  }
  \item{difftype }{Must be specified as one of the following:
    \describe{
      \item{\code{"percent"}}{Presumes the estimated differences in
	\code{compstable} are Percent, and thus will space the x-axis logarithmically;
      }
      \item{\code{"amount"}}{Presumes the estimated differences in
	\code{compstable} are Simple Amounts, and thus will space the
	x-axis in untransformed scale;
      }
      \item{\code{"simple"}}{Synonym for \code{amount}, presumes the estimated differences in
	\code{compstable} are Simple Amounts, and thus will space the
	x-axis in untransformed scale.
      }
    }
  }
  \item{analysisname }{\emph{Optional}, a character text or
    math-valid expression that will used in the graph title. The default
   value is the empty \code{""}.}
 \item{endptname }{\emph{Optional}, a character text or math-valid expression
   that that will be used as the x-axis label of the graph.
   The default
   value is the empty \code{""}.}
  \item{alpha }{Significance level, by default set to \code{0.05}. This
    is only used for labelling purposes.
  }
  \item{digits }{\emph{Optional}, For output display purposes in the
    graph,
    values will be rounded to this numeric
    value. Only the integers of 0, 1, 2, 3, and 4 are accepted. No
    rounding is done during any calculations. The default value is
    \code{NULL}, which will examine each individual data value and choose the
    one that has the maximum number of digits after any trailing
    zeroes are ignored. The max number of digits will be 4.}
  \item{explanation }{If \code{TRUE}, which is the default,
    add explanatory message to the graph rendering
    about "Error bars that do not cross the zero line indicate statistically significant difference(s)" along with the confidence
    level derived from \code{alpha}.
  }
  \item{titlestamp }{Specify text to the graph in the top of graph area,
    otherwise a default description of "Comparisons Graph" and
    \code{analysisname} will be constructed.
  }
  \item{wraplength }{On the left hand axis are each A vs. B comparison label
   from the \code{compstable} data frame. An attempt at sensible formatting
   when a newline is needed is made, but adjustment by this argument may
   be needed. The default is \code{20} characters before wrapping to a newline.
 }
 \item{cex.comps }{Similar to \code{wraplength},
   adjustment of this argument parameter can
   be made to fit the comparison labels on the left hand axis.
 }
 \item{ticklabels }{\emph{Optional}, 
    before graphing the data, remove
    any automatically generated tickmarks for the x-axis,
    and use these tickmarks instead. A vector of tickmarks to be
    placed on the x-axis. Any numeric representations will
    be coerced to character.
  }
  \item{\dots }{
    Additional arguments. None are currently used.
  }
}
\section{Warning}{
  This function was created for internal use in the \pkg{cg} package as
  its use can be seen in the \code{\link{comparisonsGraph}} methods
  source code. Therefore any direct use of it needs to be done cautiously.
}
\details{
  The minimum and maximum values across all the bar ends
  are added inside the plot region in
  blue, flush against the x-axis. In two panel cases, there is a
  tendency to fall outside the panel area even though right justified is
  used for the \code{adj} parameter of functions like \code{\link{panel.text}}.
}
\value{
  \code{comparisonsgraph} returns
  an invisible \code{NULL}. The main purpose is the side
  effect of graphing to the current device.
}
\author{
  Bill Pikounis [aut, cre, cph], John Oleynick [aut], Eva Ye [ctb]
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments. 
}
\seealso{
  \code{\link{comparisons}}
}
\examples{
data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")
canine.fit <- fit(canine.data)
canine.comps <- comparisonsTable(canine.fit)

## Easier way: notice the camel-case of the comparisonsGraph call

comparisonsGraph(canine.comps, model="olsonly")

## Manual way
## Instead of comparisonsGraph(canine.comps, model="olsonly")

canine.compstable <- comparisons(estimates=canine.fit@olsfit$coef,
                                 varcovmatrix=vcov(canine.fit@olsfit),
                                 errordf=canine.fit@olsfit$df.residual,
                                 endptscale="log",
                                 analysisname="Canine",
                                 digits=1,
                                 endptname="Prostate Volume")

comparisonsgraph(canine.compstable,
                 difftype="percent",
                 analysisname="Canine",
                 digits=1,
                 endptname=expression(paste( plain('Prostate Volume'),
                                      ' (', plain(cm)^3  ,  ')' ))  
                ) 
}

\concept{comparisons}
