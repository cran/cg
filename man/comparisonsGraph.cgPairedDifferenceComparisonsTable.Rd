\name{comparisonsGraph.cgPairedDifferenceComparisonsTable}
\docType{methods}
\alias{comparisonsGraph.cgPairedDifferenceComparisonsTable}
\alias{comparisonsGraph,cgPairedDifferenceComparisonsTable-method}
\title{
   Create an graph of the comparisons in a cgPairedDifferenceComparisonsTable object
}
\description{
  Creates a graph to see comparisons in a cgPairedDifferenceComparisonsTable
  object
}
\usage{
\S4method{comparisonsGraph}{cgPairedDifferenceComparisonsTable}(compstable,
cgtheme=TRUE, device="single", wraplength = 20, cex.comps = 0.7, ...)
}
\arguments{
  \item{compstable }{
     A \code{\link{cgPairedDifferenceComparisonsTable}} object prepared by the \cr\code{\link{comparisonsTable.cgPairedDifferenceFit}} method.
  }
  \item{cgtheme }{
    When set to the default \code{TRUE}, ensures a trellis device is active with
    limited color scheme. Namely, \code{background},
    \code{strip.shingle}, and \code{strip.background} are each set to \code{"white"}.
  }
  \item{device }{
    Can be one of three values:

    \describe{
      \item{\code{"single"}}{The default, which will put all graphs on the same
	device page. For example, when resistant & robust and classical
	least squares are present and \code{model="both"} (the default),
	a 2 x 1 paneled graph will be created.}
      \item{\code{"multiple"}}{Relevant only when resistant & robust and classical
	least squares are present and \code{model="both"} (the default). In
	that case, a new graphics device is generated to hold the resistant & robust
	version, as a single-paneled graph.
	The classical least squares version is on the previous device.}
      \item{\code{"ask"}}{Relevant only when resistant & robust and classical
	least squares are present and \code{model="both"} (the default). In
	that case, each are portrayed as a single-paneled graph, with the
	\code{ask=TRUE} argument specified in \code{\link{par}} so that
	user input confirmation is needed before the graphs are
	drawn.
      }
    }
  }
  \item{wraplength }{On the left hand axis are each A vs. B comparison label
   from the \code{compstable} object. An attempt at sensible formatting
   when a newline is needed is made, but adjustment by this argument may
   be needed. The default is \code{20} characters before wrapping to a newline.
  }
  \item{cex.comps }{Similar to \code{wraplength},
   adjustment of this argument parameter can
   be made to fit the comparison labels on the left hand axis.
  }
  \item{\dots }{
   Additional arguments. Two are currently valid:
   \describe{
     \item{\code{model} }{For
       \code{cgPairedDifferencePairedDifferenceComparisonsTable}
       objects that have
       classical least squares \code{lm()} or resistant & robust
       \code{rlm()} table slots, the following argument values are possible:

	\describe{
	  \item{\code{"both"}}{Graphs of Comparisons Tables based on both the ordinary
	    classical least squares and resistant & robust slots
	    are populated. This is the default when both slots are present in
	    the \code{cgPairedDifferenceComparisonsTable} object specified in the \code{compstable}
	    argument. If the resistant & robust fit is not available,
	    this value is not relevant.
	  }
	  \item{\code{"olsonly"}}{Only an Comparisons Graph based on the ordinary
	    classical least squares table slot is performed.
	  }
	  \item{\code{"rronly"}}{Only a Comparisons Graph based on the resistant and
	    robust table slot is performed.
	  }
	}

      }

    \item{\code{ticklabels }}{A list of two components:
	\describe{
	  \item{\code{mod}}{Can be either of these two values,
	    \describe{
	      \item{\code{"replace"}}{Before graphing the data, remove
		any automatically generated tickmarks for the x-axis,
		and create the tickmarks specified in the
		\code{marks} component below.
	      }
	      \item{\code{"add"}}{Before graphing the data, add
		tickmarks specified in the
		\code{marks} component to the automatically
		generated ones.
	      }
	    }
	  }
	  \item{\code{marks}}{A vector of tickmarks to be
	    placed on the x-axis. Any numeric representations will
	    be coerced to character.
	  }
	}
      }
    }
  }
}
\details{
  The minimum and maximum values across the bar ends
  are added inside the plot region in blue, flush against the x-axis.
  The number of decimal places are determined by the
  \code{digits} and \code{endptscale} values in the \code{compstable@settings} slot.

}
\value{
  \code{comparisonsGraph.cgPairedDifferenceComparisonsTable} returns
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
  \code{\link{cgPairedDifferenceComparisonsTable}}
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

anorexiaFT.comps0 <- comparisonsTable(anorexiaFT.fit)
anorexiaFT.comps1 <- comparisonsTable(anorexiaFT.fit,  model="olsonly", display="none")

comparisonsGraph(anorexiaFT.comps0)
comparisonsGraph(anorexiaFT.comps1)

}

