\name{diffGraph.pairedDifferenceData}
\docType{methods}
\alias{diffGraph,cgPairedDifferenceData-method}
\alias{diffGraph.cgPairedDifferenceData}
\title{
   Graph Paired Differences from a cgPairedDifferenceData object
}
\description{
  Create a graph of profile pairs in a \code{cgOnePairedDifferenceData} object.
}
\usage{
\S4method{diffGraph}{cgPairedDifferenceData}(data, \dots)
}
\arguments{
  \item{data }{
    A \code{\link{cgPairedDifferenceData}} object. 
  }
 \item{\dots }{
Additional arguments, both \emph{optional}. Two are currently valid: 
    \describe{
      \item{\code{logscale}}{
	A \code{logical} value, indicating whether or not the
	inidividual differences along the y-axis in the difference graph should be
	plotted on the logarithmic scale.
	If \code{logscale} is not
	specified, its value is taken from the \code{cgPairedDifferenceData} object,
	which \code{\link{prepareCGPairedDifferenceData}} sets
	from its \code{logscale} argument.
      }
      \item{\code{ticklabels }}{A list of two components:
	\describe{
	  \item{\code{mod}}{Can be either of these two values,
	    \describe{
	      \item{\code{"replace"}}{Before graphing the data, remove
		any automatically generated tickmarks for the y-axis,
		and create the tickmarks specified in the
		\code{marks} component below.
	      }
	      \item{\code{"add"}}{Before graphing the data, add
		tickmarks specified in the 
		\code{marks} component below, to the automatically
		generated ones.
	      }
	    }
	  }
	  \item{\code{marks}}{A vector of tickmarks to be
	    placed on the y-axis. Any numeric representations will
	    be coerced to character.
	  }
	}
      }
    }
  }
}
\details{
  The individual paired differences are displayed in 3 ways side by
  side.

  Each difference is determined by subtracting the \code{refgrp} value from
  the other group's value, for each experimental unit
  in the \code{data} object of class \code{\link{cgPairedDifferenceData}}.

  In the middle section, the individual paired difference points
  are \code{\link{jitter}}ed, and open circles
  are used to alleviate potential overlap and the danger of representing
  multiple points as a single point.

  In the right hand section, a boxplot is added, similar to to that produced by 
  \pkg{graphics}::\code{\link{boxplot.default}},
  but with the median shown as a "+" and the mean shown as a "o".  A
  warning on the lack of usefulness of a boxplot is added to the graph if there are 
  have 5 or fewer paired differences.
 
  Tick marks are attempted to be chosen wisely. For log-scaled axes in
  particular, leading digits of 2, 5, and 10 for values are included if
  possible. Since the algorithm is empirical, the \code{ticklabels}
  argument is available for further refinement or complete replacement
  of tickmarks.

  The heading for the graph includes a creation of a character
  string: The "other" group versus the \code{refgrp}, e.g. \code{B vs. A}.
  Also included in the heading is the \code{analysisname}
  setting from the
  \code{\link{cgPairedDifferenceData}}
  object.
  
  The label for the y-axis also includes the \code{B vs. A} 
  character description of the comparison. If \code{logscale=TRUE},
  precent differences represent the tickmarks on the log-spaced scale,
  since the differences in the log scale correspond to ratios in the
  original scale, e.g. \code{B / A}. Also included in the y-axis label
  is a character string derived from the \code{endptname} 
  and \code{endptunits} settings in the \code{\link{cgPairedDifferenceData}}
  object. Percent differences make up the left-hand y-axis, and the
  corresponding Ratios make the right-hand left axis.

  Minimum and maximum values from the range of the differences are
  respectively labeled in the bottom and top left corners of the graph
  region. Percentages are displayed when \code{logscale=TRUE}.
}
\value{
  \code{diffGraph.cgPairedDifferenceData} returns
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
  \code{\link{diffGraph.cgPairedDifferenceData}}
}

\examples{
data(anorexia.FT)
anorexiaFT.data <- prepareCGPairedDifferenceData(anorexiaFT, format="groupcolumns",
                                                 analysisname="Anorexia FT",
                                                 endptname="Weight",
                                                 endptunits="lbs",
                                                 expunitname="Patient",
                                                 digits=1, logscale=TRUE)
diffGraph(anorexiaFT.data) 

## Graph the data on the original scale instead of the log scale.
diffGraph(anorexiaFT.data, logscale=FALSE)
}
\concept{difference graph}









