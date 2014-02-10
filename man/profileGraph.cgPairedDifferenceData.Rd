\name{profileGraph.cgPairedDifferenceData}
\docType{methods}
\alias{profileGraph,cgPairedDifferenceData-method}
\alias{profileGraph.cgPairedDifferenceData}
\title{
   Graph Profiles of Experimental Unit Pairs in a cgPairedDifferenceData object
}
\description{
  Create a profile graph of the data in a \code{cgPairedDifferenceData} object.  
}
\usage{
\S4method{profileGraph}{cgPairedDifferenceData}(data, \dots)
}
\arguments{
  \item{data }{
    A \code{\link{cgPairedDifferenceData}} object.  
  }
 \item{\dots }{
   Additional arguments, both \emph{optional}. Two are currently valid: 
    \describe{
      \item{\code{logscale}}{
	A \code{logical} value, indicating whether or not the point graph should be
	plotted on the logarithmic scale.
	If \code{logscale} is not
	specified, its value is taken from the \code{cgPairedDifferenceData} object,
	which \code{\link{prepareCGPairedDifferenceData}} sets from its
	\code{logscale} argument.
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
  The profile graph for paired difference data is the simplest of profiles
  as each experimental unit has exactly two points connected by a
  straight line. Labels for the experimental units are added for identification.

  Individual points are \code{\link{jitter}}ed, and open circles
  are used to alleviate potential overlap and the danger of representing
  multiple points as a single point.
    
  Tick marks are attempted to be chosen wisely. For log-scaled axes in
  particular, leading digits of 2, 5, and 10 for values are included if
  possible. Since the algorithm is empirical, the \code{ticklabels}
  argument is available for further refinement or complete replacement
  of tickmarks.

  The heading for the graph is taken from the \code{cgPairedDifferenceData} object,
  which\cr \code{\link{prepareCGPairedDifferenceData}} sets from its \code{analysisname} 
  argument.

  The label for the y-axis is taken from the \code{cgPairedDifferenceData} object,
  which\cr \code{\link{prepareCGPairedDifferenceData}} sets from its \code{endptname} 
  and \code{endptunits} arguments.

  The number of decimal places printed in the ticks on the y-axis is taken
  from the \code{cgPairedDifferenceData} object,
  which \code{\link{prepareCGPairedDifferenceData}} sets from its \code{digits} 
  argument.

  Minimum and maximum values from ranges of data are
  respectively labeled in the bottom and top left corners of graph regions.
}
\value{
  \code{profileGraph.cgPairedDifferenceData} returns
  an invisible \code{NULL}. The main purpose is the side
  effect of graphing to the current device. 
}
\author{
  Bill Pikounis, John Oleynick, Eva Ye
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments. 
}

\examples{
data(anorexia.FT)
anorexiaFT.data <- prepareCGPairedDifferenceData(anorexiaFT, format="groupcolumns",
                                                 analysisname="Anorexia FT",
                                                 endptname="Weight",
                                                 endptunits="lbs",
                                                 expunitname="Patient",
                                                 digits=1, logscale=TRUE)
profileGraph(anorexiaFT.data) 

# Graph the data on the original scale instead of the log scale.
profileGraph(anorexiaFT.data, logscale=FALSE)
}

\concept{profile}









