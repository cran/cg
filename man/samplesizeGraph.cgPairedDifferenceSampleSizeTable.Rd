\name{samplesizeGraph.cgPairedDifferenceSampleSizeTable}
\docType{methods}
\alias{samplesizeGraph,cgPairedDifferenceSampleSizeTable-method}
\alias{samplesizeGraph.cgPairedDifferenceSampleSizeTable}
\title{
  Graph estimated sample sizes from a cgPairedDifferenceSampleSizeTable object
}
\description{
  Creates a graph to see estimated sample sizes in a cgPairedDifferenceSampleSizeTable
  object. 
}
\usage{
\S4method{samplesizeGraph}{cgPairedDifferenceSampleSizeTable}(sstable, Nscale, mmdscale, \dots)
}
\arguments{
  \item{sstable }{
    A sample size object of class \code{\link{cgPairedDifferenceSampleSizeTable}}. 
  }
  \item{Nscale }{
    A \code{character} indicating whether the left-hand side Y-axis, which shows the estimated
    sample sizes in terms of the number of experimental units,
    should be drawn on the log scale (\code{"log"}) or
    the original scale (\code{"original"}).
  }
  \item{mmdscale }{
    A \code{character} indicating whether the X-axis, which shows the minimum
    meaningful differences to be detected, should be drawn on the log scale (\code{"log"}) or
    the original scale (\code{"original"}).
  }
  \item{\dots }{
    Additional arguments. Two are currently valid:

    \describe{
      
      \item{\code{nscale }}{A \code{character} indicating whether the
	Y-axis, which shows the estimated
	sample sizes in terms of the number of \code{n} experimental units
	on the left-hand axis,
	should be drawn on the log scale (\code{"log"}) or
	the original scale (\code{"original"}). This will override the
	\code{Nscale} argument, which serves the same purpose. 
      }
      \item{\code{mmdticklabels }}{A list of two components:
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
		\code{marks} component below, to the automatically
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
      \item{\code{nticklabels }}{A list of two components:
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
	  \item{\code{marks }}{A numeric vector of tickmarks to be
	    placed on the y-axis. Any numeric representations will
	    be coerced to character.
	  }
	}
      }
    }
  }
}
\details{
  The minimum and maximum experimental unit sample size values
  are added inside the plot region in
  blue, flush against the y-axis in the top and bottom left corners. 
  
  Tick marks are attempted to be chosen wisely. For log-scaled axes in
  particular, leading digits of 2, 5, and 10 for values are included if
  possible. Since the algorithm is empirical, the \code{ticklabels}
  argument is available for further refinement or complete replacement
  of tickmarks.

  The method essentially portrays in a graph the same information shown
  by the print method of the
  \code{\link{cgPairedDifferenceSampleSizeTable}} object.
}
\value{
  \code{samplesizeGraph.cgPairedDifferenceSampleSizeTable} returns
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
  \code{\link{cgPairedDifferenceSampleSizeTable}}
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

## The interest is in increased weight for the anorexia FT
## (family treatment) group of patients
anorexiaFT.samplesize <- samplesizeTable(anorexiaFT.fit, direction="increasing",
                         mmdvec=c(5, 10, 15, 20)) ## print method shows table

samplesizeGraph(anorexiaFT.samplesize)

samplesizeGraph(anorexiaFT.samplesize, nticklabels=list(mod="add", marks=3))
}
\concept{sample size}

