\name{samplesizeGraph.cgOneFactorSampleSizeTable}
\docType{methods}
\alias{samplesizeGraph,cgOneFactorSampleSizeTable-method}
\alias{samplesizeGraph.cgOneFactorSampleSizeTable}
\title{
  Graph estimated sample sizes from a cgOneFactorSampleSizeTable object
}
\description{
  Creates a graph to see estimated sample sizes in a cgOneFactorSampleSizeTable
  object. 
}
\usage{
\S4method{samplesizeGraph}{cgOneFactorSampleSizeTable}(sstable, Nscale="log", mmdscale = "log", \dots)
}
\arguments{
  \item{sstable }{
    A sample size object of class \code{\link{cgOneFactorSampleSizeTable}}. 
  }
  \item{Nscale }{
    A \code{character} indicating whether the Y-axis, which shows the estimated
    samples sizes, should be drawn on the log scale (\code{"log"}) or
    the original scale (\code{"original"}).
  }
  \item{mmdscale }{
    A \code{character} indicating whether the X-axis, which shows the minimum
    meaningful differences to be detected, should be drawn on the log scale (\code{"log"}) or
    the original scale (\code{"original"}).
  }
  \item{\dots }{
    Additional arguments: 
    \describe{
      \item{\code{cgtheme} }{
	When set to the default \code{TRUE}, ensures a trellis device is active with
	limited color scheme. Namely \code{background},
	\code{strip.shingle}, and \code{strip.background} are each set to \code{"white"}.
      }
      \item{\code{device} }{Can be one of three values:
	
	\describe{
	  \item{\code{"single"}}{The default, which will put all graphs on the same
	    device page. For example, when resistant & robust and classical
	    least squares sample size estimates are present and \code{model="both"} (the default),
	    a 2 x 1 paneled graph will be created.}
	  \item{\code{"multiple"}}{Relevant only when resistant & robust and classical
	    least squares sample size estimates are present and \code{model="both"}. In
	    those cases, a new graphics device is generated to hold the resistant & robust
	    version, as a single-paneled graph, 
	    and the classical least squares version is on the previous device.}
	  \item{\code{"ask"}}{Relevant only when resistant & robust and classical
	    least squares samples size estimates are present and
	    \code{model="both"}
	    (the default). In
	    that case, each are portrayed as a single-paneled graph, with the
	    \code{ask=TRUE} argument specified in \code{\link{par}} so that
	    the user input confirmation is needed before the graphs are
	    drawn.
	  }
	}
      }
      \item{\code{model} }{For
	\code{cgOneFactorOneSampleSizeTable}
	objects that have 
	classical least squares \code{lm()} filled or resistant & robust
	\code{rlm()} filled slots, the following argument values are possible:
	
	\describe{
	  \item{\code{"both"}}{Sample Size graphs based on both the ordinary 
	    classical least squares and resistant & robust slots
	    are populated. This is the default when both slots are present in
	    the \code{cgOneFactorSampleSizeTable} object specified in the \code{sstable}
	    argument. If the resistant & robust fit is not available,
	    this value is not relevant.
	  }
	  \item{\code{"olsonly"}}{Only an Sample Size Table Graph based on the ordinary 
	    classical least squares table slot is created.
	  }
	  \item{\code{"rronly"}}{Only a Sample Size Table Graph based on the resistant and
	    robust table slot is created.
	  }
	}
      }

      \item{\code{mmdticklabels }}{A list of two components:
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
	    placed on the x-axis. Any numeric representations will
	    be coerced to character.
	  }
	}
      }
      \item{\code{Nticklabels }}{A list of two components:
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
  The minimum and maximum sample size values
  are added inside the plot region in
  blue, flush against the y-axis in the top and bottom left corners. 
  
  Tick marks are attempted to be chosen wisely. For log-scaled axes in
  particular, leading digits of 2, 5, and 10 for values are included if
  possible. Since the algorithm is empirical, the \code{ticklabels}
  argument is available for further refinement or complete replacement
  of tickmarks.
  
  The method essentially portrays in a graph the same information shown by the
  print method of the
  \code{\link{cgOneFactorSampleSizeTable}} object.
}
\value{
  \code{samplesizeGraph.cgOneFactorSampleSizeTable} returns
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
  \code{\link{cgOneFactorSampleSizeTable}}
}

\examples{
data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")
canine.fit <- fit(canine.data)

canine.samplesize <- samplesizeTable(canine.fit, direction="increasing", 
                                     mmdvec=c(10, 25, 50, 75, 100)) ## print method shows table

samplesizeGraph(canine.samplesize)

samplesizeGraph(canine.samplesize, model="olsonly",
                mmdticklabels=list(mod="add", marks=100))
}
\concept{sample size}
