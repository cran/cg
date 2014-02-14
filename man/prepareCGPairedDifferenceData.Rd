\name{prepareCGPairedDifferenceData}
\docType{class}
\alias{prepareCGPairedDifferenceData}
\alias{cgPairedDifferenceData-class}
\alias{cgPairedDifferenceData}
\title{
  Prepare data object from a data frame for Paired Samples evaluations
}
\description{
  The function \code{prepareCGPairedDifferenceData} reads in a data frame and
  settings
  in order to create a
  \code{cgPairedDifferenceData} object. The created object is designed to have exploratory and
  fit methods applied to it. 
}
\usage{
prepareCGPairedDifferenceData(dfr, format = "listed", analysisname = "",
 endptname = "", endptunits = "", logscale = TRUE, zeroscore = NULL,
 addconstant = NULL, digits = NULL, expunitname= "",
 refgrp = NULL, stamps = FALSE)
}
\arguments{
  \item{dfr }{A valid data frame, see the \code{format} argument.}
  \item{format }{Default value of \code{"listed"}. Either \code{"listed"} or
    \code{"groupcolumns"}
    must be used. Abbreviations of \code{"l"} or \code{"g"}, respectively,
    or otherwise sufficient matching values can be used:
    \describe{
      \item{\code{"listed"}}{At least two columns, with the two and only
	two levels of a factor to represent the samples. These factor
	levels would need to be in the first column and response values
	in the second column. If there are three columns, then an
	experimental unit identifier need to be defined in the first
	column instead, with the second column having the two level
	factor, and the third column having the response values.
	See the Details Input Data Frame section below.}
      \item{\code{"groupcolumns"}}{At least two columns and no more than
	three are permitted. In the two columns case, each column must
	uniquely represent one of the two samples, implying a factor
	with two and only two levels. The levels of the factor
	make up the column headers. The values in the data frame are for
	the response. Each row assumes the pairing of
	the observation within an experimental unit, such as the same
	subject. If there are three columns, then an
	experimental unit identifier need to be defined in the first
	column instead, with the second and third column having the
	response values and headers to represent the two factor levels.
	See the Details Input Data Frame section below.
      }
    }
  }
  \item{analysisname }{\emph{Optional}, a character text or
    math-valid expression that will be set for
    default use in graph title and table methods. The default
    value is the empty \code{""}.}
  \item{endptname }{\emph{Optional}, a character text or math-valid expression
    that will be set for default use as the y-axis label of graph
    methods, and also used for table methods. The default
    value is the empty \code{""}.}
  \item{endptunits }{\emph{Optional}, a character text or math-valid
    expression that can be used in combination with the endptname
    argument.
    Parentheses are
    automatically added to this input, which will be added to the end
    of the endptname character value or expression. The default
    value is the empty \code{""}.}
  \item{logscale }{Apply a log-transformation to the data for
    evaluations. The default value is \code{TRUE}.}
  \item{zeroscore }{\emph{Optional},
    replace response values of zero with a derived or specified
    numeric value, as an approach to overcome the presence of zeroes
    when evaluation in the
    logarithmic scale (\code{logscale=TRUE}) is specified. The default value
    is \code{NULL}. To derive a score value to replace zero,
    \code{"estimate"} can be specified, see Details below on the algorithm used.}
  \item{addconstant }{\emph{Optional},
    add a numeric constant to all response values, as an
    approach to overcome the presence of zeroes when evaluation in the
    logarithmic scale \code{logscale=TRUE} is desired. The default value is
    \code{NULL}. A positive numeric value can be specified to be added, or a "simple"
    algorthm specified to estimate a value to add. See Details secion
    below on the algorithm used.}
  \item{digits }{\emph{Optional}, for output display purposes in graphs
    and table methods, values will be rounded to this numeric
    value. Only the integers of 0, 1, 2, 3, and 4 are accepted. No
    rounding is done during any calculations. The default value is
    \code{NULL}, which will examine each individual data value and choose the
    one that has the maximum number of digits after any trailing
    zeroes are ignored. The max number of digits will be 4.}
  \item{expunitname }{\emph{Optional}, a character text
    that will be set for default use as the experimental unit label of graph
    methods, and also used for table methods. The default
    value is the empty \code{""}.}
  \item{refgrp}{\emph{Optional}, specify one of the factor levels to be the
    \dQuote{reference group}, such as a \dQuote{control} group.
    The default value is \code{NULL},
    which will just use the first level determined in the data frame.}
  \item{stamps }{\emph{Optional}, specify a time stamp in graphs, along
    with \pkg{cg} package
    version identification. The default value is \code{FALSE}.}
}

\details{
  \describe{
    \item{\bold{Input Data Frame}}{
      The input data frame \code{dfr} can be of the format
      \code{"listed"} or \code{"groupcolumns"}.

      If \code{format="listed"} for \code{dfr} is specified, then there
      must be three columns for an input data frame. The first column
      needs to be the experimental unit identifier,
      the second column needs to be the group identifier,
      and the third is the endpoint. The first column of the listed input data format,
      needs to have two sets of distinct values since it is the
      experimental unit identifier of response pairs. The second column of the listed
      input data format needs to have exactly 2 distinct values since
      it is the group identifier.

      If \code{format="groupcolumns"} for \code{dfr} is specified, then
      there can be two columns or three columns. 
       \describe{
	 \item{\bold{two columns}}{The column headers specify the two
	   paired group names. Each row contains the experimental unit
	   of paired numeric values under those two groups. In the
	   course of creating the \code{cgPairedDifferenceData} object,
	   another column will be binded from the left and become the
	   first column, with the column header of
	   \code{expunitname} is specified, and "expunit" if the default
	   \code{expunitname=""} is specified. A sequence of integers
	   starting with 1 up to the number of pairs/rows will be
	   generated to uniquely identify each experimental unit pair. 
	}
	\item{\bold{three columns}}{The first column needs to be unique
	  experimental unit identifiers of the paired numeric values in
	  the second and third columns. The second and third column
	  headers will be used to identify the two paired group names.
	  Each row's second and third column needs to contain the experimental unit
	  of paired numeric values under those two groups. The name of
	  the first column will be assigned to the \code{expunitname}
	  setting if \code{expunitname} is not explicity specified to
	  something else instead of its default \code{expunitname=""}. 
	}
      }
      As the evaluation data set is prepared for 
      \code{cgPairedDifferenceData} object, any experimental unit
      pairs/rows with
      missing values in the
      endpoint are flagged. This includes a check to make sure that each
      experimental unit identified has a complete pair of numeric observations.
    }
    
    \item{\bold{zeroscore}}{
      If \code{zeroscore="estimate"} is specified, a number 
      close to zero is derived to replace all zeroes for subsequent
      log-scale analyses. A spline fit (using \code{\link{spline}} and
      \code{method="natural"})
      of the log of the
      response vector on the original response vector is performed. The
      zeroscore is then derived from the log-scale value of the spline curve at the original
      scale value of zero. This approach comes from the concept of
      arithmetic-logarithmic scaling discussed in Tukey, Ciminera, and
      Heyse (1985).
    }
    
    \item{\bold{addconstant}}{
      If \code{addconstant="simple"}  is specified, a number is derived and added
      to all response values. The approach taken is
      from the "white" book on S (Chambers and Hastie, 1992),
      page 68. The range (\code{max - min}) of the response values is
      multiplied by \code{0.0001} to derive the number to add to all the
      response values.
      
    }
  }
}
\value{
  A \code{cgPairedDifferenceData} object is returned, with the following slots:
  \item{dfr }{The original input data frame that is the specified value of the
    \code{dfr} argument in the function call.}
  \item{dfru }{Processed version of the input data frame, which will be
    used for the various evaluation methods.}
  \item{dfr.gcfmt }{A groupcolumns version of the input data frame with
    an additional column of the differences between groups, where the
    \code{regfrp} column of values is the subtrahend (second term) in the subtraction.}
  \item{settings }{A list of properties associated with the data
    frame:
    \describe{
      \item{\code{analysisname}}{Drawn from the input argument value of
	\code{analysisname}.}
      \item{\code{endptname}}{Drawn from the input argument value of
	\code{endptname}.}
      \item{\code{endptunits}}{Drawn from the input argument value of
	\code{endptunits}.}
      \item{\code{endptscale}}{Has the value of \code{"log"} if
	\code{logscale=TRUE} and \code{"original"} if
	\code{logscale=FALSE}.}
      \item{\code{zeroscore}}{Has the value of \code{NULL} if the input argument
	was \code{NULL}. Otherwise has the derived (from
	\code{zeroscore="estimate"})
	or specified numeric value.}
      \item{\code{addconstant}}{Has the value of \code{NULL} if the input argument
	was \code{NULL}. Otherwise has the specified or derived numeric
	value.}
      \item{\code{digits}}{Has the value of the input argument
	\code{digits} or is set to the determined value of digits from the
	input data. Will be an integer of 0, 1, 2, 3, or 4.}
      \item{\code{grpnames}}{Of length 2 and determined from the single factor identified of the
	group names. The order is determined by the first occurence in the
	input data frame header in \code{dfr} and the \code{refgrp} specification.}
      \item{\code{expunitname}}{Drawn from the input argument value of
	\code{expunitname}.}
      \item{\code{refgrp}}{Drawn from the input argument of \code{refgrp}.}
      \item{\code{stamps}}{Drawn from the input argument of \code{stamps}.}
    }
  }
}
\references{
  Tukey, J.W., Ciminera, J.L., and Heyse, J.F. (1985). "Testing the
  Statistical Certainty of a Response to Increasing Doses of a
  Drug," \emph{Biometrics}, Volume 41, 295-301.

  Chambers, J.M, and Hastie, T.R. (1992), 
  \emph{Statistical Modeling in S.}  Chapman&Hall/CRC.
}
\author{
  Bill Pikounis, John Oleynick, Eva Ye 
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments.
}
\seealso{
  \code{\link{prepare}}
}
\examples{
data(anorexiaFT)
anorexiaFT.data <- prepareCGPairedDifferenceData(anorexiaFT, format="groupcolumns",
                                                 analysisname="Anorexia FT",
                                                 endptname="Weight",
                                                 endptunits="lbs",
                                                 expunitname="Patient",
                                                 digits=1, logscale=TRUE)
}

\keyword{datagen}
