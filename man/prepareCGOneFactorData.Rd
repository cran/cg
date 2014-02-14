\name{prepareCGOneFactorData}
\docType{class}
\alias{prepareCGOneFactorData}
\alias{cgOneFactorData-class}
\alias{cgOneFactorData}
\title{
  Prepare data object from a data frame for One Factor / One-Way /
  Unpaired Samples evaluations
}
\description{
  The function \code{prepareCGOneFactorData} reads in a data frame and
  settings
  in order to create a
  \code{cgOneFactorData} object. The created object is designed to have exploratory and
  fit methods applied to it. 
}
\usage{
prepareCGOneFactorData(dfr, format = "listed", analysisname = "",
 endptname = "", endptunits = "", logscale = TRUE, zeroscore = NULL,
 addconstant = NULL, rightcensor = NULL, leftcensor = NULL, digits = NULL,
 refgrp = NULL, stamps = FALSE)
}
\arguments{
  \item{dfr }{A valid data frame, see the \code{format} argument.}
  \item{format }{Default value of \code{"listed"}. Either \code{"listed"} or
    \code{"groupcolumns"}
    must be used. Abbreviations of \code{"l"} or \code{"g"}, respectively,
    or otherwise sufficient matching values can be used:
    \describe{
      \item{\code{"listed"}}{At least two columns, with the factor levels in
	the first column and response values in the second column. If there
	is censored data, then two or three more columns are required, see
	the Details Input Data Frame section below.}
      \item{\code{"groupcolumns"}}{Each column must represent a group. Each
	group is a unique level of the one factor, so the levels of the factor
	make up the column headers. The values in the data frame are for
	the response. If the groups have unequal sample sizes, the empty
	cells within the data frame can have \code{NA}'s or be left
	blank. Censored values can be represented; see the Details Input
	Data Frame section below. Otherwise, any character data will be coerced to
	numeric data with possibly undesirable results.
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
    \code{NULL}.   positive numeric value can be specified to be added, or a "simple"
    algorthm specified to estimate a value to add. See Details secion
    below on the algorithm used.}
  \item{rightcensor }{\emph{Optional}, can be specified with a numeric
    value where any value equal to or greater will be regarded as
    right censored in the evaluation. The value of \code{TRUE} can be
    used to coerce a binary status variable in the data frame to be
    right censored for its values.The default value is \code{NULL}.
    See the Details Input Data Frame section
    below for specifications and consequences.}
  \item{leftcensor }{\emph{Optional}, can be specified with a numeric
    value where any value equal to or lesser will be regarded as
    left censored in the evaluation. The value of \code{TRUE} can be
    used to coerce a binary status variable in the data frame to be
    right censored for its values. The default value is \code{NULL}.
    See the Details Input Data Frame section
    below for specifications and consequences.}
  \item{digits }{\emph{Optional}, for output display purposes in graphs
    and table methods, values will be rounded to this numeric
    value. Only the integers of 0, 1, 2, 3, and 4 are accepted. No
    rounding is done during any calculations. The default value is
    \code{NULL}, which will examine each individual data value and choose the
    one that has the maximum number of digits after any trailing
    zeroes are ignored. The max number of digits will be 4.}
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
      \code{"listed"} or \code{"groupcolumns"}. Another distinguishing
      characteristic is whether or not it contains censored data
      representations.

      Censored observations can be represented by \code{<} for
      left-censoring
      and \code{>} for
      right-censoring. The \code{<} value refers to values less than or equal
      to a numeric value. For example, \code{<0.76} denotes a left-censored
      value of 0.76
      or less. Similarly, \code{>2.02} denotes a value of 2.02 or greater for
      a right-censored value. There must be no space between the direction
      indicator and the numeric value. These representations can be used in
      either the \code{listed} or \code{groupcolumns} formats for \code{dfr}.

      No interval-censored representations are currently handled when
      \code{format="groupcolumns"}. 

      If \code{format="groupcolumns"} for \code{dfr} is specified, then the
      number of columns must equal the number of groups, and any censored
      values must follow the \code{<} and \code{>} representations.
      The individual group values are of mode character, since any
      censored values will be represented for example as \code{<0.76} or
       \code{>2.02}. If any of the groups have less number of
      observations than any others, i.e. there are unequal sample sizes,
      then the corresponding "no data" cells in the data frame need to
      contain empty quote \code{""} values.

      If \code{format="listed"} for \code{dfr} is specified, then there may be
      anywhere from two to four columns for an input data frame.
      
      \describe{
	\item{\bold{two columns}}{The first column has the group levels to define the
	  factor, and the second column contains the response values. Censored
	  representations of \code{<} and \code{>} can be used here. One or
	  both of
	  \code{rightcensor} or \code{leftcensor} may also be specified as a
	  number. If
	  a number is specified for \code{rightcensor}, then all values in
	  the second column equal to this value will be processed as
	  right-censored. Analogously, if
	  a number is specified for \code{leftcensor}, then all values in
	  the second column equal to this value will be processed as
	  left-censored. \bold{WARNING:} This should be used cautiously to make sure the
	  equality occurs as desired. This convention is designed for simple
	  Type I censoring scenarios.}
	\item{\bold{three columns}}{Like the two column case, the first column has
	  the group
	  levels to define the
	  factor, and the second column contains the response values, which will
	  all be coerced to numeric. Any censoring information must be specified
	  in the third column. Borrowing the convention of \code{\link{Surv}}
	  from the \pkg{survival} package, \code{0}=right censored, \code{1}=no censoring, and
	  \code{2}=left censored. If \code{rightcensor=NULL} and
	  \code{leftcensor=NULL} are left as defaults in the call, and
	  values of 0, 1, and 2 are all represented, then the
	  processing will create a suitable data frame \code{dfru} for
	  modeling that the canonical \code{\link{survreg}} function understands.

	  However, if 0 and 1 are the only specified values
	  in the third censoring status column, then one of
	  \code{rightcensor=TRUE} or \code{leftcensor=TRUE} must be specified,
	  but NOT both, or an error message will occur. A column of all 1's or
	  all 0's will also raise an error message.}
	\item{\bold{four columns}}{Like the two column case, the first column has
	  the group
	  levels to define the
	  factor. The second and third columns need to have numeric response
	  information, and the fourth column needs to have censoring
	  status. This is the most general representation, where any combination
	  of left-censoring, right-censoring, and interval-censoring is
	  permitted. The \code{rightcensor} and \code{leftcensor} input
	  arguments are ignored and set to \code{NULL}. \bold{IMPORTANT:} The
	  convention of \code{\link{Surv}}
	  from the \pkg{survival} package, 0=right censored, 1=no censoring, and
	  2=left censored, 3=interval censored, and
	  \code{type="interval"},
	  is followed. For status=0, 1, and 2, the second and
	  third columns match in value, so that the status variable in the
	  fourth column distinguishes the lower and upper bounds for the
	  right-censored (0) and left-censored (2) cases.
	  For status=3, the two values differ to
	  define the interval boundaries. The 
	  processing will create a suitable data frame \code{dfru} for
	  modeling that the canonical \code{survreg} and \code{survfit}
	  functions from the \pkg{survival} package understand.
	}
      }
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
      If \code{addconstant="simple"} or  
      \code{addconstant="VR"} is specified, a number is derived and added
      to all response values.
      
      \describe{
	\item{\code{"simple"}}{Taken from the "white" book on S (Chambers and Hastie, 1992), 
	  page 68. The range (\code{max - min}) of the response values
	  is multiplied by \code{0.0001} to derive the number to add to all the
	  response values.
	} 
	\item{\code{"VR"}}{Based on the \code{\link{logtrans}} function discussed in Venables and Ripley 
	  (2002), pages 171-172 and available in the \pkg{MASS}
	  package. The algorithm applies a Box-Cox profile likelihood
	  approach with a log scale translation model. 
	}
      }
    }
  }
}
\value{
  A \code{cgOneFactorData} object is returned, with the following slots:
  \item{dfr }{The original input data frame that is the specified value of the
    \code{dfr} argument in the function call.}
  \item{dfru }{Processed version of the input data frame, which will be
    used for the various evaluation methods.}
  \item{fmt.dfru }{A list version of the input data frame, which will only
    differ from the \code{dfr} value if the input data frame was specified in the
    \code{groupcolumns} format.}
  \item{has.censored }{Boolean \code{TRUE} or \code{FALSE} on whether there are any
    censored data observations.}
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
	was \code{NULL}. Otherwise has the specified numeric value.}
      \item{\code{rightcensor}}{Has the value of the input argument
	\code{rightcensor} or is set to \code{NULL} if no censored
	observations are determined.}
      \item{\code{leftcensor}}{Has the value of the input argument
	\code{leftcensor} or
	is set to \code{NULL} if no censored
	observations are determined.}
      \item{\code{digits}}{Has the value of the input argument
	\code{digits} or is set to the determined value of digits from the
	input data. Will be an integer of 0, 1, 2, 3, or 4.}
      \item{\code{grpnames}}{Determined from the single factor identified of the
	group names. The order is determined by their first occurence in the
	input data frame \code{dfr}.}
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
  \emph{Statistical Modeling in S.}  Chapman & Hall/CRC.

  Venables, W. N., and Ripley, B. D. (2002), 
  \emph{Modern Applied Statistics with S.}  Fourth edition. Springer.
}
\author{
  Bill Pikounis, John Oleynick, Eva Ye 
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments.
}
\seealso{
  \code{\link{Surv}}, \code{\link{canine}},
  \code{\link{gmcsfcens}},
  \code{\link{prepare}}
}
\examples{
data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")

## Censored Data
data(gmcsfcens)
gmcsfcens.data <- prepareCGOneFactorData(gmcsfcens, format="groupcolumns",
                                         analysisname="cytokine",
                                         endptname="GM-CSF (pg/ml)",
                                         logscale=TRUE)

}
\keyword{datagen}
