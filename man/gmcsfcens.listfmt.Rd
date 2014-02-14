\name{gmcsfcens.listfmt}
\alias{gmcsfcens.listfmt}
\alias{gmcsfcens.listfmt1}
\alias{gmcsfcens.listfmt2}
\alias{gmcsfcens.listfmt3}
\docType{data}
\title{GM-CSF censored data set in the cg package
}
\description{
  A data frame used to illustrate the \pkg{cg} package. It has a One Factor / One-Way /
  Unpaired Samples layout. It also contains left-censored values of
  varying degree in each of the six groups. There are three equivalent
  data frame versions documented here.
}
\usage{data(gmcsfcens.listfmt1)
       data(gmcsfcens.listfmt2)
       data(gmcsfcens.listfmt3)}
\format{
  A 45 row data frame with up to 8 observations per group from an
  experiment on the following 6 groups. 
  
  \describe{
    \item{\code{PBS/Tg 197 }}{phosphate buffered saline control group}
    \item{\code{1mg/kg/Tg 197 }}{1 mg/kg dose}
    \item{\code{3mg/kg/Tg 197} }{3 mg/kg dose}
    \item{\code{10/mg/kg/Tg 197 }}{10 mg/kg dose}
    \item{\code{30/mg/kg/Tg 197 }}{30 mg/kg dose}
    \item{\code{PBS/WT } }{phosphate buffered saline control group of wild-type mice}
  }
  The first five groups have transgenic (Tg197) mice subjects, a well
  established model to induce arthritis. The sixth group are "wild-type"
  mice that did not have
  arthritis induced. The various doses of the inner four groups are
  administrations of golimumab, a monoclonal antibody therapy.

  There can be either 2, 3, or 4 columns in the data frame.
  The above 6 items are the levels of the first column's factor, named
  \code{grp}. 
  
  \describe{
    \item{\code{2 columns }}{The data frame name is
      \code{gmcsfcens.listfmt1}. The second column \code{endpt}
      contains the character observations that can represent complete
      observations, and also left- or right-censored ones, in the same
      way that \code{\link{gmcsfcens}} does.}
    \item{\code{3 columns }}{The data frame name is
      \code{gmcsfcens.listfmt2}. The second column \code{endpt}
      contains numeric observations, and the third column \code{status}
      indicates whether the observation is complete/not censored (1),
      and 0 if left-censored. See \code{\link{prepareCGOneFactorData}}
      for the explanation of why the value of 0 and
      not 2 is required. In the example code below, the
      \code{leftcensor=TRUE} argument needs to be specified when this
      format version is used.}
    \item{\code{4 columns} }{The data frame name is
      \code{gmcsfcens.listfmt3}. The second and third columns
      \code{endpt1} and \code{endpt2}
      contain numeric observations, and the fourth column \code{status}
      indicates whether the observation is complete/not censored (1),
      and 2 if left-censored. See \code{\link{prepareCGOneFactorData}}
      for the explanation of this format, and the example code below.}
  }
}
\details{The \code{gmcsfcens.listfmt*} data sets that comes with the \pkg{cg} package
  are in a "listed" format, detailed below.

  The 6 groups are regarded as levels of one factor in the
  \code{\link{prepareCGOneFactorData}}, \code{\link{fit}}, and other methods in
  the \pkg{cg} package.

  The \code{gmcsfcens.listfmt} data sets are alternative formats of the
  \code{\link{gmcsfcens}} data set. See that help file for details. Once
  a \code{gmcsfcens.listfmt} data set is \code{\link{prepare}}d into a
  \code{\link{cgOneFactorData}} object, all the subsequent methods work
  on the object in the same way.
  
  GM-CSF stands for Granulocyte macrophage colony stimulating factor, a
  type of cytokine that is important in the growth of white blood cells.
  It is one of the outcomes measured in the experiment described in the
  references section below. Therapeutic inhibition of it may be
  beneficial in cases where too many white blood cells are produced,
  such as arthritis. In other situations where white blood cell counts
  are low, stimulation of it is desired. In the referenced study below, GM-CSF
  is evaluated in the context of inflammation.
}
\seealso{
 \code{\link{gmcsfcens}}, \code{\link{prepareCGOneFactorData}}
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments.
}
\references{
  Shealy, D., Cai, A., Staquet, K., Baker, A., Lacy, E., Johns, L., Vafa, O., Gunn, G., Tam, S., Sague, S., Wang, D., Brigham-Burke, M., Dalmonte, P., Emmell, E.,  Pikounis, B., Bugelski, P., Zhou, H., Scallon, B.,  Giles-Komar, J. (2010). "Characterization of Golimumab (CNTO148), a human monoclonal antibody specific for human tumor necrosis factor  ", \emph{mAbs},  Volume 2, Issue 4, 428-439.
}
\examples{
data(gmcsfcens.listfmt1)
str(gmcsfcens.listfmt1)

data(gmcsfcens.listfmt2)
str(gmcsfcens.listfmt2)

data(gmcsfcens.listfmt3)
str(gmcsfcens.listfmt3)

## Analogous to prepareCGOneFactorData call on gmcsfcens data frame format,
##  subsequent methods will work for gmcsfcens.listfmt.data objects below:

## leftcensor argument can be left as default NULL
gmcsfcens.listfmt1.data <- prepareCGOneFactorData(gmcsfcens.listfmt1, format="listed",
                                                  analysisname="cytokine",
                                                  endptname="GM-CSF (pg/ml)",
                                                  logscale=TRUE)

## leftcensor=TRUE argument needs to be set
gmcsfcens.listfmt2.data <- prepareCGOneFactorData(gmcsfcens.listfmt2, format="listed",
                                                  analysisname="cytokine",
                                                  endptname="GM-CSF (pg/ml)",
                                                  logscale=TRUE,
                                                  leftcensor=TRUE)

## leftcensor argument can be left as default NULL
gmcsfcens.listfmt3.data <- prepareCGOneFactorData(gmcsfcens.listfmt3, format="listed",
                                                  analysisname="cytokine",
                                                  endptname="GM-CSF (pg/ml)",
                                                  logscale=TRUE)
## as they do on gmcsfcens.data:

gmcsfcens.data <- prepareCGOneFactorData(gmcsfcens, format="groupcolumns",
                                         analysisname="cytokine",
                                         endptname="GM-CSF (pg/ml)",
                                         logscale=TRUE)

}
\keyword{datasets}


