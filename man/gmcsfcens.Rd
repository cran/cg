\name{gmcsfcens}
\alias{gmcsfcens}
\docType{data}
\title{GM-CSF censored data set in the cg package
}
\description{
  A data frame used to illustrate the \pkg{cg} package. It has a One Factor / One-Way /
  Unpaired Samples layout. It also contains left-censored values of
  varying degree in each of the six groups.
}
\usage{data(gmcsfcens)}
\format{
  A 8-by-6 data frame with up to 8 numeric observations per group from an experiment on the following 6
  groups.
  
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

  The individual group values are of mode character, since some of them
  are represented as left-censored values such as \code{<82.5}. Note
  that two of the groups have less than 8 observations, and the
  corresponding cells in the data frame actually contain empty quote \code{""} values.
}
\details{The \code{gmcsfcens} data set that comes with the \pkg{cg} package
  is in \code{groupcolumns} format. Each column represents a group, and
  the observations in that group's column are the individual response
  values. As described above, they are character valued potentially
  left-censored representations.

  The 6 groups are regarded as levels of one factor in the
  \code{\link{prepareCGOneFactorData}}, \code{\link{fit}}, and other methods in
  the \pkg{cg} package.

  Alternative formats of this data set is contained in
  \code{\link{gmcsfcens.listfmt}}. See that help file for details,
  including how such formats would be read and \code{\link{prepare}}d by \pkg{cg}.

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
 \code{\link{gmcsfcens.listfmt}}, \code{\link{prepareCGOneFactorData}}
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments.
}
\references{
  Shealy, D., Cai, A., Staquet, K., Baker, A., Lacy, E., Johns, L., Vafa, O., Gunn, G., Tam, S., Sague, S., Wang, D., Brigham-Burke, M., Dalmonte, P., Emmell, E.,  Pikounis, B., Bugelski, P., Zhou, H., Scallon, B.,  Giles-Komar, J. (2010). "Characterization of Golimumab (CNTO148), a human monoclonal antibody specific for human tumor necrosis factor  ", \emph{mAbs},  Volume 2, Issue 4, 428-439.
}
\examples{
data(gmcsfcens)
str(gmcsfcens)
}
\keyword{datasets}
