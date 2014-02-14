\name{cgInternalUtilities}
\alias{cgInternalUtilities}
\alias{stndErr} 
\alias{geoMean}
\alias{pctToRatio} 
\alias{makeZeroScore}
\alias{unwind}
\alias{unwrap}
\alias{graphStampCG} 
\alias{setupAxisTicks}
\alias{setupLog10AxisTicks}
\alias{tryAgain}
\alias{seeHelpFile}
\alias{paragraphWrap}
\alias{cgMessage}
\alias{factorInSeq}
\alias{setupGrpNameTicks}
\alias{xTicksCex}
\alias{yTicksCex}
\alias{rmTicks}
\alias{minmaxTicks}
\alias{plotGrpNameTicks}
\alias{boxplotStamp}
\alias{errorBarGraphStamp}
\alias{comparisonsGraphStamp}
\alias{errorBarGraphApproximateStamp}
\alias{trimWhiteSpace}
\alias{chopZeroes}
\alias{fmtRatioToPercent}
\alias{fmtDifference}
\alias{fmtRatio}
\alias{fmtPercent}
\alias{fmtPvalue} 
\alias{cgDevice}
\alias{contrastMatrix}
\alias{blockDiag}
\alias{rangeExtend}
\alias{getNumDigits}
\alias{makeCensored}
\alias{multcompInform}
\alias{multcompDone}
\alias{isAllEqual}
\alias{makeEndptLabel}
\alias{catCharExpr}
\alias{residualgrptrend.helper}
\alias{fround}
\alias{fround.charcens}
\alias{chop.matrix}
\alias{stripmiss}
\alias{makeTickMarks}
\alias{scaleVar}
\alias{makeContrastVec}
\alias{cg.largest.empty} 
\alias{qminmin}
\alias{unpaste}
\alias{grpsummary}
\alias{samplesize}
\alias{samplesizegraph}
\alias{boxplotcensoreddata}
\alias{descriptive.censoreddata}
\alias{pairwisecompsmatrix}
\alias{samplesize.default}

\title{ cg package Internal Utilities}
\description{
  \pkg{cg} package Internal Utility functions and objects not intended for user-level calls
}
\details{
  The functions
  
\code{stndErr
geoMean
pctToRatio 
makeZeroScore
unwind
unwrap
graphStampCG
setupAxisTicks
setupLog10AxisTicks
tryAgain
seeHelpFile
paragraphWrap
cgMessage
factorInSeq
setupGrpNameTicks
xTicksCex
yTicksCex
rmTicks
minmaxTicks
plotGrpNameTicks
boxplotStamp
errorBarGraphStamp}\cr
\code{comparisonsGraphStamp
errorBarGraphApproximateStamp
trimWhiteSpace
chopZeroes
fmtRatioToPercent
fmtDifference
fmtRatio
fmtPercent
fmtPvalue
cgDevice
contrastMatrix
blockDiag
rangeExtend
getNumDigits
makeCensored
multcompInform
multcompDone
isAllEqual
makeEndptLabel
catCharExpr}\cr
\code{residualgrptrend.helper}
\code{fround
fround.charcens
chop.matrix
stripmiss
makeTickMarks
scaleVar
makeContrastVec
cg.largest.empty
qminmin
unpaste
grpsummary
samplesize
samplesizegraph
boxplotcensoreddata
descriptive.censoreddata
pairwisecompsmatrix}

are used internally by the \pkg{cg} package. See source code for details.

The \code{blockDiag} function is adapted from a Ben Bolker
function contribution on R-help in 2002.

The \code{factorInSeq} function is exported since it may be useful for a
user. It is a simple wrapper around \code{\link{factor}}
with the order of its levels determined by first occurrence of each level in its
\code{x} vector argument.
}

\author{
  Bill Pikounis, John Oleynick, Eva Ye
}

\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments. 
}

