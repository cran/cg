## $Id: generics.R 2946 2012-03-02 18:37:07Z yye $

setGeneric("fit",
           function(data, type, ...) {
             standardGeneric("fit")
           })

setGeneric("pointGraph",
           function(data, ...) {
             standardGeneric("pointGraph")
           })

setGeneric("subjProfile",
           function(data) {
             standardGeneric("subjProfile")
           })

setGeneric("diffPlot",
           function(data) {
             standardGeneric("diffPlot")
           })

setGeneric("descriptiveTable",
           function(data, display="print", ...) {
             standardGeneric("descriptiveTable")
           })

setGeneric("correlationTable",
           function(data, display="print", ...) {
             standardGeneric("correlationTable")
           })

setGeneric("globalTest",
           function(fit, display="print", ...) {
             standardGeneric("globalTest")
           })

setGeneric("comparisonsTable",
           function(fit, 
                    mcadjust=FALSE, 
                    type="pairwisereflect", contrastmatrix=NULL,
                    refgrp=NULL, alpha=0.05, addpct=FALSE, display="print", ...) {
             standardGeneric("comparisonsTable")
           },
           )

setGeneric("grpSummaryTable",

           function(fit, mcadjust=FALSE, alpha=0.05, display="print", ...) {
             standardGeneric("grpSummaryTable")
           })

setGeneric("errorBarGraph",
           function(fit, mcadjust=FALSE,
             alpha=0.05, cgtheme=TRUE, device="single", ...) {
             standardGeneric("errorBarGraph")
           })

setGeneric("comparisonsGraph",
           function(compstable, cgtheme=TRUE, device="single",
                    wraplength=20, cex.comps=0.7, ...) {
             standardGeneric("comparisonsGraph")
           })

setGeneric("varianceGraph",
           function(fit, trend=NULL, cgtheme=TRUE,  device="single", ...) {
             standardGeneric("varianceGraph")
           })

setGeneric("qqGraph",
           function(fit, line=TRUE, cgtheme=TRUE,  device="single", ...) {
             standardGeneric("qqGraph")
           })

setGeneric("downweightedTable",
           function(fit, cutoffwt, display="print",  ...) {
             standardGeneric("downweightedTable")
           })

setGeneric("kmGraph",
           function(data, cgtheme=TRUE, distfcn="survival",
                    ylab=NULL, title=NULL, ...) {
             standardGeneric("kmGraph")
           })

setGeneric("samplesizeTable",
           function(fit, ngrps=2, direction, mmdvec, power=0.80, alpha=0.05,
                    nmax=1000, display="print", ...) {
             standardGeneric("samplesizeTable")
           })


setGeneric("samplesizeGraph",
           function(sstable, Nscale = "log", mmdscale = "log",
                    cgtheme=TRUE, device="single", ...) {
             standardGeneric("samplesizeGraph")
           })


setGeneric("showObj",
           function(object) {
             standardGeneric("showObj")
           })
                                           
