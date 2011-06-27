## $Id: p01FitOneFactorData.R 180 2010-12-20 19:23:26Z user $
## One-Factor Unpaired Groups Case

## Fit One-Factor Unpaired Groups Data

setClass("cgOneFactorFit",
         representation(olsfit="olsfit",
                        rrfit="rrfit",
                        aftfit="aftfit",
                        uvfit="uvfit",
                        settings="list"))

setMethod("fit", "cgOneFactorData",
          fit.cgOneFactorData <- 
          function(data, type="rr", ...) {
            ##
            ## PURPOSE: Fit data that has a one-factor unpaired groups
            ## structure. Standard Least
            ## Squares Linear Model always fit, with a Resistant & Robust
            ## option based on M- and S-estimation. Also can fit
            ## an accelerated failure time model with lognormal or normal
            ## distribution on censored observations, or an unequal group
            ## variances structure.
            ## 
            ## Input arguments handling
            dots <- list(...)
            validDotsArgs(dots, names=c("maxIter","sandaft"))
            
            ## initializations ('ols' is always TRUE so not specified)
            rr <- aft <- uv <- FALSE
            aftfit <- rrfit <- uvfit <- "No fit was requested."

            if(data@has.censored) { type <- "aft" }
            else if(missing(type)) { type <- "rr" }

            type <- validFitType(type)

            maxIter <- if(is.null(dots$maxIter)) 100 else dots$maxIter 
            validNumeric(maxIter, positive=TRUE, integer=TRUE)

            if(type=="rr") { rr <- TRUE }
            else if(type=="aft") { aft <- TRUE }
            else if(type=="uv") { uv <- TRUE }
            ## End input arguments handling
            
            dfru <- data@dfru
            settings <- data@settings
            
            endptscale <- settings$endptscale
            grpnames <- settings$grpnames

            oldop <- options(contrasts=c("contr.treatment", "contr.poly"))
            on.exit(oldop, add=TRUE)

            validAft(type, dfru)
            
            ## Ordinary Least Squares is *always* fit
            olsfit <- if(endptscale=="log") {
              lm(log(endpt) ~ -1 + grpf, data=dfru)
            }
            else {
              lm(endpt ~ -1 + grpf, data=dfru)
            }
            olsfit$dfru <- dfru
            names(olsfit$coef) <- grpnames

            if(rr) {
              rrfit <-
                if(endptscale=="log") {
                  try(rlm(log(endpt) ~ -1 + grpf, data=dfru, method="MM",
                          maxit=maxIter, ...))
                }
                else {
                  try(rlm(endpt ~ -1 + grpf, data=dfru, method="MM",
                          maxit=maxIter, ...))
                }
              if(class(rrfit)[1]!="try-error") {
                ## that is, the number of iterations
                ## did not exceed specified limit maxIter
                ## and thus convergence occurred
                rrfit$dfru <- dfru
                names(rrfit$coef) <- grpnames

                ## workaround so return object instance below will
                ## accept rrfit in the slot. Has something to do with
                ## setClassUnion("rrfit", c("character", "rlm", "lm")) idiom I use.
                ## see https://stat.ethz.ch/pipermail/r-devel/2008-April/049278.html
                class(rrfit) <- "rlm"
              }
              else {
                warning(cgMessage("The Resistant & Robust (rr) fit did not",
                                  "converge in the specified number of",
                                  "iterations. You may want to try again with an",
                                  "increased value for the maxIter argument.",
                                  warning=TRUE))
                rrfit <- "The fit did not converge in the specified number of iterations."
              }
            }
            else if(aft) {
              if(!is.null(sandaft <- dots$sandaft)) {
                validBoolean(sandaft)
              }
              else {
                sandaft <- TRUE
              }

              thesurvobject <- with(dfru,
                                    if(endptscale=="log") {
                                      survival::Surv(time=log(endpt1), time2=log(endpt2),
                                                     event=status, type="interval")
                                    }
                                    else {
                                      survival::Surv(time=endpt1, time2=endpt2,
                                                     event=status, type="interval")
                                    })
              aftfit <-  try(survreg(thesurvobject ~ -1 + grpf,
                                     data=dfru, dist="gaussian", maxiter=maxIter))

              
              if(class(aftfit)[1]!="try-error") {
                ## that is, the number of iterations
                ## did not exceed specified limit maxIter
                ## and thus convergence occurred
                aftfit$dfru <- dfru
                aftfit$Surv <- thesurvobject
                aftfit$maxIter <- maxIter 
                names(aftfit$coef) <- grpnames
                aftfit$sandaft <- sandaft
                if(sandaft) {
                  ## Manually compute the sandwich estimate since robust=TRUE
                  ## fails for survreg when cluster is omitted. The actual bug
                  ## appears with the application of rowsum, which is not
                  ## really needed since we assume all subjects are
                  ## independent (i.e. no clusters).
                  aftfit$naive.var <- aftfit$var
                  aftfit$var <- crossprod(resid(aftfit, "dfbeta"))
                }
              }
              else {
                warning(cgMessage("The Accelerated Failure Time (aft) fit did not",
                                  "converge in the specified number of",
                                  "iterations. You may want to try again with an",
                                  "increased value for the maxIter argument.",
                                  warning=TRUE))
                aftfit <- paste("The AFT fit did not converge in the specified",
                                "number of iterations.")
              }
            }
            else if(uv) {
              uvfit <- if(endptscale=="log") {
                gls(log(endpt) ~ -1 + grpf, data=dfru,
                    weights=varIdent(form = ~ 1 | grpf))
              }
              else {
                gls(endpt ~ -1 + grpf, data=dfru,
                    weights=varIdent(form = ~ 1 | grpf))
              }
              uvfit$dfru <- dfru
              names(uvfit$coef) <- grpnames
            }

            returnObj <- new("cgOneFactorFit",
                             olsfit=olsfit,
                             rrfit=rrfit,
                             aftfit=aftfit,
                             uvfit=uvfit,
                             settings=settings)
            
            returnObj
          })

validAft <- function(type, dfru) {
  if(type=="aft" & ncol(dfru)!=5) {
    stop(cgMessage("An accelerated failure time (AFT) model",
                   "cannot be fit as requested (type=\"aft\")",
                   "since the data frame does not seem to have",
                   "a censored status column or the required format.",
                   seeHelpFile("CGOneFactorFit")))
  }
  return(TRUE)
}

validFitType <- function(type) {
  x <- try(match.arg(type, c("ols","rr","aft","uv")))
  if(class(x)=="try-error") {
    stop(cgMessage("The type argument needs to evaluate to one",
                   "of: \"ols\", \"rr\", \"aft\", \"uv\".",
                   seeHelpFile("CGOneFactorFit")))
  }
  else return(x)
}

