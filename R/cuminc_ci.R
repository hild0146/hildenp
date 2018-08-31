#' Calculate cumulative incidence & confidence interval
#'
#' This function can be used to calculated cumulative incidence estimates for
#' survival data with competing risks. Confidence intervals can be specified
#' for a given level.
#'
#' @param ftime failure time variable
#' @param fstatus variable with distinct codes for different causes of failure
#'  and also a distinct code for censored observations
#' @param group estimates will be calculated within groups given by distinct values
#'  of this variable. Tests will compare these groups. If missing then treated
#'   as all one group (no test statistics)
#' @param times timepoint(s) for which cumulative incidence estimates and confidence
#'  intervals are desired
#' @param print_event identifies which event codes should be presented in the
#'  results. "all" can be used to request all event types
#' @param print_var logical indicator for whether or not the variance should be
#'  listed in the results
#' @param strata stratification variable. Has no effect on estimates. Tests will
#'  be stratified on this variable. (all data in 1 stratum, if missing)
#' @param rho Power of the weight function used in the tests.
#' @param cencode value of fstatus variable which indicates the failure time
#' is censored.
#' @param subset a logical vector specifying a subset of cases to include in
#'  the analysis
#' @param na.action a function specifying the action to take for any cases
#'  missing any of ftime, fstatus, group, strata, or subset.
#' @param conf.level the confidence level for confidence intervals. default is
#'  0.95
#' @param digits number of decimal places that the results should be rounded to
#' @return returns a list containing
#'  \item{test}{resulting test statistics and p-values}
#'  \item{time1}{cumulative incidence estimates and ci's for the resquested time}
#'  \item{...}{...}
#'  \item{timeN}{estimates for final time point}
#' @references Scrucca L., Santucci A., Aversa F. (2007) Competing risks analysis using
#   R: an easy guide for clinicians. Bone Marrow Transplantation, 40, 381--387.
#' @examples
#'
#' df <-
#'   data.frame(
#'     fail_time = 12 * rexp(100),
#'     group = paste0('group_', sample(1:3, 100, replace = TRUE)) ,
#'     fail_status = sample(0:2, 100, replace = TRUE)
#'   )
#'
#' cuminc_ci(
#'   ftime = df$fail_time,
#'   fstatus = df$fail_status,
#'   group = df$group,
#'   times = 12
#' )
#' @export cuminc_ci

cuminc_ci<- function(ftime
                     ,fstatus
                     ,group
                     ,times = c(12,24)
                     ,print_event = 1
                     ,print_var = F
                     ,strata
                     ,rho = 0
                     ,cencode = 0
                     ,subset
                     ,na.action = na.omit
                     ,conf.level = 0.95
                     ,digits = 3
){

  fit <- cmprsk::cuminc(ftime,fstatus,group,strata,rho,cencode,subset,na.action)
  tfit <- cmprsk::timepoints(fit, times)

  z <- qnorm(1-(1-conf.level)/2)
  lower <- tfit$est ^ exp(-z*sqrt(tfit$var)/(tfit$est*log(tfit$est)))
  upper <- tfit$est ^ exp(z*sqrt(tfit$var)/(tfit$est*log(tfit$est)))

  results<-list()

  for(i in 1:length(times)){

    results[[i]]<-data.frame(
      group=substr(rownames(tfit$est),0,nchar(rownames(tfit$est))-2)
      ,event=as.numeric(substr(rownames(tfit$est)
                               ,nchar(rownames(tfit$est))
                               ,nchar(rownames(tfit$est))))
      ,estimate=round(tfit$est[,i],digits)
      ,variance=round(tfit$var[,i],digits)
      ,lower.ci=round(lower[,i],digits)
      ,upper.ci=round(upper[,i],digits)
      ,res.pretty=paste(formatC(round(tfit$est[,i],digits),digits,format="f")
                        ," ("
                        ,formatC(round(lower[,i],digits),digits,format="f")
                        ,","
                        ,formatC(round(upper[,i],digits),digits,format="f")
                        ,")"
                        ,sep="")
      ,stringsAsFactors=F
    )

    names(results)[i]<-paste("time",i,sep="")
    names(results[[i]])[5:7]<-c(paste("lower.",conf.level*100,sep="")
                                ,paste("upper.",conf.level*100,sep="")
                                ,"estimate (CI)")
    rownames(results[[i]])<-NULL

    if(print_var==F) results[[i]]$variance<-NULL
    if(missing(group)) results[[i]]$group<-NULL
  }

  if(!missing(group)){
    fit$Tests<-cbind(dimnames(fit$Tests)[[1]],round(data.frame(fit$Tests),digits))
    colnames(fit$Tests)<-c("event","statistic","p.value","df")
    results<-c(results,list(fit$Tests))
    names(results)[length(times)+1]<-"test"
  }

  # print results to console
  if(!missing(group)){
    cat("\nTest Results\n")
    if(print_event=="all") print(results$test,row.names=F)
    else print(results$test[results$test$event==print_event, ],row.names=F)
  }

  if(missing(group)) print.cols<-c("event","estimate (CI)")
  else print.cols<-c("group","event","estimate (CI)")

  for (j in 1:length(times)){
    cat(paste("\ntime :",round(sort(times)[j],digits),"\n"))
    if(print_event=="all") print(results[[j]][ ,print.cols],row.names=F)
    else print(results[[j]][results[[j]]$event==print_event,print.cols],row.names=F)
  }
  invisible(results)
}

