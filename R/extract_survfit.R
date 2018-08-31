#' Extract data from survfit object
#'
#' This function extracts the individual estimate information from a survfit
#' object. This is mainly useful when plotting the survival function
#'
#' @param survfit_obj an object returned from the survival::survfit() function
#' @return a data frame containing the estimate of the survival function
#' @author Patrick Hilden
#' @examples
#'
#' df <- data.frame(
#'   fail_time = 12 * rexp(100),
#'   group = paste0('group_', sample(1:3, 100, replace = TRUE)),
#'   fail_status = sample(0:1, 100, replace = TRUE)
#' )
#'
#' extract_survfit(
#'   survival::survfit(
#'     survival::Surv(fail_time, fail_status) ~ group,
#'     data = df
#'   )
#' )
#'
#' @export extract_survfit


extract_survfit <- function(survfit_obj){
  f.frame <- NULL
  if(length(names(survfit_obj$strata)) == 0){
    f.frame <- data.frame(time=survfit_obj$time
                          ,n.risk=survfit_obj$n.risk
                          ,n.event=survfit_obj$n.event
                          ,n.censor = survfit_obj$n.censor
                          ,surv=survfit_obj$surv
                          ,upper=survfit_obj$upper
                          ,lower=survfit_obj$lower)

    # clean up initial start time for plotting
    f.start <- data.frame(time=c(0, f.frame$time[1])
                          ,n.risk=c(survfit_obj$n, survfit_obj$n)
                          ,n.event=c(0,0),n.censor=c(0,0)
                          ,surv=c(1,1)
                          ,upper=c(1,1)
                          ,lower=c(1,1))
    f.frame <- rbind(f.start, f.frame)
    rm(f.start)
  }
  else {
    f.strata <- NULL
    for(f.i in 1:length(survfit_obj$strata)){
      # create a vector of strata names repeated
      f.strata <- c(f.strata,rep(names(survfit_obj$strata)[f.i], survfit_obj$strata[f.i]))
    }
    f.frame <- data.frame(time=survfit_obj$time
                          ,n.risk=survfit_obj$n.risk
                          ,n.event=survfit_obj$n.event
                          ,n.censor = survfit_obj$n.censor
                          ,surv=survfit_obj$surv
                          ,upper=survfit_obj$upper
                          ,lower=survfit_obj$lower
                          ,strata=f.strata
                          ,stringsAsFactors=F)
    rm(f.strata)
    for(f.i in 1:length(survfit_obj$strata)){
      f.subset <- subset(f.frame, strata==names(survfit_obj$strata)[f.i])
      f.start <- data.frame(time=c(0, f.subset$time[1])
                            ,n.risk=rep(survfit_obj[f.i]$n, 2)
                            ,n.event=c(0,0)
                            ,n.censor=c(0,0)
                            ,surv=c(1,1)
                            ,upper=c(1,1)
                            ,lower=c(1,1)
                            ,strata=rep(names(survfit_obj$strata)[f.i],2)
                            ,stringsAsFactors=F)
      f.frame <- rbind(f.start, f.frame)
      rm(f.start, f.subset)
    }
    f.frame <- f.frame[order(f.frame$strata, f.frame$time), ]
    rownames(f.frame) <- NULL

    f.frame$group<-substr(f.frame$strata
                          ,regexpr("=",f.frame$strata)+1
                          ,nchar(f.frame$strata))
    f.frame$strata<-NULL

  }
  return(dplyr::as_data_frame(f.frame))
}
