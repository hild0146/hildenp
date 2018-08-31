#' Extract data from cuminc object
#'
#' This function extracts the individual estimate information from a cuminc
#' object. This is mainly useful when plotting the cumulative incidence
#' function
#'
#' @param cuminc_obj an object returned from the cmprsk::cuminc() function
#' @return a data frame containing the cumulative incidence estimates
#' @author Patrick Hilden
#' @examples
#'
#' df <-
#'   data.frame(
#'     fail_time = 12 * rexp(100),
#'     group = paste0('group_', sample(c('a','b','c'), 100, replace = TRUE)),
#'     fail_status = sample(0:2, 100, replace = TRUE)
#'   )
#'
#' extract_cuminc(
#'   cmprsk::cuminc(
#'       df$fail_time,
#'       df$fail_status,
#'       df$group
#'     )
#' )
#'
#' @export extract_cuminc


extract_cuminc <- function(cuminc_obj){
  f.frame <- NULL
  # if groups are present
  if(!is.null(cuminc_obj$Tests)){
    f.frame <- NULL
    for(i in 1:(length(names(cuminc_obj))-1)){
      f.frame.temp <- data.frame(time=cuminc_obj[[i]]$time
                                 ,est=cuminc_obj[[i]]$est
                                 ,event=names(cuminc_obj)[i]
                                 ,stringsAsFactors=F)
      f.frame <- rbind(f.frame, f.frame.temp)
    }
    f.frame$group<-substr(f.frame$event,1,nchar(as.character(f.frame$event))-2)
  }
  else {
    f.frame<-NULL
    for(i in 1:length(objects(cuminc_obj))){
      f.frame.temp <- data.frame(time=cuminc_obj[[i]]$time
                                 ,est=cuminc_obj[[i]]$est
                                 ,event = i)
      f.frame <- rbind(f.frame, f.frame.temp)
    }
  }

  f.frame$event<-substr(f.frame$event,nchar(as.character(f.frame$event)),nchar(as.character(f.frame$event)))

  return(dplyr::as_data_frame(f.frame))
}
