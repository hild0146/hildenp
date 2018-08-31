#' Identify differences in two data frames
#'
#' This function compares two data frames (including NA values) and returns the differing rows
#' and columns which both data frames have in common
#'
#' @param df_1 first data frame to compare
#' @param df_2 second data frame to compare
#' @param by_vars a single or combination of variables for which the input data frames are unique
#' @return a list whose individual members are data frames, one for each column containing a
#' difference
#' @examples
#'
#' df_1 <-
#'   data.frame(
#'     pt_id = sort(rep(1:10, 3)),
#'     vst_num= rep(1:3, 10),
#'     vals_1 = rbinom(30,10,0.5),
#'     vals_2 = runif(30),
#'     vals_3 = rnorm(30)
#'   )
#'
#'df_2 <- df_1[ -8, ]
#'
#'df_2[ c(3, 12), c('vals_1', 'vals_2', 'vals_3')] <- sample(c(NA, 99), 6, replace = T)
#'
#' df_compare(
#'   df_1 = df_1,
#'   df_2 =  df_2,
#'   by_vars = c('pt_id', 'vst_num')
#' )
#' @export df_compare


df_compare <- function(df_1,
                       df_2,
                       by_vars){


  prm <- as.list(match.call())


  # check to see if the data frames are unique by by_vars
  if(any(duplicated(df_1[, by_vars])) | any(duplicated(df_2[, by_vars]))){
    stop('data frames are not unique according to by_vars')
  }


  # identify, select & sort by common columns/rows
  cols <- intersect(names(df_1), names(df_2))

  # identify columns not in common
  miss_cols1 <- setdiff(names(df_1), cols)
  miss_cols2 <- setdiff(names(df_2), cols)

  miss_cols <- data.frame(
    source = c(rep(as.character(prm$df_1), length(miss_cols2)),
               rep(as.character(prm$df_2), length(miss_cols1))),
    missing = c(miss_cols2, miss_cols1)
  )

  cols <- cols[order(cols)]
  cols <- cols[!cols %in% by_vars]

  df_1 <- merge(df_1[ ,c(by_vars, cols)],
               df_2[, by_vars, drop = F],
               by = by_vars)
  df_1 <- df_1[do.call('order', df_1[ ,by_vars, drop = F]), ]

  df_2 <- merge(df_2[ ,c(by_vars, cols)],
               df_1[, by_vars, drop = F],
               by = by_vars)
  df_2 <- df_2[do.call('order', df_2[ ,by_vars, drop = F]), ]


  # print the number of common rows
  print(paste0(nrow(df_2), ' rows in common'))

  if(nrow(miss_cols) > 0){
    print('non-common columns:')

    print(miss_cols)
  }



  for(col_i in cols){
    if(col_i == cols[1]) {
      out <- NULL
      out_i <- 0
    }

    # identify if there are any differences in given column, output if different
    diff_rows <- which(df_1[ ,col_i] != df_2[ ,col_i] |
                         is.na(df_1[ ,col_i]) != is.na(df_2[ ,col_i]))

    if(length(diff_rows) > 0){

      out_i <- out_i + 1

      out[[out_i]] <-
        dplyr::as_data_frame(
          merge(df_1[diff_rows, c(by_vars, col_i)],
                df_2[diff_rows, c(by_vars, col_i)],
                by = by_vars)
        )


      # rename column output to correspond with funciton input values
      names(out[[out_i]]) <- c(by_vars,
                               as.character(prm$df_1),
                               as.character(prm$df_2))

      names(out)[out_i] <- col_i
    }

  }

  if(is.null(out)){

    print('data frames are identical within common rows & columns')

    } else return(out)

  }
