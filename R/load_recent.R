#'
#' load the most recent .rdata file in a given directory
#'
#' I typically have multiple .rdata files for a given project due to updates in the data over time. Each of these .rdata files is dated and this function will load the most recent (via sorting)
#'
#' @param dir_path path to the directory containing .rdata files, of which you want the most recent
#' @export load_recent

load_recent <- function(dir_path) {

  load(
    print(
      sort(
        list.files(
          dir_path,
          recursive = FALSE,
          full.names = TRUE
        ),
        decreasing = TRUE
      )[1]
    ),
    envir = .GlobalEnv
  )

}

