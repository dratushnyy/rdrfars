#' Reads FARS (Fatality Analysis Reporting System) data from cvs file.
#'
#' @import readr
#' @import dplyr
#'
#' @param filename A string. Path to cvs data file.
#'
#' @section Warning:
#' Function will stop if provided file not found
#'
#' @return Loaded FARS data as \code{\link{dplyr::tbl_df}}.
#'
#' @examples
#' fars_read("my_data.csv.bz2")
#' fars_read("my_data.csv")
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Genarates archive file name based on year value
#'
#' @param year A number.
#'
#' @return generated archive file name
#'
#' @examples
#' make_filename("2011")
#' make_filename(2011)
#'
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read FARS (Fatality Analysis Reporting System) data for years
#'
#' @import dplyr
#'
#' @param years A vector of years values (numbers)
#'
#' @section Warning:
#' Function will skip year if file with data for year does not exists
#'
#' @return readed FARS data as \code{\link{dplyr::tbl_df}}.
#'
#' @examples
#' fars_read_years(c(2001:2011))
#' fars_read_years(c(2001,2002, 2004))
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Summarize accidents from FARS data by year and month
#'
#' @import dplyr
#'
#' @param years A vector of years values (numbers) to summarize
#'
#' @section Warning:
#' Function will skip year if file with data for year does not exists
#'
#' @return Summarized informaion about accidents as \code{\link{dplyr::tbl_df}}.
#'
#' @examples
#' fars_summarize_years(c(2001:2011))
#' fars_summarize_years(c("2004, 2008"))
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}
