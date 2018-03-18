library(maps)

#' Read a given file into R
#'
#' This is a function that reads a given filename, given by the \code{filename}
#'	argument into R using the \code{read_csv} function from the readr 
#'	packages and then returning the read in table as a \code{tbl_df} 
#'	object from the dplyr package.
#'
#' @param filename A character string giving the name of the file to read into
#'	the session
#'
#' @return This function returns a tbl_df object from the dplyr package
#'
#' @note Calling this function with a filename that does not exist in the working
#'	director will result in an error. This function is called by the make_filename 
#'	function and usually not called on its own.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \donotrun{fars_read('accident_2013.csv.bz2')}
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Format a valid filename given a year
#'
#' This is a function that reads a given filename, given by the \code{filename}
#'	argument into R using the \code{read_csv} function from the readr 
#'	packages and then returning the read in table as a \code{tbl_df} 
#'	object from the dplyr package.
#'
#' @param year A character string or integer giving the year to use to format the filename. This parameter
#'	will be coerced to an integer in the function. 
#'
#' @return This function returns a character string of a filename corresponding to the given year
#'
#' @note This function is called by the fars_read_years function and not usually called on its
#'	own. Hence not exported.
#'
#' @examples
#' make_filename(2013)
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read a series of accident files for given years
#'
#' This is a function that reads a accident files for a vector of 
#	given years and transforms each read in data frame into 
#'	a \code{dplyr::tbl_df} with columns for the months and 
#'	given year of accidents in that year.
#'
#' @param years A vector of years for which to summarize accident months
#'
#' @return This function returns a list of tbl_df object for each year that each
#'	contain the months and year of accidents for that year
#'
#' @note Calling this function with for a year that doesn't have an accident file
#'	results in an "invalid year" warning for that year. This function is
#'	typically called by the \code{fars_summarize_years()} function and not
#'	run on its own. Hence not exported.
#'
#' @importFrom dplyr %>% mutate select
#'
#' @examples
#' \dontrun{fars_read_years(c(2013,2014))}
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

#' Summarize counts of a accidents by month for a set of years
#'
#' This is a function that summarizes counts of the number of 
#'	accidents per month for a given set of years from 
#'	accident data files. 
#'
#' @param years A vector of years for which to summarize accident months
#'
#' @return This function returns a dplyr::tbl_df object for with a column for
#'	each provided year and a row for each month. The values in the data frame
#'	are counts of accidents for the corresponding month and year.
#'
#' @importFrom dplyr %>% bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @examples
#' \dontrun{fars_summarize_years(c(2013,2014)}
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}


#' Create a map of accidents in a given state in a given year
#'
#' This is a function that read an accident files for a given year and
#'	plots a map showing accident locations for a given state in that
#'	year.
#'
#' @param year A character string or integer indicating a year
#' @param state.num An integer corresponding to a state number in the maps package
#'
#' @return This function produced a plot, if there are accidents to plot in that 
#'	state in that year
#'
#' @note If there are no accidents to plot in that state in that year, the function
#'	issues a warning. If the state number does not correspond to a state in the 
#'	accident data file the function produces an error.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{fars_map_state(12,2014)}
#' 
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
