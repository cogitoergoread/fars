#' Reads the data from a file
#'
#' This is a simple function that, reads the data from a file specified by the
#'  \code{filename} argument. The functions stop in case of no files found.
#'  The files should contain a CSV formatted data. The data read is returned
#'  as a data frame.
#'
#' @param filename The name of the file to process. The file should exist and should
#' be a CSV data file.
#'
#' @return This function returns a data frame containing the data of the file processed.
#'
#' @examples
#' \dontrun{
#' fars_read(filename="accident_2014.csv.bz2")
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Create a specific filename for the year
#'
#' This function creates a valid filename based on the \code{year} argument.
#' The filename is useful for processing the fars_data.zip dataset.
#' The data of a given year is in a separate file in fars_data.zip .
#'
#' @param year The integer value of a year. The \code{year} should be a positive decimal.
#' In order to use with the fars_data.zip dataset, the year should be in the range
#' 2013..2015
#'
#' @return This function returns string, containing a file name of one datafile in the
#' fars_data.zip dataset.
#'
#' @examples
#' make_filename(2014)
#'
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Reads the data from a file for some years
#'
#' This function tries to collect the data for one or more years from the datafile.
#'  \code{years} argument should be a list of the required years. If no data found for a
#'  specific year, a warning message is displayed and the element in the output listis NULL.
#'
#' @param years List or vector of the years to be extracted from the datafile.
#'
#' @return This function returns a list of data frames containing the data of the file processed by years.
#' Each data frame has two columns for the month and year of a sample in a data file.
#'
#' @examples
#' \dontrun{
#' fars_read(years=c(2014,2015))
#' }
#'
#' @importFrom dplyr mutate select
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

#' Summarise the data for the list of years
#'
#' This function create a summary for some years from the input files.
#' The function uses \code{fars_read_years()} to collect the data.
#' The summarized data is a non-tidy table of the number of accidents for a give month and year.
#'
#' @inheritParams fars_read_years
#'
#' @return This function returns a data frame. The data frame has 12 rows, each month has a row.
#' The data frame has one column for the month plus each year from the  \code{years} argument
#' has its own column.  specific element in a row for a given year contains the number of
#' accidents from the sample data.
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(years=c(2014,2015))
#' }
#'
#' @export
#' @importFrom dplyr  bind_rows group_by summarize
#' @importFrom tidyr spread
#' @importFrom magrittr "%>%"
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Plots a map of the accidents for a US state of a year
#'
#' This function plots a map of a US state and displays the accidents in that state
#' for a year.
#'
#' @param state.num The integer code of a US state
#' @inheritParams make_filename
#'
#' @return This function returns NULL
#'
#' @examples
#' \dontrun{
#' fars_map_state(state.num=1,year=2015)
#' }
#'
#' @export
#' @importFrom maps  map
#' @importFrom graphics points
#' @importFrom dplyr filter
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
