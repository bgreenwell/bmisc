#' Load California housing data
#' 
#' Load California housing data from source.
#' 
#' @param raw Logical indicating whether or not to return the raw data or a 
#' cleaned up version. Default is \code{FALSE}.
#' 
#' @param tibble Logical indicating whether or not to return the results as a 
#' \code{\link[tibble]{tibble}}, as opposed to a data frame. Default is 
#' \code{FALSE}.
#' 
#' @source 
#' https://scikit-learn.org/stable/modules/generated/sklearn.datasets.fetch_california_housing.html
#' 
#' @export
#' 
#' @examples 
#' \dontrun{
#' cal <- fetch_california_housing(tibble = TRUE)
#' }
fetch_california_housing <- function(raw = FALSE, tibble = FALSE) {
  
  # Download and untar data in a temporary directory
  cdir <- system.file("extdata", "cal_housing.tgz", package = "bmisc")
  
  # Create a temporary directory to extract tar file
  tdir <- tempdir()
  untar(cdir, exdir = tdir)
  
  # Read the data into R and provide column names
  cal_housing <- read.csv(
    file = file.path(tdir, "CaliforniaHousing", "cal_housing.data"), 
    header = FALSE
  )
  
  # Delete temporary directory
  unlink(tdir, recursive = TRUE)
  
  # Clean up raw data a bit
  if (!isTRUE(raw)) {
    
    # Fix columns
    columns_index <- c(8, 7, 2, 3, 4, 5, 6, 1, 0) + 1
    cal_housing <- cal_housing[, columns_index]
    names(cal_housing) <- c("AvgValue", "MedInc", "HouseAge", "AveRooms", 
                            "AveBedrms", "Population", "AveOccup", "Latitude", 
                            "Longitude")
    
    # Transform/rescale the data
    cal_housing$AveRooms <- cal_housing$AveRooms / cal_housing$AveOccup
    cal_housing$AveBedrms <- cal_housing$AveBedrms / cal_housing$AveOccup
    cal_housing$AveOccup <- cal_housing$Population / cal_housing$AveOccup
    cal_housing$AvgValue <- cal_housing$AvgValue / 100000

  }

  # Return results as a data frame or tibble
  if (isTRUE(tibble)) {
    tibble::as_tibble(cal_housing)
  } else {
    cal_housing
  }
  
}