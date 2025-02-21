## c:/Dropbox/LandUnavailability/LandUnavailabilityR/R/raster_cleaning_functions.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2017-08-16

#' Check for a raster error
#'
#' @param file (string) a path to a tif object that will be read in by
#'     \code{raster}
#' @param plot.check (logical) If set to \code{TRUE}, the function
#'     will check if the raster can be plotted. This is a deeper check
#'     of raster functionality
#' @return if there is an error in reading or plotting the raster from
#'     \code{file}, the function will return \code{file}. Otherwise it
#'     will return \code{NULL}
#' @export
lu_check_raster_error <- function(file, plot.check = FALSE) {
    out <- try({
        test <- raster(file);
        if (plot.check == TRUE) plot(test)
    })
    if (inherits(out, "try-error")) {
        return(file)
    } else {
        return(invisible(NULL))
    }
}

#' Get a list of raster files
#'
#' @param raster.file.path (string) The path to the raster TIF
#'     files
#' @return A list where each element of the list contains a
#'     \code{raster} object for the each raster tif file
#' @export
lu_get_raster_list <- function(raster.file.path) {

    ##The raw RASTER files for the US
    raster.files <- list.files(raster.file.path, full.names = TRUE) %>%
        ##Make sure they end in .tif
        .[grepl(".tif$", .)]

    ##Read in the files
    raster.list <- lapply(raster.files, raster)

    return(raster.list)

}


#' The CRS of the raster files
#'
#' Note: This function will return the CRS for the first file in
#' \code{raster.file.path}
#'
#' @param raster.file.path (string) The path to the raster TIF
#'     files
#' @return (string) The CRS for the
#' @export
lu_get_raster_crs <- function(raster.file.path) {

    first.raster.file <- list.files(raster.file.path, full.names = TRUE)[1]

    crs.out <- raster(first.raster.file) %>%
        crs %>%
        as.character

    return(crs.out)

}

#' Calculate the slope unavailability from a raster object
#'
#' Note: this function will not check if the raster slope has been
#' calculated. See \url{https://rpubs.com/ajlyons/rspatialdata} for an
#' overview.
#'
#' Also note that degrees can be calculated from the slope (in
#' percent) using the inverse tangent. See
#' \url{https://www.archtoolbox.com/measurements/geometry/slope.html}
#'
#' @param raster.values the values from a \code{raster} object
#'     with the slope calculated from the \code{raster::slope}
#'     function
#' @param slope.degrees.cutoff (numeric) The cutoff for a steep slope
#'     in degrees. The default is \code{8.531} which corresponds to
#'     the 15 percent used by Saiz (2010).
#' @return (data.table) the percentage of land between 0 and 100 that
#'     is unavailable due to a steep slope
#' @export
lu_calc_slope_unavailable <- function(raster.values, slope.degrees.cutoff = 8.531) {

    ##Remove NA values
    raster.valus <- raster.values[!is.na(raster.values)]

    ##The percentage of land that is unavailable b/c
    ##the slope is above
    slope.unavailable <- raster.values
    ##Logical set to TRUE if slope greater than
    ##private$..slope.degrees.cutoff
    slope.unavailable <- slope.unavailable > slope.degrees.cutoff
    ##The mean (the percentage with a slope greater than
    ##private$..slope.degrees.cutoff)
    slope.unavailable <- mean(slope.unavailable, na.rm = TRUE)
    ##Multiply by 100
    slope.unavailable <- slope.unavailable * 100

    return(data.table("slope.unavailable" = slope.unavailable))
}

#' Calculate the percentage of water unavailability from a usgs raster
#' object
#'
#' Note, this object will rely on Land Cover Data and calculate
#' the percentage of land unavailable due to water and wetlands.  The
#' relevant USGS codes are as follows: water -- 11; wetlands 90 and 95
#'
#' @param raster.values The values from a \code{raster}
#' @return (data.table) the percentage of land between 0 and 100 that
#'     is unavailable due to a (1) water and (2) wetlands
#' @export
lu_calc_usgs_water_unavailable <- function(raster.values) {

    ##Remove 0s and missing values
    raster.values <- raster.values[!is.na(raster.values) & raster.values != 0]

    ##Water
    water <- raster.values[raster.values == 11] %>% length
    ##Wetlands
    wetlands <- raster.values[raster.values %in% c(90, 95)] %>% length
    raster.values.length <- length(raster.values)

    water.unavailable <- water / raster.values.length * 100
    wetlands.unavailable <- wetlands / raster.values.length * 100

    return(data.table("water.unavailable" = water.unavailable,
                      "wetlands.unavailable" = wetlands.unavailable))
}


#' Calculate the Total unavailability from a Slope and Water from a
#' \code{LandUnavailability} R6 instance
#'
#' @param LU1 An instance from the LandUnavailability R6 Class
#' @param LU2 An instance from the LandUnavailability R6 Class
#' @param polygon.name.col (string) the name of the column name with a
#'     polygon names
#' @return a \code{data.table} with the total land unavailability
#'     output
#' @export
lu_total_land_unavailability <- function(LU1, LU2, polygon.name.col = NULL) {

    sf.polygon1 <- LU1$sf.polygon; sf.polygon2 <- LU2$sf.polygon
    ##Make sure that the polygon's are identical
    if (!identical(sf.polygon1[["GEOID"]], LU2$sf.polygon[["GEOID"]])) {
        stop("Error: LU1 and LU2 have a different sf.polygon IDs")
    }

    ##Check the types
    types <- c(LU1$type, LU2$type)
    if (!"slope" %in% types) {
        stop("Error: Either LU1 and LU2 needs a 'water' LandUnavailibity type")
    } else if (!"water" %in% types) {
        stop("Error: Either LU1 and LU2 needs a 'slope' LandUnavailibity type")
    }

    ##Initialize variables for data.table
    GEOID <- NULL
    total.unavailable <- NULL
    slope.unavailable <- NULL
    water.unavailable <- NULL
    wetlands.unavailable <- NULL

    ##Get the percent unavailabile for LU1 and LU2 and
    ##make sure that the GEOID is a character
    LU1.percent.unavailable <- LU1$percent.unavailable %>%
        .[, GEOID := as.character(GEOID)]
    LU2.percent.unavailable <- LU2$percent.unavailable %>%
        .[, GEOID := as.character(GEOID)]

    ##Merge
    all.out <- merge(setkey(LU1$percent.unavailable, GEOID),
                     setkey(LU2$percent.unavailable, GEOID),
                     all = TRUE)

    ##If the sf.polygon has a name, merge it in
    if (!is.null(polygon.name.col) && is.character(polygon.name.col)) {

        ##Get the polygon names
        polygon.names <- LU1$sf.polygon
        polygon.names <- polygon.names %>%
            as.data.frame %>%
            .[, c("GEOID", polygon.name.col)] %>%
            as.data.table %>%
            ##Make sure the GEOID ID is a character vector
            .[, GEOID := as.character(GEOID)] %>%
            setkey(GEOID)

        ##Merge in the names
        all.out <- merge(all.out, polygon.names, all = TRUE)
    }

    ##Calculate total land unavailability
    all.out <- all.out %>%
        .[, total.unavailable := slope.unavailable +
                water.unavailable + wetlands.unavailable]

    return(all.out)

}
