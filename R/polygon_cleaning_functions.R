## c:/Dropbox/HousingElasticity/LandUnavailabilityR/R/polygon_cleaning_functions.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2017-07-23

## -- A list of functions to clean a polygons -- ##

#' Transform ALL simple feature columns (sfc) to a new CRS
#'
#' This function is useful when a simple features object has multiple
#' simple feature (polygon columns)
#'
#' @param sf.obj A \code{sf} (simple features) polygon object
#' @param crs A valid CRS
#' @param ... Other arguments to be passed to \code{st_transform}
#' @return a \code{sf} object where of the simple feature columns are
#'     transformed to the new CRS
#' @export
lu_st_transform_all <- function(sf.obj, crs, ...) {

    ##The simple features columns
    sf.cols <- lapply(sf.obj,class) %>%
        .[vapply(., function(x) "sfc" %in% x, logical(1))] %>%
        names(.)

    ##Prject each sf column to a new CRS
    for (col in sf.cols) {
        sf.obj[[col]] <- sf::st_transform(sf.obj[[col]], crs = crs, ...)
    }
    ##Project the whole sf.obj to the new CRS
    sf.obj <- sf::st_transform(sf.obj, crs = crs, ...)

    return(sf.obj)

}


#' Create a buffer around all of the polygons an \code{sf} shapefile
#'
#' Notes: This function will use EPSG 2163 for the calculation of the
#' buffers. This is the equal area projection for the US. See
#' \url{https://epsg.io/2163}
#'
#' @param sf.obj A \code{sf} (simple features) polygon object
#' @param meters The number of meters to use for the buffer
#' @param percent the percentage with which to increase each
#'     polygon. Needs to be between 0 and 1.
#' @param out.crs The CRS for the returned data. If \code{NULL}, the
#'     CRS of the original \code{sf.obj} will be used. Defualt is
#'     \code{NULL}.
#' @return A \code{sf} object with where each polygon has a buffer of
#'     \code{meters} or \code{percent}
#' @export
lu_buffer_sf <- function(sf.obj, meters = NULL, percent = NULL, out.crs = NULL) {


    if (is.null(meters) && is.null(percent))
        stop("Error: lu_buffer_sf() needs meters or percent to be non-NULL")

    ##If out.crs is NULL
    if (is.null(out.crs)) out.crs <- sf::st_crs(sf.obj)$epsg


    ##Transform sf.obj to 2163
    sf.obj <- sf::st_transform(sf.obj, crs = 2163)

    if (!is.null(meters)) {

        ##If meters is equal to zero, return with the transformed CRS
        if (meters == 0) return(sf::st_transform(sf.obj, crs = out.crs))

        ##Increase each polygon by the same number of meters
        sf.obj <- sf::st_buffer(sf.obj, dist = meters)
    } else if (!is.null(percent)) {

        ##If meters is equal to zero, return with the transformed CRS
        if (percent == 0) return(sf::st_transform(sf.obj, crs = out.crs))

        ##Make sure percent is greater than 0
        stopifnot(percent > 0)

        ##Get the area in square meters
        sf.obj.area <- sf::st_area(sf.obj) %>% as.numeric
        ##Get the buffer percentage
        sf.obj.buffer.meters <- sqrt(sf.obj.area) * percent
        ##Apply the buffer to each polygon object
        sf.obj <- sf::st_buffer(sf.obj, dist = sf.obj.buffer.meters)
    }

    ##re-project using out.crs
    sf.obj <- sf::st_transform(sf.obj, crs = out.crs)

    return(sf.obj)

}

#' Get the bounding box (bbox) for a \code{sf} (simple features) object
#'
#' @param sf.obj A \code{sf} (simple features) object. \code{sf.obj}
#'     must have a column named "GEOID" with the geographic identifier
#'     for each polygon
#' @return a \code{data.table} with the bouding box and the following
#'     columns (in order): \code{GEOID}, \code{xmin}, \code{xmax},
#'     \code{ymin}, \code{ymax}
#' @export
lu_sf_bbox <- function(sf.obj) {

    if (!"sf" %in% class(sf.obj))
        stop("sf.obj needs to be of class sf")

    if (!"GEOID" %in% names(sf.obj))
        stop("sf.obj needs to have a name 'GEOID'")

    ##Initialize some global variables to get around R CMD check
    ##for data.table
    ##see https://github.com/Rdatatable/data.table/issues/850
    GEOID <- NULL

    out.bbox <- sf::st_geometry(sf.obj) %>%
        lapply(sf::st_bbox) %>%
        do.call("rbind", .) %>%
        as.data.table %>%
        .[, GEOID := as.character(sf.obj$GEOID)] %>%
        setcolorder(c("GEOID", "xmin", "xmax", "ymin", "ymax")) %>%
        .[order(GEOID)]

    return(out.bbox)
}

#' Get the raster IDs that intersect with a single polygon extent
#'
#' @param polygon.extent An \code{Extent} object created by
#'     \code{raster::extent} for a polygon. We want to find the raster
#'     IDs that intersect with \code{polygon.extent}. Note that this
#'     is an \code{Extent} object for a single polygon
#' @param raster.catalog A \code{data.table} with the raster. For the
#'     US, this will be the USGS catalog of raster objects. Note:
#'     there needs to be a column titled "raster.extent" of class
#'     \code{Extent} in and another column title "sourceId" with the
#'     source IDs for the Raster Files
#' @return A \code{character} vector with with the raster IDs that
#'     intersect polygon.extent
#' @export
lu_intersect_polygon_extent_raster <- function(polygon.extent, raster.catalog) {

    ##Make sure that raster.catalog has column
    ##title "raster.extent"
    if (!"raster.extent" %in% names(raster.catalog))
        stop("Error: raster.catalog in lu_intersect_polygon_extent_raster() needs a column labeled 'raster.extent' that is the output from raster::extent() ")

    ##Make sure there is a column title "sourceId"
    if (!"sourceId" %in% names(raster.catalog))
        stop("Error: raster.catalog in lu_intersect_polygon_extent_raster() needs a column labeled 'sourceId' that is the unique ID for the raster files ")

    ##Make sure that raster.catalog$raster.extent has objects of class
    ##Extent
    raster.extent.classes <- vapply(raster.catalog[["raster.extent"]],
                                    class, character(1))
    if (!all(raster.extent.classes == "Extent"))
        stop("Error: raster.catalog$raster.extent must be a list-column where each element of the list is of class 'Extent'")
    rm(raster.extent.classes)

    ##Initialize some global variables to get around R CMD check
    ##for data.table
    ##see https://github.com/Rdatatable/data.table/issues/850
    sourceId <- NULL
    raster.extent <- NULL
    intersects.bool <- NULL

    ##Initialize a data.table to store boolean values for output
    ##If intersects.bool == TRUE, the raster intersects
    ##with the shapefile polygon
    out <- copy(raster.catalog) %>%
        .[, .(sourceId, intersects.bool = FALSE)]

    ##Loop over the raster ids
    for (j in 1L:nrow(raster.catalog)) {
        ##Get the raster::extent() objecte for the RASTER polygon
        raster.extent <- raster.catalog[j, raster.extent][[1]]

        ##Does the raster raster at all overlap with polygon??
        if (!is.null(raster::intersect(polygon.extent, raster.extent))) {
            ##The polygon for the area intersects with the raster
            set(out, i = j, "intersects.bool", TRUE)
        }
    }

    ##Get just the sourceId's for out
    out <- out[intersects.bool == TRUE, sourceId]

    return(out)

}

#' Intersect a \code{sf} (simple features) object and a raster catalog
#'
#' This function will return a \code{data.table} with the boundry box
#' and the raster IDs that intersect with a \code{sf} object.
#'
#' @param sf.obj A \code{sf} (simple features) object
#' @param raster.catalog A raster catalog that contains a \code{Extent}
#'     object created by \code{raster::extent()}
#' @return A \code{data.table} with with the boundry box for each
#'     polygon in \code{sf.obj} and the corresponing raster source IDs
#'     that intersect with each polygon in \code{sf.obj}
#' @export
lu_intersect_sf_raster <- function(sf.obj, raster.catalog) {

    ##Get the bounding box for sf.obj as a data.table
    DT.polygon.bbox <- lu_sf_bbox(sf.obj)

    ##Add raster.extent if it doesn't exist
    if (!("raster.extent" %in% names(DT.polygon.bbox))) {
        for (i in 1L:nrow(DT.polygon.bbox)) {
            temp <- DT.polygon.bbox[i, c("xmin", "xmax", "ymin", "ymax")] %>%
                as.matrix
            ##convert to a vector
            temp <- temp[1, ] %>% as.numeric %>%
                raster::extent(.)

            set(DT.polygon.bbox, i, "raster.extent", list(list(temp)))
        }
    }

    ##Initialize some global variables to get around R CMD check
    ##for data.table
    ##see https://github.com/Rdatatable/data.table/issues/850
    raster.ids <- NULL

    ##Get a list of all of the columns with the raster sourceId's that
    ##intersect with the polygon shapefile
    ##Run in parallel
    cl <- makeCluster(detectCores())  ##Initialize the cluster
    clusterEvalQ(cl, {library(magrittr); library(data.table); library(raster); })
    ## temp <- lapply(DT.polygon.bbox$raster.extent[1:2],
    ##                lu_intersect_polygon_extent_raster,
    ##                raster.catalog)
    temp <- parLapply(cl, DT.polygon.bbox$raster.extent,
                      lu_intersect_polygon_extent_raster,
                      raster.catalog)
    stopCluster(cl)

    ##add temp to DT.polygon.bbox
    DT.polygon.bbox[, raster.ids := list(temp)]

    return(DT.polygon.bbox)
}


#' Apply a function to associated polygons using \code{sf} goemetric
#' predicates
#'
#' Using a goemetric binary predicates from the \code{sf} package,
#' such as \code{st_touches}, this function applies a function to the
#' associated polygons. For a list of compatable geometric binary
#' predicate functions from the \code{sf} package, see
#' \code{?sf::geos_binary_pred}.
#'
#' @param DT a \code{data.table} that has a column named \code{GEOID}
#'     and variables whose values will be subject to computation using
#'     \code{fun}
#' @param sf.obj the \code{sf} object that holds the shapefiles
#'     (polygons)
#' @param vars a character vector of variables (columns) in \code{DT}
#'     that are used in computation
#' @param remove.main.polygon (logical) If TRUE, the main polygon will
#'     be removed (not be used) in the computation. Default is
#'     \code{TRUE}
#' @param st_binary_fun a binary predicate function from the \code{sf}
#'     package. See \code{?sf::geos_binary_pred} for possible
#'     functions
#' @param fun the function to be applied.
#' @param ... optional arguments to \code{fun}
#' @return a \code{data.table}, similar to \code{DT}, with the new
#'     variables added. The new variables will have a \code{vars} with
#'     a ".out" suffix
#' @examples
#' ##The saiz data
#' data(saiz.sf)
#'
#' ##the variables
#' vars <- c("total.unavailable", "slope.unavailable")
#'
#'
#' ##Example of land unavailability output
#' data(land.unavailability.example)
#'
#' ##Calculate the mean of each variable in vars removing NA values
#' out <- lu_st_binary_polygon_apply(land.unavailability.example,
#'                                   saiz.sf,
#'                                   vars = vars,
#'                                   st_binary_fun = st_touches,
#'                                   fun = mean, na.rm = TRUE)
#' print(out)
#'
#' ##Another example using length()
#' out <- lu_st_binary_polygon_apply(land.unavailability.example,
#'                                   saiz.sf,
#'                                   vars = vars,
#'                                   st_binary_fun = st_touches,
#'                                   fun = length)
#' print(out)
#'
#' @export
lu_st_binary_polygon_apply <- function(DT, sf.obj, vars, remove.main.polygon = TRUE,
                                       st_binary_fun, fun, ...) {

  if (!("GEOID" %in% names(DT)))
    stop("Error: DT must have a column named 'GEOID'")
  if (!("GEOID" %in% names(sf.obj)))
    stop("Error: sf.obj must have a column named 'GEOID'")

  ##make sure that the GEOIDs are characters,
  ##are in the same order, and match
  sf.obj <- sf.obj %>%
    dplyr::mutate(GEOID = as.character(GEOID)) %>%
    dplyr::filter(GEOID %in% DT[["GEOID"]]) %>%
    dplyr::arrange(GEOID)

  ##Get DT in the right order
  DT <- copy(DT) %>%
    .[, GEOID := as.character(GEOID)] %>%
    .[order(GEOID)]

  if (!identical(DT[["GEOID"]], sf.obj[["GEOID"]]))
    stop("Error: 'DT' and 'sf.obj' have different GEOIDs")

  ##To pass R CMD check
  associated.polygons <- GEOID <- NULL

  ##Get all of the GEOIDs
  GEOIDs <- DT[["GEOID"]]
  ##Add the a list-column for all of the polygons that touch a given
  ##polygon
  sf.obj <- sf.obj %>%
    ##Get the index for the of touching polygons for each polygon
    dplyr::mutate(associated.polygons = apply(st_binary_fun(sf.obj, sf.obj), 2, which)) %>%
    ##Get the GEOIDs for the touching polygons
    dplyr::mutate(associated.polygons = lapply(associated.polygons, function(temp.list) {
      GEOIDs[temp.list]}
      ))

  ##Get a data.table with with just the GEOID
  ##And all GEOIDs that touch a given polygon
  associated.polygons.DT <- sf.obj %>% setDT %>%
    .[, .(GEOID, associated.polygons)] %>%
    setkey(GEOID)

  DT <- DT %>%
    setkey(GEOID) %>%
    ##left join
    associated.polygons.DT[., on = "GEOID"]

  vars.out <- paste0(vars, ".out")
  DT[, c(vars.out) := NA_real_]

  for (i in 1:nrow(DT)) {

    ##the polygons that touch as a character vector
    polygons.associated <- DT[["associated.polygons"]][i] %>%
      unlist %>%
      as.character

    if (remove.main.polygon) {
      ##make sure the GEOID for this iteration
      ##is not among the GEOIDs
      polygons.associated <- polygons.associated %>%
        .[. != DT[i, GEOID]]
    }


    ##the values
    temp.values <- DT[GEOID %in% c(polygons.associated),
                      lapply(.SD, fun, ...),
                      .SDcols = c(vars)]

    if (nrow(temp.values) > 0) {
      ##Non-missing output, set
      set(DT, i, vars.out, as.list(temp.values))
    }
  }
  return(DT)
}
