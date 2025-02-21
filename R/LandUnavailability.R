## c:/Dropbox/HousingElasticity/LandUnavailabilityR/R/LandUnavailability.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2017-07-23

##An R6 class to Calculate Land Unavailability

##For Roxygen Documentation examples see
##https://github.com/Ermlab/lightning-rstat/blob/41ba35d647d0f6953055eb61c87d950c6b21cc97/LightningR/R/lightning.R
##and
##https://github.com/wch/R6/issues/3

#' Class to calculate the the percentage of land unavailability given
#' tif files and usgs catalog
#'
#' Calculate the percentage of land unavailability using \code{out <-
#' LandUnavailability$new(type, raster.files.path, raster.catalog, shapefile, buffer.meters
#' = NULL, buffer.percent = NULL, parallel.cores = detectCores(logical = FALSE),
#' safe.compute = FALSE)}
#'
#' The function will generate temporary files that can be used in case
#' of interuption of computatation. The names of temporary files will
#' be based on the \code{id} (see private methods below) and the geoid
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @return Object \code{\link{R6Class}} with methods accessing land
#'     unavailability data
#' @format \code{\link{R6Class}} object.
#'
#' #' @section Arguments for the \code{new} method
#'
#' \describe{
#'   \item{\code{type}}{The type of analysis. The options are \code{"slope"} or \code{"water"}}
#'   \item{\code{raster.files.path}}{The path to the .tif files}
#'   \item{\code{raster.catalog}}{A \code{data.table} or file path to an rds file with the USGS or raster catalog}
#'   \item{\code{shapefile}}{A \code{sf} object or a path to a shapefile.}
#'   \item{\code{buffer.meters = NULL}}{How many meters to buffer each shapefile. The default is \code{NULL} for no buffer in meters}
#'   \item{\code{buffer.percent = NULL}}{What percentage to buffer each shapefile. Typically between 0 and 1. The default is \code{NULL} for no buffer in percentage terms.}
#'   \item{\code{parallel.cores = detectCores(logical = FALSE)}}{Number of parallel cores. Set to zero for sequential computation}
#'   \item{\code{safe.compute = FALSE}}{A safe compute option that will process each raster individually. It's slower, but likely process the rasters successfully}
#' }
#'
#' #' @section Public (Mutable) Attributes.
#'
#' \describe{
#'    \item{\code{percent.unvailable}}{A \code{data.table} with the \code{GEOID} and the percentage of land unavailable. If the \code{NA} values correspond to an error for a given \code{GEOID}}
#' }
#'
#' #' @section Public Methods.
#'
#' \describe{
#'   \item{\code{new(type = c("slope", "water"), raster.files.path, raster.catalog, shapefile, buffer.meters = NULL, buffer.percent = NULL, parallel.cores = detetCores(logical = FALSE), safe.compute = FALSE)}}{Instantiate an of the LandUnavailability class with \code{LandUnavailability$new()} function.}
#'    \item{\code{calc_unavailable_land}}{To calculate the percentage of unavailable land}
#'    \item{\code{finish()}}{To delete any temporary files associated with computation}
#' }
#'
#' #' @section Locked Data Attributes (Active Bindings. View Only, But Can Assigned to Other Objects):
#'
#' \describe{
#'    \item{\code{type}}{(character) The type of analysis. Either "slope" or "water"}
#'    \item{\code{id}}{(character) The ID used for saving files for this object}
#'    \item{\code{temp.geoid.folder}}{(character) The temporary folder where we save the land unavailability for each GEOID}
#'    \item{\code{raster.files.path}}{The path to the tif files}
#'    \item{\code{raster.catalog}}{A \code{data.table} or path to an RDS file with a \code{data.table} with the catalog of USGS or raster files}
#'    \item{\code{shapefile}}{A simple features \code{sf} shapefile polygon or the path to a shapefile to read in by \code{sf}}
#'    \item{\code{raster.list}}{The list of raster objects}
#'    \item{\code{raster.crs}}{The CRS for the raster objects. This will vary depending if the unavailability analysis is for \code{"slope"} or \code{"water"}}
#'    \item{\code{sf.polygon}}{The \code{sf} polygon object}
#'    \item{\code{buffer.meters}}{The buffer meters. If \code{NULL}, then no buffer in meters is applied}
#'    \item{\code{buffer.percent}}{The buffer; typically between 0 and 1. If \code{NULL}, then no buffer in percentage terms is applied}
#'    \item{\code{polygon.raster.intersection}}{A \code{data.table} with the USGS IDs for each GEOID}
#' }
LandUnavailability <- R6Class(
    classname = "LandUnavailability",
    private = list(
        ##The type of analysis (e.g. "slope", "water")
        ..type = NULL,
        ##An Id that aids in saving temporary -- paste0(private$type, Sys.time()
        ..id = NULL,
        ##the temporary folder for the GEOIDs
        ..temp.geoid.folder = NULL,
        ##The path for the tif files
        ..raster.files.path = NULL,
        ##The USGS or raster catalog (data.table)
        ..raster.catalog = NULL,
        ##The list of raster objects
        ..raster.list = NULL,
        ##The crs of the (usgs) raster objects
        ..raster.crs = NULL,
        ##The simple features shapefile polygon
        ..sf.polygon = NULL,
        ##A list of sp polygon objects
        ..sp.polygon.list = NULL,
        ##How many meters we're going to buffer the polygon by
        ..buffer.meters = NULL,
        ##What percentage we're going to buffer the polygon by
        ..buffer.percent = NULL,
        ##The data.table with the intersection of the bounding
        ##boxes for each polygon in
        ## private$..sf.polygon with all of the raster objects in
        ## private$..raster.catalog. See lu_intersect_sf_raster()
        ..polygon.raster.intersection = NULL,
        ##A function to safely try to Crop raster objects using
        ##the raster::crop() function. This is necessary when
        ##a polygon touches a raster, but does not intersect with
        ##it. In these cases, raster::crop() will return an error
        safe_crop = function(x, y, ...) {
            out <- try(raster::crop(x, y, ...), silent = TRUE)
            if (inherits(out, "try-error")) {
                return(invisible(NULL))
            } else {
                return(out)
            }
        },
        ##A function to save output from this geoid. This function
        ##will be used to save temporary output along the way
        save_geoid = function(temp.obj, temp.geoid) {
            folder <- private$..temp.geoid.folder
            if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
            file.name <- paste0(folder, temp.geoid, ".rds")
            saveRDS(temp.obj, file = file.name)
        },
        ##The function to calculate the unavailable land
        ##(e.g. for land or water)
        f_unavailable = NULL,
        ## A private method to calculate the percent unavailable
        ##
        ## A private method to calculate the percentage of land unavailable
        ##     due to either water or b/c grade is larger than
        ##     it's private$..slope.degrees.cutoff
        ##
        ## @param temp.geoid a Geographic ID
        ## @return a \code{data.table} with a column titled \code{GEOID} and
        ##     percentage of land that is unavailable
        f_calc_unavailable = function(temp.geoid) {

            ##Create a temporary directory on the C drive for the
            ##temporary raster files
            ##see
            ##https://stackoverflow.com/questions/25426405/raster-package-taking-all-hard-drive
            junk.folder <- paste0("c:/junk", temp.geoid)
            ##create the junk folder
            dir.create(junk.folder)
            rasterOptions(tmpdir = junk.folder)

            out <- try({

                ##Get an "sp" object for this geoid
                temp.polygon.sp <- private$..sp.polygon.list[[temp.geoid]]
                ##Get the raster IDs that match up for this GEOID
                raster.ids <- private$..polygon.raster.intersection[GEOID == temp.geoid,
                                                                raster.ids][[1]] %>%
                    ##If the entry in the ID is a number, add an `X` to match
                    ##the raster package
                    ifelse(grepl("^[0-9]{1}", .), paste0("X", .), .)

                ##The rasters
                raster.temp.geoid <- private$..raster.list[raster.ids] %>%
                    ##Crop the raster based on the extent of temp.polygon.sp
                    lapply(private$safe_crop, raster::extent(temp.polygon.sp)) %>%
                    ##remove any potential null objects from the list
                    .[!sapply(., is.null)]

                if (length(raster.temp.geoid) >= 20 || self$safe.compute) {
                    ##For a large number of Raster IDs or if self$safe.compute
                    ##is TRUE, mask individually
                    ##This is likely slower, but may work better for
                    ##larger land areas and be more memory efficient
                    temp.result <- lapply(raster.temp.geoid, mask,
                                          mask = temp.polygon.sp) %>%
                        lapply(getValues) %>%
                        do.call("c", .) %>%
                        .[!is.na(.)] %>%
                        private$f_unavailable(.)
                } else {
                    ##Normal processing of raster objects

                    ##If the length of raster.list.temp > 1, merge into a mosaic
                    if (length(raster.temp.geoid) > 1) {
                        ##Remove names of the list elements as suggested here
                        ##https://stackoverflow.com/a/33696237/1317443
                        names(raster.temp.geoid) <- NULL
                        ##Take the mean of overlapping raster values
                        raster.temp.geoid$fun <- mean
                        ##Get the Mosaic
                        raster.temp.geoid <- do.call(mosaic, raster.temp.geoid)
                    } else {
                        ##Only one usgs raster for temp.geoid
                        raster.temp.geoid <- raster.temp.geoid[[1]]
                    }

                    ##Mask raster.temp.geoid based on temp.polygon.sp
                    ##see
                    ##http://zevross.com/blog/2015/03/30/map-and-analyze-raster-data-in-r/
                    raster.temp.geoid <- mask(raster.temp.geoid,
                                              mask = temp.polygon.sp)

                    ##Get the values for the raster
                    raster.values <- getValues(raster.temp.geoid) %>%
                        ##Remove NAs
                        .[!is.na(.)]

                    rm(raster.temp.geoid)

                    ##Calculate the unavailable land
                    temp.result <- private$f_unavailable(raster.values)

                    ##remove the raster values
                    rm(raster.values)
                }

                ##Remove the raster and polygon objects
                rm(temp.polygon.sp)

                ##A data.table with the output
                data.table(GEOID = temp.geoid) %>%
                    cbind(temp.result) %>%
                    .[order(GEOID)]

            })

            ##Delete the junk folder with temporary raster files
            unlink(junk.folder, recursive = TRUE)

            ##Return NULL if error
            if (inherits(out, "try-error")) {
                print(paste0("Error for ", temp.geoid))
                print(attr(out,"condition"))
                return(data.table(GEOID = temp.geoid))
            } else {
                ##save
                private$save_geoid(out, temp.geoid)
                ##return
                return(out)
            }

        }
    ), ##End of private attributes
    active = list(
        id = function() private$..id,
        temp.geoid.folder = function() private$..temp.geoid.folder,
        type = function() private$..type,
        raster.files.path = function() private$..raster.files.path,
        raster.catalog = function() private$..raster.catalog,
        raster.crs = function() private$..raster.crs,
        raster.list = function() private$..raster.list,
        sf.polygon = function() private$..sf.polygon,
        buffer.meters = function() private$..buffer.meters,
        buffer.percent = function() private$..buffer.percent,
        polygon.raster.intersection = function() private$..polygon.raster.intersection
    ),
    public = list(

        ##The number of cores used in parallel computation
        parallel.cores = NULL, ##zero for sequential computation
        ##A safe compute option that will process each
        ##raster individually. It's slower, but likely process
        ##the rasters successfully
        safe.compute = FALSE,
        ##A data.table where for each GEOID, we record the
        ##land unavailability (as a percentage)
        percent.unavailable = NULL,
        ##A function to run on completion of running the outptu
        finish = function() unlink(private$..id, recursive = TRUE, force = TRUE),
        ##A public method to calculate the percentage of land that
        ##is unavailable
        calc_unavailable_land = function() {
            ##Get the GEOIDs for which the unavailability
            ##hasn't been calculated

            ##Get temporary geoid files, if there are any
            temp.files <- list.files(private$..temp.geoid.folder, full.names = TRUE)
            ##Check for temporary save from a previous iteration
            if (length(temp.files) > 0) {
                ##there are previous temporary saves, build a data.table
                ##based on these files
                temp.saves <- lapply(temp.files, readRDS) %>%
                    rbindlist %>%
                    .[!GEOID %in% self$percent.unavailable[["GEOID"]]]
            } else {
                ##No temporary saves, set temp.saves to empty data.table()
                temp.saves <- data.table()
            }

            self$percent.unavailable <- na.omit(self$percent.unavailable)
            self$percent.unavailable <- rbind(self$percent.unavailable,
                                              temp.saves)

            ##remove temporary variables
            rm(temp.files, temp.saves)

            ##Get the GEOIDS that for which we are going to compute land unavailability
            temp.ids <- private$..polygon.raster.intersection[["GEOID"]] %>%
                unique(.) %>%
                ##only keep GEOIDs where percent.unavailable is missing
                .[!(. %in% self$percent.unavailable[["GEOID"]])]

            ##Randomize the temp.ids so that the size of the geography is
            ##randomly assigned to cores in parallel processings
            temp.ids <- base::sample(temp.ids, size = length(temp.ids),
                                     replace = FALSE)

            if (length(temp.ids) == 0) {
                print("All GEOIDs already have the percentage of land that is unavailable.")
                return(invisible(NULL))
            }

            ##Run in parallel
            if (self$parallel.cores > 1) {

                ##Run in parallel
                cl <- makeCluster(self$parallel.cores)
                ##Make packages available for each cluster
                clusterEvalQ(cl, {
                    library(magrittr);
                    library(data.table);
                    library(raster);
                    library(sf);
                    library(sp)
                })
                temp.DT <- parLapplyLB(cl,
                                       temp.ids,
                                       private$f_calc_unavailable) %>%
                    rbindlist(fill = TRUE)
                stopCluster(cl)

            } else {
                ##Not in parallel
                temp.DT <- lapply(temp.ids,
                                  private$f_calc_unavailable) %>%
                    rbindlist(fill = TRUE)
            }

            ##Update
            self$percent.unavailable <- self$percent.unavailable %>%
                ##Delete NA rows
                na.omit %>%
                ##Bind in the new results
                rbind(., temp.DT, fill = TRUE) %>%
                ##order by GEOID
                .[order(GEOID)]

            ##Save the object
            saveRDS(self, paste0(private$..id, "/", private$..id, ".rds"))

            return(invisible(NULL))
        },
        initialize = function(type = c("slope", "water"),
                              raster.files.path, raster.catalog,
                              shapefile, buffer.meters = NULL,
                              buffer.percent = NULL, raster.crs = NULL,
                              fun.unavail.raster = NULL,
                              parallel.cores = (detectCores(logical = FALSE)),
                              safe.compute = FALSE) {

            private$..type <- type[1]; rm(type)

            ##the id -- the type plus 5 random letters plus
            ##the time object was instantiated
            temp.letters <- base::sample(LETTERS, 5, replace = TRUE) %>%
                paste0(collapse = "")
            private$..id <- paste(private$..type, temp.letters,
                                  strftime(Sys.time(), "%Y_%m_%d_%H_%M_%S"),
                                  sep = "_")
            rm(temp.letters) ##remove the temporary random letters


            ##The temporary folder for the GEOIDs
            private$..temp.geoid.folder <- paste0(private$..id,
                                                  "/", "temp_geoid_files/")

            ##The parallel cores
            self$parallel.cores <- parallel.cores; rm(parallel.cores)
            self$safe.compute <- safe.compute; rm(safe.compute)

            ##The crs
            if (!is.null(raster.crs)) {
                private$..raster.crs <- raster.crs
            } else if (private$..type == "slope") {
                ##slope CRS from files in
                ##HousingElasticity\Data\Data_USGS_1_arc_second\11-USGSUSSlopeTifFiles
                private$..raster.crs <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
            } else if (private$..type == "water") {
                ##USGS water crs files
                ##Water analysis CRS from
                ##Data_ShapeFiles\NLCD2011_LC\appendix3_nlcd2011_scene_list_by_path_row.shp
                private$..raster.crs <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
            }
            rm(raster.crs)

            ##The function used to calculate the slope
            if (!is.null(fun.unavail.raster)) {
                private$f_unavailable = fun.unavail.raster
            } else if (private$..type == "slope") {
                private$f_unavailable = lu_calc_slope_unavailable
            } else if (private$..type == "water") {
                private$f_unavailable = lu_calc_usgs_water_unavailable
            }
            rm(fun.unavail.raster)

            ##The path to the tif files
            private$..raster.files.path <- raster.files.path; rm(raster.files.path)

            ##Get the raster list
            private$..raster.list <- lu_get_raster_list(private$..raster.files.path) %>%
                ##Add names to the raster objects based on their USGS IDs
                ##Note: If the USGS IDs start with a number, the raster package
                ##will put an `X` in front of the ID for the name
                setNames(., vapply(., names, character(1)))

            ##Check that all rasters have the same crs
            raster.crs.unique <- lapply(private$..raster.list, crs) %>%
                vapply(as.character, character(1)) %>% unique
            if (length(raster.crs.unique) != 1) {
                print("Error: Rasters have different CRS")
                print(raster.crs.unique)
                stop("Error: Rasters have different CRS")
            }
            rm(raster.crs.unique)

            ##The (usgs) catalog of raster files
            if ("data.table" %in% class(raster.catalog)) {
                private$..raster.catalog <- raster.catalog
            } else if ("character" %in% class(raster.catalog)) {
                private$..raster.catalog <- readRDS(raster.catalog)
                if (!"data.table" %in% class(private$..raster.catalog))
                    stop("Error: the raster.catalog needs to be (a path to) a data.table")
            }
            rm(raster.catalog)

            ##How many meters to buffer the simple featurs polygon by
            if (!is.null(buffer.meters)) {
                private$..buffer.meters = buffer.meters; rm(buffer.meters)
            } else if (!is.null(buffer.percent)) {
                private$..buffer.percent = buffer.percent; rm(buffer.percent)
            }

            ##The sf (simple features) polygon
            if ("sf" %in% class(shapefile)) {
                ##Already an sf object
                private$..sf.polygon <- shapefile
            } else  if ("character" %in% class(shapefile)) {
                private$..sf.polygon <- sf::st_read(shapefile)
            } else {
                stop("Error: shapefile has to be either a file path or a sf object")
            }
            rm(shapefile)

            if (!"GEOID" %in% names(private$..sf.polygon))
                stop("Error: shapefile needs to have a column 'GEOID'")


            ##Add the buffer
            if (!is.null(private$..buffer.meters)) {
                private$..sf.polygon <- private$..sf.polygon %>%
                    lu_buffer_sf(meters = private$..buffer.meters,
                                 out.crs = private$..raster.crs)
            } else if (!is.null(private$..buffer.percent)) {
                private$..sf.polygon <- private$..sf.polygon %>%
                    lu_buffer_sf(percent = private$..buffer.percent,
                                 out.crs = private$..raster.crs)
            } else {
                ##transform the crs
                private$..sf.polygon <- st_transform(private$..sf.polygon,
                                                     crs = private$..raster.crs)
            }

            ##The shapefile as a list of polygon objects
            temp.sf.geometry <- sf::st_geometry(private$..sf.polygon)
            f_sp_polygon_list <- function(i) as(temp.sf.geometry[i], "Spatial")

            if (self$parallel.cores > 1) {
                ##Run in parallel
                cl <- makeCluster(self$parallel.cores)
                clusterEvalQ(cl, {library(sf); library(sp)})
                private$..sp.polygon.list <- parLapplyLB(
                    cl,
                    seq_along(temp.sf.geometry),
                    f_sp_polygon_list)

                stopCluster(cl)
            } else {
                private$..sp.polygon.list <- lapply(
                    seq_along(temp.sf.geometry),
                    f_sp_polygon_list)
            }
            private$..sp.polygon.list <- private$..sp.polygon.list %>%
                ##Set the names based on the GEOIDs
                setNames(private$..sf.polygon$GEOID)


            ##Remove the temporary sf geometry
            rm(temp.sf.geometry); rm(f_sp_polygon_list)


            ##The intersection between the simple features polygon
            ##and the raster object.
            private$..polygon.raster.intersection <-
                lu_intersect_sf_raster(sf.obj = private$..sf.polygon,
                                       raster.catalog = private$..raster.catalog)

            ##A data.table with the percentage unavailable
            self$percent.unavailable = data.table()

            ##Save the object
            if (!dir.exists(private$..id)) dir.create(private$..id)
            saveRDS(self, paste0(private$..id, "/", private$..id, ".rds"))

            print("starting main unavailability computation")
            ##Calculate the percent of land unavailable
            rasterOptions(maxmemory = 1e+11) ##set the maximum memory
            self$calc_unavailable_land()

            ##clean up
            self$finish()


        }
    ) ##end of public attributes

)  ## End of LandUnavailability R6Class
