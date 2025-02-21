## c:/Dropbox/LandUnavailability/LandUnavailabilityR/R/BuildableLand.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-04-14

##An R6 class to Calculate the Amount of Available Land

#' Class to calculate the amount of buildable land (meters) in a polygon region
#'
#' Calculate the amount of buildable land in a region
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
BuildableLand <- R6Class(
  classname = "BuildableLand",
  private = list(
    ##An Id that aids in saving temporary -- paste0(private$type, Sys.time()
    ..id = NULL,
    ##the temporary folder for the GEOIDs
    ..temp.geoid.folder = NULL,
    ##The path for the tif files
    ..slope.tif.path = NULL,
    ..mask.tif.path = NULL,
    ##The USGS or raster catalog (data.table)
    ..slope.catalog = NULL,
    ..mask.catalog = NULL,
    ##The list of raster objects
    ..slope.raster.list = NULL,
    ..mask.raster.list = NULL,
    ##The CRS of the files -- note that the
    ##slope and mask raster files must
    ##have the same crs
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
    ..slope.raster.intersection = NULL,
    ..mask.raster.intersection = NULL,
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
    safe_mask = function(x, mask, ...) {
      out <- try(raster::mask(x, mask, ...), silent = TRUE)
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
    ##The function to calculate the amount of buildable land
    ##e.g. area of a raster
    ##from https://stackoverflow.com/a/40706190/1317443
    ##In thousands of square km
    f_buildable = function(raster.slope, raster.mask, sf.obj) {
      ## raster.slope -- the raster slope file
      ## raster.mask -- the raster mask file
      ## sf.obj -- the sf obj

      ##For the slope raster file, set the slope (in degrees to 100) which
      ##will be considered as slope.unavailable
      ##for the legend see
      ##https://www.mrlc.gov/nlcd01_leg.php
      ##https://www.mrlc.gov/nlcd11_leg.php
      raster.slope[raster.mask  <= 24 | raster.mask >= 90] <- 100

      raster.slope.values <- getValues(raster.slope) %>%
        .[!is.na(.)]

      ##The percent of unavailable land
      unavailable <- lu_calc_slope_unavailable(raster.slope.values)

      region.area <- sf::st_area(sf.obj) %>% as.numeric
      ##The amount of buildable land in meters
      buildable.land <-
        unavailable[, .(available.land = region.area,
                        buildable.land = region.area * (1 - slope.unavailable / 100))]

      return(buildable.land)

    },
    ##A function to help calculate the amount of
    ##buildable land for this geoid
    f_calc_buildable = function(temp.geoid) {

      ##The sf.obj for this polygon objection
      sf.obj <- as.data.table(private$..sf.polygon) %>%
        .[GEOID == (temp.geoid), geometry]

      if (sf::st_dimension(sf.obj) == 0) {
        ##The dimension is empty for the GEOID, return a zero
        return(data.table(GEOID = temp.geoid, available.land = 0, buildable.land = 0))
      }

      ##Create a temporary directory on the C drive for the
      ##temporary raster files
      ##see
      ##https://stackoverflow.com/questions/25426405/raster-package-taking-all-hard-drive
      junk.folder <- paste0("c:/junk", temp.geoid)

      ##create the junk folder
      if (!dir.exists(junk.folder))
        dir.create(junk.folder)
      rasterOptions(tmpdir = junk.folder)

      out <- try({
        ##Get an "sp" object for this geoid
        temp.polygon.sp <- private$..sp.polygon.list[[temp.geoid]]
        ##Get the raster IDs that match up for this GEOID
        slope.raster.ids <-
          private$..slope.raster.intersection[GEOID == temp.geoid,
                                              raster.ids][[1]] %>%
          ##If the entry in the ID is a number, add an `X` to match
          ##the raster package
          ifelse(grepl("^[0-9]{1}", .), paste0("X", .), .)
        mask.raster.ids <-
          private$..mask.raster.intersection[GEOID == temp.geoid,
                                             raster.ids][[1]] %>%
          ##If the entry in the ID is a number, add an `X` to match
          ##the raster package
          ifelse(grepl("^[0-9]{1}", .), paste0("X", .), .)
        ##The rasters
        f_get_raster_geoid <- function(raster.list, raster.ids) {
          raster.temp.geoid <- raster.list[raster.ids] %>%
            ##Crop the raster based on the extent of the sp spatial object
            lapply(private$safe_crop, raster::extent(temp.polygon.sp)) %>%
            ##Mask the slope and Mask based on the polygon
            lapply(private$safe_mask, temp.polygon.sp) %>%
            ##remove any potential null objects from the list
            .[!sapply(., is.null)]
        }
        slope.raster.temp.geoid <-
          f_get_raster_geoid(private$..slope.raster.list, slope.raster.ids)
        mask.raster.temp.geoid <-
            f_get_raster_geoid(private$..mask.raster.list, mask.raster.ids)

        if (length(mask.raster.temp.geoid) > 1) {

          ##make sure that the raster objects all have the same crs and resolution
          temp.crs <- sapply(mask.raster.temp.geoid, crs) %>% unique
          if (length(temp.crs) > 1) stop("Error: CRS of masks do not match")
          rm(temp.crs)
          temp.res <- sapply(mask.raster.temp.geoid, res) %>% unique


          ##Mosaic the mask rasters for easy substraction
          ##https://stackoverflow.com/a/33696237/1317443
          names(mask.raster.temp.geoid) <- NULL
          ##Take the mean of overlapping raster values
          mask.raster.temp.geoid$fun <- mean
          ##Get the Mosaic
          mask.raster.temp.geoid <- do.call(mosaic, mask.raster.temp.geoid)
        } else {
          ##unlist
          mask.raster.temp.geoid <- mask.raster.temp.geoid[[1]]
        }

        ##If the length of slope.raster.temp.geoid > 1, merge into a mosaic
        if (length(slope.raster.temp.geoid) > 1) {
          ##Remove names of the list elements as suggested here
          ##https://stackoverflow.com/a/33696237/1317443
          names(slope.raster.temp.geoid) <- NULL
          ##Take the mean of overlapping raster values
          slope.raster.temp.geoid$fun <- mean
          ##Get the Mosaic
          slope.raster.temp.geoid <- do.call(mosaic, slope.raster.temp.geoid)
        } else {
          ##Only one usgs raster for temp.geoid
          slope.raster.temp.geoid <- slope.raster.temp.geoid[[1]]
        }

        ##resample the mask.raster.temp.geoid raster to match
        ##the slope raster
        mask.raster.temp.geoid <- raster::resample(mask.raster.temp.geoid,
                                                   slope.raster.temp.geoid,
                                                   method = "ngb")

        ##Calculate buildable land through the f_buildable() function
        temp.result <- private$f_buildable(
          raster.slope = slope.raster.temp.geoid,
          raster.mask = mask.raster.temp.geoid,
          sf.obj = sf.obj
        )

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

    } ##end of f_calc_buildable
    ), ##End of private attributes
  active = list(
  ), ##End of active bindings
  public = list(
    ##The number of cores used in parallel computation
    parallel.cores = NULL, ##zero for sequential computation
    ##A safe compute option that will process each
    ##raster individually. It's slower, but likely process
    ##the rasters successfully
    safe.compute = FALSE,
    ##A data.table where for each GEOID, we record the
    ##land unavailability (as a percentage)
    buildable.land = NULL,
    ##A function to run on completion of running the outptu
    finish = function() unlink(private$..id, recursive = TRUE, force = TRUE),
    ##A public method to calculate the amount of land that is buildable
    calc_buildable_land = function() {

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

      ##remove temporary variables
      rm(temp.files, temp.saves)

      ##Get the GEOIDS that for which we are going to compute land unavailability
      temp.ids <- private$..slope.raster.intersection[["GEOID"]] %>%
        unique(.) %>%
        ##only keep GEOIDs where percent.unavailable is missing
        .[!(. %in% self$buildable.land[["GEOID"]])]

      ##Randomize the temp.ids so that the size of the geography is
      ##randomly assigned to cores in parallel processings
      temp.ids <- base::sample(temp.ids, size = length(temp.ids),
                               replace = FALSE)

      if (length(temp.ids) == 0) {
        print("All GEOIDs already have amount of buildable land.")
        return(invisible(NULL))
      }

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
                               private$f_calc_buildable) %>%
          rbindlist(fill = TRUE)
        stopCluster(cl)

      } else {
        ##Not in parallel
        temp.DT <- lapply(temp.ids,
                          private$f_calc_buildable) %>%
          rbindlist(fill = TRUE)
      }

      ##Update
      self$buildable.land <- self$buildable.land %>%
        ##Delete NA rows
        na.omit %>%
        ##Bind in the new results
        rbind(., temp.DT, fill = TRUE) %>%
        ##order by GEOID
        .[order(GEOID)]

      ##Save the object
      saveRDS(self, paste0(private$..id, "/", private$..id, ".rds"))


    },
    initialize = function(slope.tif.path, mask.tif.path,
                          ##The raster catalogs
                          slope.catalog, mask.catalog,
                          ##The shapefile
                          shapefile,
                          ##The buffer percentage
                          buffer.meters = NULL, buffer.percent = NULL,
                          ##The crs
                          raster.crs = NULL,
                          ##The parallel cores
                          parallel.cores = (detectCores(logical = FALSE)),
                          ##The safe compute
                          safe.compute = FALSE) {

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

      ##The path of tif files
      private$..slope.tif.path = slope.tif.path; rm(slope.tif.path)
      private$..mask.tif.path = mask.tif.path; rm(mask.tif.path)

      ##Get the list of raster objects
      f_raster_list <- function(tif.path) {
        lu_get_raster_list(tif.path) %>%
          ##Add names to the raster objects based on their USGS IDs
          ##Note: If the USGS IDs start with a number, the raster package
          ##will put an `X` in front of the ID for the name
          setNames(., vapply(., names, character(1)))
      }
      private$..slope.raster.list <- private$..slope.tif.path %>%
        f_raster_list
      private$..mask.raster.list <- private$..mask.tif.path %>%
        f_raster_list

      ##Check the CRS
      f_check_crs <- function(raster.list) {
        raster.crs.unique <- lapply(raster.list, crs) %>%
          vapply(as.character, character(1)) %>% unique
        if (length(raster.crs.unique) != 1)
          stop("Error: Rasters have different CRS")
        return(raster.crs.unique)
      }
      ##Check the crs of the rasters
      slope.crs <- f_check_crs(private$..slope.raster.list)
      mask.crs <- f_check_crs(private$..mask.raster.list)

      if (slope.crs != mask.crs)
        stop("Error: Slope and Mask rasters must have the same crs")

      ##Assign the CRS to ..raster.crs
      private$..raster.crs <- slope.crs; ##same as mask.crs

      ##The (usgs) catalog of raster files
      f_clean_catalog <- function(raster.catalog) {
        ##The (usgs) catalog of raster files
        if ("character" %in% class(raster.catalog)) {
          raster.catalog <- readRDS(raster.catalog)
          if (!"data.table" %in% class(raster.catalog))
            stop("Error: the raster.catalog needs to be (a path to) a data.table")
        }
        return(raster.catalog)
      }

      private$..slope.catalog <- f_clean_catalog(slope.catalog)
      private$..mask.catalog <- f_clean_catalog(mask.catalog)

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
          lu_buffer_sf(meters = private$..buffer.meters)
      } else if (!is.null(private$..buffer.percent)) {
        private$..sf.polygon <- private$..sf.polygon %>%
          lu_buffer_sf(percent = private$..buffer.percent)
      }

      ##The shapefile as a list of polygon objects
      f_sp_polygon_list <- function(crs) {
        temp.sf.geometry <- sf::st_transform(private$..sf.polygon, crs = crs) %>%
          sf::st_geometry(.)
        f_sf_to_sp <- function(i) as(temp.sf.geometry[i], "Spatial")

        if (self$parallel.cores > 1) {
          ##Run in parallel
          cl <- makeCluster(self$parallel.cores)
          clusterEvalQ(cl, {library(sf); library(sp)})
          sp.polygon.list <- parLapplyLB(
            cl,
            seq_along(temp.sf.geometry),
            f_sf_to_sp)

          stopCluster(cl)
        } else {
          sp.polygon.list <- lapply(
            seq_along(temp.sf.geometry),
            f_sf_to_sp)
        }
        sp.polygon.list <- sp.polygon.list %>%
          ##Set the names based on the GEOIDs
          setNames(private$..sf.polygon$GEOID)
        return(sp.polygon.list)
      }
      private$..sp.polygon.list <-
        f_sp_polygon_list(crs = private$..raster.crs)

      ##The intersection between the simple features polygon
      ##and the raster object.
      private$..slope.raster.intersection <- lu_intersect_sf_raster(
        ##Make sure to transform the sf object to the right crs
        sf.obj = st_transform(private$..sf.polygon, private$..raster.crs),
        raster.catalog = private$..slope.catalog)
      private$..mask.raster.intersection <-
        lu_intersect_sf_raster(
          ##Make sure to transform the sf object to the right crs
          sf.obj = st_transform(private$..sf.polygon, private$..raster.crs),
          raster.catalog = private$..mask.catalog)

      ##A data.table with the amount of buildable land
      self$buildable.land = data.table()

      ##Save the object
      if (!dir.exists(private$..id)) dir.create(private$..id)
      saveRDS(self, paste0(private$..id, "/", private$..id, ".rds"))

      print("starting main computation")
      ##Calculate the percent of land unavailable
      rasterOptions(maxmemory = 1e+11) ##set the maximum memory
      self$calc_buildable_land()

      ##clean up
      self$finish()

    } ##End of initialize() function

  ) ##end of public attributes
) ## End of Buildable Land

