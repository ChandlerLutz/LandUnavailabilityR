## c:/Dropbox/LandUnavailability/LandUnavailabilityR/R/download_census_shp_files.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2017-10-05



#' Download pre-2010 US Census Shp files and convert to sf objects
#'
#' Census shape files from https://www.census.gov/geo/maps-data/data/prev_cartbndry_names.html
#'
#'
#' @param url the URL where the list of files shp files can be found
#'     on the census website
#' @param base.download.url the base url from where we can download a
#'     zipped pre-2010 zipped shape file from the US census
#' @param xpath the \code{rvest} \code{xpath}.
#' @param testing.files a \code{numeric} vector with the files that
#'     will be used for testing. Set to \code{NULL} for
#'     production. The default is \code{NULL}. If \code{NULL}, the
#'     function will process all files
#' @return a \code{list} with all sf objects for the shapefiles. Note: for
#'     some sets of census shapefiles, the names of the sf objets can
#'     differ.
#' @examples
#' ## for zip3
#' url <- "https://www2.census.gov/geo/tiger/PREVGENZ/zt/z300shp/"
#' base.download.url <- "https://www2.census.gov/geo/tiger/PREVGENZ/zt/z300shp/"
#' \dontrun{
#' lu_download_census_shp_pre2010(url, base.download.url)
#' }
#' @export
lu_download_census_shp_pre2010 <- function(url,
                                           base.download.url,
                                           xpath = '//*[@id="innerPage"]/table',
                                           testing.files = NULL) {

    ##Create a junck directory
    if (!dir.exists("junk")) dir.create("junk")


    ##initialize variables for data.table to pass R CMD check
    Name <- NULL

    shp.zip.file.list <- xml2::read_html(url) %>%
        ##Get the table
        rvest::html_nodes(xpath = xpath) %>%
        rvest::html_table() %>%
        .[[1]] %>%
        ##Set to data.table
        setDT %>%
        ##keep just the file names that end in zipo
        .[grepl(".zip$", Name), Name]

    ##If testing.files is not NULL, limit the files for testing
    if (!is.null(testing.files)) {
        shp.zip.file.list <- shp.zip.file.list[testing.files]
    }

    out.sf <- lapply(shp.zip.file.list, function(file) {

        ##The zip file name
        zipped.file <- paste0("junk/", file)
        ##the base file name
        base.file <- file %>% sub("_shp.zip", "", .)
        temp.url <- paste0(base.download.url, file)
        ##Download
        utils::download.file(temp.url, zipped.file)
        ##unzip to junk/base.file
        utils::unzip(zipped.file, exdir = paste0("junk/", base.file))
        ##Get the shp file
        shp.file <- list.files(paste0("junk/", base.file), full.names = TRUE) %>%
            .[grepl(".shp$", .)]

        ##Read in the shape file
        sf.out <- read_sf(shp.file) %>%
            st_as_sf(.) %>%
            ##See this SO answer for the CRS:
            ##https://gis.stackexchange.com/a/248081/15525
            st_set_crs(value = 4269)

    })

    ##delete the junk file
    unlink("junk", recursive = TRUE, force = TRUE)

    return(out.sf)
}
