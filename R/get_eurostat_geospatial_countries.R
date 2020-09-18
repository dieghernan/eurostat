#' @title Download Geospatial Country Data from GISCO
#' @description Downloads either a simple features (sf), SpatialPolygonDataFrame or a
#'    data_frame preprocessed using
#'    \code{broom::tidy()}.
#' @param output_class A string. Class of object returned, 
#' either \code{sf} \code{simple features}, \code{df} (\code{data_frame}) or
#'    \code{spdf} (\code{SpatialPolygonDataFrame})
#' @param resolution Resolution of the geospatial data. One of
#'    "60" (1:60million),
#'    "20" (1:20million)
#'    "10" (1:10million)
#'    "03" (1:3million) or
#'    "01" (1:1million).
#' @param year NUTS release year. One of
#'    "2001", "2006", "2010", "2013", "2016" or "2020"
#' @param crs projection of the map: 4-digit \href{http://spatialreference.org/ref/epsg/}{EPSG code}. One of:
#' \itemize{
#' \item "4326" - WGS84
#' \item "3035" - ETRS89 / ETRS-LAEA
#' \item "3857" - Pseudo-Mercator
#' }
#' @export
#' @details The data source URL is \url{http://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units}.
#' @author dieghernan, \url{https://github.com/dieghernan/}
#' @return a sf, data_frame or SpatialPolygonDataFrame.
#' @note COPYRIGHT NOTICE
#'
#' When data downloaded from this page
#' \url{http://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units}
#' is used in any printed or electronic publication,
#' in addition to any other provisions
#' applicable to the whole Eurostat website,
#' data source will have to be acknowledged
#' in the legend of the map and
#' in the introductory page of the publication
#' with the following copyright notice:
#' \itemize{
#' 	\item EN: (C) EuroGeographics for the administrative boundaries
#' 	\item FR: (C) EuroGeographics pour les limites administratives
#' 	\item DE: (C) EuroGeographics bezuglich der Verwaltungsgrenzen
#' }
#' For publications in languages other than
#' English, French or German,
#' the translation of the copyright notice
#' in the language of the publication shall be used.
#'
#' If you intend to use the data commercially,
#' please contact EuroGeographics for
#' information regarding their licence agreements.
#'
#' @examples
#'   sf <- get_eurostat_geospatial_countries(output_class = "sf", resolution = "60")
#'   df <- get_eurostat_geospatial_countries(output_class = "df", resolution = "20")
#'   spdf <- get_eurostat_geospatial_countries(output_class = "spdf", resolution = "10")
#'  
get_eurostat_geospatial_countries <- function(output_class = "sf",
                                              resolution = "60",
                                              year = "2016",
                                              crs = "4326") {
  # Check if you have access to ec.europe.eu.
  if (!check_access_to_data()) {
    message(
      "You have no access to ec.europe.eu. Please check your connection and/or review your proxy settings"
    )
  } else {
    # Check resolution is of correct format
    resolution <- as.character(resolution)
    resolution <- gsub("^0+", "", resolution)
    if (!as.numeric(resolution) %in% c(1, 3, 10, 20, 60)) {
      stop("Resolution should be one of 01, 1, 03, 3, 10, 20, 60")
    }
    resolution <- gsub("^1$", "01", resolution)
    resolution <- gsub("^3$", "03", resolution)
    
    # Check output_class is of correct format
    if (!output_class %in% c("sf", "df", "spdf")) {
      stop("output_class should be one of 'sf', 'df' or 'spdf'")
    }
    
    # Check year is of correct format
    year <- as.character(year)
    if (!as.numeric(year) %in% c(2001, 2006, 2010, 2013, 2016, 2020)) {
      stop("Year should be one of 2001, 2006, 2010, 2013, 2016 or 2020")
    }
    
    if (as.numeric(year) == 2001 & as.numeric(resolution) == 60) {
      stop(
        "Countries 2001 is not provided at 1:60 million resolution. Try 1:1 million, 1:3 million, 1:10 million or 1:20 million"
      )
    }
    
    # Check crs is of correct format
    crs <- as.character(crs)
    if (!as.numeric(crs) %in% c(4326, 3035, 3857)) {
      stop("crs should be one of 4326, 3035 or 3857")
    }
    
    url <- paste0("https://gisco-services.ec.europa.eu/distribution/v2/countries/geojson/CNTR_RG_",resolution,"M_",year,"_",crs,".geojson")
    
    resp <- httr::RETRY("GET", url, terminate_on = c(404))
    if (httr::http_error(resp)) {
      stop(
        paste(
          "The requested url cannot be found within the get_eurostat_geospatial function:",
          url
        )
      )
    } else {
      shp <- sf::st_read(
        httr::content(resp, as = "text"),
        stringsAsFactors = FALSE,
        quiet = TRUE
      )
    }
    if (output_class == "df") {
      countries_sp <- as(shp, "Spatial")
      countries_sp$id <- row.names(countries_sp)
      countries_ff    <- broom::tidy(countries_sp)
      shp        <- dplyr::left_join(countries_ff, countries_sp@data)
    }
    if (output_class == "spdf") {
      shp <- as(shp, "Spatial")
    }
    
    return(shp)
  }
}