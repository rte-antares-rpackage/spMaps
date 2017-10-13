#' @rdname antaresMaps
#' @export
getEuropeReferenceTable <- function(){
  data(europe_countries_ref)
  europe_countries_ref$name <- as.character(europe_countries_ref$name)
  europe_countries_ref$code <- as.character(europe_countries_ref$code)
  europe_countries_ref
}

#' Get custom Europe map (\code{SpatialPolygonsDataFrame})
#'
#' This function builds a custom Europe map and return a \code{SpatialPolygonsDataFrame}.
#' The output can be use in \link{mapLayout} with the \code{map} argument.
#'
#' @param countries \code{character}. Vector of wanted countries, without details / states.
#'   Must referred to \code{code} column of the reference table \code{getEuropeReferenceTable}.
#'   "all" (default) keep all countries
#'
#' @param states \code{character}. Vector of wanted countries, with details / states.
#'   Must referred to \code{code} column of the reference table \code{getEuropeReferenceTable}.
#'   "all" keep all countries. NULL as default.
#'
#' @return  \code{SpatialPolygonsDataFrame}
#'
#' @examples
#'
#' # default map : Europe without states
#' europe_cty <- getAntaresMap()
#' plot(europe_cty)
#'
#' # subset on some countries
#' ref_table <- getEuropeReferenceTable()
#'
#' italy_spain_fra <- getAntaresMap(countries = c("FRA", "ITA", "ESP"))
#' plot(italy_spain_fra)
#'
#' italy_spain_fra_states <- getAntaresMap(countries = NULL, states = c("FRA", "ITA", "ESP"))
#' plot(italy_spain_fra_states)
#'
#' # combine countries and states
#' combine_map <- getAntaresMap(countries = c("ITA", "ESP"), states = "FRA")
#' plot(combine_map)
#'
#' # build your custom map : you can use directly data
#' # to subset the area you really want
#' data(europe_states_provinces_10m)
#' data(europe_countries_10m)
#'
#' # for example, have a look to GBR states map
#' summary(europe_states_provinces_10m)
#' gbr_states_districts <- europe_states_provinces_10m[
#'    europe_states_provinces_10m$sr_adm0_a3 %in% "GBR" &
#'    europe_states_provinces_10m$type %in% "Administrative County",]
#' plot(gbr_states_districts)
#'
#' # combine with another map : you just have to have the same columns...
#' # getAntaresMap only return "name" column
#' custom_states <- rbind(
#'     getAntaresMap(countries = NULL, states = "FRA"),
#'     gbr_states_districts[, "name", drop = FALSE])
#'
#' plot(custom_states)
#'
#' @export
#'
#' @import sp
#'
#' @name antaresMaps
#'
getAntaresMap <- function(countries = "all", states = NULL){

  # controls
  if(is.null(countries) & is.null(states)){
    message("No countries and no states selected")
    return(NULL)
  }

  # reference table
  ref_table <- getEuropeReferenceTable()

  # don't show countries if in states
  countries <- setdiff(countries, states)
  if(length(countries) == 0) countries <- NULL

  # countries
  if(!is.null(countries)){
    stopifnot(all(countries %in% c("all", ref_table$code)))
    data(europe_countries_10m)
    if(!"all" %in% countries){
      countries_data <- europe_countries_10m[europe_countries_10m$adm0_a3 %in% countries, ]
    } else {
      countries_data <- europe_countries_10m
    }
  } else {
    countries_data <- NULL
  }

  # states
  if(!is.null(states)){
    stopifnot(all(states %in% c("all", ref_table$code)))

    data(europe_states_provinces_10m)
    if(!"all" %in% states){
      states_data <- europe_states_provinces_10m[europe_states_provinces_10m$sr_adm0_a3 %in% states, ]
    } else {
      states_data <- europe_states_provinces_10m
    }
  } else {
    states_data <- NULL
  }

  if(!is.null(countries_data) & is.null(states_data)){
    return(countries_data[, c("name"), drop = FALSE])
  } else if(is.null(countries_data) & !is.null(states_data)){
    return(states_data[, c("name"), drop = FALSE])
  } else {
    return(rbind(countries_data[, c("name"), drop = FALSE],
                 states_data[, c("name"), drop = FALSE]))
  }
}

