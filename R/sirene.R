#' Get company
#'
#' @param siret a valid siret number
#' @param endpoint API endpoint
#'
#' @return a list
#' @export
#'
#' @examples
#'
#' get_company(siret = "38235772100028")
#'
get_company <- function(
  siret,
  endpoint = "https://entreprise.data.gouv.fr/api/sirene/v1") {
  magrittr::extract2(
    httr::content(
      httr::GET(
        paste0(endpoint, "/siret/", siret)
      )
    ),
    "etablissement"
  )
}

#' Get all firms nearby a point
#'
#' @param latitude a double
#' @param longitude a double
#' @param endpoint an url
#'
#' @return a tibble
#' @export
#'
#' @examples
#' get_nearby(latitude = 48.8503, longitude = 2.30831)
#'
get_nearby <- function(
  latitude,
  longitude,
  endpoint = "https://entreprise.data.gouv.fr/api/sirene/v1") {
    httr::content(
      httr::GET(
        paste0(endpoint, "/near_point/", "?lat=", latitude, "&long=", longitude)
      )
    ) %>%
    magrittr::extract2("etablissements") %>%
    purrr::map_df(
      .f = function(x) {
        tibble::as_tibble(
          purrr::flatten(x)
          )
        }
      ) %>%
    dplyr::mutate(
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude)
      )
}

#' Get url of Sirene API User interface
#'
#' @param siret a valid siret number as a string
#' @param endpoint API endpoint
#'
#' @return a string
#' @export
#'
#' @examples
#'
#' get_url(siret = "38235772100028")
#'
#' \dontrun{
#' get_url(siret = "38235772100028") %>% httr::BROWSE()
#' }
#'
get_url <- function(siret, endpoint = "https://entreprise.data.gouv.fr") {
  paste0(endpoint, "/etablissement/", siret)
}

#' Search company
#'
#' search company by name
#'
#' @param string a company name as a string
#' @param endpoint API endpoint
#'
#' @return a list
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#'
#' search_company(string = "dataiku")
#'
search_company <- function(
  string,
  endpoint = "https://entreprise.data.gouv.fr/api/sirene/v1") {

  paste0(endpoint, "/full_text/", string) %>%
    httr::GET() %>%
    httr::content() %>%
    purrr::modify_at(
      .at = "etablissement",
      .f = function(x) {
        purrr::map_df(
          .x = x,
          .f = function(x) {
            tibble::as_tibble(
              purrr::modify_if(
                .x = x,
                .p = purrr::is_null,
                .f = function(x) {
                  NA
                  }
              )
            )
          }
        )
      }
    )
}
