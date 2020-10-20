#' Download and import precipitation data from various sources
#'
#' The function \code{download_data} downloads the selected data product.
#'
#' @importFrom stringr str_pad
#' @importFrom getPass getPass
#' @importFrom utils download.file
#' @param name a character string with the name of the desired data set. Suitable options are:
#' \itemize{
#' \item{"20cr" for 20CR v3}
#' \item{"cmap" for CMAP standard version,}
#' \item{"cpc" for CPC-Global,}
#' \item{"cru" for CRU_TS v4.04,}
#' \item{"ghcn" for GHCN-M v2}
#' \item{"gpcc" for GPCC v2018,}
#' \item{"gpcp" for GPCP v2.3,}
#' \item{"gpm" for GPM IMERG Final v06,}
#' \item{"ncep" for NCEP/NCAR,}
#' \item{"ncep2" for NCEP/DOE,}
#' \item{"precl" for PRECL,}
#' \item{"trmm" for TRMM 3B43 v7,}
#' \item{"udel" for UDEL v501.}
#' }
#' @param destination a character string with the path where the downloaded file is saved.
#' @param resolution numeric. Data spatial resolution for GPCC or PRECL. Suitable options are:
#' \itemize{
#' \item{0.25 for 0.25 degree (GPCC only),}
#' \item{0.5 for 0.5 degree,}
#' \item{1 for 1 degree,}
#' \item{2.5 for 2.5 degree.}
#' }
#' @param start_year numeric. Start year should be between 1979-2019 (CPC), 2000-2019 (GPM), or 1998-2019 (TRMM).
#' @param end_year numeric. End year should be between 1979-2019 (CPC), 2000-2019 (GPM), or 1998-2019 (TRMM), and should be greater or equal to start year.
#' @export

download_data <- function(name, destination, resolution = NULL, start_year = NULL, end_year = NULL){
  '%!in%' <- function(x, y)!('%in%'(x, y))
  if (name %!in% c("20cr", "cmap", "cpc", "cru", "ghcn", "gpcc", "gpcp", "gpm", "ncep", "ncep2", "precl", "trmm", "udel")){
    stop("Error: Data set not supported. Select one of 20cr, cmap, cpc, cru, ghcn, gpcc, gpcp, gpm, ncep, ncep2, precl, trmm, udel")
  }
  switch(name,
         "20cr" = download_20cr(destination),
         "cmap" = download_cmap(destination),
         "cpc" = download_cpc(destination, start_year, end_year),
         "cru" = download_cru(destination),
         "ghcn" = download_ghcn(destination),
         "gpcc" = download_gpcc(destination, resolution),
         "gpcp" = download_gpcp(destination),
         "gpm" = download_gpm(destination, start_year, end_year),
         "ncep" = download_ncep(destination),
         "ncep2" = download_ncep2(destination),
         "precl" = download_precl(destination, resolution),
         "trmm" = download_trmm(destination, start_year, end_year),
         "udel" = download_udel(destination)
  )
}

#' Download and import precipitation data from various sources
#'
#' The function \code{import_data} downloads the selected data product.
#'
#' @importFrom dplyr %>%
#' @importFrom R.utils gunzip
#' @param name a character string with the name of the desired data set. Suitable options are:
#' \itemize{
#' \item{"20cr" for 20CR v3}
#' \item{"cmap" for CMAP standard version,}
#' \item{"cpc" for CPC-Global,}
#' \item{"cru" for CRU_TS v4.04,}
#' \item{"ghcn" for GHCN-M v2}
#' \item{"gpcc" for GPCC v2018,}
#' \item{"gpcp" for GPCP v2.3,}
#' \item{"gpm" for GPM IMERG Final v06,}
#' \item{"ncep" for NCEP/NCAR,}
#' \item{"ncep2" for NCEP/DOE,}
#' \item{"precl" for PRECL,}
#' \item{"trmm" for TRMM 3B43 v7,}
#' \item{"udel" for UDEL v501.}
#' }
#' @param folder_path a character string with the folder path where the data set is located.
#' @export

import_data <- function(name, folder_path){
  '%!in%' <- function(x, y)!('%in%'(x, y))
  if (name %!in% c("20cr", "cmap", "cpc", "cru", "ghcn", "gpcc", "gpcp", "gpm", "ncep", "ncep2", "precl", "trmm", "udel")){
    stop("Error: Data set not supported. Select one of 20cr, cmap, cpc, cru, ghcn, gpcc, gpcp, gpm, ncep, ncep2, precl, trmm, udel")
  }
  switch(name,
         "20cr" = {precip_20cr <<- import_20cr(folder_path)},
         "cmap" = {precip_cmap <<- import_cmap(folder_path)},
         "cpc" = {precip_cpc <<- import_cpc(folder_path)},
         "cru" = {precip_cru <<- import_cru(folder_path)},
         "ghcn" = {precip_ghcn <<- import_ghcn(folder_path)},
         "gpcc" = {precip_gpcc <<- import_gpcc(folder_path)},
         "gpcp" = {precip_gpcp <<- import_gpcp(folder_path)},
         "gpm" = {precip_gpm <<- import_gpm(folder_path)},
         "ncep" = {precip_ncep <<- import_ncep(folder_path)},
         "ncep2" = {precip_ncep2 <<- import_ncep2(folder_path)},
         "precl" = {precip_precl <<- import_precl(folder_path)},
         "trmm" = {precip_trmm <<- import_trmm(folder_path)},
         "udel" = {precip_udel <<- import_udel(folder_path)}
  )
}