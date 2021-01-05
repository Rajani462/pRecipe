% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reformatters.R
\name{reformat_cru_ts}
\alias{reformat_cru_ts}
\title{CRU data reformatter}
\usage{
reformat_cru_ts(folder_path)
}
\arguments{
\item{folder_path}{a character string with the path to the "raw" folder.}
}
\description{
Function for reading CRU_TS NC.GZ file, and reformatting them into data.table which is stored in an .Rds file.
}