% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io_tidy_eora26.R
\name{io_tidy_eora26}
\alias{io_tidy_eora26}
\title{Tidy data from the EORA26 MRIO}
\usage{
io_tidy_eora26(eora26, quiet = FALSE)
}
\arguments{
\item{eora26}{list containing EORA26 years with the components T, VA, FD and
and element with the corresponding labels}

\item{quiet}{if TRUE will try to avoid printing messages}
}
\value{
Returns one \code{tibble} combining all components and years from
\code{eora26}.
}
\description{
Function to turn a list (one element for each year) of EORA MRIO components,
usually obtained from a call to \code{io_load_eora26()}, into a tibble, i.e.
a 'tidy' data frame.
}
\examples{
\dontrun{

  library(iotr)

  # set cache_dir for EORA to avoid (very) long download times on every run !!!
  cache_dir <- NULL

  eora26_raw <- io_load_icio_v2(cache_dir, years = 2016:2017)

  eora26 <- io_tidy_eora26(eora26_raw)
}
}
\references{
Lenzen M, Kanemoto K; Moran D, and Geschke A (2012) Mapping the
structure of the world economy. Environmental Science & Technology 46(15)
pp 8374–8381.

Lenzen, M., Moran, D., Kanemoto, K., Geschke, A. (2013) Building Eora: A
Global Multi-regional Input-Output Database at High Country and Sector
Resolution. Economic Systems Research, 25:1, 20-49.
}
