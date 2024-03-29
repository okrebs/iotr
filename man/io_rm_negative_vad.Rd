% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io_rm_negative_vad.R
\name{io_rm_negative_vad}
\alias{io_rm_negative_vad}
\title{Remove Negative VAD from IO Table}
\usage{
io_rm_negative_vad(iot, category_to_scale)
}
\arguments{
\item{iot}{An input-output table in long format with the columns,
\code{origin}, \code{sector}, \code{destination}, \code{use} and
\code{flow}}

\item{category_to_scale}{the use category (as integer) which to scale if
output has to be increased due to an imputed negative VAD}
}
\value{
Returns an IO table in long format as a \code{tibble} with the
columns \code{origin}, \code{sector}, \code{destination}, \code{use} and
\code{flow}
}
\description{
Remove negative imputed value added (VAD) from an input-output (IO) table by
scaling the total output and world wide use of the specific location-sector.
}
\details{
In location-sectors where a given IO table reports intermediate use
larger than output value, i.e. where imputed VAD would be negative, this
function increases output until the imputed VAD matches the smallest share of
VAD in output found in any other location-sector in the IO table. To account
for the destination of the additional production the use category
\code{category_to_scale} is increased around the world to match the increased
production. If no negative value added is imputed the IO table is returned
unchanged.
}
\examples{
\dontrun{

library(iotr)

# set cache_dir for WIOD to avoid long download times on every run !!!
cache_dir <- NULL

wiot_raw <- io_load_wiot(cache_dir, years = 2000:2014)

wiot_long <- io_tidy_wiot(wiot_raw)

# get wiot into standard long IO-table format
iot <- dplyr::filter(wiot_long, Country != "TOT", Year == "2014")
iot <-
  dplyr::select(iot,
                origin = Country,
                sector = RNr,
                destination,
                use,
                flow)

iot <- io_rm_negative_vad(iot, category_to_scale = 57)

iot <-
  io_rm_dynamics(iot,
                 dynamic_categories = c(60, 61),
                 category_to_scale = 57)

iot <- io_gen_own_trade(iot, max_replace = 1e-6)

}
}
