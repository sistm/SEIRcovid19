% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ICUcapacity_FR.R
\docType{data}
\name{ICUcapacity_FR}
\alias{ICUcapacity_FR}
\title{Numbers of ICU beds in France}
\format{
A data frame with 19 rows and 2 variables
}
\source{
DREEES 2018 \url{https://www.sae-diffusion.sante.gouv.fr/sae-diffusion/recherche.htm}
obtaine on 2020-03-25 and stored in
\code{'data/raw/capacite_rea_regions_saediffusiondrees.txt'}
}
\usage{
ICUcapacity_FR
}
\description{
A dataset containing the numbers of ICU beds in France and in each region
according to DREES as of 2018-12-31
}
\examples{
data(ICUcapacity_FR)
#ICUcapacity_FR <- read.delim("data/raw/capacite_rea_regions_saediffusiondrees.txt")

}
\keyword{datasets}
