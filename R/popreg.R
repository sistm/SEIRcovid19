#'Population numbers in France
#'
#'A dataset containing the populations of France as well as broken down by region
#'as of 2020-01-01 (according to INED)
#'
#' @format A data frame with 19 rows and 3 variables:
#' \describe{
#'   \item{maille_nom}{name in French}
#'   \item{maille_code}{administrative code}
#'   \item{population}{population}
#' }
#'
#'@examples
#'data(popreg)
#' # popreg <- read.delim(system.file("extdata",
#' #  "population_regionsFR_INED1erjan2020.tsv",
#' #  encoding = "UTF8"))
#'
#'@source INED \url{https://www.ined.fr/fichier/s_rubrique/159/estim.pop.nreg.sexe.gca.1975.2020.fr.xls}
#'stored in \code{'data/raw/population_regionsFR_INED1erjan2020.tsv'}
#'
#'
"popreg"
