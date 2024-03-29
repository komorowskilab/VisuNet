#' Rule set from the classifier of young males with autism and healthy individuals.
#'
#'
#' The rule-based classifier was created for the gene expression levels of 82 autistic and 64 non-autistic (control) male children.
#' The data has been preprocessed and the 35 most corellated genes were selected (see \href{https://www.biorxiv.org/content/10.1101/625905v1.full}{Garbulowski et al. (2019)})
#' The rule set was obtained using the rule-based classifier created by a \code{\link[R.ROSETTA]{rosetta}} function from the \pkg{R.ROSETTA} package.
#'
#' @format A data frame with 191 rows and 5 variables. Each row corresponds to one rule:
#' \describe{
#'   \item{features}{comma-separated conditions (feature=value)}
#'   \item{decision}{a rule decision}
#'   \item{accuracyRHS}{a rule accuracy value}
#'   \item{supportRHS}{a rule support value}
#'   \item{pValue}{a rule p-value value}
#'}
#' @docType data
#'
#' @usage autcon_ruleset
#'
#' @references Alter et al. (2011) PloS one, e16715
#' (\href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3040743/}{PubMed})
#'
#' @source \href{https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE25507}{GEO DataSet}
#'
"autcon_ruleset"
