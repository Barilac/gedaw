#' clr
#'
#' Source: https://cran.r-project.org/web/packages/robCompositions/index.html
#' @param x dataset
#' @keywords clr tranformation accoring to the robCompositions
#' @export
#' @examples
#' data(iris)
#' clr(iris[,1:4])
#' biplot(clr(iris[,1:4]))


clr = function (x)
{
  robCompositions::cenLR(x)
}
