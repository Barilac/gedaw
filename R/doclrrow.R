#' doclrrow
#'
#' This function allows you to process clr transformation in rows.
#' @param base The logarithm base is set to 10
#' @param a for dim
#' @param zero zero=T
#' @param na na=T
#' @keywords clr
#' @export
#' @examples
#' doclrrow(gpr**2)
doclrrow = function (a, zero=T, na=T, base=10)

{

	for (i in c(1:dim(a)[1]))

	{

	a[i,]=log(a[i,]/gmean(a[i,]), base=base)

	}

	a

}
