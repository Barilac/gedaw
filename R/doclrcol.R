#' doclrcol
#'
#' This function allows you to process clr transformation in columns.
#' @param base The logarithm base is set to 10
#' @param a for dim
#' @keywords clr
#' @export
#' @examples
#' doclrcol(gpr**2)

doclrcol = function (a, base=10)

{

	for (i in c(1:dim(a)[2]))

	{

	a[,i]=log(a[,i]/gmean(a[,i]), base=base)

	}

	a

}
