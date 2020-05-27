#' integerxy
#'
#'
#' @param a  a = dataframe, x = position in matrix, y = position in matrix
#' @keywords integer to position in matrix
#' @export
#' @examples
#' integerxy(iris[,1:3])
integerxy = function (a, x=1, y=2)
{

  a.res=a
  uniqx=unique(a[,x])
  uniqy=unique(a[,y])

  for (i in 1:length(uniqx))
  {
    a.res[which(a[,x]==uniqx[i]),x]=i
  }

  for (j in 1:length(uniqy))
  {
    a.res[which(a[,y]==uniqy[j]),y]=j
  }

  res=a.res
  res

}
