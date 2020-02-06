#' nainfo
#'
#'
#' @param x x = dataframe
#' @keywords NA
#' @export
#' @examples
#' nainfo(iris)
nainfo = function(x)  {


  sumvec = (sum(is.na(x)))      #(Sum of Na in df)

  locsum = paste("Sum of NA: ", sumvec, sep="")

  if(sum(is.na(x)) > 0)
  {

  loccol = colSums(is.na(x))  #(Sum of columns)
  print(locsum, zero.print= ".")
  print(loccol, zero.print = ".")
  }

  else {

  print("NO NA's in dataframe!!!")
      }
}
