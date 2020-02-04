#' gmean
#'
#'
#' @param na na=T
#' @param x dataset
#' @param zero zero=Ts
#' @keywords gmean
#' @export
#' @examples
#' gmean(gpr)
gmean=function(x, zero=T, na=T){

  # negative values are present

  if(length(x[x<0 & !is.na(x)])>0) {

    result=c("Input data contains negative values\n\n")
    neg.val=x[x<0 & !is.na(x)]
    neg.val.pos=which(x<0 & !is.na(x))
    cat(result)
    return(list("negative values" = neg.val, "positions of negative values" = neg.val.pos))

  } else {

    # ignore zeros

    if (zero) {

      # ignore NAs

      if (na) {

        data=(x[x > 0])
        gmean=exp(sum(log(data), na.rm=T)/length(data[!is.na(data)]))
        return(gmean)

      } else {

        # do not ignore NAs

        data=(x[x > 0])
        gmean=exp(sum(log(data), na.rm=T)/length(data))
        return(gmean)

      }} else {

        # do not ignore zeros

        return(0)

      }}}
