#' CI t table
#'
#' Computes CI based on t table
#' @param x numeric variable 
#' @param ci CI level
#' @return Returns a tibble with sample size, mean, sd, Margin error, CI_l and CI_U
#' @examples 
#' vec <- rnorm(15, mean = 90, sd = 15)
#' CI_t(vec, ci=0.95)
#' @export
CI_t <-  function (x, ci = 0.95)
{
  `%>%` <- magrittr::`%>%`
  Margin_Error <- qt(ci + (1 - ci)/2, df = length(x) - 1) * sd(x)/sqrt(length(x))
  df_out <- data.frame(  sample_size=length(x), Mean=mean(x), sd=sd(x),
                         Margin_Error=Margin_Error,
                         'CI lower limit'=(mean(x) - Margin_Error),
                         'CI Upper limit'=(mean(x) + Margin_Error)) %>%
    tidyr::pivot_longer(names_to = "Measurements", values_to ="values", 1:6 )

  return(df_out)
}


#' CI z table
#'
#' Computes CI based on z table
#' @param x numeric variable 
#' @param ci CI level
#' @return Returns a tibble with sample size, mean, sd, Margin error, CI_l and CI_U
#' @examples 
#' vec <- rnorm(100, mean = 90, sd = 15)
#' CI_z(vec, ci=0.95)
#' @export
CI_z <-  function (x, ci = 0.95)
{
  `%>%` <- magrittr::`%>%`
  standard_deviation <- sd(x)
  sample_size <- length(x)
  Margin_Error <-  abs(qnorm((1-ci)/2))* standard_deviation/sqrt(sample_size)
  df_out <- data.frame(  sample_size=length(x), Mean=mean(x), sd=sd(x),
                         Margin_Error=Margin_Error,
                         'CI lower limit'=(mean(x) - Margin_Error),
                         'CI Upper limit'=(mean(x) + Margin_Error)) %>%
    tidyr::pivot_longer(names_to = "Measurements", values_to ="values", 1:6 )

  return(df_out)
}
