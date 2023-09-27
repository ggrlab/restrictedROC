#' Melt generated data
#'
#' Melt data generated in the form of lists. Given a list or data.frame of elements,
#' melt it from
#' 		columnA columnB columnC
#' into
#' 		Distribution Value
#' 		columnA			xA_1
#' 		columnA			xA_2
#' 		...				...
#' 		columnB			xB_2
#' 		...				...
#' 		columnC			xC_2
#' 		...				...
#'
#' @param df
#' List or data.frame of values.
#'
#' @return
#' 	data.frame with two columns:
#' 		"Distribution"	Contains the name/colname of the respective value
#' 		"Value" 		The value itself
#' @export
#'
#' @examples
#' a <- list("A" = 1:3, "B" = 5:19)
#' melt_gendata(a)
#'
#' a <- data.frame("a" = 1:10, "b" = 6:15)
#' melt_gendata(a)
melt_gendata <- function(df) {
    retdf <- tibble::tibble("Value" = c(), "Distribution" = c())
    for (name_x in names(df)) {
        tmp_df <- tibble::as_tibble(df[[name_x]])
        tmp_df[["Distribution"]] <- name_x
        colnames(tmp_df)[1] <- "Value"
        retdf <- rbind(retdf, tmp_df)
    }
    return(retdf)
}
