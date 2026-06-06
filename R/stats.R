#' Summaries captures (ringed and recovered) by day
#'
#' @inheritParams read_gpkg
#' @param project Projectname. defaults to 'IMS'
#' @param years optional. vector of years (e.g. 2023:2026)
#' @param min.count minimum counter per day. defaults to 1
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @import magrittr
#' @import ggplot2
#' @export
#'
ring_history <- function(gpkg, project = 'IMS', years = NULL, min.count = 1, xlab = 'Datum', ylab = 'Tagessume') {

    . <- strProject <- dtmDate <- Year <- Date <- n <- NA

    ## read gpkg
    db <- read_gpkg(gpkg)
    ## combine tables
    df <- rbind(
        ## ringed
        db$tblRinging[,c("strRingNr", "strSpecies", "dtmDate", "strProject")],
        ## recaptured
        db$tblOpen[,c("strRingNr", "strSpecies", "dtmDate", "strProject")]) %>%
        ## filter
        dplyr::filter(., strProject == project) %>%
        dplyr::mutate(Date = lubridate::as_date(dtmDate),
                      Year = lubridate::year(dtmDate))
    if (!is.null(years)) df <- dplyr::filter(df, Year %in% years)

    ## summarise
    df <- df %>%
        dplyr::group_by(Date) %>%
        dplyr::summarise(n = dplyr::n()) %>%
        dplyr::filter(n > min.count)

    mod <- summary(stats::lm(n ~ Date, df))

    p.sig <- mod$coefficients[2,4]
    if (p.sig < 0.001) {
        p.sig = 'p<0.001***'
    } else if (p.sig < 0.01) {
        p.sig = 'p<0.01**'
    } else if (p.sig < 0.05) {
        p.sig = paste0('p=', round(p.sig,2), '*')
    } else if (p.sig > 0.05) {
        p.sig = paste0('p=', round(p.sig,2))
    }
    r.sq <- paste0('r.sq=', round(mod$r.squared,2))

    p <- ggplot(df, aes(x = Date, y = n)) +
        geom_point() +
        labs(title = project,
             x = xlab,
             y = ylab,
             caption = paste0(p.sig, '|', r.sq)) +
        geom_smooth(method = 'lm') +
        egg::theme_presentation()
    p
    return(p)
}
