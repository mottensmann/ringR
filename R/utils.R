#' Read database
#'
#' @param gpkg path to a RING2.1.gpkg file
#' @export
#'
read_gpkg <- function(gpkg = NULL) {

    ring <- lapply(c('tblRinging', 'tblOpen', 'tblRefer', 'tblGeotab'), sf::read_sf, dsn = gpkg)
    names(ring) <- c('tblRinging', 'tblOpen', 'tblRefer', 'tblGeotab')
    EURING <- NA
    load(system.file("extdata", "EURING.RData", package = "ringR"))

    ring$tblRinging$strSpeciesDE <-
        dplyr::left_join(
            ring$tblRinging[,"strSpecies"],
            EURING$TLKPSPECIES,
            by = c("strSpecies" = "STRCODE"))[["STRSPECIES"]]

    ring$tblOpen$strSpeciesDE <-
        dplyr::left_join(
            ring$tblOpen[,"strSpecies"],
            EURING$TLKPSPECIES,
            by = c("strSpecies" = "STRCODE"))[["STRSPECIES"]]

    return(ring)
}

#' Visualise recoveries
#'
#' @param ring Ring Nr
#' @param data Already-loaded list returned by \code{read_gpkg()}
#' @param limits Optional. Vector der Form c(xmin, xmax, ymin, ymax)
#' @param scale c("medium", "large")
#' @param point_size numeric
#' @param line_width numeric
#' @export
#'
plot_resights <- function(ring, data,
                    point_size = 4,
                    line_width = 0.75,
                    limits = NULL,
                    scale = c("medium", "large")) {

    strRingNr <- ggplot <- geom_sf <- theme_minimal <- geom_line <- aes <-
        x <- y <- geom_point <- Typ <- geom_text <- Date <- scale_fill_brewer <-
        coord_sf <- annotation_scale <- annotation_north_arrow <- unit <-
        north_arrow_nautical <- theme <- element_blank <- element_rect <-
        margin <- NULL

    scale <- match.arg(scale)

    ## Selektiere Berinungen und Funde------------------------------------------
    beringung <- dplyr::filter(data$tblRinging, strRingNr == ring)
    funde <- dplyr::filter(data$tblOpen, strRingNr == ring)

    df <- rbind(data.frame(strRingNr = ring,
                           Typ = 'Beringung',
                           x = beringung$lngLong,
                           y = beringung$lngLat,
                           Date = substr(beringung$dtmDate,1, 10)),
                data.frame(strRingNr = ring,
                           Typ = 'Wiederfund',
                           x = funde$lngLong,
                           y = funde$lngLat,
                           Date = substr(funde$dtmDate, 1,10)))


    ## background map ----------------------------------------------------------
    world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

    if (requireNamespace("ggplot2", quietly = TRUE)) {
        # Plot the map. group = group connects the points in the correct order
        map <-
            ggplot2::ggplot(data = world) +
            ggplot2::geom_sf() +
            ggplot2::theme_minimal() +
            ggplot2::geom_line(data = df, ggplot2::aes(x = x, y = y, group = strRingNr), linewidth = line_width) +
            ggplot2::geom_point(data = df,  ggplot2::aes(x = x, y = y, group = Typ, colour = Typ), size = point_size) +
            ggplot2::geom_text(data = df,  ggplot2::aes(x = x, y = y, group = Typ, label = Date, hjust = -0.1)) +
            ggplot2::scale_fill_brewer("Set2") +
            ggplot2::theme(
                axis.title = ggplot2::element_blank(),
                legend.title = ggplot2::element_blank(),
                legend.position = 'inside',
                legend.justification = c(0, 1),
                legend.direction = "vertical",
                legend.background = ggplot2::element_rect(fill = "white", colour = "black",
                                                          ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")))


        if (!is.null(limits)) {
            map <- map +
                ggplot2::coord_sf(xlim = c(limits[1], limits[2]), ylim = c(limits[3], limits[4]), expand = FALSE)
        }
    } else {
        warning('Please install package ggplot2!')
    }


    if (exists("map")) return(map)

}
