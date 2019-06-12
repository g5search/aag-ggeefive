#' Theme inspired by G5 Branding
#'
#' Theme inspired by the plots on
#' \href{G5}{http://getg5.com}.
#'
#' @inheritParams ggplot2::theme_grey
#' @family themes g5
#' @export
#' @importFrom grid unit
#' @import ggplot2
#' @import scales
#' @import ggthemes
#' @import dplyr
#' @importFrom tibble tibble
#' @importFrom tibble deframe
theme_geefive<- function(base_size = 12, base_family = "mono") {
  geefive_data <- tibble(name = c("Midnight Blue",
                                  "Blue Gray",
                                  "Vermillion",
                                  "Light Gray"),
                         values = c("#0d2240",
                                    "#7c98ab",
                                    "#e94f3d",
                                    "#cbd6dd"))

  colors <- deframe(geefive_data)
  (theme_foundation(base_size = base_size, base_family = base_family)
    + theme(
      line = element_line(colour = "black"),
      rect = element_rect(fill = "white",
                          linetype = 0, colour = NA),
      text = element_text(colour = colors["Blue Gray"]),
      axis.title = element_blank(),
      axis.text = element_text(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      legend.background = element_rect(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
      panel.grid = element_line(colour = NULL),
      panel.grid.major =
        element_line(colour = colors["Blue Gray"]),
      panel.grid.minor = element_blank(),
      # unfortunately, can't mimic subtitles TODO!
      plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold"),
      plot.margin = unit(c(1, 1, 1, 1), "lines"),
      strip.background = element_rect()))
}

#' G5 color palette
#'
#' The standard three-color G5 palette for line plots comprises
#' blue, blue-grey, vermillion
#'
#' @family colour geefive
#' @export
#' @importFrom grid unit
#' @import ggplot2
#' @import scales
#' @import ggthemes
#' @import dplyr
#' @importFrom tibble tibble
#' @importFrom tibble deframe
geefive_pal <- function() {
  geefive_data <- tibble(name = c("Midnight Blue",
                                  "Blue Gray",
                                  "Vermillion"),
                         values = c("#0d2240",
                                    "#7c98ab",
                                    "#e94f3d"))

  colors <- deframe(geefive_data)
  values <- unname(colors[c("Midnight Blue", "Blue Gray", "Vermillion")])
  max_n <- length(values)
  f <- manual_pal(values)
  attr(f, "max_n") <- max_n
  f
}

#' G5 color scales
#'
#' Color scales using the colors in the geefivegraphics.
#'
#' @inheritParams ggplot2::scale_colour_hue
#' @family colour geefive
#' @rdname scale_geefive
#' @seealso \code{\link{theme_geefive}()} for examples.
#' @export
scale_colour_geefive <- function(...) {
  discrete_scale("colour", "economist", geefive_pal(), ...)
}

#' @rdname scale_geefive
#' @export
scale_color_geefive <- scale_colour_geefive

#' @rdname scale_geefive
#' @export
scale_fill_geefive<- function(...) {
  discrete_scale("fill", "economist", geefive_pal(), ...)
}
