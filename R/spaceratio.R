# Space Ratio and the Space Ratio Chart
#
#' Function for plotting Space Ratio Charts or deriving a data frame of summary statistics.
#'
#' @param density_var A vector of density variables
#' @param existing_density A vector containing the existing density values per density variable in density_var. Input only if space_ratio not inputted
#' @param permissible_density A vector containing the permissible density values per density variable in density_var. Input only if space_ratio not inputted
#' @param space_ratio A vector of Space Ratio values per density variable in density_var. Input only if existing_density and permissible_density not inputted
#' @param output Output of function. Input 'plot' for the Space Ratio Chart. Input 'stats' for a data frame of summary statistics. Defaults to 'plot'
#' @importFrom dplyr "%>%"
#' @export
#' @return A Space Ratio Chart or a data frame of summary stats.
#' @examples
#' # Density variables
#' d_var <- c("FAR", "DPH", "H", "GSI")
#'
#' # Space Ratio values
#' s_val <- c(0.73, 1, 0.81, 0.91)
#'
#' # Space Ratio Chart
#' spaceratio(density_var = d_var, space_ratio = s_val, output = 'plot')
#'
#' # Summary Stats
#' spaceratio(density_var = d_var, space_ratio = s_val, output = 'stats')
#' @export

spaceratio <- function(density_var, existing_density = NULL, permissible_density = NULL, space_ratio = NULL, output = 'plot') {

  if(is.null(space_ratio) && is.null(existing_density) != T && is.null(permissible_density) != T) {
    if(min(existing_density) < 0 && min(permissible_density) < 0) {
      stop('existing_density and permissible_density values should be >= 0', call. = F)

    } else if(min(existing_density) < 0 && min(permissible_density) >= 0) {
      stop('existing_density values should be >= 0', call. = F)

    } else if(min(permissible_density) < 0 && min(existing_density) >= 0) {
      stop('permissible_density values should be >= 0', call. = F)

    } else {
      spaceratio_df <- data.frame(density_var, existing_density, permissible_density) %>%
        dplyr::mutate(space_ratio = dplyr::if_else(existing_density > permissible_density, 1, existing_density / permissible_density)) %>%
        dplyr::mutate(space_ratio = dplyr::if_else(permissible_density == 0, 1, space_ratio)) %>%
        dplyr::mutate(latent_space = 1 - space_ratio) %>%
        reshape2::melt(id.vars = "density_var") %>%
        dplyr::rename(space_ratio = value, space_var = variable, spaceratio_var = density_var) %>%
        dplyr::mutate(spaceratio_var = factor(spaceratio_var, levels = density_var)) %>%
        dplyr::filter(space_var == "space_ratio" | space_var == "latent_space")
    }

  } else if(is.null(space_ratio) != T && is.null(existing_density) && is.null(permissible_density)) {
    if(max(space_ratio) > 1 || min(space_ratio) < 0) {
      stop('space_ratio values should be >= 0 and <= 1', call. = F)

    } else {
      spaceratio_df <- data.frame(density_var, space_ratio) %>%
        dplyr::mutate(latent_space = 1 - space_ratio) %>%
        reshape2::melt(id.vars = "density_var") %>%
        dplyr::rename(space_ratio = value, space_var = variable, spaceratio_var = density_var) %>%
        dplyr::mutate(spaceratio_var = factor(spaceratio_var, levels = density_var))
    }

  } else {
    stop('error in input of variables', call. = F)
  }

  if(output == 'plot') {
    spaceratio_chart <- ggplot2::ggplot(spaceratio_df, ggplot2::aes(x = spaceratio_var, y = space_ratio, fill = space_var)) +
      ggplot2::geom_bar(stat = 'identity', color = "#bebebe", width = 0.9990, position = ggplot2::position_fill(reverse = T)) +
      ggplot2::scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("0", "0.25", "0.5", "0.75", "1")) +
      ggplot2::scale_fill_manual(values = c('space_ratio' = "#000000", 'latent_space' = "#ffffff"), breaks = c("space_ratio"), labels = c("SPACE RATIO")) +
      ggplot2::labs(x = "", y = "") +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_line(color = "#ffffff", linewidth = 0.2),
                     panel.grid.minor.y = ggplot2::element_blank(),
                     panel.grid.major.x = ggplot2::element_blank(),
                     panel.grid.minor.x = ggplot2::element_blank(),
                     panel.ontop = T,
                     plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
                     plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5),
                     axis.text.x = ggplot2::element_text(size = 11, face = "bold"),
                     legend.position = 'top',
                     legend.justification = 'centre',
                     legend.direction = "horizontal",
                     legend.key.width = ggplot2::unit(2.74, "cm"),
                     legend.key.height = ggplot2::unit(0.22, "cm"),
                     legend.title = ggplot2::element_blank(),
                     legend.text = ggplot2::element_text(size = 9, face = "bold")) +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, label.position = "top", label.vjust = -1.5))

    spaceratio_chart

  } else if(output == 'stats') {
    if(is.null(existing_density) != T && is.null(permissible_density) != T) {
      spaceratio_df <- spaceratio_df %>%
        dplyr::rename(space_ratio = space_ratio, density_var = spaceratio_var) %>%
        dplyr::filter(space_var == "space_ratio") %>%
        dplyr::select(space_ratio)

      spaceratio_stats <- data.frame(density_var, existing_density, permissible_density, spaceratio_df$space_ratio) %>%
        dplyr::rename(space_ratio = spaceratio_df.space_ratio) %>%
        tibble()

      spaceratio_stats

    } else {
      spaceratio_stats <- spaceratio_df %>%
        dplyr::rename(space_ratio = space_ratio, density_var = spaceratio_var) %>%
        dplyr::filter(space_var == "space_ratio") %>%
        dplyr::select(density_var, space_ratio) %>%
        dplyr::tibble()

      spaceratio_stats
    }

  } else {
    stop('error in output parameter', call. = F)
  }
}
