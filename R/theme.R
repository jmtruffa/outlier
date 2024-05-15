#' theme_outlier
#' 
#' Define la estética de los gráficos
#' agrega dos variables globales .pie y .paleta para poder ser usada en la construcción de gráficos
#' @return objeto devuelto por theme_grey con las características de outlier

theme_outlier <- function (base_size = 11, base_family = "Microsoft Sans Serif") 
{
  .pie <<- "Outlier"
  .paleta <<- c("#FF6600", "#00003a", "#A6CAEC", "#F8CBAD", "#000000" )
  
  blue <- "#2c3e50"
  green <- "#18BC9C"
  white <- "#FFFFFF"
  grey <- "grey80"
  
  ggplot2::theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    ggplot2::theme(
      line = ggplot2::element_line(colour = blue, linewidth = 1, linetype = 1, lineend = "butt"), 
      rect = ggplot2::element_rect(fill = white, colour = blue, linewidth = 0.5, linetype = 1), 
      text = ggplot2::element_text(size = 15),

      axis.line = ggplot2::element_blank(), 
      axis.text = ggplot2::element_text(size = ggplot2::rel(0.8)), 
      axis.ticks = ggplot2::element_line(color = grey, size = ggplot2::rel(1/3)), 
      axis.title = ggplot2::element_text(size = ggplot2::rel(0.8)), 
      
      panel.background = ggplot2::element_rect(fill = white, color = NA), 
      panel.border = ggplot2::element_rect(fill = NA, size = ggplot2::rel(1/2), color = blue), 
      panel.grid.major = ggplot2::element_line(color = grey, size = ggplot2::rel(1/3)), 
      panel.grid.minor = ggplot2::element_line(color = grey, size = ggplot2::rel(1/3)), 
      panel.grid.minor.x = ggplot2::element_blank(), 
      panel.spacing = ggplot2::unit(0.75, "cm"), 
      
      panel.grid.major.x = ggplot2::element_blank() ,
      panel.grid.minor.y = ggplot2::element_blank(),
      
      legend.key = ggplot2::element_rect(fill = white, color = NA), 
      legend.position = "bottom", 
      legend.text = element_text(size = 15),
      legend.background = ggplot2::element_blank(), 
      
      strip.background = ggplot2::element_rect(fill = blue, color = blue), 
      strip.text = ggplot2::element_text(color = white, size = ggplot2::rel(0.8), margin = ggplot2::margin(t = 5, b = 5)), 
      
      plot.title = ggplot2::element_text(face = "bold", size = 20, hjust = 0, margin = ggplot2::margin(t = 0, r = 0, b = 4, l = 0, unit = "pt")), 
      plot.subtitle = ggplot2::element_text(size = 16, hjust = 0, margin = ggplot2::margin(t = 0, r = 0, b = 3, l = 0, unit = "pt"), color="#FF6600"), 
      
      complete = FALSE,
    )
}
