#'
#' scale_x_cont_dates
#' 
#' @description Una función que crea una escala x para fechas de negocios. De esta manera permite que las 
#' barras de los ejes estén todas contiguas. Sino separa por semanas.
#' Tuve que hacer un override de la función scale_x_bd de la libreria `bdscale` porque hardcodearon el nombre
#' del eje x y no me dejaba cambiarlo.
#' 
#' @param ... Argumentos adicionales que se pasan a la función scale_x_continuous
#' @param name Nombre del eje x
#' @param business.dates Un vector de fechas de negocios. Acá le suelo pasar el vector de fechas de los días que tengo en el df con el que lo llamo
#' @param max.major.breaks Cantidad máxima de breaks mayores
#' @param max.minor.breaks Cantidad máxima de breaks menores
#' @param breaks Los breaks que se van a usar. Por defecto se usa la función `bd_breaks` de la librería `bdscale`
#' 
#' @return Una escala x para fechas de negocios
#' 
scale_x_cont_dates = function (..., name = name, business.dates, max.major.breaks = 5, max.minor.breaks = max.major.breaks * 
    5, breaks = bd_breaks(business.dates)) 
{
    scale_x_continuous(name = name, breaks = breaks(max.major.breaks), 
        minor_breaks = breaks(max.minor.breaks), trans = bd_trans(business.dates, 
            breaks), ...)
}


