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
#' @export
scale_x_cont_dates = function (..., name = name, business.dates, max.major.breaks = 5, max.minor.breaks = max.major.breaks * 
    5, breaks = bd_breaks(business.dates)) 
{   
    require(scales)
    require(bdscale)
    scale_x_continuous(name = name, breaks = breaks(max.major.breaks), 
        minor_breaks = breaks(max.minor.breaks), trans = outlier::bd_trans(business.dates, 
            breaks), ...)
}


#' 
#' t2bd
#' 
#' @description No entiendo por qué tuve que buscar esta función y pegarla acá.
#' La usa scale_x_cont_dates y por alguna razón no la encuentro exportada desde ahi
#' Entonces la armé de vuelta para usarla
#' @export
t2bd = function (ts, business.dates) 
{
    require(scales)
    require(bdscale)
    result = business.dates[pmin(pmax(round(ts, 0), 0) + 1, length(business.dates))]
    structure(result, class = "Date")
}

#'
#'  bd_trans
#' 
#' @description Idem anterior
#' @export
bd_trans = function (business.dates, breaks = bd_breaks(business.dates)) 
{
    require(scales)
    require(bdscale)
    transform <- function(dates) bd2t(dates, business.dates)
    inverse <- function(ts) t2bd(ts, business.dates)
    trans_new("date", transform = transform, inverse = inverse, 
        breaks = breaks, domain = range(business.dates))
}




