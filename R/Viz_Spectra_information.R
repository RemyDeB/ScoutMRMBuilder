
#' Visualisation des données
#'
#' @param data un Spectra object
#' @param peak_id Affiche le nom des spectres
#'
#' @return un graphique plotly
#' @export
#'
Viz_spectra_mz_rt <- function(data,peak_id=FALSE){

  PrecursorIntensity <- data[["precursorIntensity"]]
  RetentionTime <- data[["rtime"]]
  PrecursorMz <- data[["precursorMz"]]
  if (peak_id == TRUE){
    numpeak <- data[["peak_id"]]
    data_plot <- as.data.frame(cbind(PrecursorMz,
                                     PrecursorIntensity,
                                     RetentionTime,
                                     numpeak))
  }
  data_plot <- as.data.frame(cbind(PrecursorMz,
                                   PrecursorIntensity,
                                   RetentionTime
                                ))

  t <- plotly::plot_ly()
  if (peak_id == TRUE){
    t <- plotly::add_trace(t,data_plot,x=as.double(data_plot$RetentionTime),y=as.double(data_plot$PrecursorMz),mode = 'markers',type = 'scatter',size=as.double(data_plot$PrecursorIntensity),text=data_plot$numpeak)
    t <- plotly::layout(t,xaxis = list(title = 'Retention time'),
                        yaxis = list(title = 'm/z precursor'))
    return(t)
  }
  t <- plotly::add_trace(t,data_plot,x=as.double(data_plot$RetentionTime),y=as.double(data_plot$PrecursorMz),mode = 'markers',type = 'scatter',size=as.double(data_plot$PrecursorIntensity))
  t <- plotly::layout(t,xaxis = list(title = 'Retention time'),
                      yaxis = list(title = 'm/z precursor'))
  return(t)
}


#' Visualisation des scouts dans les données
#'
#' @param data données sur les paires d'ions
#' @param Scout données sur les scouts
#'
#' @return un graphique plotly
#' @export
#'
Viz_spectra_mz_rt_with_Scout <- function(data,Scout){

  PrecursorIntensity <- data[["Q1.intensity"]]
  RetentionTime <- data[["rt.secondes"]]
  PrecursorMz <- data[["mz.precursor"]]

  numpeak <- data[["CPname"]]
  data_plot <- as.data.frame(cbind(PrecursorMz,
                                    PrecursorIntensity,
                                    RetentionTime,
                                    numpeak))

  t <- plotly::plot_ly(mode = 'markers',type = 'scatter')

  t <- plotly::add_trace(t,data_plot,x=as.double(data_plot$RetentionTime),y=as.double(data_plot$PrecursorMz),size=as.double(data_plot$PrecursorIntensity),text=data_plot$numpeak)
  t <- plotly::add_trace(t,Scout,x=as.double(Scout$rt.secondes),y=as.double(Scout$mz.precursor),text=Scout$scout,marker = list(size=10,color = 'rgba(29, 146, 30, 0.8)'))
  t <- plotly::layout(t,xaxis = list(title = 'Retention time'),
                      yaxis = list(title = 'm/z precursor'))
    return(t)

}
