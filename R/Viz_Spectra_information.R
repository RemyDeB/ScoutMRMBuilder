#' Visualization of detected precursor ions
#'
#' @description
#' Interactive visualization of MS precursor ions in an m/z vs retention time space.
#'
#' @param data A Spectra object containing precursor metadata.
#' @param feature_id Logical. Display feature identifiers as tooltip labels if TRUE.
#' @param size Logical. Use precursor intensity to scale marker size if TRUE.
#'
#' @return An interactive plotly scatter plot.
#' @export
Viz_spectra_mz_rt <- function(data, feature_id = FALSE, size = FALSE) {

  required_data <- c("precursorIntensity", "rtime", "precursorMz", "feature_id")
  if (!all(required_data %in% spectraVariables(data)))
    stop("Missing required variables in 'data'. Expected: ", paste(required_data, collapse = ", "))


  df <- data.frame(
    mz = data[["precursorMz"]],
    rt = data[["rtime"]] / 60,
    intensity = data[["precursorIntensity"]],
    stringsAsFactors = FALSE
  )

  if (!size) df$intensity <- 7

  if (feature_id) {
    if (!"feature_id" %in% spectraVariables(data)) {
      stop("feature_id is TRUE but 'feature_id' variable is missing in 'data'.")
    }
    df$label <- data[["feature_id"]]
  }

  p <- plotly::plot_ly(
    data = df,
    x = ~rt, y = ~mz,
    type = "scatter", mode = "markers",
    marker = list(
      color = "black",
      size = ~as.double(intensity),
      line = list(color = "black")
    ),
    text = if (feature_id) ~label else NULL
  )

  p <- plotly::layout(
    p,
    xaxis = list(title = "Retention time (min)"),
    yaxis = list(title = "m/z")
  )

  return(p)
}


#' Visualization of scout features overlaid on precursor ions
#'
#' @description
#' Overlay identified scout ions on top of all detected precursor features.
#'
#' @param data A data frame containing ion pair results.
#' @param Scout A data frame containing scout search results.
#'
#' @return An interactive plotly scatter plot.
#' @export
Viz_spectra_mz_rt_with_Scout <- function(data, Scout) {

  required_data <- c("Q1.intensity", "rt.seconds", "mz.precursor", "Name")
  if (!all(required_data %in% colnames(data)))
    stop("Missing required columns in 'data'. Expected: ", paste(required_data, collapse = ", "))

  required_scout <- c("mz.precursor", "rt.seconds", "Scout")
  if (!all(required_scout %in% colnames(Scout)))
    stop("Missing required columns in 'Scout'. Expected: ", paste(required_scout, collapse = ", "))

  df <- data.frame(
    mz = data$mz.precursor,
    rt = data$rt.seconds / 60,
    intensity = data$Q1.intensity,
    label = data$Name,
    stringsAsFactors = FALSE
  )

  p <- plotly::plot_ly(type = "scatter", mode = "markers")

  p <- plotly::add_trace(
    p,
    data = df,
    x = ~rt, y = ~mz,
    text = ~label,
    name = "Other precursors",
    marker = list(size = 10)
  )
  p <- plotly::add_trace(
    p,
    data = Scout,
    x = ~rt.seconds / 60,
    y = ~mz.precursor,
    text = ~Scout,
    name = "Scout",
    marker = list(size = 12, color = "rgba(29,146,30,0.8)")
  )

  p <- plotly::layout(
    p,
    xaxis = list(title = "Retention time (min)"),
    yaxis = list(title = "m/z")
  )

  return(p)
}
