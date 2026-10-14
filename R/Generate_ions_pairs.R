#' Generate ion pairs from a Spectra object
#'
#' @param Spectrum A Spectra object containing MS/MS data
#' @param pairs_per_precursor Number of fragment ion pairs to keep per precursor (positive integer)
#' @param min_mass Minimum fragment m/z to consider
#' @param max_mass Maximum fragment m/z to consider. If "precursor", will use precursor m/z for each spectrum.
#' @param Spectra_name Character. Column name defining unique precursor features (default: "feature_id")
#'
#' @return A data.frame containing precursor/product m/z, intensities, and retention times
#' @export
Generate_ions_pairs <- function(Spectrum,
                                pairs_per_precursor = 3L,
                                min_mass = 0,
                                max_mass = "precursor",
                                Spectra_name = "feature_id") {

  if (!inherits(Spectrum, "Spectra"))
    stop("Input must be a Spectra object.")

  if (!is.numeric(pairs_per_precursor) || pairs_per_precursor %% 1 != 0 || pairs_per_precursor <= 0)
    stop("`pairs_per_precursor` must be a positive integer.")

  if (!Spectra_name %in% colnames(Spectra::spectraData(Spectrum)))
    stop(paste("Column", Spectra_name, "not found in spectra metadata."))

  split_spectra <- split(Spectrum, f = Spectra::spectraData(Spectrum)[[Spectra_name]])

  ions_list <- lapply(names(split_spectra), function(id) {
    spec <- split_spectra[[id]]

    precursor_mz <- precursorMz(spec)[1]
    precursor_rt <- rtime(spec)[1]
    precursor_int <- precursorIntensity(spec)[1]

    local_max_mass <- if (identical(max_mass, "precursor")) precursor_mz else max_mass

    spec_filter <- filterMzRange(spec, mz = c(min_mass, local_max_mass))
    peaks <- Spectra::peaksData(spec_filter)[[1]]
    if (is.null(peaks) || nrow(peaks) == 0) return(NULL)

    df <- data.frame(
      Name = id,
      mz.precursor = precursor_mz,
      mz.product = peaks[, 1],
      Q1.intensity = precursor_int,
      Q3.intensity = peaks[, 2],
      rt = precursor_rt
    )

    df[order(df$Q3.intensity, decreasing = TRUE), ][seq_len(min(pairs_per_precursor, nrow(df))), ]
  })

  ion_pairs <- do.call(rbind, ions_list)

  ion_pairs$rt.minutes <- round(ion_pairs$rt / 60, 2)
  ion_pairs$rt.seconds <- round(ion_pairs$rt, 2)
  ion_pairs$rt <- NULL

  return(ion_pairs)
}
