
#' Génère les paires d'ions
#'
#' @param Spectrum Un object spectra
#' @param Pairs_by_precursor nombre de paires d'ions par précurseur
#'
#' @return un data frame avec les paires d'ions
#' @export
#'
#' @examples Generate_ions_pairs(Spectra_data,Pairs_by_precursor = 3)
Generate_ions_pairs <- function(Spectrum,Pairs_by_precursor = 3){
  CPnames <- Spectrum$peak_id
  ions_pairs <- data.frame()

  for (CPname in CPnames){
    Swath_spectrum <- Spectrum[Spectrum$peak_id == CPname]
    Swath_spectrum<- filterMzRange(Swath_spectrum,mz=c(0,precursorMz(Swath_spectrum)))
    fragment_mz <- peaksData(Swath_spectrum)[[1]]
    PrecursorMz <- precursorMz(Swath_spectrum)
    PrecursorRt <- rtime(Swath_spectrum)
    PrecursorIntensity <- precursorIntensity(Swath_spectrum)

    if (length(fragment_mz)>0){
    ions_pairs <- rbind(ions_pairs, data.frame(
      CPname,
      PrecursorMz,
      PrecursorRt,
      PrecursorIntensity,
      fragment_mz
    ))}
  }


  trans.spec <- lapply(unique(ions_pairs[, 'CPname']), function(idx) {
    info <- ions_pairs[ions_pairs[, 'CPname'] == idx, , drop = FALSE]
    info <- info[order(info[,'intensity'],decreasing = TRUE)[1:Pairs_by_precursor],]
  })
  dt.trans <- do.call(rbind, trans.spec)

  dt.trans <- dt.trans[complete.cases(dt.trans), ]

  result.df <- data.frame(dt.trans[,1],
                          dt.trans[,2],
                          dt.trans[,5],
                          round(dt.trans[,4], 3),
                          round(dt.trans[,6], 3),
                          round(dt.trans[,3]/60,2),
                          round(dt.trans[,3], 2)

  )
  colnames(result.df) <- c("CPname","mz.precursor","mz.product","Q1.intensity","Q3.intensity","rt.minutes","rt.secondes")
  return(result.df)
}
