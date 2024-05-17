
PurgeBlankRelatedPeaks  <- function(XCMSnExp_object,limit=2){
  BL_sample <- which(XCMSnExp_object$sample_group=="BL")
  QC_sample <- which(XCMSnExp_object$sample_group=="QC")
  CP_keep <- c(which(chromPeakData(XCMSnExp_object)[,"ms_level"]==2))

  for (feature in rownames(featureDefinitions(XCMSnExp_object))){
    Chrompeaks_id <- unlist(featureDefinitions(XCMSnExp_object)[feature,"peakidx"])
    Chrompeaks_data <- chromPeaks(XCMSnExp_object)[Chrompeaks_id,]
    Mean_maxo_BL <- mean(Chrompeaks_data[ Chrompeaks_data[,"sample"] %in% BL_sample,"maxo"])
    Mean_maxo_QC <- mean(Chrompeaks_data[ Chrompeaks_data[,"sample"] %in% QC_sample,"maxo"])
    if (Mean_maxo_QC/Mean_maxo_BL >= limit || is.na(Mean_maxo_BL)){
      CP_keep <- c(CP_keep,Chrompeaks_id)
    }
  }
  obj <- filterChromPeaks(XCMSnExp_object,keep=CP_keep,method="keep")
  obj <- filterFile(obj,keepFeatures =TRUE,file=which(data_peak_blk_purge$sample_group =="QC"))
  return(obj)
}


Group_Spectra_by_feature <- function(Spectrum,XCMSnExp_object){

  Feature_spectra <- data.frame(
    msLevel = integer(),
    feature_id = character(),
    rtime = double(),
    precursorIntensity=double(),
    precursorMz = double(),
    CP_merge = I(list()),
    mz = I(list()),
    intensity = I(list())
  )

  features <- featureDefinitions(XCMSnExp_object)
  peak_idx_Spectrum <- Spectrum$peak_id

  for (feature in rownames(features)){

    peakidx_feature <- unlist(features[feature,"peakidx"])
    Chrompeak_names <- rownames(chromPeaks(XCMSnExp_object)[peakidx_feature, ])

    if (length(which(peak_idx_Spectrum %in% Chrompeak_names))!=0){

      Spectrum_compound <- Spectrum[which(peak_idx_Spectrum %in% Chrompeak_names)]
      Spectra_feature <- combineSpectra(Spectrum_compound, peaks = "intersect", minProp = 0,tolerance = 0.2)

      Feature_spectra <- rbind(Feature_spectra, data.frame(
        msLevel = 2L,
        peak_id = feature,  #feature ID as identifier
        rtime = features[feature, "rtmed"],
        precursorMz = features[feature, "mzmed"],
        precursorIntensity = max(precursorIntensity(Spectrum_compound)), #on prend l'intensity max sur un CP de la feature
        CP_merge = I(list(Chrompeak_names)),
        mz = I(list(mz(Spectra_feature)[[1]])),
        intensity = I(list(intensity(Spectra_feature)[[1]]))
      ))

    }
  }
  return(Spectra(Feature_spectra))
    }
