#' Crée une méthode MRM pouvant être utilisée pour des analyses en spectrométrie de masse
#'
#' @param Scout_name Nom du scout
#' @param Transi_MRM Dataframe des paires d'ions détectés dans les spectres initiaux
#' @param nombre_transi Nombre de transitions à inclure dans la méthode (par défaut 200)
#' @param chooseSMRM Sélectionne les précurseurs les plus intenses ("max"), les moins intenses ("min"), ou tous les précurseurs ("all")
#' @param polarity Polarité pour le calcul des énergies (CE, DP, EP, CXP) : 0 pour négatif, 1 pour positif
#'
#' @return Un dataframe de la méthode MRM pouvant être utilisée pour des analyses en spectrométrie de masse
#' @export
#'

Build_MRM_method <- function(Scout_name, Transi_MRM, nombre_transi = 200, chooseSMRM = c('max', 'min', 'all'), polarity = c(0, 1)) {

  # Trier les transitions par temps de rétention
  Transi_MRM <- Transi_MRM[order(Transi_MRM$rt.secondes), ]
  conversion <- 1

  # Définir la conversion selon la polarité
  if (polarity == 0) {
    print("Methode pour mode negative")
  }
  if (polarity == 1) {
    print("Methode pour mode positive")
    conversion <- -1
  }

  # Retirer les transitions des scouts trouvés
  Transi_MRM <- Transi_MRM[-which(Transi_MRM$CPname %in% Scout_name), ]

  # Créer le dataframe pour la méthode MRM
  Methode_MRM <- data.frame(
    groupeID = Transi_MRM$CPname,
    CompoundID = Transi_MRM$CPname,
    trigger = rep(FALSE, length(Transi_MRM$CPname)),
    threshold.detection = rep(0, length(Transi_MRM$CPname)),
    rt = Transi_MRM$rt.minutes,
    Q1 = Transi_MRM$mz.precursor,
    Q3 = Transi_MRM$mz.product,
    edit.DW = rep(FALSE, length(Transi_MRM$CPname)),
    DW = rep(0, length(Transi_MRM$CPname)),
    DP = rep(-100 * conversion, length(Transi_MRM$CPname)),
    EP = rep(-5 * conversion, length(Transi_MRM$CPname)),
    CE = rep(0, length(Transi_MRM$CPname)),
    CXP = rep(-5 * conversion, length(Transi_MRM$CPname)),
    Q1.intensity = Transi_MRM$Q1.intensity
  )

  # Mettre en forme les CompoundID
  Methode_MRM <- Group_Compound_ID(Methode_MRM)

  # Sélectionner les transitions selon l'intensité
  if (chooseSMRM == "max") {
    Methode_MRM <- Methode_MRM[order(Methode_MRM$Q1.intensity, decreasing = TRUE), ]
  }
  if (chooseSMRM == "min") {
    Methode_MRM <- Methode_MRM[order(Methode_MRM$Q1.intensity, decreasing = FALSE), ]
  }

  # Limiter le nombre de transitions
  Methode_MRM <- Methode_MRM[1:nombre_transi, ]

  # Calculer les énergies de collision basées sur la masse des précurseurs
  Methode_MRM <- Calcul_CE_mz_based(Methode_MRM[, 1:13], conversion = conversion)

  # Trier les transitions par temps de rétention pour la méthode finale
  Methode_MRM_Final <- Methode_MRM[order(Methode_MRM$rt), ]

  return(Methode_MRM_Final)
}


