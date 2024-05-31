#' Génère les paires d'ions
#'
#' @param Spectrum Un objet spectra qui contient les données de spectres de masse
#' @param Pairs_by_precursor Nombre de paires d'ions par précurseur (par défaut 3)
#'
#' @return Un data frame avec les paires d'ions contenant les informations sur le précurseur et les ions produits
#' @export
#'

Generate_ions_pairs <- function(Spectrum, Pairs_by_precursor = 3) {
  # Extraction des identifiants des pics du spectre
  CPnames <- Spectrum$peak_id
  ions_pairs <- data.frame()

  # Boucle à travers chaque identifiant de pic
  for (CPname in CPnames) {
    # Filtrer les spectres pour obtenir uniquement ceux avec l'identifiant de pic courant
    Swath_spectrum <- Spectrum[Spectrum$peak_id == CPname, ]
    # Filtrer les fragments en dessous de la masse du précurseur
    Swath_spectrum <- Spectra::filterMzRange(Swath_spectrum, mz = c(0, Spectra::precursorMz(Swath_spectrum)))
    fragment_mz <- Spectra::peaksData(Swath_spectrum)[[1]]
    PrecursorMz <- Spectra::precursorMz(Swath_spectrum)
    PrecursorRt <- Spectra::rtime(Swath_spectrum)
    PrecursorIntensity <- Spectra::precursorIntensity(Swath_spectrum)

    # Si des fragments sont présents, ajouter les informations au data frame des paires d'ions
    if (length(fragment_mz) > 0) {
      ions_pairs <- rbind(ions_pairs, data.frame(
        CPname,
        PrecursorMz,
        PrecursorRt,
        PrecursorIntensity,
        fragment_mz
      ))
    }
  }

  # Sélectionner les paires d'ions en fonction de l'intensité, en gardant les plus intenses
  trans.spec <- lapply(unique(ions_pairs[, 'CPname']), function(idx) {
    info <- ions_pairs[ions_pairs[, 'CPname'] == idx, , drop = FALSE]
    info <- info[order(info[, 'intensity'], decreasing = TRUE)[1:Pairs_by_precursor], ]
  })
  dt.trans <- do.call(rbind, trans.spec)

  # Retirer les lignes avec des valeurs manquantes
  dt.trans <- dt.trans[stats::complete.cases(dt.trans), ]

  # Créer le data frame final avec les colonnes souhaitées et arrondir certaines valeurs
  result.df <- data.frame(
    dt.trans[, 1],  # CPname
    dt.trans[, 2],  # mz.precursor
    dt.trans[, 5],  # mz.product
    round(dt.trans[, 4], 3),  # Q1.intensity
    round(dt.trans[, 6], 3),  # Q3.intensity
    round(dt.trans[, 3] / 60, 2),  # rt.minutes
    round(dt.trans[, 3], 2)  # rt.secondes
  )

  # Renommer les colonnes du data frame final
  colnames(result.df) <- c("CPname", "mz.precursor", "mz.product", "Q1.intensity", "Q3.intensity", "rt.minutes", "rt.secondes")

  return(result.df)
}
