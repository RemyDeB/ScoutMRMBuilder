#' Identify scout transitions from a database within detected ion pairs
#'
#' @param database_scout Data.frame containing known scout transitions
#'   Must include Q1.Mass, Q3.Mass, Name
#'   Optional: DP, EP, CE, CXP, Order
#' @param Ion_pairs Data.frame from Generate_ions_pairs()
#' @param ppm_Q1 PPM tolerance for precursor m/z
#' @param ppm_Q3 PPM tolerance for fragment m/z
#' @param Show Logical. Print progress messages.
#'
#' @return A data.frame of matched scout transitions (possibly empty)
#' @export
Scout_Search <- function(database_scout,
                         Ion_pairs,
                         ppm_Q1 = 10,
                         ppm_Q3 = 10,
                         Show = FALSE) {

  required_db_cols <- c("Q1.Mass", "Q3.Mass", "Name")
  if (!all(required_db_cols %in% colnames(database_scout)))
    stop("database_scout must contain columns: Q1.Mass, Q3.Mass, Name")

  required_ip_cols <- c("mz.precursor", "mz.product")
  if (!all(required_ip_cols %in% colnames(Ion_pairs)))
    stop("Ion_pairs must contain columns: mz.precursor, mz.product")

  optional_cols <- c("DP", "EP", "CE", "CXP", "Order")
  opt_present <- optional_cols[optional_cols %in% colnames(database_scout)]

  ppm_diff <- function(measured, theoretical) {
    abs(measured - theoretical) / theoretical * 1e6
  }

  results <- lapply(seq_len(nrow(database_scout)), function(i) {
    scout <- database_scout[i, ]

    if (Show) {
      message(sprintf("Searching transition: %s", scout$Name))
    }

    match_precursor <- ppm_diff(Ion_pairs$mz.precursor, scout$Q1.Mass) <= ppm_Q1
    if (!any(match_precursor)) return(NULL)

    candidates <- Ion_pairs[match_precursor, ]

    match_fragment <- ppm_diff(candidates$mz.product, scout$Q3.Mass) <= ppm_Q3
    matched <- candidates[match_fragment, ]
    if (nrow(matched) == 0) return(NULL)

    matched$Scout <- scout$Name
    if (length(opt_present) > 0) {
      matched[opt_present] <- scout[opt_present]
    }

    matched
  })

  result <- do.call(rbind, results)

  if (is.null(result)) {
    if (Show) message("No scouts found.")
    return(data.frame())
  }

  rownames(result) <- NULL

  if (Show) message("Scout matching completed.")

  return(result)
}
