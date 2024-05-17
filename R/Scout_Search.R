#' Trouve les scouts dans les paires d ions basé sur les m/z précurseurs, fragment et les ordres d élutions
#'
#' @param database_scout base de données scouts
#' @param Ion_pairs paires d'ions détectés dans les spectre sissue de la fonction [Generate_ions_pairs()]
#' @param tolerance_MS tolérance sur le rapport m/z des précurseurs et fragments
#'
#' @return un dataframe avec les scouts identifiés
#' @export
#'
#' @examples Scout_Search(database_scout=dataframe_database,Ion_pairs=paires_dions,tolerance_MS = 0.01)
Scout_Search <- function(database_scout,Ion_pairs,tolerance_MS = 0.01 ){
  Scout_found <- data.frame()

  for (i in 1:length(database_scout[,'Q1.Mass'])){
    cat( paste("Recherche de la transition:",database_scout[i,'Name'], "\n",sep = " "))
    match_MS1 <- data.frame()

    for(j in 1:length(Ion_pairs[,1])){
      if(abs(database_scout[i,'Q1.Mass']-Ion_pairs[j,"mz.precursor"])<tolerance_MS){
        match_MS1<- rbind(match_MS1,data.frame(Ion_pairs[j,],database_scout[i,3],database_scout[i,6],database_scout[i,7],database_scout[i,8],database_scout[i,9]))
      }

    }

    if(nrow(match_MS1>0)){

      for (l in 1:length(match_MS1[,1])){
        if(abs(match_MS1[l,"mz.product"]-database_scout[i,'Q3.Mass'])<tolerance_MS){
          Scout_found <- rbind(Scout_found,match_MS1[l,])
          cat(database_scout['Name'][i,], " found","\n", sep =" ")
        }
        }
      }

  }

  if (nrow(Scout_found)==0){
    stop("0 scout found")
  }
  else{
  colnames(Scout_found)<-c("CPname" ,"mz.precursor" ,"mz.product","Q1.intensity", "Q3.intensity" ,"rt.minutes", "rt.secondes", "scout", "DP", "EP", "CE", "CXP")
  Scout_found <- Clear_Scout(Scout_found)
  return(Scout_found)}
}
