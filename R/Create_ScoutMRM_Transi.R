#' a function to create Scout MRM method than can be directly copy paste in SCIEX OS
#'
#' @param chooseSMRM Select witch precursor while be selected most intense : "max", less intense : "min" or all precursor "all"
#' @param Scout_Data_frame Dtaframe des scouts détectés
#' @param Transi_MRM dataframe des paires d'ions détectés dans les spectres initiaux [Generate_ions_pairs()]
#' @param limitation_transi_group nombre de transtions par groupe
#' @param polarity polarité pour le calcul des énergie (CE, DP, EP, CXP) 0 négatif, 1 positif
#' @param treshold_detection Seuil de détection des scouts
#' @param DW.Scout dwell time des scouts
#'
#' @return a method file than can directly be copy paste in SCIEX OS
#' @export
#'

Build_ScoutMRM_method <- function(Scout_Data_frame,Transi_MRM,limitation_transi_group=100,chooseSMRM = c('max','min'),polarity=c(0,1),treshold_detection=2000,DW.Scout=10){
  Transi_Scout <- Scout_Data_frame[order(Scout_Data_frame$rt.secondes),]
  Transi_MRM <- Transi_MRM[order(Transi_MRM$rt.secondes),]
  conversion <- 1
  if (polarity==0){print("Methode pour mode negative")}
  if (polarity==1){print("Methode pour mode positive")
    conversion <- -1 }

  Methode_scout <- data.frame(groupeID=paste0("scout",Transi_Scout$scout),
                             CompoundID=Transi_Scout$CPname,
                             trigger=rep(TRUE,length(Transi_Scout$CPname)),
                             threshold.detection=rep(treshold_detection,length(Transi_Scout$CPname)),
                             rt = Transi_Scout$rt.minutes,
                             Q1=Transi_Scout$mz.precursor,
                             Q3 = Transi_Scout$mz.product,
                             edit.DW=rep(TRUE,length(Transi_Scout$CPname)),
                             DW =rep(DW.Scout,length(Transi_Scout$CPname)),
                             DP=Transi_Scout$DP,
                             EP =Transi_Scout$EP,
                             CE = Transi_Scout$CE,
                             CXP = Transi_Scout$CXP
  )



  #Création de la méthode avec toutes les transitions
  Methode_autres_transitions <- data.frame(groupeID=Transi_MRM$CPname,
                           CompoundID=Transi_MRM$CPname,
                           trigger=rep(FALSE,length(Transi_MRM$CPname)),
                           threshold.detection=rep(0,length(Transi_MRM$CPname)),
                           rt = Transi_MRM$rt.minutes,
                           Q1=Transi_MRM$mz.precursor,
                           Q3 = Transi_MRM$mz.product,
                           edit.DW=rep(FALSE,length(Transi_MRM$CPname)),
                           DW =rep(0,length(Transi_MRM$CPname)),
                           DP=rep(-100*conversion,length(Transi_MRM$CPname)),
                           EP =rep(-5*conversion,length(Transi_MRM$CPname)),
                           CE = rep(0,length(Transi_MRM$CPname)),
                           CXP = rep(-5*conversion,length(Transi_MRM$CPname)),
                           Q1.intensity = Transi_MRM$Q1.intensity
  )
  #on recherche les trasntions scout dans toutes les transitions
  scout_in_autres_transitions<- which(Methode_autres_transitions$CompoundID %in% Methode_scout$CompoundID)
  # et on les enlèves
  Methode_autres_transitions <- Methode_autres_transitions[-scout_in_autres_transitions,]
  #On met en forme les CompoundID
  Methode_autres_transitions <- Group_Compound_ID(Methode_autres_transitions)
  Methode_scout<- Group_Compound_ID(Methode_scout)

  Bornes_Rt <- c(unique(Methode_scout$rt),max(Methode_autres_transitions$rt))
  Borne_Rt_Basse <- 0
  Methode_Scout_Final <- data.frame()
  for (borne_rt_haute in Bornes_Rt){
    transi_in_borne <- Methode_autres_transitions[which(Methode_autres_transitions$rt>Borne_Rt_Basse & Methode_autres_transitions$rt<=borne_rt_haute ),]

    if(chooseSMRM=="max"){transi_in_borne <- transi_in_borne[order(transi_in_borne$Q1.intensity,decreasing = TRUE),]}
    if(chooseSMRM=="min"){transi_in_borne <- transi_in_borne[order(transi_in_borne$Q1.intensity,decreasing = FALSE),]}

    if (dim(transi_in_borne)[1]>limitation_transi_group){
      dim <- limitation_transi_group
    }
    else{dim <- dim(transi_in_borne)[1]}


    Methode_Scout_Final<- rbind(Methode_Scout_Final,transi_in_borne[1:dim,])
    Borne_Rt_Basse <- borne_rt_haute
    }
  Methode_Scout_Final<- Calcul_CE_mz_based(Methode_Scout_Final,conversion=conversion)
  Methode_Scout_Final <- rbind(Methode_Scout_Final[,1:13],Methode_scout)
  Methode_Scout_Final <- Methode_Scout_Final[order(Methode_Scout_Final$rt),]
  return(Methode_Scout_Final)
}


Calcul_CE_mz_based <- function(df.transi,slope=0.05,intercept=15,conversion=conversion){
  for (i in 1:length(df.transi[,1])){
    CE <- slope*df.transi[i,"Q1"] + intercept
    df.transi[i,"CE"] <- -CE*conversion
  }
  return(df.transi)}



Group_Compound_ID<- function(df.transi){
  Compound.unique <- unique(df.transi[,"groupeID"])
  for (Compound in Compound.unique){
    pos.Compound <- which(df.transi[,"groupeID"]==Compound)
    len.Compound <- length(pos.Compound)
    for (i in 1:len.Compound){
      df.transi[pos.Compound[i],"CompoundID"]<- paste0(Compound,"_",i,sep="")
    }
  }
  return(df.transi)}


