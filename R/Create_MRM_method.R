
Build_MRM_method<-  function(Scout_name,Transi_MRM,nombre_transi=200,chooseSMRM = c('max','min'),polarity=c(0,1)){

  Transi_MRM <- Transi_MRM[order(Transi_MRM$rt.secondes),]
  conversion <- 1
  if (polarity==0){print("Methode pour mode negative")}
  if (polarity==1){print("Methode pour mode positive")
    conversion <- -1 }

  Transi_MRM <- Transi_MRM[-which(Paires_ions$CPname %in% Scout_trouve$CPname),]
  Methode_MRM <- data.frame(groupeID=Transi_MRM$CPname,
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
  Methode_MRM <- Group_Compound_ID(Methode_MRM)
  if(chooseSMRM=="max"){Methode_MRM <- Methode_MRM[order(Methode_MRM$Q1.intensity,decreasing = TRUE),]}
  if(chooseSMRM=="min"){Methode_MRM <- Methode_MRM[order(Methode_MRM$Q1.intensity,decreasing = FALSE),]}

  Methode_MRM <- Methode_MRM[1:nombre_transi,]

  Methode_MRM<- Calcul_CE_mz_based(Methode_MRM[,1:13],conversion=conversion)

  Methode_MRM_Final <- Methode_MRM[order(Methode_MRM$rt),]
  return(Methode_MRM_Final)
}
