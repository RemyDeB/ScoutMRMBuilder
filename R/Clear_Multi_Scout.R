Clear_Scout <- function(df.Match){
  verif  <- df.Match

  stop <- TRUE
  while (stop) {
    Ordre.elution <- unique(df.Match$scout)

    Scout.unique <- lapply(1:length(Ordre.elution),function(x){
      if(length(unique(df.Match$rt.minutes[df.Match$scout == Ordre.elution[x]]))==1){
        df.Match[df.Match$scout == Ordre.elution[x],]
      }

    })
    Scout.unique <- do.call(rbind,Scout.unique)


    Ordre.elution.manquant <- setdiff(Ordre.elution,unique(Scout.unique$scout))
    if(length(Ordre.elution.manquant)==0){
      return(df.Match)
    }
    #on recupère les données où ils y a un doute sur le scout
    Donnee.manquante <- lapply(1:length(Ordre.elution.manquant),function(x){
      df.Match[df.Match$scout==Ordre.elution.manquant[x],]

    })
    Donnee.manquante <- do.call(rbind,Donnee.manquante)
    Rt.unique <- unique(Scout.unique$rt.minutes)
    if(is.null(Rt.unique)){stop("Pas de scout unique, revoir données scouts")}
    #elimination composé qui ont un RT trop bas pour un ordre d'élution trop haut
    compose_a_retirer =c()
    for (i in 1:length(Rt.unique)){
      RT.seuil <- Rt.unique[i]
      Ord.elu.seuil <- unique(Scout.unique$scout[Scout.unique$rt.minutes==RT.seuil])
      for(j in 1:length(Donnee.manquante$CPname)){
        RT.manquant <-  Donnee.manquante$rt.minutes[j]
        Ord.elu.manquant <-  Donnee.manquante$scout[j]
        if(RT.seuil > RT.manquant && Ord.elu.seuil < Ord.elu.manquant){
          compose_a_retirer <- c(compose_a_retirer,j)
        }
        else if(RT.seuil < RT.manquant && Ord.elu.seuil > Ord.elu.manquant){
          compose_a_retirer <- c(compose_a_retirer,j)
        }
      }
    }


    if(!is.null(compose_a_retirer)){
      donne.renvoye <- rbind(Donnee.manquante[-unique(compose_a_retirer),],Scout.unique)
    }
    else{
      donne.renvoye <- rbind(Donnee.manquante,Scout.unique)
    }
    if(identical(donne.renvoye,verif)){
      stop=FALSE
    }
    else{
      verif <- donne.renvoye
    }

    return(donne.renvoye)
  }
  }


