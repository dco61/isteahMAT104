#'
#'Generate simulated Haiti (2019) population: 11.2 millions observations
#'
#'@return Data Frame containing the data structure:
#'        GENRE: “M” = Masculin “F” = Féminin
#'        AGE: Âge, en unité d’années
#'        EDUC: Niveau de scolarité atteint à l’âge de 25 ans - Aucun -
#'              PréScol - Fond.1 - Fond.2 - Fond.3 - Second - Univ.1 -
#'              Univ.2 - Univ.3
#'        DEPARTEMENT: Département de résidence
#'         "1"  "Artibonite"
#'         "2"  "Centre"
#'         "3"  "Grand'Anse"
#'         "4"  "Nippes"
#'         "5"  "Nord"
#'         "6"  "Nord-Est"
#'         "7"  "Nord-Ouest"
#'         "8"  "Ouest"
#'         "9"  "Sud-Est"
#'         "10" "Sud"
#'
#'        CondxVie: Indice général de condition de vie, sur une échelle de 0 à 50. Plus l’indice est élevé, plus les conditions de vie de la personne sont bonnes.
#'        CRIMIDX: Indice de propension à la criminalité et à la corruption, sur une échelle de 0 à 150. Plus l’indice est élevé, plus l’individu a des tendances criminelles.
#'        HabilCog: Indice d’habiletés cognitives, sur une échelle de 20 à 200. Plus cet indice est élevé, plus les habiletés intellectuelles et cognitives de l’individu sont grandes.
#'        PrefPres: Préférence pour un candidat donné à la présidentielle de 2022. Cinq candidats sont en liste: CandA - CandE.
#'        OpinConst: Opinion concernant à un nouveau projet de Constitution:
#'          “Tout à fait d’accord”
#'          “D’accord”
#'          “Indécis”
#'          “Pas d’accord”
#'          “Pas du tout d’accord”
#'
#'L’objet sortant est un ‘DATA.FRAME’ appelé HaitiPop.
#'@export
HaitiPopGen <- function(){
  set.seed(1234567)
  # HaitiPop: Simultation de la population Haitienne (2019)
  # Genre Féminin, effectif par groupe d'âge (données de 2019)
  Age_female=c(620842, 608143, 590011, 565536, 531966, 493101, 458841,
               410081, 309636, 245577, 218103, 187285, 149090, 118763,
               80994, 60334, 34915, 15440, 5211, 1397, 217)

  # Genre Masculin, effectif par groupe d'âge (données de 2019)
  Age_male=c(645049, 629568, 607263, 571570, 525211, 479976, 442822,
             390944, 286062, 221042, 199898, 169226, 135904, 102412,
             64604, 47325, 24681, 10151, 3085, 720, 83)
  PopTotale <- sum(Age_male)+sum(Age_female)

  # Génération des âges et du genre
  age <- c(seq(4,104,by=5))
  AGE <- vector()
  GENRE <- vector()
  for(i in 1:length(age)){
    AGE <- append(AGE,floor(runif(Age_male[i],(age[i]-4),age[i]+5)))
    AGE <- append(AGE,floor(runif(Age_female[i],(age[i]-4),age[i]+5)))
    GENRE <- append(GENRE,rep("M",Age_male[i]))
    GENRE <- append(GENRE,rep("F",Age_female[i]))
  }
  Tdt <- data.frame(AGE,GENRE)
  Tdt <- Tdt[sample(nrow(Tdt)),1:2]
  # Génération des Départements et répartition de la population
  Dept <- matrix(c(1:10,"Artibonite","Centre","Grand'Anse","Nippes",
                  "Nord","Nord-Est","Nord-Ouest","Ouest","Sud-Est",
                  "Sud"),nrow=2,byrow=TRUE)
  # cat("Identification des Départements: ","\n",t(Dept))
  # Proportion de la population par département
  PropDept <- c(0.158316776, 0.037133348, 0.024201755, 0.018140699,
               0.057563731, 0.022548639, 0.042675426, 0.24647869,
               0.051350001, 0.066312107)
  DEPARTEMENT <- sample(Dept[1,],size=PopTotale,replace=TRUE, prob=PropDept)
  # Indice de condition de vie, distribution asymétrique
  CondxVie <- rchisq(PopTotale,6)
  Tdt <- data.frame(Tdt,DEPARTEMENT,CondxVie)
  # Génération des niveaux de scolarité atteints, population adulte
  EDUC=rep("NA",PopTotale)
  NivScol <- c("Aucun","PréScol","Fond.1","Fond.2","Fond.2","Second",
              "Univ.1","Univ.2","Univ.3")
  PropNivScol <- c(0.2, 0.07, 0.2, 0.18, 0.12, 0.09, 0.06, 0.05, 0.03)
  EDUC[Tdt$AGE>25] <- sample(NivScol,sum(Tdt$AGE>25),prob=PropNivScol,replace=TRUE)

  # Génération de l'indice de criminalité

  CRIMIDX <- rep("NA",PopTotale)
  idx <- vector()
  for(i in 1:10){
    idx[i] <- sample(c(1,-1),1)*runif(1,0,30)
  }

  CRIMIDX <- 0.3*CondxVie + 3.5*(Tdt$GENRE=="M")-1.2*Tdt$AGE
  CRIMIDX <- CRIMIDX +1.5*(EDUC<"Second")+10 +rnorm(1,0,4)
  CRIMIDX <- CRIMIDX + idx[as.numeric(DEPARTEMENT)]
  CRIMIDX <- CRIMIDX + (min(CRIMIDX)<0)*abs(min(CRIMIDX))
  CRIMIDX[which(Tdt$AGE<15)] <- NA

  # Génération de l'indice d'habiletés cognitives
  HabilCog <- rnorm(PopTotale,100,15)
  idx <- vector()
  for(i in Dept){
    idx[i] <- sample(c(1,-1),1)*runif(1,0,10)
  }
  HabilCog <- HabilCog+idx[DEPARTEMENT]

  # Génération de la préférence pour un candidat présidentiel
  # et de l'Opinion concernant la proposition d'une nouvelle Constitution

  PrefPres <- rep(NA,PopTotale)
  OpinConst <- rep(NA,PopTotale)

  # Probabilités associées aux variables PresProb et OpinProb
  PresProb <- matrix(c(0.58, 0.41, 0.003, 0.004, 0.003, # Nord-Ouest, Nord et Nord-Est
                      0.41, 0.54, 0.003, 0.004, 0.043, # Artibonite et Centre
                      0.27, 0.65, 0.006, 0.01, 0.064,  # Ouest
                      0.15, 0.23, 0.38, 0.19, 0.05),   # Grand<Anse, Nippes, Sud et Sud-Est
                    nrow = 4, ncol = 5, byrow = TRUE)
  OpinProb <- matrix(c(0.5,0.3,0.1,0.05,0.05,           # Nord-Ouest, Nord et Nord-Est
                      0.2,0.4,0.1,0.1,0.2,             # Artibonite et Centre
                      0.01,0.09,0.2,0.4,0.3,           # Ouest
                      0.4,0.1,0.0,0.1,0.4),            # Grand<Anse, Nippes, Sud et Sud-Est
                    nrow = 4, ncol = 5, byrow = TRUE)
  Regions <- list(c("5","6","7"), c("1","2"), c("8"),c("3","4","9","10"))
  OptPres <- c("CandA", "CandB", "CandC", "CandD", "CandE")
  OptOpin <- c("Tout à fait d'accord","D'accord","Indécis","Pas d'accord","Pas du tout d'accord")

  for (i in 1:length(Regions)){
    select <- (Tdt$AGE>=18)&(DEPARTEMENT %in% Regions[[i]])
    PrefPres[select] = wakefield::political(n=sum(select), x = OptPres,prob = PresProb[i,], name = "PrefPres")
    OpinC <- wakefield::r_sample_ordered(n=sum(select),x = OptOpin,prob = OpinProb[i,])
    OpinConst[select] <- OpinC
  }

  # DATA.FRAME de sortie
  HaitiPop <- data.frame(Tdt,EDUC,CRIMIDX,HabilCog,PrefPres,OpinConst)
  return(HaitiPop)
}

