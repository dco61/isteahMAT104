#' Simulation de tableaux de contingence 2x2
#' @param p1 Probabilité de 'succès' (variable RANGÉES) (Défaut: 0.1)
#' @param p2 Probabilité de 'succès' (variable Colonnes) (Défaut: 0.45)
#' @param n1 Taille de l'échantillon (défaut: 200)
#' @param N Nombre de tableaux à générer
#'
#' Sortie: Liste comprenant, pour chaque échantillon:
#'     a,b,c,d: Fréquences observées dans chacune des cellules
#'     rr, lrr, selnrr,llrr, ulrr: Rapports de risque, Log(Rapports de risque),
#'            Erreur-standard des rapports de risque, limites inférieures et
#'            supérieures (95%) de l'intervalle de confiance des RR.
#'     or,lnor,selnor,llor,ulor: Rapports de Cotes, Log(Rapports de Cotes),
#'            Erreur-standard des rapports de Cotes, limites inférieures et
#'            supérieures (95%) de l'intervalle de confiance des RC.
#' @export
#'
sim2x2 <- function(p1=.1, p2=.45, n1=200, N=10000){
  set.seed(NULL)
  or=rep(NA,N)
  lnor=rep(NA,N)
  a=rep(NA,N)
  b=rep(NA,N)
  c=rep(NA,N)
  d=rep(NA,N)
  rr=rep(NA,N)
  lrr=rep(NA,N)
  selnor=rep(NA,N)
  llor=rep(NA,N)
  ulor=rep(NA,N)
  selnrr=rep(NA,N)
  llrr=rep(NA,N)
  ulrr=rep(NA,N)
  #
  for(i in 1:N) {
    #   n1=as.integer(runif(1,300,600))
    x1 = runif(n1,0,1)<p1
    x2 = runif(n1,0,1)<p2
    a[i]=sum((x1==1)&(x2==1))
    b[i]=sum((x1==0)&(x2==1))
    c[i]=sum((x1==1)&(x2==0))
    d[i]=sum((x1==0)&(x2==0))
    # Relative Risk
    rr[i]=(a[i] / (a[i]+b[i])) / (c[i] / (c[i]+d[i]))
    lrr[i]=log(rr[i])
    selnrr[i]=sqrt((b[i]/a[i])/(a[i]+b[i])+(d[i]/c[i])/(c[i]+d[i]))
    llrr[i]=lrr[i]-1.96*selnrr[i]
    ulrr[i]=lrr[i]+1.96*selnrr[i]
    # Odds Ratio
    or[i]=(b[i]*c[i])/(a[i]*d[i])
    lnor[i]=log(or[i])
    selnor[i]=sqrt(1/a[i]+1/b[i]+1/c[i]+1/d[i])
    llor[i]=lnor[i]-1.96*selnor[i]
    ulor[i]=lnor[i]+1.96*selnor[i]
  }
  dataout=data.frame(a,b,c,d,rr, lrr, selnrr,llrr, ulrr,or,lnor,selnor,llor,ulor)
}
