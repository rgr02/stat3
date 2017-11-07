#-----------------------------------
#Dipl.Math. Dr. Haiko Luepsen
#http://www.uni-koeln.de/~luepsen/R/
#-----------------------------------

bf.f <- function(formel,frame)
{            # Brown & Forsythe-F-Test fuer inhomogene Varianzen
  epsb <- 0
  epsab<-0
  # check formula
  if (mode(formel)!="call") stop("invalid formula")
  # Standard-Anova
  aov.1<-aov(formel,frame)
  aov.2<-anova(aov.1)
  # Variableninfos aus aov nehmen
  vars <-names(attr(aov.1$terms,"dataClasses"))
  nfact <-length(vars)-1
  if (nfact > 2) stop("zu viele Faktoren")
  
  nr<-1                                     # Anzahl Effekte
  if (nr > 1) nr<-3
  aov.3<-drop1(aov.1, ~.,test="F")           # Typ III SSq
  aov.2[1:nr,2]<-aov.3[2:(nr+1),2]           # SSq ersetzen
  aov.2[1:nr,3]<-aov.2[1:nr,2]/aov.2[1:nr,1] # MSq neu berechnen
  # zus?tzliche Spalten f?r Box-Werte
  aov.2[,2:5]->aov.2[,3:6]
  names(aov.2)=c("Df","Df.err","Sum Sq","Mean Sq","F value","Pr(>F)")
  
  
  # Position des Residuenterms
  if (nfact==1) {ires=2} else {ires=4}  
  # Faktor A
  ni   <-table(frame[,vars[2]])
  si   <-tapply(frame[,vars[1]],frame[,vars[2]],var)
  bf   <-bf2.f(si,ni)
  aov.2[1,2]<-bf$df               # Df Residuen
  aov.2[1,5]<-aov.2[1,3]/bf$sserr # F-Wert
  aov.2[1,6] <- 1-pf(aov.2[1,5],aov.2[1,1],aov.2[1,2])
  if (nfact > 1)
  {                         # Faktor B
    ni   <-table(frame[,vars[3]])
    si   <-tapply(frame[,vars[1]],frame[,vars[3]],var)
    bf   <-bf2.f(si,ni) 
    aov.2[2,2]<-bf$df               # Df Residuen
    aov.2[2,5]<-aov.2[2,3]/bf$sserr # F-Wert
    aov.2[2,6] <- 1-pf(aov.2[2,5],aov.2[2,1],aov.2[2,2])
    # Faktor A*B
    ni   <-table(frame[,vars[2]],frame[,vars[3]])
    si   <-tapply(frame[,vars[1]],list(frame[,vars[2]],frame[,vars[3]]),var)
    bf   <-bf2.f(si,ni)
    aov.2[3,2]<-bf$df               # Df Residuen
    aov.2[3,5]<-aov.2[3,3]/bf$sserr # F-Wert
    aov.2[3,6] <- 1-pf(aov.2[3,5],aov.2[3,1],aov.2[3,2])
  }
  aov.2[ires,2] <- NA
  aov.2
}

bf2.f <- function(si,ni)
{
  n<-sum(ni)
  mi<-(1-ni/n)*si
  m<-sum(mi)
  mi<-mi/m
  df<-1/sum(mi^2/(ni-1))
  bf<-list(sserr=m,df=df)
  return(bf)
}


