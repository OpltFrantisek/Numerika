#Naprogramovat vnit�n� v�nosov� procento
f<-function(x){  return(-500+150/(1+x)^1+200/(1+x)^2+240/(1+x)^3)}

fd<-function(x){return(exp(x)-2*x)}
pd<-function(x){return(x-(exp(x)-x*x+1)/(exp(x)-2*x))}

#funkce pro deleni intervalu
puleni <- function(fce,x1,x2){
  s <- ((x1+x2)/2)                          #vypocteni stredu

  if ((x2-x1)>0.000001){                    #overeni vzdalenosti/presnosti krajnich bodu intervalu
    if((fce(x1)*fce(s))<=0){                #overeni, zda-li se koren nachazi v intervalu
      lines(s,y = NULL ,type = "l")
      puleni(fce,x1,s)                      #rekurze puleni na prvnim intervalu
    }else if((fce(s)*fce(x2))<=0){          #overeni, zda-li se koren nachazi v druhem intervalu
      lines(s,y = NULL ,type = "l")
      puleni(fce,s,x2)                      #rekurze puleni na druhy interval
    }else{return ("Koren nebyl nalezen")}   #pokud neni koren ani v jednom z intervalu vrati zpravu o nenalezeni	
  }
  else{                                     #pokud je vzd�lenost krajnich vodu prilis mala vratime jeho stred
    return (s)
  }
}

res <- puleni(f,0,3)  
print(res)