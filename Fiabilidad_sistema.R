Fallo1=function(X2,TM,SS,TANT){
  X1=0
  if(X2==0){
    SS=0
    TANT=TM #El sistema cambia de estado
  }
  #Genero el tiempo hasta la reparación de Comp1
  DR1=runif(1,2,4)
  TP1=TM+DR1
  return(c(X1,SS,TP1,TANT))
}

Fallo2=function(X1,TM,SS,TANT){
  X2=0
  if(X1==0){
    SS=0
    TANT=TM #El sistema cambia de estado
  }
  #Genero el tiempo hasta la reparación de Comp2
  DR2=runif(1,6,8)
  TP2=TM+DR2
  return(c(X2,SS,TP2,TANT))
}

Reparo1=function(SS,TM,TANT,TPAR){
  X1=1
  if(SS==0){
    SS=1
    TPAR=TPAR+(TM-TANT)
  }
  #Genero tiempo hasta el fallo de Comp1
  DF1=rweibull(1,1,2)
  TP1=TM+DF1
  return(c(X1,SS,TP1,TPAR))
}

Reparo2=function(SS,TM,TANT,TPAR){
  X2=1
  if(SS==0){
    SS=1
    TPAR=TPAR+(TM-TANT)
  }
  #Genero tiempo hasta el fallo de Comp2
  DF2=rweibull(1,1,2.5)
  TP2=TM+DF2
  return(c(X2,SS,TP2,TPAR))
}

#Notación
#TM=reloj de simulación
#Tmax=tiempo máximo de simulación
#TPAR=contador del tiempo que el sistema está parado
#DF1=tiempo de funcionamiento (hasta el fallo) de Comp1
#DF2=tiempo de funcionamiento (hasta el fallo) de Comp2
#DR1=tiempo de reparación de Comp1
#DR2=tiempo de reparación de Comp2
#TP1=instante del próximo evento Comp1
#TP2=instante del próximo evento Comp2
#X1=indicadora de si Comp1 funciona (X1=1)
#X2=indicadora de si Comp1 funciona (X2=1)
#SS=indicadora de si el sistema funciona
#TANT=instante evento anterior
#################################
#Inicialización
TM=0
TANT=0
TPAR=0

Tmax=200
X1=1
X2=1
SS=1

set.seed(1)
#Genero los próximos tiempos hasta el fallo
DF1=rweibull(1,1,2)
DF2=rweibull(1,1,2.5)

TP1=TM+DF1 #=DF1
TP2=TM+DF2

while(TM<Tmax){
  #Actualizar el reloj de simulación
  TM=min(TP1,TP2)
  if(TM==TP1){
    if(X1==1){
      k=Fallo1(X2,TM,SS,TANT)
      X1=k[1]
      SS=k[2]
      TP1=k[3]
      TANT=k[4]
    }else{
      k=Reparo1(SS,TM,TANT,TPAR)
      X1=k[1]
      SS=k[2]
      TP1=k[3]
      TPAR=k[4]
    }
  }else{
    if(X2==1){
      k=Fallo2(X1,TM,SS,TANT)
      X2=k[1]
      SS=k[2]
      TP2=k[3]
      TANT=k[4]
    }else{
      k=Reparo2(SS,TM,TANT,TPAR)
      X2=k[1]
      SS=k[2]
      TP2=k[3]
      TPAR=k[4]
    }
  }
}#end while

print("La proporción de tiempo que el sistema está parado")
TPAR/TM
