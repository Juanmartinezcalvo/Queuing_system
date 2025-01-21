Llegada=function(N1,TM,TS1,SUMA1,SUMA2,N2,TANT){
  N1=N1+1
  if(N1==1){
    u=runif(1)
    DS1=0.6*(u<=0.2)+0.7*(0.2<u & u <=0.7)+0.8*(u>0.7)
    TS1=TM+DS1
  }
  DL=rexp(1)
  TL=TM+DL
  SUMA1=SUMA1+(N1-1)*(TM-TANT)
  SUMA2=SUMA2+N2*(TM-TANT)
  TANT=TM
  return(c(N1,TL,TS1,SUMA1,SUMA2,TANT))
}

Servicio1=function(N1,N2,TM,TANT,TS2,SUMA1,SUMA2){
  N1=N1-1
  N2=N2+1
  u=runif(1)
  DS1=0.6*(u<=0.2)+0.7*(0.2<u & u<=0.7)+0.8*(u>0.7)
  TS1=ifelse(N1==0,Inf,TM+DS1)
  if(N2==1){
    u=runif(1)
    DS2=0.8*(u<=0.1)+0.9*(0.1<u & u<0.8)+1*(u>0.8)
    TS2=TM+DS2
  }
  SUMA1=SUMA1+(N1+1)*(TM-TANT)
  SUMA2=SUMA2+(N2-1)*(TM-TANT)
  TANT=TM
  return(c(N1,N2,TS1,TS2,SUMA1,SUMA2,TANT))
}

Servicio2=function(N2,TM,TANT,SUMA1,SUMA2,N1){
  N2=N2-1
  u=runif(1)
  DS2=0.8*(u<=0.1)+0.9*(0.1<u & u<0.8)+1*(u>0.8)
  TS2=ifelse(N2==0,Inf,TM+DS2)
  SUMA1=SUMA1+N1*(TM-TANT)
  SUMA2=SUMA2+(N2+1)*(TM-TANT)
  TANT=TM
  return(c(N2,TS2,SUMA1,SUMA2,TANT))
}


#Notación
#N1=número de personas en la 1a etapa del sistema
#N2=número de personas en la 2a etapa del sistema
#TM=reloj de simulación
#TL=instante de la próxima llegada
#TS1=instante de la próxima finalización del servicio 1
#TS2=instante de la próxima finalización del servicio 2
#Tmax=tiempo máximo de simulación (en min)
#DL=tiempo entre llegadas consecutivas
#DS1=duración del servicio 1
#DS2=duración del servicio 2
#SUMA1=contador suma acumulada de áreas asociada a la 1a etapa
#SUMA2=contador suma acumulada de áreas asociada a la 2a etapa


#Inicializamos las variables
N1=0
N2=0
TM=0
TANT=0
TS1=Inf
TS2=Inf
Tmax=10

SUMA1=0
SUMA2=0

set.seed(1)
#Genero la primera llegada
DL=rexp(1)
TL=DL

while(TM<Tmax){
  #Identificamos el evento que tiene lugar antes
  TM=min(TL,TS1,TS2)
  Estado=1*(TM==TL)+2*(TM==TS1)+3*(TM==TS2)
  if(Estado==1){
    print("ha habido una llegada")
    k=Llegada(N1,TM,TS1,SUMA1,SUMA2,N2,TANT)
    N1=k[1]
    TL=k[2]
    TS1=k[3]
    SUMA1=k[4]
    SUMA2=k[5]
    TANT=k[6]
  }else if(Estado==2){
    print("ha habido una finalización de Servicio 1")
    k=Servicio1(N1,N2,TM,TANT,TS2,SUMA1,SUMA2)
    N1=k[1]
    N2=k[2]
    TS1=k[3]
    TS2=k[4]
    SUMA1=k[5]
    SUMA2=k[6]
    TANT=k[7]
  }else{
    print("ha habido una finalización de Servicio 2")
    k=Servicio2(N2,TM,TANT,SUMA1,SUMA2,N1)
    N2=k[1]
    TS2=k[2]
    SUMA1=k[3]
    SUMA2=k[4]
    TANT=k[5]
  }
}

print("El número medio de clientes en la primera etapa por unidad de tiempo")
SUMA1/TM
print("El número medio de clientes en la segunda etapa por unidad de tiempo")
SUMA2/TM

print("El programa ha parado en el instante")
TM
print(c("En el instante de parada había en la primera etapa",N1,"personas"))
print(c("En el instante de parada había en la segunda etapa",N2,"personas"))

