set.seed(123)

# N(0,1) por box_muller
Z_box_muller = function(n) {
  u1 = runif(n)
  u2 = runif(n)
  x =  sqrt(-2 * log(u1)) * cos(2 * pi * u2)
  return(x)
}


Z_trunc = function(n) {
  muestrax =c()
  while(length(muestrax) < n) {
    # Generamos una normal con box_muller
    z = Z_box_muller(1)
    if(abs(z)<=2) {
      muestrax = c(muestrax, z)
    }
  }
  return(muestrax)
}

# Comprobamos la bondad
n <- 10000
muestra <- Z_trunc(n)

trunc_norm_cft <- function(x) {
  (pnorm(x) - pnorm(-2)) / (pnorm(2) - pnorm(-2))
}
ks.test(muestra, trunc_norm_cft)

hist(muestra, freq=FALSE)
trunc_norm_density <- function(x) {
  dnorm(x) / (pnorm(2) - pnorm(-2))
}
curve(trunc_norm_density, from = -2, to = 2, add = TRUE, col = "red", lwd = 2)

norm_truncada = function(n,mean,sd) {
  Z_trunc(n)*sd + mean
}

# Generamos las llegadas de la gente al aeropuerto dependiendo de los vuelos
horarios = c(4, 8, 11, 14, 17, 20, 23)*60 # para pasarlo a minutos
dentro_UE = c('Si', 'Si', 'Si', 'No', 'No', 'Si', 'Si')
dist_llegadas = c()
for (i in 1:length(horarios)){
  # llegan 189 personas con una horario que sigue una normal truncada [μ-80,μ+80] de media 100 minutos antes del vuelo y sd de 40
  dist_llegadas = c(dist_llegadas, norm_truncada(189, horarios-100, 40))
}
dist_llegadas = sort(dist_llegadas)
dist_llegadas


Llegada=function(TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
                 Ncheckin,Nseguridad,Npasaporte,Nembarque,xS){
  # distribucion de llegada
  TL=dist_llegadas[1]
  dist_llegadas <<- dist_llegadas[-1] 
  if(sum(dist_llegadas)==0){
    TL=Inf
  }
  u=runif(1)
  if (u<0.3){ # porcentaje de personas que no hace el checkin online, van a checkin
    # van al checkin con menos cola
    j= which.min(Ncheckin)
    Ncheckin[j]=Ncheckin[j]+1
    if(Ncheckin[j]==1){ # genera siguiente fin de checkin_j si no habia nadie
      DScheckin=norm_truncada(1,3,1.5)
      TScheckin[j]=TM+DScheckin
    }
    # actualizacion de sumas
    SUMAcheckin=SUMAcheckin+(sum(Ncheckin)-1)*(TM-TANT)
    SUMAseguridad=SUMAseguridad+sum(Nseguridad)*(TM-TANT)
    SUMApasaporte=SUMApasaporte+Npasaporte*(TM-TANT)
    SUMAembarque=SUMAembarque+sum(Nembarque)*(TM-TANT)
    TANT=TM
  }else{ #van a seguridad directamente
    #mandamos a la cola mas vacia
    if (sum(xS)!=0){ # si no estan todas las maquinas rotas
      #i sera el indice de la cola mas vacia cuya maquina esté operativa
      ind_maquinas_operativas=which(xS==1)
      i_filtrada=which.min(Nseguridad[ind_maquinas_operativas])
      i=ind_maquinas_operativas[i_filtrada]
    }else{
      i=which.min(Nseguridad)
    }
    Nseguridad[i]=Nseguridad[i]+1
    if(Nseguridad[i]==1){
      DSseguridad=norm_truncada(1,5,2.5)
      TSseguridad[i]=TM+DSseguridad
    }
    # actualizacion de sumas
    SUMAcheckin=SUMAcheckin+sum(Ncheckin)*(TM-TANT)
    SUMAseguridad=SUMAseguridad+(sum(Nseguridad)-1)*(TM-TANT)
    SUMApasaporte=SUMApasaporte+Npasaporte*(TM-TANT)
    SUMAembarque=SUMAembarque+sum(Nembarque)*(TM-TANT)
    TANT=TM
  }
  return(list(TL = TL,
              TScheckin = TScheckin,
              TSseguridad = TSseguridad,
              SUMAcheckin = SUMAcheckin,
              SUMAseguridad = SUMAseguridad,
              SUMApasaporte = SUMApasaporte,
              SUMAembarque = SUMAembarque,
              Ncheckin = Ncheckin,
              Nseguridad = Nseguridad,
              Npasaporte = Npasaporte,
              Nembarque = Nembarque,
              TANT = TANT))
}

Checkin=function(j,TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
                 Ncheckin,Nseguridad,Npasaporte,Nembarque,xS){
  # finalizacion de una persona en este servicio
  Ncheckin[j]=Ncheckin[j]-1
  DScheckin=norm_truncada(1,3,1.5)
  TScheckin[j]=ifelse(Ncheckin[j]==0,Inf,TM+DScheckin)
  # añade una persona a la cola de la siguiente etapa (seguridad)
  #mandamos a la cola mas vacia
  if (sum(xS)!=0){ # si no estan todas las maquinas rotas
    #i sera el indice de la cola mas vacia cuya maquina esté operativa
    ind_maquinas_operativas=which(xS==1)
    i_filtrada=which.min(Nseguridad[ind_maquinas_operativas])
    i=ind_maquinas_operativas[i_filtrada]
  }else{
    i=which.min(Nseguridad)
  }
  Nseguridad[i]=Nseguridad[i]+1
  if(Nseguridad[i]==1){ # genera siguiente fin de seguridad_i si no habia nadie 
    DSseguridad=norm_truncada(1,5,2.5)
    TSseguridad[i]=TM+DSseguridad
  }
  # actualizacion de sumas
  SUMAcheckin=SUMAcheckin+(sum(Ncheckin)+1)*(TM-TANT)
  SUMAseguridad=SUMAseguridad+(sum(Nseguridad)-1)*(TM-TANT)
  SUMApasaporte=SUMApasaporte+Npasaporte*(TM-TANT)
  SUMAembarque=SUMAembarque+sum(Nembarque)*(TM-TANT)
  TANT=TM
  return(list(TScheckin = TScheckin,
              TSseguridad = TSseguridad,
              SUMAcheckin = SUMAcheckin,
              SUMAseguridad = SUMAseguridad,
              SUMApasaporte = SUMApasaporte,
              SUMAembarque = SUMAembarque,
              Ncheckin = Ncheckin,
              Nseguridad = Nseguridad,
              Npasaporte = Npasaporte,
              Nembarque = Nembarque,
              TANT = TANT))
}

Seguridad=function(i,TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
                   Ncheckin,Nseguridad,Npasaporte,Nembarque,xS,horarios){
  if (xS[i]==1){ # si xS[i] era 1 es poque la maquina funcionaba y ha habido una finalizacion
    Nseguridad[i] = Nseguridad[i]-1
    
    #para mandar los pasajeros a un vuelo o otro usamos una weibull, que simula el tiempo de sobra que llevan los pasajeros 
    media_deseada <- 60
    scale <- media_deseada / gamma(1 + 1/2)
    datos_weibull <- rweibull(n = 1, shape = 2, scale = scale) - 10
    
    diferencias = horarios - TM
    indice <- findInterval(datos_weibull, diferencias)
    t = min(indice+1,length(dentro_UE)) #será al vuelo que va
    if (dentro_UE[t]=='No'){#añadimos que hay gente que va a pasaporte (vuelo i fuera de la UE)
      Npasaporte=Npasaporte+1
      if(Npasaporte==1){
        DSpasaporte=norm_truncada(1,2,1)
        TSpasaporte=TM+DSpasaporte
      }

    }else{#añadimos que hay gente que va directamente a vuelo j (dentro de la UE)
        Nembarque[t]=Nembarque[t]+1
        if(Nembarque[t]==1 & TM<horarios[t]){
          DSembarque=norm_truncada(1,0.2,0.1)
          TSembarque[t]=TM+DSembarque
          }
        }
    u=runif(1)
    if(u<0.005){ # probabilidad de rotura de la máquina
      print(c("SE HA ROTO LA MAQUINA",i))
      xS[i]=0
      a=runif(1,10,20) # tiempo de reparacion de la maquina
      TSseguridad[i]=TM+a # el siguiente tiempo de cambio sera cuando se arregle la maquina
      #repartimos esta cola entre el resto de maquinas operativas
      if(sum(xS)!=0){
        while (Nseguridad[i]>0.5){
          ind_maquinas_operativas=which(xS==1)
          j_filtrada=which.min(Nseguridad[ind_maquinas_operativas])
          j=ind_maquinas_operativas[j_filtrada]
          Nseguridad[i]=Nseguridad[i]-1
          Nseguridad[j]=Nseguridad[j]+1
        }
      }else{print('todas las maquinas estan rotas')}
    }else{ # si no se ha roto genero el siguiente tiempo de cambio de la manera usual
      DSseguridad=norm_truncada(1,5,2.5)
      TSseguridad[i]=ifelse(Nseguridad[i]==0,Inf,TM+DSseguridad)
    }
  }else{ # si xS[i] era 0 y ha habido una actualizacion es poque la maquina acaba de ser arreglada
    xS[i]=1
    print(c("SE HA ARREGLADO LA MAQUINA",i))
    TSseguridad[i] = Inf
  }
  # actualizacion de sumas
  SUMAcheckin=SUMAcheckin+sum(Ncheckin)*(TM-TANT)
  SUMAseguridad=SUMAseguridad+(sum(Nseguridad)+1)*(TM-TANT)
  SUMApasaporte=SUMApasaporte+(Npasaporte-1)*(TM-TANT)
  SUMAembarque=SUMAembarque+sum(Nembarque)*(TM-TANT)
  TANT=TM
  return(list(TSseguridad = TSseguridad,
              TSpasaporte = TSpasaporte,
              TSembarque = TSembarque,
              SUMAcheckin = SUMAcheckin,
              SUMAseguridad = SUMAseguridad,
              SUMApasaporte = SUMApasaporte,
              SUMAembarque = SUMAembarque,
              Ncheckin = Ncheckin,
              Nseguridad = Nseguridad,
              Npasaporte = Npasaporte,
              Nembarque = Nembarque,
              TANT = TANT,
              xS = xS ))
}

Pasaporte=function(TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
                   Ncheckin,Nseguridad,Npasaporte,Nembarque,horarios){
  # finalizacion de una persona en este servicio
  Npasaporte=Npasaporte-1
  DSpasaporte=norm_truncada(1,2,1)
  TSpasaporte=ifelse(Npasaporte==0,Inf,TM+DSpasaporte)
  # añade una persona a la cola de la siguiente etapa (embarque vuelo 2)
  vuelos_noUE=which(dentro_UE=="No")
  media_deseada <- 60
  scale <- media_deseada / gamma(1 + 1/2)
  datos_weibull <- rweibull(n = 1, shape = 2, scale = scale) - 10
  
  diferencias = horarios - TM
  indice <- findInterval(datos_weibull, diferencias[vuelos_noUE])
  t = vuelos_noUE[min(indice+1,length(vuelos_noUE))] #será al vuelo que va, quiero que sea dentroUE=no
  Nembarque[t]=Nembarque[t]+1
  if(Nembarque[t]==1 & TM<horarios[t]){
    DSembarque=norm_truncada(1,0.2,0.1)
    TSembarque[t]=TM+DSembarque
  }
  # actualizacion de sumas
  SUMAcheckin=SUMAcheckin+sum(Ncheckin)*(TM-TANT)
  SUMAseguridad=SUMAseguridad+sum(Nseguridad)*(TM-TANT)
  SUMApasaporte=SUMApasaporte+(Npasaporte+1)*(TM-TANT)
  SUMAembarque=SUMAembarque+(sum(Nembarque)-1)*(TM-TANT)
  TANT=TM
  return(list(TSpasaporte = TSpasaporte,
              TSembarque = TSembarque,
              SUMAcheckin = SUMAcheckin,
              SUMAseguridad = SUMAseguridad,
              SUMApasaporte = SUMApasaporte,
              SUMAembarque = SUMAembarque,
              Ncheckin = Ncheckin,
              Nseguridad = Nseguridad,
              Npasaporte = Npasaporte,
              Nembarque = Nembarque,
              TANT = TANT))
}

Embarque=function(i,TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
                  Ncheckin,Nseguridad,Npasaporte,Nembarque,Navion){
  # finalizacion de una persona en este servicio
  if(TM<horarios[i]){
    Nembarque[i]=Nembarque[i]-1
    Navion[i]= Navion[i]+1
    DSembarque=norm_truncada(1,0.2,0.1)
    TSembarque[i]=ifelse(Nembarque[i]==0,Inf,TM+DSembarque)
  }else{
    #si ya ha salido el vuelo cerramos la posibilidad de que los viajeros vayan a esta ventanilla 
    TSembarque[i]=Inf
  }
  # actualizacion de sumas
  SUMAcheckin=SUMAcheckin+sum(Ncheckin)*(TM-TANT)
  SUMAseguridad=SUMAseguridad+sum(Nseguridad)*(TM-TANT)
  SUMApasaporte=SUMApasaporte+(Npasaporte)*(TM-TANT)
  SUMAembarque=SUMAembarque+(sum(Nembarque)+1)*(TM-TANT)
  TANT=TM
  return(list(TSembarque = TSembarque,
              SUMAcheckin = SUMAcheckin,
              SUMAseguridad = SUMAseguridad,
              SUMApasaporte = SUMApasaporte,
              SUMAembarque = SUMAembarque,
              Ncheckin = Ncheckin,
              Nseguridad = Nseguridad,
              Npasaporte = Npasaporte,
              Nembarque = Nembarque,
              Navion = Navion,
              horarios = horarios,
              TANT = TANT))
}

#####################################################################
#Notación
#Ncheckin=número de personas en la checkin
#Nseguridad=número de personas en la seguridad
#Ncheckin=número de personas en la checkin
#Nseguridad=número de personas en la seguridad
#TM=reloj de simulación
#TL=instante de la próxima llegada
#TScheckin=instante de la próxima finalización del checkin
#TSseguridad=instante de la próxima finalización del seguridad
#TSpasaporte=instante de la próxima finalización del pasaporte
#TSembarque=instante de la próxima finalización del embarque
#Tmax=tiempo máximo de simulación (en min)
#DL=tiempo entre llegadas consecutivas
#DScheckin=duración de servicio checkin
#DSseguridad=duración de servicio seguridad
#DSpasaporte=duración de servicio pasaporte
#DSembarque=duración de servicio embarque
#SUMAcheckin=contador suma acumulada de áreas asociada a checkin
#SUMAseguridad=contador suma acumulada de áreas asociada a seguridad
#SUMApasaporte=contador suma acumulada de áreas asociada a pasaporte
#SUMAembarque=contador suma acumulada de áreas asociada a embarque

#xS[i]=1 si la seguridad_i funciona, xS[i]=0 si no
#####################################################################

#PROGRAMA PRINCIPAL
#Inicializamos las variables

horarios = c(4, 8, 11, 14, 17, 20, 23)*60 # para pasarlo a minutos
dentro_UE = c('Si', 'Si', 'No', 'Si', 'Si', 'No', 'Si')

num_segur = 6 #este es el numero de controles de seguridad (con sus respectivas colas) en paralelo
num_checkin = 2 #este es el numero de checkins (con sus respectivas colas) en paralelo
num_vuelos= length(horarios) #habria que poner 7 ya que es el numero de vuelos que tenemos en horarios y en dentro_UE

Ncheckin=rep(0,num_checkin)
Nseguridad=rep(0,num_segur)
Npasaporte=0
Nembarque=rep(0,num_vuelos) 
Navion=rep(0,num_vuelos)
TM=0
TANT=0
TScheckin=rep(Inf,num_checkin)
TSseguridad=rep(Inf,num_segur)
TSpasaporte=Inf
TSembarque=rep(Inf,num_vuelos)
Tmax=1440
xS=rep(1,num_segur)

SUMAcheckin=0
SUMAseguridad=0
SUMApasaporte=0
SUMAembarque=0

#Genero la primera llegada, va a checkin
DL=dist_llegadas[1];
dist_llegadas <<- dist_llegadas[-1] 
dist_llegadas
TL=DL
while(TM<Tmax){

  #Identificamos el evento que tiene lugar antes
  TM=min(TL,min(TScheckin),min(TSseguridad),TSpasaporte,min(TSembarque))
  Estado=1*(TM==TL)+2*(TM==min(TScheckin))+3*(TM==min(TSseguridad))+4*(TM==TSpasaporte)+5*(TM==min(TSembarque))
  if(Estado==1){
    
    k=Llegada(TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
              Ncheckin,Nseguridad,Npasaporte,Nembarque,xS)
    TL = k$TL
    TScheckin <- k$TScheckin
    TSseguridad <- k$TSseguridad
    SUMAcheckin <- k$SUMAcheckin
    SUMAseguridad <- k$SUMAseguridad
    SUMApasaporte <- k$SUMApasaporte
    SUMAembarque <- k$SUMAembarque
    Ncheckin <- k$Ncheckin
    Nseguridad <- k$Nseguridad
    Npasaporte <- k$Npasaporte
    Nembarque <- k$Nembarque
    TANT <- k$TANT
  }else if(Estado==2){
    j=which.min(TScheckin)
    k=Checkin(j,TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
              Ncheckin,Nseguridad,Npasaporte,Nembarque,xS)
    TScheckin <- k$TScheckin
    TSseguridad <- k$TSseguridad
    SUMAcheckin <- k$SUMAcheckin
    SUMAseguridad <- k$SUMAseguridad
    SUMApasaporte <- k$SUMApasaporte
    SUMAembarque <- k$SUMAembarque
    Ncheckin <- k$Ncheckin
    Nseguridad <- k$Nseguridad
    Npasaporte <- k$Npasaporte
    Nembarque <- k$Nembarque
    TANT <- k$TANT
  }else if(Estado==3){
    i=which.min(TSseguridad)
    k <- Seguridad(i,TM, SUMAcheckin, SUMAseguridad, SUMApasaporte, SUMAembarque, 
                   Ncheckin, Nseguridad, Npasaporte, Nembarque,xS,horarios)
    TSseguridad <- k$TSseguridad
    TSpasaporte <- k$TSpasaporte
    TSembarque <- k$TSembarque
    SUMAcheckin <- k$SUMAcheckin
    SUMAseguridad <- k$SUMAseguridad
    SUMApasaporte <- k$SUMApasaporte
    SUMAembarque <- k$SUMAembarque
    Ncheckin <- k$Ncheckin
    Nseguridad <- k$Nseguridad
    Npasaporte <- k$Npasaporte
    Nembarque <- k$Nembarque
    TANT <- k$TANT
    xS <- k$xS
  }else if(Estado==4){
    k=Pasaporte(TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
                Ncheckin,Nseguridad,Npasaporte,Nembarque,horarios)
    TSpasaporte <- k$TSpasaporte
    TSembarque <- k$TSembarque
    SUMAcheckin <- k$SUMAcheckin
    SUMAseguridad <- k$SUMAseguridad
    SUMApasaporte <- k$SUMApasaporte
    SUMAembarque <- k$SUMAembarque
    Ncheckin <- k$Ncheckin
    Nseguridad <- k$Nseguridad
    Npasaporte <- k$Npasaporte
    Nembarque <- k$Nembarque
    TANT <- k$TANT
  }else if(Estado==5){
    i=which.min(TSembarque)
    k=Embarque(i,TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
               Ncheckin,Nseguridad,Npasaporte,Nembarque,Navion)
    TSembarque <- k$TSembarque
    SUMAcheckin <- k$SUMAcheckin
    SUMAseguridad <- k$SUMAseguridad
    SUMApasaporte <- k$SUMApasaporte
    SUMAembarque <- k$SUMAembarque
    Ncheckin <- k$Ncheckin
    Nseguridad <- k$Nseguridad
    Npasaporte <- k$Npasaporte
    Nembarque <- k$Nembarque
    Navion <- k$Navion
    horarios <- k$horarios
    TANT <- k$TANT
  }
}
print(c("En el instante de parada había en la primera etapa",Ncheckin,"personas"))
print(c("En el instante de parada había en la segunda etapa",Nseguridad,"personas"))
print(c("En el instante de parada había en la tercera etapa",Npasaporte,"personas"))
print(c("En el instante de parada había en la cuarta etapa",Nembarque,"personas"))
print(c("En el instante de parada había en el avion",Navion,"personas"))
