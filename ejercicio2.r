##Inicialmente tenemos 5 poblaciones con 20 individuos diploides, con una frecuencia de A1(p)=0.2 y una frecuencia de A2(q)=1-p, las observaremos por 100 generaciones para saber como va cambiando la frecuencia de A1 
generations<-100
populations<-5
alelos<-40
p<-0.2
#se crea la matriz en donde se guardar�m los valores de p para cada generaci�n y cada poblaci�n, en la primera generaci�n todas las poblaciones inician con p=0.2
simulation <- matrix(NA,5,100)
simulation[1:5,1]=rep(p,5)
frec<-numeric(populations) #Vector vac�o donde se guardan las frecuencias de p para cada poblaci�n, en este caso 5

#para cada una de las generaciones
for (i in c(2:generations)){
  #para cada una de las poblaciones
  for (repe in c(1:populations)){
    A1<-0 #se inicializa el valor de A1 en cero para contabilizar el n�mero de alelos que se obtienen al final del ciclo for que viene a continuaci�n
    for (r in c(1:alelos)){ #para cada alelo en la poblaci�n
      rand=runif(1,0,1) #Se selecciona un valor aleatorio que est� entre 0 y 1
      if (rand<as.numeric(simulation[repe,i-1])){ #si el n�mero random para cada alelo es menor a la p de la generaci�n anterior se contabiliza como un alelo A1
        A1<-A1+1
      }
    }
    frec[repe]<-A1/alelos #se divide el numero de alelos A1 entre el total de alelos para obtener su frecuencia y se guarda en la matriz simulation
  }
  simulation[,i]<-frec
}


#Una vez obtenidas las frecuencias del alelo A1 en cada generaci�n y en cada poblaci�n graficamos para observar el comportamiento de dichas frecuencias.

plot(simulation[1,],type='l',xlab='generaciones',ylab='frecuencia de P',col=3, main='Simulaci�n de deriva g�nica para 5 poblaciones')
lines(simulation[2,],type='l',col=22)
lines(simulation[3,],type='l',col=66)
lines(simulation[4,],type='l',col=55)
lines(simulation[5,],type='l',col=44)
