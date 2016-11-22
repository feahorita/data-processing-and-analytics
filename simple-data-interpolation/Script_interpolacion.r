#################################################################################################################################
#Subrutina que interpola los valores de nivel de manera lineal para que coincidan con las entradas al modelo
#################################################################################################################################
rm(list=ls()) #limpia todos los valores
h1<-read.delim("h.csv", header = FALSE, sep = ";") #Lee el archivo de los datos de nivel en un formato csv
h1<-h1[,1]
h<-h1*1000
HandD<-read.delim("H and D.csv", header = FALSE, sep = ";") #Lee el archivo de los datos de nivel en un formato csv
#Revisar siempre que los decimales estén como punto y que el separador entre valores sea ;
H<-HandD[,1] #carga la primera columna como el valor de H
D<-HandD[,2] #carga la segunda columna como el valor de D
m<-length(t(H)) # Tamaño vector H
n<-length(t(h)) # Tamaño vector h
d<-c() #Vector columna vacio
k<-1
x<-1
c<-10000
ch<-1
interp=matrix(data=NA, nrow=n, ncol=2)
# identação (recuo para os blocos de comandos)
for (i in x:n) { #For para correr todos los valores del vector tiempo requerido
  for (j in k:m) { #For para buscar los datos en todo el vector de tiempo que se tiene
    fh2<-j
    fh3<-h[i]
    fh4<-H[j]
    if (h[i]>H[j]) { #Este y el siguiente if son para que el encuentre los dos valores para la interpolación
      if (h[i]<H[j+1]) {
        d[i]<-(D[j+1]-D[j])/(H[j+1]-H[j])*(h[i]-H[j])+D[j] # Interpolación dentro del intervalo
        k<-j
      } else {
        if (h[i]==H[j+1]) { #If para asignar el valor del extremo superior si coinciden los tiempos
          d[i]<-D[j+1]
          k<-j
        }
      }
    }
    if (h[i]==H[j]) { #If para asignar el valor del extremo inferior si coinciden los tiempos
      d[i]<-D[j]
      k<-jwa
    }
  }
  ch<-ch+1
  
  if ((ch %% 1000) == 0) {
    interp<-cbind(h,d) #matriz de valores interpolados
    write.csv(interp,"hadnd.csv") #valores interpolados por tiempo y valor del dato
  }
  
}
interp<-cbind(h,d) #matriz de valores interpolados
write.csv(interp,"hadnd.csv") #valores interpolados por tiempo y valor del dato