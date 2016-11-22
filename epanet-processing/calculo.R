#import junctions
junctions<-read.table("junctions.txt", quote = "\"'", na.strings = "NA", colClasses="character")

#import pipes
pipes<-read.table("pipes.txt", quote = "\"'", na.strings = "NA", colClasses="character")

#Provide Qt
Qt<-16.672

#Calc Lt
Lt<-0.00
for (variable in 1:nrow(pipes)) {
  Lt<-Lt+as.double(pipes[variable,4])
}

#Define q
q<-Qt/Lt

pipe<-""
jun_pipe<-c()
jun_pipe_aux<-c()
 
for (variable in 1:nrow(pipes)) {
  jun_pipe_aux<-c()
  pipe<-trimws(pipes[variable,2])
  pipe<-gsub("'", "", pipe)
  
  #store from 2 column
  for (var in 1:nrow(pipes)) {
    pipe2<-trimws(pipes[var,2]) 
    pipe2<-gsub("'", "", pipe2)
    
   if (pipe == pipe2) {
     jun_pipe_aux <- rbind(jun_pipe_aux, (as.double(pipes[var,4])/2))
   }
  }
  
  #store from 3 column
  for (var in 1:nrow(pipes)) {
    pipe2<-trimws(pipes[var,3])
    pipe2<-gsub("'", "", pipe2)
    
    if (pipe == pipe2) {
      jun_pipe_aux <- rbind(jun_pipe_aux, (as.double(pipes[var,4])/2))
    }
  }
  
  #sum
  sum_pip<-0
  for (var in 1:nrow(jun_pipe_aux)) {
    sum_pip<-sum_pip + jun_pipe_aux[var]
  }
  
  jun<-c(pipe, sum_pip)
  jun_pipe <- rbind(jun_pipe, jun)
}

#fulfil junctions file with demand
for (variable in 1:nrow(junctions)) {
   
  jun <- trimws(junctions[variable,1])
  jun<-gsub("'", "", jun)
  
  for (var in 1:nrow(jun_pipe)) {
    if (jun == jun_pipe[var, 1]) {
      junctions[variable,3]<-(as.double(jun_pipe[var, 2])*q)
    }
  }
}

write.table(junctions, "junctions1.txt", quote=FALSE, sep = "\t", col.names = FALSE, row.names = FALSE, na = "")