
testing<-read.csv("pml-testing.csv",na.strings=c("#DIV/0!","NA","") )
testing<-headCleaner(testing)
summary(testing)
done<-predict(modelFit,newdata=testing)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
setwd("C:\\Users\\PC\\Documents\\PracticalMacinelearning\\Answers")
pml_write_files(done)
