
getData <- function(){
  data(iris)
  dataDT <<- iris
 # DTdata <-cbind(iris[,-ncol(iris)],iris[,-ncol(iris)],  iris)
#  names(DTdata) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "a", "b", "c", "d", "e", "f", "g", "h", "Species" )
  return (dataDT)
  
}

read_csv <- function(){
  data <- read.csv('diabetes_data.csv', header=TRUE)
  data$Outcome <- factor(data$Outcome)
  return(data)
}

class_fit <- function(string, xx, yy){
  inc <- which(string == 1)  #'inc' includes those features/variables for which 'string' contains 1
  data <- read_csv()
  
  train<- data[1: 537, ]
  test <- data[538: 768, ]
  if(sum(inc)==0)                          
    return(0)
  
  outcome <- "Outcome"
  inputs <- paste(names(xx)[inc], collapse =" + ")
  fRpart <- as.formula(paste(outcome, inputs, sep=" ~ "))
  
  DT <- rpart(formula = fRpart, method="class", control = rpart.control(minsplit = 3),
              data = train)
  
  t_pred = predict(DT,test, type='class')
  
  return( mean(test$Outcome == t_pred))
  
}

classfeatureFitness <- function(string,xx,yy) {
 
  inc <- which(string == 1)  #'inc' includes those features/variables for which 'string' contains 1
 
  if(sum(inc)==0)                          
    return(0)                          
  
   
  outcome <-"Species"
  inputs <- paste(names(xx)[inc], collapse =" + ")
  
  fRpart <- as.formula(paste(outcome, inputs, sep=" ~ "))
  
  DT <- rpart(formula = fRpart, method="class", control = rpart.control(minsplit = 3),
              data = dataDT)
 
  
  t_pred = predict(DT,dataDT, type='class')
  
  #Maximise accuracy
  return( mean(dataDT$Species == t_pred))
  
  #Maximise accuracy and minimise the number of features
  #return( mean(dataDT$Species == t_pred) * (1 - sum(string == 1)/length(string) ) )
}

