#Chapter 3.4: Exercises: 7 and 8
#Create a user defined function named studentfunction that transposes a numeric matrix (columns become rows) and subsets the matrix in a way so that only the first 5 rows remain in the dataset.

studentfunction<-function(matrix){
  matrix<-t(matrix)
  if(nrow(matrix) > 5){
    matrix <- matrix[1:5,]
  }
  return(matrix)
}
  
#a) Use the studentfunction function to transform the matrix created in Exercise 7 from chapter 2.

my_matrix<-matrix(c(10,11,9,15,19, 52, 19, 7, 10, 22, 28, 40, 6, 99, 33, 35, 26, 5, 87, 91, 0, 12, 16, 81,200),byrow=TRUE,nrow=5)    
my_matrix
studentfunction(my_matrix)

#b) Use the studentfunction function to transform the matrix that was given as an example in chapter 2.1
X3 <- matrix(1:9, byrow = TRUE, nrow = 3)
X3
studentfunction(X3)

#8.Create a user defined function named transformmatrix that takes the diagonal of a matrix and calculates a vector with two elements. Element one is the mean of the diagonal and element two is the median.
transformmatrix<-function(trans_matrix){
 vector_1<-c(mean(diag(trans_matrix)),median(diag(trans_matrix)))
 return(vector_1)
}

#a) Use the transformmatrix function to transform the matrix created in Exercise 7 from chapter 2.
my_matrix<-matrix(c(10,11,9,15,19, 52, 19, 7, 10, 22, 28, 40, 6, 99, 33, 35, 26, 5, 87, 91, 0, 12, 16, 81,200),byrow=TRUE,nrow=5)    
transformmatrix(my_matrix)

#b) Use the transformmatrix function to transform the matrix that was given as an example in chapter 2.1
X3 <- matrix(1:9, byrow = TRUE, nrow = 3)
transformmatrix(X3)

#9. For the iris dataset (no need to call a function, iris is part of the base R) create for loop that does the following to each observation:
#a) changes the Species column from a character type to numeric. Assign 1 for setosa, 2 for virginica, and 3 for versicolor,

new_iris<-iris
for (i in 1:nrow(new_iris)){
  if (new_iris$Species[i]=="setosa"){new_iris$Species<-gsub("setosa","1",new_iris$Species)}
  else if(new_iris$Species[i]=="verginica"){new_iris$Species<-gsub("verginica","2",new_iris$Species)}
  else {new_iris$Species<-gsub("versicolor","3",new_iris$Species)}
}
new_iris$Species<-as.numeric(new_iris$Species)
new_iris
#b) creates a new column that groups the Petal.Length into 3 groups: group#1 for Petal.Length from 0 to 2, group #2 from 2.01 to 4.5, and group #3 from 4.51 to 7.
new_iris$new_col <- c()
for (i in 1:nrow(new_iris)){
  if (new_iris$Petal.Length[i]<=2){new_iris$new_col[i]<-"group#1"}
  else if (new_iris$Petal.Length[i]>2.01 && new_iris$Petal.Length[i]<=4.5){new_iris$new_col[i]<-"group#2"}
  else{new_iris$new_col[i]<-"group#3"}
}
new_iris

#3.Using the iris dataset:
#a) combine the Setosa and Versicolor into group “0” and label the Virginica to “1”. Create a new variable called iris$Group with the 0 or 1 labels,

myiris<-iris
myiris$iris_Group<-c()
for (i in 1:nrow(myiris)){
  if (myiris$Species[i]=="setosa" ||myiris$Species[i]=="versicolor"){myiris$iris_Group[i]<-"0"}
  else{myiris$iris_Group[i]<-"1"}
}
myiris

#b) build a logistic regression model using any available data that will predict the observation being Virginica ( value of 1 in Group variable),

myiris$iris_Group<-as.numeric(myiris$iris_Group)
virginica_pred<-glm(iris_Group~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=myiris,family = "binomial")
summary(virginica_pred)

#only use the significant variables which are Petal.Length and Petal.Wodth

#c) calculate the probability of a new plant being a Virginica for the following parameters:

#Sepal.Width =5 Petal.Length =10 Petal.Width =7 Sepal.Length=9
new_plant <- data.frame(Sepal.Length=9, Sepal.Width=5,Petal.Length=10,Petal.Width=7)
new_plant_prob <- predict(virginica_pred, new_plant, type='response')
new_plant_prob

