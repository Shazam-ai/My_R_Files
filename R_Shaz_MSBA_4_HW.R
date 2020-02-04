studentfunction<-function(mat){
  mat<-t(mat)
  if(nrow(mat) > 5){
    mat<- mat[1:5,]
  }
  return(mat)
}
sz_matrix<-matrix(c(10,11,9,15,19, 52, 19, 7, 10, 22, 28, 40, 6, 99, 33, 35, 26, 5, 87, 91, 0, 12, 16, 81,200),byrow=TRUE,nrow=5)    
sz_matrix
studentfunction(sz_matrix)
xf <- matrix(1:9, byrow = TRUE, nrow = 3)
xf
studentfunction(xf)

transformmatrix<-function(trans_matrix){
  v1<-c(mean(diag(trans_matrix)),median(diag(trans_matrix)))
  return(v1)
}

sz_matrix<-matrix(c(10,11,9,15,19, 52, 19, 7, 10, 22, 28, 40, 6, 99, 33, 35, 26, 5, 87, 91, 0, 12, 16, 81,200),byrow=TRUE,nrow=5)    
transformmatrix(sz_matrix)

xf<- matrix(1:9, byrow = TRUE, nrow = 3)
transformmatrix(xf)
head(xf)
n_iris<-iris
for (i in 1:nrow(n_iris)){
  if (n_iris$Species[i]=="setosa"){n_iris$Species<-gsub("setosa","1",n_iris$Species)}
  else if(n_iris$Species[i]=="verginica"){n_iris$Species<-gsub("verginica","2",n_iris$Species)}
  else {n_iris$Species<-gsub("versicolor","3",n_iris$Species)}
}
n_iris$Species<-as.numeric(n_iris$Species)
n_iris

n_iris$new_col <- c()
for (i in 1:nrow(n_iris)){
  if (n_iris$Petal.Length[i]<=2){n_iris$new_col[i]<-"group#1"}
  else if (n_iris$Petal.Length[i]>2.01 && n_iris$Petal.Length[i]<=4.5){n_iris$new_col[i]<-"group#2"}
  else{n_iris$new_col[i]<-"group#3"}
}
n_iris

sziris<-iris
mziris$iris_Group<-c()
for (i in 1:nrow(sziris)){
  if (sziris$Species[i]=="setosa" ||sziris$Species[i]=="versicolor"){sziris$iris_Group[i]<-"0"}
  else{sziris$iris_Group[i]<-"1"}
}
sziris

sziris$iris_Group<-as.numeric(sziris$iris_Group)
virginica_pred<-glm(iris_Group~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=sziris,family = "binomial")
summary(virginica_pred)

np<- data.frame(Sepal.Length=9, Sepal.Width=5,Petal.Length=10,Petal.Width=7)
np_pr <- predict(virginica_pred, np, type='response')
np_pr






