alloy <- as.data.frame(Alloy_working_data)
summary(alloy)
names(alloy) <- c("week", "visits","unique","pageviews","pagevisits","avg_time","bounce","newvisits","revenue","profit","lbs_sold","inquiries")
all_f <- glm(unique ~ visits+pageviews+pagevisits+avg_time+bounce+newvisits+revenue+profit+lbs_sold+inquiries, data=alloy)
summary(all_f)
head(alloy)

cor(revenue,lbs_sold)
cor(revenue,visits)
cor(revenue,pagevisits)


attach(alloy)
plot(lbs_sold,revenue,
     xlab="Pounds Sold ", ylab="Revenue ", pch=19)
scatterplot(lbs_sold ~ revenue, data=alloy)

studentfunction <- function(matrix) {
  numeric_matrix <- a(matrix)
  #subset first 5 rows
  if (nrow(numeric_matrix) >= 5) {
    return (numeric_matrix[1:5, ])
  }else{
    return (numeric_matrix)
  }
}
data <- c(10, 11, 9, 15, 19, 52, 19, 7, 10, 22, 
          28, 40, 6, 99, 33, 35, 26, 5, 87, 91,
          0, 12, 16, 81, 200)
matrix_b <- matrix(numbers, byrow = TRUE, nrow=5)
print(studentfunction(matrix_b))

matrix1<-matrix(c(10,11,9, 15, 19, 52, 19, 7 ,10 ,22 ,28 ,40, 6, 99, 33, 35, 26, 5, 87, 91, 0 ,12, 16, 81,200),5,5,byrow=TRUE)
matrix2<-matrix(c(1,2,3,4,5,6,7,8,9),3,3,byrow=TRUE)

studentfunction <- function(a){
  trans<-t(a[1:nrow(a),])
  return(trans)
}
studentfunction(matrix1)
  
matrix1<-matrix(c(10,11,9, 15, 19, 52, 19, 7 ,10 ,22 ,28 ,40, 6, 99, 33, 35, 26, 5, 87, 91, 0 ,12, 16, 81,200),5,5,byrow=TRUE)
matrix2<-matrix(c(1,2,3,4,5,6,7,8,9),3,3,byrow=TRUE)
#Check data
print(matrix1)
print(matrix2)
#Create function
studentfunction <- function(a){
  trans<-t(a[1:nrow(a),])
  return(trans)
}
#Check function
studentfunction(matrix1)
#b)
studentfunction(matrix2)

studentfunction <- function(matrix1){
  matrix2 <- t(matrix1)
  return(if (nrow(matrix2) < 5) {
    print(matrix2)} 
    else if (nrow(matrix2)>=5) {
      print(matrix2[1:5,])})
  vect_2 <- c(1:9)
  my_mat2.1 <- matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)
  my_mat2.1
  studentfunction(my_mat2.1)
  print(my_mat2.1)

