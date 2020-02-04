##############
#The for loop#
##############

for(xxxxxxxx){#opening i loop
  print(i)
}#closing i loop
###########################################
#creating a new object(vector) with a loop#
###########################################
my_vect <- c()
for(xxxxxxxxxx){
  xxxxxxxxxxxxx
  print(my_vect)
}#closing z loop

################
# Exercise : 5-7 minutes
# Create a for loop that takes the value of purpose for each observation 
#and puts it to a new vector
################


#################
#Nested for loop#
#################

my_df <- as.data.frame(matrix(nrow=4, ncol=3))
for (a in 1:3){#this loop goes over the columns
  for(b in 1:4){#this loop goes over the rows
    my_df[b,a]<- a*b
  }
}
print(my_df)

##############
#The while loop#
##############

z <- 1
while(z < 20){
  z <- z+1 
  print(z)
}

###########################
#One condition if statment#
###########################
pk <- c(1,2,3,4,5,6)

if(pk[2]==2){pk[1]<-2}
if(sum(pk)>10){pk <- NULL}

#############################
#Multi condition if statment#
#############################

pk <- c(6,5,4,3,2,1)

if(pk[2]==2){pk[1]<-2}else{pk[2]<-2}
if(sum(pk)>10){pk <- NULL}

#########################################
## Exercise:
# Create a a for loop that checks every observation in the good_bad variable
# If the value is one (good), then mark the observation as "positive" in a new variable called description
##########################################

########################
#User defined functions#
########################


my_function <- function(x, y, z){ #x,y,z are inputs
  
  #This is the body of the function that has to return an object . The body will use all the variables specified in the function call.
  my_final_obj <- x+y+z
  return(my_final_obj)
}
new <- my_function(x=1, y=2, z=3)#I'm calling "myfunction"

##################################
## Exercise:
# What task can a User Defined Function automate in
#the German Creadit Dataset
###################################

