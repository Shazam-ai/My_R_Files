mtcars
summary(mtcars)
apply(mtcars,2,sd)
apply(mtcars,2,disp)
my_func <- function(x) {
  my_mean <- mean(x)
  my_std <- sd(x)
  return(c(my_mean,my_std))
}
my_func(x=mtcars$hp)
my_func(x=mtcars$disp)
my_func(x=mtcars$mpg)
table(mtcars$gear)
