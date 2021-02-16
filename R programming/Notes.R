###Matrixes example
#Normal methon
x <- matrix(1:6, ncol = 3,nrow = 2, byrow = TRUE, dimnames = list(c("X", "Y"), c("A", "B", "C")))
x
attributes(x)
x["X","C"]

a <- 0:3
b <- 10:13

#Common used method
X_01 <- cbind(a, b)
X_02 <- rbind(a, b)

X_01
X_02

###Factors examples
y <- factor(c("Man", "Woman", "Woman", "Man", "Woman", "Woman"))
y

table(y)
unclass(y)

###Way to obtain the width of a vector
seq_along(c(1,2,3,4,5,7,8))
