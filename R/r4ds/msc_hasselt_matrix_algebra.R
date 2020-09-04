x = c(4,7,9,11)
y = c(3,5,13,15,7)
sum(x[1:4]) - sum(y[4:5]) == 7
prod(x[2:4]) - prod(y[4:5]) == 488
# y2 = y[4:5] - factorial(4)
prod(y[4:5] -factorial(4)) == 80
prod(y[4:5]) - sum(x[1:4]) == 74
a = matrix(c(-3,2,-6,5,7,-5,1,4,-2), nrow = 3, byrow = T)
b = matrix(c(12,12,16), nrow = 3)
solve(a,b)
A = matrix(c(3,2,-2,4,5,-1,8,7,-8), byrow = T, nrow = 3)
det(A)
solve(A)
