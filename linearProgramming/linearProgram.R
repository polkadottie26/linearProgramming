# Linear Programming: optimization problem

## let
# T be the number of tables, C the number of chairs
#
# constraints
# 2*T + 3*C ≤ 6000    -- labor constraint, 6000 hrs available/wk
# 3*T + 2*C ≤ 9000   -- wood constraint, 9000 ft2 available/wk

#Profit:  $12T + $10C – find values of T and C, within 
#the constraints that maximize profit

#Plot the constraints:

plot(0,0, xlim=c(0,1500), ylim=c(0,1200), pch='.', ylab='tables', xlab='chairs')

#equation for labor constraint
#When T = 0, C = 1000
#When C = 0, T = 2000
# labor constraint: T + 2*C < 2000
lines(c(0,1000),c(2000,0), col='blue', cex=10 )

#equation for wood constraint 
#when T=0, C= 3000, and
#when C=0, T = 900
# wood constraint 10T + 3C < 9000
lines(c(0,3000), c(900, 0), col='red', cex=10)

# profit = 12*T + 10*C

#profit line.. pick a fixed profit, say 8400
profit=8400

for (profit in seq(from=8400, to =19000, length.out=3)) {
  print(profit)
  chairsP=seq(from=0, to=1000, length.out = 20)
  tablesP = (profit-10*chairsP)/12

  lines(tablesP, chairsP, col='green')
}
legend('topright', c('Wood Constraint','Labor Constraint', 'Profit Lines'), 
       col=c('red','blue','green'), lty=1)
grid()

##   use lpsolve package

library(lpSolve)
help(package='lpSolve')

#Solve: maximize profit

# Equations:
#  Profit = 12*T + 10*c
#  Wood: 10*T + 3*C < 9000
#  Labor: T + 2*C < 2000
  

# solve our Tables and Chairs:
f.obj = c(12, 10)
f.con = matrix(c(1, 2, 10, 3), nrow=2, byrow=TRUE)
f.dir = c( "<=", "<=")
f.rhs = c(2000, 9000)

TCsolution = lp (direction = "max", f.obj, f.con, f.dir, f.rhs)

print(TCsolution)

# how many T and C?
print(TCsolution$solution)

# add constraint c = 4*T
f.con = matrix(c(1, 2, 10, 3, 1, -4), nrow=3, byrow=TRUE)
f.rhs = c(2000, 9000, 0)
f.dir = c( "<=", "<=", "=")

TCsolution2 = lp (direction = "max", f.obj, f.con, f.dir, f.rhs)
print(TCsolution2)

# how many T and C?
print(TCsolution2$solution)


#------------------------------------------------

#  find a solution to 
#  Minimize cost 6x+5y,
#  subject to:
#    3x + y >= 10, 
#     x + y >= 5, 
#     x     >= 3

  
