#MINEPS <- 10e-10
#M_BIG_NUMBER <- 10e10
MINEPS <- 0.01
#assignInNamespace("MINEPS", ns="ror", value=10e-10)
M_BIG_NUMBER <- 99

model = list()
model$lhs =  matrix(c(1,1,1,1,1), ncol=5)
print(model$lhs)
model$dir = c("<=")
model$rhs = c(1)
#View(model)
types <- c("B","B","B","B","B")
objective <- c(1,1,1,1,1)
obj <- L_objective(objective)
roiConst <- L_constraint(L = model$lhs, dir =model$dir, rhs=model$rhs)
lp <- OP(objective=obj, constraints=roiConst, maximum=TRUE, types=types)

res <- ROI_solve(lp, .solver)
print(res)
print(res$solution)