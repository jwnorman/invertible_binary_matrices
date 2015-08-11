# BruteForce.R

# library(MASS)
#library(parallel)
#library(ggplot2)
#library(grid)
#source("multiplot.R")

brute_force <- function(n) {
	grid <- expand.grid(rep(list(c(0, 1)), n^2))
	length(which(apply(grid, MARGIN = 1, function(x) {
		mat <- matrix(unlist(x), nrow = n, ncol = n)
		det(mat)
	}) != 0)) / 2^2^n
}

grid <- expand.grid(rep(list(c(0, 1)), 5^2))
matrix((grid[2091995,]), nrow = 5)
det(matrix((grid[2091995,]), nrow = 5))

beg2 <- Sys.time()
brute_force(2) # .375
end2 <- Sys.time()
dif2 <- end2 - beg2 # .03972888

beg3 <- Sys.time()
brute_force(3) # .6796875
end3 <- Sys.time()
dif3 <- end3 - beg3 # .03090286

beg4 <- Sys.time()
brute_force(4) # .3442383
end4 <- Sys.time()
dif4 <- end4 - beg4 # 1.813017

beg5 <- Sys.time()
brute_force(5) # .002913717
end5 <- Sys.time()
dif5 <- end5 - beg5 # 24.86995 minutes

beg6 <- Sys.time()
brute_force(6)
end6 <- Sys.time()
dif6 <- end6 - beg6