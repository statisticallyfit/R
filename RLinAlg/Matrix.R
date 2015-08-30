# lin alg book: 

# 5
outlet.job <- matrix(c(3,7,8,2,4,5,6,14,18,3,6,9), nrow=4, byrow=TRUE)
rownames(outlet.job) = c("Outlet 1", "Outlet 2", "Outlet 3", "Outlet 4")
colnames(outlet.job) = c("Executives", "Sales", "Others")
outlet.job

job.benefit = matrix(c(30000,7500,22500,4500,15000,3000), 
                      nrow=3, byrow=TRUE)
rownames(job.benefit) = colnames(outlet.job)
colnames(job.benefit) = c("Salary", "Fringe Benefits")
job.benefit

# multiply
outlet.job %*% job.benefit


# 6
people.ticket = matrix(c(2,5,8,4,12,3,6,15,10,3,9,12),nrow=4,byrow=TRUE)
month.people = matrix(c(32500,54600,121500,46300,37400,62800,136000,52900,29800,48500,99200,44100), nrow=3, byrow=TRUE)

month.ticket = month.people %*% people.ticket
month.ticket

# 7
fertilizer.chemical = matrix(c(10,10,5,25,5,5,0,10,20), nrow=3,byrow=TRUE)/100
fertilizer.field = matrix(c(5,2,4,2,1,1,3,1,3), nrow=3,byrow=TRUE)
chemical.field = t(fertilizer.chemical) %*% fertilizer.field
chemical.field


# 8
rocket.module = matrix(c(24,10,5,17,25,8,6,16,32,12,8,22,27,11,7,19), nrow=4,byrow=T)
chip.module = matrix(c(42,37,52,29,23,25,48,31,37,33,29,28,52,46,35,51), nrow=4,byrow=T)
chip.rocket = chip.module %*% t(rocket.module)
rocket.chip = t(chip.rocket)
rocket.chip

thisone <- function()
