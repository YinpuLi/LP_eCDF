source("LP_fix_inflec.R")
library(parallel)

(numCores           = detectCores())


get_greedy_val      = function(
    inner_knots_num = N){
    
    val_vec         = NULL
    
    for(i in 1:(inner_knots_num + 2 - 5)){
        for(j in (i+2):(inner_knots_num + 2 - 3)){
            val_vec = rbind(val_vec, c(i, j))
            #print(c(i,j))
        }
    }
    
    return(val_vec)
}

get_greedy_pars     = function(
    inner_knots_num = N
){
    pars_vec        = NULL
    
    val_vec         = get_greedy_val(inner_knots_num)
    for(i in 1:dim(val_vec)[1]){
        pars_temp   = val2pars(val_vec[i, ])
        pars_vec    = c(pars_vec, pars_temp)
    }
    return(pars_vec)
}

# some experimenst in parallel computing

result_rec = NULL
pars_vec_temp = rbind(c(95,1), c(95,2), c(95,3))
ptm = proc.time()

result_rec = mclapply(1:nrow(pars_vec_temp),
                      function(i){
                          solveLP_fix(pars = pars_vec_temp[i,])
                      })


ptm = proc.time() - ptm
# N = 1000
# user  system elapsed 
# 4.03    0.28    4.32 

ptm = proc.time()
for(i in 1:3){
    solveLP_fix(pars = pars_vec_temp[i,])
}

ptm = proc.time() - ptm

# the mclapply is not for Windows!
# end of experiments over parallel computing.
# Use doParallel and foreach!

library(doParallel)
library(foreach)

pars_vec = get_greedy_pars()

system.time(
    
    results <- foreach(i = 1 : dim(pars_vec)[1]) %dopar%{
        solveLP_fix(pars = pars_vec[i, ])
    }
    
    
)
# user  system elapsed 
# 3.71    0.12    3.83








