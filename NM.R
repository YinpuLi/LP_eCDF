source('LP_fix_inflec.R')

###################################################
##########  Nelder Mead Optimization ##############
###################################################


# now we have the best pars = c(93.9727493 , 0.9226362)

init_par_vec = c(93.9727493, 0.9226362)
init_loc_vec = pars2loc(init_par_vec) # 378 388
init_val_vec = pars2val(init_par_vec) #93.97275 96.48866





par_initial_min = loc2pars(init_loc_vec, x_grid = my_grid)
fn_min = solveLP_min
(fit1 = optim(par = par_initial_min,
              fn  = fn_min,
              method = "Nelder-Mead"))
# [1] "loc_vec =" "45"        "66"       
# [1] 108.910891   3.950863   0.000000
# [1] "loc_vec =" "50"        "71"       
# [1] 119.801980   3.950863   0.000000
# [1] "loc_vec =" "45"        "99"       
# [1] 108.91089109  14.84195195   0.00221868
# [1] "loc_vec =" "50"        "50"       
# [1] 119.801980  -6.940226   0.000000
# [1] "loc_vec =" "49"        "49"       
# [1] 117.079208  -1.494682   0.000000
# $par
# [1] 108.910891   3.950863
# 
# $value
# [1] 0
# 
# $counts
# function gradient 
# 5       NA 
# 
# $convergence
# [1] 0
# 
# $message
# NULL


pars2val(fit1$par)   # [1] 108.9109 160.8911



par_initial_max = init_par_vec
fn_max = solveLP_max_min

(fit2 = optim(par = par_initial_max,
              fn  = fn_max,
              method = "Nelder-Mead",
              control = list(reltol = 1e-10)))
# 
# [1] "loc_vec =" "39"        "41"       
# [1] 94.0594059  1.5994876 -0.1115176
# [1] "loc_vec =" "43"        "45"       
# [1] 103.46534653   1.59948758  -0.01981533
# [1] "loc_vec =" "39"        "99"       
# [1] 94.059405941 11.005428176 -0.008693292
# [1] "loc_vec =" "43"        "43"       
# [1] 103.465347  -7.806453   0.000000
# [1] "loc_vec =" "40"        "99"       
# [1] 96.410891089  6.302457879 -0.008307695
# [1] "loc_vec =" "41"        "43"       
# [1] 98.76237624  1.59948758 -0.03119279
# [1] "loc_vec =" "39"        "99"       
# [1] 94.059405941  6.302457879 -0.008693292
# [1] "loc_vec =" "41"        "41"       
# [1] 98.762376 -3.103483  0.000000
# [1] "loc_vec =" "40"        "61"       
# [1] 95.23514851  3.95097273 -0.02400083
# [1] "loc_vec =" "41"        "41"       
# [1] 97.5866337 -0.7519976  0.0000000
# [1] "loc_vec =" "40"        "47"       
# [1] 95.82301980  2.77523016 -0.02835565
# [1] "loc_vec =" "41"        "41"       
# [1] 96.998762  0.423745  0.000000
# [1] "loc_vec =" "40"        "44"       
# [1] 96.11695545  2.18735887 -0.03469206
# [1] "loc_vec =" "38"        "42"       
# [1] 91.41398515  2.18735887 -0.06842548
# [1] "loc_vec =" "39"        "42"       
# [1] 93.25108292  2.04039105 -0.08553184
# [1] "loc_vec =" "38"        "40"       
# [1] 91.1935334  1.4525198 -0.1089927
# [1] "loc_vec =" "39"        "41"       
# [1] 92.4243889  1.6362295 -0.1115176
# [1] "loc_vec =" "39"        "41"       
# [1] 93.2327119  1.1953261 -0.1115176
# [1] "loc_vec =" "39"        "41"       
# [1] 93.2373047  1.4065923 -0.1115176
# $par
# [1] 94.059406  1.599488
# 
# $value
# [1] -0.1115176
# 
# $counts
# function gradient 
# 19       NA 
# 
# $convergence
# [1] 0
# 
# $message
# NULL


pars2val(fit2$par)   # [1] 94.05941 99.00990


# Steve: using various numerical optimization routines available:

# based on previous experiments over N = 100,
# the max of maxLP is giving results in around (94, 100), with both inflection points falling within the range
# so I could initialize the par_initial_max within this area 
# and make N larger and larger.


ptm        = proc.time()


fit        = optim(
   par     = par_initial_max,
   fn      = fn_max,
   method  = "Nelder-Mead",
   control = list(reltol = 1e-10))

ptm_NM     = proc.time() - ptm
# user  system elapsed 
# 0.53    0.34    0.87 

old_best   = 1e60
new_best   = 0

it         = 0

time_rec   = NULL       # recording elapsed time 
impr_rec   = NULL
fitval_rec = NULL

methods    = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN")

while(old_best > (new_best + 0.01) && it < 10){
   cat("\n")
   cat("\n")
   it      = it + 1
   print(paste0("it = ", it))
   
   for(m in 1 : length(methods)){
      
      #print(methods[m])
      
      ptm  = proc.time()
      
      pre  = - fit$value
      
      print(paste0("pre fit val = ", pre))
      
      #suppressWarnings(try(
         fit = optim(par     = fit$par,
                     fn      = fn_max,
                     method  = methods[m],
                     control = list(reltol = 1e-10))
     # ))
      
      ptm_temp               = proc.time() - ptm
      
      print(paste0(methods[m], " uses ", ptm_temp[3], " s."))
      
      time_rec               = c(time_rec, ptm_temp[3])
      
      post                   = - fit$value
      
      fitval_rec             = c(fitval_rec, post)
      
      print(paste0("post fit val = ", post))
      
      impr_temp              = post - pre 
      
      print(paste0("improved = ", impr_temp))
      
      impr_rec               = c(impr_rec, impr_temp)
      
      cat("\n")
   }
   new_best                  = - fit$value
   
}

TIME_rec           = matrix(time_rec, nrow = 10, ncol = 5, byrow = T)
colnames(TIME_rec) = methods

IMPR_rec           = matrix(impr_rec, nrow = 10, ncol = 5, byrow = T)
colnames(IMPR_rec) = methods

TIME_rec
IMPR_rec

# need to check the final fit 
# are the inflection values approaching to the lower/upper bounds of the domain?
pars2val(fit$par)   # [1] 93.97275 96.48866
# the answer is no, the best inflection vals is around (94, 96.5)



new_best # 0.1634891(N = 100, it = 10)
         # 1.481341 (N = 1000, it = 5)

fit
fit$par
fit$value

# $par
# [1] 93.9727493  0.9226362
# 
# $value
# [1] -0.1634891
# 
# $counts
# function gradient 
# 10000       NA 
# 
# $convergence
# [1] 0
# 
# $message
# NULL












