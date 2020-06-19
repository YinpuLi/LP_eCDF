source('dens_obj.R')
source('constraints.R')


######### Functions:
# solveLP_fix: provide the min and max in density estimation problem
# solveLP_min: provide the min in density estimation problem
# solveLP_max: provide the max in density estimation problem
# solveLP_max_min: trying to solve LP_max problem, but adaptive to Nelder-Mead algorithm, 
                #  as NM minimize functions by default,
                #  we should maiximize f(x) by minimizing -f(x)
                #  that is, lp_min(-obj func) 

solveLP_min         = function(
    pars            = init_par_vec,     # reparameterizing
    inner_knots_num = N,
    domain_min      = domain1,
    domain_max      = domain2,
    interest        = 100
){
    #criteria_lower  = ecdf_ks_ci(sort(obs_data))$lower     # the lower bound of current KS bands
    #criteria_upper  = ecdf_ks_ci(sort(obs_data))$upper     # the upper bound of current KS bands
    cur_grid        = set_up_grid(inner_knots_num, domain_min, domain_max)
    
    obj.coeff       = get_density_obj_coeff(inner_knots_num, interest, domain_min, domain_max)
    
    const.mat       = get_const_mat(inner_knots_num)
    
    loc_vec         = pars2loc(pars, cur_grid)
    
    inflec_loc1     = loc_vec[1]
    inflec_loc2     = loc_vec[2]
    

    const.dir       = get_const_dir(inner_knots_num, cur_grid, pars)
    const.rhs       = get_const_rhs(inner_knots_num)
    
    lp_min          = lp("min", obj.coeff, const.mat, const.dir, const.rhs)
    
    #print(c(pars, lp_min$objval))
    
    return(lp_min$objval)
    
    
}

solveLP_max         = function(
    pars            = init_par_vec,                        # reparameterization  
    inner_knots_num = N,
    domain_min      = domain1,
    domain_max      = domain2,
    interest        = 100
){
    cur_grid        = set_up_grid(inner_knots_num, domain_min, domain_max)
    obj.coeff       = get_density_obj_coeff(inner_knots_num, interest, domain_min, domain_max)
    
    const.mat       = get_const_mat(inner_knots_num)
    
    loc_vec         = pars2loc(pars, cur_grid)
    
    inflec_loc1     = loc_vec[1]
    inflec_loc2     = loc_vec[2]
    
    
    
    const.dir       = get_const_dir(inner_knots_num, cur_grid, pars)
    const.rhs       = get_const_rhs(inner_knots_num)
    
    lp_max          = lp("max", obj.coeff, const.mat, const.dir, const.rhs)
    
    #print(paste0(pars, lp_max$objval))
    
    return(lp_max$objval)
    
    
}



solveLP_max_min     = function(
    pars            = init_par_vec,
    inner_knots_num = N,
    domain_min      = domain1,
    domain_max      = domain2,
    interest        = 100
){
    cur_grid        = set_up_grid(inner_knots_num, domain_min, domain_max)
    
    obj.coeff       = get_density_obj_coeff(inner_knots_num, interest, domain_min, domain_max)
    
    obj.coeff_adpt  = - obj.coeff
    
    const.mat       = get_const_mat(inner_knots_num)
    
    loc_vec         = pars2loc(pars, cur_grid)
    
    inflec_loc1     = loc_vec[1]
    inflec_loc2     = loc_vec[2]
    
    const.dir       = get_const_dir(inner_knots_num, cur_grid, pars)
    const.rhs       = get_const_rhs(inner_knots_num)
    
    lp_min          = lp("min", obj.coeff_adpt, const.mat, const.dir, const.rhs)
    
    #print(paste0(pars, lp_min$objval))
    
    return(lp_min$objval)
    
    
}


solveLP_fix         = function(
    pars            = init_par_vec,
    inner_knots_num = N,
    domain_min      = domain1,
    domain_max      = domain2,
    interest        = 100
){

    dir1            = "min";
    dir2            = "max"
    
    cur_grid        = set_up_grid(inner_knots_num, domain_min, domain_max)
    
    loc_vec         = pars2loc(pars, cur_grid)
    
    inflec_loc1     = loc_vec[1]
    inflec_loc2     = loc_vec[2]
    
    
    if(!is.na(interest)){
        obj.coeff   = get_density_obj_coeff(inner_knots_num, interest, domain_min, domain_max)
        
    }
    
    const.mat       = get_const_mat(inner_knots_num)
    const.dir       = get_const_dir(inner_knots_num, cur_grid, pars)
    const.rhs       = get_const_rhs(inner_knots_num)
    
    lp_min          = lp(dir1, obj.coeff, const.mat, const.dir, const.rhs)
    lp_max          = lp(dir2, obj.coeff, const.mat, const.dir, const.rhs)
    
    return(c(lp_min$objval, lp_max$objval))
}

# I am writing a new greedy function
solveLP_greedy_new  = function(
    
    inner_knots_num = N,
    domain_min      = domain1,
    domain_max      = domain2,
    interest        = 100
){
    dir1            = "min"; 
    dir2            = "max"
    
    cur_grid        = set_up_grid(inner_knots_num, domain_min, domain_max)
    
    
    if(!is.na(interest)){
        obj.coeff   = get_density_obj_coeff(inner_knots_num, interest, domain_min, domain_max)
        
    }
    
    const.mat       = get_const_mat(inner_knots_num)
    const.rhs       = get_const_rhs(inner_knots_num)
    
    # the location of reflection points will only influence the const_dir
    const.dir1      = get_valid_const_dir(inner_knots_num)
    const.dir2      = get_criteria_const_dir(inner_knots_num)
    
    obj_min         = NA
    obj_max         = NA
    mat             =  NULL  # matrix recording the (inflection1, inflection2, lp_min$val, lp_max$val)
    
   
    
    for(i in 1:(inner_knots_num + 2 - 5)){
        for(j in (i+2):(inner_knots_num + 2 - 3)){
            
            loc_vec = c(i, j)
            par_vec = loc2pars(loc_vec, cur_grid)
            
            const.dir3_temp = get_unimodality_const_dir(inner_knots_num, cur_grid, par_vec)
            
            const.dir_temp  = c(const.dir1, const.dir2, const.dir3_temp)
            
            lp_min  = lp(dir1, obj.coeff, const.mat, const.dir_temp, const.rhs)
            lp_max  = lp(dir2, obj.coeff, const.mat, const.dir_temp, const.rhs)
            
            vec     = c(loc_vec[1], loc_vec[2], lp_min$objval, lp_max$objval)
            mat     = rbind(mat, vec)
            
            #print(vec)
            
            
            if((is.na(obj_min)) || (lp_min$objval < obj_min)){
                obj_min = lp_min$objval
            } 
            
            
            if((is.na(obj_max)) || (lp_max$objval > obj_max)){
                obj_max = lp_max$objval
            }
            
            
            
        }
        
        if(i %% 5 == 0){
            write.csv(mat, "mat_new_greedy.csv")
        }
        
    }
    
    #print(c(obj_min, obj_max))
    
    myresult = list(greedy_process = mat, optimal_result = c(obj_min, obj_max))
    
    return(myresult)
    
}

##############  Quantities:
# new_mat: record the greedy search result 
# max_loc1 & 2 : in maxLP problem, which location gives the maximum 
# max_pars     : in maxLP problem, which reparameter gives the maiximum
# min_loc1 & 2 : in minLP problem, which location gives the minimum
# min_pars     : in minLP problem, which reparameter gives the minimum




# for N = 100, the maximum of maximum via greedy search falls at (39, 41), (38, 40),
# which in my_grid, it is in the neighborhood of (94.05, 99.0099),
# in our previous greedy search for N = 1000, the most recent result is in the neighbood of (99.6, 99.8)
# so I should try somewhere near (94 - 100, 96 - 101)

new_mat  = read.csv("mat_new_greedy.csv", header = T)

max_loc1 = new_mat$V1[which.max(new_mat$V4)]
max_loc2 = new_mat$V2[which.max(new_mat$V4)]

max_pars = loc2pars(c(max_loc1, max_loc2), x_grid = my_grid)

min_loc1 = new_mat$V1[which.min(new_mat$V3)]
min_loc2 = new_mat$V2[which.min(new_mat$V3)]

min_pars = loc2pars(c(min_loc1, min_loc2), x_grid = my_grid)


# TODO: do not change anything below before finishing writing on mat.csv 

# I have not change the greed alg yet, because I am waiting for its result and try to record it into excel file.

solveLP_greedy = function(
    inner_knots_num = N,
    obs_data  = y,
 
    interest = 100
){
    dir1   = "min"; dir2 = "max"
    
    # these are for updating the criteria bands
    criteria_lower = ecdf_ks_ci(sort(obs_data))$lower     # the lower bound of current KS bands
    criteria_upper = ecdf_ks_ci(sort(obs_data))$upper     # the upper bound of current KS bands
    if(!is.na(interest)){
        obj.coeff = get_density_obj_coeff(inner_knots_num, obs_data, interest)
    }
    
    const.mat = get_const_mat(inner_knots_num)
    const.rhs = get_const_rhs(inner_knots_num, obs_data)
    
    # the location of reflection points will only influence the const_dir
    const.dir1 = get_valid_const_dir(inner_knots_num)
    const.dir2 = get_criteria_const_dir(inner_knots_num)
    
    obj_min = NA
    obj_max = NA
    mat     =  NULL  # matrix recording the (inflection1, inflection2, lp_min$val, lp_max$val)
    
    for(i in 1:(inner_knots_num + 2 - 5)){
        for(j in (i+2):(inner_knots_num + 2 - 3)){
            pair = c(i, j)
            const.dir3_temp = get_unimodality_const_dir(inner_knots_num, pair[1], pair[2])
 
            const.dir_temp  = c(const.dir1, const.dir2, const.dir3_temp)
            
            lp_min = lp(dir1, obj.coeff, const.mat, const.dir_temp, const.rhs)
            lp_max = lp(dir2, obj.coeff, const.mat, const.dir_temp, const.rhs)
            
            vec    = c(pair[1], pair[2], lp_min$objval, lp_max$objval)
            mat    = rbind(mat, vec)
            
           # print(vec)
            
            
            if((is.na(obj_min)) || (lp_min$objval < obj_min)){
                obj_min = lp_min$objval
            } 
            
            
            if((is.na(obj_max)) || (lp_max$objval > obj_max)){
                obj_max= lp_max$objval
            }
            
            
            
        }
        
        if(i %% 5 == 0){
            write.csv(mat, "mat.csv")
        }
        
    }
    
    #print(c(obj_min, obj_max))
    
    myresult = list(greedy_process = mat, optimal_result = c(obj_min, obj_max))
    
    return(myresult)
   
}
