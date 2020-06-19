source('set_grid.R')


# There is no need to start from the inflection pair that best match with the empirical distribution.
# The best match probably won't give ugly/extreme range for the quantity of interest.
# So, it really depends on the results of our experiment to see which pair(neighborhood) is giving extreme range.
# For now, I found the inflection pair (99.7, 99.8) is giving ugly result, so I will use this neighborhood.

# This file now focuses on reparameterization.


# This file decides cons3_unimodal.R
# with the most annoying function get_unimodality_const_dir()
# the possible out-of-bound issue would be taken care of in the functions in this file
# by pushing the search inside the grid range.

######### Functions:

# validate_loc     : validating the inflection loc/indx satisfying 1 <= loc1 < loc2 <= N - 1, loc1 and loc2 being 
# validate_val     : validating the inflection vals satisfying domain1 <= val1 < val2 <= domain2


# loc2pars         : reparameterization from location/indices of grid
# val2pars         : reparameterization from grid values
# pars2loc
# pars2val
# There are some constraints over loc2:
 # 1. N - loc2 - 1 >= 0, we need loc2 <= N - 1 = length(x_grid). 
    # length(x_grid) = N + 2
    # Hence we cannot use tail(grid, 2), but c( x_grid[length(x_grid) - 4], x_grid[length(x_grid) - 3] )
 # 2. N - loc2 - 1 >= 0, we need loc2 <= N - 1
# in summary:
    # 1 <= loc1 < loc2 <= N - 1

swap                = function(
    vec             # a vector of 2 elements, to be swaped
){
    temp            = vec[1]
    vec[1]          = vec[2]
    vec[2]          = temp
    
    return(vec)
}


validate_loc        = function(
    loc_vec,
    inner_knots_num = N
){
    
    if(  ! installr::: check.integer(loc_vec[1])){
        round(loc_vec[1], 0)
    }
    if(  ! installr::: check.integer(loc_vec[2])){
        round(loc_vec[2], 0)
    }
    if(loc_vec[1]   > loc_vec[2] ){
        loc_vec     = swap(loc_vec)
    } 
    
    # loc1 could equal to loc2
    
    if(loc_vec[2]  <= 1){
        # then loc_vec[1] must <= 1 as well
        loc_vec[1]  = 1
        loc_vec[2]  = 2
    } else if(loc_vec[1] < 1){
        loc_vec[1]  = 1
    }
    
    if(loc_vec[1]  >= N - 2){
        # then loc_vec[2] must > N - 1 as well 
        loc_vec[1]  = N - 2
        loc_vec[2]  = N - 1
    } else if(loc_vec[2] > N - 1){
        loc_vec[2]  = N - 1
    }
    
    
    return(loc_vec)
    
    
}

validate_val        = function(
    val_vec,
    domain_min      = domain1,
    domain_max      = domain2
){
    if(val_vec[1]   > val_vec[2]){
        val_vec     = swap(val_vec)
    } else if(
        val_vec[1] == val_vec[2] # though loc1 == loc2 is OK but val1 == val2 is not OK
    ){
        val_vec[2]  = val_vec[1] + 0.01
    }
    
    
    if(val_vec[2]  <= domain_min){
        # then val1 < domain_min as well
        val_vec[1]  = domain_min
        val_vec[2]  = domain_min + 0.01
    } else if(
        val_vec[1]  < domain_min
    ){
        val_vec[1]  = domain_min
    }
    if(val_vec[1]  >= domain_max){
        # then val2 > domain_max as well
        val_vec[2]  = domain_max
        val_vec[1]  = domain_max - 0.01
    } else if(
        val_vec[2]  > domain_max
    ){
        val_vec[2]  = domain_max
    }
    
    return(val_vec)
    
}



val2pars            = function(
    val_vec,                             # inflection values
    domain_min      = domain1,
    domain_max      = domain2
){
    # getting the valid result of val_vec
    val_valid       = validate_val(val_vec, domain_min, domain_max)
    
    par1            = val_valid[1]
    par2            = log(val_vec[2] - val_vec[1])
    
    return(c(par1, par2))
    
}



loc2pars            = function(
    loc_vec,                            # grid indices, needs to be a vector of 2 integers, ele1 < ele2
    x_grid          = my_grid,
    domain_min      = domain1,
    domain_max      = domain2,
    inner_knots_num = N          
){
    # getting the valid result of loc_vec:
    
    valid_loc       = validate_loc(loc_vec, inner_knots_num)
    
    val1            = x_grid[valid_loc[1]]
    val2            = x_grid[valid_loc[2]]
    
    return(val2pars(c(val1, val2), domain_min, domain_max))
    
    
   
}

pars2val            = function(
    par_vec,                             # reparameterized parameter vector
    domain_min      = domain1,
    domain_max      = domain2
){
    
    val1            = par_vec[1]
    val2            = val1 + exp(par_vec[2])
    
    valid_val       = validate_val(c(val1, val2), domain_min, domain_max)
    
    
    return(valid_val)
}



pars2loc            = function(
    par_vec         = init_par_vec,
    domain_min      = domain1,
    domain_max      = domain2,
    x_grid          = my_grid,
    inner_knots_num = N
){
    val_vec         = pars2val(par_vec, domain_min, domain_max)
    # (val1, val2)
    loc1            = which(x_grid >= val_vec[1])[1] 
    loc2            = which(x_grid >= val_vec[2])[1] 
    
    valid_loc       = validate_loc(c(loc1, loc2), inner_knots_num)
    
    return(valid_loc)
}

init_loc_vec = c(41, 43)
init_par_vec = loc2pars(init_loc_vec, my_grid)
init_val_vec = pars2val(init_par_vec, my_grid)

 
# inflection1 = init_inflec()[1]
# inflection2 = init_inflec()[2]
# 
# inflect_loc = c(inflection1, inflection2)
# 
# 
# 
# inflection_val1 = init_inflec_val()[1]
# inflection_val2 = init_inflec_val()[2]
# 
# inflect_val = c(inflection_val1, inflection_val2)
# 
# par1 = inflection_val1
# par2 = log(inflection_val2 - inflection_val1)
# 
# pars = c(par1, par2)
# 
# new_pars = c(loc2pars(loc_vec = c(15, 17)))
# 
# 



#############################################################
########### Initialize the two inflection points ############
#############################################################


# init_inflec      : initializing the two inflection points, returning the index in grid
# init_inflec_val  : initializing the two inflection points, returning the exact values

# init_inflec         = function(
#     inner_knots_num = N, 
#     obs_data        = y
# ){
#     new_ecdf        = update_eCDF(inner_knots_num, obs_data)
#     
#     inflec.obj      = ede(new_ecdf$grid, new_ecdf$eCDF, index = 0) # the cdf is convex/concave, index = 0; otherwise 1
#     
#     return(c(inflec.obj[1], inflec.obj[2]))
# }


# init_inflec_val     = function(
#     inner_knots_num = N, 
#     obs_data        = y
# ){
#     inflec_indx     = init_inflec(inner_knots_num, obs_data)
#     cur_grid        = set_up_grid(inner_knots_num, obs_data)
#     
#     indx1           = inflec_indx[1]
#     indx2           = inflec_indx[2]
#     
#     val1            = cur_grid[indx1]
#     val2            = cur_grid[indx2]
#     
#     return(c(val1, val2))
# }

######### Default Quantities:

# new_ecdf              : adaptive to the grid
# To initialize the inflection points, we need smoother CDF estimation,
# so we update the KS estimation based on the new grid,
# return            : grid knots, eCDF
# size              : num_grid_knots(N + 2) by 2
# inflection1&2         : the two inflection points' indices in sequence 
# inflec_loc            : vector c(inflection1, inflection2)
# inflection_val1 &2    : the two inflection points' values in sequence
# inflec_val            : vector c(inflection_val1, inflection_val2)

# par1&par2             : reparameterization to make sure inflection_val2 > inflection_val1
#			: inflection_val1 = par1, inflection_val2 = par1 + exp(par2)
# pars                  : vector c(par1, par2)




