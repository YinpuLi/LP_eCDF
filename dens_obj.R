source('set_grid.R')





#####################################################################################
###### coefficient vector of objective function for density estimation problem ######
#####################################################################################

get_neighbor_indx   = function(
    inner_knots_num = N,         # N+1 segment, N + 2knots in grid
    domain_min      = domain1,
    domain_max      = domain2,
    interest        = 100
){
    
        #a           = min(obs_data)    # unless specified the domain, use the minimum of data
   
        #b           = max(obs_data)    # unless specified the domain, use the maximum of data
        
    
    grid_cur        = set_up_grid(inner_knots_num, domain_min, domain_max)
    
    indx2           = which(grid_cur > interest)[1]
    
    if(is.na(indx2)){
        print("It is beyond the largest in the grid") # it is not accessible
        # then use the last two knots in the grid to estimate 
        indx1 = N + 1       # there are N+2 grid knots 
        indx2 = N + 2
    } else if(indx2 == 1){
        print("It is below the smallest in the grid") # it is not accessible
        # then use the first two knots in the grid to estimate
        indx1 = 1
        indx2 = 2
    } else{
        indx1 = indx2 - 1
    }
    return(c(indx1, indx2))
}



# this is for density estimation problem
get_density_obj_coeff = function(
    inner_knots_num   = N,         # N+1 segment, N + 2knots in grid
    interest          = 100,
    domain_min        = domain1,
    domain_max        = domain2
){
    
    # f(100) = (F(100 - delta) - F(100 - delta))/2delta
    #        = (F(x_large) - F(x_small)) / ((x_large - x_small)) 
    
    # (indx1, indx2) gives the smallest neighborhood for interest = 100
    # and hence the coefficients 
    # at this pair shoudl be (-1, 1)
    # and 0 elsewhere 
    grid_cur          = set_up_grid(inner_knots_num, domain_min, domain_max)
    neighbor_indx     = get_neighbor_indx(inner_knots_num, domain_min, domain_max, interest)
    
    indx1             = neighbor_indx[1]
    indx2             = neighbor_indx[2]
    
    delta             = (domain_max - domain_min) / (N + 1) # there are N+1 segments for N inner-knots
    
    obj.coeff        = c(rep(0, indx1 - 1), c(-1, 1), rep(0, N - indx2 + 2)) / delta
    
    return(obj.coeff)
    
}


init_obj_coeff = get_density_obj_coeff()
