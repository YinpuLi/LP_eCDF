source('set_grid.R')


#################################################################
#############  KS Criteria functions and quantites ##############
#################################################################

############# Functions 
# ecdf_ks_ci(data)  :  returns(sorted data, eCDF, lower bound, upper bound)
# update_eCDF       :  get the new eCDF corresponding to all elements in the x-grid(same length wid grid)

# The KS bands are step functions, so we need to update the bands adaptive to the new grid:
    # that is, a new lower and upper bound of length same with the new grid.
# update_criteria        : get the new KS bands corresponding to all elements in the x-grid    


ecdf_ks_ci <- function(x){
    n <- length(x)
    ec <- ecdf(sort(x))
    xx <- get("x", envir = environment(ec))
    yy <- get("y", envir = environment(ec))
    D <- sfsmisc::KSd(n)
    yyu <- pmin(yy + D, 1)
    yyl <- pmax(yy - D, 0)
    ecu <- stepfun(xx, c(yyu, 1))
    ecl <- stepfun(xx, c(yyl, yyl[n]))
    fun.ecdf <- ecdf(y)
    my.ecdf <- fun.ecdf(sort(y))
    
    return(list(x=x, est = my.ecdf, upper=ecu(x),lower=ecl(x)))
}


update_eCDF         = function(
    # these are for set_up_grid
    inner_knots_num = N,                # by default, we want N + 2 = 1002 knots on x in the grid
    obs_data        = y,
    domain_min      = domain1,             
    domain_max      = domain2
){
    # get the current eCDF
    eCDF            = ecdf_ks_ci(sort(obs_data))$est
    
    # set up the grid and return a seq of grid knots, including domain ends, of length N + 2
    grid            = set_up_grid(inner_knots_num, domain_min, domain_max)
    
    new_eCDF        = NULL
    for(i in 1:length(grid)){
        cur_knot    = grid[i]
        cur_index   = which(sort(obs_data) > cur_knot)[1]
        
        cur_ecdf    = eCDF[cur_index]
        
        cur_ecdf    = ifelse(is.na(cur_ecdf), tail(new_eCDF, 1), cur_ecdf)
        
        new_eCDF    = c(new_eCDF, cur_ecdf)
        
    }
    updated.dat.ecdf  = data.frame(grid = grid, eCDF = new_eCDF)
    return(updated.dat.ecdf)
    
}

update_criteria     = function(
    # these are for set_up_grid
    inner_knots_num = N,          # by default, we want N = 1000 knots on x in the grid
    obs_data        = y,          # the sample dat
    domain_min      = domain1,
    domain_max      = domain2
){
    grid            = set_up_grid(inner_knots_num, domain_min, domain_max)
    
    criteria_lower  = ecdf_ks_ci(sort(obs_data))$lower      # the lower bound of current KS bands
    criteria_upper  = ecdf_ks_ci(sort(obs_data))$upper      # the upper bound of current KS bands
    
    
    
    new_lower = NULL; new_upper = NULL
    
    for(i in 1:length(grid)){
        cur_knot  = grid[i]
        cur_index = which(sort(obs_data) > cur_knot)[1]
        cur_lower = criteria_lower[cur_index]
        cur_upper = criteria_upper[cur_index]
        cur_lower = ifelse(is.na(cur_lower), tail(new_lower,1), cur_lower)
        cur_upper = ifelse(is.na(cur_upper), 1, cur_upper)
        new_lower = c(new_lower, cur_lower)
        new_upper = c(new_upper, cur_upper)
        
    }
    updated.ci  = data.frame(lower = new_lower, upper = new_upper)
    return(updated.ci)
    
}



############# Quatities
# ecdf_y$x       : sorted data used to form the stepfunction
# ecdf_y$est     : estimated CDF
# ecdf_y$lower   : KS lower bound
# ecdf_y$upper   : KS upper bound

# or use the data frame:
# ECDF_data      : (Time, ecdf, KS_lower, KS_uppper)



ecdf_y = ecdf_ks_ci(sort(y))
ecdf_y$x
ecdf_y$est
ecdf_y$lower
ecdf_y$upper


# organize the data(Time, ecdf, CI_lower, CI_uppper)

ECDF_data = data.frame(Time     = ecdf_y$x, 
                       ecdf     = ecdf_y$est,
                       KS_Lower = ecdf_y$lower,
                       KS_Upper = ecdf_y$upper)

new_ecdf = update_eCDF()
new_ci   = update_criteria()
