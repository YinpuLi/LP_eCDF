source('KS_criteria.R')

#################################################################
###########  Constraint 2 : falling with the  KS bands ###########
#################################################################

########### Functions:


# get_criteria_const_mat : constraint matrix, of size         2*(N+2) by (N+2)
# get_criteria_const_dir : direction, vector of length        2*(N+2)
# get_criteria_const_rhs : right hand side, vector of length  2*(N+2)





get_criteria_const_mat = function(
    inner_knots_num    = N
){
    num                = inner_knots_num + 2
    mat2 = diag(num)
    mat3 = diag(num)
    
    return(rbind(mat2, mat3))
}

get_criteria_const_dir = function(
    inner_knots_num    = N
){
    num                = inner_knots_num + 2
    const.dir2 = c(rep(">=", num))
    const.dir3 = c(rep("<=", num))
    
    const.dir = c(const.dir2, const.dir3)
    
    return(const.dir)
}

get_criteria_const_rhs = function(
    inner_knots_num    = N,
    obs_data           = y,
    domain_min         = domain1,
    domain_max         = domain2
){
     
    
    const.rhs.ci       = update_criteria(inner_knots_num, obs_data, domain_min, domain_max) 
                                         
    const.rhs    = c(const.rhs.ci$lower, const.rhs.ci$upper)
    return(const.rhs)
}



init_criteria_mat = get_criteria_const_mat()
init_criteria_dir = get_criteria_const_dir()
init_criteria_rhs = get_criteria_const_rhs()






