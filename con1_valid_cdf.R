source('KS_criteria.R')

#################################################################
###########  Constraint 1 : non-decreasing valid CDF ############
#################################################################

# The KS bands are step functions, so we need to update the bands adaptive to the new grid:
# that is, a new lower and upper bound of length same with the new grid.


########### Functions:
# get_valid__const_mat      : constraint coefficient matrix, of size         (N + 1) by (N + 2)
# get_valid_const_dir       : direction, vector of length                    (N + 1)
# get_valid_const_rhs       : right hand side coefficients, vector of length (N + 1)


get_valid_const_mat = function(
    inner_knots_num = N
){
    num             = inner_knots_num
    mat             = matrix(rep(0, (num + 1) * (num + 2)),
                             nrow = num + 1,
                             ncol = num + 2)
    
    for(i in 1:(num + 1)){
        mat[i, i] = 1
        mat[i, i + 1] = -1
    }
    
    
    
    return(mat)
}


get_valid_const_dir = function(
    inner_knots_num = N
){
    return(c(rep("<=", inner_knots_num + 1)))
}

get_valid_const_rhs = function(
    inner_knots_num = N
){
    return(c(rep(0, inner_knots_num + 1)))
}




init_valid_mat = get_valid_const_mat()
init_valid_dir = get_valid_const_dir()
init_valid_rhs = get_valid_const_rhs()


