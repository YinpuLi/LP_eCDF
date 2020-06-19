source('initial_inflections.R')

#################################################################
#################  Constraint 3 : Unimodality ###################
#################################################################

#########  Functions:
# get_unimordality_const_mat    : constraint matrix, of size        (N - 1) by (N + 2)
# get_unimordality_const_dir    : direcition, vector of length      (N - 1)
# get_unimordality_const_rhs    : right hand side, vector of length (N - 1)




get_unimodality_const_mat = function(
    inner_knots_num       = N
){
    num                   = inner_knots_num + 2
    mat = matrix(rep(0, (num-3)*num), num - 3, num)
    for(i in 1:(num-3)){
        mat[i,i:(i+3)]    = c(-1, 3, -3, 1)
    }
    return(mat)
}
get_unimodality_const_rhs = function(
    inner_knots_num       = N
){
    return(c(rep(0, inner_knots_num - 1)))
}

# we start from the fix inflection points:
get_unimodality_const_dir = function(
    inner_knots_num       = N,
    x_grid                = my_grid,
    inflec_pars           = init_par_vec          # vector of 2 elements, the reparameterized inflection parameters
){
    
    # get the index/loction of inflection points
    loc_vec               = pars2loc(par_vec = inflec_pars, x_grid)
    loc1                  = loc_vec[1]
    loc2                  = loc_vec[2]
    
    #print(paste0("loc_vec =", loc_vec))
    
    
    V = c(
        rep(">=", loc1),
        rep("<=", loc2 - loc1),
        rep(">=", inner_knots_num - loc2 - 1)
    )
    return(V)
}

init_unimodality_mat = get_unimodality_const_mat()
init_unimodality_dir = get_unimodality_const_dir()
init_unimodality_rhs = get_unimodality_const_rhs()