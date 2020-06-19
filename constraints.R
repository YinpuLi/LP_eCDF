source('con1_valid_cdf.R')
source('con2_pass_KS_criteria.R')
source('con3_unimodal.R')


##############################################################
################ Combinig all constraints ####################
##############################################################



#########  Functions:
# get_const_mat    : constraint matrix, of size        (4N + 4) by (N + 2)
# get_const_dir    : direcition, vector of length      (4N + 4)
# get_const_rhs    : right hand side, vector of length (4N + 4)



get_const_mat       = function(
    inner_knots_num = N
){
    mat1            = get_valid_const_mat(inner_knots_num)
    mat2            = get_criteria_const_mat(inner_knots_num)
    mat3            = get_unimodality_const_mat(inner_knots_num)
    
    return(rbind(mat1, mat2, mat3))
}


get_const_dir       = function(
    inner_knots_num = N,
    x_grid          = my_grid,
    inflec_pars     = init_par_vec
){
    const.dir1      = get_valid_const_dir(inner_knots_num) 
    const.dir2      = get_criteria_const_dir(inner_knots_num) 
    const.dir3      = get_unimodality_const_dir(inner_knots_num, x_grid, inflec_pars)
    
    return(c(const.dir1, const.dir2, const.dir3))
}


get_const_rhs       = function(
    inner_knots_num = N
){

    const.rhs1      = get_valid_const_rhs(inner_knots_num)
    const.rhs2      = get_criteria_const_rhs(inner_knots_num)
    const.rhs3      = get_unimodality_const_rhs(inner_knots_num)
    
    const.rhs  = c(const.rhs1, const.rhs2, const.rhs3)
    
    return(const.rhs)
}


init_mat = get_const_mat()
init_dir = get_const_dir()
init_rhs = get_const_dir()


