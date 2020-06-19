






############# Functions
# get_seg_num:   number of segmenst in the grid, given the number of knots

get_seg_num = function(N){
    return(N + 1)
}


############# Quantities

# N          : default number of knots used to partition the domain, except the two ends(min(y), max(y)) 
# seg_num    : default number of segments

N       = 1000
seg_num = get_seg_num(N)
 