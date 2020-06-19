source('time_data.R')
source('packages_in_use.R')
source('params.R')


#################################################################
#############  Set up grid: functions and quantites #############
#################################################################



############# Functions 
# set_up_grid:

    # @ inner_knots_num: knots number, except the two ends(domain min and max)
    # @ obs_data : observed data, used to get the min/max domain
    
    # returns    : a sequence of grid knots of length N + 2, including the two ends



# get the new eCDF corresponding to all elements in the x-grid
set_up_grid         = function(
    inner_knots_num = N,          # by default, we want N = 1000 knots on x in the grid
    domain_min      = domain1,
    domain_max      = domain2
    
){
    

    return(seq(domain_min, domain_max, length.out = inner_knots_num + 2))
}



############# Quantities
# my_grid       : default grid of length 1002       

my_grid = set_up_grid(N, domain1, domain2)









