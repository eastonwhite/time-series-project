

# Created by Easton White
# Created on 17-Jan-2017
# Last edited on 11-May-2017

# Calculate p value for GAM model

#SHould I use gaussian or poisson family? Poisson does not work when I have non-count data
require(mgcv)
calculate_p_value_gam = function(pop){
  smoothing_parameter=3
  year=1:length(pop)
  return(summary(gam(pop ~ s(year,k=smoothing_parameter),family=gaussian))$s.table[4])
}

