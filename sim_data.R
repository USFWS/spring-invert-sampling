## spring invert survey simulation

# simulate population size and change through time

# from Diaz et al 2200 ind/m2 +/- 453, 630 m from headwaters
sim_data <- function(mean_lam, nsites, occ_prob, detect_prob){

  #2200/m2
  #12349 m2 = area sampled 
  pop_dyn <- rpois(1,mean_lam)

# simualte sampling process

# distribute pop_dyn across sampling area
# 41 samples
  sites <- seq(1,nsites)
  dat <- cbind(sites, rep(0, nsites))

# simulate presence/absence within a 10 cm sampling frame
# found in 17% of samples
  prob <- rbinom(1, nsites, occ_prob)
  pres <- sample(dat[,1], size = prob, replace = FALSE)

# simulate abundance when species is present
  # multiplying lambda by 0.01 because of size of snail pail
  abund <- rpois(n = (length(pres)), lambda = pop_dyn*.01)
  dat[pres,2] <- abund
  
# simulate detection probability  
  dat[,2] <- round(detect_prob * dat[,2])  
  dat
}
