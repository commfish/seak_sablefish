obj$report()$fsh_slx   #fishery selectiviy
obj$report()$retention   #not saved...
obj$report()$dmr  #not saved...

obj$report()$sel_Fxx #fully selected F mortality by sex, age and Fxx; 
  #values for fully selected ages = F-ABC in output... what we want!!! 
obj$report()$Z

obj$report()$N #numbers by year, age and sex
obj$report()$Z #total mortality by year, age and sex for Fxx=0.5 (presumably?)

retention #data model of retention pro
#retention and dmr
#discard mortality array - all values = 0.16
dmr = array(data = ifelse(include_discards == TRUE, 0.16, 0), dim = c(nyr, nage, nsex))

# Probability of retaining a fish, sex- and age-based
retention <- array(data = filter(retention, Sex %in% c("Female","Male")) %>%
                     mutate(sex = ifelse(Sex == "Male", 1, 2)) %>% 
                     arrange(sex) %>% pull(p), dim = c(1, nage, nsex))

retention = 
  # 100% retention (assuming no discards)
  if(include_discards == FALSE & nsex == 1) {
    array(data = 1,
          # Number of rows could = time blocks but currently doesn't
          dim = c(1, nage, nsex))
    # Discards, single sex model
  } else if (include_discards == TRUE & nsex == 1) {
    array(data = filter(retention, Sex == "Combined") %>% pull(p),
          dim = c(1, nage, nsex))
  } else { # Discards, sex-structured
    array(data = filter(retention, Sex %in% c("Female","Male")) %>%
            mutate(sex = ifelse(Sex == "Male", 1, 2)) %>% 
            arrange(sex) %>% pull(p), dim = c(1, nage, nsex))
  }

#Survey weight at age
data_srv_waa = 
  if (nsex == 1) { # Single sex model
    filter(waa, Source == "LL survey" & Sex == "Combined") %>% 
      pull(round_kg) %>%  matrix(ncol = nage, nrow = nsex)  %>% 
      array(dim = c(1, nage, nsex))} else {
        # Sex-structured - males in first matrix, females in second
        filter(waa, Source == "LL survey" & Sex %in% c("Male", "Female")) %>% 
          arrange(desc(Sex)) %>% 
          pull(round_kg) %>% 
          matrix(ncol = nage, nrow = nsex)  %>% 
          array(dim = c(1, nage, nsex))}


F_fr_ABC <-function(B){x*(1-((exp(-(0.1+x)))/(x+0.1)))-B}
F_fr_ABC <-function(B){mean((x*(1-((exp(-(0.1+x)))/(x+0.1)))-B)^2)}
optimize(F_fr_ABC,interval=c(-10,10))

x<-seq(0.02,0.1,by=0.001)
B<-x*(1-((exp(-(0.1+x)))/(x+0.1)))
nlm(F_fr_ABC,p=1)

#Function from Jane's code:
N <- sum(N[nyr+1,,1]) + sum(N[nyr+1,,2]) # sum of projected abundance across age and sex

nat_mort <- exp(parameters$log_M)
catch <- recABC

# Search sequence of F values to obtain recommended F_ABC based on 15% constant
# change MP
fish_mort <- seq(0.03, 1.6, .000001)

catch_to_F <- function(fish_mort, N, nat_mort, catch, F_to_catch) {
  
  F_to_catch <- function(N = n, nat_mort, fish_mort){
    Z <- fish_mort + nat_mort
    N * (1 - exp(-Z)) * fish_mort / Z   #calc of ABC also should have waa and retention probability? 
                                        #that has to be the problem
     #?? use average weight?  
     #or loop through calculations as in TMB code to recreate it here? 
    #what<-N * (1 - exp(-Z[1])) * fish_mort[1] / Z[1]
    #fish_mort[1]
  }
  
  F_ABC <- catch - F_to_catch(N, nat_mort, fish_mort)  #so this is subtracting N for catch in biomass... 
      #so... catch needs to be changed to weight, of F_to_catch needs to be in biomass
  return(F_ABC)
  
  #F_ABC_x<-catch-F_to_catch(N, nat_mort, fish_mort[1])
}

# F under recommended ABC
(F_ABC <- uniroot(catch_to_F, interval = c(0.03, 1.6), N = N, catch = recABC, nat_mort = nat_mort, F_to_catch = F_to_catch)$root*0.5)

(F_ABCtest <- uniroot(catch_to_F, interval = c(0.03, 1.6), N = N, catch = maxABC, nat_mort = nat_mort, F_to_catch = F_to_catch)$root*0.5)
#PJ22: this function is producing different F_ABC than that coming out of TMB code!!!

(F_ABCtest <- uniroot(catch_to_F, interval = c(0.03, 1.6), N = N, catch = maxABC, nat_mort = nat_mort, F_to_catch = F_to_catch)$root*0.5)

#=======================================================================================
# data_srv_waa array of two "matrices" which are actually vectors...
waa.m<-data_srv_waa[,,1]  #males
waa.f<-data_srv_waa[,,2]  #females
#also need abundance by sex and age
N <- sum(N[nyr+1,,1]) + sum(N[nyr+1,,2])

N <- obj$report()$N  #str(obj$report()) 
N.m <- N[nyr+1,,1]
N.f<- N[nyr+1,,2]

length(waa.m)
length(N.m)

biom.m<-sum(waa.m*N.m)
biom.f<-sum(waa.f*N.f)

tots<-(biom.m+biom.f)* 2.20462   #comes in ~9% higher than biomass from model output below...

catch_to_F2 <- function(fish_mort, N, nat_mort, catch, F_to_catch) {
  
  F_to_catch <- function(N = n, nat_mort, fish_mort){
    
    Z <- fish_mort + nat_mort
    #LOOP through age and sex here (Z should stay constant for sex and age, maybe)
    #sum across age and sex to produce biomass estimate for each fish_mort
      #need waa matrix; might not need to loop, just select appropriate columns rows
    #!!! Try replacing N with biomass estimate!?!?! 
    
    #  proj_age2plus  ??
    N * (1 - exp(-Z)) * fish_mort / Z   #calc of ABC also should have waa and retention probability? 
    #that has to be the problem
    #?? use average weight?  
    #or loop through calculations as in TMB code to recreate it here? 
    #what<-N * (1 - exp(-Z[1])) * fish_mort[1] / Z[1]
    #fish_mort[1]
  }
  
  F_ABC <- catch - F_to_catch(N, nat_mort, fish_mort,i)  #so this is subtracting N for catch in biomass... 
  #so... catch needs to be changed to weight, of F_to_catch needs to be in biomass
  return(F_ABC)
  
  #F_ABC_x<-catch-F_to_catch(N, nat_mort, fish_mort[1])
}

(F_ABCtest <- uniroot(catch_to_F2, interval = c(0.03, 1.6), N = N, catch = maxABC, 
                      nat_mort = nat_mort, F_to_catch = F_to_catch, i=1)$root*0.5)

uniroot(F_ABC, interval = c(0.03, 1.6), N = N, catch = maxABC, 
        nat_mort = nat_mort, F_to_catch = F_to_catch, i=1)$root*0.5


# Projected total age-2+ projected biomass
(proj_age2plus <- obj$report(best)$tot_biom[nyr+1] * 2.20462)

# Comparison with current age-2+ biomass
obj$report(best)$tot_biom[nyr] * 2.20462

# Projected total female spawning biomass
(proj_fSSB <- obj$report(best)$tot_spawn_biom[nyr+1] * 2.20462)

#==============================
# N changed to projected biomass... 
fish_mort <- seq(0.03, 1.6, .000001)
B<- obj$report(best)$tot_biom[nyr+1] * 2.20462
B<-tots

catch_to_Fbio <- function(fish_mort, B, nat_mort, catch, F_to_catchbio) {
  
  F_to_catchbio <- function(B = n, nat_mort, fish_mort){
    
    Z <- fish_mort + nat_mort
    #  proj_age2plus  ??
    B * (1 - exp(-Z)) * fish_mort / Z   #calc of ABC also should have waa and retention probability? 
  }
  
  F_ABC <- catch - F_to_catchbio(B, nat_mort, fish_mort)  #so this is subtracting N for catch in biomass... 
  return(F_ABC)
  
  #F_ABC_x<-catch-F_to_catch(N, nat_mort, fish_mort[1])
}

(F_ABCbio <- uniroot(catch_to_Fbio, interval = c(0.03, 1.6), B = B, catch = maxABC, 
                      nat_mort = nat_mort, F_to_catchbio = F_to_catchbio)$root*0.5)
(F_ABCbio <- uniroot(catch_to_Fbio, interval = c(0.01, 1.6), B = B, catch = recABC, 
                     nat_mort = nat_mort, F_to_catchbio = F_to_catchbio)$root*0.5)
#well that doesn't work... 

