unique(fsh_bio_noOL$year)

fuck<-fsh_bio_noOL %>% 
  filter(age %in% aa) %>% 
  ungroup() %>% 
  select(Sex, year) %>% 
  filter(Sex %in% c("Female", "Male")) %>% 
  na.omit() %>% 
  droplevels() %>% 
  count(Sex, year) %>% 
  group_by(year) %>% 
  mutate(proportion = round(n / sum(n), 2),
         Source = "LL fishery") %>% 
  filter(Sex == "Female")

unique(fuck$year)

unique(recoveries$tag_batch_no)
with(recoveries, table(tag_batch_no,year))

#=====================================================================
# old school diagnostics
#1) check lengths with KS tests
D.func<-function(x,y){  #x<-C; y<-M
  n.x <- length(x)
  n.y <- length(y)
  n <- n.x * n.y/(n.x + n.y)
  w <- c(x, y)
  z <- cumsum(ifelse(order(w) <= n.x, 1/n.x, -1/n.y))
  max.at <- sort(w)[which(abs(z) == max(abs(z)))]
  return(max.at)
  
}
KS.func<-function(M,C,R){
  print(ks.test(M,R))
  print(D.func(M,R))
  print(ks.test(C,R))
  print(D.func(C,R))
  print(ks.test(M,C))
  print(D.func(M,C))
}

rel20<-releases[releases$year == 2020,]
str(rel20); nrow(releases[is.na(rel20$tag_no),])
M<-rel20$length[!is.na(rel20$length)]

rec20<-recoveries[recoveries$year == 2020,]
nrow(rec20[is.na(rec20$tag_no),])
recaps<-rec20$length[!is.na(rec20$length)]

cees<-read.csv(paste0("data/fishery/fishery_bio_2000_", YEAR,".csv"))
str(cees)
cees20<-cees[cees$year == 2020,]
C<-cees20$length[!is.na(cees20$length)]

Ccdf<-ecdf(C); Mcdf<-ecdf(M); Rcdf<-ecdf(recaps)
plot(Ccdf, verticals=TRUE, do.points=FALSE)
plot(Mcdf, verticals=TRUE, do.points=FALSE, add=TRUE, col="blue")
plot(Rcdf, verticals=TRUE, do.points=FALSE, add=TRUE, col="forestgreen")

ks.test(M,recaps); summary(ks.test(M,recaps)); D.func(M,recaps)
ks.test(M,C); summary(ks.test(M,C)); D.func(M,C)
ks.test(C,recaps); summary(ks.test(C,recaps)); D.func(C,recaps)

min(C)
min(M)
min(recaps)
Clte61<-C[C<=61]; Mlte61<-M[M<=61]; Rlte61<-recaps[recaps<=61]

KS.func(Mlte61,Clte61,Rlte61)

Cgt61<-C[C>61]; Mgt61<-M[M>61]; Rgt61<-recaps[recaps>61]

KS.func(Mgt61,Cgt61,Rgt61)

#** results show unequal cap prob in 1st event but not in second once stratified around 61... 
#** should evaluate how stratification changes abundance!!! 
#** not sure thats practical because unmarked second event fish are not all measured
#** would need to schwag out numbers based on length distributions... acceptable... 

#2) check mixing and geographic problems with chi-square tests
# make mixing table
# make st event table: two rows of marked (m2) and unmarked (n2-m2) for each geographic strate
# make 2nd event table with rows of recaptured (m2) - not recaptured (n1-m2) for each area (stat area


#=====================================================================
# Jane's MR jagsfunction
j=1
# Base strata on percentiles of cumulative catch. Could also use number of marks
# observed or some other variable. STRATA_NUM is the dynamic variable specifying
# the number of time strata to split the fishery into and it currently
# accomodates 8 or fewer strata.
STRATA_NUM <- j

daily_marks %>% 
  group_by(year) %>% 
  mutate(catch_strata = cut(percent_rank(cum_whole_kg) %>% round(2),
                            breaks = seq(0, 1, by = 1 / STRATA_NUM),
                            include.lowest = TRUE, right = TRUE, ordered_result = TRUE,
                            labels = paste(seq(2, STRATA_NUM + 1, by = 1)))) -> daily_marks

# Summarize by strata
#PJ note 2022: calculating C (number of fish examined for marks) from biomass of harvest? 
daily_marks %>% #filter(year == YEAR) %>% 
  group_by(year, catch_strata) %>% 
  dplyr::summarize(t = n_distinct(date),
                   catch_kg = sum(whole_kg) %>% round(1),
                   n = sum(total_obs),
                   k = sum(total_marked),
                   D = sum(fishery_D),
                   NPUE = mean(mean_npue, na.rm = TRUE) %>% round(1),
                   # Use the interpolated mean weight here so it doesn't break at large
                   # STRATA_NUMs
                   mean_weight = mean(interp_mean, na.rm = TRUE)) %>% 
  mutate(C = (catch_kg / mean_weight) %>% round(0)) -> strata_sum

# Running Chapman estimator
strata_sum %>% 
  left_join(tag_summary %>% select(year, K.0), by = "year") %>% 
  # cumulative marked and observed
  group_by(year) %>% 
  mutate(cumn = cumsum(n),
         cumk = cumsum(k),
         cumd = cumsum(D),
         K_running = K.0 - cumd) %>% 
  # This deducts period specific D's
  mutate(running_Chapman = ((K_running + 1)*(cumn + 1) / (cumk + 1)) - 1,
         Chap_var = ((K_running + 1)*(cumn + 1)*(K_running - cumk)*(cumn - cumk)) / ((cumk + 1)*(cumk + 1)*(cumk + 2)) ) -> strata_sum
#view(strata_sum)
# Prep data for JAGS ----  

# Here we're pivoting each of the catch strata into their own columns, e.g. D is now D.2 and D.3, for example.
dcast(setDT(strata_sum), year ~ catch_strata, #, fun.aggregate = function(x) sum(!is.na(x)), # sum acts like identity
      value.var = c("D", "C", "n", "t", "k", "NPUE"), sep = ".") %>%  
  left_join(tag_summary, by = "year") %>% 
  # order the columns alphanumerically - very convenient
  select(year, order(colnames(.))) -> jags_dat

jags_dat %>%
  select(year, K.0, contains("D."), contains("C."), starts_with("n."), 
         starts_with("t."), contains("k."), contains("NPUE.")) -> jags_dat

# Add in the abundance estimates from the past assessments as mu.N (the mean for
# the prior on N.1)
assessment_summary %>% 
  filter(year >= FIRST_YEAR & !year %in% NO_MARK_SRV) %>% 
  select(year, mu.N = abundance_age2plus) -> abd_est

left_join(jags_dat, abd_est, by = "year") -> jags_dat

# Length of unique years to run model
model_years <- unique(jags_dat$year)

# Create an empty list to store JAGS data (we want a list of lists)
model_dat <- vector('list', length(model_years))

# Reshape data into lists of each variable. Flexible by number of time strata.
# Will look for a more efficient way to do this next year - maybe start here:
# https://stackoverflow.com/questions/31561238/lapply-function-loops-on-list-of-lists-r

for(i in 1:length(model_years)){
  #i<-10
  sub <- jags_dat %>% filter(year == model_years[i])
  
  sub_dat <-
    list(P = STRATA_NUM + 1, # number of time Periods
         M = 0.1 / 365, # Daily natural mortality
         mu.N = sub$mu.N, # mean of starting abundance values, from past assessments
         K.0 = sub$K.0,
         D.0 = sub$D.0,
         C = select(sub, contains("C.")) %>% as.numeric() , 
         D = select(sub, contains("D."), -D.0) %>% as.numeric(),
         t = select(sub, contains("t.")) %>% as.numeric(),
         n = select(sub, contains("n.")) %>% as.numeric(),
         k = select(sub, contains("k."), -K.0) %>% as.numeric(),
         NPUE = select(sub, contains("NPUE.")) %>% as.numeric()
    )
  
  model_dat[[i]] <- sub_dat
  rm(sub_dat)
  
}

mr_jags <- function(
  mod, # model formula (as a character string)
  mod_name, # what you want to name your model, keep them straight during model selection
  model_dat, # list of lists of model data 
  model_years, # vector of years over which you want to run the model
  mpar, # parameters of interest, sample poterior distribution of 'mpar' variables
  mchains = 4, # number of desired chains
  madapt = 2000, # burn in
  miter = 50000, # number of iterations to run each chain
  mthin = 10 # thinning rate
) {
  
  
  mod1_out <- mr_jags(mod = mod1, mod_name = "Model1", 
                      model_dat = model_dat, model_years = model_years, 
                      mpar = c("N.avg", "N", "p"),
                      miter = 50000)
  
  
  # # Create an empty list to store model output 
  # model_output <- vector('list', 1)
  
  for(i in 1:length(model_years)){  #for each stratum opt run each year
    i<-1
    dat <- model_dat[[i]]
    
    cat(mod, file = paste0(mod_name, "_", model_years[i], ".jag"))  #recording for output
    
    # initialize and run model
    mod_init <- jags.model(paste0(mod_name, "_", model_years[i], ".jag"),
                           data = dat,
                           n.chains = mchains,
                           # init = tst_inits,
                           n.adapt = madapt)
    
    # sample posterior
    res <- coda.samples(mod_init,
                        var = mpar,
                        n.iter = miter,
                        thin = mthin)
    
    # format output using user-defined fxn coda_df
    coda_df(res) %>% 
      mutate(year = model_years[i],
             model = mod_name) -> coda_res
    
    #Append results 
    if(i == 1){
      coda_res_out <- coda_res
      rm(coda_res)
    } else {
      coda_res_out <- rbind(coda_res_out, coda_res) }
    
    # Diagnostic trace plots - These MR models were tested individually by year.
    # They are well mixed and have no issues with convergence.
    
     plot(res, col = 2)
     library(MCMCvis)
     MCMCtrace(res, params=c("N","N.avg","p"), ISB=TRUE, pdf=FALSE, Rhat=TRUE)
     traceplot(res, parameters=c("N","N.avg","p"), Rhat_min=1.5, layout=c(2,2), ask=FALSE)
    # Get DIC for model selection
    # https://www4.stat.ncsu.edu/~reich/st590/code/DICpois
    
    dic <- dic.samples(mod_init,
                       var = mpar,
                       n.iter = miter,
                       thin = mthin)
    
    dic <- data.frame(model = mod_name,
                      year = model_years[i],
                      deviance = sum(dic$deviance), # over all fit (smaller deviance better)
                      parameter_penalty = sum(dic$penalty), # number of parameters
                      DIC = sum(dic$deviance) + sum(dic$penalty)) # penalized deviance (aka DIC), smaller is better
    
    # Append results 
    if(i == 1){
      dic_out <- dic
      rm(dic)
    } else {
      dic_out <- rbind(dic_out, dic) }
    
    # Convergance diagnostic: gelman.diag gives you the scale reduction factors for
    # each parameter. A factor of 1 means that between variance and within chain
    # variance are equal, larger values mean that there is still a notable
    # difference between chains. General rule: everything below 1.1 or so
    # is ok.
    # https://theoreticalecology.wordpress.com/2011/12/09/mcmc-chain-analysis-and-convergence-diagnostics-with-coda-in-r/
    gelman.diag(res, multivariate = FALSE)[[1]] %>%
      data.frame() %>% 
      mutate(year = model_years[i],
             model = mod_name) %>% 
      distinct() -> convergence
    
    # Append results 
    if(i == 1){
      convergence_out <- convergence
      rm(convergence)
    } else {
      convergence_out <- rbind(convergence_out, convergence) }
    
  } 
  
  # Summarize the abundance estimate for quick model comparison
  coda_res_out %>% 
    group_by(year) %>% 
    mutate(N.avg = N.avg / 1000000) %>% 
    dplyr::summarize(mean = mean(N.avg),
                     q025 = quantile(N.avg, 0.025),
                     q975 = quantile(N.avg, 0.975)) %>% 
    mutate(model = mod_name) -> n_summary_out
  
  model_output <- list("results" = coda_res_out,
                       "N_summary" = n_summary_out,
                       "dic" = dic_out,
                       "convergence_diagnostic" = convergence_out)
  
  return(model_output)
  
}

#=========================================================
