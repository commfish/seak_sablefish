################################################################################
## Phil's Sablefish VAST stuuff
library(VAST)
library(splines)  # Used to include basis-splines
library(effects)  # 

sampling_data <- sable %>%    #could include sope, depth, soak as covariates...? 
  ungroup() %>% 
  distinct(set_cpue, year, Adfg, end_lat, end_lon) %>% 
  select(Catch_KG = set_cpue, Year = year, Vessel = Adfg, Lat = end_lat, Lon = end_lon) %>% 
  mutate(AreaSwept_km2 = 1) %>%   #areaswept = 1 because its longline (i.e., not trawl survey)
  as.data.frame()

catchability_data<- sable %>%    #could include sope, depth, soak as covariates...? 
  ungroup() %>% 
  distinct(set_cpue, year, Adfg, end_lat, end_lon, soak) %>% 
  select(Year = year, Lat = end_lat, Lon = end_lon, soak=soak) %>% 
  mutate(soak = na.approx(soak)) %>% #example.x$covariate_data[,'depth.f']<-na.approx(example.x$covariate_data[,'depth']);example.x$covariate_data[168:170,]
  #mutate(AreaSwept_km2 = 1) %>%   #areaswept = 1 because its longline (i.e., not trawl survey)
  as.data.frame()
  
covariate_data<-sable %>%    #could include sope, depth, soak as covariates...? 
  ungroup() %>% 
  distinct(set_cpue, year, Adfg, end_lat, end_lon, slope, depth) %>% 
  select(Year = year, Lat = end_lat, Lon = end_lon, slope = slope, depth = depth) %>% 
  mutate(depth = na.approx(depth)) %>% 
  #mutate(AreaSwept_km2 = 1) %>%   #areaswept = 1 because its longline (i.e., not trawl survey)
  as.data.frame()

nrow(sampling_data)
nrow(catchability_data)
nrow(covariate_data)

strata.limits <- data.frame(STRATA = "All_areas")

example <- list(sampling_data = sampling_data,
                Region = "other",
                strata.limits = strata.limits)

example.q<-list(sampling_data = sampling_data,
                catchability_data = catchability_data,
                Region = "other",
                strata.limits = strata.limits)

example.x<-list(sampling_data = sampling_data,
                covariate_data = covariate_data,
                Region = "other",
                strata.limits = strata.limits)

example.qx<-list(sampling_data = sampling_data,
                covariate_data = covariate_data,
                catchability_data = catchability_data,
                Region = "other",
                strata.limits = strata.limits)



settings = make_settings(n_x = 100, # number of knots; set at 100 by JANE
                         ObsModel = c(1, 3), #distribution for data and link-func for linear predictors; [1]=1=lognormal
                         # c("PosDist"=[Make Choice], "Link"=0); ObsMode[1]<-PDF
                         # ObsModel[2] set to 3 small # years with 100% encounter rate... 
                         #VAST checks species-years combos with 100%
                         #fixes corresponding intercepts for enc prob to a very high value
                         #why not ObsModel_ez
                         FieldConfig = c("Omega1" = 0, "Epsilon1" = 0, "Omega2"=1, "Epsilon2"=1),
                         RhoConfig = c("Beta1"=0, "Beta2"=0, "Epsilon1"=0, "Epsilon2"=0), #default is 0's for all
                         #Omega= spatial, eps=spat-temp
                         #1 and 2 are linear predictors
                         #so Jane ignores linear predictor 1... which is..?
                         Region = example$Region, 
                         purpose = "index2", #make an index for a stock assessment
                         #2021 author recomends changing index to index2
                         Options = c("Calculate_proportion"=TRUE), #["report_additional_variables"]=TRUE,
                         use_anisotropy=TRUE,
                         strata.limits = example$strata.limits, 
                         fine_scale = TRUE,
                         bias.correct = TRUE)

# Run model
fit = fit_model( "settings" = settings, 
                 "Lat_i" = example$sampling_data[, 'Lat'], 
                 "Lon_i" = example$sampling_data[, 'Lon'], 
                 "observations_LL" = example$sampling_data[, c('Lat', 'Lon')],
                 "t_i" = example$sampling_data[, 'Year'],
                 "c_i" = rep(0, nrow(example$sampling_data)), #???category... so just one for this
                 "b_i" = example$sampling_data[, 'Catch_KG'], 
                 "a_i" = example$sampling_data[, 'AreaSwept_km2'], 
                 "v_i" = example$sampling_data[, 'Vessel'],    #vessel effect; overdispersion...
                 "projargs" = "+proj=utm +zone=4 +units=km",
                 "newtonsteps" = 0 # prevent final newton step to decrease mgc score
)

plot(fit)
Index<-read.csv("Index.csv")

#== Q ==========================================================================================

#define q formulas
Q1_formula<- ~log(soak)
Q2_formula = ~log(soak)

fit.q<-fit_model( "settings" = settings, 
                  "Lat_i" = example.q$sampling_data[, 'Lat'], 
                  "Lon_i" = example.q$sampling_data[, 'Lon'], 
                  "observations_LL" = example.q$sampling_data[, c('Lat', 'Lon')],
                  "t_i" = example.q$sampling_data[, 'Year'],
                  "c_i" = rep(0, nrow(example.q$sampling_data)), #???category... so just one for this
                  "b_i" = example.q$sampling_data[, 'Catch_KG'], 
                  "a_i" = example.q$sampling_data[, 'AreaSwept_km2'], 
                  "v_i" = example.q$sampling_data[, 'Vessel'],    #vessel effect; overdispersion...
                  "projargs" = "+proj=utm +zone=4 +units=km",
                  "newtonsteps" = 0, # prevent final newton step to decrease mgc score
                  "catchability_data" = example.q$catchability_data,
                  #add catchability component based on soak time...
                  #"Q1_config_k" = rep(0, nrow(example.q$sampling_data)), #example.q$catchability_data$soak,
                  #"Q1_formula" = Q1_formula, #don't need config if specify the formula... 
                  #"Q2_config_k" = rep(1, nrow(example.q$sampling_data)), #example.q$catchability_data$soak,
                  "Q2_formula" = Q2_formula
)
plot_results(fit.q)
plot(fit.q)
Index<-read.csv("Index.csv")

out<-fit.q$Report
str(out)

 out$Index_gctl
 getwd()
 plot_results(
   fit=fit.q,
   settings = fit.q$settings,
   plot_set = c(2,3,7,16,17),
   working_dir = paste0(getwd(), "/VAST_out/qPlots/"),
   #year_labels = fit$year_labels,
   #years_to_plot = fit$years_to_plot,
   #category_names = fit$category_names,
   #strata_names = fit$strata_names,
   use_biascorr = TRUE,
   #map_list,
   check_residuals = TRUE,
   projargs = "+proj=longlat",
   #zrange,
   n_samples = 100,
   calculate_relative_to_average = FALSE,
   type = 1,
   n_cells = NULL,
   n_cells_residuals = NULL,
   RotationMethod = "PCA",
   quantiles = c(0.05, 0.5, 0.95)
 )
 covariate_data_full = fit.q$effects$covariate_data_full
 catchability_data_full = fit.q$effects$catchability_data_full
 
 # Plot 1st linear predictor, but could use `transformation` to apply link function
 pred = Effect.fit_model( fit.q,
                          focal.predictors = c("soak"),
                          which_formula = "Q2",
                          xlevels = 100,
                          transformation = list(link=identity, inverse=identity) )
 plot(pred)
  
# == X ===========================================================================================
 
 example.x$covariate_data[,'depth.f']<-na.approx(example.x$covariate_data[,'depth']);example.x$covariate_data[168:170,] 
 X1_formula = ~ bs( log(depth.f), degree=2, intercept=FALSE)
 #X1_formula = ~ poly( log(BOT_DEPTH), degree=2 )
 # I'm also showing how to construct an interaction
 X2_formula = ~ poly(log(depth.f), degree=2) + poly(slope, degree=2 )
 
  # If all covariates as "static" (not changing among years),
 #  then set Year = NA to cause values to be duplicated internally for all values of Year
 # If using a mix of static and dynamic covariates,
 #  then duplicate rows for static covariates for every value of Year
 # Here, all covariates are static, so I'm using the first approach.
 #example$covariate_data[,'Year'] = NA
 
 # Rescale covariates being used to have an SD >0.1 and <10 (for numerical stability)
 example.x$covariate_data[,'depth.f'] = example.x$covariate_data[,'depth.f'] / 100
 
 fit.x<-fit_model( "settings" = settings, 
                   "Lat_i" = example.x$sampling_data[, 'Lat'], 
                   "Lon_i" = example.x$sampling_data[, 'Lon'], 
                   "observations_LL" = example.x$sampling_data[, c('Lat', 'Lon')],
                   "t_i" = example.x$sampling_data[, 'Year'],
                   "c_i" = rep(0, nrow(example.x$sampling_data)), #???category... so just one for this
                   "b_i" = example.x$sampling_data[, 'Catch_KG'], 
                   "a_i" = example.x$sampling_data[, 'AreaSwept_km2'], 
                   "v_i" = example.x$sampling_data[, 'Vessel'],    #vessel effect; overdispersion...
                   "projargs" = "+proj=utm +zone=4 +units=km",
                   "newtonsteps" = 0, # prevent final newton step to decrease mgc score
                   #add covariates slope and depth
                   #"X1config_cp" = 0,
                   #"X2config_cp" = example.qx$covariate_data[, 1],
                   "covariate_data" = example.x$covariate_data,
                   #"X1_formula" = X1_formula,
                   "X2_formula" = X2_formula
 )
 
 plot(fit.x, working_dir = paste0(getwd(), "/VAST_out/covPlots/"),
       plot_set=c(3,11,12),
       TmbData=fit.x$data_list )
 str(summary(fit.x))
 str(fit.x$Report)
 fit.x$effects$covariate_data_full$linear_predictor
 
 # Must add data-frames to global environment (hope to fix in future)
 covariate_data_full = fit.x$effects$covariate_data_full
 catchability_data_full = fit.x$effects$catchability_data_full
 
 # Plot 1st linear predictor, but could use `transformation` to apply link function
 pred = Effect.fit_model( fit.x,
                          focal.predictors = c("depth.f"),
                          which_formula = "X2",
                          xlevels = 100,
                          transformation = list(link=identity, inverse=identity) )
 plot(pred)
 
 #also
 library(pdp)
 
 # Make function to interface with pdp
 pred.fun = function( object, newdata ){
    predict( x=object,
             Lat_i = object$data_frame$Lat_i,
             Lon_i = object$data_frame$Lon_i,
             t_i = object$data_frame$t_i,
             a_i = object$data_frame$a_i,
             c_i = object$data_frame$c_i,
            # b_i = object$data_frame$b_i,
             v_i = object$data_frame$v_i,
             what = "P1_iz",
             new_covariate_data = newdata,
             do_checks = FALSE )
 }
 
 # Run partial
 Partial = partial( object = fit.x,
                    pred.var = "slope",
                    pred.fun = pred.fun,
                    train = fit.x$covariate_data )
 
 # Make plot using ggplot2
 library(ggplot2)
 autoplot(Partial)
 
#== QX ================================================================================
 Q2_formula = ~log(soak)
 X2_formula = ~ poly(log(depth), degree=2) + poly(slope, degree=2 )  # ?poly
 
 example.qx$covariate_data[,'depth'] = example.qx$covariate_data[,'depth'] / 100
 
 fit.qx<-fit_model( "settings" = settings, 
                   "Lat_i" = example.qx$sampling_data[, 'Lat'], 
                   "Lon_i" = example.qx$sampling_data[, 'Lon'], 
                   "observations_LL" = example.qx$sampling_data[, c('Lat', 'Lon')],
                   "t_i" = example.qx$sampling_data[, 'Year'],
                   "c_i" = rep(0, nrow(example.qx$sampling_data)), #???category... so just one for this
                   "b_i" = example.qx$sampling_data[, 'Catch_KG'], 
                   "a_i" = example.qx$sampling_data[, 'AreaSwept_km2'], 
                   "v_i" = example.qx$sampling_data[, 'Vessel'],    #vessel effect; overdispersion...
                   "projargs" = "+proj=utm +zone=4 +units=km",
                   "newtonsteps" = 0, # prevent final newton step to decrease mgc score
                   "catchability_data" = example.qx$catchability_data,
                   "Q2_formula" = Q2_formula,
                   "covariate_data" = example.qx$covariate_data,
                   "X2_formula" = X2_formula
 ) 

 plot(fit.qx, working_dir = paste0(getwd(), "/VAST_out/qcovPlots/"),
      plot_set=c(3,11,12),
      TmbData=fit.qx$data_list )
 
 covariate_data_full = fit.qx$effects$covariate_data_full
 catchability_data_full = fit.qx$effects$catchability_data_full
 
 # Plot 1st linear predictor, but could use `transformation` to apply link function
 pred.dep = Effect.fit_model( fit.qx,
                          focal.predictors = c("depth"),
                          which_formula = "X2",
                          xlevels = 100,
                          transformation = list(link=identity, inverse=identity) )
 pred.sl = Effect.fit_model( fit.qx,
                              focal.predictors = c("slope"),
                              which_formula = "X2",
                              xlevels = 100,
                              transformation = list(link=identity, inverse=identity) )
 pred.soak = Effect.fit_model( fit.qx,
                              focal.predictors = c("soak"),
                              which_formula = "Q2",
                              xlevels = 100,
                              transformation = list(link=identity, inverse=identity) )
 par(mfrow=c(2,2))
 plot(pred.dep); plot(pred.sl); plot(pred.soak)
 
 plot(covariate_data$slope ~ covariate_data$depth)
 lm<-lm(covariate_data$slope ~ covariate_data$depth); summary(lm)
 abline(lm, col="red")
##lack of convergence and construction of model kind of indicates that VAST may not be best approach
## to Chatham Sablefish.  Survey is set up to catch fish at all stations, so not really a random
## survey because it targets the fishery... 

?fit_model
?make_settings
?VAST::make_data
?plot_results
?plot_variable
 
 #other plot shit I've found and throwing here for future use... 
 # Plot correlations (showing Omega1 as example)
 require(corrplot)
 Cov_omega1 = fit$Report$L_omega1_cf %*% t(fit$Report$L_omega1_cf)
 corrplot( cov2cor(Cov_omega1), method="pie", type="lower")
 corrplot.mixed( cov2cor(Cov_omega1) )
 