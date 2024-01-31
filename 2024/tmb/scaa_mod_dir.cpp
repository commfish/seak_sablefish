
// Sex-structured statistical catch-at-age model for NSEI sablefish that
// includes catch, fishery and survey CPUE, mark-recapture abundance estimates,
// fishery and survey weight-at-age, survey data about maturity-at-age and
// proportions-at-age, and fishery and survey age and length compositions.

// Original Author: Jane Sullivan, ummjane@gmail.com
// Current Driver: Phil Joy, philip.joy@alaska.gov
// Last updated March 2023

#include <TMB.hpp>
#include <numeric>

template <class Type> Type square(Type x){return x*x;}

// template <class Type> Type sumVec(vector<Type> vec){return std::accumulate(vec.begin(), vec.end(), 0.0);}

// template <class Type> Type mean(vector<Type> vec){return std::accumulate(vec.begin(), vec.end(), 0.0) / vec.size();}

// Dirichlet-multinomial

//template<class Type>
//Type ddirmult( vector<Type> x, vector<Type> prob, Type ln_theta, int give_log=0 ){

  // Pre-processing
//  int n_c = x.size();
//  vector<Type> p_exp(n_c);
//  vector<Type> p_obs(n_c);
//  Type Ntotal = x.sum();
//  p_exp = prob / prob.sum();
//  p_obs = x / Ntotal;
//  Type dirichlet_Parm = exp(ln_theta) * Ntotal;

  // https://github.com/nmfs-stock-synthesis/stock-synthesis/blob/main/SS_objfunc.tpl#L306-L314
  // https://github.com/James-Thorson/CCSRA/blob/master/inst/executables/CCSRA_v8.cpp#L237-L242
  // https://www.sciencedirect.com/science/article/pii/S0165783620303696

  // 1st term -- integration constant that could be dropped
//  Type logres = lgamma( Ntotal+1 );
//  for( int c=0; c<n_c; c++ ){
//    logres -= lgamma( Ntotal*p_obs(c) + 1.0 );
//  }

  // 2nd term in formula
//  logres += lgamma( dirichlet_Parm ) - lgamma( Ntotal+dirichlet_Parm );

  // Summation in 3rd term
 // for( int c=0; c<n_c; c++ ){
//    logres += lgamma( Ntotal*p_obs(c) + dirichlet_Parm*p_exp(c) );
//    logres -= lgamma( dirichlet_Parm * p_exp(c) );
//  }

//  if(give_log) return logres; else return exp(logres);
//}

template<class Type>
// Function to compute likelihood for dirichlet-multinomial (follows linear parameterization of
// Thorson et al. 2017)
// @param obs = Observed vector (in proportions)
// @param pred = Predicted vector (in proportions)
// @param Input_N = Input sample size
// @param Dir_Param = parameter for DM
// @param give_log = whether or not to compute log of likelihood
Type ddirmult( vector<Type> obs, 
               vector<Type> pred, 
               Type Input_N, 
               Type Dir_Param, 
               int do_log = 1){
  
  // Pre-processing
  int n_a = obs.size(); // Number of age-classes/bins
  Type Ntotal = Input_N; // Input sample size assumed
  vector<Type> p_pred = pred; // Predicted vector
  vector<Type> p_obs = obs; // Observed vector
  Type dirichlet_param = exp(Dir_Param) * Ntotal; // Dirichlet Alpha Parameter

  // NOTE: These calculations are in log space (Thorson et al. 2017)
  // 1st term -- integration constant that could be dropped
  Type logLike = lgamma(Ntotal + 1);
  for(int a = 0; a < n_a; a++) logLike -= lgamma(Ntotal * p_obs(a) + 1);

  // 2nd term in formula
  logLike += lgamma(dirichlet_param) - lgamma(Ntotal + dirichlet_param);

  // Summation in 3rd term
  for(int a = 0; a < n_a; a++){
    logLike += lgamma( (Ntotal * p_obs(a)) + (dirichlet_param * p_pred(a)) );
    logLike -= lgamma(dirichlet_param * p_pred(a));
  } // end a loop
  
  // Type phi = dirichlet_param.sum();
  // Type logLike = lgamma(Ntotal + 1.0) + lgamma(phi) - lgamma(Ntotal + phi);
  // for(int a = 0; a < n_a; a++) {
  //   logLike += -lgamma(p_obs(a) + 1.0) +
  //     lgamma(p_obs(a) + dirichlet_param(a)) -
  //     lgamma(dirichlet_param(a));
  // } // end a loop
  
  if(do_log == 1) return logLike; else return exp(logLike);
} // end function



template<class Type>
  Type objective_function<Type>::operator() ()
{
  // **DATA SECTION**
  
  // Model dimensions
  DATA_INTEGER(nyr)             // number of years in the model (year indexed as i)
  DATA_INTEGER(nage)            // number of ages in the model (age indexed as j)
  DATA_INTEGER(nsex)            // controls whether model is sex-structured or not (sex indexed as k)
  DATA_INTEGER(nlenbin)         // number of length bins (indexed as l)
  DATA_VECTOR(lenbin)           // length bins
  
  // Switch for recruitment estimate: 0 = penalized likelihood (fixed sigma_r), 1
   // = random effects
  DATA_INTEGER(random_rec)
    
  // Switch for selectivity type: 0 = a50, a95 logistic; 1 = a50, slope logistic
  DATA_INTEGER(slx_type)
    
  // Swtich for age composition type (hopefully one day length too): 0 = multinomial; 1 = Dirichlet-multinomial
  DATA_INTEGER(comp_type)
    
  // Switch for assumption on SPR equilibrium recruitment. 0 = arithmetic mean
  // (same as Federal assessment), 1 = geometric mean, 2 = median (2 not coded
  // yet)
  DATA_INTEGER(spr_rec_type)
    
  // Prior for natural mortality
  DATA_INTEGER(M_type)          // switch to fix M (0) or estimate with prior (1)
  DATA_SCALAR(p_log_M)          // mean M=0.1
  DATA_SCALAR(p_sigma_M)        // CV or sigma = 0.1
    
  // Time varying parameter blocks (indexed as h) - each vector contains the terminal years of
  // each time block. Used for both selectivity and catchability
  DATA_IVECTOR(fsh_blks)        // fishery  
  DATA_IVECTOR(srv_blks)        // survey  
    
  // Fixed parameters
  DATA_ARRAY(dmr)               // discard mortality in the fishery by year, age, and sex. assume constant dmr 0 or 0.16
  DATA_ARRAY(retention)         // probability of retaining a fish, sex- and age-based
  
  // Fxx levels that correspond with log_spr_Fxx in Parameter section (indexed as x)
  DATA_VECTOR(Fxx_levels)       // e.g. F35=0.35, F40=0.40, and F50=0.50
  
  // Priors ("p_" denotes prior)
  DATA_VECTOR(p_fsh_q)          // prior fishery catchability coefficient (on natural scale)
  DATA_VECTOR(sigma_fsh_q)      // sigma for fishery q
  DATA_SCALAR(p_srv_q)          // prior on survey catchability coefficient (on natural scale)
  DATA_SCALAR(sigma_srv_q)      // sigma for survey q
  DATA_SCALAR(p_mr_q)           // prior on mark-recapture catchability coefficient (on natural scale)
  DATA_SCALAR(sigma_mr_q)       // sigma for mark-recapture q
 
  // Weights on likelihood componets("wt_" denotes weight)
  DATA_SCALAR(wt_catch)         // catch
  DATA_SCALAR(wt_fsh_cpue)      // fishery cpue
  DATA_SCALAR(wt_srv_cpue)      // soak survey
  DATA_SCALAR(wt_mr)            // mark-recapture abundance  
  DATA_SCALAR(wt_fsh_age)       // fishery age comps
  DATA_SCALAR(wt_srv_age)       // survey age comps
  DATA_SCALAR(wt_fsh_len)       // fishery length comps
  DATA_SCALAR(wt_srv_len)       // survey length comps
  DATA_SCALAR(wt_rec_like)      // penalty on recruitment deviations
  DATA_SCALAR(wt_fpen)          // penality on fishing mortality deviations
  DATA_SCALAR(wt_spr)           // penalty on spawner per recruit calcs

  // INDICES OF ABUNDANCE
  
  // Catch
  DATA_VECTOR(data_catch)       // vector of landed catch estimates
  DATA_VECTOR(sigma_catch)      // assumed CV of 5% for catch
    
  // Mark-recapture estimates
  DATA_INTEGER(nyr_mr)          // number of years 
  DATA_IVECTOR(yrs_mr)          // vector of years
  DATA_VECTOR(data_mr)          // vector of estimates
  DATA_VECTOR(sigma_mr)         // posterior SDs for mark-recapture estimates
  
  // Fishery cpue
  DATA_INTEGER(nyr_fsh_cpue)    // number of years 
  DATA_IVECTOR(yrs_fsh_cpue)    // vector of years
  DATA_VECTOR(data_fsh_cpue)    // vector of estimates
  DATA_VECTOR(sigma_fsh_cpue)   // vector of fishery cpue SDs
    
  // Survey cpue 
  DATA_INTEGER(nyr_srv_cpue)     // number of years 
  DATA_IVECTOR(yrs_srv_cpue)     // vector of years
  DATA_VECTOR(data_srv_cpue)     // vector of estimates
  DATA_VECTOR(sigma_srv_cpue)    // vector of survey cpue SDs

  // BIOLOGICAL DATA
  
  // Timing in months (e.g. January = 1/12 = 0.083)
  DATA_SCALAR(spawn_month)        // month when spawning occurs (February)
  DATA_SCALAR(srv_month)          // month when survey begins (July)
  DATA_SCALAR(fsh_month)          // month when fishery begins (August)
  
  // Proportion mature at age (rows = 1, cols = nage); the rows could one day
  // represent annual or time-varying maturity
  DATA_MATRIX(prop_mature)
  
  // Proportion female at age in the survey (all years combined) applied in
  // calculation of spawning biomass. When nsex=2, the N matrix is
  // already split by sex, so you don't need prop_fem in spawning biomass calculation
  // (it will be a vector of 1's).
  DATA_VECTOR(prop_fem)
    
  // Sex ratio in survey (all years combined) used to split N matrix by sex.
  // First row is males at age, second row is females at age. The underlying
  // data underpins prop_fem and sex_ratio; this redundancy serves to
  // accommodate single sex and sex-structured versions of this model. When
  // nsex=1, the sex_ratio is a matrix of 1's so the N matrix doesn't get split
  // by sex.
  DATA_MATRIX(sex_ratio)
  
  // Weight-at-age (rows = 1, all years combined; cols = nage; matrices = 0 for
  // males, 1 for females); the rows could one day represent annual or
  // time-varying weight-at-age
  DATA_ARRAY(data_fsh_waa)       // Fishery (sex-specific)
  DATA_ARRAY(data_srv_waa)       // Survey (sex-specific)
  //std::cout << data_srv_waa << "\n";
    
  // Fishery age comps
  DATA_INTEGER(nyr_fsh_age)       // number of years 
  DATA_IVECTOR(yrs_fsh_age)       // vector of years
  DATA_MATRIX(data_fsh_age)       // matrix of observations (year, age)
  DATA_VECTOR(n_fsh_age)          // raw sample size for age comps
  DATA_VECTOR(effn_fsh_age)       // effective sample size
  
  // Survey age comps
  DATA_INTEGER(nyr_srv_age)       // number of years
  DATA_IVECTOR(yrs_srv_age)       // vector of years
  DATA_MATRIX(data_srv_age)       // matrix of observations (year, age)
  DATA_VECTOR(n_srv_age)          // raw sample size for age comps
  DATA_VECTOR(effn_srv_age)       // effective sample size
    
  // Fishery length comps
  DATA_INTEGER(nyr_fsh_len)       // number of years 
  DATA_IVECTOR(yrs_fsh_len)       // vector of years
  DATA_ARRAY(data_fsh_len)        // observations (nyr, nlenbin, nsex)
  DATA_ARRAY(n_fsh_len)           // raw sample size for length comps (nyr, 1, nsex) 
  DATA_ARRAY(effn_fsh_len)        // effective sample size (nyr, 1, nsex)
  
  // Survey length comps
  DATA_INTEGER(nyr_srv_len)       // number of years
  DATA_IVECTOR(yrs_srv_len)       // vector of years
  DATA_ARRAY(data_srv_len)        // observations (nyr, nlenbin, nsex)
  DATA_ARRAY(n_srv_len)           // raw sample size for length comps (nyr, 1, nsex)
  DATA_ARRAY(effn_srv_len)        // effective sample size (nyr, 1, nsex)
    
  // Ageing error transition matrix (proportion at reader age given true age)
  DATA_MATRIX(ageing_error)

  // Age-length transition matrices
  DATA_ARRAY(agelen_key_fsh)      // Fishery (nage, nlenbin, nsex)
  DATA_ARRAY(agelen_key_srv)      // Survey (nage, nlenbin, nsex)
  
  // **PARAMETER SECTION**
  
  // Dummy variable for troubleshooting 
  PARAMETER(dummy);          

  // Natural mortality
  PARAMETER(log_M);               // M_type = 0 is fixed, 1 is estimated    
  Type M = exp(log_M);
  
  // Selectivity 
  PARAMETER_ARRAY(log_fsh_slx_pars);       // Fishery selectivity (slx_type controls parameterization)
  PARAMETER_ARRAY(log_srv_slx_pars);       // Survey selectivity (slx_type controls parameterization)

  // Catchability
  PARAMETER_VECTOR(fsh_logq);     // fishery       
  PARAMETER(srv_logq);            // survey 
  PARAMETER(mr_logq);             // mark-recapture 
    
  // Recruitment (rec_devs include a parameter for all ages in the inital yr plus age-2 in all yrs)
  PARAMETER(log_rbar);                // Mean recruitment
  PARAMETER_VECTOR(log_rec_devs);     // Annual recruitment deviations (formerly nyr+nage-2, now nyr)
  PARAMETER(log_rinit);               // Mean initial numbers-at-age in syr
  PARAMETER_VECTOR(log_rinit_devs);   // Age-specific deviations from log_rinit (nage-2)
  PARAMETER(log_sigma_r);             // Variability in rec_devs and rinits, only estimated when using random effects (random_rec=1)
  
  // Fishing mortality
  PARAMETER(log_Fbar);            // Mean F on log scale 
  PARAMETER_VECTOR(log_F_devs);   // Annual deviations from log_Fbar (nyr)
  
  // SPR-based fishing mortality rates, i.e. the F at which the spawning biomass
  // per recruit is reduced to xx% of its value in an unfished stock
  PARAMETER_VECTOR(log_spr_Fxx);      // e.g. F35, F40, F50
  
  //  Parameter related to effective sample size for Dirichlet-multinomial
  //  likelihood used for composition data. Eqn 11 in Thorson et al. 2017.
  PARAMETER(log_fsh_theta); //ages... u
  PARAMETER(log_srv_theta); //ages
  PARAMETER_VECTOR(log_fsh_l_theta); 
  PARAMETER_VECTOR(log_srv_l_theta);
  //PARAMETER(log_fsh_l_theta); 
  //PARAMETER(log_srv_l_theta);
      
  // **DERIVED QUANTITIES**
  
  // Predicted indices of catch and abundance
  vector<Type> pred_catch(nyr);                 // Total catch biomass
  vector<Type> pred_landed(nyr);                // Landed catch biomass
  vector<Type> pred_wastage(nyr);               // Discarded biomass assumed dead
  vector<Type> pred_mr(nyr_mr);                 // Mark-recapture index of abundance (only years with an estimate)
  vector<Type> pred_mr_all(nyr);                // Mark-recapture index of abundance (all years!)
  vector<Type> pred_fsh_cpue(nyr_fsh_cpue);     // Fishery cpue
  vector<Type> pred_srv_cpue(nyr_srv_cpue);     // Survey cpue
  pred_catch.setZero();   
  pred_landed.setZero();  
  pred_wastage.setZero(); 
  pred_mr.setZero();
  pred_mr_all.setZero();
  pred_fsh_cpue.setZero();
  pred_srv_cpue.setZero();

  // Predicted age compositions
  matrix<Type> pred_fsh_age(nyr_fsh_age, nage);     // Fishery (with ageing error)
  matrix<Type> pred_srv_age(nyr_srv_age, nage);     // Survey (with ageing error)

  pred_fsh_age.setZero();
  pred_srv_age.setZero();
  
  // Predicted length compositions
  array<Type> pred_fsh_obsage(nyr_fsh_len, nage, nsex);   // Fishery (before age-length transition)
  array<Type> pred_srv_obsage(nyr_srv_len, nage, nsex);   // Survey (before age-length transition)
  array<Type> pred_fsh_len(nyr_fsh_len, nlenbin, nsex);   // Fishery
  array<Type> pred_srv_len(nyr_srv_len, nlenbin, nsex);   // Survey
  pred_fsh_len.setZero();
  pred_srv_len.setZero();  
  pred_fsh_obsage.setZero();
  pred_srv_obsage.setZero();
  
  // Predicted selectivity
  array<Type> fsh_slx(nyr, nage, nsex);           // Fishery selectivity-at-age by sex (on natural scale)
  array<Type> srv_slx(nyr, nage, nsex);           // Survey selectivity-at-age by sex(on natural scale)
  fsh_slx.setZero();
  srv_slx.setZero();

  // Predicted annual fishing mortality
  vector<Type> Fmort(nyr);      // On natural scale
  Fmort.setZero();
  
  // Derived matrices by year, age, and sex
  array<Type> N(nyr+1, nage, nsex);   // Abundance-at-age, projected 1 year forward
  array<Type> Z(nyr, nage, nsex);     // Total mortality
  array<Type> F(nyr, nage, nsex);     // Fishing mortality
  array<Type> S(nyr, nage, nsex);     // Total survivorship (natural + fishing)
  array<Type> C(nyr, nage, nsex);     // Total catch in numbers
  array<Type> L(nyr, nage, nsex);     // Total landed catch in numbers
  array<Type> D(nyr, nage, nsex);     // Total discards in numbers assumed to die post-release

  N.setZero();
  Z.setZero();
  F.setZero();
  S.setZero();
  C.setZero();
  L.setZero();
  D.setZero();

  // Derived time series of recruitment, biomass, and abundance (+ projected values)
  
  vector<Type> pred_rec(nyr);               // Predicted age-2 recruitment
  pred_rec.setZero();

  array<Type> biom(nyr+1, nage, nsex);      // Biomass by year, age, and sex, projected 1 year forward
  vector<Type> tot_biom(nyr+1);             // Summed over age and sex
  biom.setZero();
  tot_biom.setZero();
  
  array<Type> expl_biom(nyr+1, nage, nsex); // Exploitable biomass to fishery at the beginning of the fishery, projected 1 year forward
  array<Type> expl_abd(nyr+1, nage, nsex);  // Exploitable abundance to fishery at the beginning of the fishery, projected 1 year forward
  vector<Type> tot_expl_biom(nyr+1);        // Summed over age and sex
  vector<Type> tot_expl_abd(nyr+1);         // Summed over age and sex
  expl_biom.setZero();
  tot_expl_biom.setZero();
  expl_abd.setZero();
  tot_expl_abd.setZero();
  
  array<Type> vuln_abd(nyr+1, nage, nsex);  // Vulnerable abundance to survey at the beginning of the survey, projected 1 year forward
  vector<Type> tot_vuln_abd(nyr+1);          // Summed over age and sex
  vuln_abd.setZero();
  tot_vuln_abd.setZero();
  
  matrix<Type> spawn_biom(nyr+1, nage);     // Spawning biomass, just females
  vector<Type> tot_spawn_biom(nyr+1);       // Summed over age
  spawn_biom.setZero();
  tot_spawn_biom.setZero();

  // Other derived and projected values
  array<Type> survival_srv(nyr, nage, nsex);      // Survival at time of survey
  array<Type> survival_fsh(nyr, nage, nsex);      // Survival at time of fishery
  array<Type> survival_spawn(nyr, nage, nsex);    // Survival at time of spawning
  Type pred_rbar;                                 // Predicted mean recruitment
  Type sigma_r = exp(log_sigma_r);                // Estimated recruitment on natural scale 
  survival_srv.setZero();
  survival_fsh.setZero();
  survival_spawn.setZero();
  
  // SPR-based equilibrium reference points
  int n_Fxx = Fxx_levels.size();                  // Number of Fs estimated
  vector<Type> Fxx(n_Fxx + 1);                    // Separate Fxx vector that is scaled to fully selected values (+1 includes F=0)
  matrix<Type> Nspr(n_Fxx + 1, nage);             // Matrix of spawning numbers-at-age *FLAG* number of rows = number of estimated F_xx% (e.g. 3 = F35,F40,F50)
  vector<Type> SBPR(n_Fxx + 1);                   // Spawning biomass per recruit at various fishing levels
  vector<Type> SB(n_Fxx + 1);                     // Equilibrium spawning biomass at various fishing levels
  Fxx.setZero();
  Nspr.setZero();
  SBPR.setZero();
  SB.setZero();
  
  // ABC calculation
  array<Type> sel_Fxx(n_Fxx, nage, nsex);       // Age-specific fully-selected fishing mortality at each Fxx%
  array<Type> Z_Fxx(n_Fxx, nage, nsex);         // Total mortality at each Fxx%
  array<Type> S_Fxx(n_Fxx, nage, nsex);         // Total survivorship at each Fxx%
  matrix<Type> ABC(nyr+1, n_Fxx);               // ABCs at each F_xx%, retrospectively estimated for past years
  matrix<Type> wastage(nyr+1, n_Fxx);           // Discarded catch assumed to die under each F_xx%, retrospectively estimated for past years
  
  sel_Fxx.setZero();
  Z_Fxx.setZero();
  S_Fxx.setZero();
  ABC.setZero();
  wastage.setZero();
  
  // Priors, likelihoods, offsets, and penalty functions
  vector<Type> priors(3);       // Priors for catchability coefficients
  priors.setZero();
  Type prior_M = 0;             // Prior on natural mortality if estimated
  
  Type catch_like = 0;          // Catch  
  Type rec_like = 0;            // Recruitment
  Type fpen = 0;                // Penality for Fmort regularity
  Type spr_pen = 0;             // Penality for SPR-based reference points
  
  vector<Type> index_like(3);   // Fishery cpue, survey cpue, MR estimates 
  index_like.setZero();

  vector<Type> age_like(2);             // Fishery and survey age comps
  vector<Type> fsh_len_like(nsex);      // Fishery length comps
  vector<Type> srv_len_like(nsex);      // Survey length comps
  age_like.setZero();
  fsh_len_like.setZero();
  srv_len_like.setZero();
  
  // Offset for multinomial distribution that lets likelihood equal zero when obs = pred
  vector<Type> offset(2);               // Age comps (both fsh and srv)
  vector<Type> offset_fsh_len(nsex);    // Fishery length comps (sex-specific)
  vector<Type> offset_srv_len(nsex);    // Survey length comp (sex-specific)
  offset.setZero();
  offset_fsh_len.setZero();
  offset_srv_len.setZero();
  
  Type obj_fun = 0;             // Objective function
  
  Type c = 0.00001;             // Tiny constant to prevent likelihoods from going to 0
  
  // **MODEL**
  
  // Indexing: i = year, j = age, l = length bin, k = sex, h = time block, x = Fxx level
  
  // Fishery selectivity
  
  // Number of parameters in the chosen selectivity type: 
  int npar_slx = log_fsh_slx_pars.dim(1); // dim = array dimensions; 1 = # columns in array = # params in slx_type
  // std::cout << npar_slx << "\n number of params for slx type\n";
  
  // Preliminary calcs to bring parameters out of log space
  array<Type> fsh_slx_pars(log_fsh_slx_pars.dim);
  fsh_slx_pars.setZero();
  
  for (int k = 0; k < nsex; k++) {
    for (int h = 0; h < fsh_blks.size(); h++) {
      for (int n = 0; n < npar_slx; n++) { 
        fsh_slx_pars(h,n,k) = exp(log_fsh_slx_pars(h,n,k));
      }
    }
  }
  // std::cout << fsh_slx_pars << "\n slx out of log space\n";
  
  // Notes on the following syntax: the do while allows you to estimate parameters within a time block. It
  // "does" the looping over year and age "while" within the time block, then
  // iterates to the next block. Year is not in a for loop because it is
  // iterated by the do statement.
  
  // The switch for slx_type allows you to change parameterization. This could
  // easily be expanded to accomodate any selectivity type (the fsh_slx_pars
  // allows for a flexible number of parameters and time blocks)
  
  int i = 0;
  
  for(int h = 0; h < fsh_blks.size(); h++){
    do{
      for (int k = 0; k < nsex; k++) {
        for (int j = 0; j < nage; j++) {
          
          // Selectivity switch (case 0 or 1 references the value of slx_type)
          switch (slx_type) {
          
          case 0: // Logistic with a50 and a95, where fsh_slx_pars(h,0,k) = a50 and fsh_slx_pars(h,1,k) = a95
            fsh_slx(i,j,k) = Type(1.0) / ( Type(1.0) + exp(-log(Type(19)) * (j - fsh_slx_pars(h,0,k)) / (fsh_slx_pars(h,1,k) - fsh_slx_pars(h,0,k))) );
            break;
            
          case 1: // Logistic with a50 and slope, where fsh_slx_pars(h,0,k) = a50 and fsh_slx_pars(h,1,k) = slope.
            //  *This is the preferred logistic parameterization b/c it reduces parameter correlation*
            fsh_slx(i,j,k) = Type(1.0) / ( Type(1.0) + exp( Type(-1.0) * fsh_slx_pars(h,1,k) * (j - fsh_slx_pars(h,0,k)) ) );
            break;
          }
        }
      }
      i++;
    } while (i <= fsh_blks(h));
  }
  
  // std::cout << fsh_slx(1,1,1) << "\n Fishery selectivity \n";
  
  // Survey selectivity - see notes on syntax in fishery selectivity section

  // Preliminary calcs to bring parameters out of log space
  array<Type> srv_slx_pars(log_srv_slx_pars.dim);
  srv_slx_pars.setZero();
  
  for (int k = 0; k < nsex; k++) {
    for (int h = 0; h < srv_blks.size(); h++) {
      for (int n = 0; n < npar_slx; n++) { 
        srv_slx_pars(h,n,k) = exp(log_srv_slx_pars(h,n,k));
      }
    }
  }

  i = 0;     // re-set i to 0 (do not redeclare)

  for(int h = 0; h < srv_blks.size(); h++){
    do{
      for (int k = 0; k < nsex; k++) {
        for (int j = 0; j < nage; j++) {

          // Selectivity switch (case 0 or 1 references the value of slx_type)
          switch (slx_type) {

          case 0: // Logistic with a50 and a95, where srv_slx_pars(h,0,k) = a50 and srv_slx_pars(h,1,k) = a95
            srv_slx(i,j,k) = Type(1.0) / ( Type(1.0) + exp(-log(Type(19)) * (j - srv_slx_pars(h,0,k)) / (srv_slx_pars(h,1,k) - fsh_slx_pars(h,0,k))) );
            break;

          case 1: // Logistic with a50 and slope, where srv_slx_pars(h,0,k) = a50 and srv_slx_pars(h,1,k) = slope.
            //  *This is the preferred logistic parameterization b/c it reduces parameter correlation*
            srv_slx(i,j,k) = Type(1.0) / ( Type(1.0) + exp( Type(-1.0) * srv_slx_pars(h,1,k) * (j - srv_slx_pars(h,0,k)) ) );
            
            break;
          }
        }
      }
      i++;
    } while (i <= srv_blks(h));
  }

  // std::cout << srv_slx << "\n Survey selectivity \n";

  // Mortality and survivorship

  for (int i = 0; i < nyr; i++) {
    Fmort(i) = exp(log_Fbar + log_F_devs(i)); // Total annual fishing mortality
  }

  for (int k = 0; k < nsex; k++) {
    for (int i = 0; i < nyr; i++) {
      for (int j = 0; j < nage; j++) {

        // Fishing mortality by year, age, and sex. If discard mortality (dmr)
        // and retention probability = 1, this eqn collapses to Fmort(i) *
        // fsh_slx(i,j,k)
        F(i,j,k) = Fmort(i) * fsh_slx(i,j,k) * (retention(0,j,k) + dmr(i,j,k) * (Type(1.0) - retention(0,j,k)));

        // Total mortality by year, age, and sex
        Z(i,j,k) = M + F(i,j,k);

        // Survivorship by year, age, and sex
        S(i,j,k) = exp(Type(-1.0) * Z(i,j,k));
      }
    }
  }

  // std::cout << Fmort << "\n";
  // std::cout << F << "\n";
  // std::cout << Z << "\n";
  // std::cout << S << "\n";

  // Survival at time of survey, fishery, and spawning (fraction
  // surviving from beginning of year to the time of survey and fishery). These
  // quantities have the flexibility to vary by year, age, or sex, but currently
  // do not. Includes F because this model assumes continuous F.
  for (int k = 0; k < nsex; k++) {
    for (int i = 0; i < nyr; i++) {
      for (int j = 0; j < nage; j++) {
        survival_srv(i,j,k) = exp(Type(-1.0) * srv_month * (M + F(i,j,k)));
        survival_fsh(i,j,k) = exp(Type(-1.0) * fsh_month * (M + F(i,j,k)));
        survival_spawn(i,j,k) = exp(Type(-1.0) * spawn_month * (M + F(i,j,k)));
      }
    }
  }

  // std::cout << survival_srv << "\n";
  // std::cout << survival_fsh << "\n";

  // Abundance and recruitment

  // Start year: initial numbers-at-age (sage + 1 to plus group - 1). *FLAG*
  // Nijk here I've assumed the sex ratio from the survey. Could also use 0.5
  // (assuming 50/50 sex ratio). Don't know what the standard practice is here.
  for (int k = 0; k < nsex; k++) {
    for (int j = 1; j < nage-1; j++) {
      N(0,j,k) = exp(log_rinit - M * Type(j) + log_rinit_devs(j-1)) * Type(0.5); //sex_ratio(k,j);
    }
  }

  // Start year: plus group *FLAG* sex ratio from survey or 50/50 or ?
  for (int k = 0; k < nsex; k++) {
    N(0,nage-1,k) = exp(log_rinit - M * Type(nage-1)) / (1 - exp(-M)) * Type(0.5); //sex_ratio(k,nage-1);
  }

  // Recruitment in all years (except the projected year) *FLAG* sex ratio from
  // survey or 50/50 or ?
  for (int k = 0; k < nsex; k++) {
    for (int i = 0; i < nyr; i++) {
      N(i,0,k) = exp(log_rbar + log_rec_devs(i)) * Type(0.5); //sex_ratio(k,0);
    }
  }

  // Project remaining N matrix
  for (int k = 0; k < nsex; k++) {
    for (int i = 0; i < nyr; i++) {
      for (int j=0; j < nage-1; j++) {
        N(i+1,j+1,k) = N(i,j,k) * S(i,j,k);   // S is total survivorship
      }
    }
  }

  // Plus group for start year + 1 to final year + 1 (sum of cohort survivors and
  // surviving memebers of last year's plus group)
  for (int k = 0; k < nsex; k++) {
    for (int i = 0; i < nyr; i++) {
      N(i+1,nage-1,k) += N(i,nage-1,k) * S(i,nage-1,k);
    }
  }

  // Projected recruitment (average over recent 15 years, excluding most recent
  // 2 years), where the indexing of log_rec_devs(0,nyr+nage-3) *FLAG* sex ratio from
  // survey or 50/50 or ?
  for (int k = 0; k < nsex; k++) {
    for (int i = nyr-16; i <= nyr-2; i++) {
      N(nyr,0,k) += exp(log_rbar + log_rec_devs(i)) * Type(0.5); //sex_ratio(k,0);
    }
    N(nyr,0,k) /= Type(15.0);
  }

  // FLAG - Alternative way using mean(). Want to find a way to sum() over
  // vector
  // vector<Type> tmp = exp(log_rbar + log_rec_devs(nyr+nage-3-16, nyr+nage-3-2));
  // N(nyr,0) = mean(tmp);

  // std::cout << N << "\n";

  // In numbers-at-age: Total catch (C), landed catch (L), and discarded catch
  // assumed to die (D). Currently assuming continuous F, but may want to add
  // discrete F in the future. F is fully-selected, S is total survivorship. The
  // 0 in data_srv_waa(0,j,k) is a place holder if you ever wanted time-varying
  // weight-at-age.

  for (int k = 0; k < nsex; k++) {
    for (int i = 0; i < nyr; i++) {
      for (int j = 0; j < nage; j++) {

        // Total catch in numbers and summed to get a total catch biomass by year
        C(i,j,k) = N(i,j,k) * F(i,j,k) * (Type(1.0) - S(i,j,k)) / Z(i,j,k);

        // Landed catch in numbers and summed to get total landed catch in
        // biomass by year
        L(i,j,k) = retention(0,j,k) * N(i,j,k) * F(i,j,k) * (Type(1.0) - S(i,j,k)) / Z(i,j,k);
        pred_landed(i) += L(i,j,k) * data_fsh_waa(0,j,k) / Type(1e3) ;           // in mt

        // Discarded catch in numbers and summed to get total biomass of dead
        // discards by year
        D(i,j,k) = dmr(i,j,k) * (Type(1.0) - retention(0,j,k)) * N(i,j,k) * F(i,j,k) * (Type(1.0) - S(i,j,k)) / Z(i,j,k);
        pred_wastage(i) += D(i,j,k) * data_srv_waa(0,j,k) / Type(1e3) ;           // in mt

      }
    }
  }
  // Total catch summed to get a total catch biomass by year
    for (int i = 0; i < nyr; i++) {
    pred_catch(i) += pred_landed(i) + pred_wastage(i);

  }

  // std::cout << C << "\nTotal catch in numbers-at-age\n";
  // std::cout << pred_catch << "\nPredicted total catch biomass\n"
  // std::cout << L << "\nLanded catch in numbers-at-age\n";
  // std::cout << pred_landed << "\nPredicted landed catch biomass\n"
  // std::cout << D << "\nDead discards in numbers-at-age\n";
  // std::cout << pred_wastage << "\nPredicted dead discarded biomass\n"

  // Predicted recruitment by year, summed over the sexes
  
    //for (int i = 0; i < nyr; i++) {  // alternative approach... same answer.
   //  pred_rec(i) = exp(log_rbar + log_rec_devs(i));
   // }
  
  for (int k = 0; k < nsex; k++) {
    for (int i = 0; i < nyr; i++) {
      pred_rec(i) += N(i,0,k);
    }
  }
  // std::cout << "Predicted recruitment\n" << pred_rec << "\n";

  // Mean recruitment
  Type len_rec = pred_rec.size();
  Type sum_rec = 0; // temporary variable (sum over predicted recruitment values)

  for (int i = 0; i < nyr; i++) {
    sum_rec += pred_rec(i);
  }
  pred_rbar = sum_rec / len_rec;

  // std::cout << "sum_rec\n" << sum_rec << "\n";
  // std::cout << "pred_rbar\n" << pred_rbar << "\n";

  // Various flavors of projected biomass estimates [Note: the 0 in
  // data_srv_waa(0,j,k) and prop_mature(0,k) is a place holder if you ever
  // wanted time blocks or annual variation in weight-at-age or maturity]

  for (int k = 0; k < nsex; k++) {
    for (int i = 0; i < nyr; i++) {
      for (int j = 0; j < nage; j++) {

        // Total biomass at time of longline survey
        biom(i,j,k) = data_srv_waa(0,j,k) * N(i,j,k) * survival_srv(i,j,k);

        // Exploitable biomass to the fishery at the beginning of the fishery
        expl_biom(i,j,k) = data_srv_waa(0,j,k) * fsh_slx(i,j,k) * retention(0,j,k) * N(i,j,k) * survival_fsh(i,j,k);

        // Exploitable abundance to the fishery at the beginning of the fishery
        expl_abd(i,j,k) = fsh_slx(i,j,k) * retention(0,j,k) * N(i,j,k) * survival_fsh(i,j,k);

        // Vulnerable abundance to the survey at the beginning of the survey
        vuln_abd(i,j,k) = srv_slx(i,j,k) * N(i,j,k) * survival_srv(i,j,k);
      }
    }
  }

  // Project those values into the next year
  for (int k = 0; k < nsex; k++) {
    for (int j = 0; j < nage; j++) {
      biom(nyr,j,k) = data_srv_waa(0,j,k) * N(nyr,j,k) * survival_srv(nyr-1,j,k);
      expl_biom(nyr,j,k) = data_srv_waa(0,j,k) * fsh_slx(nyr-1,j,k) * retention(0,j,k) * N(nyr,j,k) * survival_fsh(nyr-1,j,k);
      expl_abd(nyr,j,k) = fsh_slx(nyr-1,j,k) * retention(0,j,k) * N(nyr,j,k) * survival_fsh(nyr-1,j,k);
      vuln_abd(nyr,j,k) = srv_slx(nyr-1,j,k) * N(nyr,j,k) * survival_srv(nyr-1,j,k);
    }
  }
  // std::cout << "Predicted biomass by age and sex \n" << biom << "\n";
  // std::cout << "Predicted exploited biomass by age and sex \n" << expl_biom << "\n";
  // std::cout << "Predicted vulnerable abundance by age and sex \n" << vuln_abd << "\n";

  // Sum variables to get an annual total
  for (int k = 0; k < nsex; k++) {
    for (int i = 0; i <= nyr; i++) {    // include projection year
      for (int j = 0; j < nage; j++) {

        // Total biomass at time of longline survey
        tot_biom(i) += biom(i,j,k);

        // Exploitable biomass to the fishery at the beginning of the fishery
        tot_expl_biom(i) += expl_biom(i,j,k);

        // Exploitable abundance to the fishery at the beginning of the fishery
        tot_expl_abd(i) += expl_abd(i,j,k);

        // Vulnerable abundance to the survey at the beginning of the survey
        tot_vuln_abd(i) += vuln_abd(i,j,k);
      }
    }
  }
  // std::cout << "Annual predicted biomass \n" << tot_biom << "\n";
  // std::cout << "Annual predicted exploited biomass\n" << tot_expl_biom << "\n";
  // std::cout << "Annual predicted vulnerable abundance\n" << tot_vuln_abd << "\n";

  // Female spawning biomass: Calculated a little differently if model is
  // single-sex or sex-structured

  if (nsex == 1) {

    // By year and age
    for (int i = 0; i < nyr; i++) {
      for (int j = 0; j < nage; j++) {
        spawn_biom(i,j) = data_srv_waa(0,j,0) * N(i,j,0) * survival_spawn(i,j,0) * prop_fem(j) * prop_mature(0,j);
      }
    }
    // Projected
    for (int j = 0; j < nage; j++) {
      spawn_biom(nyr,j) = data_srv_waa(0,j,0) * N(nyr,j,0) * survival_spawn(nyr-1,j,0) * prop_fem(j) * prop_mature(0,j);
    }
    // Annual totals, summed over age
    for (int i = 0; i <= nyr; i++) {    // include projection year
      for (int j = 0; j < nage; j++) {
        tot_spawn_biom(i) += spawn_biom(i,j);
      }
    }
  }

  if (nsex == 2) {

    // By year and age
    for (int i = 0; i < nyr; i++) {
      for (int j = 0; j < nage; j++) {
        spawn_biom(i,j) = data_srv_waa(0,j,1) * N(i,j,1) * survival_spawn(i,j,1) * prop_mature(0,j);
      }
    }
    // Projected
    for (int j = 0; j < nage; j++) {
      spawn_biom(nyr,j) = data_srv_waa(0,j,1) * N(nyr,j,1) * survival_spawn(nyr-1,j,1) * prop_mature(0,j);
    }
    // Annual totals, summed over age
    for (int i = 0; i <= nyr; i++) {    // include projection year
      for (int j = 0; j < nage; j++) {
        tot_spawn_biom(i) += spawn_biom(i,j);
      }
    }
  }
  // std::cout << "Predicted spawning biomass by age\n" << spawn_biom << "\n";
  // std::cout << "Annual predicted spawning biomass\n" << tot_spawn_biom << "\n";

  // Predicted values

  // Mark-recapture catchability and predicted abundance (in millions)
  Type mr_q = exp(mr_logq);

  for (int i = 0; i < nyr_mr; i++) {
    pred_mr(i) = mr_q * (tot_expl_abd(yrs_mr(i)) / Type(1e6)); // Just in years with a MR estimate
  }
  // std::cout << "Predicted MR \n" << pred_mr << "\n";

  for (int i = 0; i < nyr; i++) {
    pred_mr_all(i) = mr_q * (tot_expl_abd(i) / Type(1e6)); // All years
  }
  // std::cout << "Predicted MR for all years\n" << pred_mr_all << "\n";

  // Fishery catchability and predicted cpue - blocks for fishery correspond to
  // pre- and post- Equal Quota Share
  vector<Type> fsh_q(fsh_blks.size());
  for (int h = 0; h < fsh_blks.size(); h++){
    fsh_q(h) = exp(fsh_logq(h));
  }
  
  i = 0;
  for(int h = 0; h < fsh_blks.size(); h++){
    while (i < yrs_fsh_cpue.size() && yrs_fsh_cpue(i) <= fsh_blks(h)) {
      pred_fsh_cpue(i) = fsh_q(h) * tot_expl_biom(yrs_fsh_cpue(i));
      i++;
    }
  }
  // std::cout << "Predicted fishery cpue\n" << pred_fsh_cpue << "\n";

  // Survey catchability and predicted survey cpue
  Type srv_q = exp(srv_logq);

  for (int i = 0; i < nyr_srv_cpue; i++) {
    pred_srv_cpue(i) = srv_q * tot_vuln_abd(yrs_srv_cpue(i));
  }
  // std::cout << "Predicted srv cpue\n" << pred_srv_cpue << "\n";

  // Predicted fishery age compositions - for landed portion of the catch

  // Temporary variables for age comp calcs
  Type sumL = 0;
  vector<Type> sumL_age(nage);
  vector<Type> sumN_age(nage);

  for (int i = 0; i < nyr_fsh_age; i++) {

    // sumL = temporary variable, landed catch in numbers summed over age and
    // sex in a given year
    sumL = 0;
    for (int k = 0; k < nsex; k++) {
      for (int j = 0; j < nage; j++) {
        sumL += L(yrs_fsh_age(i),j,k);
      }
    }
    // sumL_age = temporary vector of landed catch in numbers by age in a given
    // year (combine sexes since we currently do not have sex-structured age
    // comps)
    sumL_age.setZero();
    for (int k = 0; k < nsex; k++) {
      for (int j = 0; j < nage; j++) {
        sumL_age(j) += L(yrs_fsh_age(i),j,k);
      }
    }

    // Get predicted age comps (proportions-at-age)
    for (int j = 0; j < nage; j++) {
      pred_fsh_age(i,j) = sumL_age(j) / sumL;
    }
    // Loop over each year i
  }
  pred_fsh_age = pred_fsh_age * ageing_error; // apply ageing error matrix

  // std::cout << "Predicted fishery age comps\n" << pred_fsh_age << "\n";

  // Test do the predicted age comps sum to 1
  // vector<Type> tst(nyr_fsh_age);
  // for (int i = 0; i < nyr_fsh_age; i++) {
  //   for (int j = 0; j < nage; j++) {
  //     tst(i) += pred_fsh_age(i,j);
  //   }
  // }
  // std::cout << "Do the comps sum to 1?\n" << tst << "\n";
  //
  // Type tst_n = tst.size();
  // std::cout << "Length of tst vector\n" << tst_n << "\n";

  // Predicted survey age compositions
  for (int i = 0; i < nyr_srv_age; i++) {

    // sumN_age = temporary vector of catch in numbers by age in a given year
    // (combine sexes since we currently do not have sex-structured age comps)
    sumN_age.setZero();
    for (int k = 0; k < nsex; k++) {
      for (int j = 0; j < nage; j++) {
        sumN_age(j) += vuln_abd(yrs_srv_age(i),j,k);
      }
    }
    // Get predicted age comps (proportions-at-age)
    for (int j = 0; j < nage; j++) {
      pred_srv_age(i,j) = sumN_age(j) / tot_vuln_abd(yrs_srv_age(i));
    }
    // Loop over each year i
  }
  pred_srv_age = pred_srv_age * ageing_error; // apply ageing error matrix

  // // Test do the predicted age comps sum to 1
  // vector<Type> tst(nyr_srv_age);
  // for (int i = 0; i < nyr_srv_age; i++) {
  //   for (int j = 0; j < nage; j++) {
  //     tst(i) += pred_srv_age(i,j);
  //   }
  // }
  // std::cout << "Do the age comps sum to 1?\n" << tst << "\n";

  // Type tst_n = tst.size();
  // std::cout << "Length of tst vector\n" << tst_n << "\n";

  // FLAG - would like to know the TMB syntax to do the age comps in fewer
  // lines, for example:
  // for (int i = 0; i < nyr_srv_age; i++) {
  //   for (int j = 0; j < nage; j++) {
  //     pred_srv_age(i,j) = N(yrs_srv_age(i),j) * srv_slx(j) / sum(N(yrs_srv_age(i)) * srv_slx(j));
  //   }
  // }

  // Predicted fishery length compositions (for landed portion of catch)- *FLAG*
  // early in development, would like to streamline

  matrix<Type> sumL_jk(nage,nsex);
  vector<Type> sumL_k(nsex);

  for (int i = 0; i < nyr_fsh_len; i++) {

    sumL_k.setZero();   // tmp variable for each year
    sumL_jk.setZero();

    for (int k = 0; k < nsex; k++) {
      for (int j = 0; j < nage; j++) {
        sumL_k(k) += L(yrs_fsh_len(i),j,k);          // numbers by sex
        sumL_jk(j,k) += L(yrs_fsh_len(i),j,k);       // numbers by sex and age
      }
    }

    for (int k = 0; k < nsex; k++) {
      for (int j = 0; j < nage; j++) {
        pred_fsh_obsage(i,j,k) = sumL_jk(j,k) / sumL_k(k);  // Get predicted age comps (proportions-at-age)
      }
    }
  }

  matrix<Type> tmp_pred_fsh_obsage(nyr_fsh_len,nage);
  matrix<Type> tmp_fsh_agelen(nage,nlenbin);
  matrix<Type> tmp_pred_fsh_len(nyr_srv_len,nlenbin);

  for (int k = 0; k < nsex; k++) {

    tmp_pred_fsh_obsage.setZero();
    for (int i = 0; i < nyr_fsh_len; i++) {
      for (int j = 0; j < nage; j++) {
        tmp_pred_fsh_obsage(i,j) = pred_fsh_obsage(i,j,k);    // Extract nyr x nage matrix
      }
    }

    tmp_fsh_agelen.setZero();
    for (int j = 0; j < nage; j++) {
      for (int l = 0; l < nlenbin; l++) {
        tmp_fsh_agelen(j,l) = agelen_key_fsh(j,l,k);          // Extract age-length key
      }
    }

    tmp_pred_fsh_len.setZero();
    tmp_pred_fsh_len = tmp_pred_fsh_obsage * tmp_fsh_agelen;  // Apply age-length key

    for (int i = 0; i < nyr_fsh_len; i++) {
      for (int l = 0; l < nlenbin; l++) {
        pred_fsh_len(i,l,k) = tmp_pred_fsh_len(i,l);          // Put everything back in the arry
      }
    }
  }

  // Test do the initial observed age comps sum to 1
  // vector<Type> tst2(nyr_fsh_len);
  // for (int i = 0; i < nyr_fsh_len; i++) {
  //   for (int l = 0; l < nlenbin; l++) {
  //     tst2(i) += pred_fsh_len(i,l,0);
  //   }
  // }
  // std::cout << "Do the length comps sum to 1?\n" << tst2 << "\n";

  // Predicted survey length compositions - *FLAG* would like to streamline this

  matrix<Type> sumN_jk(nage,nsex);
  vector<Type> sumN_k(nsex);

  for (int i = 0; i < nyr_srv_len; i++) {

    sumN_k.setZero();   // tmp variable for each year
    sumN_jk.setZero();

    for (int k = 0; k < nsex; k++) {
      for (int j = 0; j < nage; j++) {
        sumN_k(k) += vuln_abd(yrs_srv_len(i),j,k);          // numbers by sex
        sumN_jk(j,k) += vuln_abd(yrs_srv_len(i),j,k);       // numbers by sex and age
      }
    }

    for (int k = 0; k < nsex; k++) {
      for (int j = 0; j < nage; j++) {
        pred_srv_obsage(i,j,k) = sumN_jk(j,k) / sumN_k(k);  // Get predicted age comps (proportions-at-age)
      }
    }
  }

  matrix<Type> tmp_pred_obsage(nyr_srv_len,nage);
  matrix<Type> tmp_agelen(nage,nlenbin);
  matrix<Type> tmp_pred_len(nyr_srv_len,nlenbin);

  for (int k = 0; k < nsex; k++) {

    tmp_pred_obsage.setZero();
    for (int i = 0; i < nyr_srv_len; i++) {
      for (int j = 0; j < nage; j++) {
        tmp_pred_obsage(i,j) = pred_srv_obsage(i,j,k);      // Extract nyr x nage matrix
      }
    }

    tmp_agelen.setZero();
    for (int j = 0; j < nage; j++) {
      for (int l = 0; l < nlenbin; l++) {
        tmp_agelen(j,l) = agelen_key_srv(j,l,k);            // Extract age-length key
      }
    }

    tmp_pred_len.setZero();
    tmp_pred_len = tmp_pred_obsage * tmp_agelen;            // Apply age-length key

    for (int i = 0; i < nyr_srv_len; i++) {
      for (int l = 0; l < nlenbin; l++) {
        pred_srv_len(i,l,k) = tmp_pred_len(i,l);            // Put everything back in the array
      }
    }
  }

  // Test do the initial observed age comps sum to 1
  // vector<Type> tst3(nyr_srv_len);
  // for (int i = 0; i < nyr_srv_len; i++) {
  //   for (int l = 0; l < nlenbin; l++) {
  //     tst3(i) += pred_srv_len(i,l,0);
  //   }
  // }
  // std::cout << "Do the length comps sum to 1?\n" << tst3 << "\n";

  // Compute SPR rates and spawning biomass under different Fxx levels. Note
  // that biological reference points are only for the female component of the
  // population.

  // Preliminary calcs, get Fs out of log space
  vector<Type> spr_Fxx(n_Fxx+1);
  spr_Fxx(0) = 0;     // No fishing

  for(int x = 1; x <= n_Fxx; x++) {
    spr_Fxx(x) = exp(log_spr_Fxx(x-1));
  }

  // For SPR calculations, use only female fishery selectivity in the most recent time block for all calculations. The 'nsex-1' should work
  // for both single sex and sex-structured versions
  vector<Type> spr_fsh_slx(nage);
  for (int j = 0; j < nage; j++) {
      spr_fsh_slx(j) = fsh_slx(nyr-1,j,nsex-1);
  }
  // std::cout << "spr fishery selectivity\n" << spr_fsh_slx << "\n";

  Fxx(0) = 0;     // No fishing

  // Scale Fxx to fully selected values (not necessary for logistic selectivity
  // b/c max is 1 but may be needed for other selectivity types in future
  // development).
  for(int x = 1; x <= n_Fxx; x++) {
    Fxx(x) = spr_Fxx(x) * max(spr_fsh_slx);
  }
  // std::cout << "Fxx\n" << Fxx << "\n";

  // Populate numbers of potential spawners at age matrix - Note: for the Nspr
  // and SBPR calculations, the Federal assessment uses the equivalent of
  // spr_Fxx instead of the scales Fxx. Not sure why.
  for(int x = 0; x <= n_Fxx; x++) {

    Nspr(x,0) = Type(1.0);    // Initialize SPR with 1

    // Survival equation by age
    for(int j = 1; j < nage - 1; j++) {
      Nspr(x,j) = Nspr(x,j-1) * exp(Type(-1.0) * (Fxx(x) * spr_fsh_slx(j-1) + M));
    }

    // Plus group
    Nspr(x,nage-1) = Nspr(x,nage-2) * exp(Type(-1.0) * (Fxx(x) * spr_fsh_slx(nage-2) + M)) /
      (Type(1.0) - exp(Type(-1.0) * (Fxx(x) * spr_fsh_slx(nage-1) + M)));
  }
  // std::cout << "Number of spawners\n" << Nspr << "\n";

  // Unfished spawning biomass per recruit
  for(int j = 0; j < nage; j++) {
      SBPR(0) +=  Nspr(0,j) * prop_mature(j) * data_srv_waa(0,j,1) * survival_spawn(nyr-1,j,nsex-1); //Type(0.5) *
  }

  // Remaining spawning biomass per recruit matrix
  for(int x = 1; x <= n_Fxx; x++) {
    for(int j = 0; j < nage; j++) {

      if (nsex == 1) { // single sex model uses prop_fem vector
        SBPR(x) +=  Nspr(x,j) * prop_mature(j) * data_srv_waa(0,j,1) * exp(Type(-1.0) * spawn_month * (M + Fxx(x) * spr_fsh_slx(j))); //prop_fem(j) *

      }
      if (nsex == 2) { // sex-structured model uses sex_ratio matrix
        SBPR(x) += Nspr(x,j) * prop_mature(0,j) * data_srv_waa(0,j,1) * exp(Type(-1.0) * spawn_month * (M + Fxx(x) * spr_fsh_slx(j))); //sex_ratio(nsex-1,j) *
      }
    }
  }
  // std::cout << "Spawning biomass per recruit\n" << SBPR << "\n";

  // Mean recruitment, where spr_rec_type is a switch for different assumptions
  // of what the "mean" should be
  Type mean_rec;

  switch (spr_rec_type) {

  case 0: // Arithmentic mean

    mean_rec = 0;
    for (int i = 0; i < nyr; i++) {
      mean_rec += pred_rec(i);
    }
    mean_rec /= nyr;
    break;

  case 1: // Geometric mean

    mean_rec = 1;
    for (int i = 0; i < nyr; i++) {
      mean_rec *= pred_rec(i);
    }
    mean_rec = pow(mean_rec, Type(1)/nyr);
    break;

    // case 2: // Median *FLAG* future development
  }

  // Virgin female spawning biomass (no fishing), assuming 50:50 sex ratio for
  // recruitment (equivalent to B_100)
  // SB(0) = SBPR(0) * mean_rec;

  // Spawning biomass as a fraction of virgin spawning biomass - FLAG check this
  // for(int x = 1; x <= n_Fxx; x++) {
  //   SB(x) = Fxx_levels(x-1) * SB(0);
  // }

  for(int x = 0; x <= n_Fxx; x++) {
    SB(x) = Type(0.5) * SBPR(x) * mean_rec; //
  }


  // std::cout << "Spawning biomass\n" << SB << "\n";

  // Get Allowable Biological Catch and wastage estimates for different Fxx levels: all of
  // these should be sex-structured, then final ABC will be summed across sexes
  for(int x = 0; x < n_Fxx; x++) {
    for(int k = 0; k < nsex; k++) {
      for(int j = 0; j < nage; j++) {

        // Fully selected fishing mortality by age and sex. If discard mortality
        // (dmr) and retention probability = 1, this eqn collapses to Fmort *
        // fsh_slx)
        sel_Fxx(x,j,k) = Fxx(x+1) * fsh_slx(nyr-1,j,k) * (retention(0,j,k) + dmr(nyr-1,j,k) * (Type(1.0) - retention(0,j,k)));
        Z_Fxx(x,j,k) = M + sel_Fxx(x,j,k);    // Total instantaneous mortality at age
        S_Fxx(x,j,k) = exp(-Z_Fxx(x,j,k));                    // Total survival at age
      }
    }
  }

  for(int i = 0; i <= nyr; i++) { // include forecast year
    for(int x = 0; x < n_Fxx; x++) {
      for(int k = 0; k < nsex; k++) {
        for(int j = 0; j < nage; j++) {

          // ABC calculation (landed catch under Fxx) using projected abundance
          ABC(i,x) += data_srv_waa(0,j,k) * retention(0,j,k) * N(i,j,k) * sel_Fxx(x,j,k) * (Type(1.0) - S_Fxx(x,j,k)) / Z_Fxx(x,j,k);

          // Discarded catch assumed to die under Fxx
          wastage(i,x) += data_srv_waa(0,j,k) * dmr(nyr-1,j,k) * (Type(1.0) - retention(0,j,k)) * N(i,j,k) * sel_Fxx(x,j,k) * (Type(1.0) - S_Fxx(x,j,k)) / Z_Fxx(x,j,k);
        }
      }
    }
  }

   // The final ABC is then the difference between the preliminary ABC and wastage estimates
  for(int i = 0; i <= nyr; i++) {
    for(int x = 0; x < n_Fxx; x++) {
      ABC(i,x) = ABC(i,x) - wastage(i,x);
    }
  }

  // std::cout << "ABC\n" << ABC << "\n";
  // std::cout << "Wastage\n" << wastage << "\n";

  // Get sex-specific numbers-at-length for Luke Rogers data request
  array<Type> Nlen(nyr+1,nlenbin,nsex);
  matrix<Type> tmp_N(nyr+1,nage);
  matrix<Type> tmp_Nlen(nyr+1,nage);
  // matrix<Type> tmp_agelen(nage,nlenbin); // already defined when predicting len comps

  for (int k = 0; k < nsex; k++) {

    tmp_N.setZero();
    for (int i = 0; i < nyr+1; i++) {
      for (int j = 0; j < nage; j++) {
        tmp_N(i,j) = N(i,j,k); // Extract nyr+1 x nage matrix
      }
    }

    tmp_agelen.setZero();
    for (int j = 0; j < nage; j++) {
      for (int l = 0; l < nlenbin; l++) {
        tmp_agelen(j,l) = agelen_key_srv(j,l,k); // Extract sex-specific age-length key
      }
    }

    tmp_Nlen.setZero();
    tmp_Nlen = tmp_N * tmp_agelen; // Apply age-length key

    for (int i = 0; i < nyr+1; i++) {
      for (int l = 0; l < nlenbin; l++) {
        Nlen(i,l,k) = tmp_Nlen(i,l); // Put everything into array
      }
    }
  }

  // Priors

  // Fishery cpue catchability coefficient
  for (int h = 0; h < fsh_blks.size(); h++){
    priors(0) += square( log(fsh_q(h) / p_fsh_q(h)) ) / ( Type(2.0) * square(sigma_fsh_q(h)) );
  }

  // Survey catchability coefficient
  priors(1) = square( log(srv_q / p_srv_q) ) / ( Type(2.0) * square(sigma_srv_q) );

  // Mark-recapture abundance estimate catchability coefficient
  priors(2) = square( log(mr_q / p_mr_q) ) / ( Type(2.0) * square(sigma_mr_q) );

  // std::cout << "priors\n" << priors << "\n";

  // Only calculate prior for M if estimated (M_type = 1), otherwise stays 0 and
  // adds nothing to objective function
  if(M_type == 1) {
    prior_M += square(log_M - p_log_M) / (Type(2.0) * square(p_sigma_M));
  }

  // Catch:  normal (check)
  // for (int i = 0; i < nyr; i++) {
  //   catch_like += square( (data_catch(i) - pred_landed(i)) / pred_landed(i)) /
  //     (Type(2.0) * square(sigma_catch(i)));
  // }

  // Catch: lognormal
  // for (int i = 0; i < nyr; i++) {
  //   catch_like += square( log((data_catch(i) + c) / (pred_landed(i) + c)) )/
  //     Type(2.0) * square(sigma_catch(i));
  // }

  // Catch: lognormal alternative (these should be equivalent)
  for (int i = 0; i < nyr; i++) {
    catch_like += square( log(data_catch(i) + c) - log(pred_landed(i) + c) )/
      (Type(2.0) * square(sigma_catch(i)));
  }

  catch_like *= wt_catch;     // Likelihood weight
  // std::cout << "Catch likelihood\n" << catch_like << "\n";

  // Fishery CPUE: lognormal
  for (int i = 0; i < nyr_fsh_cpue; i++) {
    index_like(0) += square( log((data_fsh_cpue(i) + c) / (pred_fsh_cpue(i) + c)) ) /
      (Type(2.0) * square(sigma_fsh_cpue(i)));
  }
  index_like(0) *= wt_fsh_cpue; // Likelihood weight

  // Survey CPUE: lognormal
  for (int i = 0; i < nyr_srv_cpue; i++) {
    index_like(1) += square( log((data_srv_cpue(i) + c) / (pred_srv_cpue(i) + c)) ) /
      (Type(2.0) * square(sigma_srv_cpue(i)));
  }
  index_like(1) *= wt_srv_cpue; // Likelihood weight

  // Mark-recapture index: lognormal
  for (int i = 0; i < nyr_mr; i++) {
    index_like(2) += square( log((data_mr(i) + c) / (pred_mr(i) + c)) ) /
      (Type(2.0) * square(sigma_mr(i)));
  }
  index_like(2) *= wt_mr;        // Likelihood weight
  // std::cout << "Index likelihoods\n" << index_like << "\n";

  // Likelihood for fishery age compositions
  Type fsh_theta = exp(log_fsh_theta);      // Dirichlet-multinomial parameter
  vector<Type> neff_dm_fsh_age(nyr_fsh_age); //effective sample sizes calculated from DM
  //vector<Type> sum1_fsh(nyr_fsh_age);       // First sum in D-M likelihood (log of Eqn 10, Thorson et al. 2017)
  //vector<Type> sum2_fsh(nyr_fsh_age);       // Second sum in D-M likelihood (log of Eqn 10, Thorson et al. 2017)
  //fsh_theta.setZero();
  //sum1_fsh.setZero();
  //sum2_fsh.setZero();

  // Switch for composition likelihood (case 0 or 1 references the value of comp_type)
  switch (comp_type) {
  case 0: // Multinomial

    for (int i = 0; i < nyr_fsh_age; i++) {
      for (int j = 0; j < nage; j++) {

        // Offset
        offset(0) -= effn_fsh_age(i) * (data_fsh_age(i,j) + c) * log(data_fsh_age(i,j) + c);
        // Likelihood
        age_like(0) -= effn_fsh_age(i) * (data_fsh_age(i,j) + c) * log(pred_fsh_age(i,j) + c);
      }
    }

    age_like(0) -= offset(0);     // subtract offset
    age_like(0) *= wt_fsh_age;    // likelihood weight

    break;

  case 1: // Dirichlet-multinomial (D-M)

//    for (int i = 0; i < nyr_fsh_age; i++) {
      // Preliminary calcs
//      for (int j = 0; j < nage; j++) {
        // First sum in D-M likelihood (log of Eqn 10, Thorson et al. 2017)
//        sum1_fsh(i) += lgamma( n_fsh_age(i) * data_fsh_age(i,j) + Type(1.0) );
        // Second sum in D-M likelihood (log of Eqn 10, Thorson et al. 2017)
        //sum2_fsh(i) += lgamma( n_fsh_age(i) + data_fsh_age(i,j) + fsh_theta * n_fsh_age(i) * pred_fsh_age(i,j) ) -
        //  lgamma( fsh_theta * n_fsh_age(i) - pred_fsh_age(i,j) );
//        sum2_fsh(i) += lgamma( n_fsh_age(i) * data_fsh_age(i,j) + fsh_theta * n_fsh_age(i) * pred_fsh_age(i,j) ) -
//          lgamma( fsh_theta * n_fsh_age(i) * pred_fsh_age(i,j) );   // second n_fsh_age(i) should be big N in Thorso which is not specified an may be a typo? 
        //sum2_fsh(i) += lgamma( n_fsh_age(i) * data_fsh_age(i,j) + fsh_theta * bigN * pred_fsh_age(i,j) ) -
        //  lgamma( fsh_theta * n_fsh_age(i) * pred_fsh_age(i,j) );
//     }
      // Full nll for D-M, Eqn 10, Thorson et al. 2017
      //age_like(0) -= lgamma(n_fsh_age(i) + Type(1.0)) - sum1_fsh(i) + lgamma(fsh_theta * n_fsh_age(i)) -
      //  lgamma(n_fsh_age(i) + fsh_theta * n_fsh_age(i)) + sum2_fsh(i);
 //     age_like(0) -= lgamma(n_fsh_age(i) + Type(1.0)) - sum1_fsh(i) + lgamma(fsh_theta * n_fsh_age(i)) -
//        lgamma(n_fsh_age(i) + fsh_theta * n_fsh_age(i)) + sum2_fsh(i);

//    }

    vector<Type> obs_fa_vec( nage );
    vector<Type> pred_fa_vec( nage );
    Type sampsize_fa; 
      obs_fa_vec.setZero();
      pred_fa_vec.setZero();
    for (int i = 0; i < nyr_fsh_age; i++){
      obs_fa_vec = data_fsh_age(i);
      pred_fa_vec = pred_fsh_age(i);
      //sampsize_fa = n_fsh_age(i);
      sampsize_fa = effn_fsh_age(i);
      age_like(0) -= ddirmult(obs_fa_vec, pred_fa_vec, sampsize_fa, log_fsh_theta, true );
      neff_dm_fsh_age(i) = 1/(1+fsh_theta) + effn_fsh_age(i)*(fsh_theta/(1+fsh_theta));  //** need to implement this before done with dirichlet conversions
    }

    break;

    // case 2: // Multivariate logistic (MVL) - future development

    }

  // Likelihood for survey age compositions
  Type srv_theta = exp(log_srv_theta);      // Dirichlet-multinomial parameter
  //vector<Type> neff_dm_srv_age; //effective sample sizes calculated from DM
  //vector<Type> sum1_srv(nyr_srv_age);       // First sum in D-M likelihood (log of Eqn 10, Thorson et al. 2017)
  //vector<Type> sum2_srv(nyr_srv_age);       // Second sum in D-M likelihood (log of Eqn 10, Thorson et al. 2017)
  //srv_theta.setZero();
  //sum1_srv.setZero();
  //sum2_srv.setZero();

  // Switch for composition likelihood (case 0 or 1 references the value of comp_type)
  switch (comp_type) {

  case 0: // Multinomial

    for (int i = 0; i < nyr_srv_age; i++) {
      for (int j = 0; j < nage; j++) {
        // Offset
        offset(1) -= effn_srv_age(i) * (data_srv_age(i,j) + c) * log(data_srv_age(i,j) + c);
        // Likelihood
        age_like(1) -= effn_srv_age(i) * (data_srv_age(i,j) + c) * log(pred_srv_age(i,j) + c);
      }
    }
    age_like(1) -= offset(1);     // subtract offset
    age_like(1) *= wt_srv_age;    // likelihood weight

    break;

  case 1: // Dirichlet-multinomial (D-M)

//    for (int i = 0; i < nyr_srv_age; i++) {
      // Preliminary calcs
//      for (int j = 0; j < nage; j++) {
        // First sum in D-M likelihood (log of Eqn 10, Thorson et al. 2017)
//        sum1_srv(i) += lgamma( n_srv_age(i) * data_srv_age(i,j) + Type(1.0) );
        // Second sum in D-M likelihood (log of Eqn 10, Thorson et al. 2017)
        //sum2_srv(i) += lgamma( n_srv_age(i) + data_srv_age(i,j) + srv_theta * n_srv_age(i) * pred_srv_age(i,j) ) -
        //  lgamma( srv_theta * n_srv_age(i) - pred_srv_age(i,j) );
//        sum2_srv(i) += lgamma( n_srv_age(i) * data_srv_age(i,j) + srv_theta * n_srv_age(i) * pred_srv_age(i,j) ) -
//          lgamma( srv_theta * n_srv_age(i) * pred_srv_age(i,j) );   //switch - to * ??
//      }
      // Full nll for D-M, Eqn 10, Thorson et al. 2017
      //age_like(1) -= lgamma(n_srv_age(i) + Type(1.0)) - sum1_srv(i) + lgamma(srv_theta * n_srv_age(i)) -
      //  lgamma(n_srv_age(i) + srv_theta * n_srv_age(i)) + sum2_srv(i);
//      age_like(1) -= lgamma(n_srv_age(i) + Type(1.0)) - sum1_srv(i) + lgamma(srv_theta * n_srv_age(i)) -
//        lgamma(n_srv_age(i) + srv_theta * n_srv_age(i)) + sum2_srv(i);
//    }

    vector<Type> obs_sa_vec( nage );
    vector<Type> pred_sa_vec( nage );
    Type sampsize_sa; 
      obs_sa_vec.setZero();
      pred_sa_vec.setZero();
    for (int i = 0; i < nyr_srv_age; i++){
      obs_sa_vec = data_srv_age(i);
      pred_sa_vec = pred_srv_age(i);
      //sampsize_sa = n_srv_age(i);
      sampsize_sa = effn_srv_age(i);
      age_like(1) -= ddirmult(obs_sa_vec, pred_sa_vec, sampsize_sa, log_srv_theta, true );
      //neff_dm_srv_age(i) = 1/(1+srv_theta) + effn_fsh_age(i)*(srv_theta/(1+srv_theta));
    }

    break;

    // case 2: // Multivariate logistic - future development

  }


  // std::cout << "Age comp offset\n" << offset << "\n";
  // std::cout << "Age comp likelihoods\n" << age_like << "\n";

  // Fishery length comps.
 vector<Type> fsh_l_theta = exp(log_fsh_l_theta);   

switch (comp_type) {

  case 0: // Multinomial

    for (int k = 0; k < nsex; k++) {
      for (int i = 0; i < nyr_fsh_len; i++) {
        for (int l = 0; l < nlenbin; l++) {
        // Offset
        offset_fsh_len(k) -= effn_fsh_len(i,0,k) * (data_fsh_len(i,l,k) + c) * log(data_fsh_len(i,l,k) + c);
        // Likelihood
        fsh_len_like(k) -= effn_fsh_len(i,0,k) * (data_fsh_len(i,l,k) + c) * log(pred_fsh_len(i,l,k) + c);
        }
      }
    fsh_len_like(k) -= offset_fsh_len(k);     // subtract offset
    fsh_len_like(k) *= wt_fsh_len;            // likelihood weight
    }

  break;

case 1: // Dirchlet multinomial
  array<Type> temp_lenyr_obs(nyr_fsh_len,nlenbin);
  array<Type> temp_lenyr_pred(nyr_fsh_len,nlenbin);
  vector<Type> temp_ss(nyr_fsh_len);
  vector<Type> obs_fl_vec( nlenbin );
  vector<Type> pred_fl_vec( nlenbin );
  Type sampsize_fl; 
  obs_fl_vec.setZero();
  pred_fl_vec.setZero();

  for (int k = 0; k < nsex; k++) {
    temp_lenyr_obs.setZero();
    temp_lenyr_pred.setZero();
    temp_ss.setZero();
    for (int i = 0; i < nyr_fsh_len; i++){
      //temp_ss(i) = n_fsh_len(i,0,k);
      temp_ss(i) = effn_fsh_len(i,0,k);
    }
    for (int i = 0; i < nyr_fsh_len; i++){
      //temp_ss(i) = n_fsh_len(i,k);
      for (int l = 0; l < nlenbin; l++) {
        //extract sex specific length x year matrix
        temp_lenyr_obs(i,l) = data_fsh_len(i,l,k);
        temp_lenyr_pred(i,l) = pred_fsh_len(i,l,k);
      }
      obs_fl_vec = temp_lenyr_obs(i);
      pred_fl_vec = temp_lenyr_pred(i);
      sampsize_fl = temp_ss(i); //if this works should be able to get rid of little ss loop and just go with = n_fsh_len(i,0,k) ... maybe?
      fsh_len_like(k) -= ddirmult(obs_fl_vec, pred_fl_vec, sampsize_fl, log_fsh_l_theta(k), true );
      //fsh_len_like(k) -= ddirmult(obs_fl_vec, pred_fl_vec, sampsize_fl, log_fsh_l_theta(k), true );  
      //n_effective(YearI) = 1/(1+theta) + n_samp(YearI)*(theta/(1+theta));  //** need to implement this before done with dirichlet conversions
    }
  }

  break;

}
  
// Survey length comps
vector<Type> srv_l_theta = exp(log_srv_l_theta);

switch (comp_type) {
  case 0: // Multinomial

    // Multinomial likelihood for survey length comps.
  for (int k = 0; k < nsex; k++) {

    for (int i = 0; i < nyr_srv_len; i++) {
      for (int l = 0; l < nlenbin; l++) {
        // Offset
        offset_srv_len(k) -= effn_srv_len(i,0,k) * (data_srv_len(i,l,k) + c) * log(data_srv_len(i,l,k) + c);
        // Likelihood
        srv_len_like(k) -= effn_srv_len(i,0,k) * (data_srv_len(i,l,k) + c) * log(pred_srv_len(i,l,k) + c);
      }
    }

    srv_len_like(k) -= offset_srv_len(k);     // subtract offset
    srv_len_like(k) *= wt_srv_len;            // likelihood weight

  }

  break;

case 1: // Dirchlet multinomial
  array<Type> temp_lenyr_obs2(nyr_srv_len,nlenbin);
  array<Type> temp_lenyr_pred2(nyr_srv_len,nlenbin);
  vector<Type> temp_ss2(nyr_srv_len);
  vector<Type> obs_sl_vec( nlenbin );
  vector<Type> pred_sl_vec( nlenbin );
  Type sampsize_sl; 
  obs_sl_vec.setZero();
  pred_sl_vec.setZero();

  for (int k = 0; k < nsex; k++) {
    temp_lenyr_obs2.setZero();
    temp_lenyr_pred2.setZero();
    temp_ss2.setZero();
    for (int i = 0; i < nyr_srv_len; i++){
      //temp_ss2(i) = n_srv_len(i,0,k);
      temp_ss2(i) = effn_srv_len(i,0,k);
    }
    for (int i = 0; i < nyr_srv_len; i++){
      for (int l = 0; l < nlenbin; l++) {
        //extract sex specific length x year matrix
        temp_lenyr_obs2(i,l) = data_srv_len(i,l,k);
        temp_lenyr_pred2(i,l) = pred_srv_len(i,l,k);
      }
      obs_sl_vec = temp_lenyr_obs2(i);
      pred_sl_vec = temp_lenyr_pred2(i);
      sampsize_sl = temp_ss2(i); //if this works should be able to get rid of little ss loop and just go with = n_fsh_len(i,0,k) ... maybe?
      srv_len_like(k) -= ddirmult(obs_sl_vec, pred_sl_vec, sampsize_sl, log_srv_l_theta(k), true );
      //fsh_len_like(k) -= ddirmult(obs_fl_vec, pred_fl_vec, sampsize_fl, log_fsh_l_theta(k), true );  . 
      //n_effective(YearI) = 1/(1+theta) + n_samp(YearI)*(theta/(1+theta));  //** need to implement this before done with dirichlet conversions
    }
  }

  break;
}

  // Recruitment - random_rec switches between penalized likelihood and random
  // effects. Shared sigma_r between rinit_devs and rec_devs, rinit_devs are the
  // same as the rec_devs but have been reduced by mortality. They were kept
  // separate to help with accounting.

  // Penalized likelihood, sigma_r fixed, bias correction for lognormal
  // likelihood (-0.5*sigma^2) needed to get mean instead of median from
  // distribution
  if (random_rec == 0) {

    // Annual recruitment deviations
    for (int i = 0; i < nyr; i++) {
      rec_like += square(log_rec_devs(i) - Type(0.5) * square(sigma_r));
    }

    // Initial numbers-at-age (sage + 1 to plus group - 1)
    for (int j = 0; j < nage - 2; j++) {
      rec_like += square(log_rinit_devs(j) - Type(0.5) * square(sigma_r));
    }

    rec_like *= wt_rec_like;      // weight

  }

  // Random effects: mean = 0, sigma_r estimated, same bias correction as
  // penalized likelihood for lognormal distribution
  if (random_rec == 1) {

    // Recruitment deviations
    for (int i = 0; i < nyr; i++) {
      rec_like += log(sigma_r) + Type(0.5) * square(log_rec_devs(i) - Type(0.5) * square(sigma_r)) / square(sigma_r);
      // Should be equivalent to: rec_like -= dnorm(log_rec_dev(i) - Type(0.5) * square(sigma_r) , Type(0.0), sigma_r, true);
    }

    // Initial numbers-at-age (sage + 1 to plus group - 1)
    for (int j = 0; j < nage - 2; j++) {
      rec_like += log(sigma_r) + Type(0.5) * square(log_rinit_devs(j) - Type(0.5) * square(sigma_r)) / square(sigma_r);
    }

    rec_like *= wt_rec_like;      // weight
  }
  // std::cout << "Log recruitment deviations\n" << log_rec_devs << "\n";
  // std::cout << "Log deviations for initial numbers-at-age\n" << log_rinit_devs << "\n";
  // std::cout << "Recruitment likelihood\n" << rec_like << "\n";

  // Regularity penalty on fishing mortality
  for (int i = 0; i < nyr; i++) {
    fpen += square(log_F_devs(i));
  }
  fpen *= wt_fpen;              // weight

  // Large penalty on SPR calculations
  for(int x = 1; x <= n_Fxx; x++) {
    spr_pen += wt_spr * square(SBPR(x) / SBPR(0) - Fxx_levels(x-1));
  }
  // std::cout << "Log fishing mortality deviations\n" << log_F_devs << "\n";
  // std::cout << "Penality for fishing mortality\n" << fpen << "\n";
  // std::cout << "Penality for SPR calcs\n" << spr_pen << "\n";

  // Sum likelihood components
  obj_fun += priors(0);         // Fishery q
  obj_fun += priors(1);         // Survey q
  obj_fun += priors(2);         // Mark-recapture abndance index q
  obj_fun += prior_M;           // prior for natural mortality (if fixed this is 0)
  obj_fun += catch_like;        // Catch
  obj_fun += index_like(0);     // Fishery cpue
  obj_fun += index_like(1);     // Survey cpue
  obj_fun += index_like(2);     // Mark-recapture abundance index
  obj_fun += age_like(0);       // Fishery age compositions
  obj_fun += age_like(1);       // Survey age compositions
  for (int k = 0; k < nsex; k++) {
    obj_fun += fsh_len_like(k); // Fishery length comps
    obj_fun += srv_len_like(k); // Survey length comps
  }
  obj_fun += rec_like;          // Recruitment deviations
  obj_fun += fpen;              // Fishing mortality deviations
  obj_fun += spr_pen;           // SPR calculations

  // std::cout << "Objective function\n" << obj_fun << "\n";

  // obj_fun = dummy*dummy;        // Uncomment when debugging code

  // REPORT SECTION

  // Predicted indices of catch and abundance
  REPORT(pred_catch);       // Total catch
  REPORT(pred_landed);      // Landed catch
  REPORT(pred_wastage);     // Discarded catch assumed to die
  REPORT(pred_mr);          // Mark-recapture index of abundance (only years with an estimate)
  REPORT(pred_mr_all);      // Mark-recapture index of abundance (all years)
  REPORT(pred_fsh_cpue);    // Fishery cpue
  REPORT(pred_srv_cpue);    // Survey cpue

  // Predicted compositions
  REPORT(pred_fsh_age);     // Fishery
  REPORT(pred_srv_age);     // Survey
  REPORT(pred_fsh_len);     // Fishery
  REPORT(pred_srv_len);     // Survey

  // Predicted selectivity-at-age
  REPORT(fsh_slx);          // Fishery
  REPORT(srv_slx);          // Survey

  // Predicted annual fishing mortality
  REPORT(Fmort);

  // Derived matrices by year and age
  REPORT(N);                // Abundance-at-age, projected 1 year forward
  REPORT(Nlen);             // Abundance-at-length, projected 1 year forward
  REPORT(Z);                // Total mortality
  REPORT(F);                // Fishing mortality
  REPORT(S);                // Survivorship
  REPORT(C);                // Catch in numbers at age

  // Derived vectors by year
  ADREPORT(pred_rec);         // Predicted age-2 recruitment
  REPORT(pred_rec);         // Predicted age-2 recruitment
  REPORT(tot_biom);         // Total age-2+ biomass
  REPORT(tot_expl_biom);    // Vulnerable biomass to fishery at the beginning of the fishery
  REPORT(tot_expl_abd);     // Vulnerable abundance to fishery at the beginning of the fishery
  REPORT(tot_vuln_abd);     // Vulnerable abundance to survey at the beginning of the survey
  REPORT(tot_spawn_biom);   // Female spawning biomass

  // Derived arrays by year, age, sex
  REPORT(biom);             // Total age-2+ biomass
  REPORT(expl_biom);        // Vulnerable biomass to fishery at the beginning of the fishery
  REPORT(expl_abd);         // Vulnerable abundance to fishery at the beginning of the fishery
  REPORT(vuln_abd);         // Vulnerable abundance to survey at the beginning of the survey
  REPORT(spawn_biom);       // Spawning biomass

  // // SPR-based biological reference points and ABC
  REPORT(Fxx);              // Vector of Fs scaled to fully selected values
  REPORT(sel_Fxx);          // Fishery selectivity used in ABC calcs
  REPORT(SBPR);             // Vector of spawning biomass per recruit at various Fxx levels
  REPORT(mean_rec);         // Mean recruitment assumed to be equilibrium recruitment
  REPORT(SB);               // Vector of spawning biomass at various Fxx levels
  REPORT(ABC);              // ABC at various Fxx levels
  REPORT(wastage);          // Dead discarded catch at various Fxx levels

  // Other derived and projected values
  REPORT(survival_srv);     // Survival at time of survey
  REPORT(survival_fsh);     // Survival at time of fishery
  REPORT(survival_spawn);   // Survival at time of spawning
  REPORT(pred_rbar);        // Predicted mean recruitment

  // Priors, likelihoods, offsets, and penalty functions
  REPORT(priors);           // q priors
  REPORT(prior_M);          // M prior
  REPORT(catch_like);       // Catch
  REPORT(index_like);       // Abundance indices
  REPORT(age_like);         // Age compositions
  REPORT(srv_len_like);     // Survey length composition likelihoods
  REPORT(fsh_len_like);     // Fishery length composition likelihoods
  REPORT(rec_like);         // Recruitment deviations
  REPORT(fpen);             // Fishing mortality deviations
  REPORT(spr_pen);          // SPR penalty
  REPORT(obj_fun);          // Total objective function
  REPORT(offset);           // Offsets for age comp multinomial
  REPORT(offset_srv_len);   // Offsets for survey length comp multinomial
  REPORT(offset_fsh_len);   // Offsets for fishery length comp multinomial
  REPORT(fsh_theta);
  REPORT(neff_dm_fsh_age);
  REPORT(srv_theta);
  REPORT(fsh_l_theta);
  REPORT(srv_l_theta);

  return(obj_fun);          
  
  }
