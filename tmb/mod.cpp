#include <TMB.hpp>
#include <numeric>

template <class Type> Type square(Type x){return x*x;}

// template <class Type> Type sumVec(vector<Type> vec){return std::accumulate(vec.begin(), vec.end(), 0.0);}

// template <class Type> Type mean(vector<Type> vec){return std::accumulate(vec.begin(), vec.end(), 0.0) / vec.size();}

template<class Type>
  Type objective_function<Type>::operator() ()
{
  // **DATA SECTION**
  
  // Model dimensions
  DATA_INTEGER(nyr)             // number of years in the model (year indexed as i)
  DATA_INTEGER(nage)            // number of ages in the model (age indexed as j)
  DATA_INTEGER(nsex)            // controls whether model is sex-structured or not (sex indexed as k)
  
  // Switch for recruitment estimate: 0 = penalized likelihood (fixed sigma_r), 1
   // = random effects
  DATA_INTEGER(random_rec)
    
  // Switch for selectivity type: 0 = a50, a95 logistic; 1 = a50, slope logistic
  DATA_INTEGER(slx_type)
    
  // Time varying parameter blocks - each vector contains the terminal years of
  // each time block
  DATA_IVECTOR(blks_fsh_slx)    // fishery selectivity 
  DATA_IVECTOR(blks_srv_slx)    // survey selectivity 
    
  // Fixed parameters
  DATA_ARRAY(M)                 // natural mortality by year, age, and sex. assume constant M=0.1

  // Fxx levels that correspond with spr_Fxx in Parameter section
  DATA_VECTOR(Fxx_levels)       // e.g. F35=0.35, F40=0.40, and F50=0.50
  
  // Priors ("p_" denotes prior)
  DATA_SCALAR(p_fsh_q)          // prior fishery catchability coefficient (on natural scale)
  DATA_SCALAR(sigma_fsh_q)      // sigma for fishery q
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
  DATA_SCALAR(wt_rec_like)      // penalty on recruitment deviations
  DATA_SCALAR(wt_fpen)          // penality on fishing mortality deviations
  DATA_SCALAR(wt_spr)           // penalty on spawner per recruit calcs

  // INDICES OF ABUNDANCE
  
  // Catch
  DATA_VECTOR(data_catch)       // vector of catch estimates
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
    
  // Sex ratio in survey (all years combined). First row males at age, second
  // row females at age.
  DATA_MATRIX(sex_ratio)
  
  // Weight-at-age (rows = 1, all years combined; cols = nage; matrices = 0 for
  // males, 1 for females); the rows could one day represent annual or
  // time-varying weight-at-age
  DATA_ARRAY(data_srv_waa)       // Survey (sex-specific)
  //std::cout << data_srv_waa << "\n";
    
  // Fishery age comps
  DATA_INTEGER(nyr_fsh_age)       // number of years 
  DATA_IVECTOR(yrs_fsh_age)       // vector of years
  DATA_MATRIX(data_fsh_age)       // matrix of observations (year, age)
  DATA_VECTOR(effn_fsh_age)       // effective sample size
  
  // Survey age comps
  DATA_INTEGER(nyr_srv_age)       // number of years
  DATA_IVECTOR(yrs_srv_age)       // vector of years
  DATA_MATRIX(data_srv_age)       // matrix of observations (year, age)
  DATA_VECTOR(effn_srv_age)       // effective sample size
    
  // Ageing error transition matrix (proportion at reader age given true age)
  DATA_MATRIX(ageing_error)

  // **PARAMETER SECTION**
  
  // Dummy variable for troubleshooting 
  PARAMETER(dummy);          

  // Selectivity 
  PARAMETER_ARRAY(fsh_slx_pars);       // Fishery selectivity (slx_type controls parameterization)
  PARAMETER_ARRAY(srv_slx_pars);       // Survey selectivity (slx_type controls parameterization)

  // Catchability
  PARAMETER(fsh_logq);            // fishery       
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
  PARAMETER_VECTOR(spr_Fxx);      // e.g. F35, F40, F50
  
  // **DERIVED QUANTITIES**
  
  // Predicted indices of abundance
  vector<Type> pred_catch(nyr);                 // Catch
  vector<Type> pred_mr(nyr_mr);                 // Mark-recapture index of abundance (only years with an estimate)
  vector<Type> pred_mr_all(nyr);                // Mark-recapture index of abundance (all years!)
  vector<Type> pred_fsh_cpue(nyr_fsh_cpue);     // Fishery cpue
  vector<Type> pred_srv_cpue(nyr_srv_cpue);     // Survey cpue

  // Predicted age compositions
  matrix<Type> pred_fsh_age(nyr_fsh_age, nage);  // Fishery 
  matrix<Type> pred_srv_age(nyr_srv_age, nage);  // Survey
  
  // Predicted selectivity
  array<Type> fsh_slx(nyr, nage, nsex);           // Fishery selectivity-at-age by sex (on natural scale)
  array<Type> srv_slx(nyr, nage, nsex);           // Survey selectivity-at-age by sex(on natural scale)

  // Predicted annual fishing mortality
  vector<Type> Fmort(nyr);      // On natural scale
  
  // Derived matrices by year, age, and sex
  array<Type> N(nyr+1, nage, nsex);   // Abundance-at-age, projected 1 year forward
  array<Type> Z(nyr, nage, nsex);     // Total mortality
  array<Type> F(nyr, nage, nsex);     // Fishing mortality
  array<Type> S(nyr, nage, nsex);     // Survivorship
  array<Type> C(nyr, nage, nsex);     // Catch in numbers

  // Derived time series of recruitment, biomass, and abundance (+ projected values)
  
  vector<Type> pred_rec(nyr);               // Predicted age-2 recruitment
  
  array<Type> biom(nyr+1, nage, nsex);      // Biomass by year, age, and sex, projected 1 year forward
  vector<Type> tot_biom(nyr+1);             // Summed over age and sex
  biom.setZero();
  tot_biom.setZero();
  
  array<Type> expl_biom(nyr+1, nage, nsex); // Vulnerable biomass to fishery at the beginning of the fishery, projected 1 year forward
  vector<Type> tot_expl_biom(nyr+1);        // Summed over age and sex
  expl_biom.setZero();
  tot_expl_biom.setZero();
  
  array<Type> vuln_abd(nyr+1, nage, nsex);  // Vulnerable abundance to survey at the beginning of the survey, projected 1 year forward
  vector<Type> tot_vuln_abd(nyr+1);          // Summed over age and sex
  vuln_abd.setZero();
  tot_vuln_abd.setZero();
  
  matrix<Type> spawn_biom(nyr+1, nage);     // Spawning biomass, just females
  vector<Type> tot_spawn_biom(nyr+1);       // Summed over age
  spawn_biom.setZero();
  tot_spawn_biom.setZero();

  // Other derived and projected values
  array<Type> survival_srv(nyr, nage, nsex);      // Annual natural survival at time of survey
  array<Type> survival_fsh(nyr, nage, nsex);      // Annual natural survival at time of fishery
  array<Type> survival_spawn(nyr, nage, nsex);    // Annual natural survival at time of spawning
  Type pred_rbar;                                 // Predicted mean recruitment
  Type sigma_r = exp(log_sigma_r);                // Estimated recruitment on natural scale 
  
  // SPR-based reference points
  int n_Fxx = Fxx_levels.size();        // Number of Fs estimated
  vector<Type> Fxx(n_Fxx + 1);          // Separate Fxx vector that is scaled to fully selected values (+1 includes F=0)
  matrix<Type> Nspr(n_Fxx + 1, nage);   // Matrix of spawning numbers-at-age *FLAG* number of rows = number of estimated F_xx% (e.g. 3 = F35,F40,F50)
  vector<Type> SBPR(n_Fxx + 1);         // Spawning biomass per recruit at various fishing levels
  vector<Type> SB(n_Fxx + 1);           // Equilibrium spawning biomass at various fishing levels
  Nspr.setZero();
  SBPR.setZero();
  SB.setZero();
  
  // ABC calculation
  matrix<Type> sel_Fxx(n_Fxx, nage);    // Age-specific fully-selected fishing mortality at each Fxx%
  matrix<Type> Z_Fxx(n_Fxx, nage);      // Total mortality at each Fxx%
  matrix<Type> S_Fxx(n_Fxx, nage);      // Total survivorship at each Fxx%
  vector<Type> ABC(n_Fxx);              // ABCs at each F_xx%
  ABC.setZero();
  
  // Priors, likelihoods, offsets, and penalty functions
  vector<Type> priors(3);       // Priors for catchability coefficients
  priors.setZero();
  
  vector<Type> offset(2);       // Offset for multinomial distribution that lets likelihood equal zero when obs = pred
    
  Type catch_like = 0;          // Catch  
  Type rec_like = 0;            // Recruitment
  Type fpen = 0;                // Penality for Fmort regularity
  Type spr_pen = 0;             // Penality for SPR-based reference points
  
  vector<Type> index_like(3);   // Fishery cpue, survey cpue, MR estimates 
  index_like.setZero();

  vector<Type> age_like(2);     // Fishery and survey age comps
  age_like.setZero();
  
  Type obj_fun = 0;             // Objective function
  
  Type c = 0.00001;             // Tiny constant to prevent likelihoods from going to 0
  
  // **MODEL**
  
  // Indexing: i = year, j = age, k = sex, h = time block, x = Fxx level
  
  // Fishery selectivity

  // The do while allows you to estimate parameters within a time block. It
  // "does" the looping over year and age "while" within the time block, then
  // iterates to the next block. Year is not in a for loop because it is
  // iterated by the do statement.
  
  // The switch for slx_type allows you to change parameterization. This could
  // easily be expanded to accomodate any selectivity type (the fsh_slx_pars
  // allows for a flexible number of parameters and time blocks)
  
  int i = 0;
  
  for(int h = 0; h < blks_fsh_slx.size(); h++){
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
            fsh_slx(i,j,k) = Type(1.0) / ( Type(1.0) + exp(j - fsh_slx_pars(h,0,k)) * fsh_slx_pars(h,1,k) );
            break;  
          }
        }
      }
      i++;
    } while (i <= blks_fsh_slx(h));
  }
    
  // Survey selectivity - see notes on syntax in fishery selectivity section
  
  i = 0;     // re-set i to 0 (do not redeclare)
  
  for(int h = 0; h < blks_srv_slx.size(); h++){
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
            srv_slx(i,j,k) = Type(1.0) / ( Type(1.0) + exp(j - srv_slx_pars(h,0,k)) * srv_slx_pars(h,1,k) );
            break;  
          }
        }
      }
      i++;
    } while (i <= blks_srv_slx(h));
  }
  
  // std::cout << fsh_slx << "\n Fishery selectivity";
  // std::cout << srv_slx << "\n Survey selectivity";

  // Mortality and survivorship
  
  for (int i = 0; i < nyr; i++) {
    Fmort(i) = exp(log_Fbar + log_F_devs(i)); // Annual fishing mortality
  }
  
  for (int k = 0; k < nsex; k++) {
    for (int i = 0; i < nyr; i++) {
      for (int j = 0; j < nage; j++) {
        
      // Fishing mortality by year, age, and sex
      F(i,j,k) = Fmort(i) * fsh_slx(i,j,k);

      // Total mortality by year, age, and sex
      Z(i,j,k) = M(i,j,k) + F(i,j,k);

      // Survivorship by year, age, and sex
      S(i,j,k) = exp(-1.0 * Z(i,j,k));
      }
    }
  }

  // std::cout << Fmort << "\n";
  // std::cout << F << "\n";
  // std::cout << Z << "\n";
  // std::cout << S << "\n";

  // Annual natural survival at time of survey, fishery, and spawning (fraction
  // surviving from beginning of year to the time of survey and fishery). These
  // quantities have the flexibility to vary by year, age, or sex, but currently
  // do not.
  for (int k = 0; k < nsex; k++) {
    for (int i = 0; i < nyr; i++) {
      for (int j = 0; j < nage; j++) {
        survival_srv(i,j,k) = exp(-1.0 * srv_month * M(i,j,k));
        survival_fsh(i,j,k) = exp(-1.0 * fsh_month * M(i,j,k));
        survival_spawn(i,j,k) = exp(-1.0 * spawn_month * M(i,j,k));
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
      N(0,j,k) = exp(log_rinit - M(0,j,k) * Type(j) + log_rinit_devs(j-1)) * sex_ratio(k,j);
    }
  }

  // Start year: plus group *FLAG* sex ratio from survey or 50/50 or ?
  for (int k = 0; k < nsex; k++) {
    N(0,nage-1,k) = exp(log_rinit - M(0,nage-1,k) * Type(nage-1)) / (1 - exp(-M(0,nage-1,k))) * sex_ratio(k,nage-1);
  }
  
  // Recruitment in all years (except the projected year) *FLAG* sex ratio from
  // survey or 50/50 or ?
  for (int k = 0; k < nsex; k++) {
    for (int i = 0; i < nyr; i++) {
      N(i,0,k) = exp(log_rbar + log_rec_devs(i)) * sex_ratio(k,0);
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
      N(nyr,0,k) += exp(log_rbar + log_rec_devs(i)) * sex_ratio(k,0); 
    }
    N(nyr,0,k) /= Type(15.0);
  }

  // FLAG - Alternative way using mean(). Want to find a way to sum() over
  // vector
  // vector<Type> tmp = exp(log_rbar + log_rec_devs(nyr+nage-3-16, nyr+nage-3-2));
  // N(nyr,0) = mean(tmp);

  // std::cout << N << "\n";

  // Catch

  // Baranov catch eqn - Catch in sex-specific numbers-at-age (F already
  // incorporates fishery selectivity-at-age)
  for (int k = 0; k < nsex; k++) {
    for (int i = 0; i < nyr; i++) {
      for (int j = 0; j < nage; j++) {
        C(i,j,k) = N(i,j,k) * F(i,j,k) * (1 - S(i,j,k)) / Z(i,j,k);
      }
    }
  }
  // std::cout << C << "\n";

  pred_catch.setZero(); // Initialize

  // Predicted annual catch
  for (int k = 0; k < nsex; k++) {
    for (int i = 0; i < nyr; i++) {
      for (int j = 0; j < nage; j++) {
        // The 0 in data_srv_waa(0,j,k) is a place holder if you ever wanted
        // time-varying weight-at-age
        pred_catch(i) += C(i,j,k) * data_srv_waa(0,j,k) ;  // / 1e3 in mt
      }
    }
  }
  // std::cout << C << "\n";
  // std::cout << pred_catch << "\n";

  // Predicted recruitment by year, summed over the sexes
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
        
        // Vulnerable biomass to the fishery at the beginning of the fishery
        expl_biom(i,j,k) = data_srv_waa(0,j,k) * fsh_slx(i,j,k) * N(i,j,k) * survival_fsh(i,j,k);
        
        // Vulnerable abundance to the survey at the beginning of the survey
        vuln_abd(i,j,k) = srv_slx(i,j,k) * N(i,j,k) * survival_srv(i,j,k);
        
        // Spawning biomass (just females)
        if (k == 1) {
          spawn_biom(i,j) = data_srv_waa(0,j,k) * N(i,j,k) * survival_spawn(i,j,k) * prop_mature(0,j); 
        }
        
      }
    }
  }
  
  // Project those values into the next year
  for (int k = 0; k < nsex; k++) {
    for (int j = 0; j < nage; j++) {
      biom(nyr,j,k) = data_srv_waa(0,j,k) * N(nyr,j,k) * survival_srv(nyr-1,j,k);
      expl_biom(nyr,j,k) = data_srv_waa(0,j,k) * fsh_slx(nyr-1,j,k) * N(nyr,j,k) * survival_fsh(nyr-1,j,k);
      vuln_abd(nyr,j,k) = srv_slx(nyr-1,j,k) * N(nyr,j,k) * survival_srv(nyr-1,j,k);
      if (k == 1) { // just for females
        spawn_biom(nyr,j) = data_srv_waa(0,j,k) * N(nyr,j,k) * survival_spawn(nyr-1,j,k) * prop_mature(0,j); 
      }
    }
  }
  // std::cout << "Predicted biomass by age and sex \n" << biom << "\n";
  // std::cout << "Predicted exploited biomass by age and sex \n" << expl_biom << "\n";
  // std::cout << "Predicted vulnerable abundance by age and sex \n" << vuln_abd << "\n";
  // std::cout << "Predicted spawning biomass by age\n" << spawn_biom << "\n";

  // Sum variables to get an annual total
  for (int k = 0; k < nsex; k++) {
    for (int i = 0; i <= nyr; i++) {    // include projection year
      for (int j = 0; j < nage; j++) {
        
        // Total biomass at time of longline survey
        tot_biom(i) += biom(i,j,k);
        
        // Vulnerable biomass to the fishery at the beginning of the fishery
        tot_expl_biom(i) += expl_biom(i,j,k);
        
        // Vulnerable abundance to the survey at the beginning of the survey
        tot_vuln_abd(i) += vuln_abd(i,j,k);
        
        // Spawning biomass (just females)
        if (k == 1) {
          tot_spawn_biom(i) += spawn_biom(i,j); 
        }
        
      }
    }
  }
  // std::cout << "Annual predicted biomass \n" << tot_biom << "\n";
  // std::cout << "Annual predicted exploited biomass\n" << tot_expl_biom << "\n";
  // std::cout << "Annual predicted vulnerable abundance\n" << tot_vuln_abd << "\n";
  // std::cout << "Annual predicted spawning biomass\n" << tot_spawn_biom << "\n";
  
  // Predicted values

  // Mark-recapture catchability and predicted abundance (in millions)
  Type mr_q = exp(mr_logq);

  for (int i = 0; i < nyr_mr; i++) {
    pred_mr(i) = mr_q * tot_vuln_abd(yrs_mr(i)); //  / 1e6 Just in years with a MR estimate
  }
  // std::cout << "Predicted MR \n" << pred_mr << "\n";

  for (int i = 0; i < nyr; i++) {
    pred_mr_all(i) = mr_q * tot_vuln_abd(i); // / 1e6 All years
  }
  // std::cout << "Predicted MR for all years\n" << pred_mr_all << "\n";

  // Fishery catchability and predicted cpue
  Type fsh_q = exp(fsh_logq);

  for (int i = 0; i < nyr_fsh_cpue; i++) {
    pred_fsh_cpue(i) = fsh_q * tot_expl_biom(yrs_fsh_cpue(i));
  }
  // std::cout << "Predicted fishery cpue\n" << pred_fsh_cpue << "\n";

  // Survey catchability and predicted survey cpue
  Type srv_q = exp(srv_logq);

  for (int i = 0; i < nyr_srv_cpue; i++) {
    pred_srv_cpue(i) = srv_q * tot_vuln_abd(yrs_srv_cpue(i));
  }
  // std::cout << "Predicted srv cpue\n" << pred_srv_cpue << "\n";

  // Predicted fishery age compositions - *FLAG* check on sample sizes by year and sex
  for (int i = 0; i < nyr_fsh_age; i++) {
    
    // sumC = temporary variable, catch in numbers summed over age and sex in a
    // given year
    Type sumC = 0; 
    for (int k = 0; k < nsex; k++) {
      for (int j = 0; j < nage; j++) {
        sumC += C(yrs_fsh_age(i),j,k);
      }
    }
    // sumC_age = temporary vector of catch in numbers by age in a given year
    // (combine sexes since we currently do not have sex-structured age comps)
    vector<Type> sumC_age(nage);
    sumC_age.setZero();
    for (int k = 0; k < nsex; k++) {
      for (int j = 0; j < nage; j++) {
        sumC_age(j) += C(yrs_fsh_age(i),j,k);
      }
    }
    
    // Get predicted age comps (proportions-at-age) and apply ageing error
    // matrix
    for (int j = 0; j < nage; j++) {
      pred_fsh_age(i,j) = sumC_age(j) / sumC;
      // pred_fsh_age(i,j) *= ageing_error(j,j);
    }
    
    // Loop over each year i
  }
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
    vector<Type> sumN_age(nage);
    sumN_age.setZero();
    for (int k = 0; k < nsex; k++) {
      for (int j = 0; j < nage; j++) {
        sumN_age(j) += vuln_abd(yrs_srv_age(i),j,k);
      }
    }
    // Get predicted age comps (proportions-at-age) and apply ageing error
    // matrix
    for (int j = 0; j < nage; j++) {
      pred_srv_age(i,j) = sumN_age(j) / tot_vuln_abd(yrs_srv_age(i));
     // pred_srv_age(i,j) *= ageing_error(j,j);    
    }
    // Loop over each year i
  }

  // Test do the predicted age comps sum to 1
  // vector<Type> tst(nyr_srv_age);
  // for (int i = 0; i < nyr_srv_age; i++) {
  //   for (int j = 0; j < nage; j++) {
  //     tst(i) += pred_srv_age(i,j);
  //   }
  // }
  // std::cout << "Do the comps sum to 1?\n" << tst << "\n";
  // 
  // Type tst_n = tst.size();
  // std::cout << "Length of tst vector\n" << tst_n << "\n";

  // FLAG - would like to know the TMB syntax to do the age comps in fewer
  // lines, for example:
  // for (int i = 0; i < nyr_srv_age; i++) {
  //   for (int j = 0; j < nage; j++) {
  //     pred_srv_age(i,j) = N(yrs_srv_age(i),j) * srv_slx(j) / sum(N(yrs_srv_age(i)) * srv_slx(j));
  //   }
  // }

  // Compute SPR rates and spawning biomass under different Fxx levels - *FLAG* only do this for female

  // // Use selectivity in most recent time block for all
  // // calculations.
  // vector<Type> spr_fsh_slx(nage); // *FLAG* only grab female!!!
  // spr_fsh_slx = fsh_slx.row(nyr-1);
  // // std::cout << "spr fishery selectivity\n" << spr_fsh_slx << "\n";
  // 
  // Fxx(0) = 0;     // No fishing
  // 
  // // Scale Fxx to fully selected values (not necessary for logistic selectivity
  // // b/c max is 1 but may be needed for other selectivity types in future
  // // development).
  // for(int x = 1; x <= n_Fxx; x++) {
  //   Fxx(x) = spr_Fxx(x-1) * max(spr_fsh_slx);
  // }
  // // std::cout << "Fxx\n" << Fxx << "\n";
  // 
  // // Populate numbers of potential spawners at age matrix
  // for(int x = 0; x <= n_Fxx; x++) {
  // 
  //   Nspr(x,0) = Type(1.0);    // Initialize SPR with 1
  // 
  //   // Survival equation by age
  //   for(int j = 1; j < nage - 1; j++) {
  //     Nspr(x,j) = Nspr(x,j-1) * exp(-1.0 * Fxx(x) * spr_fsh_slx(j-1) + M);
  //   }
  // 
  //   // Plus group
  //   Nspr(x,nage-1) = Nspr(x,nage-2) * exp(-1.0 * Fxx(x) * spr_fsh_slx(nage-2) + M) /
  //     (1.0 - exp(-1.0 * Fxx(x) * spr_fsh_slx(nage-1) + M));
  // }
  // // std::cout << "Number of spawners\n" << Nspr << "\n";
  // 
  // // Spawning biomass per recruit matrix
  // for(int x = 0; x <= n_Fxx; x++) {
  //   for(int j = 0; j < nage; j++) {
  //     SBPR(x) +=  Nspr(x,j) * prop_fem(j) * prop_mature(j) * data_srv_waa(0,j,0) * survival_spawn; // *FLAG* get rid of prop_fem
  //   }
  // }
  // // std::cout << "Spawning biomass per recruit\n" << SBPR << "\n";
  // 
  // // Virgin spawning biomass (no fishing), assuming average recruitment
  // SB(0) = SBPR(0) * exp(log_rbar); // *FLAG* proportion female or multiply by 0.5?
  // 
  // // Spawning biomass as a fraction of virgin spawning biomass
  // for(int x = 1; x <= n_Fxx; x++) {
  //   SB(x) = Fxx_levels(x-1) * SB(0);
  // }
  // // std::cout << "Spawning biomass\n" << SB << "\n";
  // 
  // // Get Allowable Biological Catch for different Fxx levels: *FLAG* all of
  // // these should be sex-structured, then final ABC will be summed across sexes
  // for(int x = 0; x < n_Fxx; x++) {
  //   // Preliminary calcs
  //   for(int j = 0; j < nage; j++) {
  //     sel_Fxx(x,j) = Fxx(x+1) * spr_fsh_slx(j);   // Fully selected fishing mortality at age
  //     Z_Fxx(x,j) = M + sel_Fxx(x,j);              // Total instantaneous mortality at age
  //     S_Fxx(x,j) = exp(-Z_Fxx(x,j));              // Total survival at age
  //   }
  //   // ABC calculation 
  //   for(int j = 0; j < nage; j++) {
  //     ABC(x) += data_srv_waa(0,j,0) * sel_Fxx(x,j) / Z_Fxx(x,j) * N(nyr, j) * (1.0 - S_Fxx(x,j));
  //   }
  // }
  // // std::cout << "ABC\n" << ABC << "\n";
  // 
  // // Priors
  // 
  // // Fishery cpue catchability coefficient
  // priors(0) = square( log(fsh_q / p_fsh_q) ) / ( 2 * square(sigma_fsh_q) ); 
  // 
  // // Survey catchability coefficient
  // priors(1) = square( log(srv_q / p_srv_q) ) / ( 2 * square(sigma_srv_q) );
  // 
  // // Mark-recapture abundance estimate catchability coefficient
  // priors(2) = square( log(mr_q / p_mr_q) ) / ( 2 * square(sigma_mr_q) );
  // 
  //  // std::cout << "priors\n" << priors << "\n";
  // 
  // // Catch: normal (check?)
  // // for (int i = 0; i < nyr; i++) {
  // //   catch_like += square( (data_catch(i) - pred_catch(i)) / pred_catch(i)) /
  // //     Type(2.0) * square(sigma_catch(i));
  // // }
  // 
  // // // Catch: lognormal - *FLAG* pred_catch will be summed across sexes
  // for (int i = 0; i < nyr; i++) {
  //   catch_like += square( log((data_catch(i) + c) / (pred_catch(i) + c)) )/
  //     Type(2.0) * square(sigma_catch(i));
  // }
  // 
  // // Catch: lognormal2
  // // for (int i = 0; i < nyr; i++) {
  // //   catch_like += square( log(data_catch(i) + c) - log(pred_catch(i) + c) )/
  // //     Type(2.0) * square(sigma_catch(i));
  // // }
  // catch_like *= wt_catch;     // Likelihood weight
  // // std::cout << "Catch likelihood\n" << catch_like << "\n";
  // 
  // // Fishery CPUE: lognormal
  // for (int i = 0; i < nyr_fsh_cpue; i++) {
  //   index_like(0) += square( log((data_fsh_cpue(i) + c) / (pred_fsh_cpue(i) + c)) ) /
  //     Type(2.0) * square(sigma_fsh_cpue(i));
  // }
  // index_like(0) *= wt_fsh_cpue; // Likelihood weight
  // 
  // // Survey CPUE: lognormal
  // for (int i = 0; i < nyr_srv_cpue; i++) {
  //   index_like(1) += square( log((data_srv_cpue(i) + c) / (pred_srv_cpue(i) + c)) ) /
  //     Type(2.0) * square(sigma_srv_cpue(i));
  // }
  // index_like(1) *= wt_srv_cpue; // Likelihood weight
  // 
  // // Mark-recapture index: lognormal
  // for (int i = 0; i < nyr_mr; i++) {
  //   index_like(2) += square( log((data_mr(i) + c) / (pred_mr(i) + c)) ) /
  //     Type(2.0) * square(sigma_mr(i));
  // }
  // index_like(2) *= wt_mr;        // Likelihood weight
  // // std::cout << "Index likelihoods\n" << index_like << "\n";
  // 
  // // Offset for fishery age compositions - *FLAG* the age comps should be sex-structured, i.e. separate likelihood for each sex
  // for (int i = 0; i < nyr_fsh_age; i++) {
  //   offset(0) -= effn_fsh_age(i) * (data_fsh_age(i) + c) * log(data_fsh_age(i) + c);
  // }
  // 
  // // Fishery age compositions: multinomial
  // for (int i = 0; i < nyr_fsh_age; i++) {
  //   for (int j = 0; j < nage; j++) {
  //     age_like(0) -= effn_fsh_age(i) * (data_fsh_age(i,j) + c) * log(pred_fsh_age(i,j) + c);
  //     }
  // }
  // age_like(0) -= offset(0);     // subtract offset
  // age_like(0) *= wt_fsh_age;    // likelihood weight
  // 
  // // Offset for survey age compositions
  // for (int i = 0; i < nyr_srv_age; i++) {
  //   offset(1) -= effn_srv_age(i) * (data_srv_age(i) + c) * log(data_srv_age(i) + c);
  // }
  // 
  // // Survey age compositions: multinomial
  // for (int i = 0; i < nyr_srv_age; i++) {
  //   for (int j = 0; j < nage; j++) {
  //     age_like(1) -= effn_srv_age(i) * (data_srv_age(i,j) + c) * log(pred_srv_age(i,j) + c);
  //   }
  // }
  // age_like(1) -= offset(1);     // substract offset
  // age_like(1) *= wt_srv_age;    // likelihood weight
  // 
  // // std::cout << "Age comp offset\n" << offset << "\n";
  // // std::cout << "Age comp likelihoods\n" << age_like << "\n";
  // 
  // // Recruitment - random_rec switches between penalized likelihood and random
  // // effects. Shared sigma_r between rinit_devs and rec_devs, rinit_devs are the
  // // same as the rec_devs but have been reduced by mortality. They were kept
  // // separate to help with accounting.
  // 
  // // Penalized likelihood, sigma_r fixed, bias correction for lognormal
  // // likelihood (-0.5*sigma^2) needed to get mean instead of median from
  // // distribution
  // if (random_rec == 0) {
  //   
  //   // Annual recruitment deviations
  //   for (int i = 0; i < nyr; i++) {
  //     rec_like += square(log_rec_devs(i) - Type(0.5) * square(sigma_r));
  //   }
  //   
  //   // Initial numbers-at-age (sage + 1 to plus group - 1)
  //   for (int j = 0; j < nage - 2; j++) {
  //     rec_like += square(log_rinit_devs(j) - Type(0.5) * square(sigma_r));
  //   }
  //   
  //   rec_like *= wt_rec_like;      // weight
  //   
  // }
  // 
  // // Random effects: mean = 0, sigma_r estimated, same bias correction as
  // // penalized likelihood for lognormal distribution
  // if (random_rec == 1) {
  //   
  //   // Recruitment deviations
  //   for (int i = 0; i < nyr; i++) {
  //     rec_like += log(sigma_r) + Type(0.5) * square(log_rec_devs(i) - Type(0.5) * square(sigma_r)) / square(sigma_r);
  //     // Should be equivalent to: rec_like -= dnorm(log_rec_dev(i) - Type(0.5) * square(sigma_r) , Type(0.0), sigma_r, true);
  //   }
  //   
  //   // Initial numbers-at-age (sage + 1 to plus group - 1)
  //   for (int j = 0; j < nage - 2; j++) {
  //     rec_like += log(sigma_r) + Type(0.5) * square(log_rinit_devs(j) - Type(0.5) * square(sigma_r)) / square(sigma_r);
  //   }
  //   
  //   rec_like *= wt_rec_like;      // weight
  //   
  // }
  // // std::cout << "Log recruitment deviations\n" << log_rec_devs << "\n";
  // // std::cout << "Log deviations for initial numbers-at-age\n" << log_rinit_devs << "\n";
  // // std::cout << "Recruitment likelihood\n" << rec_like << "\n";
  // 
  // // Regularity penalty on fishing mortality
  // for (int i = 0; i < nyr; i++) {
  //   fpen += square(log_F_devs(i));
  // }
  // fpen *= wt_fpen;              // weight
  // 
  // // Large penalty on SPR calculations
  // for(int x = 1; x <= n_Fxx; x++) {
  //   spr_pen += square( SB(x) / SB(0) - Fxx_levels(x-1));
  // }
  // spr_pen *=  wt_spr; 
  // // std::cout << "Log fishing mortality deviations\n" << log_F_devs << "\n";
  // // std::cout << "Penality for fishing mortality\n" << fpen << "\n";
  // // std::cout << "Penality for SPR calcs\n" << spr_pen << "\n";
  // 
  // // Sum likelihood components
  // obj_fun += priors(0);         // Fishery q
  // obj_fun += priors(1);         // Survey q
  // obj_fun += priors(2);         // Mark-recapture abndance index q
  // obj_fun += catch_like;        // Catch // FLAG - NA/NaN function evaluation
  // obj_fun += index_like(0);     // Fishery cpue
  // obj_fun += index_like(1);     // Survey cpue
  // obj_fun += index_like(2);     // Mark-recapture abundance index
  // obj_fun += age_like(0);       // Fishery age compositions
  // obj_fun += age_like(1);       // Survey age compositions
  // obj_fun += rec_like;          // Recruitment deviations
  // obj_fun += fpen;              // Fishing mortality deviations
  // obj_fun += spr_pen;           // SPR calculations
  // 
  // // std::cout << "Objective function\n" << obj_fun << "\n";
  
  obj_fun = dummy*dummy;        // TEST CODE
  
  // REPORT SECTION
  
  // // Predicted indices of abundance
  // REPORT(pred_catch);       // Catch
  // REPORT(pred_mr);          // Mark-recapture index of abundance (only years with an estimate)
  // REPORT(pred_mr_all);      // Mark-recapture index of abundance (all years)
  // REPORT(pred_fsh_cpue);    // Fishery cpue
  // REPORT(pred_srv_cpue);    // Survey cpue
  // 
  // // Predicted age compositions
  // REPORT(pred_fsh_age);     // Fishery
  // REPORT(pred_srv_age);     // Survey
  // 
  // // Predicted selectivity-at-age
  // REPORT(fsh_slx);          // Fishery
  // REPORT(srv_slx);          // Survey
  // 
  // // Predicted annual fishing mortality
  // REPORT(Fmort);
  // 
  // // Derived matrices by year and age
  // REPORT(N);                // Abundance-at-age, projected 1 year forward
  // REPORT(Z);                // Total mortality
  // REPORT(F);                // Fishing mortality
  // REPORT(S);                // Survivorship
  // REPORT(C);                // Catch in numbers at age
  // 
  // // Derived vectors by year
  // REPORT(pred_rec);         // Predicted age-2 recruitment
  // REPORT(biom);             // Total age-2+ biomass
  // REPORT(expl_biom);        // Vulnerable biomass to fishery at the beginning of the fishery
  // REPORT(vuln_abd);         // Vulnerable abundance to survey at the beginning of the survey
  // REPORT(spawn_biom);       // Spawning biomass
  // 
  // // // SPR-based biological reference points and ABC
  // REPORT(Fxx);              // Vector of Fs scaled to fully selected values
  // REPORT(SBPR);             // Vector of spawning biomass per recruit at various Fxx levels
  // REPORT(SB);               // Vector of spawning biomass at various Fxx levels
  // REPORT(ABC);              // ABC at various Fxx levels
  // 
  // // Other derived and projected values
  // REPORT(survival_srv);     // Annual natural survival at time of survey
  // REPORT(survival_fsh);     // Annual natural survival at time of fishery
  // REPORT(pred_rbar);        // Predicted mean recruitment
  // 
  // // Priors, likelihoods, offsets, and penalty functions
  // REPORT(priors);           // q priors
  // REPORT(catch_like);       // Catch
  // REPORT(index_like);       // Abundance indices
  // REPORT(age_like);         // Age compositions
  // REPORT(rec_like);         // Recruitment deviations
  // REPORT(fpen);             // Fishing mortality deviations
  // REPORT(obj_fun);          // Total objective function
  // REPORT(offset);           // Offsets for multinomial

  return(obj_fun);          
  
  }
