#include <TMB.hpp>
#include <numeric>

template <class Type> Type square(Type x){return x*x;}

// template <class Type> Type sumVec(vector<Type> vec){return std::accumulate(vec.begin(), vec.end(), 0.0);}

// template <class Type> Type mean(vector<Type> vec){return std::accumulate(vec.begin(), vec.end(), 0.0) / vec.size();}

template<class Type>
  Type objective_function<Type>::operator() ()
{
  // **DATA SECTION**
    
  DATA_INTEGER(nyr)             // number of years in the model
  DATA_INTEGER(nage)            // number of ages in the model
  
  // Fixed parameters
  DATA_SCALAR(M)                // assumed constant natural mortality
  DATA_SCALAR(sigma_catch)      // assumed CV of 5% for catch
  DATA_SCALAR(sigma_cpue)       // assumed CV of 20% (currently use for all cpue indices)
  DATA_SCALAR(sigma_mr)         // assumed CV of 5% for mark-recapture estimates
  DATA_SCALAR(omega)            // effective sample size for age comps (currently use for all age comps)

  // INDICES OF ABUNDANCE
  
  // Catch
  DATA_VECTOR(data_catch)       
    
  // Mark-recapture estimates
  DATA_INTEGER(nyr_mr)          // number of years 
  DATA_IVECTOR(yrs_mr)          // vector of years
  DATA_VECTOR(data_mr)          // vector of estimates
  
  // Fishery cpue
  DATA_INTEGER(nyr_fsh_cpue)    // number of years 
  DATA_IVECTOR(yrs_fsh_cpue)    // vector of years
  DATA_VECTOR(data_fsh_cpue)    // vector of estimates
    
  // Survey (1-hr soak time) cpue 
  DATA_INTEGER(nyr_srv1_cpue)    // number of years 
  DATA_IVECTOR(yrs_srv1_cpue)    // vector of years
  DATA_VECTOR(data_srv1_cpue)    // vector of estimates

  // Survey (3-hr soak time) cpue
  DATA_INTEGER(nyr_srv2_cpue)     // number of years 
  DATA_IVECTOR(yrs_srv2_cpue)     // vector of years
  DATA_VECTOR(data_srv2_cpue)     // vector of estimates

  // BIOLOGICAL DATA
  
  // Timing in months (e.g. January = 1/12 = 0.083)
  DATA_SCALAR(spawn_month)        // month when spawning occurs (February)
  DATA_SCALAR(srv_month)          // month when survey begins (July)
  DATA_SCALAR(fsh_month)          // month when fishery begins (August)
  
  // Proportion mature at age (all years combined)
  DATA_VECTOR(prop_mature)
    
  // Proportion female at age in the survey (all years combined)
  DATA_VECTOR(prop_fem)
  
  // Weight-at-age (currently all years combined)
  DATA_VECTOR(data_fsh_waa)       // Fishery (sexes combined)
  DATA_VECTOR(data_srv_waa)       // Survey (sexes combined)
  DATA_VECTOR(data_fem_waa)       // Survey female weight-at-age (used for spawning biomass)
    
  // Fishery age comps
  DATA_INTEGER(nyr_fsh_age)       // number of years 
  DATA_IVECTOR(yrs_fsh_age)       // vector of years
  DATA_MATRIX(data_fsh_age)       // matrix of observations (year, age)
  
  // Survey age comps
  DATA_INTEGER(nyr_srv_age)       // number of years
  DATA_IVECTOR(yrs_srv_age)       // vector of years
  DATA_MATRIX(data_srv_age)       // matrix of observations (year, age)
    
  // Ageing error transition matrix (proportion at reader age given true age)
  // DATA_MATRIX(nage, nage)

  // **PARAMETER SECTION**
  
  // Dummy variable for troubleshooting 
  PARAMETER(dummy);          

  // Selectivity 
  PARAMETER(fsh_sel50);           // Fishery (50% selected)
  PARAMETER(fsh_sel95);           // Fishery (95% selected)
  PARAMETER(srv_sel50);           // Survey (50% selected)
  PARAMETER(srv_sel95);           // Survey (95% selected)

  // Catchability
  PARAMETER(fsh_logq);            // fishery       
  PARAMETER(srv1_logq);           // survey (1-hr soak time)      
  PARAMETER(srv2_logq);           // survey (3-hr soak time)      
  PARAMETER(mr_logq);             // mark-recapture 
    
  // Recruitment (rec_devs include a parameter for all ages in the inital yr plus age-2 in all yrs)
  PARAMETER(log_rbar);            // Mean recruitment
  PARAMETER_VECTOR(log_rec_devs); // Recruitment deviations (nyr+nage-2)
  
  // Fishing mortality
  PARAMETER(log_Fbar);            // Mean F on log scale 
  PARAMETER_VECTOR(log_F_devs);   // Annual deviations from log_Fbar (nyr)
  
  // **DERIVED QUANTITIES**
  
  // Predicted indices of abundance
  vector<Type> pred_catch(nyr);                 // Catch
  vector<Type> pred_mr(nyr_mr);                 // Mark-recapture index of abundance (only years with an estimate)
  vector<Type> pred_mr_all(nyr);                // Mark-recapture index of abundance (all years!)
  vector<Type> pred_fsh_cpue(nyr_fsh_cpue);     // Fishery cpue
  vector<Type> pred_srv1_cpue(nyr_srv1_cpue);   // Survey (1-hr soak time) cpue
  vector<Type> pred_srv2_cpue(nyr_srv2_cpue);   // Survey (3-hr soak time) cpue
  
  // Predicted age compositions
  matrix<Type> pred_fsh_age(nyr_fsh_age, nage);  // Fishery 
  matrix<Type> pred_srv_age(nyr_srv_age, nage);  // Survey
  
  // Predicted selectivity
  vector<Type> fsh_sel(nage);     // Fishery selectivity-at-age (on natural scale)
  vector<Type> srv_sel(nage);     // Survey selectivity-at-age (on natural scale)
  
  // Predicted annual fishing mortality
  vector<Type> Fmort(nyr);      // On natural scale
  
  // Derived matrices by year and age
  matrix<Type> N(nyr+1, nage);  // Abundance-at-age, projected 1 year forward
  matrix<Type> Z(nyr, nage);    // Total mortality
  matrix<Type> F(nyr, nage);    // Fishing mortality
  matrix<Type> S(nyr, nage);    // Survivorship
  matrix<Type> C(nyr, nage);    // Catch in numbers

  // Derived vectors by year 
  vector<Type> pred_rec(nyr);   // Predicted age-2 recruitment
  vector<Type> biom(nyr);       // Total age-2+ biomass
  vector<Type> expl_biom(nyr);  // Vulnerable biomass to fishery at the beginning of the fishery
  vector<Type> vuln_abd(nyr);   // Vulnerable abundance to survey at the beginning of the survey
  vector<Type> spawn_biom(nyr); // Spawning biomass
  biom.setZero();
  expl_biom.setZero();
  vuln_abd.setZero();
  spawn_biom.setZero();
  
  // Other derived and projected values
  Type surv_srv;                // Annual natural survival at time of survey
  Type surv_fsh;                // Annual natural survival at time of fishery
  Type pred_rbar;               // Predicted mean recruitment
  Type biom_proj;               // Projected biomass
  Type expl_biom_proj;          // Projected vulnerable biomass to fishery at the beginning of the fishery
  Type spawn_biom_proj;         // Projected spawning biomass
  
  // Priors, likelihoods, offsets, and penalty functions
  vector<Type> priors(4);       // Priors for catchability coefficients
  priors.setZero();
  
  vector<Type> offset(2);       // Offset for multinomial distribution that lets likelihood equal zero when obs = pred
    
  Type catch_like = 0;          // Catch  
  Type rec_like = 0;            // Recruitment
  Type fpen = 0;                // Penality for Fmort regularity
  
  vector<Type> index_like(4);   // Fishery cpue, survey cpue, MR estimates 
  index_like.setZero();

  vector<Type> age_like(2);     // Fishery and survey age comps
  age_like.setZero();
  
  Type obj_fun = 0;             // Objective function
  
  // **MODEL**
  
  // Indexing: i = year, j = age
  
  // Fishery selectivity
  for (int j = 0; j < nage; j++)
    fsh_sel(j) = Type(1.0) / ( Type(1.0) + exp(-log(Type(19)) * (j - fsh_sel50) / (fsh_sel95 - fsh_sel50)) );

  // Survey selectivity
  for (int j = 0; j < nage; j++)
    srv_sel(j) = Type(1.0) / ( Type(1.0) + exp(-log(Type(19)) * (j - srv_sel50) / (srv_sel95 - srv_sel50)) );

  // std::cout << fsh_sel << "\n";
  // std::cout << srv_sel << "\n";

  // Mortality and survivorship
  for (int i = 0; i < nyr; i++) {
    for (int j = 0; j < nage; j++) {

      // Annual fishing mortality
      Fmort(i) = exp(log_Fbar + log_F_devs(i));

      // Fishing mortality by year and age
      F(i,j) = Fmort(i) * fsh_sel(j);

      // Total mortality by year and age
      Z(i,j) = M + F(i,j);

      // Survivorship by year and age
      S(i,j) = exp(-1.0 * Z(i,j));
    }
  }

  // std::cout << Fmort << "\n";
  // std::cout << F << "\n";
  // std::cout << Z << "\n";
  // std::cout << S << "\n";

  // Annual natural survival at time of survey and fishery (fraction surviving
  // from beginning of year to the time of survey and fishery)
  surv_srv = exp(-1.0 * srv_month * M);
  surv_fsh = exp(-1.0 * fsh_month * M);

  // std::cout << surv_srv << "\n";
  // std::cout << surv_fsh << "\n";
  
  // Abundance and recruitment

  // Start year: recruitment age + 1 to plus group - 1
  for (int j = 1; j < nage-1; j++) {
    N(0,j) = exp(log_rbar - M * Type(j) + log_rec_devs(nage-j-2));
  }

  // Start year: plus group
  N(0,nage-1) = exp(log_rbar - M * Type(nage-1)) / (1 - exp(-M));

  // Recruitment in all years (except the projected year)
  for (int i = 0; i < nyr; i++) {
    N(i,0) = exp(log_rbar + log_rec_devs(nage+i-2));
  }

  // Project remaining N matrix
  for (int i = 0; i < nyr; i++) {
    for (int j=0; j < nage-1; j++) {
      N(i+1,j+1) = N(i,j) * S(i,j);
    }
  }
  
  // Plus group for start year + 1 to final year + 1 (sum of cohort survivors and
  // survivng memebers of last year's plus group)
  for (int i = 0; i < nyr; i++) {
    N(i+1,nage-1) += N(i,nage-1) * S(i,nage-1);
  }

  // Projected recruitment (average over recent 15 years, excluding most recent
  // 2 years), where the indexing of log_rec_devs(0,nyr+nage-3)
  for (int i = nyr+nage-3-16; i <= nyr+nage-3-2; i++) {
    N(nyr,0) += exp(log_rbar + log_rec_devs(i));
  }
  N(nyr,0) /= Type(15.0);

  // FLAG - Alternative way using mean(). Want to find a way to sum() over
  // vector
  // vector<Type> tmp = exp(log_rbar + log_rec_devs(nyr+nage-3-16, nyr+nage-3-2));
  // N(nyr,0) = mean(tmp);

  // std::cout << N << "\n";
  
  // Catch

  // Catch in numbers-at-age (F already incorporates fishery selectivity-at-age)
  for (int i = 0; i < nyr; i++) {
    for (int j = 0; j < nage; j++) {
      C(i,j) = N(i,j) * F(i,j) * (1 - S(i,j)) / Z(i,j);
    }
  }
  
  // std::cout << C << "\n";

  pred_catch.setZero(); // Initialize

  // Predicted annual catch
  for (int i = 0; i < nyr; i++) {
    for (int j = 0; j < nage; j++) {
      pred_catch(i) += C(i,j) * data_fsh_waa(j);
    }
  }
  
  // std::cout << C << "\n";
  // std::cout << pred_catch << "\n";

  // Predicted recruitment by year
  for (int i = 0; i < nyr; i++) {
    pred_rec(i) = N(i,0);
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

  // Various flavors of predicted biomass estimates
  for (int i = 0; i < nyr; i++) {
    for (int j = 0; j < nage; j++) {

      // Total biomass at time of longline survey
      biom(i) += data_srv_waa(j) * N(i,j) * surv_srv; 

      // Vulnerable biomass to the fishery at the beginning of the fishery
      expl_biom(i) += data_srv_waa(j) * fsh_sel(j) * N(i,j) * surv_fsh; 

      // Vulnerable abundance to the survey at the beginning of the survey
      vuln_abd(i) += srv_sel(j) * N(i,j) * surv_srv;

      // Spawning biomass
      spawn_biom(i) += data_srv_waa(j) * N(i,j) * exp(-spawn_month * M) * prop_fem(j) * prop_mature(j);
    }
  }
  
  // std::cout << "Predicted biomass\n" << biom << "\n";
  // std::cout << "Predicted exploited biomass\n" << expl_biom << "\n";
  // std::cout << "Predicted vulnerable abundance\n" << vuln_abd << "\n";
  // std::cout << "Predicted spawning biomass\n" << spawn_biom << "\n";
  
  // Predicted values

  // Mark-recapture catchability and predicted abundance
  Type mr_q = exp(mr_logq);

  for (int i = 0; i < nyr_mr; i++) {
    pred_mr(i) = mr_q * vuln_abd(yrs_mr(i));    // Just in years with a MR estimate
  }
  // std::cout << "Predicted MR \n" << pred_mr << "\n";
  
  for (int i = 0; i < nyr; i++) {
    pred_mr_all(i) = mr_q * vuln_abd(i);        // All years
  }
  // std::cout << "Predicted MR for all years\n" << pred_mr_all << "\n";
 
  // Fishery catchability and predicted cpue
  Type fsh_q = exp(fsh_logq);

  for (int i = 0; i < nyr_fsh_cpue; i++) {
    pred_fsh_cpue(i) = fsh_q * expl_biom(yrs_fsh_cpue(i));
  }
  // std::cout << "Predicted fishery cpue\n" << pred_fsh_cpue << "\n";
  
  // 1-hr soak survey catchability and predicted survey cpue
  Type srv1_q = exp(srv1_logq);

  for (int i = 0; i < nyr_srv1_cpue; i++) {
    pred_srv1_cpue(i) = srv1_q * vuln_abd(yrs_srv1_cpue(i));
  }
  // std::cout << "Predicted srv1 cpue\n" << pred_srv1_cpue << "\n";
  
  // 3-hr soak survey catchability and predicted survey cpue
  Type srv2_q = exp(srv2_logq);

  for (int i = 0; i < nyr_srv2_cpue; i++) {
    pred_srv2_cpue(i) = srv2_q * vuln_abd(yrs_srv2_cpue(i));
  }
  // std::cout << "Predicted srv2 cpue\n" << pred_srv2_cpue << "\n";
  
  // Predicted fishery age compositions

  // sumC = temporary variable, sum catch in numbers-at-age by year
  int syr_fsh_age = yrs_fsh_age(0);
  
  for (int i = syr_fsh_age; i < nyr; i++) {
    Type sumC = 0;
    for (int j = 0; j < nage; j++) {
      sumC += C(i,j);
    }
    for (int j = 0; j < nage; j++) {
      pred_fsh_age(i-syr_fsh_age,j) = C(i,j) / sumC;
    }
  }
   // std::cout << "Predicted fishery age comps\n" << pred_fsh_age << "\n";

  // Predicted survey age compositions

  // sumN = temporary variable sum numbers-at-age by year
  for (int i = 0; i < nyr_srv_age; i++) {
    Type sumN = 0;
    for (int j = 0; j < nage; j++) {
      sumN += N(yrs_srv_age(i),j) * srv_sel(j);
    }
    for (int j = 0; j < nage; j++) {
      pred_srv_age(i,j) = N(yrs_srv_age(i),j) * srv_sel(j) / sumN;
    }
  }

  // // Test do the predicted age comps sum to 1
  // vector<Type> tst(nyr_srv_age);
  // for (int i = 0; i < nyr_srv_age; i++) {
  //   for (int j = 0; j < nage; j++) {
  //     tst(i) += pred_srv_age(i,j);
  //   }
  // }
  // std::cout << "Do the comps sum to 1?\n" << tst << "\n";
  // 
  // Type tst_n = tst.size();;
  // std::cout << "Length of tst vector\n" << tst_n << "\n";
  
  // FLAG - would like to know the TMB syntax to do the age comps in fewer
  // lines, for example:
  // for (int i = 0; i < nyr_srv_age; i++) {
  //   for (int j = 0; j < nage; j++) {
  //     pred_srv_age(i,j) = N(yrs_srv_age(i),j) * srv_sel(j) / sum(N(yrs_srv_age(i)) * srv_sel(j));
  //   }
  // }

  // FLAG - Add Projected biomass section - just add nyr+1 to the existing
  // vectors?
  // biom_proj
  // expl_biom_proj
  // spawn_biom_proj

  // FLAG - Add section to compute SPR and management reference points

  // Priors

  // Fishery cpue catchability coefficient
  // priors(0) = square( log(fsh_q / Type(0.025)) ) / ( 2 * square(Type(1.0)) );
  // 
  // // 1-hr soak survey catchability coefficient
  // priors(1) = square( log(srv1_q / Type(0.09)) ) / ( 2 * square(Type(1.0)) );
  // 
  // // 3-hr soak survey catchability coefficient
  // priors(2) = square( log(srv2_q / Type(0.02)) ) / ( 2 * square(Type(1.0)) );

  // Mark-recapture abundance estimate catchability coefficient
  // priors(3) = square( log(mr_q / Type(1.0)) ) / ( 2 * square(Type(0.10)) );

  // std::cout << "priors\n" << priors << "\n";
  
  // Catch: lognormal
  for (int i = 0; i < nyr; i++) {
    catch_like += square(log(data_catch(i) + 0.00001) - log(pred_catch(i) + 0.00001));
  }
  catch_like /= Type(2.0) * square(sigma_catch);
  // std::cout << "Catch likelihood\n" << catch_like << "\n";
  
  // Fishery CPUE: lognormal
  for (int i = 0; i < nyr_fsh_cpue; i++) {
    index_like(0) += square( log(data_fsh_cpue(i) / pred_fsh_cpue(i)) ) /
      Type(2.0) * square(sigma_cpue);
  }

  // 1-hr soak survey CPUE: lognormal
  for (int i = 0; i < nyr_srv1_cpue; i++) {
    index_like(1) += square( log(data_srv1_cpue(i) / pred_srv1_cpue(i)) ) /
      Type(2.0) * square(sigma_cpue);
  }

  // 3-hr soak survey CPUE: lognormal
  for (int i = 0; i < nyr_srv2_cpue; i++) {
    index_like(2) += square( log(data_srv2_cpue(i) / pred_srv2_cpue(i)) ) /
      Type(2.0) * square(sigma_cpue);
  }

  // Mark-recapture index: lognormal
  for (int i = 0; i < nyr_mr; i++) {
    index_like(3) += square( log(data_mr(i) / pred_mr(i)) ) /
      Type(2.0) * square(sigma_mr);
  }
  // std::cout << "Index likelihoods\n" << index_like << "\n";

  // Offset for fishery age compositions
  for (int i = 0; i < nyr_fsh_age; i++) {
    offset(0) -= omega * ( (data_fsh_age(i) + Type(0.00001)) * log(data_fsh_age(i) + Type(0.00001)) );
  }

  // Fishery age compositions: multinomial
  for (int i = 0; i < nyr_fsh_age; i++) {
    for (int j = 0; j < nage; j++) {
      age_like(0) -= (data_fsh_age(i,j) + Type(0.00001)) * log(pred_fsh_age(i,j) + Type(0.00001));
    }
  }
  age_like(0) *= omega;       // effective sample size
  age_like(0) -= offset(0);   // subtract offset

  // Offset for survey age compositions
  for (int i = 0; i < nyr_srv_age; i++) {
    offset(1) -= omega * ( (data_srv_age(i) + Type(0.00001)) * log(data_srv_age(i) + Type(0.00001)) );
  }

  // Survey age compositions: multinomial
  for (int i = 0; i < nyr_srv_age; i++) {
    for (int j = 0; j < nage; j++) {
      age_like(1) -= (data_srv_age(i,j) + Type(0.00001)) * log(pred_srv_age(i,j) + Type(0.00001));
    }
  }
  age_like(1) *= omega;       // effective sample size
  age_like(1) -= offset(1);   // substract offset

  // std::cout << "Age comp offset\n" << offset << "\n";
  // std::cout << "Age comp likelihoods\n" << age_like << "\n";
  
  // Penalty for recruitment
  Type n_rec = log_rec_devs.size();

  for (int i = 0; i < n_rec; i++) {
    rec_like += square(log_rec_devs(i));
  }
  rec_like *= Type(0.1);      // Weight

  // std::cout << "Log recruitment deviations\n" << log_rec_devs << "\n";
  // std::cout << "Recruitment likelihood\n" << rec_like << "\n";
  
  // Penalty for fishing mortality deviations
  Type n_Fdev = log_F_devs.size();

  for (int i = 0; i < n_Fdev; i++) {
    fpen += square(log_F_devs(i));
  }
  fpen *= Type(0.1);          // Weight
  
  // std::cout << "Log fishing mortality deviations\n" << log_F_devs << "\n";
  // std::cout << "Penality for fishing mortality\n" << fpen << "\n";
  
  // Sum likelihood components
  // obj_fun += priors(0);         // Fishery q
  // obj_fun += priors(1);         // 1-hr soak time survey q
  // obj_fun += priors(2);         // 3-hr soak time survey q
  obj_fun += priors(3);         // Mark-recapture abndance index q
  obj_fun += catch_like;        // Catch
  obj_fun += index_like(0);     // Fishery cpue
  obj_fun += index_like(1);     // 1-hr soak time survey cpue
  obj_fun += index_like(2);     // 3-hr soak time survey cpue
  obj_fun += index_like(3);     // Mark-recapture abundance index
  obj_fun += age_like(0);       // Fishery age compositions
  obj_fun += age_like(1);       // Survey age compositions
  obj_fun += rec_like;          // Recruitment deviations
  obj_fun += fpen;              // Fishing mortality deviations
  
  // std::cout << "Objective function\n" << obj_fun << "\n";
  
  // obj_fun = dummy*dummy;        // TEST CODE
  
  // REPORT SECTION
  
  // Predicted indices of abundance
  REPORT(pred_catch);       // Catch
  REPORT(pred_mr);          // Mark-recapture index of abundance (only years with an estimate)
  REPORT(pred_mr_all);      // Mark-recapture index of abundance (all years)
  REPORT(pred_fsh_cpue);    // Fishery cpue
  REPORT(pred_srv1_cpue);   // Survey (1-hr soak time) cpue
  REPORT(pred_srv2_cpue);   // Survey (3+hr soak time) cpue
  
  // Predicted age compositions
  REPORT(pred_fsh_age);     // Fishery
  REPORT(pred_srv_age);     // Survey
  
  // Predicted selectivity-at-age
  REPORT(fsh_sel);          // Fishery
  REPORT(srv_sel);          // Survey
  
  // Predicted annual fishing mortality
  REPORT(Fmort);
  
  // Derived matrices by year and age
  REPORT(N);                // Abundance-at-age, projected 1 year forward
  REPORT(Z);                // Total mortality
  REPORT(F);                // Fishing mortality
  REPORT(S);                // Survivorship
  REPORT(C);                // Catch in numbers at age
  
  // Derived vectors by year
  REPORT(pred_rec);         // Predicted age-2 recruitment
  REPORT(biom);             // Total age-2+ biomass
  REPORT(expl_biom);        // Vulnerable biomass to fishery at the beginning of the fishery
  REPORT(vuln_abd);         // Vulnerable abundance to survey at the beginning of the survey
  REPORT(spawn_biom);       // Spawning biomass
  
  // Other derived and projected values
  REPORT(surv_srv);         // Annual natural survival at time of survey
  REPORT(surv_fsh);         // Annual natural survival at time of fishery
  REPORT(pred_rbar);        // Predicted mean recruitment
  REPORT(biom_proj);        // Projected biomass
  REPORT(expl_biom_proj);   // Projected vulnerable biomass to fishery at the beginning of the fishery
  REPORT(spawn_biom_proj);  // Projected spawning biomass
  
  // Priors, likelihoods, offsets, and penalty functions
  REPORT(priors);           // q priors
  REPORT(catch_like);       // Catch
  REPORT(index_like);       // Abundance indices
  REPORT(age_like);         // Age compositions
  REPORT(rec_like);         // Recruitment deviations
  REPORT(fpen);             // Fishing mortality deviations
  REPORT(obj_fun);          // Total objective function

  return(obj_fun);          
  
  }
