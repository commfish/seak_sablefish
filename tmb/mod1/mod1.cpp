#include <TMB.hpp>

template <class Type> Type square(Type x){return x*x;}

template<class Type>
  Type objective_function<Type>::operator() ()
{
  // DATA SECTION:
    
  // Model dimensions
  DATA_INTEGER(nyr)          // number of years in the model
  DATA_INTEGER(nage)         // number of ages in the model
  DATA_INTEGER(cm_comp_nyr)  // number of years containing age comp data
  DATA_IVECTOR(cm_comp_yrs)   // year with age comp data
  
  // Fixed parameters
  DATA_SCALAR(M)             // fixed natural mortality
  DATA_SCALAR(sig_catch)     // assumed CV for catch
  DATA_SCALAR(sig_cm_cpue)   // assumed CV for log CPUE
  DATA_SCALAR(omega_comp)    // effective sample size for age comps //assumed variance (?) for age comps
  
  // Data inputs
  DATA_VECTOR(data_cm_waa)   // commercial weight-at-age (constant across years)
  DATA_VECTOR(data_catch)    // catch in year i
  DATA_VECTOR(data_cm_cpue)  // commercial cpue in year i
  DATA_MATRIX(data_cm_comp)  // commercial age compositions in year i
  
  // PARAMETER SECTION:
  PARAMETER(dummy);        // dummy variable for troubleshooting 
  PARAMETER_VECTOR(logN);    // log abundance in year i
  PARAMETER(cm_sel50);       // commercial selectivity parameter (50% selected)
  PARAMETER(cm_sel95);       // commercial selectivity parameter (95% selected)
  PARAMETER_VECTOR(logF);    // log full fishing mortality in year i
  PARAMETER(logq);           // catchability coefficient
  
  matrix<Type> Nij(nyr+1,nage);     // Abundance in year i, age j
  matrix<Type> Fij(nyr,nage);       // Fishing mortality in year i, age j         
  matrix<Type> Zij(nyr,nage);       // Total mortality in year i, age
  vector<Type> Sj(nage);            // Commercial selectivity at age j
  vector<Type> Fi(nyr);             // Full fishing mortality in year i
  
  vector<Type> pred_catch(nyr);         // Predicted catch
  vector<Type> pred_cm_cpue(nyr);       // Predicted commercial cpue
  matrix<Type> pred_cm_comp(nyr,nage);  // Predicted commercial age comps
  vector<Type> Bi(nyr);                 // Biomass  
  
  Type Like1;
  Type Like2;
  Type Like3;
  Type obj_fun;
  
  // End of specifications section
  // =============================
    
    // Specify selectivity
  for (int j=0;j<nage;j++)
    Sj(j) = 1.0/(1+exp(-log(19)*(j-cm_sel50)/(cm_sel95-cm_sel50)));
  
  // Compute the F and Z matrices
  for (int i=0;i<nyr;i++)
    for (int j=0;j<nage;j++)
    {
      Fi(i) = exp(logF(i));
      Fij(i,j) = exp(logF(i))*Sj(j);
      Zij(i,j) = M + Fij(i,j);
    }
  
  // Insert all the recritments (watch out for the index pointers)
  for (int j=0;j<nage;j++) Nij(0,j) = exp(logN(nage-j-1));
  for (int i=1;i<nyr;i++) Nij(i,0) = exp(logN(nage+i-1)); //mean recruitment multiplied by log deviation
  Nij(nyr,0) = 0;
  
  // Project the whole N-matrix
  for (int i=0;i<nyr;i++)
    for (int j=0;j<nage-1;j++)
      Nij(i+1,j+1) = Nij(i,j)*exp(-Zij(i,j));
  
  // Compute the predicted exploitable biomass, catch-at-age and catch
  Type Cij; // catch at age j in numbers in year j (temporary variable)
  for (int i=0;i<nyr;i++)
  {
    Bi(i) = 0; pred_catch(i) = 0; Cij = 0;
    for (int j=0;j<nage;j++)
    {
      // Multiple by 100 to get N and catch on the same scale - not sure about this.
      pred_cm_comp(i,j) = Fij(i,j)/Zij(i,j)*Nij(i,j)*(1.0-exp(-Zij(i,j)));//*100;
      pred_catch(i) += data_cm_waa(j)*pred_cm_comp(i,j);
      Bi(i) += data_cm_waa(j)*Sj(j)*Nij(i,j)*exp(-Zij(i,j)/2.0);
      Cij += pred_cm_comp(i,j);
    }
    pred_cm_cpue(i) = exp(logq)*Bi(i);
    for (int j=0;j<nage;j++) pred_cm_comp(i,j) /= Cij;
  }
  
  // Likelihood components
  // 0: Catch
  // 1: Commercial cpue
  // 2: Commercial age comps
  
  // vector<Type> nll(3);
  // nll.setZero();
  
  // Catch data (normal likelihood)
  Like1 = 0;  
  for (int i=0;i<nyr;i++)
    Like1 += square((data_catch(i)-pred_catch(i))/pred_catch(i));
  Like1 = Like1 / (2.0*square(sig_catch));
  
  // CPUE data (lognormal likelihood)
  Like2 = 0;
  for (int i=0;i<nyr;i++)
    Like1 += square(log(data_cm_cpue(i)) - log(pred_cm_cpue(i)));
  Like2 = Like2 / (2.0*square(sig_cm_cpue));
  
  // Catch-at-age data (multinomial likelihood)
  Like3 = 0;
  
  matrix<Type> tmp_pred_comp(cm_comp_nyr, nage); 
  
  for (int i=0; i<cm_comp_nyr; i++) 
  {
    for (int j=0;j<nage;j++)
    {
    tmp_pred_comp(i) = pred_cm_comp(cm_comp_yrs(i));
    }
  }
  
  for (int i=0;i<cm_comp_nyr;i++)
  {
    for (int j=0;j<nage;j++)
    {
      if (data_cm_comp(i,j) > 0)
        //Extract years from the predicted composition matrix for which we have data
        //int new_i = cm_comp_yrs(i);
        Like3 += data_cm_comp(i,j)*log(tmp_pred_comp(i,j)/data_cm_comp(i,j));
    }
  }
  Like3 = -1*omega_comp*Like3;
  
  obj_fun = dummy*dummy + Like1 + Like2 + Like3; // dummy is for testing purposes
  
  REPORT(Sj);
  REPORT(Nij);
  ADREPORT(Fi);
  REPORT(pred_cm_cpue);
  REPORT(pred_cm_comp);
  REPORT(pred_catch);
  REPORT(Like1);
  REPORT(Like2);
  REPORT(Like3);
  REPORT(obj_fun);
  
  return(obj_fun);
  
  }
