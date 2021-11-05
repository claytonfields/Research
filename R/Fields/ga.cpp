#include <Rcpp.h>
using namespace Rcpp;



Environment pkg = Environment::namespace_env("stats");
Function f("lm"); 


// Compute the MDL penalty

// [[Rcpp::export]]
double penalty_MDL(NumericVector y,NumericVector X, NumericVector cp,
                   double x_min, double x_max, int x_inc) {                  // y   : response
// x   : predictor with varying coefficient
// cp  : cpt chromosome (m;xi_1,...,xi_m)
  int m = cp(0);                                       //# m   : number of cpts
  int m_max = 10;
  int n = X.length();
  if (m == 0) {
    double pnt = log(n);
  } else {
    NumericVector xi = cbind(x_min-x_inc,cp.erase(0),x_max+x_inc);        //xi  : cpt locations (xi_1,...,xi_m,x.max+x.inc)
    NumericVector n_r; // n.r : no. of obs in each regime
    for (int i : Range(0,m)) {                           //[!CAUTION!] This does not handle missing values!
      NumericVector temp = y[xi[i] <= X & X < xi[i+1]];
      n_r[i] = temp.size();
    }
// Try rank scheme for penalty term
// pnt <- log(m+1)+.5*sum(log(sort(n.r)[-1])) #hange n.r to xi
      NumericVector term1 = n_r[Range(1,)]
      pnt = log(m) + (m)*log(n) + 0.5*m*term1.log().sum();


  }
  return pnt;                                     // # smaller is better
}
// 
// ### ML estimation via glm() without exact covariates, returning negative likelihood value
// nloglik.M0_glm <- function(y,x,cp) {               # y   : response
// # x   : predictor with varying coefficient
// # cp  : changepoint chromosome (m; tau_1,...,tau_m)
// # link: link function for Poisson regression
//   m <- cp[1]                                       # m   : number of cpts
//   
//   if (m == 0) {
//     X <- cbind(x)
//   } else {
//     x.spl <- outer(x,cp[-1],">")*outer(x,cp[-1],"-")
//     X <- cbind(x,x.spl)
//   }
//   p <- ncol(X)-m
//     
//     fit.MLE_out <- glm(y~X,family=gaussian(link="identity"),start=c(rep(0.5,1+p),rep(0,m)))
//     nllik.glm <- -as.numeric(logLik(fit.MLE_out))
//     
//     return(nllik.glm)
// }
// ###
// 
// ### Compute the penalized log-likelihood with MDL penalty
// pnllik.MDL.M0 <- function(y,x,cp,x.min,x.max,x.inc) {       # y   : response
// # x   : predictor with varying coefficient
// # i.g : indicator variable for group
// # cp  : changepoint chromosome (m; tau_1,...,tau_m)
// # link: link function for Poisson regression
//   pnllik.MDL <- nloglik.M0_glm(y=y,x=x,cp=cp)+penalty.MDL(y=y,x=x,cp=cp,x.min=x.min,x.max=x.max,x.inc=x.inc)
//   
//   return(pnllik.MDL)
// }
// 
// 
// 
// ### ML estimation via glm() with exact covariates, returning negative log-likelihood value
// nloglik.M0Z_glm <- function(y,z,x,cp) {            # y   : response
// # z   : predictor without measurement error
// # x   : predictor with varying coefficient
// # cp  : changepoint chromosome (m; tau_1,...,tau_m)
// # link: link function for Poisson regression
//   m <- cp[1]                                       # m   : number of cpts
//   
//   if (m == 0) {
//     X <- cbind(z,x)
//   } else {
//     x.spl <- outer(x,cp[-1],">")*outer(x,cp[-1],"-")
//     X <- cbind(z,x,x.spl)
//   }
//   p <- ncol(X)-m
//     
//     fit.MLE_out <- glm(y~X,family=gaussian(link="identity"),start=c(rep(0.5,1+p),rep(0,m)))
//     nllik.glm <- -as.numeric(logLik(fit.MLE_out))
//     
//     return(nllik.glm)
// }
// ###
// 
// ### ML estimation via glm(), returning the glm fit result
// fit.glm_M0Z <- function(y,z,x,cp) {                # y   : response
// # z   : predictor without measurement error
// # x   : predictor with varying coefficient
// # cp  : changepoint chromosome (m; tau_1,...,tau_m)
// # link: link function for Poisson regression
//   m <- cp[1]                                       # m   : number of cpts
//   
//   if (m == 0) {
//     X <- cbind(z,x)
//   } else {
//     x.spl <- outer(x,cp[-1],">")*outer(x,cp[-1],"-")
//     X <- cbind(z,x,x.spl)
//   }
//   p <- ncol(X)-m
//     
//     fit.glm_out <- glm(y~X,family=gaussian(link="identity"),start=c(rep(0.5,1+p),rep(0,m)))
//     
//     return(fit.glm_out)
// }
// ###
// 
// ### Compute the penalized log-likelihood with MDL penalty
// pnllik.MDL.M0Z <- function(y,z,x,cp,x.min,x.max,x.inc) {             # y   : response
// # z   : predictor without measurement error
// # x   : predictor with varying coefficient
// # i.g : indicator variable for group
// # cp  : changepoint chromosome (m; tau_1,...,tau_m)
// # link: link function for Poisson regression
//   pnllik.MDL <- nloglik.M0Z_glm(y=y,z=z,x=x,cp=cp)+penalty.MDL(y=y,x=x,cp=cp,x.min=x.min,x.max=x.max,x.inc=x.inc)
//   
//   return(pnllik.MDL)
// }









// // 
// #*[-----------------------------------------------------------------------------------------------]*#
// ### GA for a continuous covariate x in a simple normal regression model
// #*[-----------------------------------------------------------------------------------------------]*#
// 
// 
// ### Utility Function: Get ML estimates for a given generation of chromosomes (called in ga.cpt_Norm)


// [[Rcpp::export]]
NumericMatrix mle(NumericVector y, NumericVector x, double x_min, double x_max, double x_inc, int g, List Confg, 
                 int gen_size = 200,
         double Confg_pre_sol_lik= 0 ){
//Args refrence those in ga.cpt_Norm
  int start_pos;
  NumericMatrix vals(0,1,gen_size);
  if(g==0){
    start_pos = 0;
  } else {
    start_pos = 2;
    vals(0,1) = Confg_pre_sol_lik;
  }
// Loop through generation of chromosomes
  for (int j : Range(start_pos,gen_size)) {
    NumericVector chrom = Confg[j];
    //vals[1,j] <- fitness(y=y,z=z,x=x,cp=chrom,x.min=x.min,x.max=x.max,x.inc=x.inc)
    vals(0,j) = 0;
  } // end loop
    return vals;
} //End function mle


// Utility Function: Generate initial generation from random values (called in ga.cpt_Norm)

// [[Rcpp::export]]
// List initial_population(NumericVector X, double x_min, double x_max, int x_inc, int gen_size, int max_cpt){
// // Args refrence those in ga_cpt_Norm
// 
//     
//       List Confg = List::create();
// //  A chromosome of no changepoints is always considered
//       Confg[0] = 0;
//     int j = 1;
//     while(j <= gen_size) {
// // [!CAUTION!] Adjust prob=0_4 for other settings
//       //n_cpt = rbinom(1,size=max_cpt,prob=0_4);
// // Changepoints will occur between x_min+x_inc and x_max-x_inc
//       x_cpt = sort(runif(n_cpt,min=x_min+x_inc,max=x_max-x_inc));
//       chrom = cbind(n_cpt,x-cpt);
//       Confg[j] = chrom;
// 
// // Check conditions, discard chrom if not met
//       bool is_pass = FALSE;
//         if (n_cpt == 0) {
//           is_pass = TRUE
//         } else {
//           if (all(diff(c(x_min,x_cpt,x_max)) > x_inc) & // do not allow a changepoint within x_inc of x
//             x_min < min(x_cpt) &                      // smallest location shold be > x_min
//             max(x_cpt) < x_max) {                     // greatest location shold be < x_max
//             is_pass = TRUE
//           }
//         }
//         if (length(unique(Confg[1:j])) == j & is_pass == TRUE) {
//           j = j+1;                                   // generation increases when (1) a new child chrom is born
//         }                                            //                       and (2) the above condition is met
//     } // End loop
//       return Confg;
//   }

// ### Utility Function: Generate next gneration based on previous ML estimates (called in ga.cpt_Norm)

// [[Rcpp::export]]
// List  next_gen(Confg_pre, NumericVector Confg_pre_sol, probs, double x_min, double x_max, double x_inc, int gen_size, double p_mut){
// //// Args refrence those in ga_cpt_Norm
// 
//     List Confg = = List::create();
//     Confg[0] = Confg_pre_sol;                    // Include previous solution
//     int j = 2;
//     while(j <= gen_size) {
// //// Select father and mother chromosomes
//       loc_prt = sample(1:gen_size,size=2,replace=FALSE,prob=probs);
//       loc_dad = loc_prt[1];
//       loc_mom = loc_prt[2];
//       chrom_dad = Confg_pre[[loc_dad]];
//       chrom_mom = Confg_pre[[loc_mom]];
// 
// //// Produce child chromosomes
// // Step 1: Combining
//       x_cpt_S1 = sort(union(chrom_dad[-1],chrom_mom[-1]));        // Do not allow identical chagepoint times
//         n_cpt_S1 = length(x_cpt_S1);
//         if (n_cpt_S1 == 0) {
// // Step 2: Thinning (SKIP!!!)
// // Step 3: Shifting (SKIP!!!)
// // Step 4: Mutation
// n_mut = rbinom(1,size=2,prob=p_mut);                      // [!CAUTION!] Adjust prob=0_05 for other settings
//             x_cpt_S4 = sort(runif(n_mut,min=x_min+x_inc,max=x_max-x_inc));
//             n_cpt_S4 = length(x_cpt_S4);
//         } else {
// // Step 2: Thinning
//           ran_val_S2 = runif(n_cpt_S1,min=0,max=1);
//           x_cpt_S2 = x_cpt_S1[ran_val_S2 <= 0_5];
//           n_cpt_S2 = length(x_cpt_S2);
// // Step 3: Shifting
//           ran_val_S3 = rnorm(n_cpt_S2,mean=0,sd=x_inc);             // [!CAUTION!] Maybe related to early convergence
//           x_cpt_S3_tmp = sort(unique(x_cpt_S2+ran_val_S3));
//           x_cpt_S3 = x_cpt_S3_tmp[x_min+x_inc < x_cpt_S3_tmp &
//             x_cpt_S3_tmp < x_max-x_inc];      // Changepoints must occur in (x_min+1,x_max-1)
//           n_cpt_S3 = length(x_cpt_S3);
// // Step 4: Mutation
//           n_mut = rbinom(1,size=2,prob=p_mut);                      // [!CAUTION!] Adjust prob=0_05 for other settings
//             x_cpt_S4_mut = runif(n_mut,min=x_min+x_inc,max=x_max-x_inc);
//             x_cpt_S4 = sort(unique(c(x_cpt_S3,x_cpt_S4_mut)));
//             n_cpt_S4 = length(x_cpt_S4)
//         }
//         n_cpt = n_cpt_S4;                            // number of changepoints
//           x_cpt = x_cpt_S4;
//           chrom = c(n_cpt,x_cpt);                      // changepoint locations (m; xi_1,___,xi_m)
//           Confg[[j]] = chrom;
// 
// // Check conditions, discard chromosome and rerun loop if not met
//           is_pass = FALSE;
//             if (n_cpt == 0) {
//               is_pass = TRUE;
//             } else {
//               if (all(diff(c(x_min,x_cpt,x_max)) > x_inc) & // do not allow a changepoint within x_inc of x
//                 x_min < min(x_cpt) &                   // smallest location shold be > x_min
//                 max(x_cpt) < x_max) {                  // greatest location shold be < x_max
//                 is_pass = TRUE;
//               }
//             }
//             if (length(unique(Confg[1:j])) == j & is_pass == TRUE) {
//               j = j+1;                                   // generation increases when (1) a new child chrom is born
//             }                                            //                       and (2) the above condition is met
// 
//     } // End loop
//       return Confg;
//   }

//   
// ### Utility Function to export GA ouput (called in ga.cpt_Norm)
//   export = function(g,Confg, Pnlik, WD.out){
// ## Args refrence those in ga.cpt_Norm
//     
//     capture.output(Confg,file=paste(WD.out,sprintf("GA-Gen_%03d.txt",g),sep=""),append=FALSE)
//     write.table(t(format(Pnlik[g,],nsmall=12)),file=paste(WD.out,"GA-Pnlik.csv",sep=""),
//                 sep=",",quote=FALSE,row.names=FALSE,col.names=FALSE,append=TRUE)
//   }
//   
//   
//   
////// Main Function to Execute GA for changepoint detection

// [[Rcpp::export]]
// List ga_cpt_Norm(y, x , fitness, p_mut, z = NULL, x_min = NULL, x_max = NULL, x_inc = 30,
//                           gen_size = 200, max_itr = 150, seed=2244, max_cpt = 10,
//                           is_graphic = FALSE, is_print = TRUE, is_export = FALSE,
//                           WD_out = NULL) {
// //// Args:
// // y: Vector of y values
// // x: coressponding vector of x values
// // fitness: a valid fitness function
// // p_mut: mutation probabilty, numeric [0,1]
// // x_min: Minimum value of x to consider
// // x_max: Maximum value of x to consider
// // x_inc: Denominator of (x_max - x_min)/x_inc, determines regime width, numeric > 1
// // gen_size: Size of each generation of chromosomes, int
// // max_itr: Number of generations to consider, int
// // seed: seed for random values, int
// // max_cpt: maximum number of changepoints to consider, int
// // is_grphic: graphical display of results if true, boolean
// // is_priont: print reusluts if true, boolean
// // is_export: export results to drive if true, boolean
// // WD_out: working directory for export file, string
// 
//     
// 
// //// Define range of x values
//       if(is_null(x_min)){
//         x_min = min(x,na_rm=TRUE);
//       }
//       if(is_null(x_max)){
//         x_max = max(x,na_rm=TRUE);
//       }
//       x_inc = (x_max-x_min)/x_inc;
// 
// //// Changepoint configuration data structures
//         Confg = List::create();                                  // changepoint configuration for a generation
//           Confg_sol = List::create();                             // best changepoint configuration for a generation
//             Confg_ALL =List::create();                              // changepoint configuration for all generations
//               Pnlik = matrix(0,nrow=max_itr,ncol=gen_size)    // penalized likelihood for all chagenpoint configurations
//                 Pnlik_sol = numeric()                           // smallest penalized likelihood value for each generation
// 
// //// Initial generation
// // Generate inital generation
//                   Confg = initial_population(x = x, x_min = x_min, x_max = x_max, x_inc = x_inc,
//                                              gen_size = gen_size, max_cpt = max_cpt,
//                                              is_print=is_print, is_graphic= is_graphic)
// // Get and store MLE estimates for Initial generation
//                     Pnlik[1,] = mle(y = y, x = x, x_min=x_min, x_max=x_max, x_inc=x_inc, g=1, fitness = fitness,
//                                     z = z, Confg=Confg,  gen_size = gen_size)
//                     loc_sol = which(Pnlik[1,] == min(Pnlik[1,]))
// // print(loc_sol)
//                     chrom_sol = Confg[[loc_sol]]
//                   Confg_sol[[1]] = chrom_sol
//                     Confg_ALL[[1]] = Confg
//                     Pnlik_sol[1] = Pnlik[1,loc_sol]
//                   if (is_export) {export(1,Confg, Pnlik, WD_out)}
// 
// //// Loop through generations from 2 to gen_size
//                   for (g in 2:max_itr) {
//                     if (is_print) print(paste("//----------[  Generation =",g,"has begun at",Sys_time()," ]----------//"))
// 
// // Rank chromosomes in the (g-1)th generation
//                       gen_rank = rank(-Pnlik[g-1,])
//                         gen_rank_sum = sum(gen_rank)
//                         probs = gen_rank/gen_rank_sum
// 
// // Generate g-th generation: the fittest chromosome carries over to next generation
//                         Confg_pre = Confg_ALL[[g-1]]
//                         Confg_pre_sol = Confg_sol[[g-1]]
//                         Confg_pre_sol_lik = Pnlik_sol[g-1]
//                         Confg = next_gen(Confg_pre, Confg_pre_sol, probs, x_min, x_max, x_inc, gen_size, p_mut)
// 
// //Get and Store ML estimates for geration g
//                           Pnlik[g,] = mle(y = y, x = x, x_min=x_min, x_max=x_max, x_inc=x_inc,
//                                           g=g, z = z, Confg=Confg,  gen_size = gen_size,
//                                           fitness=fitness, is_graphic = is_graphic,
//                                           Confg_pre_sol_lik=Confg_pre_sol_lik )
//                             loc_sol = which(Pnlik[g,] == min(Pnlik[g,]))
//                             chrom_sol = Confg[[loc_sol]]
//                           Confg_sol[[g]] = chrom_sol
//                             Confg_ALL[[g]] = Confg
//                             Pnlik_sol[g] = Pnlik[g,loc_sol]
// 
// // Print and export ouput for generation g
//                           if (is_print) {
//                             print(chrom_sol)
//                             print(paste("MDL =",format(Pnlik_sol[g],nsmall=3)),quote=FALSE)
//                           }
//                           if (is_export) {export(1,Confg, Pnlik, WD_out)}
//                   } // Ending loop in g
// 
// //// Return Value
//                     list(gen_all=Confg_ALL,gen_sol=Confg_sol,val_all=Pnlik,val_sol=Pnlik_sol,solution=chrom_sol)
//   }  // Ending function: ga_cpt

    
  