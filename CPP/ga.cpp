#include <Rcpp.h>
using namespace Rcpp;

// [-----------------------------------------------------------------------------------------------]
//  GA for a continuous covariate x in a simple normal regression model
// [-----------------------------------------------------------------------------------------------]


//  Utility Function: Get ML estimates for a given generation of chromosomes (called in ga.cpt_Norm)
// mle = function(y, x, x.min, x.max, x.inc, g, Confg,  fitness, z = null, gen.size = 200,
//                is.graphic = FALSE, Confg.pre.sol.lik= NULL ){
//   ## Args refrence those in ga.cpt_Norm
  
//   vals =  matrix(0,nrow=1,ncol=gen.size)               
//   if(g==1){ 
//     start.pos = 1
//   } else {
//     start.pos = 2
//     vals[1,1] = Confg.pre.sol.lik
//   }
  
//   # Loop through generation of chromosomes
//   for (j in start.pos:gen.size) {                        
//     chrom <- Confg[[j]]
//     vals[1,j] <- fitness(y=y,z=z,x=x,cp=chrom,x.min=x.min,x.max=x.max,x.inc=x.inc)
    
//     # Graph ML estimates if is.graphic == TRUE
//     if (is.graphic) {
//       if(g==1){ # Special case for initial generation
//         plot(x,y,xlab="X",col="gray",
//            main=paste("Generation",g,"& Child",j,"( PLKHD =",format(val[1,j],nsmall=3),")"))
//         abline(v=chrom[-1],col="blue",lty=2)
//       } else{
//         plot(x,y,xlab="X",col="gray",main=paste("Solution in Generation",g-1,
//               "( PLKHD =",format(Confg.pre.sol.lik,nsmall=3),") vs",
//               "Generation",g,"& Child",j,"( PLKHD =",
//               format(vals[1,j],nsmall=3),")"))
//         abline(v=chrom.sol[-1],col="red",lty=1)
//         abline(v=chrom[-1],col="blue",lty=2)
//       }
//     }
//   } # end loop
//   return(vals)
// } # End function mle


// ### Utility Function: Generate initial generation from random values (called in ga.cpt_Norm)
// initial_population  = function(x, x.min, x.max, x.inc, gen.size, max.cpt, 
//                                is.print=TRUE, is.graphic= FALSE){
//   ## Args refrence those in ga.cpt_Norm
  
//   if (is.print) print(paste("#----------[  Generation =",1,"has begun at",Sys.time()," ]----------#"))
//   Confg <- list()    
//   # A chromosome of no changepoints is always considered
//   Confg[[1]] <- 0                                
//   j <- 2                                         
//   while(j <= gen.size) {
//     # [!CAUTION!] Adjust prob=0.4 for other settings
//     n.cpt <- rbinom(1,size=max.cpt,prob=0.4)     
//     # Changepoints will occur between x.min+x.inc and x.max-x.inc
//     x.cpt <- sort(runif(n.cpt,min=x.min+x.inc,max=x.max-x.inc)) 
//     chrom <- c(n.cpt,x.cpt)                      
//     Confg[[j]] <- chrom
    
//     # Check conditions, discard chrom if not met
//     is.pass <- FALSE
//     if (n.cpt == 0) {
//       is.pass <- TRUE
//     } else {
//       if (all(diff(c(x.min,x.cpt,x.max)) > x.inc) & # do not allow a changepoint within x.inc of x
//           x.min < min(x.cpt) &                      # smallest location shold be > x.min
//           max(x.cpt) < x.max) {                     # greatest location shold be < x.max
//         is.pass <- TRUE
//       }
//     }
//     if (length(unique(Confg[1:j])) == j & is.pass == TRUE) {
//       j <- j+1                                   # generation increases when (1) a new child chrom is born
//     }                                            #                       and (2) the above condition is met  
//   } # End loop                                            
//   return(Confg)
// }

// ### Utility Function: Generate next gneration based on previous ML estimates (called in ga.cpt_Norm)
// next_gen = function(Confg.pre, Confg.pre.sol, probs, x.min, x.max, x.inc, gen.size, p.mut){
//   ## Args refrence those in ga.cpt_Norm
  
//   Confg = list()
//   Confg[[1]] = Confg.pre.sol                    # Include previous solution 
//   j <- 2                                           
//   while(j <= gen.size) {
//     ## Select father and mother chromosomes
//     loc.prt <- sample(1:gen.size,size=2,replace=FALSE,prob=probs)
//     loc.dad <- loc.prt[1]
//     loc.mom <- loc.prt[2]
//     chrom.dad <- Confg.pre[[loc.dad]]
//     chrom.mom <- Confg.pre[[loc.mom]]
    
//     ## Produce child chromosomes
//     # Step 1: Combining
//     x.cpt_S1 <- sort(union(chrom.dad[-1],chrom.mom[-1]))        # Do not allow identical chagepoint times
//     n.cpt_S1 <- length(x.cpt_S1)
//     if (n.cpt_S1 == 0) {
//       # Step 2: Thinning (SKIP!!!)
//       # Step 3: Shifting (SKIP!!!)
//       # Step 4: Mutation
//       n.mut <- rbinom(1,size=2,prob=p.mut)                      # [!CAUTION!] Adjust prob=0.05 for other settings
//       x.cpt_S4 <- sort(runif(n.mut,min=x.min+x.inc,max=x.max-x.inc))
//       n.cpt_S4 <- length(x.cpt_S4)
//     } else {
//       # Step 2: Thinning
//       ran.val_S2 <- runif(n.cpt_S1,min=0,max=1)
//       x.cpt_S2 <- x.cpt_S1[ran.val_S2 <= 0.5]
//       n.cpt_S2 <- length(x.cpt_S2)
//       # Step 3: Shifting
//       ran.val_S3 <- rnorm(n.cpt_S2,mean=0,sd=x.inc)             # [!CAUTION!] Maybe related to early convergence
//       x.cpt_S3.tmp <- sort(unique(x.cpt_S2+ran.val_S3))
//       x.cpt_S3 <- x.cpt_S3.tmp[x.min+x.inc < x.cpt_S3.tmp & 
//                                  x.cpt_S3.tmp < x.max-x.inc]      # Changepoints must occur in (x.min+1,x.max-1)
//       n.cpt_S3 <- length(x.cpt_S3)
//       # Step 4: Mutation
//       n.mut <- rbinom(1,size=2,prob=p.mut)                      # [!CAUTION!] Adjust prob=0.05 for other settings
//       x.cpt_S4.mut <- runif(n.mut,min=x.min+x.inc,max=x.max-x.inc)
//       x.cpt_S4 <- sort(unique(c(x.cpt_S3,x.cpt_S4.mut)))
//       n.cpt_S4 <- length(x.cpt_S4)
//     }
//     n.cpt <- n.cpt_S4                            # number of changepoints
//     x.cpt <- x.cpt_S4
//     chrom <- c(n.cpt,x.cpt)                      # changepoint locations (m; xi_1,...,xi_m)
//     Confg[[j]] <- chrom
    
//     # Check conditions, discard chromosome and rerun loop if not met
//     is.pass <- FALSE
//     if (n.cpt == 0) {
//       is.pass <- TRUE
//     } else {
//       if (all(diff(c(x.min,x.cpt,x.max)) > x.inc) & # do not allow a changepoint within x.inc of x
//           x.min < min(x.cpt) &                   # smallest location shold be > x.min
//           max(x.cpt) < x.max) {                  # greatest location shold be < x.max
//         is.pass <- TRUE
//       }
//     }
//     if (length(unique(Confg[1:j])) == j & is.pass == TRUE) {
//       j <- j+1                                   # generation increases when (1) a new child chrom is born
//     }                                            #                       and (2) the above condition is met  
    
//   } # End loop
//   return(Confg)
// }


// ### Utility Function to export GA ouput (called in ga.cpt_Norm)
// export = function(g,Confg, Pnlik, WD.out){
//   ## Args refrence those in ga.cpt_Norm
  
//   capture.output(Confg,file=paste(WD.out,sprintf("GA-Gen_%03d.txt",g),sep=""),append=FALSE)
//   write.table(t(format(Pnlik[g,],nsmall=12)),file=paste(WD.out,"GA-Pnlik.csv",sep=""),
//               sep=",",quote=FALSE,row.names=FALSE,col.names=FALSE,append=TRUE)
// }



// ### Main Function to Execute GA for changepoint detection
// ga.cpt_Norm <- function(y, x , fitness, p.mut, z = NULL, x.min = NULL, x.max = NULL, x.inc = 30,
//                         gen.size = 200, max.itr = 150, seed=2244, max.cpt = 10, 
//                         is.graphic = FALSE, is.print = TRUE, is.export = FALSE,
//                         WD.out = NULL) {
//   ## Args:
//   # y: Vector of y values
//   # x: coressponding vector of x values
//   # fitness: a valid fitness function
//   # p.mut: mutation probabilty, numeric [0,1]
//   # x.min: Minimum value of x to consider
//   # x.max: Maximum value of x to consider
//   # x.inc: Denominator of (x.max - x.min)/x.inc, determines regime width, numeric > 1
//   # gen.size: Size of each generation of chromosomes, int
//   # max.itr: Number of generations to consider, int
//   # seed: seed for random values, int
//   # max.cpt: maximum number of changepoints to consider, int
//   # is.grphic: graphical display of results if true, boolean
//   # is.priont: print reusluts if true, boolean
//   # is.export: export results to drive if true, boolean
//   # WD.out: working directory for export file, string
  
//   if (is.graphic) {dev.new(width=10,height=8)}
//   set.seed(seed)
  
//   ## Define range of x values
//   if(is.null(x.min)){
//     x.min <- min(x,na.rm=TRUE)
//   }
//   if(is.null(x.max)){
//     x.max <- max(x,na.rm=TRUE)
//   }
//   x.inc <- (x.max-x.min)/x.inc
  
//   ## Changepoint configuration data structures
//   Confg <- list()                                  # changepoint configuration for a generation
//   Confg.sol <- list()                              # best changepoint configuration for a generation
//   Confg.ALL <- list()                              # changepoint configuration for all generations
//   Pnlik <- matrix(0,nrow=max.itr,ncol=gen.size)    # penalized likelihood for all chagenpoint configurations
//   Pnlik.sol <- numeric()                           # smallest penalized likelihood value for each generation
  
//   ## Initial generation   
//   # Generate inital generation
//   Confg = initial_population(x = x, x.min = x.min, x.max = x.max, x.inc = x.inc,
//                              gen.size = gen.size, max.cpt = max.cpt, 
//                              is.print=is.print, is.graphic= is.graphic)
//   # Get and store MLE estimates for Initial generation
//   Pnlik[1,] = mle(y = y, x = x, x.min=x.min, x.max=x.max, x.inc=x.inc, g=1, fitness = fitness,
//                   z = z, Confg=Confg,  gen.size = gen.size)
//   loc.sol <- which(Pnlik[1,] == min(Pnlik[1,]))
//   # print(loc.sol)
//   chrom.sol <- Confg[[loc.sol]]
//   Confg.sol[[1]] <- chrom.sol
//   Confg.ALL[[1]] <- Confg
//   Pnlik.sol[1] <- Pnlik[1,loc.sol]
//   if (is.export) {export(1,Confg, Pnlik, WD.out)}

//   ## Loop through generations from 2 to gen.size
//   for (g in 2:max.itr) {
//     if (is.print) print(paste("#----------[  Generation =",g,"has begun at",Sys.time()," ]----------#"))
    
//     # Rank chromosomes in the (g-1)th generation
//     gen.rank <- rank(-Pnlik[g-1,])
//     gen.rank.sum <- sum(gen.rank)
//     probs = gen.rank/gen.rank.sum
    
//     # Generate g-th generation: the fittest chromosome carries over to next generation
//     Confg.pre <- Confg.ALL[[g-1]]
//     Confg.pre.sol <- Confg.sol[[g-1]]
//     Confg.pre.sol.lik = Pnlik.sol[g-1]
//     Confg = next_gen(Confg.pre, Confg.pre.sol, probs, x.min, x.max, x.inc, gen.size, p.mut)
    
//     #Get and Store ML estimates for geration g
//     Pnlik[g,] = mle(y = y, x = x, x.min=x.min, x.max=x.max, x.inc=x.inc,
//                     g=g, z = z, Confg=Confg,  gen.size = gen.size,
//                     fitness=fitness, is.graphic = is.graphic, 
//                     Confg.pre.sol.lik=Confg.pre.sol.lik )
//     loc.sol <- which(Pnlik[g,] == min(Pnlik[g,]))
//     chrom.sol <- Confg[[loc.sol]]
//     Confg.sol[[g]] <- chrom.sol
//     Confg.ALL[[g]] <- Confg
//     Pnlik.sol[g] <- Pnlik[g,loc.sol]
    
//     # Print and export ouput for generation g
//     if (is.print) {
//       print(chrom.sol)
//       print(paste("MDL =",format(Pnlik.sol[g],nsmall=3)),quote=FALSE)
//     }
//     if (is.export) {export(1,Confg, Pnlik, WD.out)}
//   } # Ending loop in g
  
//   ## Return Value
//   list(gen.all=Confg.ALL,gen.sol=Confg.sol,val.all=Pnlik,val.sol=Pnlik.sol,solution=chrom.sol)
// }  # Ending function: ga.cpt













