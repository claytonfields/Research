#*[-----------------------------------------------------------------------------------------------]*#
### GA for a continuous covariate x in a simple normal regression model
#*[-----------------------------------------------------------------------------------------------]*#

ga.cpt_Norm <- function(y,z,x,x.min,x.max,x.inc,
                        fitness,gen.size,max.itr,p.mut,
                        seed,is.graphic,is.print,is.export) {
  # Changepoint configuration
  max.cpt <- 10                                    # max number of possible changepoints

  Confg <- list()                                  # changepoint configuration for a generation
  Confg.sol <- list()                              # best changepoint configuration for a generation
  Confg.ALL <- list()                              # changepoint configuration for all generations

  Pnlik <- matrix(0,nrow=max.itr,ncol=gen.size)    # penalized likelihood for all chagenpoint configurations
  Pnlik.sol <- numeric()                           # smallest penalized likelihood value for each generation

  if (is.graphic) {
    dev.new(width=10,height=8)
  }

  # Initial generation
  set.seed(seed)
  for (g in 1:1) {
    if (is.print) print(paste("#----------[  Generation =",g,"has begun at",Sys.time()," ]----------#"))

    Confg[[1]] <- 0                                # A chromosome of no changepoints is always considered
    j <- 2                                         # loop index for generation
    for (k in 1:(gen.size*1000)) {                 # This loop still works both when n.cpt>=1 and n.cpt=0
      n.cpt <- rbinom(1,size=max.cpt,prob=0.4)     # [!CAUTION!] Adjust prob=0.4 for other settings
      x.cpt <- sort(runif(n.cpt,min=x.min+x.inc,max=x.max-x.inc)) # [!CAUTION!] Changepoints will occur 
                                                                  #             between x.min+x.inc and x.max-x.inc
      chrom <- c(n.cpt,x.cpt)                      # changepoint locations (m; xi_1,...,xi_m)
      Confg[[j]] <- chrom

      is.pass <- FALSE
      if (n.cpt == 0) {
        is.pass <- TRUE
      } else {
        if (all(diff(c(x.min,x.cpt,x.max)) > x.inc) & # do not allow a changepoint within x.inc of x
            x.min < min(x.cpt) &                   # smallest location shold be > x.min
            max(x.cpt) < x.max) {                  # greatest location shold be < x.max
          is.pass <- TRUE
        }
      }

      if (length(unique(Confg[1:j])) == j & is.pass == TRUE) {
        j <- j+1                                   # generation increases when (1) a new child chrom is born
      }                                            #                       and (2) the above condition is met  

      if (j > gen.size) break                      # Produce a generation of gen.size
    }                                              # Ending loop in k

    # ML estimation
    for (j in 1:gen.size) {                        # loop index for generation
      chrom <- Confg[[j]]

      Pnlik[g,j] <- fitness(y=y,z=z,x=x,cp=chrom)

      if (is.graphic) {
        plot(x,y,xlab="X",col="gray",
             main=paste("Generation",g,"& Child",j,"( PLKHD =",format(Pnlik[g,j],nsmall=3),")"))
        abline(v=chrom[-1],col="blue",lty=2)
      }
    }

    loc.sol <- which(Pnlik[g,] == min(Pnlik[g,]))
    chrom.sol <- Confg[[loc.sol]]
    Confg.sol[[g]] <- chrom.sol
    Confg.ALL[[g]] <- Confg
    Pnlik.sol[g] <- Pnlik[g,loc.sol]

    if (is.export) {
      capture.output(Confg,file=paste(WD.out,sprintf("GA-Gen_%03d.txt",g),sep=""),append=FALSE)
      write.table(t(format(Pnlik[g,],nsmall=12)),file=paste(WD.out,"GA-Pnlik.csv",sep=""),
                  sep=",",quote=FALSE,row.names=FALSE,col.names=FALSE,append=FALSE)
    }
  }                                                # Ending loop in g

  # Next generations from 2 to gen.size
  for (g in 2:max.itr) {
    if (is.print) print(paste("#----------[  Generation =",g,"has begun at",Sys.time()," ]----------#"))

    # Rank chromosomes in the (g-1)th generation
    gen.rank <- rank(-Pnlik[g-1,])
    gen.rank.sum <- sum(gen.rank)

    # Generate g-th generation: the fittest chromosome carries over to next generation
    Confg.pre <- Confg.ALL[[g-1]]
    Confg[[1]] <- Confg.sol[[g-1]]
    Pnlik[g,1] <- Pnlik.sol[g-1]

    j <- 2                                         # index for child in a generation
    for (k in 2:(gen.size*1000)) {
      # Select father and mother chromosomes
      loc.prt <- sample(1:gen.size,size=2,replace=FALSE,prob=gen.rank/gen.rank.sum)
      loc.dad <- loc.prt[1]
      loc.mom <- loc.prt[2]
      chrom.dad <- Confg.pre[[loc.dad]]
      chrom.mom <- Confg.pre[[loc.mom]]

      # Producing child chromosomes
      # Step 1: Combining
      x.cpt_S1 <- sort(union(chrom.dad[-1],chrom.mom[-1]))        # Do not allow identical chagepoint times
      n.cpt_S1 <- length(x.cpt_S1)
      if (n.cpt_S1 == 0) {
        # Step 2: Thinning (SKIP!!!)
        # Step 3: Shifting (SKIP!!!)
        # Step 4: Mutation
        n.mut <- rbinom(1,size=2,prob=p.mut)                      # [!CAUTION!] Adjust prob=0.05 for other settings
        x.cpt_S4 <- sort(runif(n.mut,min=x.min+x.inc,max=x.max-x.inc))
        n.cpt_S4 <- length(x.cpt_S4)
      } else {
        # Step 2: Thinning
        ran.val_S2 <- runif(n.cpt_S1,min=0,max=1)
        x.cpt_S2 <- x.cpt_S1[ran.val_S2 <= 0.5]
        n.cpt_S2 <- length(x.cpt_S2)

        # Step 3: Shifting
        ran.val_S3 <- rnorm(n.cpt_S2,mean=0,sd=x.inc)             # [!CAUTION!] Maybe related to early convergence
        x.cpt_S3.tmp <- sort(unique(x.cpt_S2+ran.val_S3))
        x.cpt_S3 <- x.cpt_S3.tmp[x.min+x.inc < x.cpt_S3.tmp & 
                                 x.cpt_S3.tmp < x.max-x.inc]      # Changepoints must occur in (x.min+1,x.max-1)
        n.cpt_S3 <- length(x.cpt_S3)

        # Step 4: Mutation
        n.mut <- rbinom(1,size=2,prob=p.mut)                      # [!CAUTION!] Adjust prob=0.05 for other settings
        x.cpt_S4.mut <- runif(n.mut,min=x.min+x.inc,max=x.max-x.inc)
        x.cpt_S4 <- sort(unique(c(x.cpt_S3,x.cpt_S4.mut)))
        n.cpt_S4 <- length(x.cpt_S4)
      }

      n.cpt <- n.cpt_S4                            # number of changepoints
      x.cpt <- x.cpt_S4
      chrom <- c(n.cpt,x.cpt)                      # changepoint locations (m; xi_1,...,xi_m)
      Confg[[j]] <- chrom

      is.pass <- FALSE
      if (n.cpt == 0) {
        is.pass <- TRUE
      } else {
        if (all(diff(c(x.min,x.cpt,x.max)) > x.inc) & # do not allow a changepoint within x.inc of x
            x.min < min(x.cpt) &                   # smallest location shold be > x.min
            max(x.cpt) < x.max) {                  # greatest location shold be < x.max
          is.pass <- TRUE
        }
      }

      if (length(unique(Confg[1:j])) == j & is.pass == TRUE) {
        j <- j+1                                   # generation increases when (1) a new child chrom is born
      }                                            #                       and (2) the above condition is met  

      if (j > gen.size) break                      # Produce a generation of gen.size
    }                                              # Ending loop in k

    # ML estimation
    for (j in 2:gen.size) {                        # loop index for generation
      chrom <- Confg[[j]]

      Pnlik[g,j] <- fitness(y=y,z=z,x=x,cp=chrom)

      if (is.graphic) {
        plot(x,y,xlab="X",col="gray",
             main=paste("Solution in Generation",g-1,
                        "( PLKHD =",format(Pnlik.sol[g-1],nsmall=3),") vs",
                        "Generation",g,"& Child",j,
                        "( PLKHD =",format(Pnlik[g,j],nsmall=3),")"))
        abline(v=chrom.sol[-1],col="red",lty=1)
        abline(v=chrom[-1],col="blue",lty=2)
      }
    }

    loc.sol <- which(Pnlik[g,] == min(Pnlik[g,]))
    chrom.sol <- Confg[[loc.sol]]
    Confg.sol[[g]] <- chrom.sol
    Confg.ALL[[g]] <- Confg
    Pnlik.sol[g] <- Pnlik[g,loc.sol]

    if (is.print) {
      print(c(k,j))
      print(chrom.sol)
      print(paste("MDL =",format(Pnlik.sol[g],nsmall=3)),quote=FALSE)
    }

    if (is.export) {
      capture.output(Confg,file=paste(WD.out,sprintf("GA-Gen_%03d.txt",g),sep=""),append=FALSE)
      write.table(t(format(Pnlik[g,],nsmall=12)),file=paste(WD.out,"GA-Pnlik.csv",sep=""),
                  sep=",",quote=FALSE,row.names=FALSE,col.names=FALSE,append=TRUE)
    }
  }                                                # Ending loop in g

  list(gen.all=Confg.ALL,gen.sol=Confg.sol,val.all=Pnlik,val.sol=Pnlik.sol,solution=chrom.sol)
}                                                  # Ending function: ga.cpt

