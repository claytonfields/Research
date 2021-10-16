#*[-----------------------------------------------------------------------------------------------]*#
#*[ Objective: This program applies the GA method to detect structural changes in a simple        ]*#
#*[            regression model setting.                                                          ]*#
#*[ Updated  : 07/30/2021                                                                         ]*#
#*[ Author   : Jaechoul Lee                                                                       ]*#
#*[-----------------------------------------------------------------------------------------------]*#

### Setup library, data, and output directories
# WD.lib <- c("L:/Home/JaechoulLee/!1Research/Paper/Epidemiology/P03_Splines/R_library/")
# WD.inp <- c("L:/Home/JaechoulLee/!1Research/Paper/Epidemiology/P03_Splines/Data/")
# WD.out <- c("L:/Home/JaechoulLee/!1Research/Paper/Epidemiology/P03_Splines/Application/")

WD.lib = c('Lee/')
WD.inp = c('data/')


### Load the proposed GA and fitted model likelihood funtion packages
source(file=paste(WD.lib,"lib_ga_v0-1-4.R",sep=""))
source(file=paste(WD.lib,"lib_mle_v0-2-1.R",sep=""))

#*[-----------------------------------------------------------------------------------------------]*#
### Case 1: Diabetes.csv
#*[-----------------------------------------------------------------------------------------------]*#

### Read the data (http://staff.pubhealth.ku.dk/~tag/Teaching/share/data/Diabetes.html)
data.db <- read.csv(file=paste(WD.inp,"diabetes2.csv",sep=""))
dim(data.db)
head(data.db)
summary(data.db)

### Subset the data to those with glyhb <= 8
data.db_glyhb <- data.db
# data.db_glyhb <- subset(data.db,glyhb<=8)

### Set the response and feature variables
y <- data.db_glyhb$waist/data.db_glyhb$hip*100            # y: waist/hip % ratio
x <- data.db_glyhb$glyhb                                  # x: Glycosolated Hemoglobin (feature for change)
Z <- cbind(data.db_glyhb$age,data.db_glyhb$ratio,data.db_glyhb$weight/data.db_glyhb$height) # Z: all other features

dev.new()
plot(data.frame(cbind(y,x,Z)))
cor(cbind(y,x,Z),use="pairwise.complete.obs")

### Find structural changes via the proposed GA method
x.min <- min(x,na.rm=TRUE)
x.max <- max(x,na.rm=TRUE)
x.inc <- (x.max-x.min)/50

i <- 1
seed_i <- 1000*(i-1)+543

ga.out <- ga.cpt_Norm(y=y,z=NULL,x=x,x.min=x.min,x.max=x.max,x.inc=x.inc,
                      fitness=pnllik.MDL.M0Z,
                      gen.size=100,max.itr=20,p.mut=0.05,seed=seed_i,
                      is.graphic=TRUE,is.print=TRUE,is.export=FALSE)

ga.out <- ga.cpt_Norm(y=y,z=Z,x=x,x.min=x.min,x.max=x.max,x.inc=x.inc,
                      fitness=pnllik.MDL.M0Z,
                      gen.size=100,max.itr=20,p.mut=0.05,seed=seed_i,
                      is.graphic=TRUE,is.print=TRUE,is.export=FALSE)

chrom.sol <- ga.out$solution
chrom.sol

#*[-----------------------------------------------------------------------------------------------]*#
### Case 2: mcycle.csv - Data from a Simulated Motorcycle Accident
###    http://rstudio-pubs-static.s3.amazonaws.com/11323_9d5a69cc3c784d3680b2f7243fd29370.html
### Description:
###    A data frame giving a series of measurements of head acceleration in a simulated motorcycle
###    accident, used to test crash helmets.
### Format:
###    ?times? in milliseconds after impact.
###    ?accel? in g.
### Source:
###    Silverman, B. W. (1985) Some aspects of the spline smoothing approach to non-parametric curve fitting.
###    Journal of the Royal Statistical Society series B 47, 1?52
###    http://www-personal.umich.edu/~jizhu/jizhu/wuke/Silverman-JRSSB85.pdf
#*[-----------------------------------------------------------------------------------------------]*#

### Read the data
data.cy <- read.csv(file=paste(WD.inp,"mcycle.csv",sep=""))
data.cy = mcycle
dim(data.cy)
head(data.cy)
summary(data.cy)

### Set the response and feature variablesli
y <- data.cy$accel                                       # y:
x <- data.cy$times                                       # x: times in milliseconds after impact

dev.new()
plot(x,y)
cor(cbind(y,x),use="pairwise.complete.obs")

### Find structural changes via the proposed GA method
x.min <- min(x,na.rm=TRUE)
x.max <- max(x,na.rm=TRUE)
x.inc <- (x.max-x.min)/25
x.inc <- (x.max-x.min)/30

i <- 13
seed_i <- 1000*(i-1)+543

ga.out <- ga.cpt_Norm(y=y,z=NULL,x=x,x.min=x.min,x.max=x.max,x.inc=x.inc,
                      fitness=pnllik.MDL.M0Z,
                      gen.size=200,max.itr=150,p.mut=0.30,seed=seed_i,
                      is.graphic=TRUE,is.print=TRUE,is.export=FALSE)

chrom.sol <- ga.out$solution
chrom.sol
# [1]  4.00000 14.35893 17.66042 23.00196 28.92551    MDL = 620.4726; i=5
# [1]  3.00000 14.01970 20.93885 30.23488             MDL = 621.1823; i=7
# [1]  4.00000 14.36528 17.51717 23.37114 28.91534    MDL = 621.1859; i=9
# [1]  3.00000 13.70090 20.97647 29.59870             MDL = 620.9944; i=13

#*[-----------------------------------------------------------------------------------------------]*#
### Case 3:Simulated Linear Data
### 
#*[-----------------------------------------------------------------------------------------------]*#
### Read the data
data.cy <- read.csv(file=paste(WD.inp,"sim_data_01.csv",sep=""))
dim(data.cy)
head(data.cy)
summary(data.cy)

### Set the response and feature variablesli
y <- data.cy$y                                # y
x <- data.cy$X                                # x

dev.new()
plot(x,y)
cor(cbind(y,x),use="pairwise.complete.obs")

### Find structural changes via the proposed GA method
x.min <- min(x,na.rm=TRUE)
x.max <- max(x,na.rm=TRUE)
x.inc <- (x.max-x.min)/25
x.inc <- (x.max-x.min)/30 

i <- 13
seed_i <- 1000*(i-1)+543

ga.out <- ga.cpt_Norm(y=y, x=x,fitness=pnllik.MDL.M0Z,gen.size=200, 
                      max.itr=150, p.mut=0.30, seed=seed_i)

chrom.sol <- ga.out$solution
chrom.sol












