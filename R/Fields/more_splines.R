set.seed(0)

library(splines)

x = seq( 0, 4*pi, length.out=50 )
y = cos( x ) + 0.3 * rnorm( length(x) )


#postscript("../../WriteUp/Graphics/Chapter5/dup_fig_5_1_m1.eps", onefile=FALSE, horizontal=FALSE)
plot( x, y, type="p" );
m1 = lm( y ~ bs(x,degree=1,df=1) )
lines( x, fitted(m1) )
#dev.off()


#postscript("../../WriteUp/Graphics/Chapter5/dup_fig_5_1_m2.eps", onefile=FALSE, horizontal=FALSE)
plot( x, y, type="p" );
m2 = lm( y ~ bs(x,degree=1,df=2) )
lines( x, fitted(m2) )
#dev.off()


#postscript("../../WriteUp/Graphics/Chapter5/dup_fig_5_1_m3.eps", onefile=FALSE, horizontal=FALSE)
plot( x, y, type="p" );
m3 = lm( y ~ bs(x,degree=1,df=3) )
lines( x, fitted(m3) )
#dev.off()


#postscript("../../WriteUp/Graphics/Chapter5/dup_fig_5_1_m4.eps", onefile=FALSE, horizontal=FALSE)
plot( x, y, type="p" );
m4 = lm( y ~ bs(x,degree=2,df=2) )
lines( x, fitted(m4) )
#dev.off()


#postscript("../../WriteUp/Graphics/Chapter5/dup_fig_5_1_m5.eps", onefile=FALSE, horizontal=FALSE)
plot( x, y, type="p" );
m5 = lm( y ~ bs(x,degree=2,df=3) )
lines( x, fitted(m5) )
#dev.off()


#postscript("../../WriteUp/Graphics/Chapter5/dup_fig_5_1_m6.eps", onefile=FALSE, horizontal=FALSE)
plot( x, y, type="p" );
m6 = lm( y ~ bs(x,degree=2,df=4) )
lines( x, fitted(m6) )
#dev.off()

library(ElemStatLearn) # loads the data frame SAheart
library(splines)

# Computes the logistic regression model using natural splines (note famhist is included as a factor): 
form = "chd ~ ns(sbp,df=4) + ns(tobacco,df=4) + ns(ldl,df=4) + famhist + ns(obesity,df=4) + ns(alcohol,df=4) + ns(age,df=4)"
form = formula(form)

m = glm( form, data=SAheart, family=binomial )
print( summary(m), digits=3 )

# Duplicates the numbers from Table 5.1:
# 
drop1( m, scope=form, test="Chisq" )


set.seed(0)

library(ElemStatLearn) # loads the data frame phoneme

aa_spot = phoneme$g == "aa"
ao_spot = phoneme$g == "ao"

aa_indx = which( aa_spot )
ao_indx = which( ao_spot ) 

# Lets plot some examples of the two phonemes :
# 
AA_data = phoneme[ aa_indx[1:15], 1:256 ]
AO_data = phoneme[ ao_indx[1:15], 1:256 ]

# Compute the ylimits :
#
min_l = min( c( min( AA_data ), min( AO_data ) ) )
max_l = max( c( max( AA_data ), max( AO_data ) ) )

#postscript("../../WriteUp/Graphics/Chapter5/dup_fig_5_5_top.eps", onefile=FALSE, horizontal=FALSE)

ii=1
plot( as.double(AA_data[ii,]), ylim=c(min_l,max_l), type="l", col="green", xlab="Frequency", ylab="Log-periodogram" )
for( ii in 2:dim(AA_data)[1] ){
  lines( as.double(AA_data[ii,]), col="green" )
}
for( ii in 1:dim(AO_data)[1] ){
  lines( as.double(AO_data[ii,]), col="orange" )
}

#dev.off()

# Lets attempt a naive logistic regression classifier to classify aa's from ao's:
#

# First get training/testing data:
# 
AA_data = phoneme[ aa_indx, ]
train_inds = grep( "^train", AA_data$speaker )
test_inds = grep( "^test", AA_data$speaker ) 
AA_data_train = AA_data[ train_inds, 1:256 ]
AA_data_test = AA_data[ test_inds, 1:256 ]

n_aa = dim(AA_data_train)[1]
AA_data_train$Y = rep( 1, n_aa ) # call this class 1
n_aa = dim(AA_data_test)[1]
AA_data_test$Y = rep( 1, n_aa )

AO_data = phoneme[ ao_indx, ]
train_inds = grep( "^train", AO_data$speaker )
test_inds = grep( "^test", AO_data$speaker ) 
AO_data_train = AO_data[ train_inds, 1:256 ]
AO_data_test = AO_data[ test_inds, 1:256 ]

n_ao = dim(AO_data_train)[1]
AO_data_train$Y = rep( 0, n_ao ) # call this class 0
n_ao = dim(AO_data_test)[1]
AO_data_test$Y = rep( 0, n_ao )

DT_train = rbind( AA_data_train, AO_data_train )
DT_test = rbind( AA_data_test, AO_data_test ) 

form = paste( "Y ~", paste( colnames( DT_train )[1:256], collapse="+" ) )
m = glm( form, family=binomial, data=DT_train )

# Plot the coefficients that the logistic regression fit found (to compare with the book): 
#
#postscript("../../WriteUp/Graphics/Chapter5/dup_fig_5_5_bot.eps", onefile=FALSE, horizontal=FALSE)

#mc = as.double( coefficients(m)[-1] ) # dont plot the intercept
mc = as.double( coefficients(m) )
plot( mc, ylim=c(-0.4,+0.4), type="l", xlab="Frequency", ylab="Logistic Regression Coefficients" ) 

#lines( smooth.spline(mc,df=15), col="green" )

#dev.off()

# How well does this logistic regression classifier we just trained work:
#
Y_hat_train = predict( m, DT_train[,1:256], type="response" )
predicted_class_label_train = as.double( Y_hat_train > 0.5 )

Y_hat_test  = predict( m, DT_test[,1:256], type="response" )
predicted_class_label_test = as.double( Y_hat_test > 0.5 )

err_rate_train = mean( DT_train[,257] != predicted_class_label_train )
err_rate_test  = mean( DT_test[,257] != predicted_class_label_test )
print( sprintf('Logistic Regression: err_rate_train= %10.6f; err_rate_test= %10.5f', err_rate_train, err_rate_test) )

# How well does a quadradic discriminant classifier work on this data:
#
library(MASS)
qdam = qda( DT_train[,1:256], DT_train[,257] )
predTrain = predict( qdam, DT_train[,1:256] )
predTest  = predict( qdam, DT_test[,1:256] )

err_rate_train = mean( predTrain$class != DT_train[,257] )
err_rate_test  = mean( predTest$class != DT_test[,257] )
print( sprintf('Quadradic Discriminant: err_rate_train= %10.6f; err_rate_test= %10.5f', err_rate_train, err_rate_test) )

# How well does regularized discriminant analysis do on this data set:
#
source('../Chapter4/repmat.R')
source('../Chapter4/rda.R')
source('../Chapter4/rda_alpha_gamma_CV.R')

n_aa = dim(AA_data_train)[1]; n_aa_train = floor( 0.9*n_aa )
n_ao = dim(AO_data_train)[1]; n_ao_train = floor( 0.9*n_ao )
rda_DT_train = rbind( AA_data_train[1:n_aa_train,], AO_data_train[1:n_ao_train,] )
rda_DT_cv = rbind( AA_data_train[n_aa_train:n_aa,], AO_data_train[n_ao_train:n_ao,] )
rda_DT_test = DT_test

X_train = rda_DT_train[,1:256]; y_train = rda_DT_train[,257]; y_train[ y_train==0 ] = 2
X_cv = rda_DT_cv[,1:256]; y_cv = rda_DT_cv[,257]; y_cv[ y_cv==0 ] = 2
X_test = rda_DT_test[,1:256]; y_test = rda_DT_test[,257]; y_test[ y_test==0 ] = 2 

best_params = rda_alpha_gamma_CV( X_train, y_train, X_cv, y_cv )
alpha = best_params[[1]]
gamma = best_params[[2]] 

# Refit our model on all data and look at performance:
# 
rdam = rda( rbind( X_train, X_cv ), c( y_train, y_cv ), X_test, y_test, alpha=alpha, gamma=gamma )
print( sprintf('Regularized Discriminant Analysis: err_rate_train= %10.6f; err_rate_test= %10.5f', rdam[[2]], rdam[[4]]) )


set.seed(0)

library(ElemStatLearn) # loads the data frame bone

#postscript("../../WriteUp/Graphics/Chapter5/dup_fig_5_6.eps", onefile=FALSE, horizontal=FALSE)

males = bone$gender == "male" 
females = bone$gender == "female"

boneMaleSmooth = smooth.spline( bone[males,"age"], bone[males,"spnbmd"], df=12 )
boneFemaleSmooth = smooth.spline( bone[females,"age"], bone[females,"spnbmd"], df=12 )

plot(boneMaleSmooth, ylim=c(-0.05,0.20), col="blue", type="l", xlab="Age", ylab="spnbmd")
points(bone[males,c(2,4)], col="blue", pch=20)

lines(boneFemaleSmooth, ylim=c(-0.05,0.20), col="red")
points(bone[females,c(2,4)], col="red", pch=20)

#dev.off()






