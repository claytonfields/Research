#include <iostream>
#include <vector>
// int main() {
//     std::cout << "Hello World!\n";
//     return 0;
// } 

// [-----------------------------------------------------------------------------------------------]*#
// [ Objective: This program applies the GA method to detect structural changes in a simple        ]*#
// [            regression model setting.                                                          ]*#
// [ Updated  :                                                                                    ]*#
// [ Author   : Jaechoul Lee, Clayton Fields                                                       ]*#
// [-----------------------------------------------------------------------------------------------]*#
// [-----------------------------------------------------------------------------------------------]*#
// Case 1:Simulated Linear Data:
//  
// [-----------------------------------------------------------------------------------------------]*#
int main(){
// Linear, 2 changepoints
// Generate X

double sigma = 3;
int n = 1000;
double m_true = 2;

double xi_1 = 26;
double xi_2 = 70;
std::vector<double> xi = {m_true,xi_1,xi_2};

std::cout << "xi = { ";
for (int xi_i : xi) {
    std::cout << xi_i << ", ";
}
std::cout << "}; \n";
// X = seq(from=0, to=100, length.out = n)
// X1 = X[X < xi_1]
// X2 = X[X >= xi_1 & X < xi_2]
// X3 = X[X >= xi_2]

// y1 = -.6*X1 + 67.4
// y2 = 2*X2
// y3 = -.4*X3+168


// y_true = c(y1,y2,y3)
// eps = rnorm(n,0,3)

// y = y_true + eps


// df2 = tibble(X,y,y_true)
// ggplot(df2) + geom_point(aes(X,y), color='gray') + 
//   geom_line(aes(X,y_true), color='red')


// ### Find structural changes via the proposed GA method
// # x.min = min(X,na.rm=TRUE)
// # x.max = max(X,na.rm=TRUE)
// # x.inc = (x.max-x.min)/25
// # x.inc = (x.max-x.min)/30 

// start_pos = 101
// end_pos = 110

// results = data.frame(
//   i = integer(),
//   seed_i = integer(),
//   m = integer(),
//   c1 = numeric(),
//   c2 = numeric(),
//   c3 = numeric(),
//   c4 = numeric(),
//   c5 = numeric(),
//   c6 = numeric(),
//   c7 = numeric(),
//   c8 = numeric(),
//   c9 = numeric(),
//   c10 = numeric()
// )

// p.mut = .01
// max.itr = 150
// x.inc = 45

// for(i in start_pos:end_pos){
//   begin = proc.time()  
//   seed_i = 1000*(i-1)+543
//   eps = rnorm(n,0,sigma)
//   y = y_true + eps
//   print(paste('Iteration',i))
  
//   ga.out = ga.cpt_Norm(y=y, x=X,fitness=pnllik.MDL.M0Z, p.mut=p.mut, x.inc=x.inc,
//                        max.itr=max.itr,seed=seed_i, is.print = FALSE)
//   elapsed = proc.time() - begin
//   print(elapsed)
//   chrom.sol = ga.out$solution
//   m = chrom.sol[1]
//   row = c(i,seed_i,chrom.sol,rep(0,10-m))
//   results[nrow(results)+1,] = row
// }
}