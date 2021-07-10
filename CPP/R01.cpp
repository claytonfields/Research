#include <iostream>
#include <vector>
#include <numeric>


int main(){

// Intercepts
double alpha1 = 5;
double alpha2 = 35;
double alpha3 = 0;
//  Constant slope
double beta = .5;

// # intert1als for t
// t1 = 1:180
std::vector<int> t1(180) ; // t1ector with 100 ints.
std::iota (std::begin(t1), std::end(t1), 1); // Fill with 0, 1, ..., tau1
// t2 = 181:420
std::vector<int> t2(240) ; // t1ector with 100 ints.
std::iota (std::begin(t2), std::end(t2), 181); // Fill with 0, 1, ..., tau1
// t3 = 421:600
std::vector<int> t3(180) ; // t1ector with 100 ints.
std::iota (std::begin(t3), std::end(t3), 421); // Fill with 0, 1, ..., tau1

// Print vector for debugiing
// for(int i=0;i<180;i++){
// std::cout << t3[i] << " \n";
// }
// t = 1:600

// linear equation models
// double mu1 = alpha1 + beta*t1;
// for(int i=0; i<mu1.size(); i++){

// }
// double mu2 = alpha2 + beta*t2;
// double mu3 = alpha3 + beta*t3;
// mu = c(mu1,mu2,mu3)



};