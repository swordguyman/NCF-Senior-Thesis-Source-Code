#---------------BEGIN CODE TIMING 1
ptm1 <- proc.time()

#---------------LOAD UP FUNCTION SUITE AND DEFINE DEFAULT SOURCE
#---------------CHANGE ACCORDINGLY BASED ON YOUR MACHINE!
source('C:/Users/Roberto/Desktop/College Papers/Thesis/R files/function_suite.R')
source('C:/Users/Roberto/Desktop/College Papers/Thesis/R files/defaults.R')

#---------------MAP OF LIST OF PARAMETERS AVAILABLE TO CHANGE
#0 - default run
#1 - distance matrix; solutions won't be compatible with default all_stops!
#2 - students at stops; solutions won't be compatible with default all_stops!
#3 - total number of nodes, including the school

#---------------TEST FOR A PARAMETER CHANGE N TIMES
original <- TEST()
#PLOT_TEST(original)

#parameter 1
geometry_stops <- TEST(c(1,2,3,4,5,6,7,8,9,10), 1) #Rplots
#PLOT_TEST(geometry_stops) #Lambdas

#parameter 2
geometry_students <- TEST(c(1,2,3,4,5,6,7,8,9,10), 2)
#PLOT_TEST(geometry_students)

#parameter 3
d <- TEST(c(5,6,7,8,9,10), 3)
#PLOT_TEST(d)

#---------------END CODE TIMING 1
end1 <- proc.time() - ptm1

#----------------------------------------------------------------------------

#---------------BEGIN CODE TIMING 2
ptm2 <- proc.time()

#case-study: parameter 1
cs2 <- TEST(c(9), 1, T, 'case_study_99_100', .99)

#case-study: parameter 3
cs3 <- TEST(c(10), 3, T, 'case_study_99_100', .99)

#---------------END CODE TIMING 2
end2 <- proc.time() - ptm2
