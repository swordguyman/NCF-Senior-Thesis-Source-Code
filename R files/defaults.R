#---------------DEFINE PATHS TO FILES, ETC.
#---------------CHANGE ACCORDINGLY BASED ON YOUR MACHINE!

#location of dm.dat, the data file used in CPLEX
export_dm_dest <- 'C:/Users/Roberto/Desktop/College Papers/Thesis/CPLEX/Thesis_test/dm.dat'

#location of CPLEX software
source1 <- "C:/Program Files/IBM/ILOG/CPLEX_Enterprise_Server126/CPLEX_Studio/opl/bin/x64_win64"

#location of CPLEX project folder
source2 <- "C:/Users/Roberto/Desktop/College Papers/Thesis/CPLEX/Thesis_test"

#location of visualizations destination path
plot_dest <- 'C:/Users/Roberto/Desktop/College Papers/Thesis/LaTeX files'

#location of cplex_output.R file
sols <- 'C:/Users/Roberto/Desktop/College Papers/Thesis/R files/cplex_output.R'

runconfig <- "test" #name of run configuration to use

#---------------DEFINE CONSTANTS FOR ALL STOPS PLOT
xlim <- 53937/(2772+181) #limit of x axis from 0 to ...
ylim <- 53937/(2772+181) #same as above, but for y
total <- 6 #total number of stops
slow <- 1 #lower bound for number of students
sup <- 40 #upper bound for number of students

#---------------DEFINE CONSTANTS FOR SBRP INSTANCE
proportion <- 0 #never change! Must start at 0!
carrylim <- 77 #should always be >= to sup
dlim <- 45 #should always be >= 2*distance(school, stop i)
opcost <- 971507232/18373
milecost <- 3.55
riskcost <- (1222/5256000)*(1/2)*1130000
seed <- 20

#---------------CONSTRUCT PARK STOPS
park <- 0

#---------------CONSTRUCT SCHOOL STOPS
school <- data.frame(x=c(xlim/2), y=c(ylim/2), number_of_students = 0, Type='School')

#---------------DEFAULT ALL STOPS DATA.FRAME
all_stops <- create_all_stops(xlim, ylim, slow, sup, total, park, school, seed)	
