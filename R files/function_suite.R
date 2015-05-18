library(ggplot2)
library(grid) #for arrow function
library(animation)
library(scales) #for legend transparency



#-------------------------------------------------------------------------#
#Takes number of stops and limit of x/y distance and creates stops randomly
#-------------------------------------------------------------------------#
#also generates random amount of students given lower/upper limit
#-------------------------------------------------------------------------#
stop_randomizer <- function(Number, Xlim, Ylim, Slow, Sup, Seed){
	Bus_stops <- data.frame(x=rep(NA, Number), y=NA, number_of_students = 0,
			 Type='Bus Stop')
	
	set.seed(Seed)
	for (i in 1:Number){
		Bus_stops[i,1] <- runif(1, 0, Xlim)
		Bus_stops[i,2] <- runif(1, 0, Ylim)
		Bus_stops[i,3] <- sample(Slow:Sup, 1)
		rownames(Bus_stops)[i] <- c(paste('stop', i, sep=' '))
	}
	return(Bus_stops)
}



#-------------------------------------------------------------------------#
#Randomizes stops given a data.frame with all stops
#-------------------------------------------------------------------------#
stops_scramble <- function(Xlim, Ylim, Seed, Frame){
	Len <- length(Frame[,1]) #total number of bus stops + school

	set.seed(Seed)
	for(i in 2:Len){ #no need to randomize the school's parameters!
		Frame[i,1] <- runif(1, 0, Xlim)
		Frame[i,2] <- runif(1, 0, Ylim)
	}
	return(Frame)
}



#-------------------------------------------------------------------------#
#Randomizes students at each stop given a data.frame with all stops
#-------------------------------------------------------------------------#
studs_scramble <- function(Slow, Sup, Seed, Frame){
	Len <- length(Frame[,1]) #total number of bus stops + school

	set.seed(Seed)
	for(i in 2:Len){ #school always have 0 students!
		Frame[i,3] <- sample(Slow:Sup, 1)
	}
	return(Frame)
}



#-------------------------------------------------------------------------#
#Returns straight line distance between stop 1 and stop 2
#-------------------------------------------------------------------------#
stop_distance <- function(Stop1, Stop2, Frame){
	X1 <- Frame[Stop1, 1]
	X2 <- Frame[Stop2, 1]
	Y1 <- Frame[Stop1, 2]
	Y2 <- Frame[Stop2, 2]
	Distance <- sqrt( (X2-X1)**2 + (Y2-Y1)**2 )
	return(Distance)
}



#-------------------------------------------------------------------------#
#Create all stops matrix given parks, schools, and bus stops
#-------------------------------------------------------------------------#
create_all_stops <- function(Xlim, Ylim, Slow, Sup, Total, Park, School, 
				     Seed){
	Plen <- 0 #initialize number of park stops
	Slen <- 0 #intialize number of school stops

	if (class(Park) == 'data.frame'){
		Plen <- length(Park[,1])
	}
	if (class(School) == 'data.frame'){
		Slen <- length(School[,1])
	}

	Bus <- (Total - Plen - Slen)
	Stops <- stop_randomizer(Bus, Xlim, Ylim, Slow, Sup, Seed)
	
	if (Park == 0 && School ==0){
		return(-1) #school and park cannot both be empty
	}	
	else if (Park == 0){
		Frame <- rbind(School, Stops)
	}
	else if (School == 0){
		Frame <- rbind(Park, Stops)
	}
	else{
		Frame <- rbind(School, Park, Stops)
	}
	return(Frame)
}



#-------------------------------------------------------------------------#
#Creates symmetric distance matrix given data.frame with all stops
#-------------------------------------------------------------------------#
#Note: this should only be used with school as the first node
#-------------------------------------------------------------------------#
#Order: O(n(n+1)/2)
#-------------------------------------------------------------------------#
distance_matrix <- function(Frame){
	Len <- length(Frame[,1])
	Dis_mat <- data.frame(matrix(0, nrow=Len, ncol=Len))

	for (i in 1:Len){
		for (j in i:Len){
			if (i != j){
				Dist <- stop_distance(i, j, Frame)
				Dis_mat[i, j] <- Dist
				Dis_mat[j, i] <- Dist
			}
			else{
				next
			}
		}
	}
	return(Dis_mat)
}



#-------------------------------------------------------------------------#
#Creates indices for cplex given total number of stops as string
#-------------------------------------------------------------------------#
cplex_indices <- function(Total){
	Indices <- ''

	if (Total <= 0){
		return(-1)
	}

	for (i in 1:Total){
		if (i == 1){
			Indices <- '"school"'
		}
		else{
			Stop <- paste(i-1, '"', sep='')
			Indices <- paste(Indices, '"stop', Stop, sep=' ')
		}
	}
	return(Indices)
}



#-------------------------------------------------------------------------#
#Creates number of students vector for cplex as string
#-------------------------------------------------------------------------#
cplex_students <- function(Frame){
	Students <- ''
	for (i in 1:length(Frame[,1])){
		if (i == 1){
			Students <- as.character(Frame[i,3])
		}
		else{
			Students <- paste(Students, Frame[i,3], sep=' ')
		}
	}
	return(Students)
}



#-------------------------------------------------------------------------#
#Creates cplex input for SBRP problem
#-------------------------------------------------------------------------#
cplex_input <- function(Sd, Frame, Location, Proportion, Carrylim, Dlim,
				Opcost, Milecost, Riskcost){
	FileConn <- file(Location, 'w') #create file at location
	Indices <- cplex_indices(length(Sd))	
	Students <- cplex_students(Frame)
	
	#index vectors
	write(paste('ithStop =', '{', Indices, '};'), FileConn)
	write(paste('jthStop =', '{', Indices, '};'), FileConn)
	
	#distance matrix
	write('', FileConn)
	write('Distance = ', FileConn)
	write('[', FileConn)
	for (i in 1:length(Sd)){
		Vector <- as.vector(Sd[i,])
		Line <- paste(Vector, collapse=' ')
		write(paste('[', Line, ']'), FileConn)
	}
	write('];', FileConn)
	
	#students vector
	write('', FileConn)
	write(paste('Students = [', Students, '];'), FileConn)
	
	#rest of constants
	write('', FileConn)
	write(paste('Proportion = ', Proportion, ';', sep=''), FileConn)
	write(paste('CarryLimit = ', Carrylim, ';', sep=''), FileConn)
	write(paste('DistanceLimit = ', Dlim, ';', sep=''), FileConn)
	write(paste('OperationalCost = ', Opcost, ';', sep=''), FileConn)
	write(paste('CostPerMile = ', Milecost, ';', sep=''), FileConn)
	write(paste('RiskCost = ', Riskcost, ';', sep=''), FileConn)

	close(FileConn) #close file connection
}



#-------------------------------------------------------------------------#
#Takes solution matrix and outputs list of plotting coordinates
#-------------------------------------------------------------------------#
convert_solution <- function(Sol){
	Output <- list()
	Index <- 0
	for (i in 1:length(Sol[1,])){
		for (j in 1:length(Sol[,1])){
			if (Sol[i,j] == 1){
				Index <- Index + 1
				Output[[Index]] <- c(i,j)
			}
		}
	}
	return(Output)
}



#-------------------------------------------------------------------------#
#Takes solution matrix and computes the routes
#-------------------------------------------------------------------------#
route_solution <- function(Sol){
	Indices <- convert_solution(Sol)
	Routes <- list()

	Index1 <- 0
	for (i in 1:length(Indices)){ #look for start of the routes
		if (Indices[[i]][1] == 1){
			Index1 <- Index1 + 1
			Routes[[Index1]] <- Indices[[i]]
		}
	}

	End <- length(Routes)
	Index2 <- 1
	while (length(Indices) > End){
		for (i in 1:length(Routes)){
			if (Indices[[Index2]][1] == Routes[[i]]
			[length(Routes[[i]])] && Indices[[Index2]][1] != 1){
				Routes[[i]] <- c(Routes[[i]], Indices[[Index2]])
				Indices[[Index2]] <- NULL
				Index2 <- 0
				break
			}
		}
		Index2 <- Index2 + 1
	}
	return(Routes)
}



#-------------------------------------------------------------------------#
#Decomposes route to a set of plotting indices
#-------------------------------------------------------------------------#
decompose_route <- function(Route){
	Indices <- list()

	for (i in 1:(length(Route)/2)){
		Indices[[i]] <- c(Route[1 + 2*(i-1)], Route[2 + 2*(i-1)])
	}
	return(Indices)
}



#-------------------------------------------------------------------------#
#Adds route to a given plot of stops
#-------------------------------------------------------------------------#
add_route <- function(Plot, Route, Frame, Color){
	Indices <- decompose_route(Route)

	for (i in 1:length(Indices)){
		Start <- Indices[[i]][1]
		End <- Indices[[i]][2]
		Plot <- Plot + geom_segment(data=Frame, 
		x=Frame[Start,1], y=Frame[Start,2], 
		xend=Frame[End,1], yend=Frame[End,2], arrow=arrow(angle=25, 
		length=unit(0.5, "cm")), color=Color, size=1)
	}
	return(Plot)
}



#-------------------------------------------------------------------------#
#Given a number, creates color palette suitable for plots
#-------------------------------------------------------------------------#
colors <- function(N){
	color <- list('purple', 'blue', 'red', 'orange', 'yellow', 
			  'plum2', 'deeppink', 'goldenrod4', 'cyan4', 'gray20',
			  'burlywood', 'cornflowerblue')
	return(color[1:N])
}



#-------------------------------------------------------------------------#
#Plots solution given a distance matrix
#-------------------------------------------------------------------------#
#Requires ggplot2 and grid for arrows
#-------------------------------------------------------------------------#
#Type 1 plot: all_stops and solution routes
#-------------------------------------------------------------------------#
plot_solution <- function(Sol, Lambda, Frame, Parameter, Change, Xlim=xlim, 
		     Ylim=ylim){
	Routes <- route_solution(Sol)
	Plot <- qplot(x, y, data=Frame, shape=Type, color=Type) 
	Color_list <- (colors(length(Routes)))

	for (i in 1:length(Routes)){
		Plot <- add_route(Plot, Routes[[i]], Frame, Color=Color_list[[i]])
	}

	LAMBDA <- paste('lambda == ', Lambda)
	PARAM <- paste('mu == ', Parameter)
	CHANGE <- paste('nu == ', Change)

	#alignment values for lambda, mu, and nu
	#imagine our bounding area is painting; we want a frame over it
	#half the distance of what we need outer frame to be enclosing our 
	#bounding area; use half because we need to align by center
	Align_x <- Xlim/25
	Align_y <- Ylim/25

	#arbitrary extension of upper area; to ensure legend doesnt interfere
	#with bus stops
	Extend_y <- Ylim*.22

	Plot <- Plot + 
		scale_color_discrete(breaks=c('School', 'Bus Stop')) + 
		geom_point(size=5) + 
		theme_bw(base_size = 12) +
		theme(axis.title.x = element_blank(),
		axis.title.y = element_blank(),
		axis.line = element_line(size=.7, color = "black"),
 		legend.position = c(.92, .92),
		legend.background = element_rect(fill=alpha('white', 0)),
		text = element_text(size=10)) +

		annotate("text", label = LAMBDA, x = -Align_x, 
		y = (Ylim+Extend_y) - Align_y, fontface = 3, parse=TRUE, 
		hjust=0) +

		annotate("text", label = PARAM, x = -Align_x, y = -Align_y, 
		fontface = 3, parse=TRUE, hjust=0) +

		annotate("text", label = CHANGE, x = (Xlim+(2*Align_x))-Align_x, 
		y = -Align_y, fontface = 3, parse=TRUE, hjust=1) +

		coord_cartesian(xlim = c(-2*Align_x, Xlim+(2*Align_x)), 
		ylim = c(-2*Align_y, Ylim+Extend_y))

	return(Plot)
}



#-------------------------------------------------------------------------#
#Plots all solutions required for .pdf and LaTeX
#-------------------------------------------------------------------------#
#Assumes s0-s100 are in the workspace.(if Type = 1)
#-------------------------------------------------------------------------#
#Name of plots: Rplot%_*; %:parameter, *:value of changed parameter
#-------------------------------------------------------------------------#
final_plot <- function(Dest, Frame, Parameter, Change, Type, Case_study){
	setwd(Dest)

	if(Type==1){ #if plotting solutions
		Oopt <- ani.options(interval = .05, nmax = 101)

		IMG_NAME <- paste('Rplot', Parameter, '_', Change, sep='')
		
		if(Case_study==T){
			IMG_NAME <- paste('Case_Study_', IMG_NAME, sep='')
		}
		
		saveLatex(
		for (i in 1:ani.options("nmax")) {
			S <- paste('s', i-1, sep='')
			L <- paste('l', i-1, sep='')
			print(plot_solution(get(S), get(L), Frame, Parameter, Change))
			ani.pause()	
		},
		ani.dev = 'pdf', 
		ani.type = 'pdf',
		ani.height = 6, 
		ani.width = 6, 
		ani.opts='controls,width=.8\\linewidth',
		overwrite=FALSE,
		img.name = IMG_NAME,
		pdflatex=NULL,
		documentclass=NULL)

		ani.options(Oopt)
	}
	else if(Type==2){ #if plotting lambda vs. total cost/bus
		Oopt <- ani.options(interval = .05, nmax = 2)

		IMG_NAME <- paste('Lambda', Parameter, '_', Change, sep='')

		saveLatex(
		for (i in 1:ani.options("nmax")) {
			print(plot_lambdas_versus(Frame, Parameter, Change, 
			Column=i+1))
			ani.pause()
		},
		ani.dev = 'pdf', 
		ani.type = 'pdf',
		ani.height = 6, 
		ani.width = 6, 
		ani.opts='controls,width=.8\\linewidth',
		overwrite=FALSE,
		img.name = IMG_NAME,
		pdflatex=NULL,
		documentclass=NULL)

		ani.options(Oopt)
	}
}

#-------------------------------------------------------------------------#
#Determines whether or not two solution matrices are the same or not
#-------------------------------------------------------------------------#
same_solution <- function(S1, S2){
	Num_of_elements <- length(S1)
	
	#comparing matrices elementwise
	Comparisons <- sum(S1 == S2) 
	
	#if comparisons are equal to the number of elements, return true
	if(Num_of_elements == Comparisons){return(T)}

	else{return(F)}
}



#-------------------------------------------------------------------------#
#Finds lambdas, number of buses, and cost at each solution change
#-------------------------------------------------------------------------#
#Assumes s0-s100, b0-b100, and v0-v100 are in workspace
#-------------------------------------------------------------------------#
find_lambdas_etc <- function(){	
	Lambdas_etc <- data.frame(lambda=rep(NA,101), total_cost=rep(NA,101),
			   number_of_buses=rep(NA,101), solution_change=rep(F, 101))

	#first row initialization; 0 not defined for solution change
	Lambdas_etc[1,1] <- 0
	Lambdas_etc[1,2] <- get(paste('v', 0, sep=''))
	Lambdas_etc[1,3] <- get(paste('b', 0, sep=''))

	for(i in 1:100){
		S1 <- paste('s', i-1, sep='')
		S2 <- paste('s', i, sep='')
	
		Lambdas_etc[i+1,1] <- i*.01
		Lambdas_etc[i+1,2] <- get(paste('v', i, sep=''))
		Lambdas_etc[i+1,3] <- get(paste('b', i, sep=''))

		if(same_solution(get(S1), get(S2)) == F){
			Lambdas_etc[i+1, 4] <- T
		}
	}
	
	return(Lambdas_etc)
}



#-------------------------------------------------------------------------#
#Call CPLEX using command prompt from R
#-------------------------------------------------------------------------#
CPLEX <- function(Source1, Source2, Runconfig){
	#change directory to oplrun source
	Cmd <- paste("cd ", Source1, sep='')
	shell(Cmd)
	
	#do run configuration at project file source
	Cmd <- paste('oplrun -p "', Source2, '" "', Runconfig, '"', sep='')
	shell(Cmd)
}



#-------------------------------------------------------------------------#
#Solves a random SBRP instance 101 times, iterating through the lambdas
#-------------------------------------------------------------------------#
SBRP <- function(Frame, Export_dm_dest, Proportion, Carrylim, Dlim, 
		     Opcost, Milecost, Riskcost, Source1, Source2, Runconfig, 
		     Seed){
	#construct symmetric distance matrix from frame
	Dm <- distance_matrix(Frame)

	#export dm to CPLEX
	cplex_input(Dm, Frame, Export_dm_dest, Proportion, Carrylim, Dlim, 
			Opcost, Milecost, Riskcost)

	#solve model by calling CPLEX from command prompt
	CPLEX(Source1, Source2, Runconfig)

	#does not return anything; solution file generated after CPLEX
	#finishes solving
}



#-------------------------------------------------------------------------#
#Test for critical points when a parameter p changes, n times
#-------------------------------------------------------------------------#
#Dependent on default values defined in defaults.R
#-------------------------------------------------------------------------#
#Type 1 plot output to .pdf 
#-------------------------------------------------------------------------#
#Case study only good for 0-.01, .001 intervals. 
#-------------------------------------------------------------------------#
TEST <- function(Changes=0, P=0, Case=F, Name='', PL=0){
	Final <- 0 #default return or changed parameter return
	Collection <- data.frame()
	N <- length(Changes)
	All_stops <- all_stops
	R <- runconfig 
	Propor <- proportion

	#whether or not to do a case study
	if(Case==T){
		R <- Name #name of case study
		Propor <- PL #starting proportional value; .0001 intervals
	}

	for(i in 1:N){
		#choose parameter to change
		if(P==1){
			All_stops <- stops_scramble(xlim, ylim, Changes[i], 
			all_stops)
		}
		else if(P==2){
			All_stops <- studs_scramble(slow, sup, Changes[i], 
			all_stops)
		}
		else if(P==3){
			All_stops <- create_all_stops(xlim, ylim, slow, sup, 
			Changes[i], park, school, seed)
		}

		SBRP(All_stops, export_dm_dest, Propor, 
		carrylim, dlim, opcost, milecost, riskcost, source1, 
		source2, R, seed)

		source(sols)
		final_plot(plot_dest, All_stops, Parameter=P, 
		Change=Changes[i], Type=1, Case_study=Case)

		#stick parameter and change columns to lambdas, etc.
		if(Changes != 0 && P!=0){
			Dummy <- cbind(find_lambdas_etc(), parameter=rep(P, 101), 
			         change=rep(Changes[i], 101))

			Collection <- rbind(Collection, Dummy)
		}
	}

	if(Changes==0 && P==0){Final <- find_lambdas_etc()}
	else{Final <- Collection}

	return(Final)
}



#-------------------------------------------------------------------------#
#Plotting lambdas vs total cost/number of buses
#-------------------------------------------------------------------------#
#Requires the resulting data.frame when TEST() is run
#-------------------------------------------------------------------------#
#Assumes data.frame is already subsetted
#-------------------------------------------------------------------------#
#Type 2 plot: on TEST() returned data.frames
#-------------------------------------------------------------------------#
plot_lambdas_versus <- function(Frame, Parameter, Change, Column){
	Total_Lambdas <- total_lambdas(Frame)
	Longest_Interval <- longest_lambda_interval(Frame)
	
	Ylabel <- ''

	#Column 2 is total cost; column 3 is number of buses
	if(Column==2){
		Ylab <- 'Total Cost'
	}
	else if(Column==3){
		Ylab <- 'Number of buses'
	}

	TOTAL <- paste('Total number of solution changes: ', Total_Lambdas)
	LONGEST <- paste('Longest interval between two consecutive solution',
	' changes: ', Longest_Interval, sep='')
	PARAM <- paste('mu == ', Parameter)
	CHANGE <- paste('nu == ', Change)
	
	XLIM <- 1
	YLIM <- max(Frame[,Column])

	#alignment values for mu and nu; same as plot_solution()
	Align_x <- XLIM/25
	Align_y <- YLIM/25

	#arbitrary extension of upper area so that legend doesn't
	#interfere with plot
	Extend_y <- YLIM*.22

	Plot <- qplot(x=Frame[,1], y=Frame[,Column], data=Frame, 
	color=solution_change) +
	xlab(expression(lambda)) +
	ylab(Ylab) +
	theme_bw(base_size = 12) +
	theme(legend.position = c(.89, .92),
	legend.background = element_rect(fill=alpha('white', 0)),
	axis.line = element_line(size=.7, color = "black"),
	text = element_text(size=10)) +

	annotate("text", label = TOTAL, x = -Align_x, 
	y = (YLIM+Extend_y) - Align_y, fontface = 3, 
	hjust=0, size=3) +

	annotate("text", label = LONGEST, x = -Align_x, 
	y = (YLIM+Extend_y) - 2*Align_y, fontface = 3, 
	hjust=0, size=3) +

	annotate("text", label = PARAM, x = -Align_x, y = -Align_y, 
	fontface = 3, parse=TRUE, hjust=0) +

	annotate("text", label = CHANGE, x = (XLIM+(2*Align_x))-Align_x, 
	y = -Align_y, fontface = 3, parse=TRUE, hjust=1) +

	coord_cartesian(xlim = c(-2*Align_x, XLIM+(2*Align_x)), 
	ylim = c(-2*Align_y, YLIM+Extend_y)) +

	scale_color_discrete(name="Solution Change",
	breaks=c(T, F),
      labels=c("True", "False"))

	return(Plot)
}



#-------------------------------------------------------------------------#
#Use plot_lambdas_versus across changes w.r.t. a parameter
#-------------------------------------------------------------------------#
#Type 2 plot output to .pdf
#-------------------------------------------------------------------------#
PLOT_TEST <- function(Frame){
	if(ncol(Frame)==4){
		final_plot(plot_dest, Frame, 0, 0, 2, Case_study=F)
	}
	
	else if(ncol(Frame)==6){
		PARAM <- unique(Frame[,5])
		CHANGES <- unique(Frame[,6])

		for(i in 1:length(CHANGES)){
			Subset_by_change <- Frame[Frame$change==CHANGES[i],]

			final_plot(plot_dest, Subset_by_change, PARAM, 
			CHANGES[i], 2, Case_study=F)
		}
	}
}



#-------------------------------------------------------------------------#
#Determines total number of solution changes given a TEST() output
#-------------------------------------------------------------------------#
#Assumes data.frame is already subsetted
#-------------------------------------------------------------------------#
total_lambdas <- function(Frame){
	Sum <- sum(Frame[,4])
	return(Sum)
}



#-------------------------------------------------------------------------#
#Determines longest interval between any two consecutive solution change
#-------------------------------------------------------------------------#
#Assumes data.frame is already subsetted
#-------------------------------------------------------------------------#
longest_lambda_interval <- function(Frame){
	Count <- 0 #counts interval from start of change to finish
	Flag <- F #off until we find the first solution change
	Lengths <- vector() #interval lengths

	for(i in 1:length(Frame[,1])){
		if(Flag == T){
			Count <- Count + 1
		}
		
		#start counting after we find a solution change
		#final count value ignored if i=101 is not solution change
		if(Frame[i,4] == T){
			Flag <- T #when first solution change is found

			if(Count != 0){
				# -1 to compensate for counting one step too early
				Lengths <- append(Lengths, Count-1)
				Count <- 0 #reset
			}
		}
	}

	return(max(Lengths))
}



