/*********************************************
 * OPL 12.6.0.0 Model
 * Author: Roberto
 * Creation Date: Oct 22, 2014 at 11:03:54 PM
 *********************************************/
{string} ithStop = ...;
{string} jthStop = ...;

float Distance[ithStop][jthStop] = ...; //distance required to traverse from ith stop to jth stop
int Students[ithStop] = ...; //number of students at ith stop; no differentiation between ith or jth

float Proportion = ...; //proportionality constant
int CarryLimit = ...; //maximum number of students per bus
int DistanceLimit = ...; //maximum distance bus can travel
float OperationalCost = ...; //cost of operating one bus
float CostPerMile = ...; //cost per unit distance
float RiskCost = ...; //cost of transporting one student per unit distance

assert //round trip distance between school and stops must be at most the distance limit
	forall(i in ithStop, j in jthStop: i == "school") //choice of i to be school arbitrary since matrix is symmetric
	  2*Distance[i][j] <= DistanceLimit;

assert //number of students in each stop must be at most the carry limit
	forall(i in ithStop)
	  Students[i] <= CarryLimit;

dvar int RecentStudents[ithStop] in 0..CarryLimit; //number of students picked up just after leaving ith stop
dvar float RecentDistance[ithStop] in 0..DistanceLimit; //distance traveled from depot up until ith stop
dvar boolean Traverse[ithStop][jthStop]; //whether or not traveled from ith stop to jth stop
dvar int+ Bus; //number of buses used


minimize
  sum(i in ithStop, j in jthStop : i != j)
    (
    Proportion * (CostPerMile * Distance[i][j] * Traverse[i][j]) + //cost of driving a bus 
    (1 - Proportion) * (RiskCost * RecentStudents[i] * Distance[i][j] * Traverse[i][j]) //cost per student 
    ) + 
    Proportion * OperationalCost * Bus; //cost of each bus

subject to{


	  c2: ///////////////////////////////////////// 2nd constraint
	  	sum(i in ithStop: i != "school")
	  		Traverse["school"][i] == Bus; //# of buses leaving school must equal # of buses used
	  		
	  		
	  c3: ///////////////////////////////////////// 3rd constraint
	  	sum(i in ithStop: i != "school")
	  		Traverse[i]["school"] == Bus; //# of buses coming back to school must equal # of buses used
	  		
	  		
	forall (i in ithStop: i != "school")
	  c4: ///////////////////////////////////////// 4th constraint
	  	sum(j in jthStop: j != i) //can't have same node traveling to same node
	    	Traverse[i][j] == 1; //important not to let this contraint be relaxed; otherwise nonsensical answer
	    	
	    	
	forall (j in jthStop: j != "school")
	  c5: ///////////////////////////////////////// 5th constraint
	   	sum(i in ithStop: i != j) //can't have same node traveling to same node
	   		Traverse[i][j] == 1; //important not to let this contraint be relaxed; otherwise nonsensical answer
	   		
	   		
	forall (i in ithStop, j in jthStop: i != j && i != "school" && j != "school")
	  c9: ///////////////////////////////////////// 9th constraint
	  		RecentStudents[i] - RecentStudents[j] + CarryLimit * Traverse[i][j] +  (CarryLimit - Students[i]
	  		- Students[j]) * Traverse[j][i] <= CarryLimit - Students[j];
	  		
	  		
	forall (i in ithStop: i != "school")
	  c10: ///////////////////////////////////////// 10th constraint
	  		RecentStudents[i] >= Students[i];
	  		
	  		
	forall (i in ithStop: i != "school")
	  c11: ///////////////////////////////////////// 11th constraint
	  		RecentStudents[i] - Students[i] * Traverse["school"][i] + CarryLimit * Traverse["school"][i] <= CarryLimit;
	  		
	  		
	forall (i in ithStop, j in jthStop: i != j && i != "school" && j != "school")
	  c12: ///////////////////////////////////////// 12th contraint
	  		RecentDistance[i] - RecentDistance[j] + (DistanceLimit - Distance[i]["school"] - Distance["school"][j] + 
	  		Distance[i][j]) * Traverse[i][j] + (DistanceLimit - Distance[i]["school"] - Distance["school"][j] - 
	  		Distance[j][i]) * Traverse[j][i] <= DistanceLimit - Distance[i]["school"] - Distance["school"][j];
	  		
	  		
	forall (i in ithStop: i != "school")
	  c13: ///////////////////////////////////////// 13th constraint
	  		RecentDistance[i] - Distance["school"][i] * Traverse["school"][i] >= 0;
	  		
	  		
	forall (i in ithStop: i != "school")
	  c14: ///////////////////////////////////////// 14th constraint
	  		RecentDistance[i] - Distance["school"][i] * Traverse["school"][i] + DistanceLimit * Traverse["school"][i] <= 
	  		DistanceLimit;	
}


main{ //iterating over Proportionality constant/lambda to see when solution changes
	var ofile = new IloOplOutputFile("C:/Users/Roberto/Desktop/College Papers/Thesis/R files/cplex_output.R")
	var inc = .01;
	var sbrp = thisOplModel;
	sbrp.generate()
	var is = sbrp.ithStop;
	var js = sbrp.jthStop;
	var count = 0;
	
	for (var i = 0; i <= 1+inc; i += inc){ 
		cplex.solve()
		var a = sbrp.Traverse;
		var av = cplex.getObjValue();
		var triv = cplex.status;
					
		ofile.writeln('l' + count + ' <- ' + sbrp.Proportion);
		ofile.writeln('v' + count + ' <- ' + av);
		ofile.writeln('b' + count + ' <- ' + sbrp.Bus);
		ofile.writeln('t' + count + ' <- ' + triv);
		ofile.write("s" + count + ' <- t(data.frame(');
		count += 1;
		
		var icount = 0;
		for (var I in is){ 
			icount += 1;
			ofile.write('c(');
			
			var jcount = 0;
			for (var J in js){ 
				jcount += 1;
				if (jcount == a.size){ 
					ofile.write(a[I][J] + ')');
  				}				
				else{ 
					ofile.write(a[I][J] + ', ');
 		 		}				
 			} 
 			if (icount == a.size){
 			 	ofile.write('))');
 			}
 			else{ 
 				ofile.write(', ');
  			} 			
		}
		ofile.writeln();
		ofile.writeln();
		
		var data = sbrp.dataElements;
		data.Proportion = i+inc;
			
		var src = new IloOplModelSource("bektas_model.mod");
		var def = new IloOplModelDefinition(src);
		var sbrp = new IloOplModel(def,cplex);
		sbrp.addDataSource(data);
		sbrp.generate();						
	}
	ofile.close();
}
