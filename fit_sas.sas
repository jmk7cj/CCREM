/* --------------------------------------------------------------------------- */
/* imoprt and view data
/* --------------------------------------------------------------------------- */
proc import datafile="/home/u64192155/mbr/cleaned_data.csv"
dbms = csv
out = data_long
replace;
proc print data=data_long(obs=10);
/* --------------------------------------------------------------------------- */



/* --------------------------------------------------------------------------- */
/* Model 0: Naive Approach Ignoing Changes in Group Membership (Pure 3-level Clustering)
/* --------------------------------------------------------------------------- */
/* fit model */
proc mixed data=data_long method=reml;
class school student;
model math = time female ses public suspension platform / solution;
random intercept / subject = school; *g gcorr;
random intercept time / subject = student(school) type=un;
run;
/* --------------------------------------------------------------------------- */



/* --------------------------------------------------------------------------- */
/* Model 1: Acute Effects CCREM
/* --------------------------------------------------------------------------- */
/* fit model */
proc mixed data=data_long method=reml;
class school student;
model math = time female ses public suspension platform / solution;
random intercept / subject=school; *g gcorr;
random intercept time / subject=student type=un gcorr;
run;
/* --------------------------------------------------------------------------- */



/* --------------------------------------------------------------------------- */
/* Model 2: Cumulative Effects CCREM
/* --------------------------------------------------------------------------- */
/* first, define function to construct Z matrix of cumulative effects */
/* (Cafri et al., 2015) */
%macro cummulativeformat(datain=, personID=, grpname=, outcome=, dataout=);

proc freq data=&datain;
tables &grpname/out=w;
run;

ods output summary=group;
proc means data=w n;
var &grpname;
run;

data group2;
set group;
call symput("groupN", trim(left(put(&grpname._N,best.)))); 
run;
%put  groupN ===========> |&groupN| ;

proc glimmix data=&datain outdesign(z)=zmat;
   class &grpname;
   model  &outcome = ;
   random &grpname ;
run;

proc sort data=zmat;
by &personID;
run;

data  acute ;
set zmat ;
array glimout _z1-_z&groupN;
array recode x1-x&groupN;
by &personID;
    do i=1 to &groupN;
    	if  glimout{i} > 0 then do; recode{i}=glimout{i}; recode{i}=i; end;
		if  glimout{i} = 0 then do; recode{i}=glimout{i}; end;
    end;
drop i _z1-_z&groupN;
run;

data  cumulative1 ;
set acute ;
array origvar x1-x&groupN;
array carry d1-d&groupN;
by &personID;
retain carry;
	do i=1 to &groupN;
      if first.&personID then carry{i} = . ;
    end;
    do i=1 to &groupN;
    	if  origvar{i} > 0 then do;carry{i}=origvar{i};end;
    end;
run;

data cumulative2;
set cumulative1;
array origvar x1-x&groupN;
array carry d1-d&groupN;
    do i=1 to &groupN;
      if origvar{i} = .  then carry{i} = . ;
      if origvar{i} = 0  and carry{i} = .  then  carry{i}=0 ;
    end;
run;


data &dataout ;
set cumulative2;
array carry d1-d&groupN;
array weight w1-w&groupN;
    do i=1 to &groupN;
      if carry{i}>0  then weight{i}=1;
      else weight{i}=carry{i};
	end;
	drop i;
run; 
%mend;

/* apply to data, observe new data */
%cummulativeformat(
datain = data_long,
personID = student,
grpname = school,
outcome = math,
dataout = data_long2
);
proc print data=data_long2(obs=10);


/* fit model */
proc glimmix data=data_long2;
class d1-d83 student;
effect school2 = multimember(d1-d83 / weight=(w1-w83) details);
model math = time female ses public suspension platform / dist=gaussian link=identity solution;
random school2; *g gcorr;
random intercept time / subject=student type=un;
run;
/* --------------------------------------------------------------------------- */



/* --------------------------------------------------------------------------- */
/* Model 3: Cumulative Effects CCREM with Standardized Weights
/* --------------------------------------------------------------------------- */
/* fit model */
proc glimmix data=data_long2;
class d1-d83 student;
effect school2 = multimember(d1-d83 / weight=(w1-w83) stdsize details);
model math = time female ses public suspension platform / dist=gaussian link=identity solution;
random school2; *g gcorr;
random intercept time / subject=student type=un;
run;
/* --------------------------------------------------------------------------- */



/* --------------------------------------------------------------------------- */
/* Model 4: Acute Effects CCREM with Autoregressive Group Effects
/* --------------------------------------------------------------------------- */
/* must first make duplicate time variable named time2 */
data data_long;
set data_long;
time2 = time;
run;
proc print data=data_long(obs=10);

/* fit model */
proc mixed data=data_long method=reml;
class school student time2;
model math = time female ses public suspension platform / solution;
random time2 / subject=school type=ar(1); *g gcorr;
random intercept time / subject=student type=un;
run;
/* --------------------------------------------------------------------------- */



/* --------------------------------------------------------------------------- */
/* Model 5: Acute Effects CCREM with Compound Symmetric Group Effects
/* --------------------------------------------------------------------------- */
/* fit model */
proc mixed data=data_long method=reml;
class school student time2;
model math = time female ses public suspension platform / solution;
random time2 / subject=school type=cs; *g gcorr;
random intercept time / subject=student type=un;
run;
/* --------------------------------------------------------------------------- */



/* --------------------------------------------------------------------------- */
/* Model 6: Acute Effects CCREM with Unstructured Group Effects
/* --------------------------------------------------------------------------- */
/* fit model */
proc mixed data=data_long method=reml;
class school student time2;
model math = time female ses public suspension platform / solution;
random time2 / subject=school type=un; *g gcorr;
random intercept time / subject=student type=un;
run;
/* --------------------------------------------------------------------------- */