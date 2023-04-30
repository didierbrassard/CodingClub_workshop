 /*************************************************************************/
 /*                                                                       */
 /*            Marginal effects: getting insights from a model            */
 /*                                                                       */
 /*                         Supplemental SAS code                         */
 /*                                                                       */
 /*                        Author: Didier Brassard                        */
 /*                                                                       */
 /*                               Version 1                               */
 /*                               30APR2023                               */
 /*                                                                       */
 /*************************************************************************/

/* Define path to work with relative file location */
%let path = /home/didier.brassard.10/coding_club;

	/* note: same as R <setwd(...)> */

/* Load data */
PROC IMPORT DATAFILE="&path./data/processed/cchs2015_demonstration.csv"
	DBMS=CSV
	OUT=WORK.final replace;
	GETNAMES=YES;
RUN;

	/* note: ignore error and warning which are due to <NA> value (missing) */

 /*************************************************************************/
 /*                                                                       */
 /*   Example 1: descriptive analysis of energy | age, sex, phys. act.    */
 /*                                                                       */
 /*************************************************************************/

/* Linear regression model with genmod */
	proc genmod data=final ;
	class sex;
	model energy = age sex phys_act_mod  / dist=normal link=identity ;
	ods output ParameterEstimates=Param; * output parameters in a data ;
	output out=final_n_residuals resraw=residuals; * output input data with residuals ;
	run;
	
/* Show parameters estimates */
	proc print data=param;
	title1 "E(Energy | age, sex, physical activity)";
	format Estimate	LowerWaldCL	UpperWaldCL 4.1;
	id Parameter Level1;
	var Estimate LowerWaldCL UpperWaldCL ProbChiSq;
	where parameter not in ("Scale" "Intercept");
	run;
	title1;
	
/* Verify residuals normality assumption */
	ods select BasicMeasures Plots histogram;
	proc univariate data=final_n_residuals normal plot;
	var residuals ;
	hist residuals;
	run;

 /*************************************************************************/
 /*                                                                       */
 /*              Revised model with Restricted Cubic Spline               */
 /*                                                                       */
 /*************************************************************************/

	/* note: proc glimmix used to take advantage of the <effect> option,
		but other procedure could be used with manually-derived spline. */

/* Revised model and marginal effect for <sex> */
	proc glimmix  data=final;
	class sex;
	effect spl_age = spline( age / details naturalcubic basis=tpf(noint) 
		knotmethod=percentilelist(5 35 65 95));
	effect spl_phys_act_mod = spline( phys_act_mod / details naturalcubic basis=tpf(noint) 
		knotmethod=percentilelist(10 50 90  ));
	model energy = spl_age sex spl_phys_act_mod  / solution dist=normal link=identity ;
	lsmeans sex; * least-square means for sex;
	run;

	/* LSMEANS: The LSMEANS statement computes and compares least squares means
		(LS-means) of fixed effects. LS-means are predicted population margins—that is,
		they estimate the marginal means over a balanced population. */
	

/* Revised model and marginal effect for <sex> + pairwise difference */
ods select LSMeans Diffs;
	proc glimmix  data=final;
	class sex;
	effect spl_age = spline( age / details naturalcubic basis=tpf(noint) 
		knotmethod=percentilelist(5 35 65 95));
	effect spl_phys_act_mod = spline( phys_act_mod / details naturalcubic basis=tpf(noint) 
		knotmethod=percentilelist(10 50 90  ));
	model energy = spl_age sex spl_phys_act_mod  / solution dist=normal link=identity ;
	lsmeans sex / diff=all cl ; * least-square means and difference for sex;
	run;

 /*************************************************************************/
 /*                                                                       */
 /*                          Model visualization                          */
 /*                                                                       */
 /*************************************************************************/

/* Generate model object with <store> */
ods select none;
	proc glimmix  data=final;
	class sex;
	effect spl_age = spline( age / details naturalcubic basis=tpf(noint) 
		knotmethod=percentilelist(5 35 65 95));
	effect spl_phys_act_mod = spline( phys_act_mod / details naturalcubic basis=tpf(noint) 
		knotmethod=percentilelist(10 50 90  ));
	model energy = spl_age sex spl_phys_act_mod  / solution dist=normal link=identity ;
	lsmeans sex / diff=all cl ; * least-square means for sex;
	store lm2; * option to store model object;
	run;
ods select all;
	
 /*************************************************************************/
 /*             Model visualization at representative values              */
 /*************************************************************************/

	proc plm restore=lm2  ;
		effectplot fit(x= age ) / clm at(sex="Female");
		ods output FitPlot=lm2_plot; * output in data for further customization;
	run;
	
/* Customize plot with <proc sgplot> and data generated with <proc plm> */
	proc sgplot data=lm2_plot noautolegend;
	title1 justify=left "E(energy | age, z)";
	title2 justify=left "Curve at representative values";
	band x=_XCONT1 lower=_LCLM upper=_UCLM / transparency=0.9 fillattrs=(color=black);
	series x=_XCONT1 y=_PREDICTED / lineattrs=(color=black thickness=1.5); 
	yaxis label="Energy, kcal" type = linear GRID gridattrs=(pattern=longdash)
		values=(1200 1400 1600 1800 2000 2200);
	xaxis label="Age, years" type = linear GRID gridattrs=(pattern=longdash);
	run;
	
 /*************************************************************************/
 /*                  Model visualization at mean values                   */
 /*************************************************************************/

/* create dummy-coded sex variable */
	data final;
	set final;
	if not missing(sex) then do;
		if sex="Female" then female=1;
			else female=0;
	end;
	run;

/* Plot of E(energy| age, Z) */
ods select none;
	proc glimmix  data=final;
	* note: <class sex> is omitted due to dummy coding ;
	effect spl_age = spline( age / details naturalcubic basis=tpf(noint) 
		knotmethod=percentilelist(5 35 65 95));
	effect spl_phys_act_mod = spline( phys_act_mod / details naturalcubic basis=tpf(noint) 
		knotmethod=percentilelist(10 50 90  ));
	model energy = spl_age female spl_phys_act_mod  / solution dist=normal link=identity ;
	store lm2b; * option to store model object;
	run;
ods select all;

/* Generate graph datapoint using <proc plm> */
	proc plm restore=lm2b  ;
		effectplot fit(x= age ) / clm ;
	run;
	
	/* note: notice that fit is computed at female=0.502. */
	
 /*************************************************************************/
 /*             Model visualization for more than 1 covariate             */
 /*************************************************************************/

	proc plm restore=lm2  ; * lm2 created above;
	effectplot slicefit(x= age sliceby=sex  ) / clm ;
	ods output SliceFitPlot=plot_by_sex ; * output data for further customization (not shown);
	run;

 /*************************************************************************/
 /*                                                                       */
 /*                        Test custom hypotheses                         */
 /*                                                                       */
 /*************************************************************************/

/* Custom hypothesis at user-defined values */
	ods select estimates;
	proc glimmix  data=final;
	class sex;
	effect spl_age = spline( age / details naturalcubic basis=tpf(noint) 
		knotmethod=percentilelist(5 35 65 95));
	effect spl_phys_act_mod = spline( phys_act_mod / details naturalcubic basis=tpf(noint) 
		knotmethod=percentilelist(10 50 90  ));
	model energy = spl_age sex spl_phys_act_mod  / solution dist=normal link=identity ;
	estimate "[E|age=60, Z] - [E|age=30, Z]" spl_age [-1,30][1,60] / cl;
	ods output Estimates = custom_estimate; * export estimate data for further processing;
	run;
	
	/* show data */
	proc print data=custom_estimate;
	run;

 /*************************************************************************/
 /*              Custom hypothesis for predetermined change               */
 /*************************************************************************/

/* Calculate interquartile range (IQR) */
	proc means data=final q1 q3 qrange ;
	var age ;
	output out=age_iqr q1=value_q1 q3=value_q3;
	run;
	
	/* export IQR value as macro variable */
	data _null_;
	set age_iqr;
	call symputx("age_q1",value_q1) ;
	call symputx("age_q3",value_q3) ;
	run;
	
	/* show values */
	%put The &=age_q1;
	%put The &=age_q3;
	
	ods select estimates;
	proc glimmix  data=final;
	class sex;
	effect spl_age = spline( age / details naturalcubic basis=tpf(noint) 
		knotmethod=percentilelist(5 35 65 95));
	effect spl_phys_act_mod = spline( phys_act_mod / details naturalcubic basis=tpf(noint) 
		knotmethod=percentilelist(10 50 90  ));
	model energy = spl_age sex spl_phys_act_mod  / solution dist=normal link=identity ;
	estimate "[E|age=Q3, Z] - [E|age=Q1, Z]" spl_age [1,&age_q3][-1,&age_q1] / cl;
	ods output Estimates = iqr_estimates; * export estimate data for further processing;
	run;
	
	/* show data */
	proc print data=iqr_estimates;
	run;

 /*************************************************************************/
 /*                                                                       */
 /*                     Example 2: more complex model                     */
 /*                                                                       */
 /*************************************************************************/

/* Perform some recoding to reduce the number of parameters in model */
	data final_recoded;
	set final;
	* Dummy variable for education=University level;
	if not missing(education) then do;
		if education=4 then edu_4=1;
			else edu_4=0;
	end;
	* Dummy variable for daily or occasional smokers;
	if not missing(smoking) then do;
		if smoking in (1 2) then smk_1=1;
			else smk_1=0;
	end;
	run;

	/* confirm recoding */
		proc freq data=final_recoded;
		title1 "Recoding check";
		table sex*female education *edu_4 smoking *smk_1;
		run;
		title1;
		
/* Logistic regression model with <proc logistic> */ 

	/* note: proc logistic supports auto restricted cubic spline transformation,
		so proc glimmix is not needed */
	
	proc logistic data=final_recoded ;
	effect spl_sodium = spline( sodium / details naturalcubic basis=tpf(noint) 
		knotmethod=percentilelist(10 50 90));
	effect spl_age = spline( age / details naturalcubic basis=tpf(noint) 
		knotmethod=percentilelist(10 50 90));
	effect spl_phys_act_mod = spline( phys_act_mod / details naturalcubic basis=tpf(noint) 
		knotmethod=percentilelist(10 50 90));
	model self_reported_bp(event='1') = spl_sodium spl_age female edu_4 
		smk_1 spl_phys_act_mod ;
	store logm1; * option to store model object for further analyses;
	run;

 /*************************************************************************/
 /*                          Model visualization                          */
 /*************************************************************************/
	
	proc plm restore=logm1  noinfo noclprint ;
		effectplot fit(x= sodium ) / clm ;
		* generate underlying graph data for further processing;
		ods output FitPlot= plot(rename=(_LCLM=lcl _UCLM=ucl _XCONT1=x _PREDICTED=outcome));
	run;

/* Customize plot with <proc sgplot> and data generated with <proc plm> */
	proc sgplot data=plot noautolegend;
	title1 justify=left "Pr(High blood pressure | sodium, Z)";
	title2 justify=left "Curve at mean values" ;
	band x=x lower=lcl upper=ucl / transparency=0.9 fillattrs=(color=black);
	series x=x y=outcome / lineattrs=(color=black thickness=1.5); 
	yaxis label =  "Probability of high blood pressure"
		type = linear GRID gridattrs=(pattern=longdash);
	xaxis label = "Sodium intake, mg" type = linear GRID gridattrs=(pattern=longdash) 
		min=500 max=8500;
	run;
	
	/* note: the assumptions required to interpret this graph as causal effect
		are clearly implausible for the demonstration data. The graph is shown
		only to demonstrate coding techniques. */
		
/* end of code */

