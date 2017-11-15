


# **soh*************************************************************************************************************************
# Eli Lilly and Company - Global Statistical Sciences
# CODE NAME               : gmcp_wave9_adl_adjusted.R   ( using the original ES(ADL) )
# SOURCE CODE NAME        : az_bace_ gmcp_grid_search_wave7n_iadrs.R
# PROJECT NAME            : 
# DESCRIPTION             : Simulation study to assess multiplicity control using graphical approach
# SPECIFICATIONS          : 
# VALIDATION TYPE         : 
# INDEPENDENT REPLICATION : 
# ORIGINAL CODE           : N/A, this is the original code
# COMPONENT CODE MODULES  : lzax graphical testing scheme_functions.R
#                           lzax graphical testing scheme_schemes.R
#                           are imbeded in this program for convenience
# 
# SOFTWARE/VERSION#       : R 3.3.0 
# INFRASTRUCTURE          :  
# DATA INPUT              : N/A
# OUTPUT                  : 
# SPECIAL INSTRUCTIONS    : THIS PROGRAM WILL NOT EXECUTE IN SDD!  To run, you must download it to a local area.
#                           The following non-base R packages (version-specific) must be installed:
#                           > ggplot2 '1.0.0' 
#                           > MASS '7.3.33' 
#                           > colorspace '1.2.4'
#                           > digest '0.6.4'
#                           > grid '3.1.0'
#                           > gtable '0.1.2'
#                           > munsell '0.4.2'
#                           > plyr '1.8.1' 
#                           > proto '0.3.10' 
#                           > Rcpp '0.11.2'
#                           > reshape2 '1.4' 
#                           > scales '0.2.4'
#                           > stringr '0.6.2'
#                           > gMCP '0.8.10'
#                           > xlsxjars '0.6.1'
#                           > JavaGD '0.6.1'
#                           > rJava '0.9.8' 
#                           > CommonJavaJars '1.0.5'
#                           > lattice '0.20.29'
#                           > Matrix '1.1.4'   
#                           > multcomp '1.3.3' 
#                           > mvtnorm '0.9.99992' 
#                           > PolynomF '0.94'
#                           > sandwich '2.3.0'
#                           > splines '3.1.0'
#                           > stats4 '3.1.0' 
#                           > survival '2.37.7' 
#                           > TH.data '1.0.3'
#                           > zoo '1.7.11'
# -------------------------------------------------------------------------------------------------------------------------------	
# -------------------------------------------------------------------------------------------------------------------------------
# DOCUMENTATION AND REVISION HISTORY SECTION:
#
#        Author &
# Ver# Validator              Code History Description
# ---- ---------------------- ---------------------------------------------------------------------------------------------------
# 1.0  Zongjun Zhang &  
#
# 
# based on the discussion in meeting with Lei Shen, Scott Andersen 
#
# **eoh**************************************************************************************************************************

library(pwr)
# power.t.test,power.prop.test,power.anova.test
# Exactly one of the parameters n, delta, power, sd, and sig.level must be passed as NULL, and that parameter is 
# determined from the others. Notice that the last two have non-NULL defaults so NULL must be explicitly passed 
# if you want to compute them. 

# pwr.t.test(n = NULL, d = NULL, sig.level = 0.05, power = NULL, 
#     type = c("two.sample", "one.sample", "paired"),
#     alternative = c("two.sided", "less", "greater"))

pwr.t.test(n=788,d=0.2) 
pwr.t.test(n=788,d=0.2, sig.level=0.025, type = "two.sample", alternative = "greater") 

# the following is used to validate the marginal power given the sample size and effect size
n_subj <- rep(c(734, 734, 734, 734, 734, 734, 734, 734, 734, 734),2) 
effect_size_base      <- rep( c(0.2,0.2,0.15,0.142,0.2,0.1,0.1,0.1,0.1,0.1),2)
effect_size_cogfun    <- rep( c(0.175,0.175,0.175,0.124,0.175,0.1,0.1,0.12,0.1,0.1),2)
effect_size_low       <- rep( c(0.11, 0.11, 0.12, 0.077,0.11, 0.05, 0.05,0.05,0.05,0.05),2)  

pwr.t.test(n=n_subj,d=effect_size_base, sig.level=0.025, type = "two.sample", alternative = "greater") 
pwr.t.test(n=n_subj,d=effect_size_cogfun , sig.level=0.025, type = "two.sample", alternative = "greater") 
pwr.t.test(n=n_subj,d=effect_size_low, sig.level=0.025, type = "two.sample", alternative = "greater") 





# set the path for aving simulation results 
path <- "/home/c019143/mt_gk/az_bace/out/gmcp"
setwd(path)

library(MASS)


# **sof***************************************************************************
# --------------------------------------------------------------------------------
# Function NAME : gen_pvalues
# DESCRIPTION   : Generate p-value for each endpoint
# INPUT         : See "ARGUMENTS"
# OUTPUT        : Returns a data frame of dimension [n_pvals] x length([effect_size])
#                 containing generated p-values.  Optionally saves this data frame
#                 into an external .csv file.	
# REQUIREMENTS  : See header (above)
# ASSUMPTIONS   : Endpoints may be represented using multivariate normal distribution
# --------------------------------------------------------------------------------
# ARGUMENTS:
# Name        Type     Default    Description and Valid Values
# ----------- -------- ---------- ------------------------------------------------
# n_pvals     required            Number of p-values to generate
# effect_size required            Effect sizes of the endpoints under 
#                                 consideration (1:1 relationship with [n_subj] 
#                                 and [varnames])
# n_subj      required            Total sample size of each endpoint 
# corr        required            Correlation between endpoints
# varnames    required            Variable name for each endpoint
# out_p_vals  optional            Filename for generated p-value dataset.  
#                                 Automatically formatted as .csv (i.e., do not 
#                                 include extension).
# seed        optional as.integer Seed for reproducibility
#                      (Sys.time())
#
# TYPICAL WAYS TO EXECUTE THIS CODE AND DESCRIPTION, if applicable:
#
# A <- matrix(0, 4, 4)
# diag(A) <- 1
# A[1,2] <- A[2,1] <- 0.14
# A[1,3] <- A[3,1] <- 0.22
# A[1,4] <- A[4,1] <- 0.13
# A[3,4] <- A[4,3] <- 0.02
# B <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
# corr <- B %x% A
#
# gen_pvalues(n_pvals=1000,
#             effect_size=c(0.3, 0.44, 0.16, 0.3, 0.23, 0.33, 0.12, 0.23),
#             n_subj=rep(c(380, 380, 230, 380), 2),
#             corr=corr,
#             varnames=c("pvalue_PR1..Dose.1.", "pvalue_SR1..Dose.1.", 
#                        "pvalue_SR2..Dose.1.", "pvalue_SR3..Dose.1.", 
#                        "pvalue_PR1..Dose.2.", "pvalue_SR1..Dose.2.", 
#                        "pvalue_SR2..Dose.2.", "pvalue_SR3..Dose.2."),
#             out_p_vals="pvals_base",
#             seed=6789)
# --------------------------------------------------------------------------------
# **eof***************************************************************************

gen_pvalues <- function(n_pvals,
                        effect_size,
                        n_subj,
                        corr,
                        varnames,
                        out_p_vals,
                        seed=as.integer(Sys.time()))
{
	if(!is.null(seed)){
		if(!is.na(seed)){
				set.seed(seed)
				cat("seed:", seed, "\n")
		}
	}

	z_vals <- mvrnorm(n = n_pvals, mu=(effect_size/sqrt(2/n_subj)), Sigma=corr)
	#print(colMeans(z_vals))
	#cor(z_vals)
	#sum(z_vals < 0)

	p_vals <- pnorm(z_vals, lower.tail=F)*2
	#sum(p_vals > 1)

	p_vals[p_vals > 1] <- 1
	#sum(p_vals == 1)

	colnames(p_vals) <- varnames

	if(!missing(out_p_vals)){
		write.table(as.data.frame(p_vals), file=paste(out_p_vals, "csv", sep="."), sep=",", row.names=F)
	}

	invisible(as.data.frame(p_vals))
}


# **sof***************************************************************************
# --------------------------------------------------------------------------------
# Function NAME : scheme_apply
# DESCRIPTION   : Apply graphical scheme(s) to p-values
# INPUT         : See "ARGUMENTS"
# OUTPUT        : Returns a list with elements named "simulations" and "summary",
#                 which hold data frames containing sim-by-sim results and
#                 summarized results (i.e., summary over all sims), respectively.  
#                 Optionally saves the data frames into external .csv files.	
# REQUIREMENTS  : See header (above)
# ASSUMPTIONS   : N/A
# --------------------------------------------------------------------------------
# ARGUMENTS:
# Name        Type     Default    Description and Valid Values
# ----------- -------- ---------- ------------------------------------------------
# data        required            Data frame containing generated p-values
# scheme_key  required            Data frame that maps objects (in this case,
#                                 objects containing graphical schemes) to
#                                 scheme names, scheme labels, and indices.  
#                                 p-values in [data] are applied to all objects
#                                 contained in [scheme_key].
# out_        optional            Filename for sim-by-sim results.  Automatically
# simulations                     formatted as .csv (i.e., do not include extension).                
# out_summary optional            Filename for summarized results.  Automatically
#                                 formatted as .csv (i.e., do not include extension).     
#
# TYPICAL WAYS TO EXECUTE THIS CODE AND DESCRIPTION, if applicable:
#
# scheme_apply(data=pvals_base,
#              scheme_key=scheme_key,
#              out_simulations="simulations_base",
#              out_summary="summary_base")
# --------------------------------------------------------------------------------
# **eof***************************************************************************

scheme_apply <- function(data,
                         scheme_key,
                         out_simulations,
                         out_summary)
{

	start <- Sys.time()
	simulations_frame <- NULL
	varnames <- names(data)

	for(j in 1:nrow(data)){
                
                # 20 test endpoints for BACE 
		#print(c(data[j, varnames[1]], data[j, varnames[2]], data[j, varnames[3]], data[j, varnames[4]], data[j, varnames[5]] ))
		 
	      pvals <- c(data[j, varnames[1]], data[j, varnames[2]], data[j, varnames[3]], data[j, varnames[4]], data[j, varnames[5]],
                       data[j, varnames[6]], data[j, varnames[7]], data[j, varnames[8]], data[j, varnames[9]], data[j, varnames[10]], 
                       data[j, varnames[11]], data[j, varnames[12]], data[j, varnames[13]], data[j, varnames[14]], data[j, varnames[15]],
                       data[j, varnames[16]], data[j, varnames[17]], data[j, varnames[18]], data[j, varnames[19]], data[j, varnames[20]])

		for(k in 1:dim(scheme_key)[1]){

			name <- as.character(scheme_key$scheme.name)[k]
			object <- as.character(scheme_key$scheme.object)[k]

			if(object == "graph_gs_all"){
				result <- gMCP(eval(parse(text=object)), c(pvals, 0.05))
				results <- attr(result, "rejected")
				results <- results[1:length(varnames)]
			} else {
				result <- gMCP(eval(parse(text=object)), pvals)
				results <- attr(result, "rejected")
			}

			#print(results)
			results_names <- gsub("-", ".", gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", paste(name, names(results), "rej", sep=".")))))
			#print(results_names)

			scheme_frame_results <- data.frame(t(results))
			names(scheme_frame_results) <- results_names

			if(k == 1){
				scheme_frame_sim <- scheme_frame_results
			} else {
				scheme_frame_sim <- cbind(scheme_frame_sim, scheme_frame_results)
			}
		}

		simulations_frame_sim <- cbind(data[j,], scheme_frame_sim)
		simulations_frame <- rbind(simulations_frame, simulations_frame_sim)
	}

	if(!missing(out_simulations)){
		write.table(simulations_frame, 
                            file=paste(out_simulations, "csv", sep="."), 
                            sep=",", row.names=F)
	}

	summary_frame <- cbind(data.frame(data=deparse(substitute(data)), 
                               scheme=paste(as.character(scheme_key$scheme.label), collapse=', '), 
                               sims=nrow(data)), 
                               data.frame(t(colMeans(simulations_frame[!(names(simulations_frame) %in% varnames)]))))

	summary_frame_names_ind <- !(names(summary_frame) %in% c("data", "scheme", "sims"))
	names(summary_frame)[summary_frame_names_ind] <- paste(names(summary_frame)[summary_frame_names_ind], "prop", sep=".")

	if(!missing(out_summary)){
		write.table(summary_frame, 
                            file=paste(out_summary, "csv", sep="."), 
                            sep=",", row.names=F)
	}

	end <- Sys.time()
	print(end - start)

	invisible(list(simulations=simulations_frame, summary=summary_frame))
}





# to add output the simulation data in long format (other than wide format);
scheme_apply2 <- function(data,
                         scheme_key,
                         out_simulations,
                         out_simulations_long,
                         out_summary)
{

	start <- Sys.time()
	simulations_frame <- NULL
      simulations_frame_long <- NULL
	varnames <- names(data)

	for(j in 1:nrow(data)){
                
                # 20 test endpoints for BACE 
		#print(c(data[j, varnames[1]], data[j, varnames[2]], data[j, varnames[3]], data[j, varnames[4]], data[j, varnames[5]] ))
		 
	      pvals <- c(data[j, varnames[1]], data[j, varnames[2]], data[j, varnames[3]], data[j, varnames[4]], data[j, varnames[5]],
                       data[j, varnames[6]], data[j, varnames[7]], data[j, varnames[8]], data[j, varnames[9]], data[j, varnames[10]], 
                       data[j, varnames[11]], data[j, varnames[12]], data[j, varnames[13]], data[j, varnames[14]], data[j, varnames[15]],
                       data[j, varnames[16]], data[j, varnames[17]], data[j, varnames[18]], data[j, varnames[19]], data[j, varnames[20]])

		for(k in 1:dim(scheme_key)[1]){

			name <- as.character(scheme_key$scheme.name)[k]
			object <- as.character(scheme_key$scheme.object)[k]

			if(object == "graph_gs_all"){
				result <- gMCP(eval(parse(text=object)), c(pvals, 0.05))
				results <- attr(result, "rejected")
				results <- results[1:length(varnames)]
			} else {
				result <- gMCP(eval(parse(text=object)), pvals)
				results <- attr(result, "rejected")
			}

			#print(results)
			results_names <- gsub("-", ".", gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", paste(name, names(results), "rej", sep=".")))))
                  results_names_long <- gsub("-", ".", gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", paste(names(results), "rej", sep=".")))))

			#print(results_names)
                  #print(results_names_long)

			scheme_frame_results <- data.frame(t(results))
			names(scheme_frame_results) <- results_names

                  scheme_frame_results_long <- data.frame(name,t(results))
			names(scheme_frame_results_long) <- c("scheme_name", results_names_long)
    
			if(k == 1){
				scheme_frame_sim <- scheme_frame_results
                        scheme_frame_sim_long <- scheme_frame_results_long    #added by ZZJ
			} else {
				scheme_frame_sim       <- cbind(scheme_frame_sim, scheme_frame_results)
                        scheme_frame_sim_long <- rbind(scheme_frame_sim_long, scheme_frame_results_long)  #added by ZZJ
			}
		}

            # to get simulations_frame in long format
		simulations_frame_sim <- cbind(data[j,], scheme_frame_sim)
		simulations_frame <- rbind(simulations_frame, simulations_frame_sim)
           
            # to get simulations_frame_long in short format
            simulations_frame_sim_long <- cbind(data[j,], scheme_frame_sim_long)
		simulations_frame_long <- rbind(simulations_frame_long, simulations_frame_sim_long)
	}

	if(!missing(out_simulations)){
		write.table(simulations_frame, 
                            file=paste(out_simulations, "csv", sep="."), 
                            sep=",", row.names=F)
	}

      if(!missing(out_simulations_long)){
		write.table(simulations_frame_long, 
                            file=paste(out_simulations_long, "csv", sep="."), 
                            sep=",", row.names=F)
	}

	summary_frame <- cbind(data.frame(data=deparse(substitute(data)), 
                               scheme=paste(as.character(scheme_key$scheme.label), collapse=', '), 
                               sims=nrow(data)), 
                               data.frame(t(colMeans(simulations_frame[!(names(simulations_frame) %in% varnames)]))))

	summary_frame_names_ind <- !(names(summary_frame) %in% c("data", "scheme", "sims"))
	names(summary_frame)[summary_frame_names_ind] <- paste(names(summary_frame)[summary_frame_names_ind], "prop", sep=".")

	if(!missing(out_summary)){
		write.table(summary_frame, 
                            file=paste(out_summary, "csv", sep="."), 
                            sep=",", row.names=F)
	}

	end <- Sys.time()
	print(end - start)

	invisible(list(simulations=simulations_frame, summary=summary_frame))
}








# updated on 04/12/2017 with updated testing scheme with preffered order;
# endpoint names to be used for variable, node, and tick mark naming
# 
# D1-EP01: 50mg ADAS-Cog13
# D1-EP02: 50mg iADRS
# D1-EP03: 50mg CDR-SB
# D1-EP04: 50mg ADL
# D1-EP05: 50mg MMSE
# D1-EP06: 50mg CDR-GS 
# D1-EP07: 50mg NPI
# D1-EP08: 50mg Prolonged
# D1-EP09: 50mg Initiation
# D1-EP10: 50mg FAQ
# 
# to be more flexibel in the future in case of scheme endpoint change
endpoint_label1 <- c("LY1_EP01", "LY1_EP02", "LY1_EP03","LY1_EP04", "LY1_EP05",   "LY1_EP06", "LY1_EP07", "LY1_EP08","LY1_EP09", "LY1_EP10")
endpoint_label2 <- c("LY2_EP01", "LY2_EP02", "LY2_EP03","LY2_EP04", "LY2_EP05",   "LY2_EP06", "LY2_EP07", "LY2_EP08","LY2_EP09", "LY2_EP10")
endpoint_labels <-c(endpoint_label1,endpoint_label2)



##---Simulate endpoint p-values---##

#Common input for all scenarios
# 734/arm in AZES from Scott A.
n_pvals <- 20   # use 1000 after testing done

n_subj <- rep(c(734, 734, 734, 734, 734, 734, 734, 734, 734, 734),2) 
 
corr_c <-c(
1.00000,    0.5074117,	0.41141,	0.5929294,	0.6474779,	0.5324416,	0.2804741,	0.20571,	0.3257286,	0.3690187,
0.5074117,  1.00000,	  0.59848,	0.6048592,	0.5124965,	0.2804741,	0.5122795,	0.29924,	0.3406493,	0.2962815,
0.41141,	  0.59848,	  1.00000,	0.54232,	  0.41016,	  0.20571,	  0.29924,	  0.50000,	0.27116,  	0.20508,
0.5929294,	0.6048592,	0.54232,	1.00000,  	0.5725556,	0.3257286,	0.3406493,	0.27116,	0.5389036,	0.3319307,
0.6474779,	0.5124965,	0.41016,	0.5725556,	1.00000,	  0.3690187,	0.2962815,	0.20508,	0.3319307,	0.570457,
0.5324416,	0.2804741,	0.20571,	0.3257286,	0.3690187,	1.00000,	  0.5074117,	0.41141,	0.5929294,	0.6474779,
0.2804741,	0.5122795,	0.29924,	0.3406493,	0.2962815,	0.5074117,	1.00000,	  0.59848,	0.6048592,	0.5124965,
0.20571,	  0.29924,	  0.50000,	0.27116,	  0.20508,	  0.41141,	  0.59848,	  1.00000,	0.54232,  	0.41016,
0.3257286,	0.3406493,	0.27116,	0.5389036,	0.3319307,	0.5929294,	0.6048592,	0.54232,	1.00000,	  0.5725556,
0.3690187,	0.2962815,	0.20508,	0.3319307,	0.570457,	  0.6474779,	0.5124965,	0.41016,	0.5725556,	1.00000)

# in matrix format
corr_m<-matrix(corr_c,10,10,byrow=TRUE)

corr <-rbind(cbind(corr_m,0.5*corr_m), cbind(0.5*corr_m, corr_m))




varnames <- paste("pvalue", gsub("-", ".", gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", endpoint_labels)))), sep="_")


##                               LY1(50mg)                            LY2(20mg)
##--------------------------------------------------------------------------------------------------------------                 
# LY1_EP01 LY1_EP02 LY1_EP03 LY1_EP04 LY1_EP05 LY1_EP06 LY1_EP07 LY1_EP08 LY1_EP09 LY1_EP10 
# LY2_EP01 LY2_EP02 LY2_EP03 LY2_EP04 LY2_EP05 LY2_EP06 LY2_EP07 LY2_EP08 LY2_EP09 LY2_EP10 

## base      scenario  0.2,0.2,0.15,0.1,0.2,0.15,0.1,0.1,0.1,0.1
##                     0.2,0.2,0.15,0.1,0.2,0.15,0.1,0.1,0.1,0.1

## cogfun    scenario  0.175,0.175,0.175,0.12,0.175,0.175,0.1,0.1,0.1,0.1
##                     0.175,0.175,0.175,0.12,0.175,0.175,0.1,0.1,0.1,0.1

## low       scenario  0.11, 0.11, 0.12, 0.05,0.11, 0.12, 0.05,0.05,0.05,0.05 
##                     0.11, 0.11, 0.12, 0.05,0.11, 0.12, 0.05,0.05,0.05,0.05  (updated on 03/12/2017)
                       
##--------------------------------------------------------------------------------------------------------------  
           
effect_size_base      <- rep( c(0.2,0.2,0.15,0.1,0.2,0.15,0.1,0.1,0.1,0.1),2)
effect_size_cogfun    <- rep( c(0.175,0.175,0.175,0.12,0.175,0.175,0.1,0.1,0.1,0.1),2)
effect_size_low       <- rep( c(0.11, 0.11, 0.12, 0.05,0.11, 0.12, 0.05,0.05,0.05,0.05),2) 




pvals_by_simulation <- function() {
  
pvals_base <- gen_pvalues(n_pvals=n_pvals,
                          effect_size=effect_size_base,
                          n_subj=n_subj,
                          corr=corr,
                          varnames=varnames,
                          out_p_vals="pvals_base",
                          seed=6789)
# attributes(pvals_base)
# pvals_base <- read.csv("pvals_base.csv", head=T)
# pvals_base_test <- as.matrix(pvals_base<=0.05)
# head(pvals_base_test,n=5 )
# apply(pvals_base_test,2,mean);


pvals_cogfun <- gen_pvalues(n_pvals=n_pvals,
                                effect_size=effect_size_cogfun,
                                n_subj=n_subj,
                                corr=corr,
                                varnames=varnames,
                                out_p_vals="pvals_cogfun",
                                seed=6789)


pvals_low         <- gen_pvalues(n_pvals=n_pvals,
                                 effect_size=effect_size_low,
                                 n_subj=n_subj,
                                 corr=corr,
                                 varnames=varnames,
                                 out_p_vals="pvals_low",
                                 seed=6789)


return(list(pvals_base  =pvals_base ,
            pvals_cogfun=pvals_cogfun,
            pvals_low   =pvals_low ))
}


########################################################
# approach (1) by simulation
########################################################
# pvals_simu<- pvals_by_simulation();
# pvals_base   <- pvals_simu$pvals_base
# pvals_cogfun <- pvals_simu$pvals_cogfun
# pvals_low    <- pvals_simu$pvals_low 
# end of approach (1)





########################################################
# approach (2) by bootstrapping
# /home/c019143/mt_gk/az_bace/data
# pval_base.csv  
# pval_cogfun.csv
# pval_low.csv
# pval_cogfun_low
#
########################################################
pvals_base  <-     read.csv("/home/c019143/mt_gk/az_bace/data/pval_base.csv", head=T)
pvals_cogfun<-     read.csv("/home/c019143/mt_gk/az_bace/data/pval_cogfun.csv", head=T)
pvals_low   <-     read.csv("/home/c019143/mt_gk/az_bace/data/pval_low.csv", head=T)
pvals_cogfun_low<- read.csv("/home/c019143/mt_gk/az_bace/data/pval_cogfun_low.csv", head=T)


colnames(pvals_base) <- varnames
colnames(pvals_cogfun) <- varnames
colnames(pvals_low) <- varnames
colnames(pvals_cogfun_low) <- varnames

# end of approach (2)



# for the convenience of data manipulation, add simulation_id for the data generated
pvals_base$simulation_id      <- 1:nrow(pvals_base)
pvals_cogfun$simulation_id    <- 1:nrow(pvals_cogfun)
pvals_low$simulation_id       <- 1:nrow(pvals_low)
pvals_cogfun_low$simulation_id<- 1:nrow(pvals_cogfun_low)

##Identify scenarios
# Note the scenario name must match with the name of the results by  function scheme_apply()
scenario_key <- data.frame(
 scenario=c(1:4), 
 scenario.name=  c("base      ",   "cogfun    ",           "low       ",    "cogfun_low" ), 
 scenario.label= c("CS1 base      ","CS2 cogfun    ", "CS3 low       "   ,"CS4 cogfun_low"), 
 scenario.object=c("pvals_base      ",   "pvals_cogfun    ",     "pvals_low       ", "pvals_cogfun_low" ))

scenario_key

# graphGUI()

##---Prep testing schemes---##
# scheme_key is derived per weight set up  
# source("lzax graphical testing scheme_schemes.R")


# caution for use rm(list = ls())
#First, include this sort of code before calling the scheme set-up file:

##weight 1 scenario (proposed by lei) investigated
alpha1_set<-c(1.0 )    # the initial weight w on H1(dose1) and (1-w) on H11(dose2)
w0 <-c(0.5,0.6,0.7,0.8)               # pass to Dose2 after testing on Dose1 is significant on 4/12/2017

w1 <- c(0.9)
w2 <- c(0.2)
w3 <- c(0)
w4 <- c(0.1,0.4,0.7,1.0 )
w5 <- c(0.7)

##weight 2 scenario investigated
# alpha1_set<-c(0.5 )    # the initial weight w on H1(dose1) and (1-w) on H11(dose2)
# w1 <- c(0.5,0.6,0.7,0.8,0.9)
# w2 <- c(0.5,0.6,0.7,0.8,0.9)
# w3 <- c(0.5)


n_alpha1 <-NROW(alpha1_set)
n_w0 <- NROW(w0)
n_w1 <- NROW(w1)
n_w2 <- NROW(w2)
n_w3 <- NROW(w3)
n_w4 <- NROW(w4)
n_w5 <- NROW(w5)

n_grids <- n_alpha1*n_w0*n_w1*n_w2*n_w3*n_w4*n_w5  # it will be updated due to the weight constraint conditions
schemes <- c(1:n_grids)         # is added into the scheme_key data frame

library(gMCP)
library(utils)

#graphGUI()



##---Add new schemes to this list---#


 
## to be updated
## 03/22/2017 in Wave6
## m <- rbind(LY1_EP01=c(0, 0.45, 0.05, 0, 0, 0, 0, 0, 0, 0, 0.5, 0, 0, 0, 0, 0, 0,  0, 0, 0),
##            LY1_EP02=c(0, 0, 0.3, 0.7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0),
##            LY1_EP03=c(0, 0.8, 0, 0, 0.2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0),
##            LY1_EP04=c(0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0.9, 0, 0, 0, 0, 0, 0, 0, 0,  0),
##            LY1_EP05=c(0, 0, 0, 0, 0, 0.5, 0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0),
##            LY1_EP06=c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
##            LY1_EP07=c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
##            LY1_EP08=c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
##            LY1_EP09=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
##            LY1_EP10=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
##            LY2_EP01=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.9, 0.1, 0, 0, 0, 0, 0, 0,  0),
##            LY2_EP02=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.3, 0.7, 0, 0, 0, 0, 0,  0),
##            LY2_EP03=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.8, 0, 0, 0.2, 0, 0, 0, 0,  0),
##            LY2_EP04=c(0.9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0,  0),
##            LY2_EP05=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0.5, 0, 0,  0),
##            LY2_EP06=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
##            LY2_EP07=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
##            LY2_EP08=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0),
##            LY2_EP09=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
##            LY2_EP10=c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
## weights <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
## graph <- new("graphMCP", m=m, weights=weights)
## pvalues <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
## gMCP(graph, pvalues, test="Bonferroni", alpha=0.05)






##Scheme 1 variants (can be updated after being finalized)

# i<-0.9; j<-0.8;k<-0.9; l<-0.0; alpha1<-0.5;  

scheme_key <- data.frame(scheme.name=NULL, scheme.label=NULL, scheme.object=NULL)

n_grids <-0
for (alpha1 in alpha1_set) {
for (d1 in w0)  
for(i in w1){
  for(j in w2){
    for(k in w3){
      for(l in w4){     
      for(p in w5){
      n_grids <- n_grids + 1 

      m2 <- 
      rbind(H1=c(0,   i*d1,   (1-i)*d1, 0,    0,   0,  0,  0,   0,  0,     1-d1, 0,   0,  0,  0,   0,  0,  0,   0,  0),
            H2=c(0,   0,   	j,   	    1-j,  0,   0,  0,  0,   0,  0,       0,  0,   0,  0,  0,   0,  0,  0,   0,  0),
            H3=c(0,   k,   	0,   	    0,  1-k,   0,  0,  0,   0,  0,       0,  0,   0,  0,  0,   0,  0,  0,   0,  0),
           #H4=c(0,   0,   	0,        0,    1.0, 0,  0,  0,   0,  0,       0,  0,   0,  0,  0,   0,  0,  0,   0,  0),  # for scheme 1
            H4=c(0,   0,   	0,        0,    l,   0,  0,  0,   0,  0,     1-l,  0,   0,  0,  0,   0,  0,  0,   0,  0),  # for scheme 2
            H5=c(0,   0,   	0,        0,    0,   p,  1-p,0,   0,  0,       0,  0,   0,  0,  0,   0,  0,  0,   0,  0),
            H6=c(0,   0,   	0,        0,    0,   0,  0,  1,   0,  0,       0,  0,   0,  0,  0,   0,  0,  0,   0,  0),
            H7=c(0,   0,   	0,        0,    0,   0,  0,  1,   0,  0,       0,  0,   0,  0,  0,   0,  0,  0,   0,  0),
            H8=c(0,   0,   	0,        0,    0,   0,  0,  0,   1,  0,       0,  0,   0,  0,  0,   0,  0,  0,   0,  0),
            H9=c(0,   0,   	0,        0,    0,   0,  0,  0,   0,  1,       0,  0,   0,  0,  0,   0,  0,  0,   0,  0),
            H10=c(0,  0,   	0,        0,    0,   0,  0,  0,   0,  0,       1.0,0,   0,  0,  0,   0,  0,  0,   0,  0),

            H11=c(0,  0,   	0,        0,    0,   0,  0,  0,   0,  0,       0,  i,   1-i,0,  0,   0,  0,  0,   0,  0),
            H12=c(0,  0,   	0,        0,    0,   0,  0,  0,   0,  0,       0,  0,   j,  1-j,0,   0,  0,  0,   0,  0),
            H13=c(0,  0,   	0,        0,    0,   0,  0,  0,   0,  0,       0,  k,   0,  0,  1-k, 0,  0,  0,   0,  0),
            H14=c(1-l,0,   	0,        0,    0,   0,  0,  0,   0,  0,       0,  0,   0,  0,  l,   0,  0,  0,   0,  0),
            H15=c(0,  0,   	0,        0,    0,   0,  0,  0,   0,  0,       0,  0  , 0,  0,  0,   p,  1-p,0,   0,  0),
            H16=c(0,  0,   	0,        0,    0,   0,  0,  0,   0,  0,       0,  0,   0,  0,  0,   0,  0,  1,   0,  0),
            H17=c(0,  0,   	0,        0,    0,   0,  0,  0,   0,  0,       0,  0,   0,  0,  0,   0,  0,  1,   0,  0),
            H18=c(0,  0,   	0,        0,    0,   0,  0,  0,   0,  0,       0,  0,   0,  0,  0,   0,  0,  0,   1,  0),
            H19=c(0,  0,   	0,        0,    0,   0,  0,  0,   0,  0,       0,  0,   0,  0,  0,   0,  0,  0,   0,  1),
            H20=c(1.0,0,   	0,        0,    0,   0,  0,  0,   0,  0,       0,  0,   0,  0,  0,   0,  0,  0,   0,  0))


	 
      rownames(m2) <- endpoint_labels

      # assume alpha is split between EP1 and EP10, more options to be explored;
      weights <- c(alpha1, 0, 0, 0, 0, 0, 0, 0, 0, 0,   1-alpha1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      
      # for scheme 2
      graph <- new("graphMCP", m=m2, weights=weights )
      
      nodeX1 <-c(250,100,400, 400, 250, 250,100, 400, 250,250);
      nodeX <- c(nodeX1, nodeX1 +400); 
      nodeY1 <-c(100, 200, 200, 400, 400, 500, 600, 600, 600,700) 
	nodeY <- c(nodeY1,nodeY1)

	graph@nodeAttr$X <- nodeX
	graph@nodeAttr$Y <- nodeY

	assign(paste("graph_bace_s1_w0",d1,"w1", i, "w2", j, "w3", k,"w4", l,"w5", p,"dose1", alpha1, sep="_"), graph)

   	scheme_key_indiv <- data.frame(
                     scheme.name=paste("bace_s1_w0",d1, "w1", i, "w2", j, "w3", k,"w4", l,"w5", p,"dose1", alpha1, sep="_"), 
             scheme.label=paste("Scheme 1 (w0=",d1,";w1=", i, "; w2=", j, "; w3=", k, "; w4=", l, "; w5=", p, "; dose1=", alpha1, ")", sep=""), 
             scheme.object=paste("graph_bace_s1_w0",d1, "w1", i, "w2", j, "w3", k,"w4", l,"w5", p, "dose1", alpha1, sep="_"))

	  scheme_key <- rbind(scheme_key, scheme_key_indiv) 
   }
  }
  }
}
}
}

scheme_key$scheme = c(1:n_grids)  # added for convenience


rm(weights)
rm(nodeX)
rm(nodeY)
rm(graph)
rm(scheme_key_indiv)




head(scheme_key)
tail(scheme_key)

##  head(scheme_key,n=16)
##                                                scheme.name                                                    scheme.label                                                 scheme.object scheme
## 1  bace_s1_w0_0.2_w1_0.9_w2_0.2_w3_0_w4_0.1_w5_0.7_dose1_1 Scheme 1 (w0=0.2;w1=0.9; w2=0.2; w3=0; w4=0.1; w5=0.7; dose1=1) graph_bace_s1_w0_0.2_w1_0.9_w2_0.2_w3_0_w4_0.1_w5_0.7_dose1_1      1
## 2  bace_s1_w0_0.2_w1_0.9_w2_0.2_w3_0_w4_0.4_w5_0.7_dose1_1 Scheme 1 (w0=0.2;w1=0.9; w2=0.2; w3=0; w4=0.4; w5=0.7; dose1=1) graph_bace_s1_w0_0.2_w1_0.9_w2_0.2_w3_0_w4_0.4_w5_0.7_dose1_1      2
## 3  bace_s1_w0_0.2_w1_0.9_w2_0.2_w3_0_w4_0.7_w5_0.7_dose1_1 Scheme 1 (w0=0.2;w1=0.9; w2=0.2; w3=0; w4=0.7; w5=0.7; dose1=1) graph_bace_s1_w0_0.2_w1_0.9_w2_0.2_w3_0_w4_0.7_w5_0.7_dose1_1      3
## 4    bace_s1_w0_0.2_w1_0.9_w2_0.2_w3_0_w4_1_w5_0.7_dose1_1   Scheme 1 (w0=0.2;w1=0.9; w2=0.2; w3=0; w4=1; w5=0.7; dose1=1)   graph_bace_s1_w0_0.2_w1_0.9_w2_0.2_w3_0_w4_1_w5_0.7_dose1_1      4
## 5  bace_s1_w0_0.3_w1_0.9_w2_0.2_w3_0_w4_0.1_w5_0.7_dose1_1 Scheme 1 (w0=0.3;w1=0.9; w2=0.2; w3=0; w4=0.1; w5=0.7; dose1=1) graph_bace_s1_w0_0.3_w1_0.9_w2_0.2_w3_0_w4_0.1_w5_0.7_dose1_1      5
## 6  bace_s1_w0_0.3_w1_0.9_w2_0.2_w3_0_w4_0.4_w5_0.7_dose1_1 Scheme 1 (w0=0.3;w1=0.9; w2=0.2; w3=0; w4=0.4; w5=0.7; dose1=1) graph_bace_s1_w0_0.3_w1_0.9_w2_0.2_w3_0_w4_0.4_w5_0.7_dose1_1      6
## 7  bace_s1_w0_0.3_w1_0.9_w2_0.2_w3_0_w4_0.7_w5_0.7_dose1_1 Scheme 1 (w0=0.3;w1=0.9; w2=0.2; w3=0; w4=0.7; w5=0.7; dose1=1) graph_bace_s1_w0_0.3_w1_0.9_w2_0.2_w3_0_w4_0.7_w5_0.7_dose1_1      7
## 8    bace_s1_w0_0.3_w1_0.9_w2_0.2_w3_0_w4_1_w5_0.7_dose1_1   Scheme 1 (w0=0.3;w1=0.9; w2=0.2; w3=0; w4=1; w5=0.7; dose1=1)   graph_bace_s1_w0_0.3_w1_0.9_w2_0.2_w3_0_w4_1_w5_0.7_dose1_1      8
## 9  bace_s1_w0_0.4_w1_0.9_w2_0.2_w3_0_w4_0.1_w5_0.7_dose1_1 Scheme 1 (w0=0.4;w1=0.9; w2=0.2; w3=0; w4=0.1; w5=0.7; dose1=1) graph_bace_s1_w0_0.4_w1_0.9_w2_0.2_w3_0_w4_0.1_w5_0.7_dose1_1      9
## 10 bace_s1_w0_0.4_w1_0.9_w2_0.2_w3_0_w4_0.4_w5_0.7_dose1_1 Scheme 1 (w0=0.4;w1=0.9; w2=0.2; w3=0; w4=0.4; w5=0.7; dose1=1) graph_bace_s1_w0_0.4_w1_0.9_w2_0.2_w3_0_w4_0.4_w5_0.7_dose1_1     10
## 11 bace_s1_w0_0.4_w1_0.9_w2_0.2_w3_0_w4_0.7_w5_0.7_dose1_1 Scheme 1 (w0=0.4;w1=0.9; w2=0.2; w3=0; w4=0.7; w5=0.7; dose1=1) graph_bace_s1_w0_0.4_w1_0.9_w2_0.2_w3_0_w4_0.7_w5_0.7_dose1_1     11
## 12   bace_s1_w0_0.4_w1_0.9_w2_0.2_w3_0_w4_1_w5_0.7_dose1_1   Scheme 1 (w0=0.4;w1=0.9; w2=0.2; w3=0; w4=1; w5=0.7; dose1=1)   graph_bace_s1_w0_0.4_w1_0.9_w2_0.2_w3_0_w4_1_w5_0.7_dose1_1     12
## 13 bace_s1_w0_0.5_w1_0.9_w2_0.2_w3_0_w4_0.1_w5_0.7_dose1_1 Scheme 1 (w0=0.5;w1=0.9; w2=0.2; w3=0; w4=0.1; w5=0.7; dose1=1) graph_bace_s1_w0_0.5_w1_0.9_w2_0.2_w3_0_w4_0.1_w5_0.7_dose1_1     13
## 14 bace_s1_w0_0.5_w1_0.9_w2_0.2_w3_0_w4_0.4_w5_0.7_dose1_1 Scheme 1 (w0=0.5;w1=0.9; w2=0.2; w3=0; w4=0.4; w5=0.7; dose1=1) graph_bace_s1_w0_0.5_w1_0.9_w2_0.2_w3_0_w4_0.4_w5_0.7_dose1_1     14
## 15 bace_s1_w0_0.5_w1_0.9_w2_0.2_w3_0_w4_0.7_w5_0.7_dose1_1 Scheme 1 (w0=0.5;w1=0.9; w2=0.2; w3=0; w4=0.7; w5=0.7; dose1=1) graph_bace_s1_w0_0.5_w1_0.9_w2_0.2_w3_0_w4_0.7_w5_0.7_dose1_1     15
## 16   bace_s1_w0_0.5_w1_0.9_w2_0.2_w3_0_w4_1_w5_0.7_dose1_1   Scheme 1 (w0=0.5;w1=0.9; w2=0.2; w3=0; w4=1; w5=0.7; dose1=1)   graph_bace_s1_w0_0.5_w1_0.9_w2_0.2_w3_0_w4_1_w5_0.7_dose1_1     16



 

##Identify and test schemes as example
# graph_bace_s1_w0_0.3_w1_0.9_w2_0.2_w3_0_w4_0.7_w5_0.7_dose1_1  
# graphGUI("graph_bace_s1_w0_0.3_w1_0.9_w2_0.2_w3_0_w4_0.7_w5_0.7_dose1_1")



##---Apply schemes to (pvalue) data---##
## begin of checking;

 data            <- pvals_base
 scheme_key      <-scheme_key  
 out_simulations <-"simulations_ck"
 out_summary     <-"summary_ck"

 start <- Sys.time()
 simulations_frame <- NULL
simulations_frame_long <- NULL
 varnames <- names(data)

 nrow(data)

 j<-1
 k <-1

 ## blocked for only j=1 	for(j in 1:nrow(data)){
                
            # 20 test endpoints for BACE 
		#print(c(data[j, varnames[1]], data[j, varnames[2]], data[j, varnames[3]], data[j, varnames[4]], data[j, varnames[5]] ))
		 
	      pvals <- c(data[j, varnames[1]], data[j, varnames[2]], data[j, varnames[3]], data[j, varnames[4]], data[j, varnames[5]],
                       data[j, varnames[6]], data[j, varnames[7]], data[j, varnames[8]], data[j, varnames[9]], data[j, varnames[10]],
                       data[j, varnames[11]], data[j, varnames[12]], data[j, varnames[13]], data[j, varnames[14]], data[j, varnames[15]],
                       data[j, varnames[16]], data[j, varnames[17]], data[j, varnames[18]], data[j, varnames[19]], data[j, varnames[20]] )

 ## blocked for only k=1 		for(k in 1:dim(scheme_key)[1]){

			name <- as.character(scheme_key$scheme.name)[k]
			object <- as.character(scheme_key$scheme.object)[k]

			result <- gMCP(eval(parse(text=object)), pvals)
                  attributes(result)
			results <- attr(result, "rejected")
			 

			#print(results)
			results_names <- gsub("-", ".", gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", paste(name, names(results), "rej", sep=".")))))
	            results_names_long <- gsub("-", ".", gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", paste(names(results), "rej", sep=".")))))

			#print(results_names)

			scheme_frame_results <- data.frame(t(results))
			names(scheme_frame_results) <- results_names

                  scheme_frame_results_long <- data.frame(name,t(results))
			names(scheme_frame_results_long) <- c("scheme_name", results_names_long)
    

			if(k == 1){
				scheme_frame_sim <- scheme_frame_results
                        scheme_frame_sim_long <- scheme_frame_results_long    #added by ZZJ
                           
			} else {
				scheme_frame_sim       <- cbind(scheme_frame_sim, scheme_frame_results)
                        scheme_frame_sim_long <- rbind(scheme_frame_sim_long, scheme_frame_results_long)  #added by ZZJ
			}
 ## blocked for only k=1		}
            # to get simulations_frame in long format
		simulations_frame_sim <- cbind(data[j,], scheme_frame_sim)
		simulations_frame <- rbind(simulations_frame, simulations_frame_sim)
           
            # to get simulations_frame_long in short format
            simulations_frame_sim_long <- cbind(data[j,], scheme_frame_sim_long)
		simulations_frame_long <- rbind(simulations_frame_long, simulations_frame_sim_long)

 ## blocked for only j=1	}



	if(!missing(out_simulations)){
		write.table(simulations_frame, 
                            file=paste(out_simulations, "csv", sep="."), 
                            sep=",", row.names=F)
	}

	summary_frame <- cbind(data.frame(data=deparse(substitute(data)), 
                               scheme=paste(as.character(scheme_key$scheme.label), collapse=', '), 
                               sims=nrow(data)), 
                               data.frame(t(colMeans(simulations_frame[!(names(simulations_frame) %in% varnames)]))))

	summary_frame_names_ind <- !(names(summary_frame) %in% c("data", "scheme", "sims"))
	names(summary_frame)[summary_frame_names_ind] <- paste(names(summary_frame)[summary_frame_names_ind], "prop", sep=".")

	if(!missing(out_summary)){
		write.table(summary_frame, 
                            file=paste(out_summary, "csv", sep="."), 
                            sep=",", row.names=F)
	}

	end <- Sys.time()
	print(end - start)

  scheme_simu_ck <- list(simulations=simulations_frame, summary=summary_frame) 
 
  # end of checking;









 


base <- scheme_apply2(data=pvals_base,   # can use subset data such as pvals_base[1:10, ] for test
                     scheme_key=scheme_key,
                     out_simulations="simulations_base",
                     out_simulations_long="simulations_long_base",
                     out_summary="summary_base")

 
cogfun <- scheme_apply2(data=pvals_cogfun ,
                            scheme_key=scheme_key,
                            out_simulations="simulations_cogfun",
                            out_simulations_long="simulations_long_cogfun",
                            out_summary="summary_cogfun")


low <- scheme_apply2(data=pvals_low ,
                            scheme_key=scheme_key,
                            out_simulations="simulations_low",
                            out_simulations_long="simulations_long_low",
                            out_summary="summary_low")


cogfun_low <- scheme_apply2(data=pvals_cogfun_low,
                            scheme_key=scheme_key,
                            out_simulations="simulations_cogfun_low",
                            out_simulations_long="simulations_long_cogfun_low",
                            out_summary="summary_cogfun_low")



##post-process the simulation data and accumulate them for analysis;
# scenario_key
#   scenario scenario.name scenario.label scenario.object
# 1        1      base         CS1 base         pvals_base   
# 2        2      cogfun       CS2 cogfun       pvals_cogfun 
# 3        3      low          CS3 low          pvals_low    
# 4        4      cogfun_low   CS4 cogfun_low   pvals_cogfun_low    


simulations_long_base       <- read.csv("simulations_long_base.csv", head=T)
simulations_long_cogfun     <- read.csv("simulations_long_cogfun.csv", head=T)
simulations_long_low        <- read.csv("simulations_long_low.csv", head=T)
simulations_long_cogfun_low <- read.csv("simulations_long_cogfun_low.csv", head=T)


simulations_long_base$data   <- scenario_key$scenario.label[1] 
simulations_long_cogfun$data <- scenario_key$scenario.label[2] 
simulations_long_low$data    <- scenario_key$scenario.label[3] 
simulations_long_cogfun_low$data <- scenario_key$scenario.label[4] 


simulations_long_allcases    <- rbind( simulations_long_base, 
                                       simulations_long_cogfun, 
                                       simulations_long_low,
                                       simulations_long_cogfun_low)
 
#  Note: scheme_name ( bace_s1_w0_0.2_w1_0.9_w2_0.2_w3_0_w4_0.1_w5_0.7_dose1_1)
simulations_long_allcases$w0   <-as.numeric( gsub('_w1.*',"",    gsub(".*w0_", "", simulations_long_allcases$scheme_name  ) )  )
simulations_long_allcases$w1   <-as.numeric( gsub('_w2.*',"",    gsub(".*w1_", "", simulations_long_allcases$scheme_name  ) )  )
simulations_long_allcases$w2   <-as.numeric( gsub('_w3.*',"",    gsub(".*w2_", "", simulations_long_allcases$scheme_name  ) )  )
simulations_long_allcases$w3   <-as.numeric( gsub('_w4.*',"",    gsub(".*w3_", "", simulations_long_allcases$scheme_name  ) )  )
simulations_long_allcases$w4   <-as.numeric( gsub('_w5.*',"",    gsub(".*w4_", "", simulations_long_allcases$scheme_name  ) )  )
simulations_long_allcases$w5   <-as.numeric( gsub('_dose1.*',"", gsub(".*w5_", "", simulations_long_allcases$scheme_name  ) )  )
simulations_long_allcases$w_d1 <-as.numeric( gsub(".*dose1_", "", simulations_long_allcases$scheme_name  ) )  

simulations_long_allcases<-
simulations_long_allcases[order(simulations_long_allcases$data ,
                                simulations_long_allcases$scheme_name,
                                simulations_long_allcases$simulation_id,decreasing =FALSE ),]

dim(simulations_long_allcases)
head(simulations_long_allcases)
tail(simulations_long_allcases)
  
write.table(simulations_long_allcases, "simulations_long_allcases.csv", sep=",", row.names=F)
















attributes(base)
attributes(cogfun)

##---Put summaries together---##
scenario_key

summary_frame_final <- NULL
for(i in 1:nrow(scenario_key))
{
	summary_frame_final <- rbind(summary_frame_final, 
    eval(parse(text=as.character(scenario_key[i, "scenario.name"])))$summary)
}

write.table(summary_frame_final, "summary_all.csv", sep=",", row.names=F)
#summary_frame_final <- read.csv("summary_all.csv", head=T)



##---Put simulations together  added by ZZJ---##
simulations_frame_final <- NULL
for(i in 1:nrow(scenario_key))
{
  sim.res<-eval(parse(text=as.character(scenario_key[i, "scenario.name"])))$simulations
  sim.res$data <- as.character(scenario_key[i, "scenario.label"])
  simulations_frame_final <- rbind(simulations_frame_final, sim.res  )
}

write.table(simulations_frame_final, "simulations_all.csv", sep=",", row.names=F)
# simulations_frame_final <- read.csv("simulations_all.csv", head=T)
# names(simulations_frame_final)
# dim(simulations_frame_final)

# check
ind.l<-grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", endpoint_labels[1]))), names(summary_frame_final))
ind  <-grep(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", endpoint_labels[1]))), names(summary_frame_final))
names(summary_frame_final)[ind.l]
names(summary_frame_final)[ind]
str(names(summary_frame_final))
# end check

attributes(summary_frame_final)
namelist <- list(
  names(summary_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  endpoint_labels[1]))), names(summary_frame_final))], 

  names(summary_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  endpoint_labels[2]))), names(summary_frame_final))], 

  names(summary_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  endpoint_labels[3]))), names(summary_frame_final))], 

  names(summary_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  endpoint_labels[4]))), names(summary_frame_final))], 

  names(summary_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  endpoint_labels[5]))), names(summary_frame_final))],


  names(summary_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  endpoint_labels[6]))), names(summary_frame_final))], 

  names(summary_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  endpoint_labels[7]))), names(summary_frame_final))], 

  names(summary_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  endpoint_labels[8]))), names(summary_frame_final))], 

  names(summary_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  endpoint_labels[9]))), names(summary_frame_final))], 

  names(summary_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  endpoint_labels[10]))), names(summary_frame_final))],



  names(summary_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  endpoint_labels[11]))), names(summary_frame_final))], 

  names(summary_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  endpoint_labels[12]))), names(summary_frame_final))], 

  names(summary_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  endpoint_labels[13]))), names(summary_frame_final))], 

  names(summary_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  endpoint_labels[14]))), names(summary_frame_final))], 

  names(summary_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  endpoint_labels[15]))), names(summary_frame_final))],


  names(summary_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  endpoint_labels[16]))), names(summary_frame_final))], 

  names(summary_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  endpoint_labels[17]))), names(summary_frame_final))], 

  names(summary_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  endpoint_labels[18]))), names(summary_frame_final))], 

  names(summary_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  endpoint_labels[19]))), names(summary_frame_final))], 

  names(summary_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  endpoint_labels[20]))), names(summary_frame_final))]
)



summary_frame_final_alt <- reshape(
  data=summary_frame_final, 
  direction="long", 
  drop="scheme", 
  idvar=c("data", "sims"), 
  v.names=paste(endpoint_labels, "prop", sep="."), 
  varying=namelist, 
  times=unlist(strsplit(as.character(unique(summary_frame_final$scheme)), ",", fixed=T)), 
  timevar=c("scheme"))

row.names(summary_frame_final_alt) <- NULL



summary_frame_final_alt <- summary_frame_final_alt[order(summary_frame_final_alt$data), ]
write.table(summary_frame_final_alt, "summary_all_alt.csv", sep=",", row.names=F)
#summary_frame_final_alt <- read.csv("summary_all_alt.csv", head=T)





## by ZZJ
namelist.simu <- list(
  names(simulations_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  paste(endpoint_labels[1],"rej", sep=".")     ))), names(simulations_frame_final))], 

  names(simulations_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  paste(endpoint_labels[2],"rej", sep=".")     ))), names(simulations_frame_final))], 

  names(simulations_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  paste(endpoint_labels[3],"rej", sep=".")     ))), names(simulations_frame_final))], 

  names(simulations_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  paste(endpoint_labels[4],"rej", sep=".")     ))), names(simulations_frame_final))], 

  names(simulations_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  paste(endpoint_labels[5],"rej", sep=".")     ))), names(simulations_frame_final))]  )


# to get the name list for each of endpoints
# length(namelist.ep1.simu)

namelist.ep1.simu <- 
  names(simulations_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  paste(endpoint_labels[1],"rej", sep=".")     ))), names(simulations_frame_final))] 

namelist.ep2.simu <- 
  names(simulations_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  paste(endpoint_labels[2],"rej", sep=".")     ))), names(simulations_frame_final))] 

namelist.ep3.simu <- 
  names(simulations_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  paste(endpoint_labels[3],"rej", sep=".")     ))), names(simulations_frame_final))] 

namelist.ep4.simu <- 
  names(simulations_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  paste(endpoint_labels[4],"rej", sep=".")     ))), names(simulations_frame_final))] 

namelist.ep5.simu <- 
  names(simulations_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  paste(endpoint_labels[5],"rej", sep=".")     ))), names(simulations_frame_final))] 

namelist.ep6.simu <- 
  names(simulations_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  paste(endpoint_labels[6],"rej", sep=".")     ))), names(simulations_frame_final))] 

namelist.ep7.simu <- 
  names(simulations_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  paste(endpoint_labels[7],"rej", sep=".")     ))), names(simulations_frame_final))] 

namelist.ep8.simu <- 
  names(simulations_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  paste(endpoint_labels[8],"rej", sep=".")     ))), names(simulations_frame_final))] 

namelist.ep9.simu <- 
  names(simulations_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  paste(endpoint_labels[9],"rej", sep=".")     ))), names(simulations_frame_final))] 

namelist.ep10.simu <- 
  names(simulations_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  paste(endpoint_labels[10],"rej", sep=".")     ))), names(simulations_frame_final))] 





namelist.ep11.simu <- 
  names(simulations_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  paste(endpoint_labels[11],"rej", sep=".")     ))), names(simulations_frame_final))] 

namelist.ep12.simu <- 
  names(simulations_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  paste(endpoint_labels[12],"rej", sep=".")     ))), names(simulations_frame_final))] 

namelist.ep13.simu <- 
  names(simulations_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  paste(endpoint_labels[13],"rej", sep=".")     ))), names(simulations_frame_final))] 

namelist.ep14.simu <- 
  names(simulations_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  paste(endpoint_labels[14],"rej", sep=".")     ))), names(simulations_frame_final))] 

namelist.ep15.simu <- 
  names(simulations_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  paste(endpoint_labels[15],"rej", sep=".")     ))), names(simulations_frame_final))] 

namelist.ep16.simu <- 
  names(simulations_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  paste(endpoint_labels[16],"rej", sep=".")     ))), names(simulations_frame_final))] 

namelist.ep17.simu <- 
  names(simulations_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  paste(endpoint_labels[17],"rej", sep=".")     ))), names(simulations_frame_final))] 

namelist.ep18.simu <- 
  names(simulations_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  paste(endpoint_labels[18],"rej", sep=".")     ))), names(simulations_frame_final))] 

namelist.ep19.simu <- 
  names(simulations_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  paste(endpoint_labels[19],"rej", sep=".")     ))), names(simulations_frame_final))] 

namelist.ep20.simu <- 
  names(simulations_frame_final)[grepl(gsub("\\)", ".", gsub("\\(", ".", gsub(" ", ".", 
  paste(endpoint_labels[20],"rej", sep=".")     ))), names(simulations_frame_final))] 



grepl(  paste(endpoint_labels[2],"rej", sep=".") , names(simulations_frame_final))
# sum(grepl(  paste(endpoint_labels[2],"rej", sep=".") , names(simulations_frame_final))

# test
# gsub(pattern, replacement, x)
# grepl(pattern, x,...)
# grepl returns a logical vector (match or not for each element of x). 
# x<-"lzax_s1_w1_1_w2_1_w3_0.8.ADCS.rej     lzax_s1_w1_1_w2_1_w3_0.9.ADCS.rej      lzax_s1_w1_1_w2_1_w3_1.ADCS.rej"
# y<-gsub(".ADCS.rej","",x)
# grepl(".ADCS.rej","",x)
# grep(".ADCS.rej","",x)


library(reshape2)
# melt(data, id.vars, measure.vars,
#  variable.name = "variable", ..., na.rm = FALSE, value.name = "value",
#  factorsAsStrings = TRUE)
# Arguments
# 
# data          	data frame to melt 
# id.vars       	vector of id variables. Can be integer (variable position) or string (variable name). If blank, will use all non-measured variables.
# measure.vars  	vector of measured variables. Can be integer (variable position) or string (variable name)If blank, will use all non id.vars
# variable.name 	name of variable used to store measured variable names
# value.name    	name of variable used to store values
# na.rm             Should NA values be removed from the data set? This will convert explicit missings to implicit missings. 
# ...               further arguments passed to or from other methods.
# factorsAsStrings  Control whether factors are converted to character when melted as measure variables. When FALSE, coercion is forced if levels are not identical across the measure.vars.
 

head(simulations_frame_final)

 
############################

# for test  k<-2

# to derive all the simulations.long.ep1   -  simulations.long.ep20
for(k in 1:length(endpoint_labels) ) {


 tmp <- melt(simulations_frame_final,
          id.vars=c("data","simulation_id",  
          "pvalue_LY1_EP01", "pvalue_LY1_EP02", "pvalue_LY1_EP03", "pvalue_LY1_EP04", "pvalue_LY1_EP05", 
          "pvalue_LY1_EP06", "pvalue_LY1_EP07", "pvalue_LY1_EP08", "pvalue_LY1_EP09", "pvalue_LY1_EP10",                                    
          "pvalue_LY2_EP01", "pvalue_LY2_EP02", "pvalue_LY2_EP03", "pvalue_LY2_EP04", "pvalue_LY2_EP05",
          "pvalue_LY2_EP06", "pvalue_LY2_EP07", "pvalue_LY2_EP08", "pvalue_LY2_EP09", "pvalue_LY2_EP10"),

      measure.vars= eval(parse(text=paste("namelist.ep",k,".simu",sep=""))),
      variable.name= "Scheme_Weights",
      value.name = paste(endpoint_labels[k], "rej", sep=".") )
    
      tmp$Scheme_Weights <- gsub(paste(endpoint_labels[k], "rej", sep="."),"",tmp$Scheme_Weights)

      # to order them for easy merge later
      tmp<-tmp[order( tmp$data , tmp$simulation_id, tmp$Scheme_Weights ),]

      assign(paste("simulations.long.ep",k,sep=""),tmp)

       h<- head( eval(parse(text= paste("simulations.long.ep", k, sep="") ))   ) 
       print (h)
}



## another approach but tedious
#  endpoint 1
## simulations.long.ep1<-melt(simulations_frame_final,
## id.vars=c("data", "simulation_id", 
##           "pvalue_LY1_EP01", "pvalue_LY1_EP02", "pvalue_LY1_EP03", "pvalue_LY1_EP04", "pvalue_LY1_EP05", 
##           "pvalue_LY1_EP06", "pvalue_LY1_EP07", "pvalue_LY1_EP08", "pvalue_LY1_EP09", "pvalue_LY1_EP10",                                    
##           "pvalue_LY2_EP01", "pvalue_LY2_EP02", "pvalue_LY2_EP03", "pvalue_LY2_EP04", "pvalue_LY2_EP05",
##           "pvalue_LY2_EP06", "pvalue_LY2_EP07", "pvalue_LY2_EP08", "pvalue_LY2_EP09", "pvalue_LY2_EP10"),
## measure.vars= namelist.ep1.simu,
## variable.name= "Scheme_Weights",
## value.name = paste(endpoint_labels[1], "rej", sep="."))

## head(simulations.long.ep1)
## simulations.long.ep1$Scheme_Weights <- gsub(paste(endpoint_labels[1], "rej", sep="."),"",simulations.long.ep1$Scheme_Weights)

#endpoint 2
## simulations.long.ep2<-melt(simulations_frame_final,
## id.vars=c("data","simulation_id",  
##           "pvalue_LY1_EP01", "pvalue_LY1_EP02", "pvalue_LY1_EP03", "pvalue_LY1_EP04", "pvalue_LY1_EP05", 
##           "pvalue_LY1_EP06", "pvalue_LY1_EP07", "pvalue_LY1_EP08", "pvalue_LY1_EP09", "pvalue_LY1_EP10",                                    
##           "pvalue_LY2_EP01", "pvalue_LY2_EP02", "pvalue_LY2_EP03", "pvalue_LY2_EP04", "pvalue_LY2_EP05",
##           "pvalue_LY2_EP06", "pvalue_LY2_EP07", "pvalue_LY2_EP08", "pvalue_LY2_EP09", "pvalue_LY2_EP10"),
## measure.vars= namelist.ep2.simu,
## variable.name= "Scheme_Weights",
## value.name = paste(endpoint_labels[2], "rej", sep="."))

## head(simulations.long.ep2)
## simulations.long.ep2$Scheme_Weights <- gsub(paste(endpoint_labels[2], "rej", sep="."),"",simulations.long.ep2$Scheme_Weights)





# check data before merge 

for(k in 1:length(endpoint_labels) ) {
   # vnm <-paste("simulations.long.ep", k, sep="")
   # print (k)
   # print(vnm)
   h<- head( eval(parse(text= paste("simulations.long.ep", k, sep="") ))   ) 
   print (h)
}


dim(simulations.long.ep5)/(4*6*20)

# order the simu data and cbind them together. merge() does not work due to large data
# simulations.long.ep1<-simulations.long.ep1[order( simulations.long.ep1$data , simulations.long.ep1$Scheme_Weights ),]
# simulations.long.ep2<-simulations.long.ep2[order( simulations.long.ep2$data , simulations.long.ep2$Scheme_Weights ),]

# Error: cannot allocate vector of size 33.2 Mb  HPC computation is preferred later
# If five of them combined simutatious
names(simulations.long.ep1)

  simulations_frame_final_alt <-
   cbind(simulations.long.ep1,
         simulations.long.ep2[ "LY1_EP02.rej" ],
         simulations.long.ep3[ "LY1_EP03.rej"],
         simulations.long.ep4[ "LY1_EP04.rej" ],
         simulations.long.ep5[ "LY1_EP05.rej" ],
         simulations.long.ep6[ "LY1_EP06.rej" ],
         simulations.long.ep7[ "LY1_EP07.rej" ],
         simulations.long.ep8[ "LY1_EP08.rej"],
         simulations.long.ep9[ "LY1_EP09.rej" ],
         simulations.long.ep10["LY1_EP10.rej"],

         simulations.long.ep11[ "LY2_EP01.rej" ],
         simulations.long.ep12[ "LY2_EP02.rej" ],
         simulations.long.ep13[ "LY2_EP03.rej"],
         simulations.long.ep14[ "LY2_EP04.rej" ],
         simulations.long.ep15[ "LY2_EP05.rej" ],
         simulations.long.ep16[ "LY2_EP06.rej" ],
         simulations.long.ep17[ "LY2_EP07.rej" ],
         simulations.long.ep18[ "LY2_EP08.rej"],
         simulations.long.ep19[ "LY2_EP09.rej" ],
         simulations.long.ep20[ "LY2_EP10.rej"] 

) 

names(simulations_frame_final_alt)
head(simulations_frame_final_alt)
dim(simulations_frame_final_alt)
# merge() does not work !
# Error: cannot allocate vector of size 22.2 Mb
# simulations_frame_final_alt_merge <-
#   merge(simulations.long.ep1,simulations.long.ep2,
#         by = c("data", "Scheme_Weights", "pvalue_ADAS", "pvalue_ADCS", "pvalue_FAQ","pvalue_MMSE","pvalue_ADL") )


#  Note: scheme_name ( bace_s1_w1_0.9_w2_0.1_w3_0.1_w4_0_dose1_0.5 )
simulations_frame_final_alt$w0   <-as.numeric( gsub('_w1.*',"", gsub(".*w0_", "", simulations_frame_final_alt$Scheme_Weights) )  )
simulations_frame_final_alt$w1   <-as.numeric( gsub('_w2.*',"", gsub(".*w1_", "", simulations_frame_final_alt$Scheme_Weights ) )  )
simulations_frame_final_alt$w2   <-as.numeric( gsub('_w3.*',"", gsub(".*w2_", "", simulations_frame_final_alt$Scheme_Weights ) )  )
simulations_frame_final_alt$w3   <-as.numeric( gsub('_w4.*',"", gsub(".*w3_", "", simulations_frame_final_alt$Scheme_Weights ) )  )

simulations_frame_final_alt$w4   <-as.numeric( gsub('_w5.*',"", gsub(".*w4_", "", simulations_frame_final_alt$Scheme_Weights) )  )
simulations_frame_final_alt$w5   <-as.numeric( gsub('_dose1.*',"", gsub(".*w5_", "", simulations_frame_final_alt$Scheme_Weights) )  )
simulations_frame_final_alt$w_d1 <-as.numeric( gsub('.$',"",    gsub(".*dose1_", "", simulations_frame_final_alt$Scheme_Weights ) )  )

simulations_frame_final_alt<-
simulations_frame_final_alt[order(simulations_frame_final_alt$data ,
                                  simulations_frame_final_alt$Scheme_Weights,
                                  simulations_frame_final_alt$simulation_id,decreasing =FALSE ),]


head(simulations_frame_final_alt)
tail(simulations_frame_final_alt)
dim(simulations_frame_final_alt)
write.table(simulations_frame_final_alt, "simulations_all_alt.csv", sep=",", row.names=F)

######################Parameter check
n_scenario <- dim(scenario_key)[1]
n_scheme   <- dim(scheme_key)[1]

n_simulation <- dim(pvals_base)[1]  
n_endpoint   <- length(endpoint_labels)

n_obs_ck <-  n_scenario*n_simulation*n_scheme 

n_scenario
n_simulation
n_scheme
n_endpoint
n_obs_ck
####################




###########################################################################
# after post-prossing of data in sas
# runing gatekeeping_lzax_case1_R_Import.sas which derive data 
# summary_all_alt_post.csv and 
# simulations_all_alt_post.csv
###########################################################################

post.process.ck <- function( ) {
summary_all_alt_post <- read.csv("summary_all_alt_post.csv", head=T)
simulations_all_alt_post <- read.csv("simulations_all_alt_post.csv", head=T)

head(simulations_all_alt_post)
head(summary_all_alt_post)
}

# post.process.ck()










