#Bonferroni correction is defined as alpha divided by n (where n is number of tests)

#The code below defines a complete population of 1M data points. The for loop
#creates a control group and two treatment options (treatment A and treatment B) 
#that are created using a fraction of the original population sample size.
#In theory, there is no difference between the control and treatments, but 
#with an alpha of 0.05, we should see a family-wise type one error rate of
#less than or equal to 0.0975 (the formula is 1-(1-alpha)^n).


#NO BONFERRONI CORRECTION
set.seed(543)
population_original<-rnorm(1000000,mean=0,sd=1)

output<-data.frame(NULL)
set.seed(024)
sample_size<-50
for (i in 1:10000){
    control_group<-sample(population_original,size=sample_size,replace=FALSE)
    
    treatment_a<-sample(population_original,size=sample_size,replace=FALSE)
    treatment_b<-sample(population_original,size=sample_size,replace=FALSE)
    
    test_a<-t.test(control_group,treatment_a,alternative = "less",
                   paired=FALSE,conf.level = 0.95)
    
    test_b<-t.test(control_group,treatment_b,alternative = "less",
                   paired=FALSE,conf.level = 0.95)
    
    output<-rbind(output,data.frame(iteration=i,
                                    group_a_pvalue=test_a$p.value,
                                    group_b_pvalue=test_b$p.value))
    
    
}

head(output)

#number of positives pvalue in treatment A
nrow(output%>%filter(group_a_pvalue<=0.05))/nrow(output)

#number of positive pvalues in treatment B
nrow(output%>%filter(group_b_pvalue<=0.05))/nrow(output)

#number of positive pvalues in treatment A or B
nrow(output%>%filter(group_a_pvalue<=0.05|group_b_pvalue<=0.05))

#Family wise type one error:
nrow(output%>%filter(group_a_pvalue<=0.05|group_b_pvalue<=0.05)) / nrow(output)
#Output of this gives 8.84% type 1 error (which is in line with our calculations)
#To get our family wise error down, apply the Bonferroni correction

#######
#BONFERRONI CORRECTION
#######

#Change alpha to 0.025 (or 0.05/2)
output_bonferroni<-data.frame(NULL)
set.seed(024)
sample_size<-50
for (i in 1:10000){
    control_group<-sample(population_original,size=sample_size,replace=FALSE)
    
    treatment_a<-sample(population_original,size=sample_size,replace=FALSE)
    treatment_b<-sample(population_original,size=sample_size,replace=FALSE)
    
    test_a<-t.test(control_group,treatment_a,alternative = "less",
                   paired=FALSE,conf.level = 0.975)
    
    test_b<-t.test(control_group,treatment_b,alternative = "less",
                   paired=FALSE,conf.level = 0.975)
    
    output_bonferroni<-rbind(output_bonferroni,data.frame(iteration=i,
                                                          group_a_pvalue=test_a$p.value,
                                                          group_b_pvalue=test_b$p.value))
    
    
}

head(output_bonferroni)

#number of positives pvalue in treatment A
nrow(output_bonferroni%>%filter(group_a_pvalue<=0.025))/nrow(output_bonferroni)

#number of positive pvalues in treatment B
nrow(output_bonferroni%>%filter(group_b_pvalue<=0.025))/nrow(output_bonferroni)

#number of positive pvalues in treatment A or B
nrow(output_bonferroni%>%filter(group_a_pvalue<=0.025|group_b_pvalue<=0.025))

#Family wise type one error:
nrow(output_bonferroni%>%filter(group_a_pvalue<=0.025|group_b_pvalue<=0.025)) / nrow(output_bonferroni)

#Output here is 4.68% type 1 error, which is back in line with what we expected. 
#Bonferroni worked

