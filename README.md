# Pitch Study

In a pitch study, we are interested in the relationship between **pitch** and **politeness**.   

There are two levels politeness (a formal register: pol, and an informal register: inf). On top of that, we also have an additional fixed effect, gender. Each subject was tested on several scenarios (e.g., asking a peer for a favor (informal condition) or asking a professor for a favor (formal condition)). The pitch measurements are typically **correlated** for the same subject and in the same scenario.  

(a) Exploratory analysis: provide boxplots to show the relation between gen- der/attitude and pitch (ignoring different scenarios).  
(b) Fit a mixed effects model with random intercepts for different subjects (gender and attitude being the fixed effects). What is the covariance matrix for a subject Yi? What is the covariance matrix for the estimates of fixed effects (Hint: 3 × 3 matrix for intercept, gender and attitude)? What are the BLUPs for subject-specific intercepts? What are the residuals?  
(c) Fit a mixed effects model with intercepts for different subjects (gender, attitude and their interaction being the fixed effects). Use likelihood ratio test to compare this model with the model in part (b) to determine whether the interaction term is significantly associated with pitch.   
(d) Write out the mixed effects model with random intercepts for both subjects and scenarios (gender and attitude being the fixed effects). Fit the model using lmer in the lme4 package. Write out the covariance matrix for a subject Yi. What is the interpretation of the coefficient for the fixed effect term attitude?
