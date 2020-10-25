#              WORKSHOP 1- PROJECT ORGANISATION, TOOLS FOR VERSION         #
#                          CONTROL AND COLLABORATING                       #
############################################################################

library(tidyverse)
library(ggplot2)


#TASK1). CREATING A NEW PROJECT WITHOUT COLLABORATION USING THE 'GITHUB
#        FIRST APPROACH.


#LET'S ORGANISE THE DATAFRAME INTO "TIDY FORMAT"
chaff2 <- gather(chaff, "sex", "mass", 1:2)


#CREATE A FILE HOLDING CHAFF2 (DATAFRAME NOW IN TIDY FORMAT)
file <-  "chaff2.txt"

#PUT FILE INTO A DATAFRAME...
write.table(chaff2, 
            file, 
            quote = FALSE,
            row.names = FALSE)


#NOW LET'S SUMMARISE AND ANALYSE THE DATA...

#QUESTION OF INTEREST: "IS THERE A DIFFERENCE IN CHAFFINCH MASS THAT CAN
#                       BE EXPLAINED BY CHAFFINCH SEX (MALE OR FEMALE)?


#VISUAL OBSERVATIONS OF CHAFF2#

#1). THE 'MASS'RESPONSE VARIABLE IS SEEMINGLY CONTINUOUS AND SEEMINGLY
#DEVOID OF REPEAT VALUES...

#2). THE EXPLANATORY VARIABLE IS 'SEX' OF WHICH THERE ARE 2X LEVELS
#MALE AND FEMALE. RESPONSE VARIABLE IS NUMERICAL AND IS THE MASS OF THE
#FINCHES.

#LET'S GENERATE SOME SUMMARY STATISTICS (MEAN, SD, MEDIAN, SAMPLE SIZE,
#AND SE)


#CREATING A DATAFRAME HOLDING SUMMARY STATISTICS
chaff2.sum <- chaff2 %>%
  group_by(sex) %>%
  summarise(mean = mean(mass),
            sd = sd(mass),
            median = median(mass),
            n = length(mass),
            se = sd/sqrt(n))
  

#WHAT STATISTICAL TEST TO USE?

#A TWO-SAMPLE T-TEST IS LIKELY THE BEST TEST TO APPLY HERE WITH THE 
#ASSUMPTIONS BEING THAT THE RESIDUALS ARE NORMALLY DISTRIBUTED AND
#THAT THERE IS HOMOGENEITY OF RESIDUAL VARIANCE.

#LET'S RUN A 2-SAMPLE T-TEST...
t.test(data = chaff2, mass ~ sex, var.equal = T)

#Two Sample t-test
#data:  mass by sex
#t = -2.6471, df = 38, p-value = 0.01175
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#-3.167734 -0.422266

#TWO-SAMPLE T-TEST CONCLUSION
#A SIGNIFICANT DIFFERENCE WAS DETECTED IN MASS DIFFERENCES BETWEEN 
#MALE AND FEMALE CHAFFINCHES (t = -2.6471, df = 38, p-value = 0.01175).



#LET'S MERGE THE CHAFF2 DATAFRAME WITH THE CHAFF2SUM DATAFRAME BY THEIR 
#COMMONALITY WHICH IS "SEX"...
chaff2 <- merge(chaff2, chaff2.sum[,1:2], by = "sex")


#LET'S ADD A COLUMN INTO THE CHAFF2 DATAFRAME HOLDING ALL THE RESIDUALS 
chaff2 <- chaff2 %>%
  mutate(residual = mass - mean)


#NORMALITY CHECKS

#1). ARE THE RESIDUALS NORMALLY DISTRIBUTED?
shapiro.test(chaff2$residual)

#Shapiro-Wilk normality test
#data:  chaff2$residual
#W = 0.98046, p-value = 0.7067
#YES THEY ARE! :)

#2). DO THE RESIDUALS DEMONSTRATE HOMOGENEITY OF VARIANCE FROM THEIR
#RESPECTIVE GROUP MEANS?
ggplot(data = chaff2, aes(x = mean, y = residual))+geom_point()
#THE RESIDUALS ARE NEAR HOMOGENOUS IN THEIR SPREAD FROM THE
#RESPECTIVE GROUP MEANS :)

#MAKING A GGPLOT OF THE DATA...
fig1 <- ggplot()+
  geom_point(data = chaff2, aes(x = sex, y = mass),
             position = position_jitter(0.1),
             colour = "black")+
  geom_errorbar(data = chaff2.sum, aes(x = sex, ymin = mean - se,
                ymax = mean + se), width = 0.3)+
  geom_errorbar(data = chaff2.sum, aes(x = sex, ymin = mean, 
                ymax = mean), width = 0.2)
                  
  
#FIGURE SAVING SETTINGS
units <- "in"  
fig_w <- 3.5
fig_h <- fig_w
dpi <- 300
device <- "tiff" 


#LET'S SAVE FIG1 AS A FILE...
ggsave("fig1.tiff",
       plot = fig1,
       device = device,
       width = fig_w,
       height = fig_h,
       units = units,
       dpi = dpi)


