## Publication-quality tables of model output from R
## texreg: Conversion of R regression output to LaTeX or HTML tables

# install.packages("texreg")
library(texreg)

# Load some data
admissions <- read.csv("http://people.virginia.edu/~mpc8t/admissions.csv", header=T)

str(admissions)
summary(admissions)
admissions$year <- as.factor(admissions$year)

# Generate some models
gpa_all <- lm(gpa ~ rank + act + year, data=admissions)
gpa_red <- lm(gpa ~ rank + act, data=admissions)
gpa_int <- lm(gpa ~ rank*act, data=admissions)

# Make a table
screenreg(list(gpa_all, gpa_red, gpa_int))

# Make it nicer
screenreg(list(gpa_all, gpa_red, gpa_int), 
          omit.coef="year",
          custom.model.names=c("With Year Effects", "No Year Effects", "Interaction"),
          custom.coef.names=c("Intercept", "HS Rank", "ACT Score", NA, NA, NA, NA, "Rank * ACT"),
          include.rsquared=FALSE, custom.gof.names=c("adjR^2", "N"),
          stars=c(0.01, 0.05), custom.note="%stars. Year effects not significant.")

# Output it as LaTeX table
texreg(list(gpa_all, gpa_red, gpa_int))

texreg(list(gpa_all, gpa_red, gpa_int), 
          omit.coef="year",
          custom.model.names=c("With Year Effects", "No Year Effects", "Interaction"),
          custom.coef.names=c("Intercept", "HS Rank", "ACT Score", NA, NA, NA, NA, "Rank * ACT"),
          include.rsquared=FALSE, custom.gof.names=c("adj$R^2$", "N"),
          stars=c(0.01, 0.05), custom.note="%stars. Year effects not significant.",
          caption="First-Year GPA by HS Rank and ACT Score")

# Output as html file (can be read into Word, or used in markdown)
htmlreg(list(gpa_all, gpa_red, gpa_int))

htmlreg(list(gpa_all, gpa_red, gpa_int), 
       file="gpaTable.doc", omit.coef="year",
       custom.model.names=c("With Year Effects", "No Year Effects", "Interaction"),
       custom.coef.names=c("Intercept", "HS Rank", "ACT Score", NA, NA, NA, NA, "Rank * ACT"),
       include.rsquared=FALSE, custom.gof.names=c("adjR^2", "N"),      
       stars=c(0.01, 0.05), custom.note="%stars. Year effects not significant.", 
       caption="First-Year GPA by HS Rank and ACT Score", caption.above=TRUE)

# Coefficient plot
plotreg(list(gpa_all, gpa_red, gpa_int))
plotreg(list(gpa_red, gpa_int), omit.coef="Inter")
                
plotreg(gpa_int, omit.coef="Inter",,
        custom.coef.names=c("Intercept", "HS Rank", "ACT Score", "Rank * ACT"),
        main="Coefficient Plot: First-Year GPA")

# coefplot in package arm is better
library(arm)
par(mfrow = c(1, 1))
coefplot(gpa_int)
coefplot(gpa_int,xlim=c(-.1,.05), frame.plot=T) # make axis cross zero/null
coefplot(gpa_red, add=T, col="gray")
# A new package, "coefplot" builds on ggplot2

# Highly customizable and supports wide set of models
?extract

# Other useful packages include:
#   xtable: particularly good for tables of summary statistics
#   stargazer: summary tables and model output, may be catching up


#################

## One more example: Ph.D. completion rates
# Load the data: NRC Data on Research-Doctorate Programs
# http://www.nap.edu/rdp/index.html??record_id=12850
nrc <- read.csv("http://people.virginia.edu/~mpc8t/nrcGradAssess2010.csv", header=T, 
                na.strings=c("", "N/D", "NA", "*", "N/R"),
                as.is=c("institution"))

levels(nrc$fieldBroad)

# Model completion rate for each field
model.sbe <- lm(compRate ~ pubFac + citePub + facGrant + fySupport + instType, 
                data=subset(nrc, subset= fieldBroad=="Social and Behavioral Sciences"))
model.pms <- lm(compRate ~ pubFac + citePub + facGrant + fySupport + instType, 
                data=subset(nrc, subset= fieldBroad=="Physical and Mathematical Sciences"))
# citePub is entirely missing for humanities
model.hum <- lm(compRate ~ pubFac + facGrant + fySupport + instType, 
                data=subset(nrc, subset= fieldBroad=="Humanities"))
model.engr <- lm(compRate ~ pubFac + citePub + facGrant + fySupport + instType, 
                 data=subset(nrc, subset= fieldBroad=="Engineering"))
model.bhs <- lm(compRate ~ pubFac + citePub + facGrant + fySupport + instType, 
                data=subset(nrc, subset= fieldBroad=="Biological and Health Sciences"))
model.ags <- lm(compRate ~ pubFac + citePub + facGrant + fySupport + instType, 
                data=subset(nrc, subset= fieldBroad=="Agricultural Sciences"))

# Make a table
screenreg(list(model.sbe, model.pms, model.hum, model.engr, model.bhs, model.ags), 
          custom.model.names=c("Social", "Physical", "Humanities", "Engineering", 
                               "Bio/Health", "Agricultural"),               
          custom.coef.names=c("Intercept", "Faculty Publications", "Citation Rate", 
                              "Faculty Grants", "First Year Support", "Public University"),                               
          include.rsquared=F)

# Beta regression (for proportions data, if you prefer)
library(betareg)
nrc$compRate2 <- ifelse(nrc$compRate==0,0.01,nrc$compRate)
nrc$compRate2 <- ifelse(nrc$compRate==1,0.99,nrc$compRate2)
beta.sbe <- betareg(compRate2 ~ pubFac + citePub + facGrant + fySupport + instType, 
                    data=subset(nrc, subset= fieldBroad=="Social and Behavioral Sciences"))
beta.pms <- betareg(compRate2 ~ pubFac + citePub + facGrant + fySupport + instType, 
                    data=subset(nrc, subset= fieldBroad=="Physical and Mathematical Sciences"))
# citePub is entirely missing for humanities
beta.hum <- betareg(compRate2 ~ pubFac + facGrant + fySupport + instType, 
                    data=subset(nrc, subset= fieldBroad=="Humanities"))
beta.engr <- betareg(compRate2 ~ pubFac + citePub + facGrant + fySupport + instType, 
                     data=subset(nrc, subset= fieldBroad=="Engineering"))
beta.bhs <- betareg(compRate2 ~ pubFac + citePub + facGrant + fySupport + instType, 
                    data=subset(nrc, subset= fieldBroad=="Biological and Health Sciences"))
beta.ags <- betareg(compRate2 ~ pubFac + citePub + facGrant + fySupport + instType, 
                    data=subset(nrc, subset= fieldBroad=="Agricultural Sciences"))

screenreg(list(beta.sbe, beta.pms, beta.hum, beta.engr, beta.bhs, beta.ags), 
          custom.model.names=c("Social", "Physical", "Humanities", "Engineering", "Bio/Health", "Agricultural"),               
          custom.coef.names=c("Intercept", "Faculty Publications", 
                              "Citation Rate", "Faculty Grants", 
                              "First Year Support", "Public University", "Precision"))

