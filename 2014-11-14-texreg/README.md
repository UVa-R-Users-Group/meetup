Michele Claibourn's R script for her demo of **texreg**. texreg is an R package that converts R modelling output to LaTeX or HTML tables. 

In the first example we use college admissions data to model gpa as a function of class rank, act score and year of admission. Three linear models are fit and summarized in a single table using the texreg functions `screenreg`, `texreg` and `htmlreg`. These functions create nicely formatted tables for the R console, LaTeX, and HTML, respectively. The second example uses NRC Data on research-doctorate programs to do beta regressions of PhD completion rates on Faculty Publications, Citation Rate, Faculty Grants and Institution type. This example serves to show how texreg works for other modelling functions in addition to `lm`.

After this demonstration, hopefully Meetup participants will know more about using the texreg package to assist in preparing modelling output for publication or presentation.

