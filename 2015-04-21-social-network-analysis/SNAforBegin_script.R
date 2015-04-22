 
# ==============================================
# Social Network Analysis for Beginners Workshop
# Spring 2015
# Yun Tai 
# ytai@virginia.edu
# ==============================================

# --------------------
# Some basic R syntex 
# --------------------

1 + 2
a <- 2  # assign a as 2
a       # what is a?

b <- 1*3 
b
a == b  # is a equal to b?
a != b  # is a not equal to b?

ls()    # list objects in the R environment

rm(a)   # remove a single object
rm(list=ls())  # remove everything in the environment


# Vectors and matrices ---------------------------------------

# replication and sequence
rep(1,times=7)      # repeat one for 7 times
rep(1:5,times=2)    # repeat sequence one to five twice
rep(1:5,each=2)     # repeat each of the elements twice
rep(1:5,times=5:1)  # repeat one to five, from 5 times to 1 time

x1 <- 1:5  # create a vector
x1 

x2 <- c(1,3,5,7,9)  # create a vector using the c() function
x2 

y <- c(x1,x2)  # combine vectors 
y

# accessing elements of a vector with bracket notation
y[2]              # what's the second element in y?
y[1:3]            # first three
y[-(1:3)]         # all but first three
y[c(1,3,6)]       # first, third and sixth
y[y > 2]          # y such that y is greater than 2
y[y > 2 & y < 6]  # y such that y is greater than 2 and less than 6

y
length(y)   # how long is the vector?
y>2         # are the elements greater than 2?
any(y>2)		# are any elements greater than 2?
all(y>2)		# are all elements greater than 2?
which(y>2)	# which elements are greater than 2?

# create a 5 by 5 matrix, with 1 to 25 as the elements
m <- matrix(data=1:25, nrow=5, ncol=5) 
m
m[1,3]  	  # select an element (row 1, column 3)
m[1,]		    # the first row
m[,2]		    # the second column
m[1:2,3:5]	# select submatrices (row 1 to 2, column 3 to 5)
m[-1,]		  # all but the first row
m[-2,-3]		# get rid of row 2 and column 3

# create a 5 by 2 matrix using the cbind() (column bind) function
m2 <- cbind(1:5,1:5)  
m2

# create a 2 by 5 matrix using the rbind() (row bind) function
m3 <- rbind(1:5,1:5) 
m3

nrow(m3) # how many rows
ncol(m3) # how many colums
dim(m3)  # dimentions

# combind matrices: make sure you have compatible dimensions
m3
m3c <- cbind(m3,m3)  # column bind two matrices 
m3c
m3r <- rbind(m3,m3)  # row bind two matrices 
m3r


# Data Frame --------------------------------------------------

# Create a data frame with 4 columns (variables)
dat <- data.frame(id=LETTERS[1:5],age=20:24,sex=c("F","M","M","F","M"),smart=c(T,T,T,F,F))
dat
dat$age   # extract a variable
dat[1,2]  # find out age of the first observation
dat[1,3]  # note that factors for string variables are provided
str(dat)  # display the structure of an R object
# if you don't want to make strings as factors
datII <- data.frame(id=LETTERS[1:5], age=20:24, sex=c("F","M","M","F","M"), 
                   smart=c(T,T,T,F,F), stringsAsFactors=FALSE)
datII[1,3]
str(datII)

# change values
dat$sex[3]
dat$sex[3] <- "F"
dat$sex[3]

# build a data frame 
dat2 <- as.data.frame(cbind(3:7,1:5))
dat2
is.matrix(dat2)  # is it a matrix?
is.data.frame(dat2) 

rm(list=ls())


# ---------------------------------
# work with network/relational data 
# ---------------------------------

# Read in your data ------------------------------------------------

setwd('C:/Users/yt3f/Desktop/SNAforBegin/data')   # set your working directory 
setwd("~/_workshops/SNAforBegin/SNAforBegin//data")
list.files()  # list files in the working directory

# We will use "gotRel.csv" here. Let's first take a look of the original file.
# weighted network

# Import an adjacency matrix using read.csv() function. R stores it as a data frame.
gotdf <- read.csv(file='gotRel.csv',header=TRUE,row.names=1,stringsAsFactors=FALSE)
gotdf     # header=T and row.names=1 help to make it as a 10 by 10 matrix
gotdfI <- read.csv(file='gotRel.csv',header=TRUE,stringsAsFactors=FALSE)
gotdfI    # row.names not assigned as 1, a column x is generated, makes it a 10 by 11 matrix
gotdfII <- read.csv(file='gotRel.csv',header=FALSE,row.names=1,stringsAsFactors=FALSE)
gotdfII   # header=F, a row of column names v2 to v11 are generated, makes it a 11 by 10 matrix 
rm(gotdfI, gotdfII)

is.matrix(gotdf)     # is it a matrix?

# need to make a matrix object
gotmat <- as.matrix(gotdf)  # convert it to a matrix form
is.matrix(gotmat)
isSymmetric(gotmat)  # is it a symmetric matrix?


# The "Network" package: "network" objects -------------------------------------------

# A network object is a R object that can store both relational data and its metadata. 
# Here we use the "network" package to construct, describe, manipulate, and plot network objects.    

install.packages('network')  # install the package 
library(network)              # load the package

# Using as.network.matrix() to convert the matrix (gotmat) into a network object, 
# which is undirected and carries edge values named "weight"
gotmat
gotnet <- as.network.matrix(gotmat,directed=FALSE,ignore.eval=FALSE,names.eval="weight")
# ignore.eval=FALSE mean treat as weighted

gotnet                     # quick description of the network 
# vertices = nodes
summary(gotnet)            # more info of the network  			
network.edgecount(gotnet)  # how many edges in the network	
network.size(gotnet)			 # how many nodes in the network
					
plot(gotnet)  # a quick plot of the network
plot(gotnet,displaylabels=TRUE)  # put on node names 

# In case you like to add or delete edges:
network.vertex.names(gotnet)     # list the nodes
gotnet[2,3]       # are Cersei and Ned connected? 

gotnet["Arya","Ned"]
gotnet[2:3,4:7]  	# subset of row 2 to 3. column 4 to 7
gotnet[-10,-10]		# if you want to kill Sansa (from the graph, of course...)
    	
gotnet[2,]   			# all of Cersei's ties, i.e., extract the Cersei row
                  # note that this doesn't display edge values
gotmat[2,]
as.sociomatrix(gotnet,attrname="weight")  # retrieving edge values in a matrix form
as.sociomatrix(gotnet)  # the default gives you a dichotomous one
gotnet[,]  # the other way to do it

# plot the network with presenting edge values by line width
plot(gotnet,displaylabels=TRUE,edge.lwd="weight")


# Network and node attributes --------------------------------------------

# Let's take a look of the node attributes file 

# import nodes attributes data
gotatt <- read.csv(file='gotAttri.csv',header=TRUE,stringsAsFactors=FALSE)
gotatt

# load the node atrributes into the network object, i.e., gotnet
# create vertext attribute names
gotnet %v% "id" <- gotatt$ID
gotnet %v% "name" <- gotatt$NAME
gotnet %v% "sex" <- gotatt$SEX
gotnet %v% "family" <- gotatt$FAMILY
gotnet %v% "status" <- gotatt$STATUS  
gotnet %v% "appear" <- gotatt$APPEAR

gotnet
list.vertex.attributes(gotnet)  # list node atrribute names
gotnet %v% "id"    # retrieve the "id" attribute
gotnet %v% "name"  # retrieve the "name" attribute


# ---------------------------
# Some Basic Network Analysis 
# ---------------------------

# use a build-in dataset
data(package="network")  # datasets come with the network package
?flo
data(flo)		# load the "flo" dataset
flo         # take a look of it
is.matrix(flo)

install.packages('sna')
library(sna)  # load the "sna" library

# The "sna" package can handle many forms of network data, e.g., network object, adjacency matix, 
# or a list of adjacency matrices. In other words, functions of this package can be used on 
# various forms of data. We'll work with edgelist in the following example.

floel <- as.edgelist.sna(flo)  # convert the flo matrix into a sna edgelist 
head(floel)
# [40,]  13  16   1; node 13 connected to node 16

attr(floel,"n")                # how many nodes?
attr(floel,"vnames")           # node names
as.sociomatrix.sna(floel)      # convert it to a sna matrix

# sna edgelists are three-column matrices. How edge lists are made:
mat <- cbind(rep(1,5),2:6,8:12)  # 3 columns: 5 ones, two to six, 5 eights  
mat
colnames(mat) <-c ("sen","rec","val")  # assign column names
mat
attr(mat,"n") <- 6  # set total number of nodes=6
mat  # an edgelist form

# sen rec val
# [1,]   1   2   8
# nodes 1 and 2 interact 8 times

as.sociomatrix.sna(mat)  # a matrix form

attr(mat,"n") <- 7  # in the case that there are 7 nodes, 
                    # but one of them is not connected at all
mat
as.sociomatrix.sna(mat)


# Measure centrality ---------------------------------------

data(emon)  # load the "emon" dataset
?emon       # it contains 7 network objects
emon.tx <- emon$Texas  # we'll only use the Texas one here
emon.tx     # a directed network

# degree centrality
degree(emon.tx)  # total degree of each node
degree(flo)      # works for a matrix object as well
indg <- degree(emon.tx, cmode="indegree")  # in-degree
indg
oudg <- degree(emon.tx, cmode="outdegree") # out-degree
oudg

# plot in-degree by out-degree
plot(x=indg,y=oudg,xlab="Indegree",ylab="Outdegree")

# Which GOT character is more central than others, in terms of degree centrality?
gotindg <- degree(gotnet)
max(gotindg)  # the max value in gotindg
which(gotindg==12)  # returns the position of the value=12
network.vertex.names(gotnet)

# betweeness and closeness centrality
bet <- betweenness(emon.tx)
bet  # betweenness centrlity of each node
clo <- closeness(emon.tx)
clo  # closeness centrlity of each node

# plot the relationship of closeness and betweenness
plot(x=clo, y=bet, xlab="Closeness", ylab="Betweenness")

# what about betweenness and closeness in our GOT network
gotbet <- betweenness(gotnet,gmode="graph")  # "digraph" by default
gotclo <- closeness(gotnet,gmode="graph")
max(gotbet)
which(gotbet==18)
max(gotclo)
which(gotclo==0.75)


# --------------------
# Network Visulization 
# --------------------

plot(gotnet)  # default plot 
plot(gotnet, displaylabels=TRUE)  # add labels
plot(gotnet, displaylabels=TRUE, 
             vertex.cex=4,        # expansion factor=>node size
             vertex.border="red", # node border color
             label.cex=0.8,       # label size 
             label.col="white",   # label color
             label.pos=5)         # label position, here we place labels at the vertex positions

# Of course we want to color the nodes by gender stereotype...
ncolor <- ifelse(gotatt$SEX=="F",yes="lightpink",no="lightblue") 
ncolor  # a list of color names assigned to each nodes
plot(gotnet,displaylabels=T,vertex.cex=3,vertex.col=ncolor,label.pos=5)

# or the "status" of them
ncolor2 <- ifelse(gotatt$STATUS=="alive",yes="green",no="antiquewhite3")
plot(gotnet,displaylabels=T,vertex.cex=3,vertex.col=ncolor2,label.pos=5)

# more colors
(pal = palette())  # the default color palette in R
colors()           # named colors in R

# a directed network
plot(emon.tx)  
par(mar=c(0,0,0,0))  # get rid of the large margin; the numerical vector indicating 
                     # margin size c(bottom, left, top, right) in lines.                      
plot(emon.tx)
plot(emon.tx, displaylabels=TRUE)

# try a different layout
plot(emon.tx, displaylabels=TRUE,
              vertex.cex=2,
              label.cex=0.7, 
              label.col="midnightblue", 
              label.pos=5,
              mode="circle")  # vertex placement algorithm
?gplot.layout  # more info of layouts

# interactive mode for tweaking
plot(emon.tx, displaylabels=TRUE,
              vertex.cex=2,
              label.cex=0.7, 
              label.col="midnightblue", 
              label.pos=5,
              mode="circle",
              interactive=TRUE)  # interactive mode

# save a layout for reuse
Coords1 <- plot(emon.tx, displaylabels=TRUE,
                         vertex.cex=2,
                         label.cex=0.7, 
                         label.col="midnightblue", 
                         label.pos=5,
                         mode="circle",
                         interactive=TRUE)
head(Coords1)  # the nodes coordinates 
plot(emon.tx, coord=Coords1)  # apply a saved layout
plot(emon.tx)

# more info of graph plotting
?gplot






detach("package:network")
detach("package:sna")

rm(list=ls())

# ------------------------------------------------
# Transform column-based data into relational data 
# ------------------------------------------------

# In this section, I will walk you through how *I* transform a column-based dataset into 
# a relational one. You may be able to come up with a alternate/better way to do so - if 
# so, please let me know! You won't need to do the following if your dataset comes in handy, 
# i.e., already in shapes of adjacency matrix or edgelist. However, this transformation 
# may help you finding/presenting relations in a datasets that don't seem to be a network 
# type of data.
# WARNING: you may want to try the following steps on a small data first.

# Let's take a look of the original data.

library(reshape2)
library(plyr)
df <- read.csv(file='gotAttri.csv', stringsAsFactors=FALSE)
names(df)
dfw <- df[,-(2:6)]  # drop variables we don't need here  
dfw

# What we want to do is to present the relationship between characters, connecting by appearing
# in the same episodes, and create an edgelist of this episode-connecting network. We begin by
# folding app1 to app9 into one single variable, which represents who appears in which episodes.

# Get character-episode dyads:
# we do this by reshaping a wide format into a long format, using reshape2::melt() function.
# The basic syntax is melt(data, id.vars, measure.vars), where "data" is your data frame, 
# "id.vars" are the ID variables (ie, variables that will still have their own column after 
# reshaping) and "measure.vars" are the variables that are getting "melted". Column headers 
# of the "measure.vars" become a single variable in the melted data frame as does the values 
# under those column headers.

dfl <- melt(dfw, id.vars="ID", 
            measure.vars=c("app1","app2","app3","app4","app5", "app6","app7","app8","app9"), 
            variable.name="app", value.name="eid")
head(dfl,12)
tail(dfl)

dfl <- dfl[,-2]            # drop app
colnames(dfl)[1] <- "cid"  # rename ID=cid
dfl <- na.omit(dfl)        # drop NAs, i.e., characters that don't appear
rm(dfw)

# Check if these are unique character-episode dyads: in this case, we don't have duplicate
# cid-eid dyads, because one character's appearance in one episode can only be counted once.
# However, in some cases, you may get the same dyads more than once. It's your choice to 
# decide if these duplicate dyads are meaningful for your research. We do this by counting 
# eid within each group of cid. 

dfl <- dfl[order(dfl$cid, dfl$eid),]   # sort by cid then eid
head(dfl, 15)

# Then we use plyr::ddply to generate a variable "flag" as the counts of eid within each cid group.
# first argument: input data frame
# second argument: grouping variable to split data frame by
# third argument: function to apply to each group
# fourth argument: argument passed to third function

dfl2 <- ddply(dfl, .(cid,eid), mutate, flag=1:length(eid))
# .(cid,eid)=> group by cid eid
# The "." function here allows you specify the grouping variable without quotes.
# mutate=> add new columns that are functions of existing columns

rm(dfl2)

# Get character-character dyads, bridging by episodes, i.e.,finding all pairwise combinations 
# of characters that both appeared in each and every one episode.
# We use merge() function to match characters with shared episodes.

# merge() combines two files, so we start by copying dfl into two files, temp1 and temp2 
temp1 <- dfl
temp2 <- dfl
colnames(temp1)[1] <- "cid1"   # in temp1, rename cid=cid1 
colnames(temp2)[1] <- "cid2"   # in temp2, rename cid=cid2
head(temp1)
head(temp2)

# See how merge() works when values in the common column are not unique  
# Consider this simplified example:
c1 <- c(1,2,4)
c2 <- c(2,2,1)
dat1 <- as.data.frame(cbind(c1,c2))  
colnames(dat1) <- c("cid1","eid")
dat2 <- as.data.frame(cbind(c2,c1))  
colnames(dat2) <- c("eid","cid2")
dat1  # think this data as our temp1 file
dat2  # think this data as our temp2 file

dat <- merge(dat1, dat2)
dat   # returns all possible pairs connected by eid1=1 and eid1=2

rm(c1,c2,dat1,dat2,dat)

# Back to our GOT data
# Get character-character edgelist (undirected)
dfc <- merge(temp1,temp2)   # merge temp1 and temp2 by eid
head(dfc)
dfc <- subset(dfc, subset= (cid1!=cid2))   # drop self-loops, i.e., if cid1=cid2

# Since this is considered as an undirected network, dyads are reciprocated, e.g., pair 
# (2,1) is considered to be a duplicate pair of (1,2). We need to identify them.

dfc <- dfc[order(dfc$cid1, dfc$cid2),] # sort by cid1 then cid2
dfc$t1 <- paste(dfc$cid1, dfc$cid2)    # concat joint pairs in order1 (cid1, cid2)
dfc$t2 <- paste(dfc$cid2, dfc$cid1)    # concat joint pairs in order2 (cid2, cid1)
dfc[92,]
dfc[129,]
ind <- dfc$cid1>dfc$cid2               # make an index of obs. such that cid1>cid2
dfc$t1[ind] <- dfc$t2[ind]             # replace t1=t2 if cid1>cid2                                 
dfc[92,]
dfc[129,]
head(dfc,15)                           # t1 has pairs all in ascending order 
tail(dfc,15)

# Now we have character-character dyads based on each episode. If we want to get 
# c-c dyads without considering "which" episodes are connecting them, but how many
# episodes they are both featured, then we do the following.

# Get weight, i.e., how many shared episodes
dfc <- ddply(dfc, .(t1), mutate, weight=length(t1)/2)  # edges are double counted 
dfc <- ddply(dfc, .(t1), mutate, flag=1:length(t1))    # group duplicate dyads
el <- subset(dfc, subset= flag==1)  # keep the first record, 
                                    # i.e., drop all the duplicate dyads
el <- subset(el, select=c(cid1, cid2, weight)) 
row.names(el) <- NULL
el <- el[order(el$cid1, el$cid2),]
el

# The GOT episode-connecting network
library(igraph)
epi <- as.matrix(el)
epi <- graph.edgelist(epi[,1:2], directed=F)
plot(epi)
