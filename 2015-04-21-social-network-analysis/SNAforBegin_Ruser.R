
# =====================================
# Social Network Analysis for Beginners 
# R Users Group meetup, April 2015 
# Yun Tai 
# ytai@virginia.edu
# =====================================


# ---------------------------------
# work with network/relational data 
# ---------------------------------

# Read in your data ------------------------------------------------

setwd('C:/Users/yt3f/ytai/Rusersgroup')   

# We will use "gotRel.csv" here. Let's first take a look of the original file.

# Import an adjacency matrix using read.csv() function. 
gotdf <- read.csv(file='gotRel.csv',header=TRUE,row.names=1,stringsAsFactors=FALSE)
gotdf     # header=T and row.names=1 help to make it as a 10 by 10 matrix
gotmat <- as.matrix(gotdf)  # convert it to a matrix form


# The "Network" package: "network" objects -------------------------------------------

# A network object is a R object that can store both relational data and its metadata. 
# Here we use the "network" package to construct, describe, manipulate, and plot network objects.    

#install.packages('network')   
library(network)              

# Using as.network.matrix() to convert the matrix (gotmat) into a network object, 
# which is undirected and carries edge values named "weight"
gotmat
gotnet <- as.network.matrix(gotmat,directed=FALSE,ignore.eval=FALSE,names.eval="weight")
gotnet                     # quick description of the network 
summary(gotnet)            # more info of the network    		
network.edgecount(gotnet)  # how many edges in the network	
network.size(gotnet)			 # how many nodes in the network

plot(gotnet)  # a quick plot of the network
plot(gotnet,displaylabels=TRUE)  # put on node names 

# In case you like to add or delete edges:
network.vertex.names(gotnet)     # list the nodes
gotnet[2,3]       # are Cersei and Ned connected? 
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
gotnet %v% "id" <- gotatt$ID
gotnet %v% "name" <- gotatt$NAME
gotnet %v% "sex" <- gotatt$SEX
gotnet %v% "family" <- gotatt$FAMILY
gotnet %v% "status" <- gotatt$STATUS  
gotnet %v% "appear" <- gotatt$APPEAR

gotnet
list.vertex.attributes(gotnet)  # list node atrribute names
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

#install.packages('sna')
library(sna)  

# The "sna" package can handle many forms of network data, e.g., network object, adjacency matix, 
# or a list of adjacency matrices. In other words, functions of this package can be used on 
# various forms of data. We'll work with edgelist in the following example.

floel <- as.edgelist.sna(flo)  # convert the flo matrix into a sna edgelist 
head(floel)
attr(floel,"n")                # how many nodes?
attr(floel,"vnames")           # node names
as.sociomatrix.sna(floel)      # convert it to a sna matrix


# Measure centrality ---------------------------------------

data(emon)  # load the "emon" dataset
?emon       # it contains 7 network objects
emon.tx <- emon$Texas  # we'll only use the Texas one here
emon.tx     # a directed network

# degree cnetrality
degree(emon.tx)  # total degree of each node
degree(flo)      # works for a matrix object as well
indg <- degree(emon.tx, cmode="indegree")  # in-degree
indg
oudg <- degree(emon.tx, cmode="outdegree") # out-degree
oudg

# plot in-degree by out-degree
plot(x=indg,y=oudg,xlab="Indegree",ylab="Outdegree")

# Which GOT character is more central than others, in terms of degree centrality?
gotdg <- degree(gotnet)
max(gotdg)  # the max value in gotdg
which(gotdg==12)  # returns the position of the value=12
network.vertex.names(gotnet)

# betweeness and closeness centrality
bet <- betweenness(emon.tx)
bet  # betweenness centrlity of each node
clo <- closeness(emon.tx)
clo  # closeness centrlity of each node

# plot the relationship of closeness and betweenness
plot(x=clo, y=bet, xlab="Closeness", ylab="Betweenness")

# how about betweenness and closeness in our GOT network
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
plot(emon.tx)
plot(emon.tx, coord=Coords1)  # apply a saved layout

# more info of graph plotting
?gplot
