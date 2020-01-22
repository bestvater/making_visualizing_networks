#' ---
#' title: "Making and Visualizing Networks with R"
#' subtitle: "PLSC 508 Political Networks"
#' author: "Sam Bestvater"
#' date: "1/27/2020"
#' ---
#' 
#' The `R` programming language has been growing massively in popularity as a tool for network analysis thanks to its flexibility, 
#' visualization tools, and the availability of packages like `statnet` developed for working with networks and relational data. 
#' In this tutorial, we will get up and running with with a basic working example exploring the core functionality of the  `network` 
#' library. We will also explore `ggnetwork`, a tool for integrating `network` objects with the visualization library `ggplot2`. 
#' 
#' Let's start by loading some packages. `statnet` is a full suite of tools for working with networks. We'll only be using functions 
#' from the `network` and `sna` libraries, but for convenience' sake let's go ahead and load the entire `statnet` suite. We'll also 
#' load `ggplot2`, which is probably the most popular visualization library for `R` (and is one of `R`'s major comparative advantages 
#' against other languages such as python), and two supplemental libraries for `ggplot2`: `GGally` and `ggnetwork`. 
#' 
#' 
#' 
#'
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# install.packages('statnet')
# install.packages('ggplot2')
# install.packages('GGally')
# install.packages('ggnetwork')

require(statnet)
require(ggplot2)
require(GGally)
require(ggnetwork)

set.seed(8675309) # set a random number seed so that visualizations appear the same each time the script runs
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' # MAKING NETWORKS IN R
#' 
#' We'll start by taking relational data in several different formats and using it to create a `network` object. We'll be working with 
#' a network of social movement organizations active during the American Civil Rights movement, constructed from the Dynamics of 
#' Collective Action data (DCA), a protest event dataset collected by Doug McAdam, John McCarthy, Susan Olzak, and Sarah Soule 
#' [(More info here)](https://web.stanford.edu/group/collectiveaction/cgi-bin/drupal/). For a little background, social movement 
#' organizations (SMOs) appear as nodes in this network if they were recorded as participating in at least 2 civil rights-related 
#' protest or counterprotest by the New York Times, and organizations are linked to each other if they both appeared at the same event.
#' 
#' 
#' 
#' ## CREATING A NEW `network` OBJECT
#' 
#' ### FROM ADJACENCY MATRIX
#' 
#' One of the most common ways to store relational data (at least for relatively small networks) is an adjacency matrix--a square matrix 
#' where the rows and columns both represent the nodes of the network. The presence or absence of a tie between node $i$ and node $j$ is 
#' indicated by the value at cell $i,j$--in an unweighted network, such as the version of the DCA civil rights network we're about to load,
#' cells can take the value of either 1 or 0. Let's go ahead and load the adjacency matrix of this network as an object in `R` and examine 
#' the first few rows and columns:
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

DCA_adj <- as.matrix(read.csv('./data/DCA_adj.csv', row.names = 1))

dim(DCA_adj)
DCA_adj[1:5, 1:5]

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' So we see that this is an 88x88 matrix representing the nodes and links in the network. Next, let's convert this into a `network` object.
#' 
#' The core command in the `network` library is the `network()` function, which takes input data and produces a `network` object that can 
#' be used throughout the rest of the `statnet` suite. `network()` requires a matrix with the relational data to be specified, along with a 
#' value for `matrix.type` from either `'adjacency'`, `'edgelist'`, or `'incidence'`. Additionally, we can supply additional vertex attributes, 
#' and boolean values for network characteristics such as whether the network is directed, bipartite, etc. This version of the DCA civil 
#' rights network is an undirected one-mode network, so our command is `network(DCA_adj, matrix.type = 'adjacency', directed = F)`.
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
net1 <- network(DCA_adj, matrix.type = 'adjacency', directed = F)

net1
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' If we examine this object, we see that we have created a `network` object with 88 vertices and 100 edges. The vertices are named, 
#' but have no other attributes, and edges are undirected.
#' 
#' ### FROM EDGELIST
#' 
#' Another common format to store relational data is the edgelist, which in its simplest form is an Nx2 matrix where each row represents
#' an edge from the node in the first column to the node in the second column. If needed, further edge attributes such as weights can be 
#' specified in additional columns. Let's load the DCA civil rights network data in edgelist format and examine it:
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

DCA_el <- as.matrix(read.csv('./data/DCA_el.csv'))

dim(DCA_el)
head(DCA_el)

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' We can see that this object contains a 100x2 matrix where each cell is the name (ID) of a vertex in the network, and each row 
#' represents an edge between two vertices. 
#' 
#' The `network()` function can also take this edgelist matrix as an input, as long as we specify which columns hold the vertex IDs. 
#' The only thing we will need to change from the command we used earlier is the `matrix.type` argument should be set to `'edgelist'`.
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

net2 <- network(DCA_el[,1:2], matrix.type = 'edgelist', directed = F)

net2

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' We can see that once again we have an undirected, single-mode network with 100 edges. But this time, there are only 61 vertices, 
#' rather than 88. One reason that this could happen is that edgelists, by construction, can't contain information on nodes that are 
#' not connected to any other nodes (isolates). Since each row in the edgelist matrix represents an edge, nodes without edges simply 
#' do not appear in the edgelist representation of a network, even if they do exist in the adjacency matrix. We can check if this is 
#' what's going on here using the `isolates()` command from the `sna` package, which takes a `network` object as input and returns a 
#' vector of IDs for any isolated nodes.   
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

sna::isolates(net1)

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' It looks like several nodes in `net1` are isolates.
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

sna::isolates(net2)

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' And `net2` has no isolates. Mystery solved!
#' 
#' ## CONVERTING OTHER NETWORK FORMATS TO `network` OBJECT
#' 
#' ### FROM PAJEK
#' 
#' As popular as `R` has become for network analysis, it's certainly not the only program people use for this task, and for compatibility 
#' sometimes you'll want to be able to load network data from other sources. One common file format for network data is the `.net` format 
#' from the `pajeck` software. Luckily, `network` includes a dedicated function for reading in pajek files, `read.paj()`.
#' 
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

net3 <- read.paj('./data/DCA_pajek.net')

net3

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' ### FROM IGRAPH
#' 
#' `statnet` is also not the only network analysis suite within `R`. It competes with another library called `igraph`. If you have data 
#' in the form of an `igraph` object, `network` unfortunately can't read that directly. However, there is another library called 
#' `intergraph` which exists to translate between the formats for us. Let's give it a try:
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#install.packages('igraph')
#install.packages('intergraph')

require(igraph)
require(intergraph)

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' In order to convert a `network` object into an `igraph` object, we use the function `intergraph::asIgraph()`. 
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

net4 <- asIgraph(net1)

net4

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' We can see that `igraph` objects look a little different from the `network` objects we're used to at this point. Let's convert back, 
#' using `intergraph::asNetwork()`.
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
net5 <- asNetwork(net4)

net5

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' There we go, this network should be identical to the one we started with.
#' 
#' It's worth noting that in addition to competing for functionality, `igraph` and `network` compete for namespace within `R`, meaning 
#' that there is overlap in function names between both libraries. This can create problems in your script if you try to load both packages 
#' at the same time. Generally, it's best practice to pick one and stick with it. If you need to use both for some reason, the best way to 
#' insure you won't run into namespace issues is by specifying the library for the commands you use with the prefixes `igraph::` and `network::`. 
#' Right now we'll just go ahead and remove `igraph` from our workspace to prevent confusion. 
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

detach(package:igraph, unload = T)

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' ## MODIFYING A `network` OBJECT
#' 
#' Okay, so that was a few ways to load existing network data into `R` as a `network` object. But what if we want to modify an existing 
#' network object? `network` makes this easy as well.
#' 
#' ### ADDING NEW EDGES AND VERTICES
#' 
#' Let's say we want to add a few new nodes to the network. This is easily achieved with the `add.vertices()` command, which in its 
#' simplest form takes a network object and a number of new nodes as arguments:
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

add.vertices(net1, 2)

net1

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' Okay, so now we have 90 nodes rather than 88, so that was a success, but in our DCA civil rights network, the nodes have names, and 
#' we haven't supplied names for our new nodes yet. Let's look at the `vertex.names` attribute of our network and see what that looks like now. 
#' There are a few ways to do this. First, we can use `get.vertex.attribute()` which takes as arguments the name of a network object and the 
#' name of an attribute we'd like to see:
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

get.vertex.attribute(net1, 'vertex.names')

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' This can also be written in shorthand, making use of `network`'s `%v%` operator, and can be subset using `tail()` or indexing just 
#' like any other vector:
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

tail(net1 %v% 'vertex.names')

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' Both of the above commands will work with any vertex attribute the network might have. But because names are an especially common 
#' attribute, they get their own command in addition to the general-purpose ones: `network.vertex.names()`.
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

tail(network.vertex.names(net1))

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' That's three different ways to access the same information, all of which confirm that our new nodes don't have any names associated 
#' with them. Let's look at how we might go about adding some:
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

new_names <- c('Foo', 'Bar')

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' Let's start by accessing the `vertex.names` vector again, and assigning it to a new variable:
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

old_names <- net1 %v% 'vertex.names'

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' Now, lets replace the last 2 indices of this vector with our new names:
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

new_names <- c(old_names[1:88], new_names)

tail(new_names)

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' Now we can re-assign the `vertex.names` vector with the new vector:
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

network.vertex.names(net1) <- new_names

tail(network.vertex.names(net1))

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' Perfect! Now let's add some links between these new nodes. This is done using the `add.edges()` command, which takes as arguments 
#' a network object, a vector of node IDs for the tails (senders) of new ties, and a vector of node IDs for the heads (receivers) of 
#' new ties. Let's add a few:
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

add.edges(net1, c(90,89,90,89), c(1,5,36,28))

net1

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' There we go, now our network has 90 nodes and 104 edges.
#' 
#' To delete elements from a `network` object, we can use the commands `delete.edges()` and `delete.vertices()`. When we remove a node, 
#' all associated edges are removed along with it:
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

delete.vertices(net1, vid = c(89,90))

net1

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' ### Adding other network attributes
#' 
#' We can also add other attributes to the nodes if we want. Let's start by loading a dataframe containing several other characteristics 
#' for the SMOs in our network:
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

nodelist <- read.csv('./data/DCA_nodelist.csv', stringsAsFactors = F)

head(nodelist)

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' Let's start with the column called `event_count`. This column contains information on the number of protest events that each group 
#' is recorded as attending in the DCA data during the civil rights movement. We can see from just the first few rows that there's a 
#' pretty wide variance here. Some central groups to the movement, such as CORE or the NAACP were involved in hundreds of protests, 
#' while others were just at a handful. Let's add `event_count` as a node attribute in our network. All we need in order to do this 
#' is a vector of values for our new attribute and the `set.vertex.attribute()` command, which takes as arguments a `network` object,
#'  the name to be assigned to the new attribute, and a vector of values equal in length to the number of nodes in the network. In 
#'  this case, we'll make a new attribute called `event_count` and assign it the values from `nodelist$event_count`.
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

set.vertex.attribute(net1, 'event_count', nodelist$event_count)

net1

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' The DCA data also contains information about the valence of each SMO toward the central issues of the protest. For example, it's 
#' probably clear to you that while the NAACP and the Ku Klux Klan were both organizations involved in the civil rights movement, they 
#' represent *very* different positions on the issue. The `nodelist` dataframe we just loaded contains this information in the column 
#' called `valence`, where a value of 1 represents support for civil rights and 2 represents opposition. Let's add this to the network 
#' as a node attribute, but first let's recode these values as strings so they are more easily interpreted. 
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

valence <- ifelse(nodelist$valence == 2, 'Opposition', 'Support')

set.vertex.attribute(net1, 'valence', valence)

net1

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' This, of course, just scratches the surface of how we can modify `network` objects to suit our specific needs, but hopefully it provides 
#' a helpful overview of the logic underlying `network` objects and the tools we can apply to them. 
#' 
#' # VISUALIZING NETWORKS IN R
#' 
#' The final thing we'll cover today is perhaps one of the most gratifying parts of beginning to work with network data: actually plotting 
#' the network and getting to visualize the connections. This is remarkably easy to do, using the `plot()` command built into `network`. 
#' 
#' ## Using `network::plot`
#' 
#' At a minimum, all that  `network::plot()` requires is a network object. It will, by default, calculate a Fruchterman-Reingold 
#' force-directed layout for the nodes, draw them, then draw the edges:
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

plot(net1)

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' We can add labels to our plot:
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

plot(net1, 
     displaylabels = T)

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' Okay, so that's pretty ugly. We really don't have enough room in this figure to include all of these names and have them be at all 
#' readable. Let's try just labeling the groups that participated in at least 10 events. We can do this by adding a new vertex attribute 
#' to the network and using that as the labels instead:
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

nodelist$label <- ifelse(nodelist$event_count >= 10, nodelist$name, NA) #make a new vector of just those labels

set.vertex.attribute(net1, 'labels', nodelist$label)

plot(net1, label = net1 %v% 'labels')

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' That looks a little better, but some of the longer names are still getting cut off (on my screen at least). We'll look at one 
#' solution to this issue in a moment.
#' 
#' We can also change the color of vertices and edges:
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

plot(net1, 
     label = net1 %v% 'labels',
     vertex.col = 'blue',
     edge.col = 'grey')

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' You may have noticed that each time we made a change and re-drew the figure, the layout was different. This is because force-directed 
#' algorithms such as Fruchterman-Reingold have a random component and will provide a different layout solution each time they are run. 
#' If we want to be able to replicate the layout of a plot several times, we can save the coordinates and reuse them. Let's start by 
#' creating another plot using the `plot()` command and assigning it to an object, `xy`.
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

xy <- plot(net1)

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' Now, we can make another plot use the `coord` argument to reuse these coordinates:
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

plot(net1, 
     label = net1 %v% 'labels',
     vertex.col = 'blue',
     edge.col = 'grey', 
     coord=xy)

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' There's a lot more that can be done with `network::plot()` graphics--type `?plot.network.default()` for more details on all the 
#' different parameters that can be adjusted.
#' 
#' People who use base `R` graphics tend to like the visualization tools included in `network`, since a lot of the functionality is 
#' mirrored and stuff tends to work basically the same way. I for one, am never particularly happy with the way base `R` plots 
#' (and, by extention `network::plot()` figures) end up looking. I generally think that `ggplot2`'s "grammar of graphics" is a better 
#' way to approach visualization, and is one of the primary comparative advantages of `R` as a programming language. Luckily, for folks 
#' like me, there's `ggnetwork`, a function that makes it super easy to plot `network` objects using `ggplot2`. Let's take a look:
#' 
#' ## USING `ggnetwork` AND `ggplot2`
#' 
#' The core function of the `ggnetwork` library is just called `ggnetwork()`, and it does something pretty cool--it takes a `network` 
#' object as an input, runs a layout algorithm, and outputs a dataframe of x and y coordinates for the nodes and links in the network 
#' that can be directly passed to `ggplot()`. By default, `ggnetwork()` will use the Fruchterman-Reingold layout algorithm, but several 
#' others are also available. 
#' See [the documentation for `sna::gplot.layout()`](https://www.rdocumentation.org/packages/sna/versions/2.5/topics/gplot.layout) 
#' for a full list. Let's pass our `net1` object to `ggnetwork()` and assign the result to a new dataframe: 
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

net1_fr <- ggnetwork(net1, layout = 'fruchtermanreingold')

head(net1_fr)

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' Let's also try a few other layout algorithms, like the Kamada-Kawai force-directed algorithm, and just a simple circle layout:
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

net1_kk <- ggnetwork(net1, layout = 'kamadakawai')

head(net1_kk)

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

net1_cir <- ggnetwork(net1, layout = 'circle')

head(net1_cir)

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' We see that `ggnetwork()` has produced a dataframe containing coordinates for both the vertices (`x` and `y`) and edges (`xend` and `yend`), 
#' as well as all our vertex attributes. We can add these as aesthetics to a `ggplot` object, and the add the geom `geom_nodes()`, which 
#' essentially functions like `geom_point()`.
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
ggplot(data = net1_fr, aes(x, y, xend=xend, yend=yend))+
  geom_nodes()

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' This doesn't look like too much yet, but we can see the results of the Fruchterman-Reingold layout--it's mapping the nodes of our 
#' network into 2-dimensions, scaled between 0 and 1. The x and y axes aren't themselves interpretable, but this is the information 
#' that `ggplot` (and any other graphics system we might want to use) is actually using to plot the nodes. 
#' 
#' Like with any other geom added to a `ggplot` object, we can add more aesthetics to our `geom_nodes()`, which is really helpful for 
#' displaying node attributes in our network plot. Let's assign `valence` to `color` and the log of `event_count` to `size`:
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
ggplot(data = net1_fr, aes(x, y, xend=xend, yend=yend))+
  geom_nodes(aes(color = valence, size = log(event_count)))

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' Okay, this is showing us a good deal of information already, but it's not a network until it has edges. These are added with the 
#' `geom_edges()` geom:
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
ggplot(data = net1_fr, aes(x, y, xend=xend, yend=yend))+
  geom_nodes(aes(color = valence, size = log(event_count)))+
  geom_edges()

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' There we go! Like before, we can make adjustments to the aesthetics of `geom_edges()` if we wish. If we were working with a weighted 
#' network, we might want to assign the edge weight values to `size` so that stronger edges appear thicker in our resulting plot. In 
#' this case, though, we don't have any edge characteristics that vary, so we'll just change the color. Let's also swap the positions 
#' of `geom_nodes()` and `geom_edges()`--when `ggplot()` draws a figure, it draws the layer described by each geom in the order it 
#' appears in the code. Therefore, in our plot nodes are getting drawn first and then edges are getting drawn over top of them, which 
#' looks a little weird. We could probably fix this by playing around with the parameters for edge length and trying to force a gap at 
#' each node, but it's much easier to simply have `ggplot()` draw the edges first, then the nodes on top. Let's do that:
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
ggplot(data = net1_fr, aes(x, y, xend=xend, yend=yend))+
  geom_edges(color = 'grey75')+
  geom_nodes(aes(color = valence, size = log(event_count)))

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' Now let's label the nodes. We can add text to our nodes with `geom_nodetext()`. We could supply any values we wanted here, but let's 
#' again use the attribute we just created to label only the most active groups:
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
ggplot(data = net1_fr, aes(x, y, xend=xend, yend=yend))+
  geom_edges(color = 'grey75')+
  geom_nodes(aes(color = valence, size = log(event_count)))+
  geom_nodetext(aes(label = labels))

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' That's looking pretty good, but some of the names are still getting cut off. Here's one area where `ggplot2` can really shine for 
#' network visualization: it can place labels so that they "repel" each other and do not overlap or get placed outside the border of 
#' the figure. To use this feature, we use `geom_nodetext_repel()` instead of `geom_nodetext()`.
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
ggplot(data = net1_fr, aes(x, y, xend=xend, yend=yend))+
  geom_edges(color = 'grey75')+
  geom_nodes(aes(color = valence, size = log(event_count)))+
  geom_nodetext_repel(aes(label = labels))

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' That looks pretty good. Now, since it's a standard `ggplot` object, we can make any other changes to it that we might wish. Let's remove 
#' axis ticks and grid lines using `theme_blank()`, move the legend to the bottom of the figure using `theme(legend.position='bottom')`, 
#' remove the legend for centrality using `guides(size=F)`, and add a title. Let's also assign it a variable name so we can call it again if we want.
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
plot_fr <- ggplot(data = net1_fr, aes(x, y, xend=xend, yend=yend))+
  geom_edges(color = 'grey75')+
  geom_nodes(aes(color = valence, size = log(event_count)))+
  geom_nodetext_repel(aes(label = labels))+
  theme_blank()+
  theme(legend.position="bottom")+
  guides(size = F)+
  labs(title = 'Network of co-protest ties between SMOs in the civil rights movement', color = 'Group Valence')

plot_fr

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' Let's see what this looks like if we use the other layouts we calculated earlier:
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
plot_kk <- ggplot(data = net1_kk, aes(x, y, xend=xend, yend=yend))+
  geom_edges(color = 'grey75')+
  geom_nodes(aes(color = valence, size = log(event_count)))+
  geom_nodetext_repel(aes(label = labels))+
  theme_blank()+
  theme(legend.position="bottom")+
  guides(size = F)+
  labs(title = 'Network of co-protest ties between SMOs in the civil rights movement', color = 'Group Valence')

plot_kk

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' So the Kamada-Kawai layout is similar to the Fuchterman-Reingold, but the connected component at the center of the figure seems to be 
#' layed out in a somewhat tighter configuration. How about the circle layout?
#' 
#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
plot_cir <- ggplot(data = net1_cir, aes(x, y, xend=xend, yend=yend))+
  geom_edges(color = 'grey75')+
  geom_nodes(aes(color = valence, size = log(event_count)))+
  geom_nodetext_repel(aes(label = labels))+
  theme_blank()+
  theme(legend.position="bottom")+
  guides(size = F)+
  labs(title = 'Network of co-protest ties between SMOs in the civil rights movement', color = 'Group Valence')

plot_cir

#' ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#' 
#' And finally, any `ggplot` object can be easily saved with `ggsave()`, so let's go ahead and export the FR version of the plot as a .png:
#' 
## ---- warning=F----------------------------------------------------------
ggsave('DCA_civilrights_network.png', plot = plot_fr, width = 10, height = 10, units = 'in', dpi = 600)













