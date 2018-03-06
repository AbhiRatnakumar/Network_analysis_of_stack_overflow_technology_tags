library("tidyverse")
library("tidygraph")
library("ggraph")
library("igraph")
library("ggrepel")

## Data was downloaded from KAGGLE:
## https://www.kaggle.com/stackoverflow/stack-overflow-tag-network

### Network analysis of stack overflow network tags #######
node_data <- read_csv("stack_network_nodes.csv")
edge_data <- read_csv("stack_network_links.csv")

### Make the network #########
net <- tbl_graph(nodes=node_data, 
                 edges=edge_data, 
                 directed=FALSE)

### Plot the network #######
pdf("network1.pdf")
net %>%
  ggraph(layout = 'nicely') + 
  geom_edge_link() + 
  geom_node_point(size = 5, 
                  colour = 'light blue') +
  geom_node_text(aes(label = name), 
                 colour = 'black', 
                 vjust=0.5,
                 size = 3) + 
  labs(title='Stack Overflow Technologies') + 
  theme_graph()
dev.off()

######## Plot a histogram of the in-degree distribution ##########
pdf("Indegree_distribution.pdf")
hist(degree(net, mode = "in"), col="dark red", xlab = "In-degree", main = "In-degree distribution")
dev.off()
######## Plot a network, highlighting the nodes with high in-degree #######
pdf("Colour_code_nodes_indegree_gt_19.pdf")
net %>% 
  mutate(centrality = centrality_degree()) %>% mutate(cent_imp= centrality > 19) %>%
  ggraph(layout = 'kk') + 
  geom_edge_link() + 
  geom_node_point(aes(size = centrality, colour = cent_imp)) + 
  scale_color_discrete() + 
  theme_graph()
dev.off()
#### Extract out the nodes with in-degree >= 20 ############
pdf("important_nodes.pdf")
net %>% 
  filter(important == TRUE) %>%
  ggraph(layout = 'gem') + 
  geom_node_point(aes(size = nodesize*10), colour = 'cyan') + 
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_graph()
dev.off()
#### Calculate Eignvector Centrality ############
top_20_centrality <- net %>% 
  activate(nodes) %>% 
  mutate(cent_degree = centrality_eigen()) %>% 
  arrange(desc(cent_degree)) %>% 
  top_n(20) 
top_20_centrality[[1:20]]

##############Community detection #######################
data <- read.csv("stack_network_links.csv", header = T)
y <- data.frame(data$source, data$target)
net <- graph.data.frame(y, directed = F)
pdf("stack_overflow_community_detection.pdf")
par(mfrow=c(1,1))
cnet <- cluster_edge_betweenness(net)
plot(cnet, 
     net,
     vertex.size = 8,
     vertex.label.cex = 0.8)
dev.off()
#### Print out all the elements in the communities ########
cnet[ 1:length(cnet) ]
##############################