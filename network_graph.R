library(tidyverse)

library(dplyr)

library(timeDate)

library(ggplot2)

library(grid)

as.Date(Orders$order_purchase_timestamp)

Orders = filter(Orders, between(as.Date(order_purchase_timestamp), as.Date("2017-11-01"), as.Date("2017-12-01")))

edge_list <- tibble(from = Orders$seller_state, to = Orders$customer_state)
node_list <- tibble(id = 1:27)


sources <- Orders %>%
  distinct(seller_state) %>%
  rename(label = seller_state)

destinations <- Orders %>%
  distinct(customer_state)%>%
  rename(label = customer_state)

nodes <- full_join(sources, destinations, by = "label")

nodes <- nodes %>% rowid_to_column("id")

per_route <- Orders %>%  
  group_by(seller_state, customer_state) %>%
  summarise(weight = n()) %>% 
  ungroup()

per_route<-per_route[!(per_route$seller_state==per_route$customer_state),]

edges <- per_route %>% 
  left_join(nodes, by = c("seller_state" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("customer_state" = "label")) %>% 
  rename(to = id)

edges <- select(edges, from, to, weight)

install.packages('network')
library(network)

routes_network <- network(edges, vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE)

plot(routes_network, vertex.cex = 3, mode = "circle")

library(tidygraph)
library(ggraph)

routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

routes_igraph_tidy <- as_tbl_graph(routes_igraph)

routes_tidy %>% 
  activate(edges) %>% 
  arrange(desc(weight))


ggraph(routes_tidy) + geom_edge_link() + geom_node_point() + theme_graph()

ggraph(routes_tidy, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label,col='red'), repel = TRUE) +
  labs(edge_width = "Letters") +
  theme_graph()

ggraph(routes_tidy, layout = "linear") + 
  geom_edge_arc(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label)) +
  labs(edge_width = "Letters") +
  theme_graph()

subs_pal <- colorspace::qualitative_hcl(5)
ggraph(routes_tidy, layout = "linear",circular = TRUE) + 
  geom_edge_arc(aes(width = weight),arrow = arrow(length = unit(3, 'mm')),  alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label),size =4.5,col='red', repel =TRUE ,
                 hjust = 1,
                 vjust = 1.25) + labs( title = "Seller State to Customer State network path (Dec 2017)") +
  labs(edge_width = "Number of deliveries") +
  theme_graph()+
  theme(legend.position = "bottom")

help(geom_edge_arc)

install.packages('visNetwork')

library(visNetwork)

install.packages('networkD3')

library(networkD3)

visNetwork(nodes, edges) %>%
  visInteraction(dragNodes = FALSE, dragView = FALSE, zoomView = FALSE)

help(visInteraction)

edges1 <- mutate(edges, width = weight/5 + 1)

visNetwork(nodes, edges1) %>% 
  visIgraphLayout(layout = "layout_with_fr") %>% 
  visEdges(arrows = "middle")

visLayout(randomSeed = 123)


library(igraph)
net.igraph <- graph_from_data_frame(
  d = edges, vertices = nodes, 
  directed = TRUE
)

set.seed(123)
plot(net.igraph, edge.arrow.size = 0.1,
     layout = layout_with_graphopt)
