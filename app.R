#New Author: Mike Cioce - 2/10/18

library(shiny)
# install.packages('phangorn', repos='http://cran.us.r-project.org')
# install.packages('castor', repos='http://cran.us.r-project.org')
# install.packages('visNetwork', repos='http://cran.us.r-project.org')
# install.packages('hashmap', repos='http://cran.us.r-project.org')
# install.packages('plyr', repos='http://cran.us.r-project.org')
# library(phangorn)
# library(devtools)
library(ape)
library(castor)
# library(strataG)
library(visNetwork)
library(hashmap)
library(plyr)
#check out dplyr
#check out lubridate package
library(network)
library(igraph)
library(data.table)


# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Zika Virus Transmission Graph"),
  fluidRow(
    column(
      width = 2,
      selectInput("degree", "Scale Node Size By:",
                    c("outdegree", "indegree"), width="150px")
      )
    ),
  visNetworkOutput("network", width = "1500px", height = "1000px")
  )


# Define server logic required to draw a histogram ----
server <- function(input, output) {

  output$network <- renderVisNetwork({


countryRef <- c(1, 2, 3, 3, 3, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 3, 3, 3, 4, 4, 4, 4, 5, 4, 6, 7, 8, 3, 3, 3, 3,
  4, 3, 4, 3, 4, 5, 6, 6, 8, 8, 5, 5, 5, 5, 5, 5, 5, 6, 5, 5, 5, 5, 3, 3, 3, 3, 4, 4, 9, 9, 9, 9, 9, 9, 10, 10, 10, 9, 6, 6,
  10, 10, 10, 9, 9, 11, 12, 12, 13, 13, 14, 15, 13, 7, 13, 12, 9, 9, 9, 9, 9, 9, 9, 13, 15, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
  16, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 17, 17, 9, 9, 18, 18, 18, 18, 13, 13, 18, 18, 18, 18, 19, 19, 19, 18, 18,
  19, 18, 18, 18, 18, 15, 20, 5, 5, 6, 20, 18, 21, 21, 18, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 13,
  37, 37, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 22, 22, 23, 23, 23, 23, 23, 24, 24, 24, 24, 23, 23, 22, 22,
  22, 22, 22, 22, 22, 22, 25, 25, 26, 27, 28, 29, 29, 30, 30, 30, 30, 7, 30, 30, 16, 16, 16, 16, 16, 16, 16, 16,
  16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
  16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
  16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
  16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
  16, 16, 16, 16, 16, 30, 30, 16, 31, 32, 7, 14, 33, 33, 13, 7, 7, 34, 13, 7, 33, 7, 14, 7, 31,
  32, 32, 32, 9, 9, 35, 35, 35, 9, 9, 9, 9, 15, 36, 37, 37, 37, 37, 37, 37, 37, 15, 15, 15,
  15, 15, 15, 15, 15, 15, 15, 15, 15, 38, 15, 15, 15, 15, 15, 1, 15, 15, 15, 15, 6, 6, 17,
  15, 15, 15, 15, 15, 15, 15, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
  6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 15, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
  6, 6, 6, 15, 15, 1, 1, 1, 31, 15, recursive=TRUE)



 ref2 <- c("KY785438.1", "KY785454.1", "KY014310.1", "KY014319.1", "KY785442.1", "KY765317.1", "KY765322.1", "KY765318.1",
  "KY765321.1", "MF434521.1", "KY785416.1", "KY014312.1", "KY785452.1", "KY328289.1", "KY014311.1", "KY014306.1", "KY014327.1",
   "KY693677.1", "KY765325.1", "KY765327.1", "KY785461.1", "KY014315.1", "KY785418.1", "KX421194.1", "KX421195.1", "KY765319.1", 
   "KY765320.1", "KY606271.1", "KY765324.1", "MF159531.1", "KY927808.1", "KU870645.1", "KY785431.1", "KY693676.1", "KY785414.1", 
   "KY785448.1", "MF434516.1", "KY785458.1", "MF434522.1", "KY785471.1", "MF434517.1", "KY606274.1", "KY325465.1", "KY325479.1", 
   "KU501217.1", "KU501216.1", "KY606273.1", "KX247632.1", "KX446951.2", "KX446950.2", "KY120348.1", "KY120349.2", "KX766029.1", 
   "KX827268.1", "KY606272.1", "KY631493.1", "KY631494.1", "MF098771.1", "KX262887.1", "KX694534.2", "KX906952.1", "KY785444.1", 
   "KY765323.1", "KY765326.1", "KY785410.1", "KU926309.1", "KY014308.1", "KY785437.1", "KY272991.1", "KY785409.1", "KX377337.1", 
   "KU501215.1", "KX601168.1", "KY785436.1", "KY075933.1", "KY075934.1", "KY785462.1", "KY785464.1", "KY785481.1", "KY785456.1", 
   "KU365778.1", "KU758877.1", "KU937936.1", "KY348640.1", "KU761564.1", "KU740184.2", "KU820898.1", "KX766028.1", "KU955590.1", 
   "MF167360.1", "KX056898.1", "KU312312.1", "KY014297.1", "KY785450.1", "KU365777.1", "KU365780.1", "KU365779.1", "KU707826.1", 
   "KY631492.1", "KY693680.1", "KY785483.1", "KX197205.1", "KX830930.1", "KY785446.1", "KY559021.1", "KY014296.1", "KY014320.1", 
   "KY014301.1", "KY785479.1", "KY014309.1", "KY785427.1", "KY014317.1", "KY241788.1", "KY559005.1", "KY559007.1", "KY559013.1", 
   "KU729218.1", "KU940224.1", "KX520666.1", "KX101067.1", "KU940228.1", "KU940227.1", "KX101063.1", "KX101062.1", "KX101064.1", 
   "KX101065.1", "KX101061.1", "KY003153.1", "KY003154.1", "KX101060.1", "KX101066.1", "KY785477.1", "KY785417.1", "KY785469.1", 
   "KX247646.1", "KX702400.1", "KX893855.1", "KU820897.5", "MF574553.1", "MF574552.1", "KX087102.2", "KX156774.2", "KX156776.2", 
   "KX156775.2", "KY989971.1", "KY785466.1", "KX198135.2", "KY317938.1", "KY317940.1", "KY317936.1", "KY317939.1", "KY014303.1", 
   "KU647676.1", "KU922923.1", "KU922960.1", "KY075932.1", "KY785451.1", "KY317937.1", "KY693678.1", "KY693679.1", "KX548902.1", 
   "KY559015.1", "KY785480.1", "KY785455.1", "KY558999.1", "KU497555.1", "KU991811.1", "KY014307.1", "KY014313.1", "KY785433.1", 
   "KY785439.1", "KY559027.1", "KY785429.1", "KR872956.1", "MF352141.1", "KU321639.1", "KU744693.1", "KX051563.1", "KU509998.3", 
   "KX447518.1", "KX447519.1", "KX447513.1", "KX447520.1", "KX447511.1", "KX369547.1", "KX447512.1", "KX447521.1", "KX447514.1", 
   "KX447509.1", "KJ776791.2", "DQ859059.1", "KY288905.1", "HQ234501.1", "KX601166.1", "KX198134.2", "KF383116.1", "KF383117.1", 
   "KF383115.1", "KF268949.1", "KF268950.1", "KF268948.1", "KF383118.1", "KF383119.1", "LC002520.1", "NC_012532.1", "KX421193.1", 
   "KY989511.1", "KX830960.1", "KU720415.1", "KX601169.1", "KX377335.1", "KX377336.1", "KX601167.1", "EU545988.1", "KU681082.3", 
   "KY553111.1", "JN860885.1", "KU955593.1", "KF993678.1", "KY272987.1", "KX051561.1", "KX051562.1", "KY328290.1", "KX051560.1", 
   "KU681081.3", "KX813683.1", "KY241713.1", "KY241689.1", "KY241736.1", "KY241671.1", "KY241716.1", "KY241697.1", "KY241783.1", 
   "KY241698.1", "KY241740.1", "KY241721.1", "KY241706.1", "KY241749.1", "KY241729.1", "KY241773.1", "KY241714.1", "KY241738.1", 
   "KY241744.1", "KY241777.1", "KY241782.1", "KY241707.1", "KY241751.1", "KY241734.1", "KY241675.1", "KY241685.1", "KY241767.1", 
   "KY241677.1", "KY241678.1", "KY241680.1", "KY241684.1", "KY241676.1", "KY241681.1", "KY241682.1", "KY241776.1", "KY241778.1", 
   "KY241780.1", "KY241727.1", "KY241708.1", "KY241771.1", "KY241760.1", "KY241761.1", "KY241784.1", "KY241728.1", "KY241731.1", 
   "KY241725.1", "KY241704.1", "KY241726.1", "KY241696.1", "KY241683.1", "KY241700.1", "KY241695.1", "KX827309.1", "KY241687.1", 
   "KY241758.1", "KY241750.1", "KY241733.1", "KY241701.1", "KY241752.1", "KY241743.1", "KY241745.1", "KY241753.1", "KY241747.1", 
   "KY241746.1", "KY241703.1", "KY241786.1", "KY241709.1", "KY241748.1", "KY241754.1", "KY241755.1", "KY241742.1", "KY241739.1", 
   "KY241711.1", "KY241763.1", "KY241719.1", "KY241679.1", "KY241779.1", "KY241781.1", "KY241785.1", "KY241741.1", "KY241702.1", 
   "KY241673.1", "KY241766.1", "KY241730.1", "KY241787.1", "KY241712.1", "KY241717.1", "KY241720.1", "KY241757.1", "KY241768.1", 
   "KY241764.1", "KY241770.1", "KY241765.1", "KY241772.1", "KY241762.1", "KY241699.1", "KY241737.1", "KY241705.1", "KY241735.1", 
   "KY241715.1", "KY241724.1", "KY241732.1", "KY241718.1", "KY241710.1", "KY241723.1", "KY241686.1", "KY241690.1", "KY241694.1", 
   "KY241692.1", "KY241775.1", "KY241759.1", "KY241774.1", "KY241693.1", "KY241722.1", "KY241756.1", "KY241688.1", "MF692778.1", 
   "KY126351.1", "KY241691.1", "LC219720.1", "KX447517.1", "KU761560.1", "KU820899.2", "KU963796.1", "KX185891.1", "KU955589.1", 
   "KY967711.1", "MF036115.1", "KX266255.1", "KX253996.1", "KU997667.1", "KU866423.2", "KX013000.1", "KX117076.1", "KU761561.1", 
   "LC191864.1", "KX447516.1", "KX447515.1", "KX447510.1", "KX280026.1", "KX811222.1", "KX879603.1", "MF794971.1", "KX879604.1", 
   "KU527068.1", "KU926310.1", "KU729217.2", "KY785426.1", "KY785460.1", "KX673530.1", "KY415988.1", "KY415986.1", "KY415990.1", 
   "KY415987.1", "KY415989.1", "KY415991.1", "MF384325.1", "KY785434.1", "MF098766.1", "KY014305.1", "KY785423.1", "KY014318.1", 
   "KY785420.1", "KY785413.1", "KY785447.1", "MF098768.1", "MF098769.1", "MF664436.1", "KY785441.1", "MF438286.1", "KY785475.1", 
   "KY785473.1", "KY785465.1", "KY785435.1", "KY785484.1", "KY785419.1", "KY785449.1", "KY785463.1", "KU853012.1", "KU853013.1", 
   "KX922707.1", "KY014316.1", "KX269878.1", "KY014302.1", "KY014304.1", "KY785476.1", "KY014314.1", "KY014321.1", "KY785415.1", 
   "KY785470.1", "KX832731.1", "KY014326.1", "KY325467.1", "KX842449.2", "KX922704.1", "KY014325.1", "KY325481.1", "KY325478.1", 
   "KX922706.1", "KX838904.2", "KY014324.1", "KY075938.1", "KY785472.1", "KY325477.1", "KY325483.1", "KY014295.1", "KY075936.1", 
   "KY325480.1", "KY785474.1", "KY325472.1", "KX922703.1", "KY785445.1", "KY075935.1", "KY785412.1", "KX838905.2", "KY014323.1", 
   "KY325468.1", "KY325469.1", "KY325466.1", "KY325475.1", "KY075939.2", "KY785468.1", "KY325470.1", "KY785443.1", "KY785457.1", 
   "KY785425.1", "KX922705.1", "KY014298.1", "KX922708.1", "KY014299.1", "KY785459.1", "KX838906.2", "KY014322.1", "KY325473.1", 
   "KY325474.1", "KY075937.1", "KY785422.1", "KY325482.1", "KY325464.1", "KY325476.1", "KY325471.1", "KY014300.1", "KY785453.1", 
   "KY785430.1", "KY785432.1", "KY785424.1", "LC190723.1", "KY785428.1", recursive=TRUE)
 
 rootedTree <- read.tree("zikaRootedTree.nexus")
 H <- hashmap(ref2, countryRef)

 countryStates <- c()
#Assign ancestral state to each tip in tree

 for(accession in rootedTree$tip.label) {
  if(accession == "AY632535.2") {
    countryStates <- c(countryStates, 22)
  } 
  else { 
    countryStates <- c(countryStates, H$find(accession))
  }
}

#Get ancestral states for each node in the tree using max parsimony algorithm
ancestralStates = asr_max_parsimony(rootedTree, countryStates, 38)

H$clear()

for(i in 1:length(countryStates)) {
  H$insert(i, countryStates[i])
}

#Loop through each ancestral state, get the state with the highest probability

for (i in 474:(length(ancestralStates$ancestral_likelihoods[,1])+473)) { counter <- c()
   for (j in 1: length(ancestralStates$ancestral_likelihoods[1,])) { 
     counter <- c(counter, ancestralStates$ancestral_likelihoods[i-473,j] )
   }
   H$insert(i, match(max(counter), counter))
}

sourceList <- c()
targetList <- c()

#Loop through each edge in the tree, if there's a state change between the two edges, add them to their respective list
for(row in 1:nrow(rootedTree$edge)) {
   if (H$find(rootedTree$edge[row, 1]) != H$find(rootedTree$edge[row, 2])) {
     # cat(H$find(rootedTree$edge[row, 1]), "===>", H$find(rootedTree$edge[row, 2]), "\n")
     sourceList <- c(sourceList, H$find(rootedTree$edge[row, 1]))
     targetList <- c(targetList, H$find(rootedTree$edge[row, 2]))
   }
 }

dat <- data.frame(x = sourceList, y = targetList)

unique <- unique(dat)

edgeCount <- count(dat)

weights <- c()

for(row in 1:nrow(unique)) {
  for(row2 in 1:nrow(edgeCount)) {
    if(unique[row, 1] == edgeCount[row2, 1] && unique[row, 2] == edgeCount[row2, 2]) {
      weights <- c(weights, edgeCount[row2, 3])
    }
  }
}


countries <- c("Jamaica", "El Salvador", "Honduras", "Nicaragua", "Mexico", "United States", "China", "Guatemala", "Brazil", "Puerto Rico",
 "French Guiana", "Suriname", "Venezuela", "Fiji", "Dominican Republic", "Singapore", "Italy", "Colombia", "Panama", "Martinique", "Peru",
 "Uganda", "Senegal", "Central African Republic", "Malaysia", "Micronesia", "Philipines", "South Korea", "Cambodia", "Thailand", "Japan",
 "French Polynesia", "American Samoa", "Samoa", "Ecuador", "Guadeloupe", "Haiti", "Cuba")

graph <- graph.data.frame(dat, directed=T)
graph <- simplify(graph)

V(graph)$outdegree <- centr_degree(graph, mode = "out")$res

nodes <- get.data.frame(graph, what="vertices")
nodes <- data.frame(id = nodes$name, title = countries[as.numeric(nodes$name)], value = nodes$outdegree,  label =countries[as.numeric(nodes$name)], group = nodes$outdegree, outdegree = nodes$outdegree)
setnames(nodes, "outdegree", "out-degree centrality")
nodes <- nodes[order(nodes$id, decreasing = F),]

edges <- data.frame(from = unique$x, to = unique$y, value = weights)
visNetwork(nodes, edges, height = "1500px", width = "1500px") %>%
visOptions(selectedBy = "out-degree centrality", highlightNearest = TRUE, nodesIdSelection = TRUE)%>%
visGroups(groupname = "11", color = list(background = "#990033", 
                        border = "#630021")) %>%
visGroups(groupname = "1", color = list(background = "#8FBC8F", 
                        border = "#658765")) %>%
visGroups(groupname = "0", color = list(background = "#0299bb", 
                        border = "#005f75")) %>% 
visGroups(groupname = "2", color = list(background = "#49536d", 
                        border = "#262c3a")) %>% 
visGroups(groupname = "4", color = list(background = "#008955", 
                        border = "#02472d")) %>%
visGroups(groupname = "5", color = list(background = "#C5B358", 
                        border = "#c4b154")) %>%
visGroups(groupname = "6", color = list(background = "#86adad", 
                        border = "#577272")) %>% 
visPhysics(stabilization = FALSE)%>% 
 visEdges(arrows = list(to = list(enabled = TRUE,
  scaleFactor = 0.5)))


observe({
  if(input$degree == "indegree") {
    output$network <- renderVisNetwork({
    graph <- graph.data.frame(dat, directed=T)
    graph <- simplify(graph)
    V(graph)$indegree <- centr_degree(graph, mode = "in")$res
    nodes <- get.data.frame(graph, what="vertices")
    nodes <- data.frame(id = nodes$name, title = countries[as.numeric(nodes$name)], value = nodes$indegree,  label =countries[as.numeric(nodes$name)], group = nodes$indegree, indegree = nodes$indegree)
    setnames(nodes, "indegree", "in-degree centrality")
    nodes <- nodes[order(nodes$id, decreasing = F),]
    visNetwork(nodes, edges, height = "1500px", width = "1500px") %>%
    visOptions(selectedBy = "in-degree centrality", highlightNearest = TRUE, nodesIdSelection = TRUE)%>%
    visGroups(groupname = "11", color = list(background = "#990033", 
                            border = "#630021")) %>%
    visGroups(groupname = "1", color = list(background = "#8FBC8F", 
                            border = "#658765")) %>%
    visGroups(groupname = "0", color = list(background = "#0299bb", 
                            border = "#005f75")) %>% 
    visGroups(groupname = "2", color = list(background = "#49536d", 
                            border = "#262c3a")) %>% 
    visGroups(groupname = "4", color = list(background = "#008955", 
                            border = "#02472d")) %>%
    visGroups(groupname = "5", color = list(background = "#C5B358", 
                            border = "#c4b154")) %>%
    visGroups(groupname = "6", color = list(background = "#86adad", 
                            border = "#577272")) %>% 
    visPhysics(stabilization = FALSE)%>% 
     visEdges(arrows = list(to = list(enabled = TRUE,
      scaleFactor = 0.5)))
     })
  }
  else {
    output$network <- renderVisNetwork({
    graph <- graph.data.frame(dat, directed=T)
    graph <- simplify(graph)
    V(graph)$outdegree <- centr_degree(graph, mode = "out")$res
    nodes <- get.data.frame(graph, what="vertices")
    nodes <- data.frame(id = nodes$name, title = countries[as.numeric(nodes$name)], value = nodes$outdegree,  label =countries[as.numeric(nodes$name)], group = nodes$outdegree, outdegree = nodes$outdegree)
    setnames(nodes, "outdegree", "out-degree centrality")
    nodes <- nodes[order(nodes$id, decreasing = F),]
    visNetwork(nodes, edges, height = "1500px", width = "1500px") %>%
    visOptions(selectedBy = "out-degree centrality", highlightNearest = TRUE, nodesIdSelection = TRUE)%>%
    visGroups(groupname = "11", color = list(background = "#990033", 
                            border = "#630021")) %>%
    visGroups(groupname = "1", color = list(background = "#8FBC8F", 
                            border = "#658765")) %>%
    visGroups(groupname = "0", color = list(background = "#0299bb", 
                            border = "#005f75")) %>% 
    visGroups(groupname = "2", color = list(background = "#49536d", 
                            border = "#262c3a")) %>% 
    visGroups(groupname = "4", color = list(background = "#008955", 
                            border = "#02472d")) %>%
    visGroups(groupname = "5", color = list(background = "#C5B358", 
                            border = "#c4b154")) %>%
    visGroups(groupname = "6", color = list(background = "#86adad", 
                            border = "#577272")) %>% 
    visPhysics(stabilization = FALSE)%>% 
     visEdges(arrows = list(to = list(enabled = TRUE,
      scaleFactor = 0.5)))
      })
  }
  
})
  })

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)