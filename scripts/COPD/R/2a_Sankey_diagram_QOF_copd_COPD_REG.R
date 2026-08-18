# extract COPD rules for Sankey diagram.


library(dplyr)
library(data.table)
library(bit64)
library(networkD3)
library(htmlwidgets)

#--------------- read in data with rules columns
dat <- fread("X:/QOF_project/Data/COPD/working_data/1_cohort_COPD_with_indicators_Sankey.csv")

# ensure they are in the integer format
dat <- dat %>% mutate(across(COPD_REG_RN1:COPD014_num, as.integer))

str(dat)
# created using the help of this web page: 
# https://medium.com/@emtiazahmed.cs/sankey-diagram-step-by-step-using-r-b3e7bea53224 




# #------------------ this just helps show how the rules have been created:
# 
# # R = reject
# # S = select
# # N = next rule
# 
# #---------------------------------# COPD_REG
# note COPD_REG was made in Stata but was this:
#
# dat <- dat %>% mutate(COPD_REG = ifelse(COPD_REG_RN1 == 1 |
#                                          COPD_REG_RN2 == 1 |
#                                          COPD_REG_RN3 == 1 |
#                                          COPD_REG_RN4 == 1, 1, 0))





#-----------creating the weights for the sankey diagram:

# we do a lot of filtering by 1 or by 0 followed by counting the rows
# we use the QOF document and the if_else statement above to help

GMS <- nrow(dat)

#Rule 1
COPD_REG_RN1_S <- dat %>% filter(COPD_REG_RN1 == 1) %>% nrow()
COPD_REG_RN1_N <- dat %>% filter(COPD_REG_RN1 == 0) %>% nrow()

#Rule 2
COPD_REG_RN2_S <- dat %>% filter(COPD_REG_RN1 == 0 & COPD_REG_RN2 == 1) %>% nrow()
COPD_REG_RN2_N <- dat %>% filter(COPD_REG_RN1 == 0 & COPD_REG_RN2 == 0) %>% nrow()

#Rule 3
COPD_REG_RN3_S <- dat %>% 
  filter(COPD_REG_RN1 == 0 & COPD_REG_RN2 == 0 & COPD_REG_RN3 == 1) %>% nrow()
COPD_REG_RN3_N <- dat %>% 
  filter(COPD_REG_RN1 == 0 & COPD_REG_RN2 == 0 & COPD_REG_RN3 == 0) %>% nrow()

#Rule 4
COPD_REG_RN4_S <- dat %>% 
  filter(COPD_REG_RN1 == 0 & COPD_REG_RN2 == 0 & COPD_REG_RN3 == 0 & COPD_REG_RN4 == 1) %>% nrow()
COPD_REG_RN4_R <- dat %>% 
  filter(COPD_REG_RN1 == 0 & COPD_REG_RN2 == 0 & COPD_REG_RN3 == 0 & COPD_REG_RN4 == 0) %>% nrow()



#---------------- check the weights
GMS
COPD_REG_RN1_S
COPD_REG_RN1_N
COPD_REG_RN2_S
COPD_REG_RN2_N
COPD_REG_RN3_S
COPD_REG_RN3_N
COPD_REG_RN4_S
COPD_REG_RN4_R

tabulate(dat$COPD_REG)

# if this is true then it has worked correctly:
# (as the numbers should all add up to those in the register)
GMS == sum(COPD_REG_RN1_S,
           COPD_REG_RN2_S,
           COPD_REG_RN3_S,
           COPD_REG_RN4_S,
           COPD_REG_RN4_R)



#------------------ create a dataframe with the nodes

nodes <- data.frame("name" = c("GMS (n = 2,750,220)", "Selected rule 1: Earliest unresolved COPD diagnosis was before 1 April 2023 (n = 42,092)",
                                "Selected rule 2: Earliest unresolved COPD diagnosis was on/after 1 April 2023 and FEV1/FVC < 0.7 within 3 months before and 6 months after diagnosis (n = 503)",
                                "Selected rule 3: Earliest unresolved COPD diagnosis was on/after 1 April 2023, 12 months of registration, FEV1/FVC ratio < 0.7 within 6 months after registration (n = 0)", 
                                "Selected rule 4: Earliest unresolved COPD diagnosis was on/after 1 April 2023 (n = 3,980)",
                                "Rejected rule 4: Reject remaining patients (n = 2,703,645)"),
                    "group" = c("GMS", "selected", "selected", "selected", "selected", 
                                "excluded"))

#------------ create the data frame showing how the nodes relate to each other

# this shows how the nodes relate to each other - for us it is straightforward as all from the register
links = as.data.frame(matrix(c(0, 1, COPD_REG_RN1_S, 
                               0, 2, COPD_REG_RN2_S,
                               0, 3, COPD_REG_RN3_S, 
                               0, 4, COPD_REG_RN4_S, 
                               0, 5,  COPD_REG_RN4_R),
                             byrow = TRUE, ncol = 3))
# this line provides column names to the newly created 'links' data frame
names(links) = c("source", "target", "value")



#---------------------- colour code the nodes according to their group:
colour_scale <- '
d3.scaleOrdinal()
  .domain(["selected","excluded","GMS"])
  .range(["purple","black","grey"])'


#--------------------- create the diagram itself
p <- sankeyNetwork(
  Links = links, Nodes = nodes,
  Source = "source", Target = "target",
  Value = "value", NodeID = "name",
  NodeGroup = "group",              # <-- IMPORTANT: column name (string)
  colourScale = colour_scale,         # <-- optional but useful
  fontSize = 16, nodeWidth = 40, width = 400, fontFamily = "calibri"
)

p   # unformatted graph



# the rest of the script i don't understand, it is chat GPT working some
# magic to get the formatting to work out

#-------- get the labels outside of the nodes on the righthand side
p <- onRender(p, '
function(el, x) {

  var svg = d3.select(el).select("svg");
  var g   = svg.select("g"); // sankey content group

  // 1) Allow text to render outside the widget viewport (no clipping)
  svg.style("overflow", "visible");
  d3.select(el).style("overflow", "visible");

  // Some versions add clipPaths; disable them
  svg.selectAll("clipPath").remove();

  // 2) Classify left vs right nodes based on x-position
  // (more reliable than is_source_node, which is not always present)
  var nodes = d3.select(el).selectAll(".node");
  var width = el.getBoundingClientRect().width;

  // 3) Move labels outside nodes
  d3.select(el).selectAll(".node text")
    .each(function(d) {
      var isLeft = d.x < width / 2;
      d3.select(this)
        .attr("x", isLeft ? -10 : (x.options.nodeWidth + 10))
        .attr("text-anchor", isLeft ? "end" : "start");
    });

  // 4) Optional: add a little padding by shifting the whole diagram right
  // (helps left-side labels not get clipped by the container edge)
  g.attr("transform", "translate(30,0)");
}
')



#---------- compress the graph a little

compress_factor <- 0.8  # smaller = shorter arms (try 0.7, 0.6, 0.5)

p <- onRender(p, sprintf('
function(el, x){

  var k = %f;
  var nodeWidth = x.options.nodeWidth;
  var curvature = 0.5;

  // 1) Compress the underlying node x positions (this affects link geometry too,
  //    because links reference the same node objects)
  d3.select(el).selectAll(".node")
    .each(function(d){
      d.x = d.x * k;
    })
    .attr("transform", function(d){ return "translate(" + d.x + "," + d.y + ")"; });

  // 2) Recompute link path "d" using networkD3-style cubic bezier
  function linkPath(d){
    var x0 = d.source.x + nodeWidth,
        x1 = d.target.x,
        xi = d3.interpolateNumber(x0, x1),
        x2 = xi(curvature),
        x3 = xi(1 - curvature),
        y0 = d.source.y + d.sy + d.dy / 2,
        y1 = d.target.y + d.ty + d.dy / 2;

    return "M" + x0 + "," + y0
         + "C" + x2 + "," + y0
         + " " + x3 + "," + y1
         + " " + x1 + "," + y1;
  }

  d3.select(el).selectAll(".link")
    .attr("d", linkPath);
}
', compress_factor))


#-----------------------
# allow it to interpret '\n' as a new line, and allow trailing whitespace

p <- onRender(p, '
function(el){

  d3.select(el).selectAll(".node text")
    .attr("xml:space", "preserve")
    .each(function(d){

      var text = d3.select(this);
      var lines = d.name.split("\\n");

      if(lines.length > 1){
        text.text(null);

        for (var i = 0; i < lines.length; i++) {
          text.append("tspan")
              .attr("x", 0)
              .attr("dy", i ? "1.2em" : 0)
              .text(lines[i]);
        }
      }
    });
}
')

#------------------- and the final graph is here!!! 
p

# done!!!!! 

# then you can fiddle about with the arms to make the spacing nice, and save it
# i just save it using point and click as I don't know how to code changes in the
# arms (which are interactive)


