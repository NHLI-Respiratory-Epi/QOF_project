# extract asthma rules for Sankey diagram.


library(dplyr)
library(data.table)
library(bit64)
library(networkD3)
library(htmlwidgets)

#--------------- read in data with rules columns
dat <- readRDS("Z:/Group_work/QOF_project/Data/Asthma/cohort/3_cohort_asthma_with_AST_indicators.RDS")

# ensure they are in the integer format
dat <- dat %>% select(patid, starts_with("AST_REG"), starts_with("AST0"))
dat <- dat %>% mutate(across(AST_REG_RN1:AST012_nume, as.integer))

str(dat)
# created using the help of this web page: 
# https://medium.com/@emtiazahmed.cs/sankey-diagram-step-by-step-using-r-b3e7bea53224 





# #------------------ this just helps show how the rules have been created:
# 
# # R = reject
# # S = select
# # N = next rule
# 
# #---------------------------------# AST007
# 
# dat <- dat %>% mutate(AST007_denom = if_else(AST_REG == 1 & 
#                                                (AST007_denom_RN1 == 1 | (
#                                                  AST007_denom_RN2 == 0 &
#                                                    AST007_denom_RN3 == 0 &
#                                                    AST007_denom_RN4 == 0 &
#                                                    AST007_denom_RN5 == 0 &
#                                                    AST007_denom_RN6 == 0 &
#                                                    AST007_denom_RN7 == 0)), 
#                                              1, 0, missing = 0))



#-----------creating the weights for the sankey diagram:

# we do a lot of filtering by 1 or by 0 followed by counting the rows
# we use the QOF document and the if_else statement above to help

AST_REG_N <- dat %>% filter(AST_REG == 1) %>% nrow()

AST007_denom_RN1_N <- dat %>% filter(AST_REG == 1 & AST007_denom_RN1 == 1) %>% nrow()

AST007_denom_RN2_N <- dat %>% filter(AST_REG == 1 &     AST007_denom_RN1 == 0 & 
                                       AST007_denom_RN2 == 1) %>% nrow()

AST007_denom_RN3_N <- dat %>% filter(AST_REG == 1 &     AST007_denom_RN1 == 0 & 
                                       AST007_denom_RN2 == 0 & 
                                       AST007_denom_RN3 == 1 )  %>% nrow()


AST007_denom_RN4_N <- dat %>% filter(AST_REG == 1 &     AST007_denom_RN1 == 0 &  
                                       AST007_denom_RN2 == 0 & 
                                       AST007_denom_RN3 == 0 & 
                                       AST007_denom_RN4 == 1)  %>% nrow()

AST007_denom_RN5_N <- dat %>% filter(AST_REG == 1 &     
                                       AST007_denom_RN1 == 0 & 
                                       AST007_denom_RN2 == 0 & 
                                       AST007_denom_RN3 == 0 & 
                                       AST007_denom_RN4 == 0 & 
                                       AST007_denom_RN5 == 1)  %>% nrow()

AST007_denom_RN6_N <- dat %>% filter(AST_REG == 1 &     
                                       AST007_denom_RN1 == 0 & 
                                       AST007_denom_RN2 == 0 & 
                                       AST007_denom_RN3 == 0 & 
                                       AST007_denom_RN4 == 0 & 
                                       AST007_denom_RN5 == 0 & 
                                       AST007_denom_RN6 == 1)  %>% nrow()

AST007_denom_RN7_rejected_N <- dat %>% filter(AST_REG == 1 &     
                                       AST007_denom_RN1 == 0 & 
                                       AST007_denom_RN2 == 0 & 
                                       AST007_denom_RN3 == 0 & 
                                       AST007_denom_RN4 == 0 & 
                                       AST007_denom_RN5 == 0 & 
                                       AST007_denom_RN6 == 0 & 
                                       AST007_denom_RN7 == 1)  %>% nrow()

AST007_denom_RN7_selected_N <- dat %>% filter(AST_REG == 1 &     
                                                AST007_denom_RN1 == 0 & 
                                                AST007_denom_RN2 == 0 & 
                                                AST007_denom_RN3 == 0 & 
                                                AST007_denom_RN4 == 0 & 
                                                AST007_denom_RN5 == 0 & 
                                                AST007_denom_RN6 == 0 & 
                                                AST007_denom_RN7 == 0)  %>% nrow()


#---------------- check the weights
AST_REG_N
AST007_denom_RN1_N
AST007_denom_RN2_N
AST007_denom_RN3_N
AST007_denom_RN4_N
AST007_denom_RN5_N
AST007_denom_RN6_N
AST007_denom_RN7_rejected_N
AST007_denom_RN7_selected_N

# if this is true then it has worked correctly:
# (as the numbers should all add up to those in the register)
AST_REG_N == sum(AST007_denom_RN1_N,
                 AST007_denom_RN2_N,
                 AST007_denom_RN3_N,
                 AST007_denom_RN4_N,
                 AST007_denom_RN5_N,
                 AST007_denom_RN6_N,
                 AST007_denom_RN7_rejected_N,
                 AST007_denom_RN7_selected_N)



#------------------ create a dataframe with the nodes

# I use the rough names first for testing, and then create the clean names after
 
# nodes <- data.frame("name" = c("AST_REG", "AST007_denom_RN1", "AST007_denom_RN2", "AST007_denom_RN3",
#                               "AST007_denom_RN4", "AST007_denom_RN5", "AST007_denom_RN6",
#                               "AST007_denom_RN7_rejected", "AST007_denom_RN7_selected"),
#                     "group" = c("register", "selected_numerator", "excluded",
#                  "excluded", "excluded", "excluded", "excluded", "excluded",
#                  "selected_denominator"))

# these are the cleaner names, with their group (divided into 4 options: 
#          register,  excluded, selected numerator, selected denominator

nodes <- data.frame("name" = c("Asthma \nregister \n(n = 156404) ", 
                               "Selected rule 1: Asthma review with plan and record of exacerbations (n = 98958)",
                               "Rejected rule 2: Unsuitable for QI care (n = 479)",
                               "Rejected rule 3: Declined asthma monitoring care (n = 229)",
                               "Rejected rule 4: Declined QI care (n = 2147)",
                               "Rejected rule 5: Invited for monitoring (n = 30695)",
                               "Rejected rule 6: Newly diagnosed asthma <3 months (n = 827)",
                               "Rejected rule 7: Registered at practice <3 months (n = 591)",
                               "Selected rule 7: Remainder in denominator (n = 22478)"),
                    "group" = c("register", "selected_numerator", "excluded",
                                "excluded", "excluded", "excluded", "excluded", "excluded",
                                "selected_denominator"))

#------------ create the data frame showing how the nodes relate to each other

# this shows how the nodes relate to each other - for us it is straightforward as all from the register
links = as.data.frame(matrix(c(0, 1, AST007_denom_RN1_N, # rejected for inappropriate age
                               0, 2, AST007_denom_RN2_N, # selected for appropriately controlled BP clinical measurement
                               0, 3, AST007_denom_RN3_N, # node 1 -> node 7 with weight 3
                               0, 4, AST007_denom_RN4_N,
                               0, 5, AST007_denom_RN5_N,
                               0, 6, AST007_denom_RN6_N,
                               0, 7, AST007_denom_RN7_rejected_N,
                               0, 8, AST007_denom_RN7_selected_N),
                             byrow = TRUE, ncol = 3))
# this line provides column names to the newly created 'links' data frame
names(links) = c("source", "target", "value")



#---------------------- colour code the nodes according to their group:
colour_scale <- '
d3.scaleOrdinal()
  .domain(["register","excluded","selected_numerator","selected_denominator"])
  .range(["purple","black","green","white"])'


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


