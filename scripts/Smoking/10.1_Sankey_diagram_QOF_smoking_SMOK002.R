# extract smoking rules for Sankey diagram.


library(dplyr)
library(data.table)
library(bit64)
library(networkD3)
library(htmlwidgets)

#--------------- read in data with rules columns
dat <- readRDS("Z:/Group_work/QOF_project/Data/Smoking/cohort/3_cohort_smoking_with_rules.RDS")

# ensure they are in the integer format
dat <- dat %>% select(patid, starts_with("SMOK1_REG"), starts_with("SMOK2_REG"), starts_with("SMOK0"))
# dat <- dat %>% mutate(across(SMOK1_REG_RN1:SMOK005_numerator, as.integer))

str(dat)
# created using the help of this web page: 
# https://medium.com/@emtiazahmed.cs/sankey-diagram-step-by-step-using-r-b3e7bea53224 




# #------------------ this just helps show how the rules have been created:
# 
# # R = reject
# # S = select
# # N = next rule
# 
# #---------------------------------# SMOK002
# 
# dat <- dat %>% mutate(SMOK002_denom = if_else(SMOK1_REG == 1 & 
#                                                (SMOK002_denom_RN1 == 1 | (
#                                                  SMOK002_denom_RN2 == 0 &
#                                                    SMOK002_denom_RN3 == 0 &
#                                                    SMOK002_denom_RN4 == 0 &
#                                                    SMOK002_denom_RN5 == 0 &
#                                                    SMOK002_denom_RN6 == 0 &
#                                                    SMOK002_denom_RN7 == 0 &
#                                                    SMOK002_denom_RN8 == 0 &
#                                                    SMOK002_denom_RN9 == 0 &
#                                                    SMOK002_denom_RN10 == 0 &
#                                                    SMOK002_denom_RN11 == 0 &
#                                                    SMOK002_denom_RN12 == 0)), 
#                                              1, 0, missing = 0))



#-----------creating the weights for the sankey diagram:

# we do a lot of filtering by 1 or by 0 followed by counting the rows
# we use the QOF document and the if_else statement above to help

SMOK1_REG_N <- dat %>% filter(SMOK1_REG == 1) %>% nrow()

SMOK002_denom_RN1_N <- dat %>% filter(SMOK1_REG == 1 & SMOK002_denom_RN1 == 0) %>% nrow()

SMOK002_denom_RN2_N <- dat %>% filter(SMOK1_REG == 1 &     
                                      SMOK002_denom_RN1 == 1 & 
                                      SMOK002_denom_RN2 == 1) %>% nrow()

SMOK002_denom_RN3_N <- dat %>% filter(SMOK1_REG == 1 &     SMOK002_denom_RN1 == 1 & 
                                       SMOK002_denom_RN2 == 0 & 
                                       SMOK002_denom_RN3 == 1)  %>% nrow()


SMOK002_denom_RN4_N <- dat %>% filter(SMOK1_REG == 1 &     SMOK002_denom_RN1 == 1 &  
                                       SMOK002_denom_RN2 == 0 & 
                                       SMOK002_denom_RN3 == 0 & 
                                       SMOK002_denom_RN4 == 1)  %>% nrow()

SMOK002_denom_RN5_N <- dat %>% filter(SMOK1_REG == 1 &     
                                       SMOK002_denom_RN1 == 1 & 
                                       SMOK002_denom_RN2 == 0 & 
                                       SMOK002_denom_RN3 == 0 & 
                                       SMOK002_denom_RN4 == 0 & 
                                       SMOK002_denom_RN5 == 1)  %>% nrow()

SMOK002_denom_RN6_N <- dat %>% filter(SMOK1_REG == 1 &     
                                       SMOK002_denom_RN1 == 1 & 
                                       SMOK002_denom_RN2 == 0 & 
                                       SMOK002_denom_RN3 == 0 & 
                                       SMOK002_denom_RN4 == 0 & 
                                       SMOK002_denom_RN5 == 0 & 
                                       SMOK002_denom_RN6 == 1)  %>% nrow()

SMOK002_denom_RN7_N <- dat %>% filter(SMOK1_REG == 1 &    
                                        SMOK002_denom_RN1 == 1 &  
                                        SMOK002_denom_RN2 == 0 & 
                                        SMOK002_denom_RN3 == 0 & 
                                        SMOK002_denom_RN4 == 0 & 
                                        SMOK002_denom_RN5 == 0 & 
                                        SMOK002_denom_RN6 == 0 & 
                                        SMOK002_denom_RN7 == 1)  %>% nrow()

SMOK002_denom_RN8_N <- dat %>% filter(SMOK1_REG == 1 &     
                                        SMOK002_denom_RN1 == 1 & 
                                        SMOK002_denom_RN2 == 0 & 
                                        SMOK002_denom_RN3 == 0 & 
                                        SMOK002_denom_RN4 == 0 & 
                                        SMOK002_denom_RN5 == 0 & 
                                        SMOK002_denom_RN6 == 0 & 
                                        SMOK002_denom_RN7 == 0 & 
                                        SMOK002_denom_RN8 == 1)  %>% nrow()

SMOK002_denom_RN9_N <- dat %>% filter(SMOK1_REG == 1 &     
                                        SMOK002_denom_RN1 == 1 & 
                                        SMOK002_denom_RN2 == 0 & 
                                        SMOK002_denom_RN3 == 0 & 
                                        SMOK002_denom_RN4 == 0 & 
                                        SMOK002_denom_RN5 == 0 & 
                                        SMOK002_denom_RN6 == 0 & 
                                        SMOK002_denom_RN7 == 0 & 
                                        SMOK002_denom_RN8 == 0 & 
                                        SMOK002_denom_RN9 == 1)  %>% nrow()

SMOK002_denom_RN10_N <- dat %>% filter(SMOK1_REG == 1 &     
                                        SMOK002_denom_RN1 == 1 &  
                                        SMOK002_denom_RN2 == 0 & 
                                        SMOK002_denom_RN3 == 0 & 
                                        SMOK002_denom_RN4 == 0 & 
                                        SMOK002_denom_RN5 == 0 & 
                                        SMOK002_denom_RN6 == 0 & 
                                        SMOK002_denom_RN7 == 0 & 
                                        SMOK002_denom_RN8 == 0 & 
                                        SMOK002_denom_RN9 == 0 & 
                                        SMOK002_denom_RN10 == 1)  %>% nrow()

SMOK002_denom_RN11_N <- dat %>% filter(SMOK1_REG == 1 &     
                                        SMOK002_denom_RN1 == 1 & 
                                        SMOK002_denom_RN2 == 0 & 
                                        SMOK002_denom_RN3 == 0 & 
                                        SMOK002_denom_RN4 == 0 & 
                                        SMOK002_denom_RN5 == 0 & 
                                        SMOK002_denom_RN6 == 0 & 
                                        SMOK002_denom_RN7 == 0 & 
                                        SMOK002_denom_RN8 == 0 & 
                                        SMOK002_denom_RN9 == 0 & 
                                        SMOK002_denom_RN10 == 0 & 
                                        SMOK002_denom_RN11 == 1)  %>% nrow()

SMOK002_denom_RN12_rejected_N <- dat %>% filter(SMOK1_REG == 1 &     
                                                  SMOK002_denom_RN1 == 1 & 
                                                  SMOK002_denom_RN2 == 0 & 
                                                  SMOK002_denom_RN3 == 0 & 
                                                  SMOK002_denom_RN4 == 0 & 
                                                  SMOK002_denom_RN5 == 0 & 
                                                  SMOK002_denom_RN6 == 0 & 
                                                  SMOK002_denom_RN7 == 0 & 
                                                  SMOK002_denom_RN8 == 0 & 
                                                  SMOK002_denom_RN9 == 0 & 
                                                  SMOK002_denom_RN10 == 0 & 
                                                  SMOK002_denom_RN11 == 0 & 
                                                  SMOK002_denom_RN12 == 1)  %>% nrow()

SMOK002_denom_RN12_selected_N <- dat %>% filter(SMOK1_REG == 1 &     
                                                SMOK002_denom_RN1 == 1 & 
                                                SMOK002_denom_RN2 == 0 & 
                                                SMOK002_denom_RN3 == 0 & 
                                                SMOK002_denom_RN4 == 0 & 
                                                SMOK002_denom_RN5 == 0 & 
                                                SMOK002_denom_RN6 == 0 & 
                                                SMOK002_denom_RN7 == 0 & 
                                                SMOK002_denom_RN8 == 0 & 
                                                SMOK002_denom_RN9 == 0 & 
                                                SMOK002_denom_RN10 == 0 & 
                                                SMOK002_denom_RN11 == 0 & 
                                                SMOK002_denom_RN12 == 0)  %>% nrow()


#---------------- check the weights
SMOK1_REG_N
SMOK002_denom_RN1_N
SMOK002_denom_RN2_N
SMOK002_denom_RN3_N
SMOK002_denom_RN4_N
SMOK002_denom_RN5_N
SMOK002_denom_RN6_N
SMOK002_denom_RN7_N
SMOK002_denom_RN8_N
SMOK002_denom_RN9_N
SMOK002_denom_RN10_N
SMOK002_denom_RN11_N
SMOK002_denom_RN12_rejected_N
SMOK002_denom_RN12_selected_N

# if this is true then it has worked correctly:
# (as the numbers should all add up to those in the register)
SMOK1_REG_N == sum(SMOK002_denom_RN1_N,
                 SMOK002_denom_RN2_N,
                 SMOK002_denom_RN3_N,
                 SMOK002_denom_RN4_N,
                 SMOK002_denom_RN5_N,
                 SMOK002_denom_RN6_N,
                 SMOK002_denom_RN7_N,
                 SMOK002_denom_RN8_N,
                 SMOK002_denom_RN9_N,
                 SMOK002_denom_RN10_N,
                 SMOK002_denom_RN11_N,
                 SMOK002_denom_RN12_rejected_N,
                 SMOK002_denom_RN12_selected_N)



#------------------ create a dataframe with the nodes

# I use the rough names first for testing, and then create the clean names after

# nodes <- data.frame("name" = c("SMOK1_REG", "SMOK002_denom_RN1", "SMOK002_denom_RN2", "SMOK002_denom_RN3",
#                               "SMOK002_denom_RN4", "SMOK002_denom_RN5", "SMOK002_denom_RN6",
#                               "SMOK002_denom_RN7_rejected", "SMOK002_denom_RN7_selected"),
#                     "group" = c("register", "selected_numerator", "excluded",
#                  "excluded", "excluded", "excluded", "excluded", "excluded",
#                  "selected_denominator"))

# these are the cleaner names, with their group (divided into 4 options: 
#          register,  excluded, selected numerator, selected denominator

nodes <- data.frame("name" = c("Smoking \nregister ", 
                               # "Selected rule 1: Has a diagnosis of CHD, PAD, stroke or TIA, hypertension, diabetes, COPD, CKD, asthma, schizophrenia, bipolar affective disorder or other psychoses",
                               "",
                               "Selected rule 2: Has current smoking status",
                               "Selected rule 3: Patients >25 who have never smoked",
                               "Selected rule 4: Patients <25 who have never smoked",
                               "Selected rule 5: Patients who are ex-smokers",
                               "Selected rule 6: Patients w/ no 'current smoker' code but have 3 consecutive years of 'ex-smoker' status",
                               "Rejected rule 7: Unsuitable for QI care",
                               "Rejected rule 8: Chose not to disclose smoking status",
                               "Rejected rule 9: Declined QI care",
                               "Rejected rule 10: Did not respond to at least 2 smoking care review invitations",
                               "Rejected rule 11: Recently diagnosed with any of above conditions",
                               "Rejected rule 12: Registered at practice <3 months",
                               "Selected rule 12: Remainder in denominator"),
                    "group" = c("register", 
                                "selected_numerator",
                                "selected_numerator", "selected_numerator", "selected_numerator",
                                "selected_numerator", "selected_numerator", "excluded", "excluded", "excluded",
                                "excluded", "excluded", "excluded", "selected_denominator"))

#------------ create the data frame showing how the nodes relate to each other

# this shows how the nodes relate to each other - for us it is straightforward as all from the register
links = as.data.frame(matrix(c(
                               0, 1, SMOK002_denom_RN1_N,
                               0, 2, SMOK002_denom_RN2_N, # selected for appropriately controlled BP clinical measurement
                               0, 3, SMOK002_denom_RN3_N, # node 1 -> node 7 with weight 3
                               0, 4, SMOK002_denom_RN4_N,
                               0, 5, SMOK002_denom_RN5_N,
                               0, 6, SMOK002_denom_RN6_N,
                               0, 7, SMOK002_denom_RN7_N,
                               0, 8, SMOK002_denom_RN8_N,
                               0, 9, SMOK002_denom_RN9_N,
                               0, 10, SMOK002_denom_RN10_N,
                               0, 11, SMOK002_denom_RN11_N,
                               0, 12, SMOK002_denom_RN12_rejected_N,
                               0, 13, SMOK002_denom_RN12_selected_N),
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

compress_factor <- 0.9  # smaller = shorter arms (try 0.7, 0.6, 0.5)

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
