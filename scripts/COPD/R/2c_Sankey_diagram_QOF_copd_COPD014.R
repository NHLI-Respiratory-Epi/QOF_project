# extract COPD rules for Sankey diagram.


library(dplyr)
library(data.table)
library(bit64)
library(networkD3)
library(htmlwidgets)

library(tableone)

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
# #---------------------------------# COPD014
# note COPD_014_denom was made in Stata
#
# gen COPD014_denom=1 if COPD_REG==1 & COPD014_RN1==1 & COPD014_RN2==0 & COPD014_RN3==1 
# replace COPD014_denom=1 if COPD_REG==1 & COPD014_RN1==1 & COPD014_RN2==0 & COPD014_RN3==0 &  
#                            COPD014_RN4==0 & COPD014_RN5==0 & COPD014_RN6==0 & COPD014_RN7==0 & 
#                            COPD014_RN8==0 & COPD014_RN9==0 & COPD014_RN10==0 & COPD014_RN11==0




#-----------creating the weights for the sankey diagram:

# we do a lot of filtering by 1 or by 0 followed by counting the rows
# we use the QOF document and the if_else statement above to help

COPD_REG_N <- dat %>% filter(COPD_REG == 1) %>% nrow()

#Rule 1
COPD014_denom_RN1_N <- dat %>% filter(COPD_REG == 1 & COPD014_RN1 == 1) %>% nrow()
COPD014_denom_RN1_R <- dat %>% filter(COPD_REG == 1 & COPD014_RN1 == 0) %>% nrow()

#Rule 2
COPD014_denom_RN2_R <- dat %>% filter(COPD_REG == 1 & COPD014_RN1 == 1 &
                                        COPD014_RN2 == 1) %>% nrow()
COPD014_denom_RN2_N <- dat %>% filter(COPD_REG == 1 & COPD014_RN1 == 1 &
                                        COPD014_RN2 == 0) %>% nrow()

#Rule 3
COPD014_denom_RN3_S <- dat %>% filter(COPD_REG == 1 & COPD014_RN1 == 1 &
                                        COPD014_RN2 == 0 &
                                        COPD014_RN3 == 1) %>% nrow()
COPD014_denom_RN3_N <- dat %>% filter(COPD_REG == 1 & COPD014_RN1 == 1 &
                                        COPD014_RN2 == 0 &
                                        COPD014_RN3 == 0) %>% nrow()

#Rule 4
COPD014_denom_RN4_R <- dat %>% filter(COPD_REG == 1 & COPD014_RN1 == 1 &
                                        COPD014_RN2 == 0 &
                                        COPD014_RN3 == 0 &
                                        COPD014_RN4 == 1) %>% nrow()
COPD014_denom_RN4_N <- dat %>% filter(COPD_REG == 1 & COPD014_RN1 == 1 &
                                        COPD014_RN2 == 0 &
                                        COPD014_RN3 == 0 &
                                        COPD014_RN4 == 0) %>% nrow()

#Rule 5
COPD014_denom_RN5_R <- dat %>% filter(COPD_REG == 1 & COPD014_RN1 == 1 &
                                        COPD014_RN2 == 0 &
                                        COPD014_RN3 == 0 &
                                        COPD014_RN4 == 0 &
                                        COPD014_RN5 == 1) %>% nrow()
COPD014_denom_RN5_N <- dat %>% filter(COPD_REG == 1 & COPD014_RN1 == 1 &
                                        COPD014_RN2 == 0 &
                                        COPD014_RN3 == 0 &
                                        COPD014_RN4 == 0 &
                                        COPD014_RN5 == 0) %>% nrow()

#Rule 6
COPD014_denom_RN6_R <- dat %>% filter(COPD_REG == 1 & COPD014_RN1 == 1 &
                                        COPD014_RN2 == 0 &
                                        COPD014_RN3 == 0 &
                                        COPD014_RN4 == 0 &
                                        COPD014_RN5 == 0 &
                                        COPD014_RN6 == 1) %>% nrow()
COPD014_denom_RN6_N <- dat %>% filter(COPD_REG == 1 & COPD014_RN1 == 1 &
                                        COPD014_RN2 == 0 &
                                        COPD014_RN3 == 0 &
                                        COPD014_RN4 == 0 &
                                        COPD014_RN5 == 0 &
                                        COPD014_RN6 == 0) %>% nrow()

#Rule 7
COPD014_denom_RN7_R <- dat %>% filter(COPD_REG == 1 & COPD014_RN1 == 1 &
                                        COPD014_RN2 == 0 &
                                        COPD014_RN3 == 0 &
                                        COPD014_RN4 == 0 &
                                        COPD014_RN5 == 0 &
                                        COPD014_RN6 == 0 &
                                        COPD014_RN7 == 1) %>% nrow()
COPD014_denom_RN7_N <- dat %>% filter(COPD_REG == 1 & COPD014_RN1 == 1 &
                                        COPD014_RN2 == 0 &
                                        COPD014_RN3 == 0 &
                                        COPD014_RN4 == 0 &
                                        COPD014_RN5 == 0 &
                                        COPD014_RN6 == 0 &
                                        COPD014_RN7 == 0) %>% nrow()

#Rule 8
COPD014_denom_RN8_R <- dat %>% filter(COPD_REG == 1 & COPD014_RN1 == 1 &
                                        COPD014_RN2 == 0 &
                                        COPD014_RN3 == 0 &
                                        COPD014_RN4 == 0 &
                                        COPD014_RN5 == 0 &
                                        COPD014_RN6 == 0 &
                                        COPD014_RN7 == 0 &
                                        COPD014_RN8 == 1) %>% nrow()
COPD014_denom_RN8_N <- dat %>% filter(COPD_REG == 1 & COPD014_RN1 == 1 &
                                        COPD014_RN2 == 0 &
                                        COPD014_RN3 == 0 &
                                        COPD014_RN4 == 0 &
                                        COPD014_RN5 == 0 &
                                        COPD014_RN6 == 0 &
                                        COPD014_RN7 == 0 &
                                        COPD014_RN8 == 0) %>% nrow()

#Rule 9
COPD014_denom_RN9_R <- dat %>% filter(COPD_REG == 1 & COPD014_RN1 == 1 &
                                        COPD014_RN2 == 0 &
                                        COPD014_RN3 == 0 &
                                        COPD014_RN4 == 0 &
                                        COPD014_RN5 == 0 &
                                        COPD014_RN6 == 0 &
                                        COPD014_RN7 == 0 &
                                        COPD014_RN8 == 0 &
                                        COPD014_RN9 == 1) %>% nrow()
COPD014_denom_RN9_N <- dat %>% filter(COPD_REG == 1 & COPD014_RN1 == 1 &
                                        COPD014_RN2 == 0 &
                                        COPD014_RN3 == 0 &
                                        COPD014_RN4 == 0 &
                                        COPD014_RN5 == 0 &
                                        COPD014_RN6 == 0 &
                                        COPD014_RN7 == 0 &
                                        COPD014_RN8 == 0 &
                                        COPD014_RN9 == 0) %>% nrow()

#Rule 10
COPD014_denom_RN10_R <- dat %>% filter(COPD_REG == 1 & COPD014_RN1 == 1 &
                                        COPD014_RN2 == 0 &
                                        COPD014_RN3 == 0 &
                                        COPD014_RN4 == 0 &
                                        COPD014_RN5 == 0 &
                                        COPD014_RN6 == 0 &
                                        COPD014_RN7 == 0 &
                                        COPD014_RN8 == 0 &
                                        COPD014_RN9 == 0 &
                                        COPD014_RN10 == 1) %>% nrow()
COPD014_denom_RN10_N <- dat %>% filter(COPD_REG == 1 & COPD014_RN1 == 1 &
                                        COPD014_RN2 == 0 &
                                        COPD014_RN3 == 0 &
                                        COPD014_RN4 == 0 &
                                        COPD014_RN5 == 0 &
                                        COPD014_RN6 == 0 &
                                        COPD014_RN7 == 0 &
                                        COPD014_RN8 == 0 &
                                        COPD014_RN9 == 0 &
                                        COPD014_RN10 == 0) %>% nrow()

#Rule 11
COPD014_denom_RN11_R <- dat %>% filter(COPD_REG == 1 & COPD014_RN1 == 1 &
                                         COPD014_RN2 == 0 &
                                         COPD014_RN3 == 0 &
                                         COPD014_RN4 == 0 &
                                         COPD014_RN5 == 0 &
                                         COPD014_RN6 == 0 &
                                         COPD014_RN7 == 0 &
                                         COPD014_RN8 == 0 &
                                         COPD014_RN9 == 0 &
                                         COPD014_RN10 == 0 &
                                         COPD014_RN11 == 1) %>% nrow()
COPD014_denom_RN11_S <- dat %>% filter(COPD_REG == 1 & COPD014_RN1 == 1 &
                                         COPD014_RN2 == 0 &
                                         COPD014_RN3 == 0 &
                                         COPD014_RN4 == 0 &
                                         COPD014_RN5 == 0 &
                                         COPD014_RN6 == 0 &
                                         COPD014_RN7 == 0 &
                                         COPD014_RN8 == 0 &
                                         COPD014_RN9 == 0 &
                                         COPD014_RN10 == 0 &
                                         COPD014_RN11 == 0) %>% nrow()





#---------------- check the weights
COPD_REG_N
COPD014_denom_RN1_N
COPD014_denom_RN1_R
COPD014_denom_RN2_R
COPD014_denom_RN2_N
COPD014_denom_RN3_S
COPD014_denom_RN3_N
COPD014_denom_RN4_R
COPD014_denom_RN4_N
COPD014_denom_RN5_R
COPD014_denom_RN5_N
COPD014_denom_RN6_R
COPD014_denom_RN6_N
COPD014_denom_RN7_R
COPD014_denom_RN7_N
COPD014_denom_RN8_R
COPD014_denom_RN8_N
COPD014_denom_RN9_R
COPD014_denom_RN9_N
COPD014_denom_RN10_R
COPD014_denom_RN10_N
COPD014_denom_RN11_R
COPD014_denom_RN11_S

# if this is true then it has worked correctly:
# (as the numbers should all add up to those in the register)
COPD_REG_N == sum(COPD014_denom_RN1_R,
                  COPD014_denom_RN2_R,
                  COPD014_denom_RN3_S,
                  COPD014_denom_RN4_R,
                  COPD014_denom_RN5_R,
                  COPD014_denom_RN6_R,
                  COPD014_denom_RN7_R,
                  COPD014_denom_RN8_R,
                  COPD014_denom_RN9_R,
                  COPD014_denom_RN10_R,
                  COPD014_denom_RN11_R,
                  COPD014_denom_RN11_S)



#------------------ create a dataframe with the nodes

nodes <- data.frame("name" = c("COPD \nregister \n(n = 46,575) ", "Rejected rule 1: No MRC Score >= 3 in preceding 12 months (n = 29,169)",
                               "Rejected rule 2: Attended PR between diagnosis date and MRC Score >= 3 date (n = 4,734)",
                               "Selected rule 3: Referred to PR on/after MRC Score >= 3 date (n = 3,769)",
                               "Rejected rule 4: COPD service or PR service was unavailable (n = 99)",
                               "Rejected rule 5: Unsuitable for PR (n = 2,133)",
                               "Rejected rule 6: Unsuitable for QI care (n = 64)",
                               "Rejected rule 7: Declined PR service referral (n = 5,423)",
                               "Rejected rule 8: Declined QI care (n = 49)",
                               "Rejected rule 9: Did not respond to COPD care review invitations (n = 878)",
                               "Rejected rule 10: Newly diagnosed COPD <3 months (n = 27)",
                               "Rejected rule 11: Registered at practice <3 months (n = 14)",
                               "Selected rule 11: Remainder in denominator (n = 216)"),
                    "group" = c("register", "excluded", "excluded", "selected_numerator", "excluded",
                                "excluded", "excluded", "excluded", "excluded", "excluded", "excluded",
                                "excluded", "selected_denominator"))

#------------ create the data frame showing how the nodes relate to each other

# this shows how the nodes relate to each other - for us it is straightforward as all from the register
links = as.data.frame(matrix(c(0, 1, COPD014_denom_RN1_R,
                               0, 2, COPD014_denom_RN2_R,
                               0, 3, COPD014_denom_RN3_S,
                               0, 4, COPD014_denom_RN4_R,
                               0, 5, COPD014_denom_RN5_R,
                               0, 6, COPD014_denom_RN6_R,
                               0, 7, COPD014_denom_RN7_R,
                               0, 8, COPD014_denom_RN8_R,
                               0, 9, COPD014_denom_RN9_R,
                               0, 10, COPD014_denom_RN10_R,
                               0, 11, COPD014_denom_RN11_R,
                               0, 12, COPD014_denom_RN11_S),
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


