#Infrastructure to produce CONSORT diagram
#requires DiagrammeR (older version 0.9.2), DiagrammeRsvg, and rsvg packages
generate_consort <- function(data){
  # Setup --------------------------------------------------------------
  #create the node data frame
  ndf <-
    create_node_df(
      n = 14,
      label = c(data, 
                rep("", 7)),
      style = c(rep("solid", 7), 
                "invis", "solid", "invis", "solid", "invis", "solid", "invis"),
      shape = c(rep("box", 7),
                rep("point", 7)),
      width = c(rep(2.5, 7), 
                rep(0.001, 7)),
      hight = c(rep(0.5, 7), 
                rep(0.001, 7)),
      fontsize = c(rep(10, 14)),
      fontname = c(rep('Calibri', 14)),
      penwidth = 2.0,
      fixedsize = "true")
  
  edf <-
    create_edge_df(
      arrowhead = c("none", "vee", "none", "vee", "none", "vee", rep("none", 6), #columns
                    "none", "vee", "none", "vee", "none", "vee", "none"), #rows
      color = c("black", "black", "black", "black", "black", "black", rep("#00000000", 6), #columns
                "#00000000", "black", "#00000000", "black", "#00000000", "black", "#00000000"), #rows
      constraint = c("true", "true", "true", "true", "true", "true", rep("true", 6), #columns
                     rep("false", 7)), #rows
      from = c(1, 9, 3, 11, 5, 13, # column 1
               8, 2, 10, 4, 12, 6, # column 2
               1, # row 1
               9, # row 2
               3, # row 3
               11, # row 4
               5, # row 5
               13, # row 6
               7), # row 7
      to = c(9, 3, 11, 5, 13, 7, # column 1
             2, 10, 4, 12, 6, 14, # column 2
             8, # row 1
             2, # row 2
             10, # row 3
             4, # row 4
             12, # row 5
             6, # row 6
             14)) # row 7
  
  #create edges frame in different way
  #lists id, from, to, arrowhead, color, constraint (horizontal(false) or vertical(true)?)
  # Create Graph ------------------------------------------------------------
  g <- create_graph(ndf, 
                    edf,
                    attr_theme = NULL)
  
  # Plotting ----------------------------------------------------------------
  render_graph(g)
}