
# TODO --------------------------------------------------------------------

# DONE: Give access to the results table which can be downloaded. The regions should bease base on euclidean distance measurements

# DONE: Place leget outside the graph.

# DONE: Option to add a single point insted of the excel sheet

# DONE: These should also be a Id colum for the points selected

# Source files ------------------------------------------------------------


pkgs <- c("shiny", "tidyverse", "patchwork", "readxl")
missing_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs)
}
pkginst <- lapply(pkgs, library, character.only = TRUE)




if(!requireNamespace("PbIso")){
  devtools::install_github("shereearmistead/PbIso")
}
library(PbIso)
source("2stagemodle.R")
source("isocron.R")
# source("graphmaker/2stagemodle.R")
# source("graphmaker/isocron.R")


# Load Database -----------------------------------------------------------

data_base <- read_csv("../data/new_data.csv",
                      locale = locale(encoding = "ISO-8859-1"))


# Graph ------------------------------------------------------

graph <- function(df, 
                  isotope_ratios = c("pb64", "pb74", "pb84", "id"),
                  df2,
                  isotope_ratios2 = c("206Pb/204Pb", "207Pb/204Pb", "208Pb/204Pb"),
                  title = "Title", 
                  pch = 1, cex = 1, col = 1,
                  pch2 = 1, cex2 = 0.5, col2 = 1,
                  xlim = NULL, ylim74 = NULL, ylim84 = NULL,
                  iso_lines = TRUE,
                  legend_position = "bottomright",
                  legend = "Legend",
                  legend_col = "red",
                  legend_ncol = 4,
                  show_label = TRUE,
                  ...) {
  df <- df
  par(mfrow = c(2,1), oma = c(6, 0, 0, 0))
  # First Plot
  par(mar = c(0.05, 4.5, 4, 1))
  plot(
    df[[isotope_ratios[1]]],
    df[[isotope_ratios[2]]],
    xlab = NULL,
    ylab = expression(paste(
      phantom(x)^207, "Pb /", phantom(x)^204, "Pb"
    )),
    xaxt = "n",
    xaxp = c(17, 19, 10),
    type = "n",
    main = title, 
    xlim = xlim,
    ylim = ylim74
  )
  if(iso_lines){
    isocron76(...)
  }
  points(df2[[isotope_ratios2[1]]],
         df2[[isotope_ratios2[2]]],
         pch = pch2, cex = cex2, col = col2)
  points(df[[isotope_ratios[1]]],
         df[[isotope_ratios[2]]],
         pch = pch, cex = cex, col = col)
  if(show_label){
    text(df[[isotope_ratios[1]]] + 0.015,
         df[[isotope_ratios[2]]] + 0.015,
         labels = df[[isotope_ratios[4]]],
         pch = pch, cex = cex, col = col)
  }
  

  
  # Scond Plot
  par(mar = c(4, 4.5, 0.05, 1))
  plot(
    df[[isotope_ratios[1]]],
    df[[isotope_ratios[3]]],
    xlab = expression(paste(
      phantom(x)^206, "Pb /", phantom(x)^204, "Pb"
    )),
    ylab = expression(paste(
      phantom(x)^208, "Pb /", phantom(x)^204, "Pb"
    )),
    xaxt = "t",
    xaxp = c(17, 19, 10),
    type = "n",
    xlim = xlim,
    ylim = ylim84
  )
  if(iso_lines){
    isocron86(...)
  }
  points(df2[[isotope_ratios2[1]]],
         df2[[isotope_ratios2[3]]],
         pch = pch2, cex = cex2, col = col2)
  points(df[[isotope_ratios[1]]],
         df[[isotope_ratios[3]]], 
         pch = pch, cex = cex, col = col)
  if(show_label){
    text(df[[isotope_ratios[1]]] + 0.015,
         df[[isotope_ratios[3]]] + 0.015,
         labels = df[[isotope_ratios[4]]],
         pch = pch, cex = cex, col = col)
  }
  par(xpd = NA)
  legend(
    x = par("usr")[2], # Aligns with the right side of the plot
    y = par("usr")[3] - (diff(par("usr")[3:4]) * 0.25), # Moves it below the axis
    xjust = 1, # Right-justified
    legend = legend,
    pch = 16, 
    ncol = legend_ncol,
    col = legend_col,
    bty = "n"
  )
}

