# --------------------------------------------------
# NOTE: This script requires private datasets that are not included due to confidentiality. 
# Replace the paths with your own files structured similarly to the expected data frames.
# --------------------------------------------------

#Import libraries
library(grid)
library(checkmate)
library(abind)
library(forestplot)
library(dplyr)
library(plotly)


#Displays adjusted odds ratios and 95% confidence intervals for six groups, each with a designated reference category
#Combines numeric results with forest plot visuals, including a reference line at 1
#Uses zebra striping and spacing to enhance readability and make comparisons clearer


#Create data manually
Dataset1 <- tibble::tibble(
  mean  = c(0.011, 0.69, 0.88, 1.65, 0.80, 1.12, 0.78, 1.06, 1.05, 1.07, 0.92, 0.66, 0.85),
  lower = c(0.009, 0.65, 0.82, 1.44, 0.66, 1.06, 0.71, 1.01, 1.01, 1.00, 0.88, 0.63, 0.72),
  upper = c(0.013, 0.73, 0.95, 1.87, 0.94, 1.18, 0.84, 1.12, 1.11, 1.12, 0.96, 0.69, 0.98),
  study = c("Data1", "Data2", "Data3", "Data4", "Data5", "Data6",
            "Data7", "Data8", "Data9", "Data10", "Data11", "Data12", "Data13"),
  numbers = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
  values = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
)

Reference1 <- tibble::tibble(
  study = c("Reference1", "Reference2", "Reference3", "Reference4", "Reference5", "Reference6"),
  odds = c("Reference", "Reference", "Reference", "Reference", "Reference", "Reference"),
  numbers = c(1, 2, 7, 9, 10, 13),
  values = c(1, 2, 7, 9, 10, 13)
)

Words1 <- tibble::tibble(
  study = c("Column 1",
            "Column 2",
            "Column 3",
            "Column 4",
            "Column 5",
            "Column 6"),
  numbers = c(1, 2, 7, 9, 10, 13)
)

#Add in the odds column without doing it manually
Dataset1$odds <- sprintf("%.2f (%.2f–%.2f)", Dataset1$mean, Dataset1$lower, Dataset1$upper)

#Change the odds of a column manually to change decimal places
Dataset1[1,7] <- "0.751 (0.691-0.822)"

####Graphing code####

#Bind all dataframes together into one dataframe
total1 <- bind_rows(Words1, Reference1, Dataset1)

#Arrange them by number
total1 <- total1 %>% arrange(numbers)

#Add space in the front of race names
total1$study <- ifelse(is.na(total1$values),
                       total1$study,
                       paste0("        ",total1$study))

#Add a column of consecutive numbers
total1$consecutive <- 1:nrow(total1) 

#Specify the colors for the forestplot
#This only does one color for the lines and another color for filling in the boxes
styles <- fpShapesGp(
  lines = rep(list(gpar(col = "black")), 27),
  box = rep(list(gpar(fill = "black")), 27)
)


####Final plot####

#Form the forestplot
total1 |>
  forestplot(
    labeltext = c(study, odds),
    mean,
    lower,
    upper,
    graph.pos = "right",
    boxsize = 0.4,
    zero = 1,
    is.summary = c(TRUE, rep(FALSE, 2),
                   TRUE, rep(FALSE, 6),
                   TRUE, rep(FALSE, 3),
                   TRUE, rep(FALSE, 2),
                   TRUE, rep(FALSE, 4),
                   TRUE, rep(FALSE, 3)),
    col = fpColors(lines = "black", zero = "black", vrtcl_lines = "black"),
    shapes_gp = styles,
    plotwidth = unit(25, "cm"),
    colgap = unit(12, "mm"),
    lineheight = unit(1.4, "lines"),
    xticks = c(0, 1, 2),
    cols = c(1:2, 3:4, 5:6, 7:8, 10:12, 14:15),
    txt_gp = fpTxtGp(
      label = gpar(cex = 1),
      ticks = gpar(cex = 0.85),
      title = gpar(cex = 1.5)
    )
  ) |>
  fp_add_header(
    study = c("", " "),
    odds = c("", "Adjusted odds ratio")
  ) |>
  fp_set_zebra_style("#EFEFEF")
