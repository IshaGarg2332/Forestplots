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


#Displays six categories, each comparing a Reference group to an Adjusted risk ratio group with percentages and 95% confidence intervals
#Uses a clean black-and-white forest plot style with indented group labels and zebra striping
#Combines numerical table data (percentages, adjusted risk ratios) with visual risk ratio markers and confidence interval lines for easy comparison


#Create data manually
##Data creation######
#Data for table 1
Dataset1Category1 <- tibble::tibble(mean  = c(14.37, 5.62, 12.45, 7.18, 19.83, 3.41),
                                    lower = c(12.84, 4.93, 10.25, 5.97, 18.02, 2.67),
                                    upper = c(15.92, 6.31, 14.67, 8.49, 21.45, 4.12),
                                    study = c("Name", "Name", "Name", "Name", "Name", "Name"),
                                    odds  = c("14.37 (12.84-15.92)", "5.62 (4.93-6.31)", "12.45 (10.25-14.67)",
                                              "7.18 (5.97-8.49)", "19.83 (18.02-21.45)", "3.41 (2.67-4.12)"),
                                    numbers = c(1, 2, 3, 4, 5, 6),
                                    values = c(1, 2, 3, 4, 5, 6),
                                    percentages = c("42.17%", "67.89%", "11.23%", "54.68%", "29.35%", "88.46%"))

Dataset1Category2 <- tibble::tibble(study = c("Other name", "Other name", "Other name", "Other name", "Other name", "Other name"),
                            odds=c("Reference", "Reference", "Reference", "Reference", "Reference", "Reference"),
                            numbers = c(1, 2, 3, 4, 5, 6),
                            values = c(1, 2, 3, 4, 5, 6),
                            percentages = c("42.37%", "15.82%", "98.44%", "63.21%", "27.65%", "54.19%"))

Words1 <- tibble::tibble(study = c("Category 1",
                                   "Category 2",
                                   "Category 3",
                                   "Category 4",
                                   "Category 5",
                                   "Category 6"),
                         numbers = c (1, 2, 3, 4, 5, 6))



####Graphing code####

#Bind all dataframes together into one dataframe
total1 <- bind_rows(Words1, Dataset1Category2, Dataset1Category1)

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
  lines = rep(list(gpar(col = "black")), 20),
  box = rep(list(gpar(fill = "black")), 20)
)



####Form the forestplot####

#Form the forestplot
total1 |>
  forestplot(
    labeltext = c(study, percentages, odds),
    mean,
    lower,
    upper,
    boxsize = 0.4,
    zero = 1,
    is.summary = c(TRUE, rep(FALSE, 2),
                   TRUE, rep(FALSE, 2),
                   TRUE, rep(FALSE, 2),
                   TRUE, rep(FALSE, 2),
                   TRUE, rep(FALSE, 2),
                   TRUE, rep(FALSE, 2)),
    col = fpColors(lines = "black", zero = "black", vrtcl_lines = "black"),
    shapes_gp = styles,
    plotwidth = unit(25, "cm"),
    colgap = unit(4, "mm"),
    lineheight = unit(1.4, "lines"),
    xticks = c(1, 22),
    cols = c(1:2, 3:4, 5:6, 7:8, 10:12, 14:15),
    txt_gp = fpTxtGp(
      label = gpar(cex = 1),
      ticks = gpar(cex = 0.85),
      title = gpar(cex = 1.5)
    )
  ) |>
  fp_add_header(
    study = c("", "Title"),
    percentages = c("", "Percentages"),
    odds = c("", "Adjusted risk ratio")
  ) |>
  fp_set_zebra_style("#EFEFEF")
