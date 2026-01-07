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


#Shows six outcome measures comparing a Reference group to an Adjusted risk ratio group with 95% confidence intervals and percentages
#Uses a forest plot with a logarithmic x-axis, black-and-white styling and zebra striping


#Generate the four datasets
####Data sets####
#Data for table 1
Dataset1Category1 <- tibble::tibble(mean  = c(12.34, 7.89, 3.21, 15.67, 4.56, 9.01),
                                    lower = c(10.98, 6.54, 2.87, 14.22, 3.98, 7.45),
                                    upper = c(14.50, 9.12, 3.95, 17.89, 5.23, 10.67),
                                    study = c("Category 1", "Category 1", "Category 1", "Category 1", "Category 1", "Category 1"),
                                    odds = c( "12.34 (10.98-14.50)", "7.89 (6.54-9.12)", "3.21 (2.87-3.95)",
                                              "15.67 (14.22-17.89)", "4.56 (3.98-5.23)", "9.01 (7.45-10.67)"),
                                    numbers = c(1, 2, 3, 4, 5, 6),
                                    values = c(1, 2, 3, 4, 5, 6),
                                    percentages = c("10.4%", "34.2%", "9.4%", "1.4%", "40.2%", "6.3%"))
Dataset1Category2 <- tibble::tibble(study = c("Category 2", "Category 2", "Category 2", "Category 2", "Category 2", "Category 2"),
                                    odds = c("Reference", "Reference", "Reference", "Reference", "Reference", "Reference"),
                                    numbers = c(1, 2, 3, 4, 5, 6),
                                    values = c(1, 2, 3, 4, 5, 6),
                                    percentages = c("27.2%", "19.1%", "10.9%", "2.0%", "4.2%", "9.2%"))
Words1 <- tibble::tibble(study = c("Column 1",
                                   "Column 2",
                                   "Column 3",
                                   "Column 4",
                                   "Column 5",
                                   "Column 6"),
                         numbers = c (1, 2, 3, 4, 5, 6))

####Graphing code####
#Bind all dataframes together into one dataframe
total1 <- bind_rows(Words1, Dataset1Category1, Dataset1Category2)

#Arrange them by number
total1 <- total1 %>% arrange(numbers)

#Add space in the front of race names
total1$study <- ifelse(is.na(total1$values),
                       total1$study,
                       paste0("        ",total1$study))

#Add a column of consecutive numbers
total1$consecutive <- 1:nrow(total1) 


#### Graph ####
#Specify the colors for the forestplot
#This only does one color for the lines and another color for filling in the boxes
styles <- fpShapesGp(
  lines = rep(list(gpar(col = "black")), 20),
  box = rep(list(gpar(fill = "black")), 20)
)

#Specify the x ticks for the forestplot
my_xticks <- c(1, 2, 4, 8, 16, 26)
my_labels <- c("1", "2", "4", "8", "16", "26")
attr(my_xticks, "labels") <- my_labels

#Create the forestplot with a logarithmic scale 
total1 |>
  forestplot(
    labeltext = c(study, percentages, odds),
    mean,
    lower,
    upper,
    boxsize = 0.5,
    zero = 1,
    xlog = TRUE,
    is.summary = c(TRUE, rep(FALSE, 2),
                   TRUE, rep(FALSE, 2),
                   TRUE, rep(FALSE, 2),
                   TRUE, rep(FALSE, 2),
                   TRUE, rep(FALSE, 2),
                   TRUE, rep(FALSE, 2)),
    col = fpColors(lines = "black", zero = "black", vrtcl_lines = "black"),
    shapes_gp = styles,
    plotwidth = unit(25, "cm"),
    colgap = unit(8, "mm"),
    lineheight = unit(1.5, "lines"),
    xticks = my_xticks,
    cols = c(1:2, 3:4, 5:6, 7:8, 10:12, 14:15),
    txt_gp = fpTxtGp(
      label = gpar(cex = 1.4),
      ticks = gpar(cex = 1.2),
      title = gpar(cex = 1.5)
    )
  ) |>
  fp_add_header(
    study = c("", "Title 1"),
    percentages = c("", "Percentages"),
    odds = c("", "Adjusted risk ratio")
  ) |>
  fp_set_zebra_style("#EFEFEF")


#Create graph 2
