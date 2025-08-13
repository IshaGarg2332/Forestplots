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


#Makes four sets of data showing numbers, ranges, and labels for different categories
#Uses these data sets to create four similar forest plots with black boxes and lines, showing risk ratios on a log scale


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

#Data for table 2
Dataset2Category1 <- tibble::tibble(mean  = c(9.42, 5.83, 12.15, 7.67, 3.56, 14.89, 3.1),
                                    lower = c(8.21, 4.90, 10.98, 6.12, 2.88, 13.47, 2.8),
                                    upper = c(10.63, 6.75, 13.62, 9.43, 4.21, 16.38, 3.6),
                                    study = c("Category 1", "Category 1", "Category 1", "Category 1", "Category 1", "Category 1", "Category 1"),
                                    odds = c("9.42 (8.21-10.63)", "5.83 (4.90-6.75)", "12.15 (10.98-13.62)",
                                             "7.67 (6.12-9.43)", "3.56 (2.88-4.21)", "14.89 (13.47-16.38)",
                                             "3.10 (2.80-3.60)"),
                                    numbers = c(1, 2, 3, 4, 5, 6, 7),
                                    values = c(1, 2, 3, 4, 5, 6, 7),
                                    percentages = c("32.8%", "19.4%", "73.2%", "44.8%", "23.9%", "82.1%", "0.4%"))
Dataset2Category2 <- tibble::tibble(study = c("Category 2", "Category 2", "Category 2", 
                                              "Category 2", "Category 2", "Category 2", 
                                              "Category 2"),
                                    odds = c("Reference", "Reference", "Reference", "Reference", "Reference", "Reference", "Reference"),
                                    numbers = c(1, 2, 3, 4, 5, 6, 7),
                                    values = c(1, 2, 3, 4, 5, 6, 7),
                                    percentages = c("12.4%", "99.2%", "4.9%", "32.4%", "11.2%", "82.4%", "55.7%"))
Words2 <- tibble::tibble(study = c("Column 1",
                                   "Column 2",
                                   "Column 3",
                                   "Column 4",
                                   "Column 5",
                                   "Column 6",
                                   "Column 7"),
                         numbers = c (1, 2, 3, 4, 5, 6, 7))

#Data for table 3
Dataset3Category1 <- tibble::tibble(mean  = c(7.25, 3.84, 5.12, 19.78, 6.45, 2.13),
                                    lower = c(6.80, 3.60, 4.90, 17.45, 5.90, 1.85),
                                    upper = c(7.90, 4.12, 5.40, 22.30, 6.98, 2.45),
                                    study = c("Category 1", "Category 1", "Category 1", "Category 1", "Category 1", "Category 1"),
                                    odds = c("7.25 (6.80-7.90)", "3.84 (3.60-4.12)", "5.12 (4.90-5.40)",
                                             "19.78 (17.45-22.30)", "6.45 (5.90-6.98)", "2.13 (1.85-2.45)"),
                                    numbers = c(1, 2, 3, 4, 5, 6),
                                    values = c(1, 2, 3, 4, 5, 6),
                                    percentages = c("7.4%", "11.2%", "3.5%", "2.8%", "0.9%", "1.7%"))
Dataset3Category2 <- tibble::tibble(study = c("Category 2", "Category 2", "Category 2", "Category 2", "Category 2", "Category 2"),
                                    odds=c("Reference", "Reference", "Reference", "Reference", "Reference", "Reference"),
                                    numbers = c(1, 2, 3, 4, 5, 6),
                                    values = c(1, 2, 3, 4, 5, 6),
                                    percentages = c("3.2%", "7.8%", "1.4%", "0.5%", "2.9%", "0.8%"))
Words3 <- tibble::tibble(study = c("Column 1",
                                   "Column 2",
                                   "Column 3",
                                   "Column 4",
                                   "Column 5",
                                   "Column 6"),
                         numbers = c (1, 2, 3, 4, 5, 6))

#Data for table 4
Dataset4Category1 <- tibble::tibble(mean  = c(4.12, 3.45, 2.78, 1.95, 5.67, 4.03, 3.56, 2.89),
                                    lower = c(3.89, 3.10, 2.50, 1.70, 5.10, 3.60, 3.30, 2.50),
                                    upper = c(4.45, 3.80, 3.10, 2.10, 6.05, 4.40, 3.80, 3.20),
                                    study = c("Category 1", "Category 1", "Category 1", "Category 1", "Category 1", "Category 1", "Category 1", "Category 1"),
                                    odds = c("4.12 (3.89-4.45)", "3.45 (3.10-3.80)", "2.78 (2.50-3.10)",
                                             "1.95 (1.70-2.10)", "5.67 (5.10-6.05)", "4.03 (3.60-4.40)",
                                             "3.56 (3.30-3.80)", "2.89 (2.50-3.20)"),
                                    numbers = c(1, 2, 3, 4, 5, 6, 7, 8),
                                    values = c(1, 2, 3, 4, 5, 6, 7, 8),
                                    percentages = c("12.7%", "8.4%", "2.3%", "15.9%", "4.7%", "6.1%", "3.2%", "1.8%"))
Dataset4Category2 <- tibble::tibble(study = c("Category 2", "Category 2", "Category 2", "Category 2", "Category 2", "Category 2", "Category 2", "Category 2"),
                                    odds=c("Reference", "Reference", "Reference", "Reference", "Reference", "Reference", "Reference", "Reference"),
                                    numbers = c(1, 2, 3, 4, 5, 6, 7, 8),
                                    values = c(1, 2, 3, 4, 5, 6, 7, 8),
                                    percentages = c("6.3%", "3.3%", "0.1%", "16.9%", "0.1%", "0.7%", "0.9%", "0.1%"))
Words4 <- tibble::tibble(study = c("Column 1",
                                   "Column 2",
                                   "Column 3",
                                   "Column 4",
                                   "Column 5",
                                   "Column 6",
                                   "Column 7",
                                   "Column 8"),
                         numbers = c (1, 2, 3, 4, 5, 6, 7, 8))


####Graphing code####

#Bind all dataframes together into one dataframe
total1 <- bind_rows(Words1, Dataset1Category1, Dataset1Category2)
total2 <- bind_rows(Words2, Dataset2Category1, Dataset2Category2)
total3 <- bind_rows(Words3, Dataset3Category1, Dataset3Category2)
total4 <- bind_rows(Words4, Dataset4Category1, Dataset4Category2)

#Arrange them by number
total1 <- total1 %>% arrange(numbers)
total2 <- total2 %>% arrange(numbers)
total3 <- total3 %>% arrange(numbers)
total4 <- total4 %>% arrange(numbers)

#Add space in the front of race names
total1$study <- ifelse(is.na(total1$values),
                       total1$study,
                       paste0("        ",total1$study))
total2$study <- ifelse(is.na(total2$values),
                       total2$study,
                       paste0("        ",total2$study))
total3$study <- ifelse(is.na(total3$values),
                       total3$study,
                       paste0("        ",total3$study))
total4$study <- ifelse(is.na(total4$values),
                       total4$study,
                       paste0("        ",total4$study))

#Add a column of consecutive numbers
total1$consecutive <- 1:nrow(total1) 
total2$consecutive <- 1:nrow(total2) 
total3$consecutive <- 1:nrow(total3) 
total4$consecutive <- 1:nrow(total4) 




#Create graph 1
#### Graph 1 ####
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
    study = c("", "Neonatal outcomes"),
    percentages = c("", "Percentages"),
    odds = c("", "Adjusted risk ratio")
  ) |>
  fp_set_zebra_style("#EFEFEF")


#Create graph 2
#### Graph 2 ####
#Specify the colors for the forestplot
#This only does one color for the lines and another color for filling in the boxes
styles <- fpShapesGp(
  lines = rep(list(gpar(col = "black")), 23),
  box = rep(list(gpar(fill = "black")), 23)
)

#Specify the x ticks for the forestplot
my_xticks <- c(1, 2, 4, 6)
my_labels <- c("1", "2", "4", "6")
attr(my_xticks, "labels") <- my_labels

#Create the forestplot with a logarithmic scale 
total2 |>
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
    study = c("", "Neonatal outcomes"),
    percentages = c("", "Percentages"),
    odds = c("", "Adjusted risk ratio")
  ) |>
  fp_set_zebra_style("#EFEFEF")


#Create graph 3
#### Graph 3 ####
#Specify the colors for the forestplot
#This only does one color for the lines and another color for filling in the boxes
styles <- fpShapesGp(
  lines = rep(list(gpar(col = "black")), 20),
  box = rep(list(gpar(fill = "black")), 20)
)

#Specify the x ticks for the forestplot
my_xticks <- c(1, 2, 4, 8, 16, 34)
my_labels <- c("1", "2", "4", "8", "16", "34")
attr(my_xticks, "labels") <- my_labels

#Create the forestplot with a logarithmic scale 
total3 |>
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
    study = c("", "Neonatal outcomes"),
    percentages = c("", "Percentages"),
    odds = c("", "Adjusted risk ratio")
  ) |>
  fp_set_zebra_style("#EFEFEF")

#Create graph 4
#### Graph 4 ####
#Specify the colors for the forestplot
#This only does one color for the lines and another color for filling in the boxes
styles <- fpShapesGp(
  lines = rep(list(gpar(col = "black")), 26),
  box = rep(list(gpar(fill = "black")), 26)
)

#Specify the x ticks for the forestplot
my_xticks <- c(1, 2, 4, 6)
my_labels <- c("1", "2", "4", "6")
attr(my_xticks, "labels") <- my_labels

#Create the forestplot with a logarithmic scale 
total4 |>
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
    lineheight = unit(1.4, "lines"),
    xticks = my_xticks,
    cols = c(1:2, 3:4, 5:6, 7:8, 10:12, 14:15),
    txt_gp = fpTxtGp(
      label = gpar(cex = 1.4),
      ticks = gpar(cex = 1.2),
      title = gpar(cex = 1.5)
    )
  ) |>
  fp_add_header(
    study = c("", "Neonatal outcomes"),
    percentages = c("", "Percentages"),
    odds = c("", "Adjusted risk ratio")
  ) |>
  fp_set_zebra_style("#EFEFEF")
