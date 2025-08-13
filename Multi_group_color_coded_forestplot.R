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


#Compares five groups across five outcomes, showing each group’s percentages and adjusted risk ratios with 95% confidence intervals
#Uses a color-coded forest plot where each group’s boxes and lines have distinct shades, with zebra-striped rows
#Aligns numerical data with visual markers to make it easy to compare both relative risks and raw percentages 


#Create data manually
White1 <- tibble::tibble(study = c("Non-Hispanic white", "Non-Hispanic white", "Non-Hispanic white", "Non-Hispanic white", "Non-Hispanic white"),
                         odds=c("Reference", "Reference", "Reference", "Reference", "Reference"),
                         numbers = c(1, 2, 3, 4, 5),
                         values = c(1, 2, 3, 4, 5),
                         percentages = c("35.28%", "58.47%", "12.63%", "87.15%", "9.42%"))
Black1 <- tibble::tibble(mean  = c(8.54, 14.23, 6.78, 10.91, 3.62),
                         lower = c(7.89, 12.87, 5.90, 9.45, 2.99),
                         upper = c(9.25, 15.78, 7.65, 12.50, 4.20),
                         study = c("Non-Hispanic Black", "Non-Hispanic Black", "Non-Hispanic Black", "Non-Hispanic Black", "Non-Hispanic Black"),
                         odds = c("8.54 (7.89-9.25)", "14.23 (12.87-15.78)", "6.78 (5.90-7.65)", 
                                  "10.91 (9.45-12.50)", "3.62 (2.99-4.20)"),
                         numbers = c(1, 2, 3, 4, 5),
                         values = c(1, 2, 3, 4, 5),
                         percentages = c("48.67%", "32.14%", "57.89%", "26.53%", "11.74%"))
Hispanic1 <- tibble::tibble(mean  = c(5.12, 8.93, 3.45, 7.67, 9.21),
                            lower = c(4.60, 7.85, 2.90, 6.88, 8.10),
                            upper = c(5.75, 9.80, 4.10, 8.50, 10.30),
                            study = c("Hispanic", "Hispanic", "Hispanic", "Hispanic", "Hispanic"),
                            odds = c("5.12 (4.60-5.75)", "8.93 (7.85-9.80)", "3.45 (2.90-4.10)", 
                                     "7.67 (6.88-8.50)", "9.21 (8.10-10.30)"),
                            numbers = c(1, 2, 3, 4, 5),
                            values = c(1, 2, 3, 4, 5),
                            percentages = c("52.31%", "29.78%", "41.62%", "68.47%", "33.89%"))
Asian1 <- tibble::tibble(mean  = c(6.15, 8.27, 3.10, 7.34, 9.02),
                         lower = c(5.62, 7.58, 2.65, 6.50, 8.01),
                         upper = c(6.78, 9.00, 3.55, 8.10, 10.23),
                         study = c("Asian/Pacific Islanders", "Asian/Pacific Islanders", "Asian/Pacific Islanders", "Asian/Pacific Islanders", "Asian/Pacific Islanders"),
                         odds = c("6.15 (5.62-6.78)", "8.27 (7.58-9.00)", "3.10 (2.65-3.55)", 
                                  "7.34 (6.50-8.10)", "9.02 (8.01-10.23)"),
                         numbers = c(1, 2, 3, 4, 5),
                         values = c(1, 2, 3, 4, 5),
                         percentages = c("38.47%", "82.13%", "27.91%", "54.62%", "21.38%"))
AIAN1 <- tibble::tibble(mean  = c(5.28, 7.44, 3.67, 8.12, 9.87),
                        lower = c(4.80, 6.90, 3.10, 7.55, 9.01),
                        upper = c(5.90, 7.98, 4.20, 8.70, 10.45),
                        study = c("AIAN", "AIAN", "AIAN", "AIAN", "AIAN"),
                        odds = c("5.28 (4.80-5.90)", "7.44 (6.90-7.98)", "3.67 (3.10-4.20)",
                                 "8.12 (7.55-8.70)", "9.87 (9.01-10.45)"),
                        numbers = c(1, 2, 3, 4, 5),
                        values = c(1, 2, 3, 4, 5),
                        percentages = c("53.12%", "65.78%", "21.43%", "38.59%", "12.87%"))
Words1 <- tibble::tibble(study = c("Column 1", 
                                   "Column 2",
                                   "Column 3",
                                   "Column 4",
                                   "Column 5"),
                         numbers = c (1, 2, 3, 4, 5))

#Bind all dataframes together into one dataframe
total1 <- bind_rows(White1, Black1, Hispanic1, Asian1, AIAN1, Words1)

#Arrange them by number
total1 <- total1 %>% arrange(numbers)

#Add space in the front of race names
total1$study <- ifelse(is.na(total1$values),
                       total1$study,
                       paste0("        ",total1$study))

#Add a column of consecutive numbers
total1$consecutive <- 1:nrow(total1) 

#Rearrange the rows so that the column names are at the top
total1 <- total1 %>% arrange(factor(consecutive, levels = c('6', '1', '2', '3', '4', '5',
                                                            '12', '7', '8', '9', '10', '11',
                                                            '18', '13', '14', '15', '16', '17',
                                                            '24', '19', '20', '21', '22', '23',
                                                            '30', '25', '26', '27', '28', '29')
))

#Specify the colors for the forestplot
styles <- fpShapesGp(
  lines = list(
    gpar(col = "#D95F59"),
    gpar(col = "#D95F59"),
    gpar(col = "#D95F59"),
    gpar(col = "#D95F59"),
    gpar(col = "#D95F59"),
    gpar(col = "#C63C51"),
    gpar(col = "#8C3061"),
    gpar(col = "#522258"),
    gpar(col = "#D95F59"),
    gpar(col = "#D95F59"),
    gpar(col = "#D95F59"),
    gpar(col = "#C63C51"),
    gpar(col = "#8C3061"),
    gpar(col = "#522258"),
    gpar(col = "#D95F59"),
    gpar(col = "#D95F59"),
    gpar(col = "#D95F59"),
    gpar(col = "#C63C51"),
    gpar(col = "#8C3061"),
    gpar(col = "#522258"),
    gpar(col = "#D95F59"),
    gpar(col = "#D95F59"),
    gpar(col = "#D95F59"),
    gpar(col = "#C63C51"),
    gpar(col = "#8C3061"),
    gpar(col = "#522258"),
    gpar(col = "#D95F59"),
    gpar(col = "#D95F59"),
    gpar(col = "#D95F59"),
    gpar(col = "#C63C51"),
    gpar(col = "#8C3061"),
    gpar(col = "#522258")
  ),
  box = list(
    gpar(fill = "#D95F59"),
    gpar(fill = "#D95F59"),
    gpar(fill = "#D95F59"),
    gpar(fill = "#D95F59"),
    gpar(fill = "#D95F59"),
    gpar(fill = "#C63C51"),
    gpar(fill = "#8C3061"),
    gpar(fill = "#522258"),
    gpar(fill = "#D95F59"),
    gpar(fill = "#D95F59"),
    gpar(fill = "#D95F59"),
    gpar(fill = "#C63C51"),
    gpar(fill = "#8C3061"),
    gpar(fill = "#522258"),
    gpar(fill = "#D95F59"),
    gpar(fill = "#D95F59"),
    gpar(fill = "#D95F59"),
    gpar(fill = "#C63C51"),
    gpar(fill = "#8C3061"),
    gpar(fill = "#522258"),
    gpar(fill = "#D95F59"),
    gpar(fill = "#D95F59"),
    gpar(fill = "#D95F59"),
    gpar(fill = "#C63C51"),
    gpar(fill = "#8C3061"),
    gpar(fill = "#522258"),
    gpar(fill = "#D95F59"),
    gpar(fill = "#D95F59"),
    gpar(fill = "#D95F59"),
    gpar(fill = "#C63C51"),
    gpar(fill = "#8C3061"),
    gpar(fill = "#522258")
  ) 
)


#Form the forestplot
total1 |>
  forestplot(labeltext = c(study, percentages, odds),
             mean,
             lower,
             upper,
             graph.pos = "right",
             boxsize=0.75,
             clip = c(0.1, 2.0), 
             shapes_gp = styles,
             plotwidth=unit(15, "cm"),
             colgap = unit(13, "mm"),
             lineheight = "auto",
             cols=c(1:2,3:4,5:6,7:8, 10:12, 14:15),
             xticks = c(1, 2, 4, 8, 16, 20),
             txt_gp=fpTxtGp(label= gpar(cex = 1, fontfamily = "Times New Roman"),
                            title = gpar(cex = 1))) %>%
  fp_add_header(study = c("" , "Table1"),
                percentages = c("", "Percentages"),
                odds = c("", "aRR (95% CI)")) |>
  
  fp_set_zebra_style("#EFEFEF")

