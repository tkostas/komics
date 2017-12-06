#-------------------------------------- 
# perform multiple t-tests and correct p-values for multiple hypothesis testing error

#--------------------------------------
# install packages (you need to do it only once)
install.packages("readxl")
install.packages("devtools")
devtools::install_github("tkostas/komics", dependencies = TRUE)

#--------------------------------------
# load packages (you need to do it in every session)
library(readxl)
library(komics)

d <- read_excel(file.choose(), col_names = TRUE)
d <- as.data.frame(d)
head(d)
str(d)
names(d)
?calc_ttest_padj
?p.adjust.methods

d2 <- calc_ttest_padj(d,
                id_groupA = "WT_", 
                id_groupB = "Mut_", 
                t.test_var.equal = FALSE, 
                pAdj_method = "BH", 
                output_name = "this is the output1.txt")

d2 <- calc_ttest_padj(d,
                id_groupA = "SL.000", 
                id_groupB = "SL.317", 
                t.test_var.equal = TRUE, 
                pAdj_method = "BH", 
                output_name = "this is another output.txt")
