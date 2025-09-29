#####set up##############################
install.packages("BiocManager")
library("BiocManager")
BiocManager::install()
BiocManager::install("DESeq2")
BiocManager::install("edgeR")

install.packages("tidyverse")
library(tidyverse)

####Task 4###############################
#1.	What is the square root of 10?
sqrt(10)
#[1] 3.162278

#2.	What is the logarithm of 32 to the base 2?
log2(32)
#[1] 5

#3.	What is the sum of the numbers from 1 to 1000?
sum(1:1000)
#[1] 500500
  
#4.	What is the sum of all even numbers from 2 to 1000?
sum(seq(2, 1000, by = 2))
#[1] 250500

#5.	How many pairwise comparisons are there for 100 genes?
choose(100, 2)
#[1] 4950
  
#6.	And how many ways to arrange 100 genes in triples?
choose(100, 3)
#[1] 161700

#####Task 5#############################
#1. Use the R internal CO2 dataset (“data(CO2)”).
data(CO2)

#2. Describe briefly the content of the CO2 dataset using the help function.
help(CO2)
#Carbon Dioxide Uptake in Grass Plants
#Description
  #The CO2 data frame has 84 rows and 5 columns of data from an experiment on the cold tolerance of the grass species Echinochloa crus-galli.
#Usage
  #CO2
#Format
  #An object of class c("nfnGroupedData", "nfGroupedData", "groupedData", "data.frame") containing the following columns:
  #Plant
    #an ordered factor with levels Qn1 < Qn2 < Qn3 < ... < Mc1 giving a unique identifier for each plant.
  #Type
    #a factor with levels Quebec Mississippi giving the origin of the plant
  #Treatment
    #a factor with levels nonchilled chilled
  #conc
    #a numeric vector of ambient carbon dioxide concentrations (mL/L).
  #uptake
    #a numeric vector of carbon dioxide uptake rates (μmol/m2sec).
  #Details
    #The CO2 uptake of six plants from Quebec and six plants from Mississippi was measured at several levels of ambient CO2 concentration. Half the plants of each type were chilled overnight before the experiment was conducted.
    #This dataset was originally part of package nlme, and that has methods (including for [, as.data.frame, plot and print) for its grouped-data classes.
#Source
  #Potvin, C., Lechowicz, M. J. and Tardif, S. (1990) “The statistical analysis of ecophysiological response curves obtained from experiments involving repeated measures”, Ecology, 71, 1389–1400.
  #Pinheiro, J. C. and Bates, D. M. (2000) Mixed-effects Models in S and S-PLUS, Springer.

?CO2       # Display help
str(CO2)   # Check the data structure
head(CO2)  # Check the first 6 rows

#3. What is the average and median CO2 uptake of the plants from Quebec and Mississippi?
tapply(CO2$uptake, CO2$Type, mean)   # average
#Quebec Mississippi 
#33.54286    20.88333 
tapply(CO2$uptake, CO2$Type, median) # median
#Quebec Mississippi 
#37.15       19.30 

#4. [Optional] In the “airway” example data from Bioconductor, how many genes are expressed in each sample? How many genes are not expressed in any sample?
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("airway")

library(airway)
data(airway)

# The airway data is a SummarizedExperiment object
counts <- assay(airway)  # Gene expression count matrix (genes x samples)

# Count the number of genes with expression >= 1 for each sample
genes_expressed_per_sample <- colSums(counts > 0)

# Number of genes that are never expressed in any sample
genes_never_expressed <- sum(rowSums(counts > 0) == 0)

# Output
list(
  #avg_by_region = tapply(CO2$uptake, CO2$Type, mean),
  #median_by_region = tapply(CO2$uptake, CO2$Type, median),
  genes_expressed_per_sample = genes_expressed_per_sample,
  genes_never_expressed = genes_never_expressed
)
#$genes_expressed_per_sample
#SRR1039508 SRR1039509 SRR1039512 SRR1039513 SRR1039516 SRR1039517 
#24633      24527      25699      23124      25508      25998 
#SRR1039520 SRR1039521 
#24662      23991 

#$genes_never_expressed
#[1] 30208

#####Task 6#############################
#1. Write a function that calculates the ratio of the mean and the median of a given vector.
#This is a helpful measure to detect data with outlying values.
#Note: See Reference for R language
mean_median_ratio <- function(x) {
  if (!is.numeric(x)) stop("Input must be a numeric vector")
  m <- mean(x, na.rm = TRUE)
  med <- median(x, na.rm = TRUE)
  ratio <- m / med
  return(ratio)
}

# Example usage
vec <- c(1, 2, 3, 4, 100)
mean_median_ratio(vec)
#[1] 7.333333

#2. Write a function that ignores the lowest and the highest value from a given vector and calculate the mean.
trimmed_mean <- function(x) {
  if (!is.numeric(x)) stop("Input must be a numeric vector")
  if (length(x) <= 2) stop("Vector must have more than 2 elements")
  
  # Remove the minimum and maximum values
  x_trimmed <- x[x != min(x) & x != max(x)]
  
  # Handle the case where all values are the same (min == max)
  if (length(x_trimmed) == 0) {
    warning("All values are the same or only min and max values exist. Returning NA.")
    return(NA)
  }
  
  return(mean(x_trimmed, na.rm = TRUE))
}

# Example usage
vec <- c(1, 2, 3, 4, 100)
trimmed_mean(vec)
#[1] 3

####Task 7###########################
#1. Compare the distributions of the body heights of the two species from the 'magic_guys.csv' dataset graphically
suppressMessages({library(nummenmaa)})
install.packages("remotes")
library(remotes)
install_url("http://emotion.utu.fi/wp-content/uploads/2019/11/nummenmaa_1.0.tar.gz", dependencies=TRUE)

# Load data
magic <- read.csv("magic_guys.csv")
#a. using the basic 'hist' function as well as ‘ggplot’ and ‘geom_histogram’
   #functions from the ggplot2 package. Optimize the plots for example by trying
   #several different 'breaks'. Note that ggplot2-based functions give you many
   #more options for changing the visualization parameters, try some of them.
# Basic hist function
jedi <- magic %>% filter(species =="jedi")
sith <- magic %>% filter(species == "sith")

hist(jedi$length, main="Height of Jedi", xlab="Height", breaks=10, col="lightblue")
hist(sith$length, main="Height of Sith", xlab="Height", breaks=10, col="pink")

#b. Do the same comparison as in a. but with boxplots. If you want to use the 
   #ggplot2-package, use the functions ‘ggplot’ and ‘geom_boxplot’.
# Basic boxplot
# Basic boxplot
boxplot(magic[["length"]] ~ magic$species,
        main="Height by Species",
        ylab="Height",
        col=c("lightblue","pink"))

# ggplot2 boxplot
library(ggplot2)
ggplot(magic, aes(x=species, y=magic[["length"]], fill=species)) +
  geom_boxplot(alpha=0.7) +
  labs(title="Height Boxplot by Species", x="Species", y="Height") +
  theme_minimal()

#c. Save the plots with the ‘png’,‘pdf’, and ‘svg’ formats. In which situation would you use which file format?
gg <- ggplot(magic, aes(x=species, y=length, fill=species)) +
  geom_boxplot(alpha=0.7) +
  labs(title="Height Boxplot by Species", x="Species", y="Height") +
  theme_minimal()

# Save plots in different formats
ggsave("7_1_height_boxplot.png", plot=gg, width=6, height=4, dpi=300)
ggsave("7_1_height_boxplot.pdf", plot=gg, width=6, height=4)
ggsave("7_1_height_boxplot.svg", plot=gg, width=6, height=4)

#2. Load the gene expression data matrix from the ‘microarray_data.tab’ dataset provided in the shared folder, 
   #it is a big tabular separated matrix.a. 
# Load data
microarray <- read.table("microarray_data.tab", header=TRUE, sep="\t", stringsAsFactors=FALSE)
#a. How big is the matrix in terms of rows and columns?
dim(microarray)
#[1]  553 1000

#b. Count the missing values per gene and visualize this result.
na_count <- apply(microarray, 1, function(x) sum(is.na(x)))

# Visualize missing values
hist(na_count, breaks=20, main="Missing Values per Gene", xlab="Number of Missing Values", col="lightgray")

#c. Find the genes for which there are more than X% (X=10%, 20%, 50%) missing values.
thresholds <- c(0.1, 0.2, 0.5)
for (x in thresholds) {
  genes_over <- rownames(microarray)[na_count > x * ncol(microarray)]
  cat("Genes with >", x*100, "% missing:", length(genes_over), "\n")
}
#Genes with > 10 % missing: 337 
#Genes with > 20 % missing: 236 
#Genes with > 50 % missing: 8 

#d. Replace the missing values by the average expression value for the particular gene. (Note: Imputing data has to be used with caution!)
microarray_imputed <- t(apply(microarray, 1, function(x) {
  x[is.na(x)] <- mean(x, na.rm=TRUE)
  return(x)
}))
microarray_imputed <- as.data.frame(microarray_imputed)

#3. Visualize the data in the CO2 dataset in a way that gives you a deeper understanding of the data. What do you see?
data(CO2)

# Basic plot
plot(CO2$conc, CO2$uptake, main="CO2 Uptake vs Concentration", xlab="CO2 Concentration", ylab="Uptake", pch=19, col=CO2$Type)

# Enhanced ggplot2 visualization
ggplot(CO2, aes(x=conc, y=uptake, color=Type, shape=Treatment)) +
  geom_point(size=3) +
  geom_smooth(method="loess", se=FALSE) +
  labs(title="CO2 Uptake by Concentration, Type, and Treatment") +
  theme_minimal()

#####Task 8#############################
devtools::install_github("hirscheylab/tidybiology")
library(tidyverse)
library(tidybiology)
library(dplyr)

# Load data
data("chromosome")
data("proteins")

# a. Extract summary statistics (mean, median and maximum) for the following
    #variables from the ‘chromosome’ data: variations, protein coding genes, and
    #miRNAs. Utilize the tidyverse functions to make this as simply as possible.
chromosome_summary <- chromosome %>%
  summarise(
    mean_variations = mean(variations, na.rm = TRUE),
    median_variations = median(variations, na.rm = TRUE),
    max_variations = max(variations, na.rm = TRUE),
    mean_protein_coding_genes = mean(protein_codinggenes, na.rm = TRUE),
    median_protein_coding_genes = median(protein_codinggenes, na.rm = TRUE),
    max_protein_coding_genes = max(protein_codinggenes, na.rm = TRUE),
    mean_miRNAs = mean(mi_rna, na.rm = TRUE),
    median_miRNAs = median(mi_rna, na.rm = TRUE),
    max_miRNAs = max(mi_rna, na.rm = TRUE)
  )
chromosome_summary
#  mean_variations median_variations max_variations
#<dbl>             <dbl>          <dbl>
#  1        6484572.           6172346       12945965

# b. How does the chromosome size distribute? Plot a graph that helps to
    #visualize this by using ggplot2 package functions.
ggplot(chromosome, aes(x = length_mm)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Chromosome Sizes", x = "Chromosome Length (mm)", y = "Count") +
  theme_minimal()

# c. Does the number of protein coding genes or miRNAs correlate with the length
    #of the chromosome? Make two separate plots to visualize these relationships.
# Protein coding genes vs chromosome length
ggplot(chromosome, aes(x = length_mm, y = protein_codinggenes)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "black", fill = "lightgreen") +
  labs(title = "Protein Coding Genes vs Chromosome Length", 
       x = "Chromosome Length (Mbp)", y = "Protein Coding Genes") +
  theme_minimal()

# miRNAs vs chromosome length
ggplot(chromosome, aes(x = length_mm, y = mi_rna)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", se = TRUE, color = "black", fill = "plum") +
  labs(title = "miRNAs vs Chromosome Length", 
       x = "Chromosome Length (Mbp)", y = "miRNAs") +
  theme_minimal()

# d. Calculate the same summary statistics for the ‘proteins’ data variables length
    #and mass. Create a meaningful visualization of the relationship between
    #these two variables by utilizing the ggplot2 package functions. Play with the 
    #colors, theme- and other visualization parameters to create a plot that pleases you.
proteins_summary <- proteins %>%
  summarise(
    mean_length = mean(length, na.rm = TRUE),
    median_length = median(length, na.rm = TRUE),
    max_length = max(length, na.rm = TRUE),
    mean_mass = mean(mass, na.rm = TRUE),
    median_mass = median(mass, na.rm = TRUE),
    max_mass = max(mass, na.rm = TRUE)
  )
proteins_summary
#  mean_length median_length max_length mean_mass median_mass
#<dbl>         <dbl>      <dbl>     <dbl>       <dbl>
#  1        557.           414      34350    62061.      46140.

# Visualization of relationship between length and mass
ggplot(proteins, aes(x = length, y = mass)) +
  geom_point(aes(color = mass), alpha = 0.7, size = 4) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_color_gradient(low = "lightblue", high = "red") +
  labs(title = "Protein Length vs Mass", x = "Length", y = "Mass") +
  theme_minimal()
