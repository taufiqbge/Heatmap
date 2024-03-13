###heatmap code working

library(ggplot2)
library(reshape2)

# Read the CSV file
data <- read.csv("heatmap_all.csv")

# Select the four columns for the heatmap
heatmap_data <- data[, c("genes", "h3h", "h6h", "h24h")]

# Melt the data and specify column names
melted_data <- melt(heatmap_data, id.vars = "genes", variable.name = "column", value.name = "value")

# Create the heatmap using ggplot2
p <- ggplot(data = melted_data, aes(x = column, y = genes, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "red", high = "green") +
  labs(x = "", y = "") +
  theme_minimal()
p
  




##======================================


library(ggplot2)
library(reshape2)
library(stats)

# Read the CSV file
data <- read.csv("heatmap_all.csv")

# Select the four columns for the heatmap
heatmap_data <- data[, c("genes", "h3h", "h6h", "h24h")]

# Perform hierarchical clustering
dist_mat <- dist(heatmap_data[, -1])  # Compute the distance matrix
hc_rows <- hclust(dist_mat)  # Hierarchical clustering for rows
hc_cols <- hclust(dist(t(heatmap_data[, -1])))  # Hierarchical clustering for columns

# Reorder rows and columns based on clustering results
heatmap_data <- heatmap_data[hc_rows$order, ]
heatmap_data <- heatmap_data[, c(1, hc_cols$order + 1)]

# Melt the data and specify column names
melted_data <- melt(heatmap_data, id.vars = "genes", variable.name = "column", value.name = "value")

# Create the heatmap using ggplot2
p <- ggplot(data = melted_data, aes(x = column, y = genes, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "red", high = "green") +
  labs(x = "", y = "") +
  theme_minimal()
p

# Set up a coloring scheme using colorRampPalette
red <- rgb(1, 0, 0)
green <- rgb(0, 1, 0)
blue <- rgb(0, 0, 1)
black <- rgb(0, 0, 0)
RtoBrange <- colorRampPalette(c(red, black))
BtoGrange <- colorRampPalette(c(black, green))

p <- p + scale_fill_gradient2(low = RtoBrange(100), mid = "black", high = BtoGrange(100))
p


##---------------------------

df <- scale(mtcars)
# Default plot
heatmap(df, scale = "none")



# Read the CSV file
data <- read.csv("nba.csv",sep=",")
mat <- as.matrix(data)


heatmap(data, scale = "none")

### Creating heatmap

# Read the CSV file
data <- read.csv("nba.csv", header = TRUE, row.names = 1)

# Convert data to a numeric matrix
mat <- as.matrix(data)

# Generate the heatmap
heatmap(mat, scale = "none")



##----------------------------------------

# Read the CSV file
data <- read.csv("heatmap_all.csv", header = TRUE, row.names = 1)

# Convert data to a numeric matrix
mat <- as.matrix(data)

# Trim column names
colnames(mat) <- trimws(colnames(mat))

# Define the desired column order
column_order <- c("h3h", "h6h", "h24h")  # Replace with your desired column names in the desired order

# Reorder the columns
mat <- mat[, column_order]

# Generate the heatmap
# Generate the heatmap with a scale bar
#heatmap(mat, scale = "none", ColSideColors = c("red", "white", "green"), key = TRUE)

heatmap(mat, scale = "none")




# Read the CSV file
data <- read.csv("heatmap_all.csv", header = TRUE, row.names = 1)

# Convert data to a numeric matrix
mat <- as.matrix(data)

# Trim column names
colnames(mat) <- trimws(colnames(mat))

# Define the desired column order
column_order <- c("h3h", "h6h", "h24h")  # Replace with your desired column names in the desired order

# Reorder the columns
mat <- mat[, column_order]

# Generate the heatmap with a color bar
heatmap(mat, scale = "none", col = colorRampPalette(c("red", "white", "green"))(100), legend = TRUE)
##==========================


## working heatmap wit pheatmap library

library(pheatmap)

# Read the CSV file
data <- read.csv("heatmap_all.csv", header = TRUE, row.names = 1)

# Convert data to a numeric matrix
mat <- as.matrix(data)

# Trim column names
colnames(mat) <- trimws(colnames(mat))

# Define the desired column order
column_order <- c("h3h", "h6h", "h24h")  # Replace with your desired column names in the desired order

# Reorder the columns
mat <- mat[, column_order]

pheatmap(mat, scale = "none",border_color = "NA",cluster_rows = TRUE, cluster_cols = FALSE)

# Create the heatmap using pheatmap
#pheatmap(mat, scale = "none", color = colorRampPalette(c("red", "blue", "green"))(10), 
         #cluster_rows = TRUE, cluster_cols = FALSE)
