library(plotly)
library(ggplot2)
library(readr)

# Read the data (assuming your data is in a CSV file)
df <- read_csv("Final Risk Optimization.csv") # Replace with the path to your CSV file
df <- df[df$score >= 0, ]
fig <- plot_ly(data = df, x = ~sma, y = ~takeprofit, z = ~entry, type = 'scatter3d', mode = 'markers')
#fig <- plot_ly(z = ~df$score, type = "surface")
fig <- fig %>% add_trace(data = df, x = ~sma, y = ~takeprofit, z = ~entry, 
                         type = 'scatter3d', mode = 'marker',
                         marker = list(size = 10, color = ~score, colorscale = 'Viridis'))


# Show the plot


fig