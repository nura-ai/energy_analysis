#1. С plot3D (статичный, но красивый 4D-scatter)
library(plot3D)

# Пример данных: x, y, z + w (четвёртая переменная)
x <- rnorm(100)
y <- rnorm(100)
z <- rnorm(100)
w <- x + y + z + rnorm(100)  # четвёртая размерность

scatter3D(x, y, z, colvar = w,  # w — цвет
          pch = 16, cex = abs(w)/max(abs(w))*2,  # размер по |w|
          theta = 20, phi = 30,
          xlab = "X", ylab = "Y", zlab = "Z",
          clab = "W (четвёртая переменная)",
          main = "4D Scatter Plot")

#2. С plotly (интерактивный 4D-scatter)
library(plotly)

plot_ly(x = ~x, y = ~y, z = ~z,
        type = "scatter3d", mode = "markers",
        marker = list(size = ~abs(w)*10,  # размер по w
                      color = ~w,         # цвет по w
                      colorscale = "Viridis",
                      showscale = TRUE)) %>%
  layout(title = "Интерактивный 4D Scatter Plot",
         scene = list(xaxis = list(title = "X"),
                      yaxis = list(title = "Y"),
                      zaxis = list(title = "Z")))

#3. С rgl (полностью интерактивный)
library(rgl)

plot3d(x, y, z, col = rainbow(100)[cut(w, 100)], size = abs(w)*5)
# Вращай мышкой в окне rgl!