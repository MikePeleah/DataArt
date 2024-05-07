######################################################################################################################
##
##  Replication of Infographic Poster Coffee Export from Brasil to Italy 1939
##  Original https://archive.org/details/pequenoatlascafe1/page/n16/mode/1up 
##
##  Реплика инфографического плаката «Экспорт кофе из Бразилии в Италию», 1939 г.
##  Оригинал  https://archive.org/details/pequenoatlascafe1/page/n16/mode/1up 
##
##  Version / Версия: 1.3, 7 May 2024    
##  Creator:  Mike Peleah            🧐 https://www.linkedin.com/in/peleah/
##  Автор:    Михаил Пелях           🐦 https://twitter.com/MikePeleah
##                                   💻 https://peleah.me/data-artist/
##
##  License:  Attribution-NonCommercial-ShareAlike 4.0 International
##  Лицензия: https://creativecommons.org/licenses/by-nc-sa/4.0/
##            BY: Credit must be given to the creator               / Укажи автора 
##            NC: Only noncommercial uses of the work are permitted / Не наживайся на чужом 
##            SA: Adaptations must be shared under the same terms   /  Изучай, повторяй, делись CC BY-NC-SA 4.0
##
######################################################################################################################
# Loading libraries 🔹 Загружаем библиотеки
library(tidyverse)
library(ggplot2) 
library(sf)
library(patchwork)
library(showtext)

# Set working directory 🔹 Устанавливаем рабочую папку там, где запустился RStudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load fonts            🔹 Загружаем шрифты для пакета showtext
showtext_auto()
## Loading Google fonts (https://fonts.google.com/)
font_add_google("Alegreya Sans", "alegreya")      # Sans Serif font for main text
                                                  # Шрифт без засечек для основного текста, таблиц
font_add_google("Fascinate Inline", "fascinate")  # Art Deco decorative front for header 
                                                  # Шрифт в стилое Арт Деко для заголовка
font_add_google("Satisfy", "satisfy")             # Handwritten font for chart titles 
                                                  # Рукописный шрифт, которым выполнены заголовки гарфиков 
font_add_google("Girassol", "girassol")           # Headvy serif for for chart text and titles 
                                                  # Тяжёлый шрифт с зачеками для надписей на графиках
                                                  # Font for the red 1939 in header is not clear 
                                                  # Шрифт для года 1939, красного цвета я не нашёл пока 

### Part 1 : Make map of Italy in the center  ################################################################################################
###          Делаем карту Италии в центре
# Load Europe map as of 1930
# Так как данные за 1939 год, карта Европы выглядела СОВСЕМ по-другому. Ближйшая карта есть 
# за 1930 год, в части Италии изменении не было (вроде)
# Reference:  Источник данных для карты
#    . MPIDR [Max Planck Institute for Demographic Research] and CGG [Chair for Geodesy and Geoinformatics, 
#    . University of Rostock] 2013: MPIDR Population History GIS Collection – Europe (partly based on 
#    . © EuroGeographics for the administrative boundaries). Rostock.
#    . Downloaded from Historical GIS datafiles https://censusmosaic.demog.berkeley.edu/data/historical-gis-files
#
europe1930_shapefile <- st_read("./europe_1900-2003/Europe_1930_v.1.0.shp")
# Transform shapefile into EPSG:4326 World Geodetic System 1984
# Преобразование в EPSG:4326 World Geodetic System 1984.
# See https://guides.library.duke.edu/r-geospatial/CRS
europe1930_df <- st_transform(europe1930_shapefile, crs = 4326) %>% st_as_sf() %>% st_sf()
st_geometry(europe1930_df)

# Select Italy and other European countries in maps and union subregions for unified map of country
# Карты Италии и Европы объединением субрегионов и объединением карт, т.к. нужны только контуры
italy_map <- st_union(europe1930_df[europe1930_df$COUNTRY == 110, ]) 
europe_map <- st_union(st_make_valid(europe1930_df[europe1930_df$COUNTRY != 110, ]))

# Dataframe with coordinates of Italian ports in EPSG:4326 World Geodetic System 1984
# Фрейм данных с координатами итальянских портов в формате EPSG:4326 World Geodetic System 1984.
port_data <- data.frame(
  Port = c("Ancona", "Fiume", "Gênova", "Livorno", "Nápoles", "Palermo", "Trieste", "Veneza"),
  Latitude = c(43.6158, 45.3271, 44.4056, 43.5485, 40.8522, 38.1157, 45.6495, 45.4408),
  Longitude = c(13.5189, 14.4422, 8.9463, 10.3106, 14.2681, 13.3618, 13.7768, 12.3155)
)
port_sf <- st_as_sf(port_data, coords = c("Longitude", "Latitude"), crs = 4326)
st_geometry(port_sf)

# Making a round vignete for the Italy Map 
# Делаем круглую виньетку для карты Италии
# Define coordinates for the square
# Определим координаты квадрата
square_top_left <- c(4, 49)
square_bottom_right <- c(20, 34)
# Create the outer square polygon
# Создаем внешний квадратный многоугольник
square_outer <- st_polygon(list(rbind(square_top_left, c(square_bottom_right[1], square_top_left[2]),
                                      square_bottom_right, c(square_top_left[1], square_bottom_right[2]), square_top_left)))
# Define coordinates for the circular hole
# Определим координаты круглого пространства
circle_center <- c((square_top_left[1]+square_bottom_right[1])/2, (square_top_left[2]+square_bottom_right[2])/2)
rx = (square_bottom_right[1]-square_top_left[1])/2
ry = (square_bottom_right[2]-square_top_left[2])/2
circle_radius <- min(abs(rx), abs(ry))
circle_hole <- st_buffer(st_point(circle_center), circle_radius)
polygon_with_hole <- st_difference(square_outer, circle_hole)
# Convert polygon_with_hole to dataframe for ggplot in EPSG:4326 World Geodetic System 1984
# Преобразование порлигонов в фрейм данных для ggplot в EPSG:4326 World Geodetic System 1984
polygon_df <- st_as_sf(st_sfc(polygon_with_hole), crs = 4326)
circle_df <- st_as_sf(st_sfc(circle_hole), crs = 4326)
st_geometry(polygon_df)

### Preparing for plot, converting into EPSG:4087 WGS 84 / World Equidistant Cylindrical for 1:1 aspect ratio
### Подготовка к отрисовке, преобразование в EPSG:4087 WGS 84 / World Equidistant Cylindrical для соотношения сторон 1:1
port_sf_4087 <- st_transform(port_sf, crs = 4087)
italy_map_4087 <- st_transform(italy_map, crs = 4087)
europe_map_4087 <- st_transform(europe_map, crs = 4087)
polygon_df_4087 <- st_transform(polygon_df, crs = 4087)
circle_df_4087 <- st_transform(circle_df, crs = 4087)
# st_geometry(port_sf_4087)
# st_geometry(italy_map_4087)
# st_geometry(polygon_df_4087)
italy_bbox_4087 <- st_bbox(italy_map_4087)
polygon_bbox_4087 <- st_bbox(polygon_df_4087)

italy_plot_4087 <- ggplot() +
  geom_sf(data = italy_map_4087, fill = "lightyellow", color = "darkgray") +  # Italy (shaded in light yellow)
  geom_sf(data = europe_map_4087, fill = "lightgray", color = "black") +  # Other countries (shaded in lightgray)
  geom_sf(data = port_sf_4087, color = "blue", size = 3) +  # Ports as big blue dots
  geom_sf(data = polygon_df_4087, fill = "white", color = "white", alpha = 1) + # Polygon with hole 
  geom_sf(data = circle_df_4087, fill = "lightyellow", lwd=1, color = "black", alpha = 0) +  # and circle vignette 
  geom_sf_text(data = port_sf_4087, aes(label = Port), size = 3, vjust = 1, hjust = 1, color = "blue") + # Port names in blue 
  coord_sf(xlim = c(polygon_bbox_4087['xmin'],polygon_bbox_4087['xmax']), 
           ylim = c(polygon_bbox_4087['ymin'],polygon_bbox_4087['ymax']), expand = FALSE) +  # Set plot extent to focus on Italy
  theme_void() +  # Remove background and axes
  theme(legend.position = "none")  # Remove legend
italy_plot_4087

### Part 2 : Make pie charts and bar charts ################################################################################################
###          Создание круговых диаграмм и гистограмм 
# Read data on export by ports. 
# Чтение данных об экспорте по портам.
# Data from the original publication table / Данные из исходной таблицы публикации
# https://archive.org/details/pequenoatlascafe1/page/n17/mode/1up 
export_ports <- read.csv("./Coffee Export Brasil to Italy 1939 Ports.csv", stringsAsFactors = FALSE)

# Calculate for brazilian port -----------------------------------------------------------
# Рассчёты для бразильских портов 
brasil_ports <- export_ports %>% 
  filter(Destination=="Total", Origin!= "Total") %>% 
  arrange(desc(Amount)) %>%
  mutate(Origin = factor(Origin, levels = Origin), shAmount = Amount / sum(Amount))  %>% 
  # Prepare two labels, 
  #   labAmount for pie chart -- only if share more than 2%, otherwise slices are too narrow
  #   barAmount for bar chart for all data points, with some extra space arranged by new lines
  # Подготовка подписей данных
  #   labAmount для круговой диаграммы — только если доля превышает 2%, иначе фрагменты будут слишком узкими.
  #   barAmount для столбиковой дигараммы для всех точек данных, с дополнительным пространством, организованным новыми строками
  mutate(labAmount = ifelse(shAmount>0.02,  str_c(Origin, "\n", format(Amount, big.mark = ",")), ""),
         barAmount = str_c(Origin, "\n", format(Amount, big.mark = ","), "\n"))

# Save factor for using in small pie charts to follow the same color scheme 
# Сохраняем факторы = названия портов для использования в небольших круговых диаграммах, чтобы они соответствовали той же цветовой схеме.
port_factors <- brasil_ports %>% 
  pull(Origin) %>% 
  factor(levels = unique(export_ports$Origin))
# Define a custom pastel color palette with 8 distinct colors, inspired by  https://www.pinterest.com/pin/354377064434356336/
# Определяем собственную пастельную цветовую палитру из 8 различных цветов, вдохновленную https://www.pinterest.com/pin/354377064434356336/
pastel_palette <- c("#EAEEE0", "#CAB08B", "#EBF6FA", "#AED9EA", "#FFFFCC", "#E1E2E4", "#B5A28A", "#B4C6DC")

# Create the Brasil Export pie chart
# Создаём круговую диаграмму экспорта из Бразилии
brasil_pie_chart <- ggplot(brasil_ports, aes(x = "", y = Amount, fill = Origin)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = labAmount), position = position_stack(vjust = 0.5), color = "black", family="girassol") + 
  coord_polar(theta = "y", direction = -1, start = 0) +
  scale_fill_manual(limits = port_factors, values = pastel_palette) +
  theme_void() +
  theme(legend.position = NULL) +  # Adjust legend position if needed
  labs(title = "BRASIL",
       subtitle = "Procedência\nEn sacos de 60 quilos") +   # Chart title
  theme(
    legend.position = "none",  # Remove the fill legend
    plot.title = element_text(hjust = 0, vjust = 1, size=20, family="girassol"),
    plot.subtitle = element_text(hjust = 0, vjust = 1, size=12, family="satisfy")
  )
# Print the chart
print(brasil_pie_chart)

# Create the Brasil Export bar chart
# ... и столбиковую диаграмму 
brasil_bar_chart <- ggplot(brasil_ports, aes(x = Origin, y = Amount, fill = Origin)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = barAmount, y = 0), color = "black", family="girassol") +  # Add labels inside segments
  annotate("text", x=4, y=220000, label= "Portos de Procedência", size=8, family="satisfy") + 
  annotate("text", x=4, y=200000, label = "En sacos de 60 quilos", size=6, family="satisfy") + 
  # coord_polar("y", start = 0) +
  scale_fill_manual(values = pastel_palette) +
  theme_void() +
  # theme(legend.position = NULL) +  # Adjust legend position if needed
  scale_y_continuous(labels = scales::label_comma(), 
                     breaks = scales::pretty_breaks(n = 10)) + 
  # Use annotate instead of title to locate title in the middle of the chart
  # Используется аннотация вместо заголовка, чтобы запихать заголовок в середину диаграммы
  labs(title = "",
       subtitle = "",
       caption = "\n.") +   # Chart title
  theme(
    legend.position = "none",  # Remove the fill legend
    plot.title = element_text(hjust = 0.5, vjust = .5, size=18, family="satisfy"),
    plot.subtitle = element_text(hjust = 0.5, vjust = .5, size=14, family="satisfy")
  )
print(brasil_bar_chart)

# Create small pie charts by destinations 
# Создание небольших круговых диаграмм по пунктам назначения
# Unique destinations without Total 
# Уникальные направления
Destination <- export_ports %>% 
  filter(Destination != "Total") %>%   
  distinct(Destination) %>% 
  pull(Destination) 

# Apply Origin ports as factors, arranged by total amount, calculated earlier, 
# to maintain palette in all small pie charts 
# Используем порты отправления в виде фаторов, упорядоченных по общей сумме, рассчитанной ранее,
# для сохранения палитры во всех маленьких круговых диаграммах
O_ports <- export_ports %>% 
  mutate(Origin = factor(Origin, levels = port_factors))

# Empty list of charts 
# Пустой список графиков
D_pie_charts_list <- list()
# Fill it with pie charts 
# Заполняем его круговыми диаграммами
for (D in Destination) {
  D_ports <- O_ports %>% 
    filter(Destination==D, Origin!= "Total", !is.na(Amount)) %>% 
    arrange(desc(Amount)) %>%
    mutate(shAmount = Amount / sum(Amount))  %>% 
    # Generate labels only for slices more than 10%
    # Генерировать метки только для кусочков более 10%
    mutate(labAmount = ifelse(shAmount>0.10,  str_c(Origin, "\n", format(Amount, big.mark = ",")), ""))

  D_port_pie_chart <- ggplot(D_ports, aes(x = "", y = Amount, fill = Origin)) +
    geom_bar(stat = "identity", width = 1) +
    geom_text(aes(label = labAmount), position = position_stack(vjust = 0.5), color = "black", family="girassol") + 
    coord_polar(theta = "y", direction = -1, start = 0) +
    scale_fill_manual(limits = port_factors, values = pastel_palette) +
    theme_void() +
    theme(legend.position = NULL) +  # Adjust legend position if needed
    # Add desitination port as a title 
    labs(title = D,
         subtitle = "") +   # Chart title
    theme(
      legend.position = "none",  # Remove the fill legend
      plot.title = element_text(hjust = .5, vjust = 1, size=20, family="girassol"),
      plot.subtitle = element_text(hjust = .5, vjust = 1, size=12, family="satisfy")
    )
  # Print the chart
  print(D_port_pie_chart)
  D_pie_charts_list[[D]] <- D_port_pie_chart
}

# Calculate for italian ports --------------------------------------------------------
# То же самое для портов в Италии 
italy_ports <- export_ports %>% 
  filter(Origin=="Total", Destination != "Total") %>% 
  arrange(desc(Amount)) %>%
  mutate(Destination = factor(Destination, levels = Destination))  %>% 
  mutate(shAmount = Amount / sum(Amount))  %>% 
  mutate(labAmount = ifelse(shAmount>0.02,  str_c(Destination, "\n", format(Amount, big.mark = ",")), ""),
         barAmount = str_c(Destination, "\n", format(Amount, big.mark = ","), "\n"))


# Create the pie chart for ports in Italy 
# Круговая диаграмма длля портов назначения в Италии
italy_pie_chart <- ggplot(italy_ports, aes(x = "", y = Amount, fill = Destination)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = labAmount), position = position_stack(vjust = 0.5), color = "black", family="girassol") + 
  coord_polar(theta = "y", direction = -1, start = 0) +
  scale_fill_manual(values = pastel_palette) +
  theme_void() +
  labs(title = "ITALIA",
       subtitle = "Destino\nEn sacos de 60 quilos") +   # Chart title
  theme(
    legend.position = "none",  # Remove the fill legend
    plot.title = element_text(hjust = 0, vjust = 1, size=20, family="girassol"),
    plot.subtitle = element_text(hjust = 0, vjust = 1, size=12, family="satisfy")
  )
# Print the chart
print(italy_pie_chart)

# Create the bar chart for ports in Italy 
# ... и столбиковая диаграмма 
# In this bar chart bars are going in reverse order 
# В данном случае столбики идут в обратном порядке, поэтому реверсируем названия портов
italy_bar_chart <- ggplot(italy_ports, aes(x = rev(Destination), y = Amount, fill = Destination)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = barAmount, y = 0), color = "black", family="girassol") +  # Add labels inside segments
  annotate("text", x=3, y=115000, label= "Portos de Destino", size=8, family="satisfy") + 
  annotate("text", x=3, y=100000, label = "En sacos de 60 quilos", size=6, family="satisfy") + 
  scale_fill_manual(values = pastel_palette) +
  theme_void() +
  scale_y_continuous(labels = scales::label_comma()) + 
  scale_x_discrete(position = "top")+ # Flip the x-axis
  #  geom_text(aes(x = Inf, y = -Inf, label = "BRASIL\nProcedência\nEn sacos de 60 quilos", family = "satisfy"), 
  #            hjust = 0, vjust = 1) + 
  labs(title = "",
       subtitle = "",
       caption = "\n.") +   # Chart title
  theme(
    legend.position = "none",  # Remove the fill legend
    plot.title = element_text(hjust = 0.5, vjust = .5, size=18, family="satisfy"),
    plot.subtitle = element_text(hjust = 0.5, vjust = .5, size=14, family="satisfy")
  )
print(italy_bar_chart)

### Part 3 : Combine them together ################################################################################################
###          Собираем всё вместе
# Design of poster. Letters for charts, # for empty space 
# Для пакета patchwork надо задать дизайн. 
design <- "#ACCE#
           BBCCFF
           BBCCFF
           DDDDDD"
coffee_poster <- wrap_plots(A = brasil_pie_chart, 
           B = brasil_bar_chart, 
           C = italy_plot_4087,
           D = wrap_plots(D_pie_charts_list, ncol = 8), # Wrap small pie charts in a wide strip 
           E = italy_pie_chart, 
           F = italy_bar_chart, 
           design = design) + 
  plot_annotation(title = "EXPORTAÇÃO de CAFE'para a ITALIA * 1939", 
                  caption = "\nDesigned by Mike Peleah linkedin.com/in/peleah/ based on original chart in Pequeno Atlas Estatistico do Café Nº1 - 1940, CC BY-NC-SA 4.0. The maps in based on ©EuroGeographics for the administrative boundaries",
                  theme = theme(plot.title = element_text(size = 56, 
                                                          vjust = 0.5, 
                                                          hjust = 0.5, 
                                                          family = "fascinate"),
                                plot.caption = element_text(size = 12, 
                                                            vjust = 0.5, 
                                                            hjust = 0, 
                                                            family = "girassol",
                                                            color="darkgray")))

print(coffee_poster)
# Saving from RStudio fullscreen image gives slightly better results Coffee Export Brasil to Italy 1939 RRReplica save.png
# than saving using ggsave Coffee Export Brasil to Italy 1939 RRReplica ggsave.png
# Сохранение из RStudio в полноэкранном режиме работает чуть лучше, чем ggsave 
ggsave(filename = "Coffee Export Brasil to Italy 1939 RRReplica ggsave.png",
       plot = coffee_poster,
       width = 1920, height = 1200, units = "px",
       device = "png",
      )

print("* * * That's all, folks! * * *")
