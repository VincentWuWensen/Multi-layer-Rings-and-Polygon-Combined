# Load libraries
library(ggplot2)
library(dplyr)
library(cowplot)
library(readxl)

set.seed(123)
n <- 67

# 1.Dataset
df <- data.frame(
  ID = paste0("A", 1:n),
  Feature_1   = runif(n, 0, 100),   # Example values 1
  Feature_2 = runif(n, 0, 1000),  # Example values 2
  Rising   = sample(0:1, n, replace=TRUE),   # 0=falling, 1=Rising
  Huge= sample(0:1, n, replace=TRUE)    # 0=Huge, 1=Small
) %>%
  # Normalize values to [0,1] for plotting heights
  mutate(
    mon_norm = Feature_1   / max(Feature_1),
    ppl_norm = Feature_2 / max(Feature_2)
  )


# 2. Define ring radii and angles
radii <- 0:4            # radii 0,1,2,3,4 (so each ring has thickness 1)
total_angle <- 180      # total angular span in degrees
angles <- seq(90, -total_angle, length.out = n + 1)  # boundaries of 67 segments


# 3. Generate polygon coordinates for each ring and ID
polygon_data <- data.frame()
for (i in 1:n) {
  a_start <- angles[i]
  a_end   <- angles[i+1]
  for (ring in 1:4) {
    r0 <- radii[ring]
    r1 <- radii[ring+1]
    # Four corners of the polygon (in order)
    coords <- data.frame(
      ID = df$ID[i],
      ring    = ring,
      x = c(r0*cos(a_start*pi/180), r0*cos(a_end*pi/180),
            r1*cos(a_end*pi/180), r1*cos(a_start*pi/180)),
      y = c(r0*sin(a_start*pi/180), r0*sin(a_end*pi/180),
            r1*sin(a_end*pi/180), r1*sin(a_start*pi/180))
    )
    polygon_data <- bind_rows(polygon_data, coords)
  }
}

# 4. Create line segments for rings 3 and 4
# calculate mean normalized values
mean_ppl_norm <- quantile(df$ppl_norm, 0.75)
mean_mon_norm <- quantile(df$mon_norm, 0.75)

segment_data <- data.frame()

for (i in 1:n) {
  mid_angle <- (angles[i] + angles[i+1]) / 2
  rad <- mid_angle * pi/180
  
  # Check if below mean → double the normalized value
  ppl_val <- ifelse(df$ppl_norm[i] < mean_ppl_norm, df$ppl_norm[i], df$ppl_norm[i])
  mon_val <- ifelse(df$mon_norm[i] < mean_mon_norm, df$mon_norm[i], df$mon_norm[i])
  
  # Ring 3: base radius = 2
  x0_3 <- 2 * cos(rad);   y0_3 <- 2 * sin(rad)
  x1_3 <- (2 + ppl_val) * cos(rad)
  y1_3 <- (2 + ppl_val) * sin(rad)
  
  # Ring 4: base radius = 3
  x0_4 <- 3 * cos(rad);   y0_4 <- 3 * sin(rad)
  x1_4 <- (3 + mon_val) * cos(rad)
  y1_4 <- (3 + mon_val) * sin(rad)
  
  segment_data <- bind_rows(segment_data,
                            data.frame(ID=df$ID[i], ring=3,
                                       x=x0_3, y=y0_3, xend=x1_3, yend=y1_3),
                            data.frame(ID=df$ID[i], ring=4,
                                       x=x0_4, y=y0_4, xend=x1_4, yend=y1_4))
}


# 5. Calculate label coordinates just outside outer ring (radius ~4.1)
label_data <- data.frame(
  ID = df$ID,
  angle = (angles[-(n+1)] + angles[-1]) / 2,
  x = 4.1 * cos(((angles[-(n+1)] + angles[-1]) / 2) * pi/180),
  y = 4.1 * sin(((angles[-(n+1)] + angles[-1]) / 2) * pi/180)
)


# 6. Assign fill colors
polygon_data <- polygon_data %>%
  left_join(df, by = "ID") %>%
  mutate(fill_color = case_when(
    ring == 1 & Huge == 1 ~  "#84CAC0",   
    ring == 1 & Huge == 0 ~ "#4387B5",     
    ring == 2 & Rising == 1   ~ "#8ecae6",        
    ring == 2 & Rising == 0   ~ "#219ebc",  
    TRUE ~ "white"                                # rings 3 and 4 → white background
  ))


# 7. Plot with ggplot2
p_base = ggplot() +
  # Rings 1-2: colored by category
  geom_polygon(data = polygon_data %>% filter(ring %in% 1:2),
               aes(x = x, y = y, group = interaction(ID, ring), fill = fill_color),
               color = "black") +
  scale_fill_identity()+
  # Rings 3-4: white background
  geom_polygon(data = polygon_data %>% filter(ring %in% 3:4),
               aes(x = x, y = y, group = interaction(ID, ring)),
               fill = "white", color = "black") +
  # Lollipop lines: ring 3 (people affected)
  geom_segment(data = segment_data %>% filter(ring == 3),
               aes(x = x, y = y, xend = xend, yend = yend),
               color = "salmon", linewidth = 1) +
  geom_point(data = segment_data %>% filter(ring == 3),
             aes(x = xend, y = yend),
             color = "salmon", size = 1.8) +
  # Lollipop lines: ring 4 (monetary loss)
  geom_segment(data = segment_data %>% filter(ring == 4),
               aes(x = x, y = y, xend = xend, yend = yend),
               color = "blue", linewidth = 1) +
  geom_point(data = segment_data %>% filter(ring == 4),
             aes(x = xend, y = yend),
             color = "blue", size = 1.8) +
  # ID labels at outer rim (rotated to match angle)
  geom_text(data = label_data,
            aes(x = x, y = y, label = ID, angle = angle),
            size = 3, hjust = 0.2, vjust = 0.5, color = "#333333", fontfamily= "sans") +
  coord_equal() +
  theme_void() +
  xlim(-4.5, 4.5) + ylim(-4.5, 4.5)
p_base

# Define points on radius 90° (x=0, y=r)
points_data <- data.frame(
  x = c( 0, 0),
  y = c( 3.5, 4.0),
  label = c("141", "1910")  # custom labels
)

points_data2 <- data.frame(
  x2 = c( 0, 0),
  y2 = c( 2.5, 2.94),
  label2 = c( "793", "14500")  # custom labels
)

# Add points
p_base <- p_base +
  geom_point(data = points_data, aes(x = x, y = y), color = "#333333", size = 1)+
  geom_text(data = points_data, aes(x = x-0.1, y = y, label = label),
            vjust = 0.3, color = "#333333", fontfamily= "sans", fontface = "italic",size =2)+
  geom_point(data = points_data2, aes(x = x2, y = y2), color = "#333333", size = 1)+
  geom_text(data = points_data2, aes(x = x2-0.08, y = y2, label = label2),
            vjust = -0.2, color = "#333333", fontfamily= "sans", fontface = "italic",size =2)
p_base


counts_dev <- df %>% count(Huge)
counts_rise <- df %>% count(Rising)
med_mon <- median(df$Feature_1)
med_people <- median(df$Feature_2)
q1_mon <- quantile(df$Feature_1, 0.25)
q3_mon <- quantile(df$Feature_1, 0.75)
q1_people <- quantile(df$Feature_2, 0.25)
q3_people <- quantile(df$Feature_2, 0.75)

iqr <- IQR(df$Feature_1)
q1 <- quantile(df$Feature_1, 0.25)
q3 <- quantile(df$Feature_1, 0.75)
df_filtered <- df %>%
  filter(Feature_1 >= (q1 - 1.2*iqr),
         Feature_1 <= (q3 + 1.2*iqr))

iqrX <- IQR(df$Feature_2)
q1X <- quantile(df$Feature_2, 0.25)
q3X <- quantile(df$Feature_2, 0.75)
df_filtered2 <- df %>%
  filter(Feature_2 >= (q1X - 1.5*iqr),
         Feature_2 <= (q3X + 1.5*iqr))

med_monX <- median(df_filtered$Feature_1)
med_peopleX <- median(df_filtered2$Feature_2)
q1_monX <- quantile(df_filtered$Feature_1, 0.25)
q3_monX <- quantile(df_filtered$Feature_1, 0.75)
q1_peopleX <- quantile(df_filtered2$Feature_2, 0.25)
q3_peopleX <- quantile(df_filtered2$Feature_2, 0.75)


# Boxplot for ring 4 (Feature_1)
# Calculate median

p_box4 <- ggplot(df_filtered, aes(x = factor(1), y = Feature_1)) +
  geom_boxplot(width = 0.5, fill = "white", outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "blue") +
  geom_label(aes(x = 0.7, y = med_monX, label = paste0("", round(med_mon,0))),
             fill = NA, label.size = 0, size = 3, colour = "#333333",
             fontfamily= "sans", fontface = "bold") +
  geom_label(aes(x = 0.7, y = q1_monX, label = paste0("", round(q1_mon,0))),
             fill = NA, label.size = 0, size = 3, colour = "#333333",
             fontfamily= "sans", fontface = "bold") +
  geom_label(aes(x = 0.7, y = q3_monX, label = paste0("", round(q3_mon,0))),
             fill = NA, label.size = 0, size = 3, colour = "#333333",
             fontfamily= "sans", fontface = "bold") +
  coord_cartesian(ylim = c(0, max(df_filtered$Feature_1)*1.2)) +
  theme_void()+
  geom_rect(aes(xmin = 1 - 0.4, xmax = 1 + 0.4, ymin = -0.5, ymax = max(df_filtered$Feature_1)*1.2),
            color = "black", fill = NA, inherit.aes = FALSE, size = 0.5)


# Boxplot for ring 3 (Feature_2)
p_box3 <- ggplot(df_filtered2, aes(x = factor(1), y = Feature_2)) +
  geom_boxplot(width = 0.5, fill = "white", outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "salmon") +
  geom_label(aes(x = 0.7, y = med_peopleX, label = paste0("", round(med_people,0))),
             fill = NA, label.size = 0, size = 3, colour = "#333333",
             fontfamily= "sans", fontface = "bold") +
  geom_label(aes(x = 0.7, y = q1_peopleX, label = paste0("", round(q1_people,0))),
             fill = NA, label.size = 0, size = 3, colour = "#333333",
             fontfamily= "sans", fontface = "bold") +
  geom_label(aes(x =0.7, y = q3_peopleX, label = paste0("", round(q3_people,0))),
             fill = NA, label.size = 0, size = 3, colour = "#333333",
             fontfamily= "sans", fontface = "bold") +
  coord_cartesian(ylim = c(0, max(df_filtered2$Feature_2)*1.2)) +
  theme_void()+
  geom_rect(aes(xmin = 1 - 0.4, xmax = 1 + 0.4, ymin =-0.5, ymax = max(df_filtered2$Feature_2)*1.2),
            color = "black", fill = NA, inherit.aes = FALSE, size = 0.5)


# Stacked bar for ring 2 (Rising: 0 vs 1)
p_bar2 <- ggplot(df, aes(x = factor(1), fill = factor(Rising))) +
  geom_bar(width = 0.6) +
  geom_rect(aes(xmin = 1 - 0.3, xmax = 1 + 0.3, ymin = 0, ymax = 67),
            color = "black", fill = NA, inherit.aes = FALSE, size = 0.5) +
  geom_label(data = counts_rise %>% mutate(pos = c(45, 10)),
             aes(x = 1, y = pos, label = paste0(ifelse(Rising==1, "", ""), n)),
             fill = NA, label.size = 0, size = 3, colour = "#333333",
             fontfamily= "sans", fontface = "bold") +
  scale_fill_manual(values = c("#219ebc","#8ecae6"),
                    labels = c("Fallback", "Rising")) +
  coord_cartesian(ylim = c(0, 67)) +
  theme_void() +
  theme(legend.position = 'none') +
  guides(fill = guide_legend(ncol = 2))


# Stacked bar for ring 1 (Huge: 0 vs 1)
# Calculate counts
p_bar1 <- ggplot(df, aes(x = factor(1), fill = factor(Huge))) +
  geom_bar(width = 0.6) +
  geom_rect(aes(xmin = 1 - 0.3, xmax = 1 + 0.3, ymin = 0, ymax = 67),
            color = "black", fill = NA, inherit.aes = FALSE, size = 0.5) +
  geom_label(data = counts_dev %>% mutate(pos = c(45, 10)),
             aes(x = 1, y = pos, label = paste0(ifelse(Huge==1, "", ""), n)),
             fill = NA, label.size = 0, size = 3, colour = "#333333",
             fontfamily= "sans", fontface = "bold") +
  scale_fill_manual(values = c("#4387B5", "#84CAC0"),
                    labels = c("Huge", "Small")) +
  coord_cartesian(ylim = c(0, 67)) +
  theme_void() +
  theme(legend.position = 'none') +
  guides(fill = guide_legend(ncol = 2))

#Adjustment of position
final_plot <- ggdraw(p_base) +
  draw_plot(p_box4,  x = 0.117, y = 0.50, width = 0.1, height = 0.4) +
  draw_plot(p_box3,  x = 0.213, y = 0.50, width = 0.1, height = 0.4) +
  draw_plot(p_bar2,  x = 0.311, y = 0.495, width = 0.1, height = 0.4) +
  draw_plot(p_bar1,  x = 0.405, y = 0.495, width = 0.1, height = 0.4)
final_plot

#图例
legend_plot <- ggplot(df, aes(x = factor(1), fill = factor(Huge))) +
  geom_bar() +
  labs(fill = NULL) +
  scale_fill_manual(values = c("#4387B5", "#84CAC0"),
                    labels = c("Huge", "Small")) +
  theme(legend.position = "right")

legend_plot2 <- ggplot(df, aes(x = factor(1), fill = factor(Rising))) +
  geom_bar() +
  labs(fill = NULL) +
  scale_fill_manual(values = c("#219ebc","#8ecae6"),
                    labels = c("Falling", "Rising")) +
  theme(legend.position = "right")

legend_grob <- get_legend(legend_plot)
legend_grob2 <- get_legend(legend_plot2)

final_plot_with_legend <- ggdraw(final_plot) +
  draw_plot(legend_grob, x = 0.77, y = 0.76, width = 0.2, height = 0.2) +
  draw_plot(legend_grob2, x = 0.77, y = 0.68, width = 0.2, height = 0.2)

final_plot_with_legend

