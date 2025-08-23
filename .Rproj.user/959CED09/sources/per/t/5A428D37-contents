library("tidyverse")
library("patchwork")
library(png)
library(grid)
img1 <- readPNG("doc/symb/Full_slab.png")
g1<- rasterGrob(img1, interpolate=TRUE)

img2 <- readPNG("doc/symb/Trough.png")
g2<- rasterGrob(img2, interpolate=TRUE)

#data <- read.csv("dat/raw/data_all.csv")
#saveRDS(data, file = "dat/raw/data_all.rds")
all_dat <- read_rds("dat/raw/data_all.rds") |>
  mutate(profile=recode_factor(profile,"1"="Full slab","4"="Trough"),
         noWood=recode_factor(noWood,"0"="without wood","1"="with wood"))

p1 <- ggplot(all_dat, aes(x=as.factor(reorder(noWood, -Res)), y=v, fill=as.factor(reorder(noWood, -Res)))) +
  geom_boxplot(alpha=0.8) +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 20),
    text = element_text(size = 20),
    axis.title.y = element_text(angle = 90, vjust = 0.5, size = 20),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(), 
    axis.title.y.right = element_text(angle = 0, vjust = 0.5)
  ) +
  scale_y_continuous(breaks = scales::breaks_extended(n = 5)) +
  labs(x = "", y = expression(italic(velocity) ~ " [m/s]")) +
  geom_vline(aes(xintercept = 1.5), linetype = "dashed", colour = "black", size = 0.7) +
  scale_fill_manual(values = c("#A27146", "#bdbdbd"), labels = c("with wood","without wood"), name = "") 

p1
 ggsave("plt/front_velocity.png", plot = p1, device = png, height = 15, width = 22.5, dpi = 300, units = "cm")
 
 p2 <- ggplot(all_dat, aes(x=as.factor(reorder(noWood, -Res)), y=h,fill=as.factor(reorder(noWood, -Res))))+
   geom_boxplot(alpha=0.8)+
   theme_bw()+
   theme(
     legend.position = "top",
     legend.text = element_text(size = 20),
     text = element_text(size = 20),
     axis.title.y = element_text(angle = 90, vjust = 0.5, size = 20),
     axis.title.x = element_blank(),
     axis.text.x = element_blank(),  
     axis.title.y.right = element_text(angle = 0, vjust = 0.5)
   ) +
   scale_y_continuous(breaks = scales::breaks_extended(n = 5))+
   labs( x="",y = expression(italic(flow~depth)~" [mm]"))+
   geom_vline(aes(xintercept=1.5),linetype="dashed",colour="black",size=0.7)+
   #scale_color_manual(values = c("#bdbdbd", "#A27146"),labels=c("with wood","without wood"), name = "")+
   scale_fill_manual(values = c("#A27146","#bdbdbd"),labels=c("with wood","without wood"), name = "")
 p2
 ggsave("plt/flow_depth.png", plot = p2, device = png, height = 15, width = 22.5, dpi = 300, units = "cm")
 