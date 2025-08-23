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
  mutate(profile=recode_factor(profile,"1"="Full slab","4"="Through"),
         noWood=recode_factor(noWood,"0"="without wood","1"="with wood"))

# ####################################
# # Boxplots of residuals between experiments without wood and with wood per profile
# ####################################

# p1 <- ggplot(all_dat, aes(x=profile, y=Res,fill=as.factor(reorder(noWood, -Res))))+
#   geom_boxplot(alpha=0.8)+
#   theme_bw()+
#   theme(legend.position="top",legend.text=element_text(size=16),text = element_text(size=16),axis.title.y = element_text(angle = 90, vjust = 0.5,size = 25),axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y.right = element_text(angle = 0, vjust = 0.5))+
#   scale_y_continuous(breaks = scales::breaks_extended(n = 5), limits = c(5,125))+
#   labs( x="",y = expression(italic(F[F])~" [N]"))+
#   geom_vline(aes(xintercept=1.5),linetype="dashed",colour="black",size=0.7)+
#   #scale_color_manual(values = c("#bdbdbd", "#A27146"),labels=c("with wood","without wood"), name = "")+
#   scale_fill_manual(values = c("#A27146","#bdbdbd"),labels=c("with wood","without wood"), name = "")
#   #coord_cartesian(clip = 'off') +
#   #annotation_custom(g1, x = 0.75, y = -10, ymax = -16, xmax = 1.25)+
#   #annotation_custom(g2, x = 1.75, y = -10, ymax = -16, xmax = 2.25)
# # p1
# #ggsave("plt/res.png", plot = p1, device = png, height = 15, width = 22.5, dpi = 300, units = "cm")
# 
#  p2 <- ggplot(all_dat, aes(x=profile, y=Resy,fill=as.factor(reorder(noWood, -Res))))+
#    geom_boxplot(alpha=0.8)+
#    theme_bw()+
#   theme(legend.position="top",legend.text=element_text(size=16),text = element_text(size=16),axis.title.y = element_text(angle = 90, vjust = 0.5,size = 25),axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y.right = element_text(angle = 0, vjust = 0.5))+
#    scale_y_continuous(breaks = scales::breaks_extended(n = 5), limits = c(5,125))+
#    labs( x="",y = bquote(italic(F[F]~~(t[F[Y~","~max]]))~" [N]"))+
#    geom_vline(aes(xintercept=1.5),linetype="dashed",colour="black",size=0.7)+
#    #scale_color_manual(values = c("#bdbdbd", "#A27146"),labels=c("without wood","with wood"), name = "")+
#    scale_fill_manual(values = c("#A27146","#bdbdbd"),labels=c("with wood","without wood"), name = "")
#    #coord_cartesian(clip = 'off') +
#    #annotation_custom(g1, x = 0.75, y = -10, ymax = -16, xmax = 1.25)+
#    #annotation_custom(g2, x = 1.75, y = -10, ymax = -16, xmax = 2.25)
# #p2
#  #ggsave("plt/res1.png", plot = p2, device = png, height = 15, width = 22.5, dpi = 300, units = "cm")
#  
#  p3 <- ggplot(all_dat, aes(x=profile, y=Reszm,fill=as.factor(reorder(noWood, -Res))))+
#    geom_boxplot(alpha=0.8)+
#    theme_bw()+
#    theme(legend.position = "none",legend.text=element_text(size=16),text = element_text(size=25),axis.title.y = element_text(angle = 90, vjust = 0.5,size = 25),axis.text.x = element_text(angle = 0,hjust = 0.5, vjust = 0.5),axis.title.x = element_text(vjust = 0.5,size = 16),axis.title.y.right = element_text(angle = 0, vjust = 0.5))+
#    scale_y_continuous(breaks = scales::breaks_extended(n = 5), limits = c(5,125))+
#    labs( x="",y = bquote(italic(F[F]~~(t[F[Z~","~max]]))~" [N]"))+
#    geom_vline(aes(xintercept=1.5),linetype="dashed",colour="black",size=0.7)+
#    #scale_color_manual(values = c("#bdbdbd", "#A27146"),labels=c("without wood","with wood"), name = "")+
#    scale_fill_manual(values = c("#A27146","#bdbdbd"),labels=c("with wood","without wood"), name = "")+
#    coord_cartesian(clip = 'off') +
#    annotation_custom(g1, x = 0.75, y = -10, ymax = -16, xmax = 1.25)+
#    annotation_custom(g2, x = 1.75, y = -10, ymax = -16, xmax = 2.25)
#  #p3
#  #ggsave("plt/res2.png", plot = p3, device = png, height = 15, width = 22.5, dpi = 300, units = "cm")
#  
#  p4 <- ggplot(all_dat, aes(x=profile, y=Resz,fill=as.factor(reorder(noWood, -Res))))+
#    geom_boxplot(alpha=0.8)+
#    theme_bw()+
#    theme(legend.position = "none",legend.text=element_text(size=16),text = element_text(size=25),axis.title.y = element_text(angle = 90, vjust = 0.5,size = 25),axis.text.x = element_text(angle = 0,hjust = 0.5, vjust = 0.5),axis.title.x = element_text(vjust = 0.5,size = 16),axis.title.y.right = element_text(angle = 0, vjust = 0.5))+
#    scale_y_continuous(breaks = scales::breaks_extended(n = 5), limits = c(5,125))+
#    labs( x="",y = bquote(italic(F[F]~~(t[F[Z~","~min]]))~" [N]"))+
#    geom_vline(aes(xintercept=1.5),linetype="dashed",colour="black",size=0.7)+
#    #scale_color_manual(values = c("#bdbdbd", "#A27146"),labels=c("without wood","with wood"), name = "")+
#    scale_fill_manual(values = c("#A27146","#bdbdbd"),labels=c("with wood","without wood"), name = "")+
#    coord_cartesian(clip = 'off') +
#    annotation_custom(g1, x = 0.75, y = -10, ymax = -16, xmax = 1.25)+
#    annotation_custom(g2, x = 1.75, y = -10, ymax = -16, xmax = 2.25)
#  #p4
#  #ggsave("plt/res3.png", plot = p4, device = png, height = 15, width = 22.5, dpi = 300, units = "cm")
#  patchwork <- (p1+p2)/(p3+p4)+ plot_layout(guides = 'collect') & theme(legend.position="top",legend.text=element_text(size=30),text = element_text(size=30))
#  ggsave("plt/resall.png", plot = patchwork, device = png, height = 35, width = 45, dpi = 300, units = "cm")
 # ####################################
 # # Boxplots of maximum forces between experiments without wood and with wood per profile
 # ####################################
 p1 <- ggplot(all_dat, aes(x=profile, y=Yy,fill=as.factor(reorder(noWood, -Res))))+
   geom_boxplot(alpha=0.8)+
   theme_bw()+
   theme(legend.position="top",legend.text=element_text(size=20),text = element_text(size=20),axis.title.y = element_text(angle = 90, vjust = 0.5,size = 20),axis.text.x = element_text(angle = 0,hjust = 0.5, vjust = 0.5),axis.title.x = element_text(vjust = 0.5,size = 20),axis.title.y.right = element_text(angle = 0, vjust = 0.5))+
   scale_y_continuous(breaks = scales::breaks_extended(n = 5), limits = c(5,125))+
   labs( x="",y = expression(italic(F[Y][max])~" [N]"))+
   geom_vline(aes(xintercept=1.5),linetype="dashed",colour="black",size=0.7)+
   #scale_color_manual(values = c("#bdbdbd", "#A27146"),labels=c("without woody debris","with woody debris"), name = "")+
   scale_fill_manual(values = c("#A27146","#bdbdbd"),labels=c("with wood","without wood"), name = "")+
   coord_cartesian(clip = 'off') +
   annotation_custom(g1, x = 0.75, y = -8, ymax = -13, xmax = 1.25)+
   annotation_custom(g2, x = 1.75, y = -8, ymax = -13, xmax = 2.25)
 p1
 ggsave("plt/Fymax.png", plot = p1, device = png, height = 15, width = 22.5, dpi = 300, units = "cm")
 
 p2 <- ggplot(all_dat, aes(x=profile, y=Zzm,fill=as.factor(reorder(noWood, -Res))))+
   geom_boxplot(alpha=0.8)+
   theme_bw()+
   theme(legend.position="top",legend.text=element_text(size=20),text = element_text(size=20),axis.title.y = element_text(angle = 90, vjust = 0.5,size = 20),axis.text.x = element_text(angle = 0,hjust = 0.5, vjust = 0.5),axis.title.x = element_text(vjust = 0.5,size = 20),axis.title.y.right = element_text(angle = 0, vjust = 0.5))+
   scale_y_continuous(breaks = scales::breaks_extended(n = 5), limits = c(2,25))+
   labs( x="",y = expression(italic(F[Z][max])~" [N]"))+
   geom_vline(aes(xintercept=1.5),linetype="dashed",colour="black",size=0.7)+
   #scale_color_manual(values = c("#bdbdbd", "#A27146"),labels=c("without wood","with wood"), name = "")+
   scale_fill_manual(values = c("#A27146","#bdbdbd"),labels=c("with wood","without wood"), name = "")+
   coord_cartesian(clip = 'off') +
   annotation_custom(g1, x = 0.84, y = -4, ymax = 2.03, xmax = 1.16)+
   annotation_custom(g2, x = 1.84, y = -4, ymax = 2.03, xmax = 2.16)
 p2
 ggsave("plt/Fzmax.png", plot = p2, device = png, height = 15, width = 22.5, dpi = 300, units = "cm")
 
 p3 <- ggplot(all_dat, aes(x=profile, y=Zz,fill=as.factor(reorder(noWood, -Res))))+
   geom_boxplot(alpha=0.8)+
   theme_bw()+
   theme(legend.position="top",legend.text=element_text(size=20),text = element_text(size=20),axis.title.y = element_text(angle = 90, vjust = 0.5,size = 20),axis.text.x = element_text(angle = 0,hjust = 0.5, vjust = 0.5),axis.title.x = element_text(vjust = 0.5,size = 20),axis.title.y.right = element_text(angle = 0, vjust = 0.5))+
   scale_y_continuous(breaks = scales::breaks_extended(n = 5), limits = c(-95,0))+
   labs( x="",y = expression(italic(F[Z][min])~" [N]"))+
   geom_vline(aes(xintercept=1.5),linetype="dashed",colour="black",size=0.7)+
   #scale_color_manual(values = c("#bdbdbd", "#A27146"),labels=c("with wood","without wood"), name = "")+
   scale_fill_manual(values = c("#A27146","#bdbdbd"),labels=c("with wood","without wood"), name = "")+
   coord_cartesian(clip = 'off') +
   annotation_custom(g1, x = 0.86, y = -102, ymax = -113, xmax = 1.16)+
   annotation_custom(g2, x = 1.86, y = -102, ymax = -113, xmax = 2.16)
 p3
 ggsave("plt/Fzmin.png", plot = p3, device = png, height = 15, width = 22.5, dpi = 300, units = "cm")
 #patchwork <- p2+p3+ plot_layout(guides = 'collect') & theme(legend.position="top",legend.text=element_text(size=25),text = element_text(size=25))
 #ggsave("plt/Fzall.png", plot = patchwork, device = png, height = 35, width = 45, dpi = 300, units = "cm")
 
 