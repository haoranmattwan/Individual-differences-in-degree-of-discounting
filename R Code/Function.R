# Save as Function.R

floor <- function(x, level=1) round(x - 5*10^(-level-1), level)
ceiling <- function(x, level=1) round(x + 5*10^(-level-1), level)
error <- function(x) {
  r <- quantile(x, probs = c(0.025, 0.975))
  names(r) <- c("ymin", "ymax")
  r
}
mattheme <- theme(text = element_text(size = 14, 
                                      family = "Serif", color = "black", face = "bold"), 
                  axis.text.y = element_text(colour = "black", size = 12, face = "bold", family = "Serif"), 
                  axis.text.x = element_text(colour = "black", size = 12, face = "bold", angle = 0, family = "Serif"), 
                  axis.title.x = element_text(margin = margin(7, 0, 0, 0), size = 16), 
                  axis.title.y = element_text(margin = margin(0, 7, 0, 0), size = 16), 
                  axis.line.x = element_blank(), 
                  axis.line.y = element_blank(), 
                  plot.title = element_text(size = 16, face = "bold", hjust = 0.5, family = "Serif"), 
                  panel.background = element_rect(fill = "white", linetype = 1, color = "black", size = 1), 
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(), 
                  plot.background = element_rect(fill = "white"),
                  plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
                  strip.background =element_rect(fill="White", color = "black", size = 1),
                  strip.text = element_text(size=20),
                  legend.position = c(0.6, 0.8),
                  legend.key = element_rect(fill = "transparent"),
                  legend.background = element_rect(fill = "transparent"),
                  legend.text=element_text(family = "Serif", color = "black", face = "bold"),
                  legend.title.align=0.5,
                  plot.tag.position = c(.121, .05),
                  plot.tag = element_text(colour = "black", size = 12, family = "Serif"),
                  axis.text.y.right = element_blank())