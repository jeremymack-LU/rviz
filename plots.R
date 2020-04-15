library(ggplot2)
theme <- theme(panel.background=element_rect(color="black", fill="lightyellow", size=0.25),
               panel.grid=element_blank(),
               plot.title=element_text(size=9, color="black"),
               strip.background=element_rect(color="black", size=0.25),
               axis.ticks=element_line(size=0.25),
               axis.text=element_text(size=7, color="black"),
               axis.title=element_text(size=8, color="black"),
               legend.title=element_text(size=8, color="black"),
               legend.text=element_text(size=7, color="black"),
               legend.justification="top",
               legend.key=element_rect(color="black", fill="lightyellow", size=0.1))
#######################################################
p1 <- ggplot(data=faithful) + theme

png(file="images/p1.png",
    width=2800, height=1600, units="px", res=600)
p1
dev.off()
#######################################################
p2 <- ggplot(data=faithful,
       mapping=aes(x=eruptions,
                   y=waiting)) + 
  theme

png(file="images/p2.png",
    width=2800, height=1600, units="px", res=600)
p2
dev.off()
#######################################################
p3 <- ggplot(data=faithful,
             mapping=aes(x=eruptions,
                         y=waiting)) +
  geom_point(size=0.5) +
  theme

png(file="images/p3.png",
    width=2800, height=1600, units="px", res=600)
p3
dev.off()
#######################################################
p4 <- ggplot(data=faithful,
       mapping=aes(x=eruptions,
                   y=waiting)) +
  geom_point(size=0.5, aes(color=eruptions < 3)) +
  theme

png(file="images/p4.png",
    width=2800, height=1600, units="px", res=600)
p4
dev.off()
#######################################################
p5 <- ggplot(data=faithful,
             mapping=aes(x=eruptions,
                         y=waiting)) +
  geom_point(size=0.5, color='blue') +
  theme

png(file="images/p5.png",
    width=2800, height=1600, units="px", res=600)
p5
dev.off()
#######################################################
p5b <- ggplot(data=faithful,
             mapping=aes(x=eruptions,
                         y=waiting)) +
  geom_point(size=0.5, color='blue') +
  # Microsoft Excel theme
  theme_excel()

png(file="images/p5b.png",
    width=2800, height=1600, units="px", res=600)
p5b
dev.off()
#######################################################
p5c <- ggplot(data=faithful,
              mapping=aes(x=eruptions,
                          y=waiting)) +
  geom_point(size=0.5, color='blue') +
  # Wall Street Journal theme
  theme_wsj()

png(file="images/p5c.png",
    width=2800, height=1600, units="px", res=600)
p5c
dev.off()
#######################################################
p6 <- ggplot(data=faithful,
             mapping=aes(x=eruptions)) +
  geom_histogram() +
  theme

png(file="images/p6.png",
    width=2800, height=1600, units="px", res=600)
p6
dev.off()
#######################################################
p7 <- ggplot(data=faithful,
             mapping=aes(x=eruptions,
                         y=waiting)) +
  geom_point(size=0.5) +
  geom_smooth(method="lm", size=2) +
  theme

png(file="images/p7.png",
    width=2800, height=1600, units="px", res=600)
p7
dev.off()
#######################################################
p7b <- ggplot(data=faithful,
             mapping=aes(x=eruptions,
                         y=waiting)) +
  geom_smooth(method="lm", size=2) +
  geom_point(size=0.5) +
  theme

png(file="images/p7b.png",
    width=2800, height=1600, units="px", res=600)
p7b
dev.off()
#######################################################
pw1 <- ggplot(data=faithful,
              mapping=aes(x=eruptions,
                          y=waiting)) +
  geom_point(size=0.5) +
  theme
pw2 <- ggplot(data=faithful,
              mapping=aes(x=eruptions,
                          y=waiting)) +
  geom_point(size=0.5) +
  geom_smooth(method="lm") +
  theme
pw3 <- ggplot(data=faithful,
              mapping=aes(x=eruptions)) +
  geom_histogram() +
  theme

png(file="images/pw1.png",
    width=2800, height=1600, units="px", res=600)
pw1 + pw2
dev.off()

png(file="images/pw2.png",
    width=2800, height=1600, units="px", res=600)
pw1 / pw2
dev.off()

png(file="images/pw3.png",
    width=2800, height=1600, units="px", res=600)
(pw1 + pw2) / pw3
dev.off()

#######################################################
p8a <- ggplot(data=gapminder,
       aes(x=gdpPercap, y=lifeExp, size=pop, color=country)) +
  # Add a point geom
  geom_point(alpha=0.7, show.legend=FALSE) +
  theme

png(file="images/p8a.png",
    width = 2100, height = 900, res = 300)
p8a
dev.off()
#######################################################
p8b <- ggplot(data=gapminder,
              aes(x=gdpPercap, y=lifeExp, size=pop, color=country)) +
  # Add a point geom
  geom_point(alpha=0.7, show.legend=FALSE) +
  # Add some manual scaling 
  scale_colour_manual(values=country_colors) +
  scale_size(range=c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent, nrow=1) +
  theme

png(file="images/p8b.png",
    width = 2100, height = 900, res = 300)
p8b
dev.off()
#######################################################
anim <- ggplot(data=gapminder,
               aes(x=gdpPercap, y=lifeExp, size=pop, color=country)) +
  # Add a point geom
  geom_point(alpha=0.7, show.legend=FALSE) +
  # Add some manual scaling 
  scale_colour_manual(values=country_colors) +
  scale_size(range=c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent, nrow=1) +
  # Animate figure with gganimate package
  transition_time(year) +
  ease_aes('linear') +
  labs(title='Year: {frame_time}', 
       x='GDP per capita', 
       y='Life expectancy at birth') +
  theme

options(gganimate.dev_args = list(width = 2100, height = 900, res = 300))

anim_save(anim, filename="gapminder.gif", path="./images")
#######################################################
p9 <- ggplot(data=gapminder, 
             aes(x=gdpPercap, y=lifeExp, size=pop, color=continent)) +
  # Add a point geom
  geom_point(alpha=0.7, show.legend=FALSE, aes(frame = year, ids = country)) +
  # Add some manual scaling
  scale_colour_manual(values=continent_colors) +
  scale_size(range=c(2, 12)) +
  scale_x_log10() +
  labs(x='GDP per capita', 
       y='Life expectancy at birth') +
  theme

fig <- ggplotly(p9, height=500, width=750) %>%
  animation_opts(
    1000, easing = "linear", redraw = FALSE) %>%
  animation_slider(
    currentvalue=list(prefix="YEAR ",
                      font=list(color="red"),
                      xanchor='left',
                      yanchor='top')) %>%
  animation_button(
    x = 1, xanchor = "right", y = -0.2, yanchor = "bottom"
  )

fig

htmlwidgets::saveWidget(fig, "test.html")
#######################################################
