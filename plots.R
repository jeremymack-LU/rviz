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
p5a <- ggplot(data=faithful,
             mapping=aes(x=eruptions,
                         y=waiting,
                         color='blue')) +
  geom_point(size=0.5) +
  theme

png(file="images/p5a.png",
    width=2800, height=1600, units="px", res=600)
p5a
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
p5e <- ggplot(data=faithful) +
  geom_point(mapping=aes(
    x=eruptions,
    y=waiting,
    color=eruptions < 3,
    size=waiting),
    alpha=0.5) +
  scale_size_continuous(range=c(0.5,4)) +
  theme

png(file="images/p5e.png",
    width=2800, height=1600, units="px", res=600)
p5e
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

png(file="images/pw5.png",
    width=2800, height=1600, units="px", res=600)
pw1 + pw2 + plot_layout(widths=c(2,1))
dev.off()

pw1 <- pw1 + theme(axis.title = element_blank())
pw2 <- pw2 + theme(axis.title = element_blank())
patch    <- pw1 + pw2
patch.gr <- patchworkGrob(patch)

png(file="images/pw4.png",
    width=2800, height=1600, units="px", res=600)
grid.arrange(patch.gr, 
             left=textGrob("Time until next eruption", gp=gpar(fontsize=8), rot=90),
             bottom=textGrob("Length of eruption", gp=gpar(fontsize=8)))
dev.off()

library(ggstatsplot)
stats <- 

png(file="images/pw4.png",
    width=2800, height=1600, units="px", res=600)
stats
dev.off()

#######################################################
p8a <- ggplot(data=gapminder,
       aes(x=gdpPercap, y=lifeExp, size=pop, color=country)) +
  # Add a point geom
  geom_point(alpha=0.7, show.legend=FALSE) +
  theme

png(file="images/stats.png",
    width = 2100, height = 1300, res = 300)
ggscatterstats(
  data = faithful,
  x = eruptions,
  y = waiting,
  xlab = "Length of eruption",
  ylab = "Time until next eruption",
  title = "How long until Old Faithful erupts?")
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
library(patchwork)
library(sf)
library(urbnmapr)
library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)

states_sf <- get_urbn_map("states", sf = TRUE)

pop <- "https://drive.google.com/uc?open&id=1gAdOoa097FFAkNIgriG0bX2iAPNpXXIS"
pop <- read.csv(pop, sep=",", header=TRUE)
pop <- pop %>% mutate(population=as.numeric(gsub(",","",population)))
names(pop)[1] <- "state_name"

ts <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
ts <- read.csv(ts, sep=",", header=TRUE)

ts <- ts %>% 
  filter(iso2=="US") %>%
  filter(!Province_State %in% c("Diamond Princess", "Grand Princess")) %>%
  select(5:7, 12:ncol(.)) %>%
  gather(date, cases, 4:ncol(.), factor_key=TRUE) %>% 
  arrange(FIPS, date) %>% 
  group_by(FIPS) %>%
  droplevels() %>%
  as.data.frame()

states <- ts %>%
  group_by(Province_State, date) %>%
  summarize(cases=sum(cases)) %>%
  as.data.frame()

count <- states %>% 
  group_by(Province_State) %>% 
  summarize(count=length(Province_State))

dates <- data.frame(rep(seq(as.Date('2020-01-22'), 
                            as.Date('2020-01-22') + as.numeric(count[1,2]-1),
                            by = 'days'),
                        times = 51))
names(dates) <- "date"

df.us <- cbind(states, dates)
df.us <- df.us[,c(1,4,3)]
names(df.us)[1] <- "state_name"
df.us <- df.us %>%
  group_by(state_name) %>%
  mutate(new=cases-lag(cases, default=0)) %>%
  mutate(new7=rollapply(new,7,mean,fill=0,align="right"))

df.us2 <- df.us %>%
  group_by(state_name) %>%
  arrange(date) %>%
  slice(n()) %>%
  right_join(pop, by="state_name") %>%
  mutate(new7b=round(new7/(population/100000),1))

states_sf <- states_sf %>% 
  right_join(df.us2, by="state_name")

states_sf <- states_sf %>%
  mutate(bin=cut(new7b,
                 breaks=c(0,0.9,9.5,24.5,1000),
                 labels=c("A","B","C","D")))

cols <- c("A"="#197d7d", "B"="#dbc037", "C"="#e08f38", "D"="#8C1111")

usa <- ggplot() +
  geom_sf(data=states_sf, aes(fill=bin),
          size=0.25, color="white") +
  scale_fill_manual(values=cols) +
  theme_void() +
  theme(
    legend.position="none",
    text=element_text(color="#22211d"),
    plot.background=element_rect(fill="white", color=NA), 
    panel.background=element_rect(fill="white", color=NA), 
    plot.title=element_text(size= 12, hjust=0.5, color="#4e4d47", 
                            margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle=element_text(size=8, hjust=0.5, color="#4e4d47", face="italic",
                               margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))

usa2 <- ggplot() +
  geom_sf(data=states_sf, aes(fill=bin),
          size=0.25, color="white") +
  scale_fill_manual(values=cols) +
  coord_sf(crs=st_crs(3857)) +
  theme_void() +
  theme(
    legend.position="none",
    text=element_text(color="#22211d"),
    plot.background=element_rect(fill="white", color=NA), 
    panel.background=element_rect(fill="white", color=NA), 
    plot.title=element_text(size= 12, hjust=0.5, color="#4e4d47", 
                            margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle=element_text(size=8, hjust=0.5, color="#4e4d47", face="italic",
                               margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))

leg.R <- ggplot() +
  annotate(geom="rect", xmin=0, xmax=2, ymin=1.8, ymax=2, color=NA, fill="#8C1111") +
  annotate(geom="text", label="RED", x=1, y=1.9, color="white", size=3) +
  annotate(geom="text",
           label="Threshold:",
           x=0, hjust=0, y=1.75, vjust=1, size=2.5, fontface="bold") +
  annotate(geom="text",
           label="                    25+ daily\nnew cases per 100,000\npeople",
           x=0, hjust=0, y=1.75, vjust=1, size=2.5) +
  annotate(geom="text",
           label="Indicates:",
           x=0, hjust=0, y=1.3, vjust=1, size=2.5, fontface="bold") +
  annotate(geom="text",
           label="                  unchecked\ncommunity spread",
           x=0, hjust=0, y=1.3, vjust=1, size=2.5) +
  scale_y_continuous(limits=c(1,2)) +
  theme_void() +
  theme(
    text=element_text(color="#22211d"),
    plot.background=element_rect(fill="white", color=NA), 
    panel.background=element_rect(fill="white", color=NA), 
    legend.background=element_rect(fill="white", color=NA)
  )

leg.O <- ggplot () +
  annotate(geom="rect", xmin=2.5, xmax=4.5, ymin=1.8, ymax=2, color=NA, fill="#e08f38") +
  annotate(geom="text", label="ORANGE", x=3.5, y=1.9, color="white", size=3) +
  annotate(geom="text",
           label="Threshold:",
           x=2.5, hjust=0, y=1.75, vjust=1, size=2.5, fontface="bold") +
  annotate(geom="text",
           label="                    10-24 daily\nnew cases per 100,000\npeople",
           x=2.5, hjust=0, y=1.75, vjust=1, size=2.5) +
  annotate(geom="text",
           label="Indicates:",
           x=2.5, hjust=0, y=1.3, vjust=1, size=2.5, fontface="bold") +
  annotate(geom="text",
           label="                  escalating\ncommunity spread",
           x=2.5, hjust=0, y=1.3, vjust=1, size=2.5) +
  scale_y_continuous(limits=c(1,2)) +
  theme_void() +
  theme(
    text=element_text(color="#22211d"),
    plot.background=element_rect(fill="white", color=NA), 
    panel.background=element_rect(fill="white", color=NA), 
    legend.background=element_rect(fill="white", color=NA)
  )

leg.Y <- ggplot() +
  annotate(geom="rect", xmin=5, xmax=7, ymin=1.8, ymax=2, color=NA, fill="#dbc037") +
  annotate(geom="text", label="YELLOW", x=6, y=1.9, color="white", size=3) +
  annotate(geom="text",
           label="Threshold:",
           x=5, hjust=0, y=1.75, vjust=1, size=2.5, fontface="bold") +
  annotate(geom="text",
           label="                    1-9 daily\nnew cases per 100,000\npeople",
           x=5, hjust=0, y=1.75, vjust=1, size=2.5) +
  annotate(geom="text",
           label="Indicates:",
           x=5, hjust=0, y=1.3, vjust=1, size=2.5, fontface="bold") +
  annotate(geom="text",
           label="                  potential\ncommunity spread",
           x=5, hjust=0, y=1.3, vjust=1, size=2.5) +
  scale_y_continuous(limits=c(1,2)) +
  theme_void() +
  theme(
    text=element_text(color="#22211d"),
    plot.background=element_rect(fill="white", color=NA), 
    panel.background=element_rect(fill="white", color=NA), 
    legend.background=element_rect(fill="white", color=NA)
  )

leg.G <- ggplot() +
  annotate(geom="rect", xmin=7.5, xmax=9.5, ymin=1.8, ymax=2, color=NA, fill="#197d7d") +
  annotate(geom="text", label="GREEN", x=8.5, y=1.9, color="white", size=3) +
  annotate(geom="text",
           label="Threshold:",
           x=7.5, hjust=0, y=1.75, vjust=1, size=2.5, fontface="bold") +
  annotate(geom="text",
           label="                    < 1 daily\nnew cases per 100,000\npeople",
           x=7.5, hjust=0, y=1.75, vjust=1, size=2.5) +
  annotate(geom="text",
           label="Indicates:",
           x=7.5, hjust=0, y=1.3, vjust=1, size=2.5, fontface="bold") +
  annotate(geom="text",
           label="                  close to\ncontainment",
           x=7.5, hjust=0, y=1.3, vjust=1, size=2.5) +
  scale_y_continuous(limits=c(1,2)) +
  theme_void() +
  theme(
    text=element_text(color="#22211d"),
    plot.background=element_rect(fill="white", color=NA), 
    panel.background=element_rect(fill="white", color=NA), 
    legend.background=element_rect(fill="white", color=NA)
  )

jpeg(file="images/covid19a.jpeg",
     width=7,height=4.75,
     units="in",
     res=1200)
usa + (leg.G/leg.Y/leg.O/leg.R) + plot_layout(widths = c(4, 1)) + plot_annotation(
  title='Which Places Have The Most New Daily Cases?',
  subtitle=paste("Data as of 11:59 p.m. ET", Sys.Date()-1)
) & theme(plot.title=element_text(size= 12, hjust=0.5, color="#4e4d47", 
                                  margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
          plot.subtitle=element_text(size=8, hjust=0.5, color="#4e4d47", face="italic",
                                     margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))
dev.off()

jpeg(file="images/covid19b.jpeg",
     width=7,height=4.75,
     units="in",
     res=1200)
usa2 + (leg.G/leg.Y/leg.O/leg.R) + plot_layout(widths = c(4, 1)) + plot_annotation(
  title='Which Places Have The Most New Daily Cases?',
  subtitle=paste("Data as of 11:59 p.m. ET", Sys.Date()-1)
) & theme(plot.title=element_text(size= 12, hjust=0.5, color="#4e4d47", 
                                  margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
          plot.subtitle=element_text(size=8, hjust=0.5, color="#4e4d47", face="italic",
                                     margin=margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))
dev.off()

jpeg(file="images/covid19c.jpeg",
     width=7,height=4.75,
     units="in",
     res=1200)
usa
dev.off()

jpeg(file="images/covid19h.jpeg",
     width=7,height=4.75,
     units="in",
     res=1200)
usa2
dev.off()

# Load necessary packages
pacman::p_load(tidyverse, raster, ggthemes, sp, sf, rasterVis, rayshader)
# Load raster dataset
r    <- raster("data/CRLA_DEM_fill2.tif")
r.df <- as.data.frame(r, xy=TRUE)

r.plot <- ggplot() +
  geom_raster(data=r.df,
              aes(x=x, y=y, fill=CRLA_DEM_fill2),
              interpolate=TRUE,
              show.legend=FALSE) +
  scale_fill_gradientn(name = "Elevation", colors=terrain.colors(10)) +
  coord_quickmap() +
  theme_dark(base_size=6) +
  theme(axis.title=element_blank())

png(file="images/map.png",
    width = 2100, height = 900, res = 300)
r.plot
dev.off()

plot <- ggplot(data=faithful,
               mapping=aes(
                 x=eruptions,
                 y=waiting)) +
  geom_smooth(method="lm") +
  geom_point(size=0.25)

plot <- plot +
  theme(plot.background=element_blank(),
        panel.border=element_blank(),
        panel.background=element_blank(),
        axis.ticks=element_line(size=0.25),
        axis.text=element_text(size = 7),
        axis.line=element_line(size=0.25),
        axis.title=element_text(size = 7),
        legend.text=element_text(size = 7),
        legend.title=element_text(size = 7),
        legend.key=element_blank(),
        plot.title=element_text(hjust=.5, size=8)) +
  labs(title="Old Faithful Waiting Time",
       x="Length of eruption (minutes)",
       y="Waiting time to next eruption (minutes)")

png(file="images/faithful.png",
    width=2800, height=1600, units="px", res=600)
plot
dev.off()

png(file="images/pa_state.png",
    width=2200, height=1600, units="px", res=600)
non_sf %>% 
  filter(abbr=="PA") %>%
  ggplot(aes(x=x,y=y,group=group)) +
  geom_polygon(size=0.2,fill='lightblue',color='black') +
  geom_point(size=0.2) +
  coord_fixed() +
  ggthemes::theme_map()
dev.off()

png(file="images/pa_state2.png",
    width=2200, height=1600, units="px", res=600)
states_sf %>%
  filter(state_abbv=='PA') %>%
  ggplot() +
  geom_sf(size=0.2,color='black',fill='lightblue') +
  ggthemes::theme_map()
dev.off()

pacman::p_load(gapminder)

png(file="images/plotly.png",
    width=2800, height=1600, units="px", res=600)
ggplot(data=gapminder, 
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
dev.off()
