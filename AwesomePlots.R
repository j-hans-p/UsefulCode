library(ggplot2)
library(gapminder)
library(gganimate)
library(ggExtra)
library(ggcorrplot)
library(quantmod)
library(scales)
library(ggalt)
library(ggthemes)
library(treemapify)
library(ggfortify)
library(lubridate)

### SOURCE ###
### http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
### SOURCE ###

####################################################
#Scatter Plot
####################################################
#Uses: library(ggplot2)
# load data
options(scipen=999)  # turn-off scientific notation like 1e+48
theme_set(theme_bw())  # pre-set the bw theme.
data("midwest", package = "ggplot2")
midwest <- read.csv("http://goo.gl/G1K41K")  # bkup data source

scatterplot <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state, size=popdensity)) + 
  geom_smooth(method="loess", se=F) + 
  xlim(c(0, 0.1)) + 
  ylim(c(0, 500000)) + 
  labs(subtitle="Area Vs Population", 
       y="Population", 
       x="Area", 
       title="Scatterplot", 
       caption = "Source: midwest")

png(filename = "scatterplot.png")
plot(scatterplot) 
dev.off()

####################################################
#Scatterplot with encirlcing 
####################################################
#uses library(ggplot2)
#uses library(ggalt) 
options(scipen = 999)
midwest_select <- midwest[midwest$poptotal > 350000 & 
                            midwest$poptotal <= 500000 & 
                            midwest$area > 0.01 & 
                            midwest$area < 0.1, ]

gg2 <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state, size=popdensity)) +   # draw points
  geom_smooth(method="loess", se=F) + 
  xlim(c(0, 0.1)) + 
  ylim(c(0, 500000)) +   # draw smoothing line
  geom_encircle(aes(x=area, y=poptotal), 
                data=midwest_select, 
                color="red", 
                size=2, 
                expand=0.08) +   # encircle
  labs(subtitle="Area Vs Population", 
       y="Population", 
       x="Area", 
       title="Scatterplot + Encircle", 
       caption="Source: midwest")

png(filename = "EncircledScatter.png")
plot(gg2)
dev.off()

####################################################
#Jitter Plot
####################################################
#Uses: library(ggplot2)
data(mpg, package="ggplot2")
theme_set(theme_bw())  # pre-set the bw theme.
  
jitterplot <- ggplot(mpg, aes(cty, hwy))
  
jitterplot <- jitterplot + geom_jitter(width = .5, size=1) +
  labs(subtitle="mpg: city vs highway mileage", 
       y="hwy", 
       x="cty", 
       title="Jittered Points")

png(filename = "jitterplot.png")
plot(jitterplot)
dev.off()

####################################################
#Counts Plot
####################################################
#Uses library(ggplot2)
theme_set(theme_bw())  # pre-set the bw theme
countsplot <- ggplot(mpg, aes(cty, hwy))
countsplot <- countsplot + geom_count(col="tomato3", show.legend=F) +
  labs(subtitle="mpg: city vs highway mileage", 
       y="hwy", 
       x="cty", 
       title="Counts Plot")

png(filename = "countsplot.png")
plot(countsplot)
dev.off()

####################################################
#BubblePlot
####################################################
#Uses: library(ggplot2)
data(mpg, package="ggplot2")

mpg_select <- mpg[mpg$manufacturer %in% c("audi", "ford", "honda", "hyundai"), ]
theme_set(theme_bw())  # pre-set the bw theme.
bubbleplot <- ggplot(mpg_select, aes(displ, cty)) + 
  labs(subtitle="mpg: Displacement vs City Mileage",
       title="Bubble chart")

bubbleplot <- bubbleplot + geom_jitter(aes(col=manufacturer, size=hwy)) + 
  geom_smooth(aes(col=manufacturer), method="lm", se=F)

png(filename = "bubbleplot.png")
plot(bubbleplot)
dev.off()

####################################################
#AnimatedBubblePlot //not currently working
####################################################
#Uses: library(ggplot2)
#Uses: library(gapminder)
#USes: library(gganimate) // been depreciated

#theme_set(theme_bw())  # pre-set the bw theme.

#animatedbubbleplot <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, frame = year)) +
#  geom_point() +
#  geom_smooth(aes(group = year), 
#              method = "lm", 
#              show.legend = FALSE) +
#  facet_wrap(~continent, scales = "free") +
#  scale_x_log10()  # convert to log scale

#animatedbubbleplot <- gganimate(animatedbubbleplot, interval=0.2)

####################################################
#Marginal Histogram / Boxplot
####################################################
#Note: better formatting if you plot in RStudio and export
#Uses: library(ggplot2)
#USes: library(ggExtra)

data(mpg, package="ggplot2")

theme_set(theme_bw())  # pre-set the bw theme.
mpg_select <- mpg[mpg$hwy >= 35 & mpg$cty > 27, ]
marginal <- ggplot(mpg, aes(cty, hwy)) +   geom_count() + geom_smooth(method="lm", se=F)

marginal1 <- ggMarginal(marginal, type = "histogram", fill="transparent")
marginal2 <- ggMarginal(marginal, type = "boxplot", fill="transparent")
marginal3 <- ggMarginal(marginal, type = "density", fill="transparent")

png(filename = "marginal1.png")
plot(marginal1)
dev.off()

png(filename = "marginal2.png")
plot(marginal2)
dev.off()

png(filename = "marginal3.png")
plot(marginal3)
dev.off()

####################################################
#Correlogram
####################################################
#Uses: library(ggplot2)
#Uses: library(ggcorrplot)

# Correlation matrix
data(mtcars)
corr <- round(cor(mtcars), 1)

correlogram <- ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of mtcars", 
           ggtheme=theme_bw)

png(filename = "correlogram.png")
plot(correlogram)
dev.off()

####################################################
#Diverging bars
####################################################
#Uses: library(ggplot2)

theme_set(theme_bw())  

# Data Prep
data("mtcars")  # load data
mtcars$`car name` <- rownames(mtcars)  # create new column for car names
mtcars$mpg_z <- round((mtcars$mpg - mean(mtcars$mpg))/sd(mtcars$mpg), 2)  # compute normalized mpg
mtcars$mpg_type <- ifelse(mtcars$mpg_z < 0, "below", "above")  # above / below avg flag
mtcars <- mtcars[order(mtcars$mpg_z), ]  # sort
mtcars$`car name` <- factor(mtcars$`car name`, levels = mtcars$`car name`)  # convert to factor to retain sorted order in plot.

# Diverging Barcharts
divergingbars <- ggplot(mtcars, aes(x=`car name`, y=mpg_z, label=mpg_z)) + 
  geom_bar(stat='identity', aes(fill=mpg_type), width=.5)  +
  scale_fill_manual(name="Mileage", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Normalised mileage from 'mtcars'", 
       title= "Diverging Bars") + 
  coord_flip()

png(filename = "divergingbars.png")
plot(divergingbars)
dev.off()

####################################################
#Diverging Lollipop Chart
####################################################
#Uses: library(ggplot2)

# Data Prep
data("mtcars")  # load data
mtcars$`car name` <- rownames(mtcars)  # create new column for car names
mtcars$mpg_z <- round((mtcars$mpg - mean(mtcars$mpg))/sd(mtcars$mpg), 2)  # compute normalized mpg
mtcars$mpg_type <- ifelse(mtcars$mpg_z < 0, "below", "above")  # above / below avg flag
mtcars <- mtcars[order(mtcars$mpg_z), ]  # sort
mtcars$`car name` <- factor(mtcars$`car name`, levels = mtcars$`car name`)  # convert to factor to retain sorted order in plot.

# Diverging Lollipop
diverginglollipop <- ggplot(mtcars, aes(x=`car name`, y=mpg_z, label=mpg_z)) + 
  geom_point(stat='identity', fill="black", size=6)  +
  geom_segment(aes(y = 0, 
                   x = `car name`, 
                   yend = mpg_z, 
                   xend = `car name`), 
               color = "black") +
  geom_text(color="white", size=2) +
  labs(title="Diverging Lollipop Chart", 
       subtitle="Normalized mileage from 'mtcars': Lollipop") + 
  ylim(-2.5, 2.5) +
  coord_flip()

png(filename = "diverginglollipop.png")
plot(diverginglollipop)
dev.off()

####################################################
#Diverging Dot Plot
####################################################
#Uses: library(ggplot2)

# Data Prep
data("mtcars")  # load data
mtcars$`car name` <- rownames(mtcars)  # create new column for car names
mtcars$mpg_z <- round((mtcars$mpg - mean(mtcars$mpg))/sd(mtcars$mpg), 2)  # compute normalized mpg
mtcars$mpg_type <- ifelse(mtcars$mpg_z < 0, "below", "above")  # above / below avg flag
mtcars <- mtcars[order(mtcars$mpg_z), ]  # sort
mtcars$`car name` <- factor(mtcars$`car name`, levels = mtcars$`car name`)  # convert to factor to retain sorted order in plot.

# Plot
divergingdotplot <- ggplot(mtcars, aes(x=`car name`, y=mpg_z, label=mpg_z)) + 
  geom_point(stat='identity', aes(col=mpg_type), size=6)  +
  scale_color_manual(name="Mileage", 
                     labels = c("Above Average", "Below Average"), 
                     values = c("above"="#00ba38", "below"="#f8766d")) + 
  geom_text(color="white", size=2) +
  labs(title="Diverging Dot Plot", 
       subtitle="Normalized mileage from 'mtcars': Dotplot") + 
  ylim(-2.5, 2.5) +
  coord_flip()

png(filename = "divergingdotplot.png")
plot(divergingdotplot)
dev.off()

####################################################
#Area Chart
####################################################
#Uses: library(ggplot2)
#Uses: library(quantmod)

data("economics", package = "ggplot2")

# Compute % Returns
economics$returns_perc <- c(0, diff(economics$psavert)/economics$psavert[-length(economics$psavert)])

# Create break points and labels for axis ticks
brks <- economics$date[seq(1, length(economics$date), 12)]
lbls <- lubridate::year(economics$date[seq(1, length(economics$date), 12)])

# Plot
areachart <- ggplot(economics[1:100, ], aes(date, returns_perc)) + 
  geom_area() + 
  scale_x_date(breaks=brks, labels=lbls) + 
  theme(axis.text.x = element_text(angle=90)) + 
  labs(title="Area Chart", 
       subtitle = "Perc Returns for Personal Savings", 
       y="% Returns for Personal savings", 
       caption="Source: economics")

png(filename = "areachart.png")
plot(areachart)
dev.off()

####################################################
#Ordered Bar Chart
####################################################
#Uses: library(ggplot2)

# Prepare data: group mean city mileage by manufacturer.
cty_mpg <- aggregate(mpg$cty, by=list(mpg$manufacturer), FUN=mean)  # aggregate
colnames(cty_mpg) <- c("make", "mileage")  # change column names
cty_mpg <- cty_mpg[order(cty_mpg$mileage), ]  # sort
cty_mpg$make <- factor(cty_mpg$make, levels = cty_mpg$make)  # to retain the order in plot.
head(cty_mpg, 4)
#>          make  mileage
#> 9     lincoln 11.33333
#> 8  land rover 11.50000
#> 3       dodge 13.13514
#> 10    mercury 13.25000

# Draw plot
theme_set(theme_bw())
orderedbarchart <- ggplot(cty_mpg, aes(x=make, y=mileage)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Ordered Bar Chart", 
       subtitle="Make Vs Avg. Mileage", 
       caption="source: mpg") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

png(filename = "orderedbarchart.png")
plot(orderedbarchart)
dev.off()

####################################################
#Ordered Lollipop Chart
####################################################
#Uses: library(ggplot2)

# Prepare data: group mean city mileage by manufacturer.
cty_mpg <- aggregate(mpg$cty, by=list(mpg$manufacturer), FUN=mean)  # aggregate
colnames(cty_mpg) <- c("make", "mileage")  # change column names
cty_mpg <- cty_mpg[order(cty_mpg$mileage), ]  # sort
cty_mpg$make <- factor(cty_mpg$make, levels = cty_mpg$make)  # to retain the order in plot.
head(cty_mpg, 4)
#>          make  mileage
#> 9     lincoln 11.33333
#> 8  land rover 11.50000
#> 3       dodge 13.13514
#> 10    mercury 13.25000

# Plot
orderedlollipop <- ggplot(cty_mpg, aes(x=make, y=mileage)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=make, 
                   xend=make, 
                   y=0, 
                   yend=mileage)) + 
  labs(title="Lollipop Chart", 
       subtitle="Make Vs Avg. Mileage", 
       caption="source: mpg") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

png(filename = "orderedlollipop.png")
plot(orderedlollipop)
dev.off()

####################################################
#Dot Plot
####################################################
#Uses: library(ggplot2)
#Uses: library(scales)

# Prepare data: group mean city mileage by manufacturer.
cty_mpg <- aggregate(mpg$cty, by=list(mpg$manufacturer), FUN=mean)  # aggregate
colnames(cty_mpg) <- c("make", "mileage")  # change column names
cty_mpg <- cty_mpg[order(cty_mpg$mileage), ]  # sort
cty_mpg$make <- factor(cty_mpg$make, levels = cty_mpg$make)  # to retain the order in plot.
head(cty_mpg, 4)
#>          make  mileage
#> 9     lincoln 11.33333
#> 8  land rover 11.50000
#> 3       dodge 13.13514
#> 10    mercury 13.25000

dotplot <- ggplot(cty_mpg, aes(x=make, y=mileage)) + 
  geom_point(col="tomato2", size=3) +   # Draw points
  geom_segment(aes(x=make, 
                   xend=make, 
                   y=min(mileage), 
                   yend=max(mileage)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  labs(title="Dot Plot", 
       subtitle="Make Vs Avg. Mileage", 
       caption="source: mpg") +  
  coord_flip()

png(filename = "dotplot.png")
plot(dotplot)
dev.off()

####################################################
#Slope Chart
####################################################
#Uses: library(ggplot2)
#Uses: library(scales)

# prep data
df <- read.csv("https://raw.githubusercontent.com/selva86/datasets/master/gdppercap.csv")
colnames(df) <- c("continent", "1952", "1957")
left_label <- paste(df$continent, round(df$`1952`),sep=", ")
right_label <- paste(df$continent, round(df$`1957`),sep=", ")
df$class <- ifelse((df$`1957` - df$`1952`) < 0, "red", "green")

theme_set(theme_classic())

# Plot
p <- ggplot(df) + geom_segment(aes(x=1, xend=2, y=`1952`, yend=`1957`, col=class), size=.75, show.legend=F) + 
  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  scale_color_manual(labels = c("Up", "Down"), 
                     values = c("green"="#00ba38", "red"="#f8766d")) +  # color of lines
  labs(x="", y="Mean GdpPerCap") +  # Axis labels
  xlim(.5, 2.5) + ylim(0,(1.1*(max(df$`1952`, df$`1957`))))  # X and Y axis limits

# Add texts
p <- p + geom_text(label=left_label, y=df$`1952`, x=rep(1, NROW(df)), hjust=1.1, size=3.5)
p <- p + geom_text(label=right_label, y=df$`1957`, x=rep(2, NROW(df)), hjust=-0.1, size=3.5)
p <- p + geom_text(label="Time 1", x=1, y=1.1*(max(df$`1952`, df$`1957`)), hjust=1.2, size=5)  # title
p <- p + geom_text(label="Time 2", x=2, y=1.1*(max(df$`1952`, df$`1957`)), hjust=-0.1, size=5)  # title

# Minify theme
p <- p + theme(panel.background = element_blank(), 
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          panel.border = element_blank(),
          plot.margin = unit(c(1,2,1,2), "cm"))

png(filename = "slopechart.png")
plot(p)
dev.off()


####################################################
#Dumbbell Plot
####################################################
#Uses: library(ggplot2)
#Uses: library(ggalt)

health <- read.csv("https://raw.githubusercontent.com/selva86/datasets/master/health.csv")
health$Area <- factor(health$Area, levels=as.character(health$Area))  # for right ordering of the dumbells

dumbbell <- ggplot(health, aes(x=pct_2013, xend=pct_2014, y=Area, group=Area)) + 
  geom_dumbbell(color="#a3c4dc", 
                size=0.75, 
                point.colour.l="#0e668b") + 
  scale_x_continuous(label=percent) + 
  labs(x=NULL, 
       y=NULL, 
       title="Dumbbell Chart", 
       subtitle="Pct Change: 2013 vs 2014", 
       caption="Source: https://github.com/hrbrmstr/ggalt") +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())

png(filename = "dumbbell.png")
plot(dumbbell)
dev.off()

####################################################
#Histograms
####################################################
#Uses: library(ggplot2)

theme_set(theme_classic())

# Histogram on a Continuous (Numeric) Variable
hist <- ggplot(mpg, aes(displ)) + scale_fill_brewer(palette = "Spectral")

# Histogram with auto number of bins
histauto <- hist + geom_histogram(aes(fill=class), 
                   binwidth = .1, 
                   col="black", 
                   size=.1) +  # change binwidth
  labs(title="Histogram with Auto Binning", 
       subtitle="Engine Displacement across Vehicle Classes")  

# Histogram with fixed number of bins
histfixed <- hist + geom_histogram(aes(fill=class), 
                   bins=5, 
                   col="black", 
                   size=.1) +   # change number of bins
  labs(title="Histogram with Fixed Bins", 
       subtitle="Engine Displacement across Vehicle Classes") 

# Histogram on a Categorical variable
histcat <- ggplot(mpg, aes(manufacturer))
histcat <- histcat + geom_bar(aes(fill=class), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Manufacturer across Vehicle Classes") 

png(filename = "histfixed.png")
plot(histfixed)
dev.off()

png(filename = "histauto.png")
plot(histauto)
dev.off()

png(filename = "histcat.png")
plot(histcat)
dev.off()

####################################################
#Density Plot
####################################################
#Uses: library(ggplot2)

library(ggplot2)
theme_set(theme_classic())

# Plot
desnityplot <- ggplot(mpg, aes(cty))
desnityplot <- desnityplot + geom_density(aes(fill=factor(cyl)), alpha=0.8) + 
  labs(title="Density plot", 
       subtitle="City Mileage Grouped by Number of cylinders",
       caption="Source: mpg",
       x="City Mileage",
       fill="# Cylinders")

png(filename = "desnityplot")
plot(desnityplot)
dev.off()

####################################################
#Box Plot
####################################################
#Uses: library(ggplot2)

library(ggplot2)
theme_set(theme_classic())

# Plot
boxplot <- ggplot(mpg, aes(class, cty))
boxplot <- boxplot + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Box plot", 
       subtitle="City Mileage grouped by Class of vehicle",
       caption="Source: mpg",
       x="Class of Vehicle",
       y="City Mileage")

png(filename = "boxplot.png")
plot(boxplot)
dev.off()

####################################################
#Box Plot2
####################################################
#Uses: library(ggplot2)
#Uses: library(ggthemes)

boxplot2 <- ggplot(mpg, aes(class, cty))
boxplot2 <- boxplot2 + geom_boxplot(aes(fill=factor(cyl))) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Box plot", 
       subtitle="City Mileage grouped by Class of vehicle",
       caption="Source: mpg",
       x="Class of Vehicle",
       y="City Mileage")

png(filename = "boxplot2.png")
plot(boxplot2)
dev.off()

####################################################
#DotBox Plot
####################################################
#Uses: library(ggplot2)

theme_set(theme_bw())

boxdot <- ggplot(mpg, aes(manufacturer, cty))
boxdot <- boxdot + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .5, 
               fill="red") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Box plot + Dot plot", 
       subtitle="City Mileage vs Class: Each dot represents 1 row in source data",
       caption="Source: mpg",
       x="Class of Vehicle",
       y="City Mileage")

png("boxdot.png")
plot(boxdot)
dev.off()

####################################################
#Tuffe Box Plot
####################################################
#Uses: library(ggplot2)
#Uses: library(ggthemes)

theme_set(theme_tufte())  # from ggthemes

# plot
tuftebox <- ggplot(mpg, aes(manufacturer, cty))
tuftebox <- tuftebox + geom_tufteboxplot() + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Tufte Styled Boxplot", 
       subtitle="City Mileage grouped by Class of vehicle",
       caption="Source: mpg",
       x="Class of Vehicle",
       y="City Mileage")

png(filename = "tuftebox.png")
plot(tuftebox)
dev.off()

####################################################
#Violin Plot
####################################################
#Uses: library(ggplot2)

theme_set(theme_bw())

# plot
violin <- ggplot(mpg, aes(class, cty))
violin <- violin + geom_violin() + 
  labs(title="Violin plot", 
       subtitle="City Mileage vs Class of vehicle",
       caption="Source: mpg",
       x="Class of Vehicle",
       y="City Mileage")

png(filename = "violin.png")
plot(violin)
dev.off()

####################################################
#PopPyramid Plot
####################################################
#Uses: library(ggplot2)
#Uses: library(ggthemes)

options(scipen = 999)  # turns of scientific notations like 1e+40

# Read data
email_campaign_funnel <- read.csv("https://raw.githubusercontent.com/selva86/datasets/master/email_campaign_funnel.csv")

# X Axis Breaks and Labels 
brks <- seq(-15000000, 15000000, 5000000)
lbls = paste0(as.character(c(seq(15, 0, -5), seq(5, 15, 5))), "m")

# Plot
poppyramid <- ggplot(email_campaign_funnel, aes(x = Stage, y = Users, fill = Gender)) +   # Fill column
  geom_bar(stat = "identity", width = .6) +   # draw the bars
  scale_y_continuous(breaks = brks,   # Breaks
                     labels = lbls) + # Labels
  coord_flip() +  # Flip axes
  labs(title="Email Campaign Funnel") +
  theme_tufte() +  # Tufte theme from ggfortify
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank()) +   # Centre plot title
  scale_fill_brewer(palette = "Dark2")  # Color palette

png(filename = "poppyramid.png")
plot(poppyramid)
dev.off()

####################################################
#Waffle Chart
####################################################
#Uses: library(ggplot2)

var <- mpg$class  # the categorical data 

## Prep data (nothing to change here)
nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
categ_table <- round(table(var) * ((nrows*nrows)/(length(var))))
categ_table
#>   2seater    compact    midsize    minivan     pickup subcompact        suv 
#>         2         20         18          5         14         15         26 

df$category <- factor(rep(names(categ_table), categ_table))  
# NOTE: if sum(categ_table) is not 100 (i.e. nrows^2), it will need adjustment to make the sum to 100.

## Plot
waffle <- ggplot(df, aes(x = x, y = y, fill = category)) + 
  geom_tile(color = "black", size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_brewer(palette = "Set3") +
  labs(title="Waffle Chart", subtitle="'Class' of vehicles",
       caption="Source: mpg") + 
  theme(panel.border = element_rect(size = 2),
        plot.title = element_text(size = rel(1.2)),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.position = "right")

png(filename = "waffle.png")
plot(waffle)
dev.off()

####################################################
#Pie Chart
####################################################
#Uses: library(ggplot2)

theme_set(theme_classic())

# Source: Frequency table
df <- as.data.frame(table(mpg$class))
colnames(df) <- c("class", "freq")
pie1 <- ggplot(df, aes(x = "", y=freq, fill = factor(class))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="class", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of class", 
       caption="Source: mpg")

pie1 <- pie1 + coord_polar(theta = "y", start=0)

# Source: Categorical variable.
# mpg$class
pie2 <- ggplot(mpg, aes(x = "", fill = factor(class))) + 
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="class", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of class", 
       caption="Source: mpg")

pie2 <- pie2 + coord_polar(theta = "y", start=0)

png(filename = "pieFreqTable.png")
plot(pie1)
dev.off()

png(filename = "pieCat.png")
plot(pie2)
dev.off()

####################################################
#Bar Chart
####################################################
#Uses: library(ggplot2)

# prep frequency table
freqtable <- table(mpg$manufacturer)
df <- as.data.frame.table(freqtable)
#head(df)
#>          Var1 Freq
#> 1        audi   18
#> 2   chevrolet   19
#> 3       dodge   37
#> 4        ford   25
#> 5       honda    9
#> 6     hyundai   14
# plot
theme_set(theme_classic())
# Plot
bar <- ggplot(df, aes(Var1, Freq))
bar <- bar + geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
  labs(title="Bar Chart", 
       subtitle="Manufacturer of vehicles", 
       caption="Source: Frequency of Manufacturers from 'mpg' dataset") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


png(filename = "barchart.png")
plot(bar)
dev.off()

####################################################
#Catagorical Bar Chart
####################################################
#Uses: library(ggplot2)

# From on a categorical column variable
barcat <- ggplot(mpg, aes(manufacturer))
barcat <- barcat + geom_bar(aes(fill=class), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Categorywise Bar Chart", 
       subtitle="Manufacturer of vehicles", 
       caption="Source: Manufacturers from 'mpg' dataset")

png(filename = "CatBarChart.png")
plot(barcat)
dev.off()

####################################################
#Time Series Plot From a Time Series Object (ts)
####################################################
#Uses: library(ggplot2)
#Uses: library(ggfortify)

# Plot 
theme_set(theme_classic())
timeseries <- autoplot(AirPassengers) + 
  labs(title="AirPassengers") + 
  theme(plot.title = element_text(hjust=0.5))

png(filename = "timeseries.png")
plot(timeseries)
dev.off()


####################################################
#Time Series Plot From a data frame
####################################################
#Uses: library(ggplot2)

# Allow Default X Axis Labels
timeseriesDF <- ggplot(economics, aes(x=date)) + 
  geom_line(aes(y=returns_perc)) + 
  labs(title="Time Series Chart", 
       subtitle="Returns Percentage from 'Economics' Dataset", 
       caption="Source: Economics", 
       y="Returns %")

png(filename = "timeseriesDF.png")
plot(timeseriesDF)
dev.off()

####################################################
#Time Series Plot for Montly data using Lubridate
####################################################
#Uses: library(ggplot2)
#Uses: library(lubridate)

theme_set(theme_bw())

economics_m <- economics[1:24, ]

# labels and breaks for X axis text
lbls <- paste0(month.abb[month(economics_m$date)], " ", lubridate::year(economics_m$date))
brks <- economics_m$date

# plot

monthlyplot <- ggplot(economics_m, aes(x=date)) + 
  geom_line(aes(y=returns_perc)) + 
  labs(title="Monthly Time Series", 
       subtitle="Returns Percentage from Economics Dataset", 
       caption="Source: Economics", 
       y="Returns %") +  # title and caption
  scale_x_date(labels = lbls, 
               breaks = brks) +  # change to monthly ticks and labels
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
        panel.grid.minor = element_blank())  # turn off minor grid 

png(filename = "montlytimeseries.png")
plot(monthlyplot)
dev.off()

####################################################
#Time Series Plot for Yearly data using Lubridate
####################################################
#Uses: library(ggplot2)
#Uses: library(lubridate)

theme_set(theme_bw())

economics_y <- economics[1:90, ]

# labels and breaks for X axis text
brks <- economics_y$date[seq(1, length(economics_y$date), 12)]
lbls <- lubridate::year(brks)

# plot
yearlytimeseries <- ggplot(economics_y, aes(x=date)) + 
  geom_line(aes(y=returns_perc)) + 
  labs(title="Yearly Time Series", 
       subtitle="Returns Percentage from Economics Dataset", 
       caption="Source: Economics", 
       y="Returns %") +  # title and caption
  scale_x_date(labels = lbls, 
               breaks = brks) +  # change to monthly ticks and labels
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
        panel.grid.minor = element_blank())  # turn off minor grid

png(filename = "yearlytimeseries.png")
plot(yearlytimeseries)
dev.off()