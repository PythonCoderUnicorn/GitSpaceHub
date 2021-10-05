


#=========== make a dataframe with tribble
library(tidyverse)

df = tribble(
  ~race, ~sex, ~gender, ~LGBT,
  1124, 354, 354, 1294,
  115,1165,1153,36,
  2,0,10,141,
  167,0,10,141,
  2, 0,2,11,
  81, 0, 0, 0,
  28, 0, 0,0 )


df %>% 
  mutate(gender = as.factor(1:7),
         gender = c("Male","Female",
                    "Non-Binary","Trans-Female",
                    "Trans-Male",'Agender',"Prefer Not to Say"))






#========== colors Usage in R base plots:
barplot(1:5, col=rainbow(5))
# Use heat.colors
barplot(1:5, col=heat.colors(5))
# Use terrain.colors
barplot(1:5, col=terrain.colors(5))
# Use topo.colors
barplot(1:5, col=topo.colors(5))
# Use cm.colors
barplot(1:5, col=cm.colors(5))








#-- ggplot 
ggplot(data= mpg) +
  geom_point( aes(x= displ, y=hwy, size= class), color= "orange")



ggplot(mpg) +
  geom_histogram( aes(x= hwy), 
                  binwidth = 5, 
                  fill="gold", 
                  color='black') +
  scale_x_continuous(breaks = seq(0,45,5), limits =  c(0,50), expand = c(0,0))+
  scale_y_continuous(breaks = seq(0,90,10), limits =  c(0,90), expand = c(0,0))


ggplot(mpg) +
  geom_boxplot( aes(x= class, y=cty, fill=class))


ggplot(mpg) +
  geom_bar( aes(x= class, fill= factor(cyl))) +
  labs(title = "cylinders by class", fill="cylinders") +
  coord_flip()


ggplot(mpg) +
  geom_bar( aes(x= class, fill= factor(cyl)), position = 'dodge') +
  labs(title = "cylinders by class", fill="cylinders") +
  coord_flip()


ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth()



# use jitter to avoid overcrowding points
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy,  col=displ), position = "jitter", alpha= 0.5,)







#======== plotly scatterplot
# Scatterplot
library(plotly)

data <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/school_earnings.csv")

fig <- plot_ly(data, x = ~Women, y = ~Men, text = ~School, type = 'scatter', mode = 'markers',
        marker = list(size = ~Gap, opacity = 0.5,
                      color = 'rgb(255, 65, 54)'))
fig <- fig %>% layout(title = 'Gender Gap in Earnings per University',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE))

fig

#-------
library(plotly)
f <- list(
  family = "Courier New, monospace",
  size = 28,
  color = "#7f7f7f"
)
x <- list(
  title = "x Axis",
  titlefont = f
)
y <- list(
  title = "y Axis",
  titlefont = f
)
fig <- plot_ly(x = ~rnorm(10), y = ~rnorm(10), mode = "markers")
fig <- fig %>% layout(xaxis = x, yaxis = y)

fig









#================= forcats 
#  forcats = factors

library(tidyverse) 
library(forcats)

# factors gave levels
f1 = c(letters[1:5])

month_levels = month.abb


# using the factor() function
# default is alphabetical order for levels
f3 = factor(f1, levels = month_levels)
f3 

f4 = factor(f1)
f4

f5 = c(1:10)
uf = factor(f5, levels = unique(f5))
uf 


# --------
counts = rnorm(4, 50, 10)

orderedData = data.frame(
  Month= month_levels,
  Count = counts
  ) 
  ggplot( orderedData) +
  geom_bar( aes(x= Month, y= Count),
            stat = "identity")

# --------------

  

  
  
  
df = gss_cat
head(df)

df %>% count(marital)

df = df %>% 
  add_count(marital, name = "MaritalCount")

# - simple barchart
df %>% 
  ggplot() +
  geom_bar( aes(x= fct_rev(marital)))
  
# organize columns
df.organized = df %>% 
  ggplot() +
  geom_bar( aes(x= marital))


  
df %>% 
  mutate(marital = marital %>% 
           fct_infreq() %>%  fct_rev()) %>% 
  ggplot() +
  geom_bar( aes(x= marital)) 



religiousity = df %>% 
  group_by(relig) %>% 
  summarise(
    age = mean(age, na.rm=T),
    tvhours = mean(tvhours, na.rm=T),
    n = n()
  )
religiousity

# scatterplot
ggplot( religiousity,
        aes(tvhours, relig)) +
  geom_point()


# - reorder the levels of religion
# arranged based on tv hours
ggplot(religiousity,
       aes(x= tvhours,
           fct_reorder(relig, tvhours))) +
  geom_point()


# -- change the order of levels
# 'no answer' will come first in levels
religiousity = religiousity %>% 
  mutate(relig = fct_reorder(relig, tvhours)) %>% 
  mutate(relig = fct_relevel(relig, "No answer"))
ggplot( religiousity,
        aes(tvhours, relig)) +
  geom_point()



# - look @ part id
df.party = df %>% 
  count(partyid)
df.party

# -- clean up the level names
df.party2 = df %>% 
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong" = "Strong republican",
                              "Republican, weak" = "Not str republican",
                              "Independent, near Republican" = "Ind,near rep",
                              "Independent, near Democrat" = "Ind,near dem",
                              "Democrat, weak" = "Not str democrat",
                              "Democrat, strong" = "Strong democrat"
                              ))

df.party2 

df.party2 = df.party %>% 
  mutate(partyid = fct_collapse(partyid,
                                "other" = c("Don't know", "Other party"),
                                "Republican" = c("Strong republican",
                                                 "Not str republican"),
                                "Independent" = c("Ind,near rep",
                                                  "Ind,near dem",
                                                  "Independent"),
                                "Democrat" = c("Not str democrat",
                                               "Strong democrat") ))
df.party %>% count(partyid)




# - fct_lump() to lump count the top 3 levels everything else is in 'other'
df %>% 
  mutate(marital = fct_lump_n(marital, n= 3)) %>% 
  count(marital)












#=============== LINEAR REGRESSION MODEL

# LINEAR MODELS

library(broom)
library(tidyverse)
library(AER)


cars = mtcars

summary(cars)

cars <- cars %>%
  mutate(across(.cols = c("cyl", "vs", "am", "gear"), .fns = factor))

# --- Linear Regression
#   ~ . means all other columns
LinReg = lm(mpg ~ ., data = cars )
LinReg
summary(LinReg)


# another method
columns = "mpg ~ cyl + sqrt(disp) + hp + drat + wt + qsec + hp:wt"
LinReg = lm(columns, data = cars)
summary(LinReg)


# get the summary stats of Linear Regression
tidy(LinReg)

glance(LinReg)

augmentedcars = augment(LinReg, data= cars)
augmentedcars





## terminal color styling 

library(tidyverse)

library(crayon)
cat(yellow$cyan$bold("hello", "Crayon World!\n"))

library(cli)
cli({
  cli_h1(yellow$bold("Title"))
  cli_h2("Subtitle")
  cli_ul(c("this", "that", "end"))
})




#=============== beepr library
library(beepr)
beep(sound = 8)
beep(3)

#beep('mario') # nice
beep('coin')  # nice
#beep('ping')
#beep('fanfare')
#beep('complete')
#beep('treasure') # ok
#beep('sword')

# downloaded 
#beep('./wav/alien-spaceship_daniel_simion.wav')


# https://soundbible.com   -- can download .wav files for sounds and save in a file then call them
