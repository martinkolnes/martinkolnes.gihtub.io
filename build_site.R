#Set our working directory. 
#This helps avoid confusion if our working directory is 
#not our site because of other projects we were 
#working on at the time. 
setwd("C:/Users/Martin/ownCloud/Projects/Rwebsite/martinkolnes.github.io")

#render your sweet site. 
rmarkdown::render_site(encoding = 'UTF-8')