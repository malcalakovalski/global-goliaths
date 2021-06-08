theme(
  text = element_text(family = "Bai Jamjuree"),
  plot.title = element_text(
    size = 50,
    hjust = 0.5,
    vjust = -6.2,
    face = 'bold',
    margin = margin(0, 0, 45, 0)
  ),
  legend.position = c(0.79, 0.5),
  legend.text = element_text(size = 30),
  legend.title = element_blank(),
  plot.background = element_rect(fill = "#faf0e6"),
  legend.background = element_rect(fill = "#faf0e6", color = "#faf0e6"),
  legend.key.size = unit(1.1, 'cm')
)




#Define gppr_theme() function

theme_goliath <- function(){ 
  font <- "Baj Jamjuree"   #assign font family up front
  
  theme_hc() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_blank(),          #strip axis ticks
      
      #since theme_minimal() already strips axis lines, 
      #we don't need to do that again
      
      #text elements
      plot.title = element_textbox_simple(             #title
        family = font,            #set font family
        size = 20,                #set font size
        face = 'bold',            #bold typeface
        hjust = 0,                #left align
        vjust = 2),               #raise slightly
      
      plot.subtitle = element_text(          #subtitle
        family = font,            #font family
        size = 14),               #font size
      
      plot.caption = element_text(           #caption
        family = font,            #font family
        size = 9,                 #font size
        hjust = 1),               #right align
      
      axis.title = element_text(             #axis titles
        family = font,            #font family
        size = 10),               #font size
      
      axis.text = element_text(              #axis text
        family = font,            #axis famuly
        size = 9),                #font size
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10))
      
      #since the legend often requires manual tweaking 
      #based on plot content, don't define it here
    )
}
