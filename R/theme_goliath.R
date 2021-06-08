




#Define gppr_theme() function

theme_goliath <- function(){ 
  font <- "Lato"   #assign font family up front
  
  theme_hc() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_blank(),          #strip axis ticks
      
      #since theme_minimal() already strips axis lines, 
      #we don't need to do that again
      
      #text elements
      
      text = element_text(family = font),
      
      plot.title = element_textbox_simple(             #title
        family = font,            #set font family
        size = 32,                #set font size
        face = 'bold',            #bold typeface
        hjust = 0,                #left align
        vjust = 2,              #raise slightly
        lineheight = 1, padding = margin(0, 0, 5, 0)
      ),
      
      
      plot.subtitle = element_textbox_simple(          #subtitle
        family = font,            #font family
        size = 24),               #font size
      
      
      plot.caption = element_textbox_simple(           #caption
        family = font,            #font family
        size = 18,                 #font size
        hjust = 0,
        lineheight = 1
        ),               #right align
      plot.caption.position =  "plot",
      
      axis.title = element_text(             #axis titles
        family = font,            #font family
        size = 14),               #font size
      
      axis.text = element_text(              #axis text
        family = font,            #axis famuly
        size = 18),                #font size
      
      legend.text = element_text(
        family  = font,
        size  = 18
      ),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 16)),
      
      # LEGENDS
      legend.title = element_blank(),
      #plot.background = element_rect(fill = "#faf0e6"),
      #legend.background = element_rect(fill = "#faf0e6", color = "#faf0e6"),
      legend.key.size = unit(1, 'cm')
      
      #since the legend often requires manual tweaking 
      #based on plot content, don't define it here
    )
}
