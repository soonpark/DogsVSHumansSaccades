# imports
library(shiny)
library(ggplot2)
library(dplyr)

# UI ----------------------------------------------------------------------
metricOptions<-c(
  "Duration (ms)"="duration",
  "Peak velocity (deg/s)"="peakVelocity",
  "Saccadic skewness (%)"="saccadicSkewness")

ui <- fluidPage(theme = shinytheme("spacelab"),
  titlePanel(title = "Dogs VS Humans: saccade characteristics",
             windowTitle = "Tab title in web browser"),
  sidebarLayout(
    sidebarPanel(
      h3("Saccade metrics"),
      selectInput(inputId = 'y', label = 'Select a saccade metric', choices = metricOptions),
      
    ),
    mainPanel(h3("Compare distribution of each metric"),
      tabsetPanel(type = 'tabs',
                  tabPanel(title = 'violin plot', plotOutput(outputId = 'out_violinplot'),
                           h4("- solid line=median"), h4("- dashed lines=quantiles")),
                  tabPanel(title = "histogram", plotOutput(outputId = 'out_histogram')))
    )
    
  )
  
)


# SERVER ------------------------------------------------------------------

server <- function(input, output) {
  filtered = reactive(saccades)
  
  # The violin plot
  output$out_violinplot = renderPlot({
    ggplot(data = filtered(), aes_string(y=input$y, x="species", group="species"))  + 
      geom_violin(draw_quantiles = c(0.25, 0.75),fill="azure3",
                  linetype = "dashed",color="black") +
      geom_violin(fill="transparent",draw_quantiles = 0.5,color="black") +
      stat_summary(fun.data=mean_se, fun.args = list(mult=1), 
                   geom="errorbar", color="blue", width=0.05, size=1) +
      stat_summary(fun.y=mean,aes(shape="mean with SE"), size=1, color="blue", show.legend=TRUE)+
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),panel.background=element_rect(fill='white', colour='azure3'),
          panel.grid.major = element_line(colour="azure3", size=.3), 
          panel.grid.minor = element_line(colour="azure3", size=.3),panel.grid.major.x = element_blank(),
          axis.title.x = element_text(size=23,margin = margin(t = 20, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(size=23,margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.text.x = element_text(size=18, vjust = 1),
          axis.text.y = element_text(size=18, vjust = .5),
          plot.title = element_text(lineheight=.8,size=16, hjust=0),
          legend.key=element_rect(colour="transparent", fill="transparent"),
          legend.text=element_text(size=15),
          legend.background = element_rect(colour = 'transparent', fill = 'transparent', linetype='solid'),
          legend.title=element_blank(),legend.position = c(0.5, 0.85),
          )+
      labs(x = "Species", y = names(metricOptions[which(metricOptions == input$y)])+
             scale_shape_manual("mean", values= "") 
           ) 
  })
  
  
  output$out_histogram = renderPlot({
    ggplot(data = filtered(), aes_string(x=input$y))  +
      geom_histogram(aes(y=..count../sum(..count..)), 
                     color='black', fill='azure3',bins=50)+ 
      facet_grid(species~.)+
      theme(plot.margin = unit(c(1, 1, 1, 1), "cm"), panel.spacing.y=unit(2,"lines"),
            panel.background=element_rect(fill='white', colour='azure3'),
            panel.grid.major = element_line(colour="azure3", size=.3), 
            panel.grid.minor = element_line(colour="azure3", size=.3),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            strip.text.y = element_text(size = 0),
            axis.title.x = element_text(size=23,margin = margin(t = 20, r = 0, b = 0, l = 0)),
            axis.title.y = element_text(size=23,margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.text.x = element_text(size=18, vjust = 1),
            axis.text.y = element_text(size=18, vjust = .5),
            plot.title = element_text(lineheight=.8,size=16, hjust=0),
            legend.key=element_rect(colour="transparent", fill="transparent"),
            legend.text=element_text(size=12),
            legend.background = element_rect(colour = 'transparent', fill = 'transparent', linetype='solid'),
            strip.background = element_blank()
            )+
    labs(x = names(metricOptions[which(metricOptions == input$y)]))+
      geom_text_npc(size=6,
        data    = data.frame(label=c("dog","human"), species=c("dog","human")),
        mapping = aes(npcx = .95, npcy =.98, label = label)
      )
    })
}
shinyApp(ui, server)