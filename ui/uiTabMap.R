############ tabMap.R ############
tabPanel("Map",
         div(class="outer",
             tags$head(
               tags$style(HTML("
                               div.outer {
                               
                               position: fixed;
                               top: 51px;
                               left: 0;
                               right: 0;
                               bottom: 0;
                               overflow: hidden;
                               padding: 0;
                               }


#controls {
  /* Appearance */
  background-color: white;
  padding: 0 20px 20px 20px;
  cursor: move;
  /* Fade out while not hovering */
  opacity: 0.65;
  zoom: 0.9;
  transition: opacity 500ms 1s;
}
#controls:hover {
  /* Fade in while hovering */
  opacity: 0.95;
  transition-delay: 0;
}
                               
                               "))
               ),
         leafletOutput("logger_map", width="100%", height="100%"),
         
         absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                       draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                       width = 330, height = "auto",
                       selectInput("choose_map", "Choose Map", choices = unlist(providers)),
                       selectInput("choose_tag", "Choose Tag", choices = NULL),
                       checkboxInput("show_antennae_outline", "Show antennae outlines", value=TRUE),
                       checkboxInput("activate_single_data","Show Timeline",value = FALSE),
                       conditionalPanel(condition="input.activate_single_data==true",
                        sliderInput("choose_single_data_set","Data Steps", min=1, max =500,value = 1, animate = list(interval=700), step = 1),
                        plotOutput("miniplot", height = "150px")
                       ),
                       htmlOutput("singal_select_prop")
         )
    )
)