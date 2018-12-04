
#Open Library and Load FIles

shinyUI(fluidPage(
  
  # Application theme
  theme = shinytheme("united"),

  # Application title
  titlePanel("BCC Discharge and SWE"),

  # Sidebar with user input controls
  sidebarLayout( 
    sidebarPanel(
      selectInput(inputId='site', 
                  label='Choose SNOTEL Site', 
                  choices=unique(merged$site_no),
                  selected = NULL, 
                  multiple = FALSE,
                  selectize = TRUE, 
                  width = NULL, 
                  size = NULL),
      
       dateRangeInput("futuredates", label = h3("Period of Study"),
                      start  = "1986-10-01",
                      end    = "2005-12-31",
                      min    = "1986-10-01",
                      max    = "2005-12-31",
                      format = "mm/dd/yy",
                      separator = " - "),

    #check box for future data                               
    checkboxInput("checkbox", label = "Select SWE Data", value = FALSE),
    
    #submit button
    submitButton("Submit"),
    
    #otes to user  
    helpText("Note:***Review User Guide before moving forward***", style = "color:red")
    ),

    
    
##### Main Panel #####
   mainPanel(

# Show outputs, text, etc. in the main panel
       h3(strong("User Guide"),style = "color:blue"),
       h5("Step 1: Select Site Location",style = "color:blue"),
       h5("Step 2: Define Period of Study",style = "color:blue"),
       h5("Step 3: Check Box to Display SWE Data in First Plot",style = "color:blue"),
       h5("Step 4: Check Submit Button to Populate",style = "color:blue"),
       h5("Step 5: Select Additional Tabs to View Other Information",style = "color:blue"),
     
# Output: Tabset w/ plot, summary, and table ----
    tabsetPanel(type = "tabs",
              tabPanel("Plot", plotOutput("futureplot"), plotOutput("Futureplot")),
              tabPanel("SWE to Dischrage", plotOutput("SWEdischarge")),
              tabPanel("Box Plots", plotOutput("boxplot1"), plotOutput("boxplot2")),
              tabPanel("Site Map", leafletOutput("mymap",height = 500)),
              tabPanel("Notes", plotOutput("Notes"))
               )
            )
  )
))



