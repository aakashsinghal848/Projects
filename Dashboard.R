library(shiny)
library(shinydashboard)
library(plyr)
library(leaflet)


#first read the csv file in variable DF
df= read.csv("\\Users\\Administrator\\Downloads\\Meteorite-Landings.csv")
df= as.data.frame(df)
df= na.omit(df)
#lets check and view the data
nrow(df)
View(df)
df1= df[30000:45000,]
df1

#to check any missing values in the dataframe
sapply(df, anyNA)


#check the type of every column
str(df)

## chane the name of column

names(df)[names(df)=="mass..g."]= "mass"
 
## heaviest meteor definig

max_mass= max(df$mass)
max_mass= format(max_mass, scientific=FALSE)
max_mass

##defining variable which shows rock burn in air

fell_count=nrow(subset(df, fall=="Fell"))
fell_count

Ind_met= sum(ifelse (df$reclat> 8.4 & df$reclat <37.6 & df$reclong> 68.7 & df < 97.25,1,0 ),na.rm=TRUE)
Ind_met

##server which consist dynamic data of the application

server= function(input,output){
    output$Ind_box= renderValueBox({
        valueBox(
            value= Ind_met,
            subtitle= "Number of Meteors fall in India",
            icon= icon("globe"),
            color= if (Ind_met>input$threshold){"blue"} else {"red"}
        )
    })
    output$plot= renderLeaflet({
        leaflet(data = df1)%>%
            addTiles()%>%
            addCircleMarkers(lng = df$reclat, lat = df$reclong, popup = df$year,label= df$name,
                             color = "orange")
    })
    
}
## designing of dashboard body

body_1= dashboardBody(
    fluidRow(
        valueBox(
            width=6,
            value=max_mass,
            subtitle="Heaviest Meteorite Weight",
            icon= icon("meteor")
    ),
    valueBox(
        width = 6,
        value=fell_count,
        subtitle = "Numeber of Meteorites Burn in Air",
        icon=icon("fire")
    
    ),
    
    valueBoxOutput("Ind_box"
                   
    ),
    leafletOutput("plot")
       
    
    )
    
)

## designing of sidebar off ddashboard

sidebar_1= dashboardSidebar(
    sliderInput(
        
        inputId = "threshold",
        label = "To check threshold color",
        min=1,
        max = 45000,
        value = 990
    )
    
)

##interface of dashboard

UI= dashboardPage(
    skin = "red",
    header=dashboardHeader(),
    body= body_1,
    sidebar = sidebar_1
)

## to run the Application command
shinyApp(UI, server)




