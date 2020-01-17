#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(DT)
library(spotifyr)
library(plotly)
library(httr)
library(plyr)

kat <- read_csv("kat.csv") %>% keep(is.numeric) %>% dplyr::select(4:16)
sp <- read_csv("spotify.csv")

spc <- sp %>% dplyr::select(11:23) %>% mutate(like=0)


mine_katc <- kat %>% mutate(like=1)
colnames(mine_katc) <- colnames(spc)
new <- rbind.fill(spc,mine_katc)

new <- new %>% 
    mutate(
        key=as.factor(key),
        mode=as.factor(mode),
        time_signature=as.factor(time_signature),
        like=as.numeric(like)
    )

sp_new1 <- new %>%
    distinct() %>% 
    keep(is.numeric) %>% 
    mutate(like=as.factor(like))
sp_new2 <- data.frame((sp_new1[1:10]))

sp_new2[11] <- sp_new1[11]
sp_new1 <- sp_new2

set.seed(123)
library(caret)
dsub <- createDataPartition(sp_new1$like,p=0.7,list = F)
sp_train <- sp_new1[dsub,]
sp_test <- sp_new1[-dsub,]
sp_train <- DMwR::SMOTE(like ~ ., data.frame(sp_train), perc.over = 100, perc.under = 200)


logreg=glm(like ~ 
               danceability+
               loudness+
               speechiness+
               instrumentalness+
               liveness+
               valence+
               duration_ms,
           data = na.omit(sp_train),
           family = binomial(link="logit"))

like_pred <- predict(logreg, sp_test, type = "response")

ideal_cutoff <- InformationValue::optimalCutoff(
    actuals = sp_test$like,
    predictedScores = like_pred,
    optimiseFor = "Both")
colnames(kat)




# Note that secret is not really secret, and it's fine to include inline
#options(shiny.host = '0.0.0.0', shiny.port = 1410, shiny.trace = TRUE) 
if (interactive()) {
    # testing url
    options(shiny.host = '0.0.0.0', shiny.port = 1410, shiny.trace = TRUE)
    APP_URL <- "http://localhost:1410/"
} else {
    # deployed URL
    APP_URL <- "https://yan-yun.shinyapps.io/spotify/"
}

app <- oauth_app("shinygithub",
                 key = "db1c8d5fd8324997b4d677e617e2fad7",
                 secret = "2e55e7cde31544d2935897f9eb17df8f",
                 redirect_uri = APP_URL
)

# Here I'm using a canned endpoint, but you can create with oauth_endpoint()
api <- oauth_endpoint(NULL, "https://accounts.spotify.com/authorize",
                      "https://accounts.spotify.com/api/token")

# Always request the minimal scope needed. For github, an empty scope
# gives read-only access to public info
scope <- ("ugc-image-upload,user-read-playback-state,streaming,user-read-email,playlist-read-collaborative,user-modify-playback-state,user-read-private,playlist-modify-public,user-library-modify,user-top-read,user-read-currently-playing,playlist-read-private,user-follow-read,app-remote-control,user-read-recently-played,playlist-modify-private,user-follow-modify,user-library-read")
state <- 'F0w03Nr2kV'

has_auth_code <- function(params) {
    return(!is.null(params$code))
}


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Spotify Research"),
    
    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(3,
        selectInput(inputId = "choice1",
                    label = "x-axis",
                    choices = c("danceability","energy","key","loudness","mode","speechiness","acousticness", "instrumentalness","liveness","valence","tempo","duration_ms","time_signature"))),
        column(3,
        selectInput(inputId = "choice2",
                    label = "y-axis",
                    choices = c("energy","danceability","key","loudness","mode","speechiness","acousticness", "instrumentalness","liveness","valence","tempo","duration_ms","time_signature"))),
        column(3,
            textInput(inputId = "mpg",
                         label = "What is the track id?",
                         value = '4taTrtJjCPpgr4cQR7WMnQ')),
        column(3,
            actionButton("submit", ("Submit")))
    ),
    # Show a plot of the generated distribution
    verbatimTextOutput("recommendation"),
    dataTableOutput("summary"), 
    plotlyOutput("choicePlot", width = 'auto', height = 'auto')
)

uiFunc <- function(req) {
    if (!has_auth_code(parseQueryString(req$QUERY_STRING))) {
        url <- oauth2.0_authorize_url(api, app, scope = scope,state=state)
        redirect <- sprintf("location.replace(\"%s\");", url)
        tags$script(HTML(redirect))
    } else {
        token <- oauth2.0_token(
            endpoint = api,
            app = app,
            credentials = oauth2.0_access_token(api, app, parseQueryString(req$QUERY_STRING)$code),
            cache = TRUE
        )
        return(ui)
    }
}

# Define server logic required to draw a histogram
server <- function(input, output) {
    

    
    
    out <- eventReactive(input$submit, {
        Sys.setenv(SPOTIFY_CLIENT_ID = 'db1c8d5fd8324997b4d677e617e2fad7')
        Sys.setenv(SPOTIFY_CLIENT_SECRET = '2e55e7cde31544d2935897f9eb17df8f')
        temp <- get_track_audio_features(input$mpg)
        temp <- temp %>% dplyr::select(1:11,17:18)
        
    })
    
    out1 <- eventReactive(input$submit, {
        Sys.setenv(SPOTIFY_CLIENT_ID = 'db1c8d5fd8324997b4d677e617e2fad7')
        Sys.setenv(SPOTIFY_CLIENT_SECRET = '2e55e7cde31544d2935897f9eb17df8f')
        temp <- get_track_audio_features(input$mpg)
        temp_pred <- predict(logreg, temp, type = "response")
        
    })
    
    output$recommendation <- renderText({
        if(out1() < ideal_cutoff) print("Ummm, I don't think Katherine would like this song.")
        else print('A nice song! I believe Katherine would enjoy it!')
    })

    output$summary <- renderDataTable({
    

    datatable(out(), options = list(scrollX = TRUE,orderClasses = TRUE,lengthMenu = c(1, 2, 3), pageLength = 1))})

    
    output$choicePlot <- renderPlotly({
        p1 <- ggplot() +
            geom_point(data=kat,aes_string(x=input$choice1,y=input$choice2))+
            geom_point(data=out(),aes_string(x=input$choice1,y=input$choice2),color="red",size=5)+
            theme_minimal()
        
       ggplotly(p1)
    })
}

# Run the application 
shinyApp(ui = uiFunc, server = server)
