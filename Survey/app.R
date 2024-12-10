# remotes::install_github("surveydown-dev/surveydown", force = TRUE)
library(surveydown)

# Database setup

# surveydown stores data on a database that you define at https://supabase.com/
# To connect to a database, update the sd_database() function with details
# from your supabase database. For this demo, we set ignore = TRUE, which will
# ignore the settings and won't attempt to connect to the database. This is
# helpful for local testing if you don't want to record testing data in the
# database table. See the documentation for details:
# https://surveydown.org/store-data

db <- sd_database(
  host   = "aws-0-us-east-1.pooler.supabase.com",
  dbname = "postgres",
  port   = "6543",
  user   = "postgres.xshbmvovkegnqhoqwued",
  table  = "ANTH 5203 PreSurvey",
  ignore = T
)


# Server setup
server <- function(input, output, session) {

  # Main server to control the app
  sd_server(all_questions_required = TRUE)

}


# shinyApp() initiates your app - don't change it
shiny::shinyApp(ui = sd_ui(), server = server)
