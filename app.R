library(ElemStatLearn)
library(shinydashboard)
library(shiny)
library(tidyverse)

set.seed(42)

country_df <- data.frame(matrix(nrow = 33, ncol = 2))
names(country_df) <- c("id", "name")
country_num <- c(1:17, 21:32, 34, 35, 40, 41)
courtry_name <-
  c(
    "Mexico",
    "Guatemala",
    "El Salvador",
    "Honduras",
    "Nicaragua",
    "Costa Rica",
    "Panama",
    "Colombia",
    "Ecuador",
    "Bolivia",
    "Peru",
    "Paraguay",
    "Chile",
    "Uruguay",
    "Brazil",
    "Venezuela",
    "Argentina",
    "Dominican Republic",
    "Haiti",
    "Jamaica",
    "Guyana",
    "Trinidad & Tobago",
    "Belize",
    "Suriname",
    "Bahamas",
    "Barbados",
    "Grenada",
    "Saint Lucia",
    "Dominica",
    "Saint Vincent and the Grenadines",
    "Saint Kitts and Nevis",
    "United States",
    "Canada"
  )
country_df["id"] <- country_num
country_df["name"] <- courtry_name


country <-
  c(
    "Mexico",
    "Guatemala",
    "El Salvador",
    "Honduras",
    "Nicaragua",
    "Costa Rica",
    "Panama",
    "Colombia",
    "Ecuador",
    "Bolivia",
    "Peru",
    "Paraguay",
    "Chile",
    "Uruguay",
    "Brazil",
    "Venezuela",
    "Argentina",
    "Dominican Republic",
    "Haiti",
    "Jamaica",
    "Guyana",
    "Trinidad & Tobago",
    "Belize",
    "Suriname",
    "Bahamas",
    "Barbados",
    "Grenada",
    "Saint Lucia",
    "Dominica",
    "Saint Vincent and the Grenadines",
    "Saint Kitts and Nevis",
    "United States",
    "Canada"
  )

topic <-
  c(
    "Authoritarian Values",
    "Control",
    "Corruption",
    "Crime and Rule of Law",
    "Democracy",
    "Economic",
    "Elections and Political Rights",
    "Geographic",
    "Internal Conflict and War",
    "International Context",
    "Interpersonal Trust",
    "Local Government",
    "Political System Support",
    "Positive-Negative Participation",
    "Social Participation",
    "Socio-Demographic",
    "Special Topics"
  )

year <- seq(2000, 2018, 2)

questions <-
  data.frame(matrix("test question", nrow = 3, ncol = length(topic)))
names(questions) <- topic
questions["Democracy"] <-
  c(
    "Democracy is Better than Any Other Form of Government",
    "Satisfaction with Democracy",
    "Perception of Democracy"
  )
questions["Local Government"] <-
  c(
    "Requested Help from Municipal Office",
    "The Issue or Petition Was Resolved",
    "Quality of Municipal Services"
  )


# Fake data
N <- 10000
data <- data.frame(matrix(nrow = N))
data$id <- seq(1, N, 1)
data$country <- sample(courtry_name, N, replace = TRUE)
data$year <- sample(year, N, replace = TRUE)
data$gender <- sample(c("male", "female"), N, replace = TRUE)
data$age <- sample(seq(10, 90, 1), N, replace = TRUE)
data$`Democracy is Better than Any Other Form of Government` <-
  sample(seq(1, 7, 1), N, replace = TRUE)
data$`Satisfaction with Democracy` <-
  sample(seq(1, 4, 1), N, replace = TRUE)
data$`Perception of Democracy` <-
  sample(seq(1, 3, 1), N, replace = TRUE)
data$`Requested Help from Municipal Office` <-
  sample(c(1, 0), N, replace = TRUE)
data$`The Issue or Petition Was Resolved` <-
  sample(c(1, 0), N, replace = TRUE)
data$`Quality of Municipal Services` <-
  sample(seq(1, 5, 1), N, replace = TRUE)
data <- data %>%
  select(
    id,
    country,
    year,
    age,
    gender,
    `Democracy is Better than Any Other Form of Government`,
    `Satisfaction with Democracy`,
    `Perception of Democracy`,
    `Requested Help from Municipal Office`,
    `The Issue or Petition Was Resolved`,
    `Quality of Municipal Services`
  ) %>%
  mutate(
    id = as.integer(id),
    gender = as.factor(gender),
    country = as.factor(country),
    year = as.integer(year),
    `Democracy is Better than Any Other Form of Government` = as.integer(`Democracy is Better than Any Other Form of Government`),
    `Satisfaction with Democracy` = as.integer(`Satisfaction with Democracy`),
    `Perception of Democracy` = as.integer(`Perception of Democracy`),
    `Requested Help from Municipal Office` = as.integer(`Requested Help from Municipal Office`),
    `The Issue or Petition Was Resolved` = as.integer(`The Issue or Petition Was Resolved`),
    `Quality of Municipal Services` = as.integer(`Quality of Municipal Services`)
  ) %>%
  arrange(country, year, age, gender)


## app.R ##

header <-
  dashboardHeader(title = "LAPOP Dashboard", titleWidth = 300)



sidebar <- dashboardSidebar(width = 300,
                            sidebarMenuOutput("sidebar"))



body <- dashboardBody(tabItems(
  tabItem(
    tabName = "overview",
    # Boxes need to be put in a row (or column)
    fluidRow(
      # Dynamic valueBoxes
      valueBoxOutput("numberBox"),
      valueBoxOutput("femalepercentBox"),
      valueBoxOutput("ageBox")
    ),
    fluidRow(
      tabBox(
        title = "Histogram of Respondents' age structure",
        tabPanel("Overall",  plotOutput("overview_hist")),
        tabPanel("Gender",  plotOutput("overview_hist_gender"))
      ),
      tabBox(
        title = "Density of Respondents' age structure",
        tabPanel("Overall",  plotOutput("overview_density")),
        tabPanel("Gender",  plotOutput("overview_density_gender"))
      ),
      
      box(
        title = "Total number of respondents each year",
        br(),
        "More box content",
        plotOutput("overview_year"),
        solidHeader = TRUE,
        collapsible = TRUE
      ),
      box(
        title = "Female ratio of respondents each year",
        br(),
        "More box content",
        plotOutput("overview_gender_year"),
        solidHeader = TRUE,
        collapsible = TRUE
      ) #,
      # box(solidHeader = TRUE,
      #     collapsible = TRUE,
      #     title = "Box content here", br(), "More box content",
      #     sliderInput("slider", "Slider input:", 1, 100, 50),
      #     textInput("text", "Text input:")
      #     )
    )
  ),
  
  
  tabItem(
    tabName = "basic",
    fluidRow(
      # Dynamic valueBoxes
      valueBoxOutput("basic_scaleBox"),
      valueBoxOutput("basic_meanBox"),
      valueBoxOutput("basic_modeBox")
    ),
    fluidRow(
      box(
        title = "Question Introduction:",
        solidHeader = TRUE,
        collapsible = TRUE
      )
    ),
    fluidRow(
      tabBox(
        title = "Distribution of Answers",
        tabPanel("Overall",  plotOutput("basic_scorehist")),
        tabPanel("Gender",  plotOutput("basic_scorehist_gender"))
      ),
      tabBox(
        title = "Average Answer for Each Age",
        tabPanel("Overall",  plotOutput("basic_agedist")),
        tabPanel("Gender",  plotOutput("basic_agedist_gender"))
      ),
      box(
        title = "Boxplot of Answer by Gender",
        plotOutput("basic_genderscore"),
        solidHeader = TRUE,
        collapsible = TRUE
      )#,
      #box(textOutput("basic_question1"))
      
    ),
    fluidRow(
      box(
        title = "Original table:",
        tableOutput("basic_table"),
        solidHeader = TRUE,
        collapsible = TRUE
      )
    )
  ),
  
  
  tabItem(tabName = "timeseries",
          fluidRow(
            tabBox(
              title = "Average answer by Year",
              tabPanel("Overall",  plotOutput("time_year_mean_score")),
              tabPanel("Gender",  plotOutput("time_year_mean_score_gender"))
            ),
            tabBox(
              title = "Boxplot of distribution by Year",
              tabPanel("Overall",  plotOutput("time_year_boxplot")),
              tabPanel("Gender",  plotOutput("time_year_boxplot_gender"))
            ),
            box(
              title = "Heat map",
              plotOutput("time_heatplot"),
              solidHeader = TRUE,
              collapsible = TRUE
            )
          )),
  
  
  tabItem(
    tabName = "cross",
    fluidRow(
      box(
        selectInput(
          "topic2",
          "Choose topic for cross:",
          topic,
          selected = "Local Government",
          multiple = FALSE
        ),
        uiOutput("cross_select")
      ),
      box(
        title = "Introduction",
        solidHeader = TRUE,
        collapsible = TRUE
      )
    ),
    fluidRow(
      # Dynamic valueBoxes
      valueBoxOutput("cross_scaleBox"),
      valueBoxOutput("cross_meanBox"),
      valueBoxOutput("cross_corBox")
    ),
    fluidRow(
      tabBox(
        title = "Average answer by Year",
        tabPanel("Overall",  plotOutput("cross_cross_mean_score")),
        tabPanel("Gender",  plotOutput("cross_cross_mean_score_gender"))
      ),
      tabBox(
        title = "Boxplot of distribution by Year",
        tabPanel("Overall",  plotOutput("cross_boxplot")),
        tabPanel("Gender",  plotOutput("cross_boxplot_gender"))
      ),
      box(
        title = "Heat map",
        plotOutput("cross_heatplot"),
        solidHeader = TRUE,
        collapsible = TRUE
      )
    )
  )
  
))



ui <- dashboardPage(header, sidebar, body, skin = "purple")

server <- function(input, output) {
  # Sidebar
  output$sidebar <- renderMenu({
    sidebarMenu(
      selectInput(
        "country",
        "Choose countries:",
        country,
        selected = "Canada",
        multiple = TRUE
      ),
      selectInput(
        "year",
        "Choose year:",
        year,
        selected = c(2014, 2016, 2018),
        multiple = TRUE
      ),
      menuItem(
        "Respondent Overview",
        tabName = "overview",
        icon = icon("dashboard")
      ),
      selectInput(
        "topic1",
        "Choose topic:",
        topic,
        selected = "Democracy",
        multiple = FALSE
      ),
      selectInput("question1", "Choose question:", questions[input$topic1], multiple = FALSE),
      menuItem(
        "Basic Analysis",
        tabName = "basic",
        icon = icon("chart-bar")
      ),
      menuItem(
        "Time Series Analysis",
        tabName = "timeseries",
        icon = icon("chart-line")
      ),
      menuItem(
        "Cross Analysis",
        tabName = "cross",
        icon = icon("th")
      ),
      menuItem(
        "Download Data",
        icon = icon("download"),
        href = "http://lapop.ccp.ucr.ac.cr/en/"
      )
      
    )
  })
  
  # Overview
  output$numberBox <- renderValueBox({
    df <- data %>%
      filter(year %in% input$year,
             country %in% input$country)
    
    valueBox(nrow(df),
             "Respondents",
             icon = icon("users"),
             color = "purple")
  })
  
  output$femalepercentBox <- renderValueBox({
    df <- data %>%
      filter(year %in% input$year,
             country %in% input$country)
    
    df_female <- data %>%
      filter(year %in% input$year,
             country %in% input$country,
             gender == "female")
    
    valueBox(
      paste0(round(
        nrow(df_female) / nrow(df) * 100, digits = 2
      ), "%"),
      "Female Ratio",
      icon = icon("venus-double"),
      color = "yellow"
    )
  })
  
  output$ageBox <- renderValueBox({
    df <- data %>%
      filter(year %in% input$year,
             country %in% input$country)
    
    valueBox(
      round(mean(df$age), digits = 2),
      "Average age",
      icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$overview_hist <- renderPlot({
    data %>%
      filter(year %in% input$year,
             country %in% input$country) %>%
      ggplot() +
      geom_bar(aes(x = age), width = 1, fill = "#1C6771") +
      theme_classic() +
      theme(legend.position = "top")
  })
  
  output$overview_hist_gender <- renderPlot({
    data %>%
      filter(year %in% input$year,
             country %in% input$country) %>%
      ggplot() +
      geom_bar(aes(x = age, fill = gender),
               width = 1,
               position = "dodge") +
      theme_classic() +
      theme(legend.position = "top")
  })
  
  output$overview_density <- renderPlot({
    data %>%
      filter(year %in% input$year,
             country %in% input$country) %>%
      ggplot() +
      geom_density(aes(x = age),
                   fill = "#1C6771",
                   color =  "#1C6771",
                   alpha = 0.5) +
      theme_classic() +
      theme(legend.position = "top")
  })
  
  output$overview_density_gender <- renderPlot({
    data %>%
      filter(year %in% input$year,
             country %in% input$country) %>%
      ggplot() +
      geom_density(aes(
        x = age,
        fill = gender,
        alpha = 0.5,
        color = gender
      )) +
      theme_classic() +
      theme(legend.position = "top")
  })
  
  output$overview_year <- renderPlot({
    data %>%
      filter(year %in% input$year,
             country %in% input$country) %>%
      group_by(year, gender) %>%
      summarise(number = n()) %>%
      ggplot() +
      geom_point(aes(x = year, y = number, color = gender), size = 2) +
      geom_line(aes(x = year, y = number, color = gender), size = 1.5) +
      theme_classic() +
      theme(legend.position = "top")
    
  })
  
  output$overview_gender_year <- renderPlot({
    data %>%
      filter(year %in% input$year,
             country %in% input$country) %>%
      group_by(year) %>%
      mutate(all_number = n()) %>%
      ungroup() %>%
      group_by(year, gender) %>%
      mutate(number = n()) %>%
      mutate(ratio = number / all_number) %>%
      filter(gender == "female") %>%
      summarise(female_ratio = mean(ratio)) %>%
      ggplot() +
      geom_line(aes(x = year, y = female_ratio),
                size = 1.5,
                color = "purple") +
      geom_point(aes(x = year, y = female_ratio),
                 size = 2.5,
                 color = "purple") +
      geom_smooth(
        aes(x = year, y = female_ratio),
        size = 1.5,
        color = "lightblue",
        se = FALSE
      ) +
      theme_classic() +
      theme(legend.position = "top")
    
  })
  
  
  
  
  
  # Basic
  
  output$basic_question1 <- renderText({
    question1 <- input$question1
    question1
  })
  
  
  output$basic_scaleBox <- renderValueBox({
    question1 <- input$question1
    df <- data %>%
      filter(year %in% input$year,
             country %in% input$country) %>%
      select(question1)
    valueBox(
      paste0(min(unique(as.matrix(
        df
      ))), " - ", max(unique(as.matrix(
        df
      )))),
      "Answer Scale",
      icon = icon("list-ol"),
      color = "purple"
    )
  })
  
  
  output$basic_meanBox <- renderValueBox({
    question1 <- input$question1
    df <- data %>%
      filter(year %in% input$year,
             country %in% input$country) %>%
      select(question1)
    
    valueBox(
      round(mean(as.matrix(df)), digits = 2),
      "Average answer",
      icon = icon("list"),
      color = "yellow"
    )
  })
  
  output$basic_modeBox <- renderValueBox({
    question1 <- input$question1
    df <- data %>%
      filter(year %in% input$year,
             country %in% input$country) %>%
      select(question1)
    mode <- names(sort(-table(as.matrix(df))))[1]
    valueBox(mode,
             "Mode answer",
             icon = icon("sort-amount-down"),
             color = "purple")
  })
  
  
  output$basic_scorehist <- renderPlot({
    question1 <- input$question1
    data %>%
      filter(year %in% input$year,
             country %in% input$country) %>%
      group_by(!!as.symbol(question1), gender) %>%
      summarise(number = n()) %>%
      ggplot() +
      geom_col(aes(x = !!as.symbol(question1), y = number),
               width = 0.7,
               fill = "#1C6771") +
      geom_smooth(
        aes(x = !!as.symbol(question1), y = number),
        se = FALSE,
        color = "lightblue",
        lwd = 2
      ) +
      theme_classic() +
      labs(title = paste0("Distribution for Answer of ", question1),
           x = "Answer") +
      theme(legend.position = "top")
  })
  
  output$basic_scorehist_gender <- renderPlot({
    question1 <- input$question1
    data %>%
      filter(year %in% input$year,
             country %in% input$country) %>%
      group_by(!!as.symbol(question1), gender) %>%
      summarise(number = n()) %>%
      ggplot() +
      geom_col(
        aes(
          x = !!as.symbol(question1),
          y = number,
          fill = gender
        ),
        width = .7,
        position = "dodge"
      ) +
      theme_classic() +
      labs(title = paste0("Distribution for Answer of ", question1),
           x = "Answer") +
      theme(legend.position = "top")
  })
  
  output$basic_agedist <- renderPlot({
    question1 <- input$question1
    data %>%
      filter(year %in% input$year,
             country %in% input$country) %>%
      group_by(age) %>%
      summarise(mean_score = mean(!!as.symbol(question1))) %>%
      ggplot() +
      geom_col(aes(x = age, y = mean_score),
               width = 1,
               fill = "#1C6771") +
      geom_smooth(
        aes(x = age, y = mean_score),
        se = FALSE,
        color = "lightblue",
        lwd = 2
      ) +
      theme_classic() +
      labs(title = paste0("Answer of ", question1, " by Age"),
           x = "Age")
  })
  
  
  output$basic_agedist_gender <- renderPlot({
    question1 <- input$question1
    data %>%
      filter(year %in% input$year,
             country %in% input$country) %>%
      group_by(age, gender) %>%
      summarise(mean_score = mean(!!as.symbol(question1))) %>%
      ggplot() +
      geom_col(aes(x = age, y = mean_score, fill = gender),
               width = 1,
               position = "dodge") +
      # geom_smooth(aes(x = age, y= mean_score), se = FALSE, color = "lightblue", lwd = 2)+
      theme_classic() +
      labs(title = paste0("Answer of ", question1, " by Age"),
           x = "Age") +
      theme(legend.position = "top")
  })
  
  
  output$basic_genderscore <- renderPlot({
    question1 <- input$question1
    data %>%
      filter(year %in% input$year,
             country %in% input$country) %>%
      ggplot() +
      geom_boxplot(
        aes(
          x = gender,
          y = !!as.symbol(question1),
          fill = gender,
          color = gender
        ),
        lwd = 1,
        alpha = 0.7
      ) +
      theme_classic() +
      labs(
        title = paste0("Answer of ", question1, " by Gender"),
        x = "Gender",
        y = "Answer"
      )
  })
  
  
  
  
  output$basic_table <- renderTable({
    question1 <- input$question1
    data %>%
      filter(year %in% input$year,
             country %in% input$country) %>%
      select(id, country, year, gender, question1)
  })
  
  
  
  
  # Time Series
  output$time_year_mean_score <- renderPlot({
    question1 <- input$question1
    data %>%
      filter(year %in% input$year,
             country %in% input$country) %>%
      group_by(year) %>%
      summarise(mean_score = mean(!!as.symbol(question1))) %>%
      ggplot() +
      geom_col(aes(x = as.factor(year), y = mean_score),
               width = 0.7,
               fill = "#1C6771") +
      # geom_smooth(aes(x = year, y= mean_score), se = FALSE, color = "lightblue", lwd = 2)+
      theme_classic() +
      labs(title = paste0("Answer of ", question1, " by Year"),
           x = "Year") +
      theme(legend.position = "top")
  })
  
  output$time_year_mean_score_gender <- renderPlot({
    question1 <- input$question1
    data %>%
      filter(year %in% input$year,
             country %in% input$country) %>%
      group_by(year, gender) %>%
      summarise(mean_score = mean(!!as.symbol(question1))) %>%
      ggplot() +
      geom_col(
        aes(
          x = as.factor(year),
          y = mean_score,
          fill = gender,
        ),
        width = 0.7,
        position = "dodge"
      ) +
      theme_classic() +
      labs(title = paste0("Answer of ", question1, " by Year(Gender)"),
           x = "Year") +
      theme(legend.position = "top")
  })
  
  output$time_year_boxplot <- renderPlot({
    question1 <- input$question1
    data %>%
      filter(year %in% input$year,
             country %in% input$country) %>%
      ggplot() +
      geom_boxplot(
        aes(x = as.factor(year),
            y = !!as.symbol(question1)),
        lwd = 1,
        alpha = 0.7,
        fill = "#1C6771",
        color = "#1C6771"
      ) +
      theme_classic() +
      labs(title = paste0("Boxplot of Answer of ", question1, " by Year"),
           x = "Year") +
      theme(legend.position = "top")
  })
  
  
  output$time_year_boxplot_gender <- renderPlot({
    question1 <- input$question1
    data %>%
      filter(year %in% input$year,
             country %in% input$country) %>%
      ggplot() +
      geom_boxplot(
        aes(
          x = as.factor(year),
          y = !!as.symbol(question1),
          fill = gender,
          color = gender
        ),
        lwd = 1,
        alpha = 0.7
      ) +
      theme_classic() +
      labs(title = paste0("Boxplot of Answer of ", question1, " by Year"),
           x = "Year") +
      theme(legend.position = "top")
  })
  
  
  output$time_heatplot <- renderPlot({
    question1 <- input$question1
    
    filter_data <- data %>%
      filter(year %in% input$year,
             country %in% input$country)
    whole_number <- nrow(filter_data)
    
    data %>%
      filter(year %in% input$year,
             country %in% input$country) %>%
      group_by(!!as.symbol(question1), year) %>%
      summarise(number = n() / whole_number) %>%
      ggplot(aes(as.factor(!!as.symbol(question1)), as.factor(year))) +
      geom_tile(aes(fill = number), color = "white") +
      geom_text(aes(label = paste0(round(
        number * 100, digits = 2
      ), "%")),
      color = "white",
      check_overlap = TRUE) +
      theme_minimal() +
      labs(x = question1, y = "Year") +
      theme(legend.position = "none") +
      scale_fill_gradient(low = "white", high = "#1C6771")
  })
  
  
  #Cross
  output$cross_select <- renderUI({
    selectInput(
      "question2",
      "Choose question for cross:",
      questions[input$topic2],
      selected = "Quality of Municipal Services",
      multiple = FALSE
    )
  })
  
  output$cross_scaleBox <- renderValueBox({
    question2 <- input$question2
    df <- data %>%
      filter(year %in% input$year,
             country %in% input$country) %>%
      select(question2)
    valueBox(
      paste0(min(unique(as.matrix(
        df
      ))), " - ", max(unique(as.matrix(
        df
      )))),
      "Answer Scale of Cross Question",
      icon = icon("list-ol"),
      color = "purple"
    )
  })
  
  
  output$cross_meanBox <- renderValueBox({
    question2 <- input$question2
    df <- data %>%
      filter(year %in% input$year,
             country %in% input$country) %>%
      select(question2)
    
    valueBox(
      round(mean(as.matrix(df)), digits = 2),
      "Average answer of Cross Question",
      icon = icon("list"),
      color = "yellow"
    )
  })
  
  output$cross_corBox <- renderValueBox({
    question1 <- input$question1
    question2 <- input$question2
    df <- data %>%
      filter(year %in% input$year,
             country %in% input$country)
    question1_df <- df %>% select(question1)
    question2_df <- df %>% select(question2)
    cor <- cor(as.matrix(question1_df), as.matrix(question2_df))
    valueBox(
      round(cor, digits = 2),
      "Correlation",
      icon = icon("sync-alt"),
      color = "purple"
    )
  })
  
  
  output$cross_cross_mean_score <- renderPlot({
    question1 <- input$question1
    question2 <- input$question2
    data %>%
      group_by(!!as.symbol(question1)) %>%
      summarise(mean_cross_variable = mean(!!as.symbol(question2))) %>%
      ggplot() +
      geom_col(aes(x = as.factor(!!as.symbol(question1)), y = mean_cross_variable),
               width = .5,
               fill = "#1C6771") +
      geom_smooth(
        aes(x = !!as.symbol(question1), y = mean_cross_variable),
        size = 1.5,
        color = "lightblue",
        se = FALSE
      ) +
      theme_classic() +
      labs(x = paste0("Mean of", question1), y = question2)
  })
  
  
  output$cross_cross_mean_score_gender <- renderPlot({
    question1 <- input$question1
    question2 <- input$question2
    data %>%
      group_by(!!as.symbol(question1), gender) %>%
      summarise(mean_cross_variable = mean(!!as.symbol(question2))) %>%
      ggplot() +
      geom_col(
        aes(
          x = as.factor(!!as.symbol(question1)),
          y = mean_cross_variable,
          fill = gender
        ),
        width = .5,
        position = "dodge"
      ) +
      # geom_smooth(aes(x=!!as.symbol(question1), y=mean_cross_variable),
      #             size=1.5,
      #             color="lightblue",
      #             se = FALSE)+
      theme_classic() +
      labs(x = paste0("Mean of", question1), y = question2)
  })
  
  
  
  output$cross_boxplot <- renderPlot({
    question1 <- input$question1
    question2 <- input$question2
    data %>%
      filter(year %in% input$year,
             country %in% input$country) %>%
      ggplot() +
      geom_boxplot(
        aes(
          x = as.factor(!!as.symbol(question1)),
          y = !!as.symbol(question2)
        ),
        lwd = 1,
        alpha = 0.7,
        fill = "#1C6771",
        color = "#1C6771"
      ) +
      theme_classic() +
      labs(x = question1, y = question2) +
      theme(legend.position = "top")
  })
  
  output$cross_boxplot_gender <- renderPlot({
    question1 <- input$question1
    question2 <- input$question2
    data %>%
      filter(year %in% input$year,
             country %in% input$country) %>%
      ggplot() +
      geom_boxplot(
        aes(
          x = as.factor(!!as.symbol(question1)),
          y = !!as.symbol(question2),
          fill = gender,
          color = gender
        ),
        lwd = 1,
        alpha = 0.7
      ) +
      theme_classic() +
      labs(x = question1, y = question2) +
      theme(legend.position = "top")
  })
  
  
  output$cross_heatplot <- renderPlot({
    question1 <- input$question1
    question2 <- input$question2
    
    filter_data <- data %>%
      filter(year %in% input$year,
             country %in% input$country)
    whole_number <- nrow(filter_data)
    
    data %>%
      filter(year %in% input$year,
             country %in% input$country) %>%
      group_by(!!as.symbol(question1),!!as.symbol(question2)) %>%
      summarise(number = n() / whole_number) %>%
      ggplot(aes(as.factor(!!as.symbol(question1)), as.factor(!!as.symbol(question2)))) +
      geom_tile(aes(fill = number), color = "white") +
      geom_text(aes(label = paste0(round(
        number * 100, digits = 2
      ), "%")),
      color = "white",
      check_overlap = TRUE) +
      theme_minimal() +
      labs(x = question1, y = question2) +
      theme(legend.position = "none") +
      scale_fill_gradient(low = "white", high = "#1C6771")
    
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)