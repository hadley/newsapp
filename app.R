library(httr2)
library(shiny)
library(shinydashboard)

get_news_articles <- function(query, days_ago = 1, page_size = 10) {
  req <- request("https://newsapi.org/v2/everything") |>
    req_url_query(
      q = paste0('`"', query, '"`'),
      from = Sys.Date() - days_ago,
      pageSize = page_size,
      apiKey = Sys.getenv("NEWS_API_KEY")
    )
  resp <- req_perform(req)
  resp_body_json(resp)
}

ui <- dashboardPage(
  dashboardHeader(title = "News Search"),
  dashboardSidebar(
    sidebarMenu(
      textInput("query", "Search Query", ""),
      numericInput("days_ago", "Days Ago", 1, min = 1, max = 30),
      numericInput("page_size", "Number of Articles", 10, min = 1, max = 100),
      actionButton("search", "Search")
    )
  ),
  dashboardBody(
    uiOutput("articles")
  )
)

server <- function(input, output, session) {
  results <- eventReactive(input$search, {
    get_news_articles(input$query, input$days_ago, input$page_size)
  })

  output$articles <- renderUI({
    req(results())
    articles <- results()$articles
    if (length(articles) == 0) {
      return(h3("No articles found."))
    }
    lapply(articles, function(article) {
      shinydashboard::box(
        title = article$title,
        p(article$description),
        a(href = article$url, "Read more", target = "_blank")
      )
    })
  })
}

shinyApp(ui, server)
# Example usage:
# results <- get_news_articles("cats")
# results <- get_news_articles("dogs", days_ago = 7, page_size = 20)
