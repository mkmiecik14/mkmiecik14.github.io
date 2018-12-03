# Main color palette (Flatly bootstrap theme)
flatlyPal <- c("#2C3E50", "#18BC9C", "#94A5A6", "#3498DC", "#F39C13", "#E74C3C")

# Plot constant
plotFinish <- theme(
  plot.title = element_text(hjust = 0.5, size = 15),
  text = element_text(size = 15),
  plot.caption = element_text(hjust = .5)
)

# Server ----
server <- function(input, output) {

  # Defined variables
  vars <- 2 # Minimum of 2 variables are required for correlation
  mu <- rep(0, vars) # Means of all the vars are 0

  # Calculates sigma
  re_sigma <- reactive({
    matrix(input$rUser, nrow = vars, ncol = vars)
  })

  # Establihses sigma = 1
  re_sigma2 <- reactive({
    re_sigma() - diag(vars) * (input$rUser) + diag(vars)
  })

  # Gathers correlations
  re_rawvars <- reactive({
    as.tibble(mvrnorm(
      n = input$sampleN,
      mu = mu,
      Sigma = re_sigma2(),
      empirical = T
    )) %>%
      rename(x = V1, y = V2)
  })

  # Bootstrapping procedure ----
  re_bootTable <- reactive({
    re_rawvars() %>% modelr::bootstrap(., n = input$bootIters)
  })
  
  re_bootMap <- reactive({
    map(re_bootTable()$strap, ~lm(scale(y) ~ scale(x), data = .))
  })
  
  re_bootResults <- reactive({
     map_df(re_bootMap(), broom::tidy, .id = "id") %>% 
      filter(term != "(Intercept)")
  })

  # Confidence interval calculations ----
  re_ciUserL <- reactive({
    (1 - input$ciUser) / 2
  })
  re_ciUserU <- reactive({
    input$ciUser + re_ciUserL()
  })
  re_ciObs <- reactive({
    quantile(
      re_bootResults()$estimate,
      c(re_ciUserL(), re_ciUserU())
    )
  })

  # Permutation testing procedure ----
  # From: https://www.rdocumentation.org/packages/modelr/versions/0.1.1/topics/permute
  re_perms <- reactive({
    re_rawvars() %>% permute(input$permIters, y)
  })
  re_models <- reactive({
    map(
      re_perms()[["perm"]],
      ~lm(scale(y) ~ scale(x), data = .)
    )
  })
  re_tidyd <- reactive({
    map_df(re_models(), broom::tidy, .id = "id") %>%
      filter(term != "(Intercept)")
  })

  # Calculates p value from permutation testing
  re_permPVal <- reactive({
    if (input$rUser >= 0) {
      (sum(re_tidyd()[["estimate"]] >= input$rUser) + 1) / input$permIters
    } else if (input$rUser < 0) {
      (sum(re_tidyd()[["estimate"]] <= input$rUser) + 1) / input$permIters
    }
  })

  # Scatterplot ----

  output$scatterPlot <- renderPlot({

    # Caption for Scatterplot
    corResP <- round(cor.test(re_rawvars()$x, re_rawvars()$y)$p.value, 3)
    corResPFinal <- ifelse(corResP > .001, corResP, .001)
    pSign <- ifelse(corResP > .001, "= ", "< ")
    scatRes <- paste0(
      "r (", input$sampleN - 2, ") = ", input$rUser,
      ", p ", pSign, corResPFinal
    )

    # Plot
    ggplot(re_rawvars(), aes(x, y)) +
      geom_smooth(
        method = "lm", se = T,
        color = flatlyPal[2], fill = flatlyPal[3]
      ) +
      geom_point(color = flatlyPal[1], alpha = .9) +
      labs(x = "X", y = "Y", caption = scatRes) +
      theme_classic() +
      plotFinish
  })

  # Bootstrap Plot ----
  output$bootPlot <- renderPlot({
    ggplot(re_bootResults(), aes(estimate)) +
      geom_histogram(binwidth = .05, fill = flatlyPal[1]) +
      geom_errorbarh(aes(
        xmin = re_ciObs()[[1]], xmax = re_ciObs()[[2]],
        x = input$rUser, y = nrow(re_bootResults()) / 10
      ),
      height = 0, color = flatlyPal[2], size = 1
      ) +
      geom_point(aes(x = input$rUser, y = nrow(re_bootResults()) / 10),
        size = 4, color = flatlyPal[2]
      ) +
      labs(
        x = "Correlation", y = "Frequency",
        caption = paste0(
          input$ciUser * 100, "% CI [",
          round(re_ciObs()[[1]], 2), ", ",
          round(re_ciObs()[[2]], 2), "]"
        )
      ) +
      theme_classic() +
      plotFinish
  })

  # Permutation plot ----
  output$permPlot <- renderPlot({
    ggplot(re_tidyd(), aes(estimate)) +
      geom_histogram(binwidth = .05, fill = flatlyPal[1]) +
      geom_vline(aes(xintercept = input$rUser),
        color = flatlyPal[2], linetype = 2, size = .75
      ) +
      labs(
        x = "Correlation", y = "Frequency",
        caption = paste0("r = ", input$rUser, ", p = ", round(re_permPVal(), 5))
      ) +
      theme_classic() +
      plotFinish
  })

  output$ui_file <- renderText({
    read_file("ui.R")
  })

  output$server_file <- renderText({
    read_file("server.R")
  })
}
