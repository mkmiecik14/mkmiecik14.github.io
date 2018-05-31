# Packages required ----
library(shiny); library(tidyverse); library(broom); library(MASS); 
library(RColorBrewer); library(psych); library(modelr); library(gridExtra)
library(shinythemes); library(shinycssloaders)

# Global aesthetics ----

# Main color palette (Flatly bootstrap theme)
flatlyPal <- c('#2C3E50', '#18BC9C', '#94A5A6', '#3498DC', '#F39C13', '#E74C3C') 

# loading spinner options
options(spinner.color = flatlyPal[1], spinner.type = 8)

# Plot constant
plotFinish <- theme(plot.title = element_text(hjust = 0.5),
                    text = element_text(size = 15),
                    plot.caption = element_text(hjust = .5)
                    )

# Turning off scientific notation for p-values
options(scipen = 999)

# UI ----

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"),
  
  # Title
  h2("Bootstrapping and Permutation Testing"),
  
  # Author info
  h4("Matthew J. Kmiecik & Ekarin Pongpipat"),
  
  # Blogpost info
  p("See our ", 
    a("blog post", href="https://mattkmiecik.com/"), 
    "for more information about this shiny app."
    ),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "rUser",
                  label = "Correlation (r): ",
                  min = -1,
                  max = 1,
                  value = .3,
                  step = .1,
                  ticks = FALSE),
      sliderInput(inputId = "sampleN",
                  label = "Sample size (N): ",
                  min = 10,
                  max = 2000,
                  value = 100,
                  step = 10,
                  ticks = FALSE),
      sliderInput(inputId = "bootIters",
                  label = "Bootstrap Iterations: ",
                  min = 100,
                  max = 2000,
                  value = 100,
                  step = 100,
                  ticks = FALSE),
      sliderInput(inputId = "ciUser",
                  label = "Bootstrap Confidence Interval: ",
                  min = .80,
                  max = .99,
                  value = .90,
                  step = .01,
                  ticks = FALSE),
      sliderInput(inputId = "permIters",
                  label = "Permutation Iterations: ",
                  min = 100,
                  max = 2000,
                  value = 100,
                  step = 100,
                  ticks = FALSE), 
      width = "2"),
    
    # Plots
    mainPanel(
      splitLayout(
        plotOutput("scatterPlot") %>% withSpinner(),
        plotOutput("bootPlot")  %>% withSpinner(),
        plotOutput("permPlot")  %>% withSpinner()
      )
    )
  )
)

# Server ----
server <- function(input, output) {

  # Defined variables
  vars    <- 2 # Minimum of 2 variables are required for correlation
  mu <- rep(0, vars) # Means of all the vars are 0
  
  # Calculates sigma
  re_sigma <- reactive({matrix(input$rUser, nrow = vars, ncol = vars)})
  
  # Establihses sigma = 1
  re_sigma2 <- reactive({re_sigma() - diag(vars)*(input$rUser) + diag(vars)}) 
  
  # Gathers correlations
  re_rawvars <- reactive({as.tibble(mvrnorm(n = input$sampleN, 
                                            mu = mu, 
                                            Sigma = re_sigma2(), 
                                            empirical = T
                                            )
                                    ) %>%
      rename(x = V1, y = V2)}
      )
  
  # Bootstrapping procedure ----
  re_bootResults <- reactive({re_rawvars() %>%
    broom::bootstrap(input$bootIters) %>%
    do(tidy(lm(scale(y) ~ scale(x), .))) %>%
    filter(term != '(Intercept)')})
   
  # Confidence interval calculations ----
  re_ciUserL <- reactive({(1-input$ciUser)/2})
  re_ciUserU <- reactive({input$ciUser + re_ciUserL()})
  re_ciObs <- reactive({quantile(re_bootResults()$estimate, 
                                 c(re_ciUserL(), re_ciUserU()))})

  # Permutation testing procedure ----
  # From: https://www.rdocumentation.org/packages/modelr/versions/0.1.1/topics/permute
  re_perms <- reactive({re_rawvars() %>% permute(input$permIters, y)})
  re_models <- reactive({map(re_perms()[["perm"]], 
                             ~ lm(scale(y) ~ scale(x), data = .))})
  re_tidyd <- reactive({map_df(re_models(), broom::tidy, .id = 'id') %>% 
      filter(term != '(Intercept)')})
  
  # Calculates p value from permutation testing
  re_permPVal <- reactive({
    if (input$rUser >= 0) {
      (sum(re_tidyd()[["estimate"]] >= input$rUser) + 1)/input$permIters
    } else if ( input$rUser < 0 ) {
      (sum(re_tidyd()[["estimate"]] <= input$rUser) + 1)/input$permIters
    }
  }
  )
    
  # Scatterplot ----
  
   output$scatterPlot <- renderPlot({
     
     # Caption for Scatterplot
     corResP <- round(cor.test(re_rawvars()$x, re_rawvars()$y)$p.value, 3)
     corResPFinal <- ifelse(corResP > .001, corResP, .001)
     pSign <- ifelse(corResP > .001, '= ', '< ')
     scatRes <- paste0('r (', input$sampleN-2, ') = ', input$rUser,
                       ', p ', pSign,corResPFinal)
     
     # Plot
     ggplot(re_rawvars(), aes(x, y)) +
       geom_smooth(method = 'lm', se = T, 
                   color = flatlyPal[6], fill = flatlyPal[3]) +
       geom_point(color = flatlyPal[1], alpha = 2/3) +
       labs(x = 'X', y = 'Y', 
            title = 'Scatterplot', caption = scatRes) +
       theme_classic() +
       plotFinish
     
        })

  # Bootstrap Plot ----
   output$bootPlot <- renderPlot({
     ggplot(re_bootResults(), aes(estimate)) +
       geom_vline(aes(xintercept = input$rUser), 
                  color = flatlyPal[6], linetype = 3) +
       geom_histogram(binwidth = .05, fill = flatlyPal[1]) +
       geom_errorbarh(aes(xmin = re_ciObs()[[1]], xmax = re_ciObs()[[2]], 
                          x = input$rUser, y = nrow(re_bootResults())/10), 
                      height = 0, color = flatlyPal[2], size = 1) +
       geom_point(aes(x = input$rUser, y = nrow(re_bootResults())/10),
                  size = 4, color = flatlyPal[2]) +
       labs(x = 'Correlation', y = 'Frequency', 
            title = 'Bootstrapping', 
            caption = paste0(input$ciUser*100, '% CI [', 
                             round(re_ciObs()[[1]], 2), ', ',
                             round(re_ciObs()[[2]], 2), ']')
            ) +
       theme_classic() +
       plotFinish
   })
   
   # Permutation plot ----
   output$permPlot <- renderPlot({
     ggplot(re_tidyd(), aes(estimate)) +
       geom_histogram(binwidth = .05, fill = flatlyPal[1]) +
       geom_vline(aes(xintercept = input$rUser), 
                  color = flatlyPal[6], linetype = 2, size = .75) +
       labs(x = 'Correlation', y = 'Frequency', 
            title = 'Permutation Testing',
            caption = paste0('r = ', input$rUser, ', p = ', round(re_permPVal(), 5))
            ) +
       theme_classic() +
       plotFinish
     
   })
}

# Application ----
shinyApp(ui = ui, server = server)