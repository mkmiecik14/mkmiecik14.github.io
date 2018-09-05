# Packages required ----
library(shiny)
library(dplyr)
library(ggplot2)
library(tibble)
library(purrr)
library(broom)
library(MASS)
library(modelr)
library(shinythemes)
library(shinycssloaders)
library(shinydashboard)
library(readr)

# Global aesthetics ----
swidth <- 375 # Width of the sidebar

# Main color palette (Flatly bootstrap theme)
flatlyPal <- c("#2C3E50", "#18BC9C", "#94A5A6", "#3498DC", "#F39C13", "#E74C3C")

# loading spinner options
options(spinner.color = flatlyPal[1], spinner.type = 8)

# Turning off scientific notation for p-values
options(scipen = 999)

# UI ----

# dashboard ----
dashboardPage(
  skin = "blue",

  # header ----
  dashboardHeader(title = "Bootstrapping and Permutation Testing", titleWidth = swidth),

  # sidebar ----
  dashboardSidebar(
    width = swidth,
    sidebarMenu(
      menuItem("App", icon = icon("database"), tabName = "shinyapp"),
      menuItem("Description", icon = icon("book"), tabName = "description"),
      menuItem("Code", icon = icon("terminal"), tabName = "code"),
      menuItem("References", icon = icon("university"), tabName = "references"),
      menuItem("Authors",
        icon = icon("flask"), startExpanded = F, selected = F,
        menuItem("Matthew J. Kmiecik",
          startExpanded = T, selected = F,
          menuSubItem("Website", icon = icon("link"), href = "https://mattkmiecik.com/"),
          menuSubItem("GitHub", icon = icon("github"), href = "https://github.com/mkmiecik14")
        ),
        menuItem("Ekarin E. Pongpipat",
          startExpanded = T, selected = F,
          menuSubItem("Website", icon = icon("link"), href = "https://ekarinpongpipat.com"),
          menuSubItem("GitHub", icon = icon("github"), href = "https://github.com/epongpipat")
        )
      ),
      menuItem("MIT License 2018", icon = icon("copyright"), href = "https://raw.githubusercontent.com/mkmiecik14/mkmiecik14.github.io/app-update/boot-perm-dash/LICENSE")
    )
  ),

  # body ----
  dashboardBody(
    tabItems(
      # shiny app ----
      tabItem(
        tabName = "shinyapp",
        fluidRow(
          box(
            title = "User Input", solidHeader = T, width = 6, status = "primary",
            sliderInput(
              inputId = "rUser",
              label = "Correlation (r): ",
              min = -1,
              max = 1,
              value = .3,
              step = .1,
              ticks = FALSE
            ),
            sliderInput(
              inputId = "sampleN",
              label = "Sample size (N): ",
              min = 10,
              max = 200,
              value = 50,
              step = 10,
              ticks = FALSE
            ),
            sliderInput(
              inputId = "bootIters",
              label = "Bootstrap Iterations: ",
              min = 100,
              max = 2000,
              value = 100,
              step = 100,
              ticks = FALSE
            ),
            sliderInput(
              inputId = "ciUser",
              label = "Bootstrap Confidence Interval: ",
              min = .80,
              max = .99,
              value = .95,
              step = .01,
              ticks = FALSE
            ),
            sliderInput(
              inputId = "permIters",
              label = "Permutation Iterations: ",
              min = 100,
              max = 2000,
              value = 100,
              step = 100,
              ticks = FALSE
            )
          ),
          box(
            title = "Scatterplot", solidHeader = T, width = 6, status = "primary",
            plotOutput("scatterPlot") %>% withSpinner()
          )
        ),
        fluidRow(
          box(
            title = "Bootstrapped Correlations", solidHeader = T, width = 6, status = "primary",
            plotOutput("bootPlot") %>% withSpinner()
          ),
          box(
            title = "Permuted Correlations", solidHeader = T, width = 6, status = "primary",
            plotOutput("permPlot") %>% withSpinner()
          )
        )
      ),
      # description ----
      tabItem(
        tabName = "description",
        fluidRow(
          box(
            title = "Purpose", width = 12, status = "primary", solidHeader = T,
            "This post serves as a brief overview of the difference between bootstrapping and permutation testing with a shiny app to visualize their differences. The example statistic we use here is a correlation; however, these techniques can be extended to a variety of statistics (e.g., t-test, ANOVA, PCA)."
          )
        ),
        fluidRow(
          box(
            title = "Bootstrapping", width = 12, status = "primary", solidHeader = T,
            p(
              "When you think of boostrapping, think confidence intervals. Bootstrapping",
              tags$b("samples observations with replacement"),
              "without breaking the relationship between measures (e.g., X and Y).",
              "The number of samples is equal to the number of observations (i.e., sample size).",
              "After sampling with replacement is finished, the statistic of interest,",
              "such as a correlation, is computed and stored.",
              tags$blockquote("When you think of boostrapping, think confidence intervals."),
              "This process explained above is then repeated hundreds or thousands of iterations",
              "resulting in a distribution of values for your statistic of interest.",
              "This distibution will be centered about the original statistical value that you computed before any resampling occured.",
              "In other words, the mean of these stored values will equal your observed statistic."
            ),
            p(
              "As with any distribution, you can calculate what are the lower bounds and upper bounds of values for a given percentage.",
              "This percentage is determined by the researcher/statistician/hockey analyst enthusiast and is called a confidence interval",
              "(95% is a common confidence interval). These bounds are usually reported in square brackets in the format: confidence interval",
              "% [lowerbound, upper bound]. For example, 'There was a positive correlation observed between X and Y,", tags$em("r"), " = .31, 95% CI [.21, .41].'"
            )
          )
        ),
        fluidRow(
          box(
            title = "Permutation Testing", width = 12, status = "primary", solidHeader = T,
            p(
              "When you think of permutation testing, think of p-values.",
              "Permutation testing", tags$b("does not sample observations with replacement, but instead breaks the relationship between measures (e.g., X and Y)."),
              "This is done by shuffling/randomizing/sampling the observed data points for one variable, while keeping the other (or others) intact.",
              "In terms of correlation, this would mean that X would be shuffled within the sample, while Y remained the original values.",
              "After the responses for one variable are randomized the statistic of interest, such as a correlation, is computed and stored.",
              tags$blockquote("When you think of permutation testing, think of p-values"),
              "This process explained above is then repeated hundreds or thousands of iterations resulting in a distribution of values for your statistic of interest.",
              "This distibution", tags$b("will not be centered about the original statistic value that you computed before any shuffling occured, but rather will be centered around the null."),
              "In terms of correlation, a null distribution would center about", tags$em("r"),
              " = 0; meaning no linear relationship between variables."
            ),
            p("In other words, a null distribution is created by shuffling the values in X but not Y. This is because the relationship has been broken between X and Y."),
            p(
              "A p-value is calculated by first counting the number of statistical values that are more extreme than your observed statistic.",
              "Put another way, how many times did the statistical value that emerged from a \"null distribution\" surpass your original computed statistic (before any shuffling).",
              "Then, you take the number of times that the null distribution is more extreme than your original value and divide it by the number of permutation iterations (number of observations in your null distribution)."
            ),
            p(
              "For example, let's say I ran a permutation test on a correlation of",
              tags$em("r"), " = .5 and shuffled X, kept Y, computed their correlation, stored this value, and repeated this 100 times. Out of 100 times, there were 4 correlations that emerged that were greater than .5.",
              "Therefore, my p-value for this correlation would be 4/100 =", 4 / 100
            )
          )
        )
      ),

      # code ----
      tabItem(
        tabName = "code",
        fluidRow(
          tabBox(
            title = "Code", id = "code", width = 12,
            tabPanel("ui.R", verbatimTextOutput("ui_file")),
            tabPanel("server.R", verbatimTextOutput("server_file"))
          )
        )
      ),

      # references ----
      tabItem(
        tabName = "references",
        fluidRow(
          box(
            title = "References", width = 12, status = "primary", solidHeader = T,
            p("Readers who would like more theoretical or technical explanations of boostrapping and permutation testing are encouraged to read:"),
            HTML("<p> 1. Efron, B. (1979). Bootstrap methods: Another look at the jacknife. <i>The Annals of Statistics, 7</i>(1), 1-26. </p>"),
            HTML("<p> 2. Ludbrook, J., & Dudley, H. (1998). Why Permutation Tests are Superior to t and F Tests in Biomedical Research. <i>The American Statistician, 52</i>(2), 127-132 <a href='https://doi.org/10.1080/00031305.1998.10480551' target='_blank'>doi:10.1080/00031305.1998.10480551</a></p>"),
            HTML("<p> 3. Wright, D. B., London, K., & Field, A. P. (2018). Using Bootstrap Estimation and the Plug-in Principle for Clinical Psychology Data. <i>Journal of Experimental Psychopathology, 2</i>(2). <a href='https://doi.org/10.5127/jep.013611' target='_blank'>doi:10.5127/jep.013611</a></p>")
          )
        )
      )
    )
  )
)
