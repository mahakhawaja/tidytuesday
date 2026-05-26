#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)

energy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-26/energy_cleaned.csv')

wind_countries <- c("Denmark", "Germany", "Ireland", "China", 
                    "United States", "Brazil", "Sweden", "Canada")

wind_data <- energy %>%
    filter(country_name %in% wind_countries) %>%
    filter(!is.na(wind_energy_consumption_tfec_pct)) %>%
    select(country_name, yr, wind_energy_consumption_tfec_pct)

# UI (this is like the look of it all)
ui <- fluidPage(
    tags$head(tags$style(HTML("
    * { margin: 0; padding: 0; box-sizing: border-box; }
    body { background: #0a0f1a; font-family: sans-serif; color: white; }
    .header { padding: 28px 36px 16px; }
    .eyebrow { font-size: 11px; letter-spacing: 0.12em; text-transform: uppercase;
               color: #4fc3f7; font-weight: 600; margin-bottom: 8px; }
    .main-title { font-size: 26px; font-weight: 800; color: #fff; margin-bottom: 4px; }
    .main-title span { color: #4fc3f7; }
    .subtitle { font-size: 12px; color: #667; }
    .body-wrap { display: flex; min-height: 500px; }
    .sidebar { width: 220px; background: #0d1421; padding: 24px 20px;
               border-right: 1px solid #1a2540; flex-shrink: 0; }
    .sidebar-label { font-size: 11px; color: #4fc3f7; text-transform: uppercase;
                     letter-spacing: 0.1em; margin-bottom: 10px; font-weight: 600; }
    .main-panel { flex: 1; padding: 24px 32px; }
    .windmill-row { display: flex; align-items: center; gap: 40px; margin-bottom: 24px; }
    .stat-block { }
    .stat-label { font-size: 11px; color: #667; text-transform: uppercase;
                  letter-spacing: 0.08em; margin-bottom: 4px; }
    .stat-value { font-size: 52px; font-weight: 800; color: #4fc3f7; line-height: 1; }
    .stat-unit { font-size: 13px; color: #556; margin-top: 4px; }
    .stat-desc { font-size: 12px; color: #667; margin-top: 8px;
                 line-height: 1.6; max-width: 300px; }
    .chart-box { background: #0d1421; border-radius: 10px; padding: 20px 24px;
                 border: 1px solid #1a2540; }
    .chart-label { font-size: 11px; color: #4fc3f7; text-transform: uppercase;
                   letter-spacing: 0.08em; margin-bottom: 14px; }
    .caption { font-size: 10px; color: #334; padding: 16px 36px; text-align: right; }
    /* country buttons */
    .country-btn { display: block; width: 100%; text-align: left; padding: 8px 12px;
                   border-radius: 6px; border: none; background: transparent;
                   color: #888; font-size: 13px; cursor: pointer; margin-bottom: 4px;
                   transition: all 0.2s; }
    .country-btn:hover { background: #151e30; color: #ccc; }
    .country-btn.selected { background: #1a2e4a; color: #4fc3f7; font-weight: 600; }
    /* Shiny selectInput override */
    .selectize-input { background: #1a2e4a !important; border: 1px solid #2a3f5a !important;
                       color: #4fc3f7 !important; }
    .selectize-dropdown { background: #0d1421 !important; border: 1px solid #2a3f5a !important; }
    .selectize-dropdown-content .option { color: #aaa !important; }
    .selectize-dropdown-content .option:hover { background: #1a2e4a !important; color: #4fc3f7 !important; }
  "))),
    
    div(class = "header",
        div(class = "eyebrow", "TidyTuesday 2026 · Week 21 · SE4ALL"),
        div(class = "main-title",
            "How fast is the ", tags$span("wind blowing"), "?"
        ),
        div(class = "subtitle",
            "Wind energy consumption as % of total final energy — pick a super swag country to explore"
        )
    ),
    
    div(class = "body-wrap",
        div(class = "sidebar",
            div(class = "sidebar-label", "Select country"),
            selectInput("country", label = NULL,
                        choices = wind_countries,
                        selected = "Denmark",
                        width = "100%")
        ),
        
        div(class = "main-panel",
            # Windmill + stat
            div(class = "windmill-row",
                uiOutput("windmill_svg"),
                div(class = "stat-block",
                    div(class = "stat-label", "Wind share of energy mix"),
                    uiOutput("stat_value"),
                    uiOutput("stat_unit"),
                    uiOutput("stat_desc")
                )
            ),
            # Chart
            div(class = "chart-box",
                div(class = "chart-label", "Wind energy share over time"),
                plotOutput("wind_chart", height = "220px")
            )
        )
    ),
    
    div(class = "caption", "Source: SE4ALL via #TidyTuesday 2026 Week 21 | Maha Khawaja")
)

# Server
server <- function(input, output, session) {
    
    country_data <- reactive({
        wind_data %>%
            filter(country_name == input$country) %>%
            arrange(yr)
    })
    
    latest_val <- reactive({
        country_data() %>%
            filter(yr == max(yr)) %>%
            pull(wind_energy_consumption_tfec_pct) %>%
            round(1)
    })
    
    latest_yr <- reactive({
        country_data() %>%
            pull(yr) %>%
            max()
    })
    
    # Spin speed based on wind value — faster = more wind
    spin_duration <- reactive({
        val <- latest_val()
        # scale: 0% = 6s (slow), 30%+ = 0.8s (fast)
        duration <- max(0.8, 6 - (val / 30) * 5.2)
        round(duration, 2)
    })
    
    output$windmill_svg <- renderUI({
        dur <- spin_duration()
        HTML(paste0('
      <svg width="160" height="200" viewBox="0 0 180 200">
        <rect x="84" y="110" width="12" height="80" fill="#1a2e4a" rx="3"/>
        <rect x="70" y="185" width="40" height="8" fill="#1a2e4a" rx="2"/>
        <circle cx="90" cy="108" r="7" fill="#2a3f5a"/>
        <circle cx="90" cy="108" r="3" fill="#4fc3f7"/>
        <g style="transform-origin: 90px 108px;
                  animation: spin ', dur, 's linear infinite;">
          <rect x="87" y="48" width="6" height="58" rx="3" fill="#4fc3f7" opacity="0.9"/>
          <rect x="87" y="48" width="6" height="58" rx="3" fill="#4fc3f7" opacity="0.9"
                style="transform-origin:90px 108px; transform: rotate(120deg)"/>
          <rect x="87" y="48" width="6" height="58" rx="3" fill="#4fc3f7" opacity="0.9"
                style="transform-origin:90px 108px; transform: rotate(240deg)"/>
        </g>
        <style>
          @keyframes spin { from { transform: rotate(0deg); } to { transform: rotate(360deg); } }
        </style>
      </svg>
    '))
    })
    
    output$stat_value <- renderUI({
        div(class = "stat-value", paste0(latest_val(), "%"))
    })
    
    output$stat_unit <- renderUI({
        div(class = "stat-unit", paste0("in ", latest_yr(), " · ", input$country))
    })
    
    output$stat_desc <- renderUI({
        val <- latest_val()
        desc <- case_when(
            val >= 20 ~ paste0(input$country, " is a wind energy powerhouse — over 1 in 5 units of energy comes from wind. So much swag, clearly."),
            val >= 10 ~ paste0(input$country, " has made serious investments in wind, with a meaningful share of its energy mix."),
            val >= 3  ~ paste0(input$country, " is growing its wind capacity but still has significant room to expand. A plethora of room to expand, perchance."),
            TRUE      ~ paste0(input$country, " currently relies very little on wind energy in its total energy mix. Not very sexy.")
        )
        div(class = "stat-desc", desc)
    })
    
    output$wind_chart <- renderPlot({
        df <- country_data()
        max_val <- max(df$wind_energy_consumption_tfec_pct, na.rm = TRUE)
        
        ggplot(df, aes(x = yr, y = wind_energy_consumption_tfec_pct)) +
            geom_area(fill = "#4fc3f7", alpha = 0.2) +
            geom_line(color = "#4fc3f7", linewidth = 1.2) +
            geom_point(data = df %>% filter(yr == max(yr)),
                       color = "#4fc3f7", size = 3) +
            scale_y_continuous(
                limits = c(0, max(max_val * 1.2, 1)),
                labels = function(x) paste0(x, "%")
            ) +
            scale_x_continuous(breaks = seq(1990, 2025, by = 5)) +
            labs(x = NULL, y = NULL) +
            theme_void() +
            theme(
                plot.background  = element_rect(fill = "#0d1421", color = NA),
                panel.background = element_rect(fill = "#0d1421", color = NA),
                axis.text.x = element_text(color = "#556", size = 10),
                axis.text.y = element_text(color = "#556", size = 10),
                panel.grid.major.y = element_line(color = "#1a2540", linewidth = 0.4),
                plot.margin = margin(8, 8, 8, 8)
            )
    }, bg = "#0d1421")
}

# Combine it all
shinyApp(ui, server)
