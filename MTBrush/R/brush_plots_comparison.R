#' Function to set a minimal theme
#' 
#' @import ggplot2
set_theme <- function() {
  minimal_theme <- theme_minimal() + 
    theme(
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#f7f7f7"),
      panel.border = element_rect(fill = NA, color = "#0c0c0c", size = 0.6),
      axis.text = element_text(size = 11),
      strip.text = element_text(size = 14),
      axis.title = element_text(size = 16),
      legend.position = "bottom"
    )
  theme_set(minimal_theme)
}

#' Reshape output from tests to enable comparison
#' 
#' @import tidyr
#' @import dplyr
#' @import magrittr
#' @export
reshape_comparisons <- function(comparison_df, group, cur_ids, axes_vars, 
                                cur_var, effect_names) {
  comparisons_df <- comparison_df %>%
    filter(.data[[group]] %in% cur_ids) %>%
    pivot_longer(all_of(axes_vars)) %>%
    unite("comparison", comparison:name, sep = "-") %>%
    select(-comparison_id) %>%
    pivot_wider(names_from = "comparison", values_from = "value") %>%
    mutate(across(-.data[[group]], round, 3)) %>%
    arrange(-across(starts_with(cur_var)))
  
  if (ncol(comparisons_df) > 1) {
    comparisons_df <- set_names(comparisons_df, c(group, effect_names))
    
  }

  comparisons_df
}

#' Faceted scatterplot of compared statistics
#' 
#' @import ggplot2
#' @import dplyr
#' @import magrittr
#' @export
comparison_plot <- function(comparison_df, group, axes_vars, cur_ids) {
  selected_subset <- comparison_df %>%
    filter(.data[[group]] %in% cur_ids)
  
  ggplot(comparison_df, aes_string(axes_vars[1], axes_vars[2])) +
    geom_vline(xintercept = 0, col = "#676767", size = 0.5) +
    geom_hline(yintercept = 0, col = "#676767", size = 0.5) +
    geom_abline(slope = 1, intercept = 0, col = "#676767") +
    geom_point(size = 0.5, alpha = 0.8) +
    geom_point(data = selected_subset, col = "red", size = 1.5) +
    facet_wrap(~ comparison, scales = "free", ncol = 2)
}

#' Interactive scatterplot to compare statistics
#' @import dplyr
#' @import shiny
#' @import magrittr
#' @importFrom DT DTOutput renderDT datatable
#' @export
brush_plots_comparison <- function(df, comparison_df, group_list, group, 
                                   axes_vars, value, effect_names) {
  set_theme()
  past_candidates <- c("-1")

  shinyApp(
    ui = fluidPage(
      column(plotOutput("compPlot", brush = brushOpts(id = "brush")), width = 6),
      column(plotOutput("plot_g2"), width = 6), 
      DTOutput("table"),
      selectInput("group", "ID", choices = group_list, multiple = TRUE),
    ),
    
    server = function(input, output, session) {
      brushed_ids <- reactive({
        current_points <- brushedPoints(comparison_df, input$brush)
        current_points %>%
          select(comparison, .data[[group]])
      })
      
      table_data <- reactive({
        cur_var <- as.character(brushed_ids()$comparison[1])
        cur_ids <- brushed_ids()[[group]]
        reshape_comparisons(
          comparison_df, 
          group, 
          cur_ids, 
          axes_vars, 
          cur_var, 
          effect_names
        )
      })
      
      
      output$compPlot <- renderPlot(
        comparison_plot(comparison_df, group, axes_vars, brushed_ids()[[group]])
      )
      
      output$plot_g2 <- renderPlot({
        cur_groups <- unique(input$group)
        if (length(cur_groups) != 0) {
          x_small <- df %>%
            filter(.data[[group]] %in%  cur_groups) %>%
            mutate(group = factor(.data[[group]], levels = cur_groups))
        } else {
          x_small <- df[sample(1:nrow(df), 1000), ]
        }
        
        p <- ggplot(x_small) +
          geom_point(aes(condition, value)) + 
          scale_y_log10(breaks = 10 ^ (5:9)) +
          labs(y = "Intensity Values") +
          theme(axis.text.x = element_text(angle = 90))
        
        if (length(cur_groups) != 0) {
          p <- p + facet_wrap(~ group, scales = "free_y")
        }
        p
      })
      
      output$table <- renderDT({
        if(is.null(brushed_ids())) {
          return()
        }

        datatable(
          table_data(),
          rownames = FALSE,
          filter="top",
          options = list(sDom = '<"top">lrt<"bottom">ip', columnDefs = list(list(visible=FALSE, targets=8)))
        )
      })
      
      observeEvent(input$brush, {
        candidates <- table_data()[[group]]
        if (!all(candidates == past_candidates)) {
          selected_ids <- head(candidates, 12)
          past_candidates <- candidates
        } else {
          selected_ids <- input$group
        }
        
        updateSelectInput(session, "group", choices = candidates, selected = selected_ids)
      })
    }
  )
}