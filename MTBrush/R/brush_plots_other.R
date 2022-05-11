#' generate a shiny app for general data type
#'
#'This is the method to generate a shiny app displaying the histograms of statistics and scatter plots and a table
#'of selected observations.
#'
#' @param df the processed data frame
#' @param stats_df the output table from fit_statistics function
#' @param group_list a list of distinct observations
#' @param group the column name for grouping variable
#' @param value the column name for response variable
#'
#' @return a shiny app
#' @export
#' @import dplyr
#' @import ggplot2
#' @import shiny
#' @importFrom magrittr %>%

brush_plots_other <- function(df, stats_df, group_list, group, value) {
  stats_df_param <- stats_df %>%
    filter(term != "(Intercept)")
  past_candidates <- c("-1")
  set_theme()

  shinyApp(
    ui = fluidPage(
      column(
        plotOutput("distPlot", brush = brushOpts(direction = "x", id = "brush")),
        width = 6
      ),

      column(
        plotOutput("plot_g2"),
        width = 6
      ),

      dataTableOutput("table"),
      selectInput("group", "ID", choices = group_list, multiple = TRUE),
      selectInput("feature", "Features:", choices = names(df)[2:(ncol(df))], multiple = FALSE)
    ),

    server = function(input, output, session) {

      brushed_ids <- reactive({
        message("brush ids")
        current_points <- brushedPoints(stats_df_param, input$brush)
        current_points %>%
          select(term, .data[[group]])
      })


      table_data <- reactive({
        message("table data")
        current_rows <- stats_df_param %>%
          mutate(statistic = round(statistic, 5)) %>%
          select(.data[[group]], term, statistic) %>%
          filter(.data[[group]] %in% brushed_ids()[[group]]) %>%
          pivot_wider(names_from = term, values_from = statistic)

        if(nrow(current_rows) > 0) {
          current_rows$magnitude <- abs(current_rows[, brushed_ids()$term[1]])
          max <- max(current_rows$magnitude)
          min <- min(current_rows$magnitude)
          stat_term <- brushed_ids()$term[1]


          current_rows <- stats_df_param %>%
            mutate(statistic = round(statistic, 5)) %>%
            select(.data[[group]], term, statistic) %>%
            pivot_wider(names_from = term, values_from = statistic)
          current_rows$magnitude <- abs(current_rows[, brushed_ids()$term[1]])

          current_rows <- current_rows %>%
            filter(magnitude <= max & magnitude >= min)

          current_rows <- current_rows %>%
            arrange(-magnitude) %>%
            arrange_at(vars(-cluster:-magnitude))
          current_rows <- current_rows %>%
            mutate(color = sign(current_rows[, brushed_ids()$term[1]])) %>%
            select(-magnitude)
        }
      })

      output$distPlot <- renderPlot({
        if(is.null(table_data())){
          draw_stats_histogram(stats_df)
        }
        else{

          current_data_po <- stats_df_param %>%
            filter(.data[[group]] %in% (table_data() %>%
                                          filter(color == 1))[[group]])

          current_data_ne <- stats_df_param %>%
            filter(.data[[group]] %in% (table_data() %>%
                                          filter(color == -1))[[group]])

          ggplot(stats_df_param, aes(x = statistic)) +
            geom_histogram(bins = 100) +
            geom_histogram(data = current_data_po, fill = "red", bins = 100) +
            geom_histogram(data = current_data_ne, fill = "orange", bins = 100) +
            scale_y_continuous(expand = c(0, 0, .1, .1)) +
            labs(x = "ANOVA test statistic") +
            facet_wrap(~ term, scales = "free") +
            theme(strip.text = element_text(size = 14))
        }
      })

      output$table <- renderDataTable({
        message("data table")
        datatable(
          table_data(),
          rownames = FALSE,
          filter="top",
          options = list(sDom = '<"top">lrt<"bottom">ip', columnDefs = list(list(visible=FALSE, targets=8)))
        ) %>% formatStyle('color', target = 'row', backgroundColor = styleEqual(c(-1,1), c('#ffc14e', '#ff7676')))
      })

      output$plot_g2 <- renderPlot({
        message("plot g2")
        cur_groups <- unique(input[[group]])
        if (length(cur_groups) != 0) {
          x_small <- df %>%
            filter(.data[[group]] %in%  cur_groups) %>%
            mutate(group = factor(.data[[group]], levels = cur_groups))
        } else {
          x_small <- df[sample(1:nrow(df), 1000), ]
        }

        p <- ggplot(x_small) +
          geom_point(aes_string(input$feature, value)) +
          labs(y =  "Intensity Values") +

          if (length(cur_groups) != 0) {
            p <- p + facet_wrap(~ group, scales = "free_y")
          }
        p
      })

      observe({
        message("observe")
        if(is.null(table_data())){
          candidates = NULL
        } else{
          candidates <- table_data()[[group]]
        }


        if (!all(candidates == past_candidates)) {
          selected_ids <- candidates[1:12]
          past_candidates <- candidates
        } else {
          selected_ids <- input[[group]]
        }

        updateSelectInput(session, group, choices = candidates, selected = selected_ids)
      })
    },
  )
}
