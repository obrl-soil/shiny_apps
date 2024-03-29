# Shiny app to replicate Splinetool.exe
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(mpspline2)
library(shiny)
library(DT)
options(stringsAsFactors = FALSE)

# for plotting
sfm <-
  ggplot2::scale_fill_manual(name   = 'Input data',
                             guide  = 'legend',
                             labels = '',
                             values = c('grey50' = 'grey50'))
scm <-
  ggplot2::scale_colour_manual(name   = 'Output data',
                               labels = c('Depth layers', '1cm increments'),
                               guide  = 'legend',
                               values = c('darkblue' = 'darkblue',
                                          'red'      = 'red'))
scm2 <-
  ggplot2::scale_colour_manual(name   = 'Output data',
                               labels = c('1cm increments', 'Depth layers'),
                               guide  = 'legend',
                               values = c('darkblue' = 'darkblue',
                                          'red'      = 'red'))

ui <- fluidPage(
  tags$head(includeHTML("ga.html")), # analytics
  titlePanel("SplineApp"),
  tags$hr(),

  fluidRow(
    column(width = 10,
           fileInput(inputId     = 'file_1',
                     label       = 'Input CSV',
                     multiple    = FALSE,
                     accept      = c('text/csv',
                                     'text/comma-separated-values,text/plain',
                                     '.csv'),
                     width       = '100%',
                     buttonLabel = 'Select CSV')),
    column(width = 2,
           actionButton(inputId = "ld",
                        label   = "Upload CSV",
                        width   = '100%'),
           style = 'margin-top: 25px; text-align: center;')),

  fluidRow(

    # Sidebar Panel
    column(width = 4,
           wellPanel(
             actionButton(inputId = 'splinetime', label = 'Calculate Splines', width = '100%',
                          style = 'font-align:center; font-weight: bold;'),
             tags$br()),
           wellPanel(
             selectInput('SID', 'Site IDs: ', choices = list(),
                         size = 5, selectize = FALSE, multiple = FALSE),
             conditionalPanel('input.SID',
                              tags$hr(),
                              tags$b("Input Data"),
                              DT::DTOutput('table_1'))
           )
           ),

    # Main Panel
    column(width = 8,
           tabsetPanel(id = 'maintabs',
             ## About tab
             tabPanel(title = tags$h3('About'), value = 'Ipanel', tags$br(),
                      tags$p(class = 'lead', 'This is a web-based version of the ACLEP Spline Tool (Jacquier and Seaton, 2011), available from ', tags$a(href = 'https://www.asris.csiro.au/methods.html', 'the ASRIS methods page.'), ' The UI takes its cues from the standalone app, but depends on the ', tags$a(href = 'https://github.com/obrl-soil/mpspline2', tags$code('mpspline2')), ' R package (v. 0.1.7).'), tags$hr(),
                      tags$h4('Instructions for use'),
                      tags$p('The app expects source data in the form of a four-column csv with the following structure:'),
tags$pre('> str(indat)
data.frame:	9 obs. of  4 variables:
$ SID  : int  1 1 1 1 2 2 2 2 2
$ UD   : int  0 20 50 80 0 25 65 90 120
$ LD   : int  10 30 60 90 10 35 80 100 135
$ VALUE: num  7.6 7.5 6.5 3.2 7.5 7.6 6.3 3.4 4.3'),
                      tags$p('The first column is a site identifier and can be numeric or text. The next two columns are upper and lower sample depths respectively, measured in centimeters below ground level. The final column contains the attribute values to be splined.'),
                      tags$p('Upload your csv and press the \'Calculate Splines\' button. Outputs can be customised in the Settings tab, and viewed in the Results tab. The Export tab will allow download of outputs in csv format, or as an rds file containing the R object returned by', tags$code('mpspline2::mpspline_tidy()'), '.'), tags$hr(),
tags$h4('References'),
tags$p("Bishop, T. F. A., McBratney, A. B., and Laslett, G. M. 1999 ‘Modelling Soil Attribute Depth Functions with Equal-Area Quadratic Smoothing Splines’. ", tags$em('Geoderma'), " 91, 1:27–45. ", tags$a(href = 'https://doi.org/10.1016/S0016-7061(99)00003-8', '10.1016/S0016-7061(99)00003-8')),
tags$p("Malone, B. P., McBratney, A. B., Minasny, B. and Laslett, G. M. 2009 ‘Mapping Continuous Depth Functions of Soil Carbon Storage and Available Water Capacity’. ", tags$em('Geoderma'), " 154, 1:138–52.", tags$a(href = 'https://doi.org/10.1016/j.geoderma.2009.10.007', '10.1016/j.geoderma.2009.10.007')),
tags$hr(),
tags$h4('Update History'),
tags$ul(tags$li('2022-04-17: Updated to use mpspline2, refreshed appearance.'),
        tags$li('2017-10-23: Added plot scaling options, handled bug in lowest 1cm depth data'),
        tags$li('2017-10-22: App launch'))),

             # Settings tab
             tabPanel(title = tags$h3('Settings'), value = 'Spanel',
               fluidRow(tags$br(),
                  column(width = 6,
                         wellPanel(tags$h4('Output depth ranges'),
                                   tags$p("Standard output depths are 0-5cm, 5-15cm, 15-30cm, 30-60cm, 60-100cm, and 100-200cm. To alter these, select 'Custom...' and add at least one value (0 is added automatically), then click 'Calculate Splines'.",
                                     style = 'color:#A9A9A9; font-weight: bold;'),
                   radioButtons(inputId = 'depth_choice',
                                label   = NULL,
                                choices = list('Standard Depths' = 'sd',
                                              'Custom...' = 'ns'),
                                width   = '100%'),
                   # choosing custom output depth ranges:
                   conditionalPanel("input.depth_choice == 'ns'",
                                   numericInput(inputId = 'cd',
                                             label = 'Custom Depths',
                                             value = '',
                                             width = '100%',
                                             min = 0, max = 1000, step = 1),
                                   tags$i(textOutput(outputId = 'cd_vals')),
                                   tags$br(),
                                   actionButton(inputId = 'cd_add',
                                                label = 'Add',
                                                icon = icon('plus'),
                                                width = '100%'),
                                   tags$br(),tags$br(),
                                   actionButton(inputId = 'cd_reset',
                                                label =  'Reset',
                                                icon = icon('undo'),
                                                width = '100%')
               ) # end conpanel
               ), # end well Panel 1
               wellPanel(
                 tags$h4('Output Rounding'),
                 tags$p('Control rounding of output values. Default is 3 decimal places.',
                        style = 'color:#A9A9A9; font-weight: bold;'),
                 numericInput(inputId = 'rnd',
                              label = 'Rounding',
                              value = 3,
                              min = -10, max = 10,
                              width = '50%')
                 ) # end well panel 2
               ), # end column
               column(width = 6,
                      wellPanel(tags$h4('Lambda'),
                                tags$p('Control the degree of smoothing. Default is 0.1.',
                                       style = 'color:#A9A9A9; font-weight: bold;'),
                                numericInput(inputId = 'c_ld',
                                             label = 'Lambda',
                                             value = 0.1,
                                             width = '50%')), # end lambdapanel
                      wellPanel(tags$h4('Output Limits'),
                               tags$p('Set limits on spline output values. Defaults are 0, 1000.',
                                       style = 'color:#A9A9A9; font-weight: bold;'),
                                numericInput(inputId = 'low_lim',
                                             label   = 'Lower Limit',
                                             value = 0,
                                             width = '50%'),
                                numericInput(inputId = 'high_lim',
                                             label = 'Upper Limit',
                                             value = 1000,
                                             width = '50%')) # end lims wellPanel
                      ) #end col 2
               ) # end fluidRow
               ), # end Settings tab

             # Results tab
             tabPanel(title = tags$h3('Results'), value = 'Ppanel',
              fluidRow(conditionalPanel('input.SID && input.splinetime',
               column(width = 10, offset = 1, tags$br(), plotOutput('splineplot')),
              fluidRow(
                column(width = 6, offset = 1, tags$br(),
                       checkboxGroupInput(inputId = 'pick_geoms',
                                          label = NULL,
                                          choices = list('Original'       = 1,
                                                         'Depth layers'   = 2,
                                                         '1cm increments' = 3),
                                          selected = c(1,2,3),
                                          inline = TRUE,
                                          width = '100%')),
                column(width = 4,
                       downloadButton(outputId = 'dl_plot',
                                                label = 'Save plot as png',
                                                class = 'dlb-sml'),
                       tags$head(tags$style(".dlb-sml{margin-top: 25px; width: 80%;}"))
                       )),
              fluidRow(column(width = 3, offset = 1,
                              selectInput(inputId = 'plot_scale',
                                          label   = 'Plot Scale',
                                          choices = list('Scale to site'          = 1,
                                                         'Scale to quantile 0.75' = 2,
                                                         'Scale to quantile 0.95' = 3,
                                                         'Scale to whole dataset' = 4),
                                          selected = 1,
                                          multiple = FALSE,
                                          width = '80%')))), tags$br()
              ), # end plot area
              fluidRow(
                conditionalPanel('input.SID && input.splinetime', tags$hr(),
                                 tags$h3('Splined output values'),
                column(width = 6, tags$h4('Depth ranges'),  DT::DTOutput('splinetable_sd')),
                column(width = 6, tags$h4('1cm intervals'), DT::DTOutput('splinetable_cm'))
                )
              ), conditionalPanel('input.SID && input.splinetime', tags$br(), tags$hr())),

               #Exports tab
               tabPanel(title = tags$h3('Export'), value = 'Epanel',
                 fluidRow(tags$br(),
                   column(width = 6, offset = 3,
                   wellPanel(downloadButton(outputId = 'dl_sd',
                                            label = 'Resampled depth intervals as csv',
                                            class = 'dlb'),
                           tags$br(), tags$br(),
                           downloadButton(outputId = 'dl_1cm',
                                          label = '1cm depth intervals as csv',
                                          class = 'dlb'),
                           tags$br(), tags$br(),
                           downloadButton(outputId = 'dl_rds',
                                          label = 'Full output of mpspline2::mpspline_tidy() as rds',
                                          class = 'dlb'),
                           tags$head(tags$style(".dlb{width: 100%;}"))
                           ) # end export wellpanel
                   ))) # end export tabpanel
             )) #end main
    )
)

####################################################################################################

server <- function(input, output, session) {

  ### SIDEBAR
  # receive data
  to_be_splined <- eventReactive(input$ld, {
    in_file <- input$file_1
    req(in_file)
    readr::read_csv(in_file$datapath)
    })

  # Feed SID choices from input df to drop-down box
  observe({
    sids <- levels(factor(to_be_splined()[[1]]))
    updateSelectInput(session,
                      inputId  = 'SID',
                      choices  = sids,
                      selected = sids[1])
  })

  # display loaded data
  output$table_1 <- DT::renderDT({
    req(input$SID)
    to_be_splined()[to_be_splined()[, 1] == input$SID, ]
    },
    rownames = FALSE,
    options = list(lengthChange = FALSE, pageLength = 10,
                   scrollX = FALSE, scrollY = '300px',
                   paging = FALSE, searching = FALSE, info = FALSE)
  )

### MAIN

# Deal with default and custom depth ranges
# expose mpspline inputs to user with defaults set (settings panel)
out_ranges <- reactiveValues('default' = c(0, 5, 15, 30, 60, 100, 200))

## get custom depth choices
observe({
  if(input$cd_add > 0) {
    out_ranges$custom <- c(isolate(out_ranges$custom), isolate(input$cd))
    if(!(0 %in% isolate(out_ranges$custom))) {
      out_ranges$custom <- c(0, isolate(out_ranges$custom))
      }
    out_ranges$custom <- unique(isolate(out_ranges$custom))
    updateNumericInput(session, inputId = "cd", value = '')
    }
  })

output$cd_vals <- renderText({
  req(out_ranges$custom)
   paste0('Custom depths chosen: ', toString(sort(out_ranges$custom)), ' cm')
  })

# handle depth-range reset
observe({
  if(input$cd_reset > 0) {
    out_ranges$custom <- NULL
    updateRadioButtons(session, 'depth_choice', selected = 'sd')
    }
  })

# get custom lambda
lambda <- reactiveValues('val' = 0.1)

observe({ lambda$val <- input$c_ld })

# get custom min/max depths
out_lims <- reactiveValues(minval = 0, maxval = 1000)
observe({
  out_lims$minval <- input$low_lim
  out_lims$maxval <- input$high_lim
  })

## spline data - output = list of four
splined <- eventReactive(input$splinetime, {

  # get depths
  use_depths <- if(input$depth_choice == 'ns') {
    out_ranges$custom
    } else {
      out_ranges$default
    }

  # get lambda
  use_lambda <- lambda$val

  # get outlims
  use_minval <- out_lims$minval
  use_maxval <- out_lims$maxval

  mpspline2::mpspline_tidy(
    obj      = to_be_splined(),
    var_name = names(to_be_splined()[4]),
    lam      = use_lambda,
    d        = use_depths,
    vlow     = use_minval,
    vhigh    = use_maxval
      )

  })

# when 'Process Spline' button is clicked, jump to plot/output tab
observeEvent(input$splinetime, {
  updateTabsetPanel(session, "maintabs", selected = "Ppanel")
  })

## Process outputs for graphing and csv export
# expose desired output rounding val to user (settings panel)
rnd_val <- reactiveValues(default = 3)
observe({ rnd_val$default <- input$rnd })

## Process standard depths
sd_out <- reactive({
   splined()$est_dcm %>%
     dplyr::mutate(SPLINED_VALUE = round(SPLINED_VALUE, rnd_val$default))
 })

## Process 1cm depths
cm_out <- reactive({
  splined()$est_1cm %>%
    dplyr::mutate(SPLINED_VALUE = round(SPLINED_VALUE, rnd_val$default))
})

## Plot data

# subset for by-site plot
plot_tbs <- reactive({
  to_be_splined()[to_be_splined()[[1]] == input$SID, ]
  })

plot_sd_out <- reactive({
  so <- sd_out()[sd_out()[[1]] == input$SID, ]
  so <- tidyr::pivot_longer(data      = so,
                            cols      = c('UD', 'LD'),
                            names_to  = 'key',
                            values_to = 'DEPTH')
  so <- dplyr::select(so, everything(), SPLINED_VALUE)
  dplyr::filter(so, !(is.na(SPLINED_VALUE)))
  })

# helps prevent plotting where splining failed
can_plot <- reactive({ nrow(plot_sd_out()) > 0 })

plot_cm_out <- reactive({
  co <- cm_out()[cm_out()[[1]] == input$SID, ]
  co <- tidyr::pivot_longer(data      = co,
                             cols      = c(UD, LD),
                             names_to  = 'key',
                             values_to = 'DEPTH')
  co <- dplyr::select(co, everything(), SPLINED_VALUE)
  co <- dplyr::filter(co, !(is.na(SPLINED_VALUE)))
  dplyr::filter(co, key != 'UD')
  })

# get input XY lims to keep graph consistently sized across sites
# max values have Options
sp_xmin <- reactive({
  floor(min(min(to_be_splined()[[4]],   na.rm = TRUE),
            min(sd_out()$SPLINED_VALUE, na.rm = TRUE),
            min(cm_out()$SPLINED_VALUE, na.rm = TRUE)))
  })

sp_xmax <- reactive({
  if(input$plot_scale == 1) {
    ceiling(max(max(plot_tbs()[[4]],   na.rm = TRUE),
                max(plot_sd_out()$SPLINED_VALUE, na.rm = TRUE),
                max(plot_cm_out()$SPLINED_VALUE, na.rm = TRUE)))
    } else if(input$plot_scale == 2) {
      ceiling(max(quantile(to_be_splined()[[4]],
                           probs = 0.75, na.rm = TRUE, names = FALSE),
                  quantile(sd_out()$SPLINED_VALUE, probs = 0.75, na.rm = TRUE,
                           names = FALSE),
                  quantile(cm_out()$SPLINED_VALUE, probs = 0.75, na.rm = TRUE,
                           names = FALSE)))
      } else if(input$plot_scale == 3) {
        ceiling(max(quantile(to_be_splined()[[4]],
                             probs = 0.95, na.rm = TRUE, names = FALSE),
                    quantile(sd_out()$SPLINED_VALUE, probs = 0.95, na.rm = TRUE,
                             names = FALSE),
                    quantile(cm_out()$SPLINED_VALUE, probs = 0.95, na.rm = TRUE,
                             names = FALSE)))
        } else {
          ceiling(max(max(to_be_splined()[[4]],   na.rm = TRUE),
                      max(sd_out()$SPLINED_VALUE, na.rm = TRUE),
                      max(cm_out()$SPLINED_VALUE, na.rm = TRUE)))
        }
  })

sp_ymin <- reactive({
  floor(min(min(to_be_splined()[[2]], na.rm = TRUE),
            min(sd_out()$UD,          na.rm = TRUE),
            min(cm_out()$UD,       na.rm = TRUE))) # usually 0, lbr
  })

sp_ymax <- reactive({
  if(input$plot_scale == 1) {
    ceiling(max(max(plot_tbs()[[3]],   na.rm = TRUE),
                max(plot_sd_out()$DEPTH, na.rm = TRUE),
                max(plot_cm_out()$DEPTH, na.rm = TRUE)))
    } else if(input$plot_scale == 2) {
      ceiling(max(quantile(unlist(to_be_splined()[[3]]),   probs = 0.75,
                           na.rm = TRUE, names = FALSE),
                  quantile(sd_out()$LD, probs = 0.75, na.rm = TRUE,
                           names = FALSE),
                  quantile(cm_out()$LD, probs = 0.75, na.rm = TRUE,
                           names = FALSE)))
      } else if(input$plot_scale == 3) {
        ceiling(max(quantile(unlist(to_be_splined()[[3]]),   probs = 0.95,
                          na.rm = TRUE, names = FALSE),
                    quantile(sd_out()$LD, probs = 0.95, na.rm = TRUE,
                             names = FALSE),
                    quantile(cm_out()$LD, probs = 0.95, na.rm = TRUE,
                             names = FALSE)))
   } else {
     ceiling(max(max(to_be_splined()[[3]],   na.rm = TRUE),
                 max(sd_out()$LD, na.rm = TRUE),
                 max(cm_out()$LD, na.rm = TRUE)))
   }
 })

## ggplot setup
# deploy graph with tickyboxes for any combo of the three geoms

base_plot <- reactive({

  ggplot() +
    scale_x_reverse(name      = 'Soil Depth',
                    limits    = c(sp_ymax(), sp_ymin())) +
    scale_y_continuous(name   = 'Analyte Value',
                       limits = c(sp_xmin(), sp_xmax())) +
    guides(alpha = 'none',
           fill  = guide_legend(override.aes = list(alpha = 0.5))) +
    coord_flip() +
    theme_minimal() +
    ggtitle('Spline data', subtitle = paste0('Site ', input$SID))

   })

in_plot <- reactive({

  geom_rect(data    = plot_tbs(),
            mapping = aes(ymin = plot_tbs()[[4]], ymax = sp_xmin(),
                          xmin = plot_tbs()[[2]], xmax = plot_tbs()[[3]],
                          alpha = 0.5, fill = 'grey50'))

  })

sd_plot <- reactive({

  # NB geom_step has a funny interaction with scale_*_reverse - do not use
  geom_path(data    = plot_sd_out(),
            mapping = aes(x = DEPTH, y = SPLINED_VALUE, col = 'darkblue'),
            size    = 1)

  })

cm_plot <- reactive({

  geom_line(data    = plot_cm_out(),
            mapping = aes(x = DEPTH - 0.5, y = SPLINED_VALUE, col = 'red'),
            size    = 1)

  })

out_plot <- reactive({

  req(input$SID)
  req(input$splinetime)

  gopts <- input$pick_geoms

  if(is.null(gopts)) {
    base_plot()
    # all unticked
    } else if(!('1' %in% gopts) & !('2' %in% gopts) & !('3' %in% gopts)) {
      base_plot()
      # only input ticked
      } else if(('1' %in% gopts) & !('2' %in% gopts) & !('3' %in% gopts)) {
        base_plot() + in_plot() + sfm
        # only standard output ticked
        } else if(!('1' %in% gopts) & ('2' %in% gopts) & !('3' %in% gopts)) {
          base_plot() + sd_plot() + scm
          # only 1cm output ticked
          } else if(!('1' %in% gopts) & !('2' %in% gopts) & ('3' %in% gopts)) {
            base_plot() + cm_plot() + scm
            # input and standard output ticked
            } else if(('1' %in% gopts) & ('2' %in% gopts) & !('3' %in% gopts)) {
              base_plot() + in_plot() + sd_plot() + sfm + scm
              # input and 1 cm output ticked
              } else if(('1' %in% gopts) & !('2' %in% gopts) & ('3' %in% gopts)) {
                base_plot() + in_plot() + cm_plot() + sfm + scm2
                # standard and 1cm output ticked
                } else if(!('1' %in% gopts) & ('2' %in% gopts) & ('3' %in% gopts)) {
                  base_plot() + sd_plot() + cm_plot() + scm
                  # all options ticked
                  } else if(('1' %in% gopts) & ('2' %in% gopts) & ('3' %in% gopts)) {
                    base_plot() + in_plot() + sd_plot() + cm_plot() + sfm + scm
                    }
  })

 output$splineplot <- renderPlot({

   validate(need(can_plot(),
                 paste0('Data for ', input$SID, ' could not be splined.')))
   out_plot()

   })

  output$splinetable_sd <- DT::renderDT({
    req(input$SID)
    req(input$splinetime)
    sd_out()[sd_out()[[1]] == input$SID, ] },
    rownames = FALSE,
    options = list(lengthChange = FALSE, pageLength = 10,
    scrollX = FALSE, scrollY = '300px',
    paging = FALSE, searching = FALSE, info = FALSE))

  output$splinetable_cm <- DT::renderDT({
    req(input$SID)
    req(input$splinetime)
    cm_out()[cm_out()[[1]] == input$SID, ] },
    rownames = FALSE,
    options = list(lengthChange = FALSE, pageLength = 10,
                   scrollX = FALSE, scrollY = '300px',
                   paging = FALSE, searching = FALSE, info = FALSE))


  ## DOWNLOADS

  # Standard depths as CSV
  output$dl_sd <- downloadHandler(
    filename = function() {
      fn <- input$file_1
      paste0(tools::file_path_sans_ext(fn$name),
             "_stdout_", format(Sys.Date(), '%Y%m%d'), ".csv")
    },
    content = function(file) {
      req(sd_out())
      readr::write_csv(sd_out(), file)
    },
    contentType = 'text/csv'
  )

  # 1cm depths as CSV
  output$dl_1cm <- downloadHandler(
    filename = function() {
      fn <- input$file_1
      paste0(tools::file_path_sans_ext(fn$name),
             "_cmsout_", format(Sys.Date(), '%Y%m%d'), ".csv")
    },
    content = function(file) {
      req(cm_out())
      readr::write_csv(cm_out(), file)
    },
    contentType = 'text/csv'
  )

  # everything as RDS
  output$dl_rds <- downloadHandler(
    filename = function() {
      fn <- input$file_1
      paste0(tools::file_path_sans_ext(fn$name),
             "_all_outputs_", format(Sys.Date(), '%Y%m%d'), ".rds")
    },
    content = function(file) {
      req(splined())
      saveRDS(splined(), file)
    }
  )

  # plot as PNG
  output$dl_plot <- downloadHandler(
    filename = function() {
      fn <- input$file_1
      paste0(tools::file_path_sans_ext(fn$name), '_Site_', input$SID, '_splineplot.png') },
    content = function(file) {
      ggsave(file, plot = out_plot(), device = "png")
    }
  )
}

shinyApp(ui = ui, server = server)
