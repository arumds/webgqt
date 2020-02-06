####load the required libraries########
require(shiny)
require(shinyjs)
require(shinydashboard)
require(ipc)
require(promises)
require(future)
require(V8)
require(data.table)
require(ggplot2)
require(glue)
require(here)

plan(multiprocess)

appDir <- getwd()
###default gqt path
gqt_path.default <- '/usr/local/gqt/bin/gqt'  

###set gqt path as shinyoption        
gqt_path <- getShinyOption("gqt_path", gqt_path.default)    

###set images resource folder used in the app
addResourcePath('imageFol', system.file('extdata', 'webgqt/images', package = 'webGQT'))

jsResetCode <- "shinyjs.reset = function() {history.go(0)}"


### This is the UI logic for the app
##Code for dashboard sidebar menu items

sidebar <- dashboardSidebar(
  width = 200,
  sidebarMenu(
    id = "tabs",
    menuItem("Home",
             tabName = "documentation",
             icon = icon("fas fa-home")),
    menuItem(
      "Quick Start-up Guide",
      tabName = "getStarted",
      icon = icon("fa-info")
    ),
    menuItem(
      "Variant Data (VCF)",
      tabName = "dataset",
      icon = icon("fas fa-upload")
    ),
    menuItem(
      "Phenotype File",
      tabName = "pheno",
      icon = icon("fas fa-file")
    ),
    menuItem(
      "Pedigree module",
      tabName = "pedigree_module",
      menuSubItem("Dominant", tabName = "dominantModel"),
      menuSubItem("Homozygous Recessive", tabName = "recessiveModel"),
      menuSubItem("Compound Heterozygous", tabName = "compoundHetModel"),
      menuSubItem("Recessive De novo", tabName = "recdenovoModel"),
      menuSubItem("Dominant De novo", tabName = "domdenovoModel")
    ),
    menuItem(
      "Case-Control module",
      tabName = "cc_module",
      menuSubItem("Case-specific variants", tabName = "csModel"),
      menuSubItem("Shared variants in cases", tabName = "sharedModel")
    ),
    menuItem(
      "Population module",
      tabName = "pop_module",
      menuSubItem("Individual count", tabName = "popcountModel"),
      menuSubItem("Allele Frequency", tabName = "popModel"),
      menuSubItem("Sample", tabName = "sampleModel")
    ),
    menuItem("Variant count",
             tabName = "countModel")
  )
)


####code for the body of each menu item defined above########

bodyGuide <- tabItem(tabName = "getStarted",
                     value = "main_panel",
                     fluidRow(
                       box(
                         title = "Quick Start-up guide",
                         width = 12,
                         status = "primary",
                         solidHeader = TRUE,
                         includeHTML(
                           system.file('extdata', "webgqt/www/guide.html", package = "webGQT")
                         )
                       )
                     ))


bodyInfo <- tabItem(
  tabName = "documentation",
  value = "main_panel",
  fluidRow(
    box(
      width = 12,
      status = "primary",
      solidHeader = TRUE,
      includeHTML(
        system.file('extdata', "webgqt/www/Info.html", package = "webGQT")
      )
    )
  ),
  
  
  fluidRow(
    column(width = 4,
           box(
             title = NULL,
             width = 12,
             status = "info",
             img(src = 'imageFol/recessive-human.png',
                 align = "center",
                 width = "100%")
           )),
    column(width = 4,
           box(
             title = NULL,
             width = 12,
             status = "info",
             img(src = 'imageFol/dominant-human.png', align = "center", width =
                   "100%")
           )),
    column(width = 4,
           box(
             #      title="Results",
             width = 12,
             status = "info",
             img(src = 'imageFol/denovo-human.png', align = "center", width =
                   "100%")
           ))
  ),
  
  
  fluidRow(
    column(
      width = 4,
      box(
        #title = "Results: Table",
        width = 12,
        status = "info",
        img(src = 'imageFol/summary.png', align = "center", width =
              "100%")
      )
    ),
    column(
      width = 4,
      box(
       # title = "Results: Table",
        width = 12,
        status = "info",
        img(src = 'imageFol/table.png', align = "center", width = "100%")
      )
    ),
    column(
      width = 4,
      box(
       title = "Plot",
        width = 12,
        status = "info",
        img(src = 'imageFol/plot.png', align = "center", width = "100%")
      )
    )
  )
)

bodyVCF <- tabItem(
  tabName = "dataset",
  value = "vcf_raw_input",
  h2("Variant Data in VCF/BCF format"),
  fluidRow(
    column(
      12,
      offset = 0.5,
      radioButtons(
        "dataset_rd",
        label = "Chose Dataset:",
        c("1000 Genomes", "Upload VCF"),
        selected = "1000 Genomes"
      )
    ),
    
    conditionalPanel(
      "input.dataset_rd == '1000 Genomes'",
      column(
        12,
        offset = 0.5,
        actionButton("phase3_vcf_input_next", "Next", icon("arrow-right"))
      ),
      br(),
      box(
        #title = "1000G Phase3 data",
        color = "blue",
        width = 12,
        solidHeader = TRUE,
        includeHTML(
          system.file('extdata', "webgqt/www/Help.html", package = "webGQT")
        )
      )
    ),
    
    conditionalPanel(
      "input.dataset_rd == 'Upload VCF'",
      column(
        12,
        offset = 0.5,
        tabsetPanel(
          id = "input_vcf",
          tabPanel(
            title = "Upload VCF",
            value = "upload_vcf",
            fileInput(
              inputId = "uploadVCF",
              label = "",
              multiple = TRUE,
              accept = c(".vcf", ".bcf", ".gz", ".bim", ".csi", ".off", ".vid", ".gqt")
            )
          )
        ),
        actionButton("uploadvcf_input_next", "Next", icon("arrow-right"))
      ),
      box(
        title = "Instructions for uploading VCF",
        status = "primary",
        width = 12,
        solidHeader = TRUE,
        includeHTML(
          system.file('extdata', "webgqt/www/vcf.html", package = "webGQT")
        )
      )
    )
  )
)


bodyPED <- tabItem(
  tabName = "pheno",
  value = "ped_input",
  h2("Select Phenotype file"),
  fluidRow(
    column(
      12,
      offset = 0.5,
      radioButtons(
        "ped_rd",
        label = "Chose Dataset:",
        choices = list("1000 Genomes",
                       "Upload PED"),
        selected = "1000 Genomes"
      )
    ),
    
    conditionalPanel(
      "input.ped_rd == '1000 Genomes'",
      box(
        id = "phase3_ped_box",
        title = "1000 Genomes Phase3 Sample Database",
        width = 7,
        status = "info",
        tabPanel(
          "Phase 3 Sample Info",
          value = "phase3_samples",
          DT::dataTableOutput("phase3_ped_table"),
          actionButton("phase3_ped_input_next", "Next", icon("arrow-right"))
        )
      ),
      box(
        title = "Help",
        collapsible = TRUE,
        status = "info",
        width = 5,
        solidHeader = TRUE,
        includeHTML(
          system.file('extdata', "webgqt/www/ped.html", package = "webGQT")
        )
      )
    )
  ),
  
  fluidRow(conditionalPanel(
    "input.ped_rd == 'Upload PED'",
    box(
      id = "ped_box",
      title = "Create GQT Database",
      width = 7,
      status = "info",
      tabsetPanel(
        id = "ped_tab",
        tabPanel(
          "Upload PED",
          value = "upload_ped",
          fileInput(
            inputId = "uploadPED",
            label = "Upload PED file",
            multiple = FALSE,
            accept = c(".ped")
          ),
          checkboxInput('header', label = 'Header', TRUE),
          actionButton("checkdb", "Check Samples")
        ),
        
        tabPanel(
          "Create GQT database",
          value = "samples",
          tableOutput("ped_summary"),
          icon = icon("info"),
          DT::dataTableOutput("ped_full_table"),
          actionButton("createdb", "Create DB")
        )
      )
    ),
    
    box(
      title = "Help",
      collapsible = TRUE,
      status = "info",
      width = 5,
      solidHeader = TRUE,
      includeHTML(
        system.file('extdata', "webgqt/www/ped.html", package = "webGQT")
      )
    )
  ))
)

bodyDom <- tabItem(tabName = "dominantModel",
                   h2("Dominant"),
                   fluidRow(
                     box(
                       width = 7,
                       status = "info",
                       tabsetPanel(
                         id = "dom_tab",
                         tabPanel(
                           "Parameters",
                           numericInput(
                             "domCases",
                             label = "Maximum number of cases allowed to have missing genotype",
                             value = 0,
                             max = 10
                           ),
                           numericInput(
                             "domMAF",
                             label = "Maximum MAF in controls",
                             value = 0,
                             min = 0,
                             max = 1,
                             step = 0.1
                           ),
                           actionButton("dom_run", "Filter"),
                           actionButton("dom_cancel", "Cancel")
                           #actionButton("dom_status", "Check Status")
                         ),
                         
                         tabPanel(
                           "Results",
                           value = "dom_results",
                           navbarPage(
                             NULL,
                             tabPanel(
                               "Summary",
                               textOutput("dom_count"),
                               tabPanel("Summary", tableOutput("dom_summary"), icon = icon("list-alt"))
                             ),
                             tabPanel(
                               "Table",
                               DT::dataTableOutput("dom_table"),
                               icon = icon("table"),
                               downloadButton("dom_downList", "Download Table"),
                               downloadButton("dom_VCFdownList", "Download VCF")
                             ),
                             tabPanel(
                               "Plot",
                               plotOutput("dom_plot"),
                               icon = icon("bar-chart-o"),
                               downloadButton("dom_downPlot", "Download Plot")
                             )
                           )
                           
                         )
                       )
                     ),
                     
                     box(
                       #title = "Description",
                       width = 5,
                       status = "info",
                       includeHTML(
                         system.file('extdata', "webgqt/www/dom.html", package = "webGQT")
                       ),
                       img(src = 'imageFol/dominant-human.png', align = "center", width =
                             "100%")
                       #tabBox(id = "dom_infobox", height = "100%", width = "100%")
                     )
                   ))

bodyCompHet <- tabItem(tabName = "compoundHetModel",
                   h2("Compound Heterozygous"),
                   fluidRow(
                     box(
                       width = 7,
                       status = "info",
                       tabsetPanel(
                         id = "compHet_tab",
                         tabPanel(
                           "Parameters",
                           numericInput(
                             "compHetCases",
                             label = "Maximum number of cases allowed to have missing genotype",
                             value = 0,
                             max = 10
                           ),
                           numericInput(
                             "compHetMAF",
                             label = "Maximum MAF in controls",
                             value = 0,
                             min = 0,
                             max = 1,
                             step = 0.1
                           ),
                           actionButton("comp_run", "Filter"),
                           actionButton("comp_cancel", "Cancel")
                           #actionButton("dom_status", "Check Status")
                         ),
                         
                         tabPanel(
                           "Results",
                           value = "compHet_results",
                           navbarPage(
                             NULL,
                             tabPanel(
                               "Summary",
                               textOutput("compHet_count"),
                               tabPanel("Summary", tableOutput("compHet_summary"), icon = icon("list-alt"))
                             ),
                             tabPanel(
                               "Table",
                               DT::dataTableOutput("compHet_table"),
                               icon = icon("table"),
                               downloadButton("compHet_downList", "Download Table"),
                               downloadButton("compHet_VCFdownList", "Download VCF")
                             ),
                             tabPanel(
                               "Plot",
                               plotOutput("compHet_plot"),
                               icon = icon("bar-chart-o"),
                               downloadButton("comp_downPlot", "Download Plot")
                             )
                           )
                           
                         )
                       )
                     ),
                     
                     box(
                       #title = "Description",
                       width = 5,
                       status = "info",
                       includeHTML(
                         system.file('extdata', "webgqt/www/compHet.html", package = "webGQT")
                       )
                       # img(src = 'imageFol/dominant-human.png', align = "center", width =
                       #       "100%")
                       #tabBox(id = "dom_infobox", height = "100%", width = "100%")
                     )
                   ))

bodyRec <- tabItem(tabName = "recessiveModel",
                   h2("Homozygous Recessive"),
                   fluidRow(
                     box(
                       width = 7,
                       status = "info",
                       tabsetPanel(
                         id = "rec_tab",
                         tabPanel(
                           "Parameters",
                           numericInput(
                             "recCases",
                             label = "Maximum number of cases allowed to have missing genotype",
                             value = 0,
                             max = 10
                           ),
                           numericInput(
                             "recMAF",
                             label = "Maximum MAF in controls",
                             value = 0,
                             min = 0,
                             max = 1,
                             step = 0.1
                           ),
                           actionButton("rec_run", "Filter"),
                           actionButton("rec_cancel", "Cancel")
                           #actionButton("rec_status", "Check Status")
                         ),
                         
                         tabPanel(
                           "Results",
                           value = "rec_results",
                           navbarPage(
                             NULL,
                             tabPanel(
                               "Summary",
                               textOutput("rec_count"),
                               tabPanel("Summary", tableOutput("rec_summary"), icon = icon("list-alt"))
                             ),
                             tabPanel(
                               "Table",
                               DT::dataTableOutput("rec_table"),
                               icon = icon("table"),
                               downloadButton("rec_downList", "Download Table"),
                               downloadButton("rec_VCFdownList", "Download VCF")
                             ),
                             tabPanel(
                               "Plot",
                               plotOutput("rec_plot"),
                               icon = icon("bar-chart-o"),
                               downloadButton("rec_downPlot", "Download Plot")
                             )
                           )
                           
                         )
                       )
                     ),
                     
                     box(
                       #title = "Description",
                       width = 5,
                       status = "info",
                       includeHTML(
                         system.file('extdata', "webgqt/www/rec.html", package = "webGQT")
                       ),
                       img(src = 'imageFol/recessive-human.png', align = "center", width =
                             "100%")
                      
                     )
                   ))


bodyDomDenovo <- tabItem(tabName = "domdenovoModel",
                         h2("Dominant De novo"),
                         fluidRow(
                           box(
                             width = 7,
                             status = "info",
                             tabsetPanel(
                               id = "dom_denovo_tab",
                               tabPanel(
                                 "Parameters",
                                 numericInput(
                                   "domDenovoCases",
                                   label = "Maximum number of cases allowed to have missing genotype",
                                   value = 0,
                                   max = 10
                                 ),
                                 numericInput(
                                   "domDenovoMAF",
                                   label = "Maximum MAF in controls",
                                   value = 0,
                                   min = 0,
                                   max = 1,
                                   step = 0.1
                                 ),
                                 actionButton("dom_denovo_run", "Filter"),
                                 actionButton("dom_denovo_cancel", "Cancel")
                               ),
                               tabPanel(
                                 "Results",
                                 value = "dom_denovo_results",
                                 navbarPage(
                                   NULL,
                                   tabPanel(
                                     "Summary",
                                     textOutput("dom_denovo_count"),
                                     tabPanel(
                                       "Summary",
                                       tableOutput("dom_denovo_summary"),
                                       icon = icon("list-alt")
                                     )
                                   ),
                                   tabPanel(
                                     "Table",
                                     DT::dataTableOutput("dom_denovo_table"),
                                     icon = icon("table"),
                                     downloadButton("dom_denovo_downList", "Download Table"),
                                     downloadButton("dom_denovo_VCFdownList", "Download VCF")
                                   ),
                                   tabPanel(
                                     "Plot",
                                     plotOutput("dom_denovo_plot"),
                                     icon = icon("bar-chart-o"),
                                     downloadButton("dom_denovo_downPlot", "Download Plot")
                                   )
                                 )
                                 
                               )
                             )
                           ),
                           
                           box(
                             #title = "Description",
                             width = 5,
                             status = "info",
                             includeHTML(
                               system.file('extdata', "webgqt/www/domdenovo.html", package = "webGQT")
                             ),
                             img(src = 'imageFol/denovo-human.png', align = "center", width =
                                   "100%")
                           )
                         ))


bodyRecDenovo <- tabItem(tabName = "recdenovoModel",
                         h2("Recessive De novo"),
                         fluidRow(
                           box(
                             width = 7,
                             status = "info",
                             tabsetPanel(
                               id = "rec_denovo_tab",
                               tabPanel(
                                 "Parameters",
                                 numericInput(
                                   "recDenovoCases",
                                   label = "Maximum number of cases allowed to have missing genotype",
                                   value = 0,
                                   max = 10
                                 ),
                                 numericInput(
                                   "recDenovoMAF",
                                   label = "Maximum MAF in controls",
                                   value = 0,
                                   min = 0,
                                   max = 1,
                                   step = 0.1
                                 ),
                                 actionButton("rec_denovo_run", "Filter"),
                                 actionButton("rec_denovo_cancel", "Cancel")
                               ),
                               tabPanel(
                                 "Results",
                                 value = "rec_denovo_results",
                                 navbarPage(
                                   NULL,
                                   tabPanel(
                                     "Summary",
                                     textOutput("rec_denovo_count"),
                                     tabPanel(
                                       "Summary",
                                       tableOutput("rec_denovo_summary"),
                                       icon = icon("list-alt")
                                     )
                                   ),
                                   tabPanel(
                                     "Table",
                                     DT::dataTableOutput("rec_denovo_table"),
                                     icon = icon("table"),
                                     downloadButton("rec_denovo_downList", "Download Table"),
                                     downloadButton("rec_denovo_VCFdownList", "Download VCF")
                                   ),
                                   tabPanel(
                                     "Plot",
                                     plotOutput("rec_denovo_plot"),
                                     icon = icon("bar-chart-o"),
                                     downloadButton("rec_denovo_downPlot", "Download Plot")
                                   )
                                 )
                                 
                               )
                             )
                           ),
                           
                           box(
                             width = 5,
                             status = "info",
                             includeHTML(
                               system.file('extdata', "webgqt/www/recdenovo.html", package = "webGQT")
                             ),
                             img(src = 'imageFol/denovo-human.png', align = "center", width =
                                   "100%")
                           )
                         ))

bodyCS <- tabItem(tabName = "csModel",
                  h2("Case-specific module"),
                  fluidRow(
                    box(
                      width = 7,
                      status = "info",
                      tabsetPanel(
                        id = "case_specific_tab",
                        tabPanel(
                          "Parameters",
                          numericInput(
                            "cs_missing_cases",
                            label = "Maximum number of cases allowed to have missing genotype",
                            value = 0,
                            max = 10
                          ),
                          selectizeInput(
                            "case_specific_vartype",
                            "Chose Genotype",
                            choices = c("HET" , "HOM_ALT", "HET HOM_ALT"),
                            multiple = FALSE,
                            selected = "HOM_ALT",
                            options = list(maxItems = 1)
                          ),
                          actionButton("case_specific_run", "Filter"),
                          actionButton("case_specific_cancel", "Cancel")
                        ),
                        tabPanel(
                          "Results",
                          value = "case_specific_results",
                          navbarPage(
                            NULL,
                            tabPanel(
                              "Summary",
                              textOutput("case_specific_count"),
                              tabPanel(
                                "Summary",
                                tableOutput("case_specific_summary"),
                                icon = icon("list-alt")
                              )
                            ),
                            tabPanel(
                              "Table",
                              DT::dataTableOutput("case_specific_table"),
                              icon = icon("table"),
                              downloadButton("case_specific_downList", "Download Table"),
                              downloadButton("case_specific_VCFdownList", "Download VCF")
                            ),
                            tabPanel(
                              "Plot",
                              plotOutput("case_specific_plot"),
                              icon = icon("bar-chart-o"),
                              downloadButton("case_specific_downPlot", "Download Plot")
                            )
                          )
                          
                        )
                      )
                    ),
                    box(
                      title = "Help",
                      width = 5,
                      status = "info",
                      solidHeader = TRUE,
                      includeHTML(
                        system.file('extdata', "webgqt/www/cs.html", package = "webGQT")
                      )
                    )
                  ))

bodyShared <- tabItem(tabName = "sharedModel",
                      h2("Cases shared module"),
                      fluidRow(
                        box(
                          width = 7,
                          status = "info",
                          tabsetPanel(
                            id = "case_shared_tab",
                            tabPanel(
                              "Parameters",
                              selectizeInput(
                                "case_shared_vartype",
                                "Chose Genotype",
                                choices = c("HET" , "HOM_ALT", "HET HOM_ALT"),
                                multiple = FALSE,
                                selected = "HOM_ALT",
                                options = list(maxItems = 1)
                              ),
                              numericInput(
                                "case_shared_count",
                                label = "Minimum cases to have the genotype",
                                value = 1,
                                min = 0,
                                max = 10,
                                step = 1
                              ),
                              numericInput(
                                "sharedMAF",
                                label = "Maximum MAF in controls",
                                value = 1,
                                min = 0,
                                max = 1,
                                step = 0.1
                              ),
                              actionButton("case_shared_run", "Filter"),
                              actionButton("case_shared_cancel", "Cancel")
                            ),
                            tabPanel(
                              "Results",
                              value = "case_shared_results",
                              navbarPage(
                                NULL,
                                tabPanel(
                                  "Summary",
                                  textOutput("case_shared_count"),
                                  tabPanel(
                                    "Summary",
                                    tableOutput("case_shared_summary"),
                                    icon = icon("list-alt")
                                  )
                                ),
                                tabPanel(
                                  "Table",
                                  DT::dataTableOutput("case_shared_table"),
                                  icon = icon("table"),
                                  downloadButton("case_shared_downList", "Download Table"),
                                  downloadButton("case_shared_VCFdownList", "Download VCF")
                                ),
                                tabPanel(
                                  "Plot",
                                  plotOutput("case_shared_plot"),
                                  icon = icon("bar-chart-o"),
                                  downloadButton("case_shared_downPlot", "Download Plot")
                                )
                              )
                              
                            )
                          )
                        ),
                        
                        box(
                          title = "Help",
                          width = 5,
                          status = "info",
                          solidHeader = TRUE,
                          includeHTML(
                            system.file('extdata', "webgqt/www/shared.html", package = "webGQT")
                          )
                        )
                      ))

bodypopcount <- tabItem(
  tabName = "popcountModel",
  value = "popcount_input",
  h2("Querying by population"),
  fluidRow(
    box(
      width = 7,
      status = "info",
      tabsetPanel(
        id = "popcount_tab",
        tabPanel(
          "Parameters",
          selectizeInput(
            inputId = "pop1count",
            label = "Select Population 1",
            choices = NULL,
            multiple = TRUE,
            options = list(placeholder = 'enter population names')
          ),
          selectizeInput(
            inputId = "pop1_vartype",
            label = "Chose genotype",
            choices = c("HET" , "HOM_ALT", "HET HOM_ALT"),
            multiple = FALSE,
            selected = "HET",
            options = list(maxItems = 1)
          ),
          numericInput(
            inputId = "pop1count_min",
            label = "Minimum percentage of individuals",
            value = 1,
            min = 1,
            max = 100,
            step = 10
          ),
          selectizeInput(
            inputId = "pop2count",
            label = "Select Population 2",
            choices = NULL,
            multiple = TRUE,
            options = list(placeholder = 'enter population names')
          ),
          selectizeInput(
            inputId = "pop2_vartype",
            label = "Chose genotype",
            choices = c("HET" , "HOM_ALT", "HET HOM_ALT"),
            multiple = FALSE,
            selected = "HET",
            options = list(maxItems = 1)
          ),
          numericInput(
            inputId = "pop2count_max",
            label = "Maximum percentage of individuals",
            value = 0,
            min = 0,
            max = 100,
            step = 10
          ),
          actionButton("popcount_run", "Filter"),
          actionButton("popcount_cancel", "Cancel")
        ),
        
        tabPanel(
          "Results",
          value = "popcount_results",
          navbarPage(
            NULL,
            tabPanel(
              "Summary",
              textOutput("popcount_count"),
              
              tabPanel(
                "Summary",
                tableOutput("popcount_summary"),
                icon = icon("list-alt")
              )
            ),
            tabPanel(
              "Table",
              DT::dataTableOutput("popcount_table"),
              icon = icon("table"),
              downloadButton("popcount_downList", "Download Table"),
              downloadButton("popcount_VCFdownList", "Download VCF")
            ),
            tabPanel(
              "Plot",
              plotOutput("popcount_plot"),
              icon = icon("bar-chart-o"),
              downloadButton("popcount_downPlot", "Download Plot")
            )
          )
          
        )
      )
    ),
    
    box(
      title = "Help",
      solidHeader = TRUE,
      status = "info",
      width = 5,
      includeHTML(
        system.file('extdata', "webgqt/www/popcount.html", package = "webGQT")
      )
    )
  )
)


bodypop <- tabItem(
  tabName = "popModel",
  value = "pop_input",
  h2("Querying by population"),
  fluidRow(
    box(
      width = 7,
      status = "info",
      tabsetPanel(
        id = "pop_tab",
        tabPanel(
          "Parameters",
          selectizeInput(
            inputId = "pop1",
            "Select Population 1",
            choices = NULL,
            multiple = TRUE,
            options = list(placeholder = 'enter population names')
          ),
          selectizeInput(
            inputId = "pop1_gender",
            "Chose by Gender",
            choices = NULL,
            multiple = TRUE,
            options = list(placeholder = 'enter gender')
          ),
          numericInput(
            inputId = "pop1_min_MAF",
            label = "Minimum MAF",
            value = 0,
            min = 0,
            max = 1,
            step = 0.1
          ),
          selectizeInput(
            inputId = "pop2",
            "Select Population 2",
            choices = NULL,
            multiple = TRUE,
            options = list(placeholder = 'enter population names')
          ),
          selectizeInput(
            inputId = "pop2_gender",
            "Chose by Gender",
            choices = NULL,
            multiple = TRUE,
            options = list(placeholder = 'enter gender')
          ),
          numericInput(
            inputId = "pop2_max_MAF",
            label = "Maximum MAF",
            value = 1,
            min = 0,
            max = 1,
            step = 0.1
          ),
          actionButton("pop_run", "Filter"),
          actionButton("pop_cancel", "Cancel")
        ),
        
        tabPanel(
          "Results",
          value = "pop_results",
          navbarPage(
            NULL,
            tabPanel(
              "Summary",
              textOutput("pop_count"),
              tabPanel("Summary", tableOutput("pop_summary"), icon = icon("list-alt"))
            ),
            tabPanel(
              "Table",
              DT::dataTableOutput("pop_table"),
              icon = icon("table"),
              downloadButton("pop_downList", "Download Table"),
              downloadButton("pop_VCFdownList", "Download VCF")
            ),
            tabPanel(
              "Plot",
              plotOutput("pop_plot"),
              icon = icon("bar-chart-o"),
              downloadButton("pop_downPlot", "Download Plot")
            )
          )
          
        )
      )
    ),
    
    box(
      title = "Help",
      solidHeader = TRUE,
      status = "info",
      width = 5,
      includeHTML(
        system.file('extdata', "webgqt/www/pop.html", package = "webGQT")
      )
    )
  )
)
bodySamples <- tabItem(tabName = "sampleModel",
                       value = "sample_input",
                       h2("Querying by sample"),
                       fluidRow(
                         box(
                           width = 7,
                           status = "info",
                           tabsetPanel(
                             id = "sample_tab",
                             tabPanel(
                               "Parameters",
                               selectizeInput(
                                 inputId = "samples",
                                 "Select IndividualID",
                                 choices = NULL,
                                 multiple = TRUE,
                                 options = list(placeholder = 'enter sample names')
                               ),
                               selectizeInput(
                                 "sample_vartype",
                                 "Chose Genotype",
                                 choices = c("HET" , "HOM_ALT", "HET HOM_ALT"),
                                 multiple = FALSE,
                                 selected = "HET",
                                 options = list(maxItems = 1)
                               ),
                               numericInput(
                                 "sample_min_count",
                                 label = "Minimum Count",
                                 value = 1,
                                 min = 1,
                                 max = 100
                               ),
                               actionButton("sample_run", "Filter"),
                               actionButton("sample_cancel", "Cancel")
                             ),
                             
                             tabPanel(
                               "Results",
                               value = "sample_results",
                               navbarPage(
                                 NULL,
                                 tabPanel(
                                   "Summary",
                                   textOutput("sample_count"),
                                   tabPanel("Summary", tableOutput("sample_summary"), icon = icon("list-alt"))
                                 ),
                                 tabPanel(
                                   "Table",
                                   DT::dataTableOutput("sample_table"),
                                   icon = icon("table"),
                                   downloadButton("sample_downList", "Download Table"),
                                   downloadButton("sample_VCFdownList", "Download VCF")
                                 ),
                                 tabPanel(
                                   "Plot",
                                   plotOutput("sample_plot"),
                                   icon = icon("bar-chart-o"),
                                   downloadButton("sample_downPlot", "Download Plot")
                                 )
                               )
                               
                             )
                           )
                         ),
                         box(
                           title = "Help",
                           solidHeader = TRUE,
                           status = "info",
                           width = 5,
                           includeHTML(
                             system.file('extdata', "webgqt/www/sample.html", package = "webGQT")
                           )
                         )
                       ))


bodyCount <- tabItem(
  tabName = "countModel",
  value = "count_input",
  h2("Count variants in a sample"),
  fluidRow(box(
    width = 7,
    status = "info",
    tabsetPanel(
      id = "count_tab",
      tabPanel(
        "Parameters",
        selectizeInput(
          inputId = "names",
          "Select IndividualID",
          choices = NULL,
          multiple = FALSE,
          options = list(placeholder = 'enter sample name')
        ),
        selectizeInput(
          "count_vartype",
          "Chose Genotype",
          choices = c("HET" , "HOM_ALT", "HET HOM_ALT"),
          multiple = FALSE,
          selected = "HET",
          options = list(maxItems = 1)
        ),
        actionButton("variant_run", "Get Count")
      ),
      
      tabPanel("Results",
               value = "variant_results",
               navbarPage(NULL,
                          tabPanel(
                            "Summary", textOutput("variant_count")
                          )))
    )
  ),
  box(
    title = "Help",
    width = 5,
    status = "info",
    solidHeader = TRUE,
    includeHTML(
      system.file('extdata', "webgqt/www/count.html", package = "webGQT")
    )
  ))
)



# Put them together into a dashboardPage
ui <- shinyUI(dashboardPage(
  skin = "blue",
  dashboardHeader(title = "webGQT", titleWidth = 150),
  sidebar,
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      bodyInfo,
      bodyGuide,
      bodySamples,
      bodyVCF,
      bodyPED,
      bodyRec,
      bodyCompHet,
      bodyDom,
      bodyRecDenovo,
      bodyDomDenovo,
      bodyCS,
      bodyShared,
      bodypopcount,
      bodypop,
      bodyCount
    )
  )
))



# This is the server logic of a Shiny webgqt application.


options(shiny.maxRequestSize = 100000 * 1024 ^ 2)

server <- function(input, output, session) {
  wrk_dir <- getwd()
  tmp_dir <- tempdir()
  setwd(tmp_dir)
  
  ####code to define the path for default gqt index files with system.file######
  ###the user can set the path of custom dataset here ####
  gqt_list <- reactive({
    if (input$dataset_rd == "1000 Genomes") {
      gz_file  <-
        system.file("extdata", 'webgqt/data/chr1.vcf.gz' , package = "webGQT")
      gqt_file <-
        system.file("extdata", 'webgqt/data/chr1.vcf.gz.gqt' , package = "webGQT")
      bim_file <-
        system.file("extdata", 'webgqt/data/chr1.vcf.gz.bim', package = "webGQT")
      csi_file <-
        system.file("extdata", 'webgqt/data/chr1.vcf.gz.csi', package = "webGQT")
      off_file <-
        system.file("extdata", 'webgqt/data/chr1.vcf.gz.off', package = "webGQT")
      vid_file <-
        system.file("extdata", 'webgqt/data/chr1.vcf.gz.vid', package = "webGQT")
      
      
      gz_base <- tools::file_path_sans_ext(basename(gz_file))
      gz_new <- paste0(tools::file_path_sans_ext(gz_file), ".gz")
      gqt_new <-
        paste0(dirname(gqt_file), "/", gz_base, ".gz.gqt")
      bim_new <-
        paste0(dirname(bim_file), "/", gz_base, ".gz.bim")
      csi_new <-
        paste0(dirname(csi_file), "/", gz_base, ".gz.csi")
      off_new <-
        paste0(dirname(off_file), "/", gz_base, ".gz.off")
      vid_new <-
        paste0(dirname(vid_file), "/", gz_base, ".gz.vid")
      
      file.copy(gz_file, gz_new)
      file.copy(gqt_file, gqt_new)
      file.copy(bim_file, bim_new)
      file.copy(csi_file, csi_new)
      file.copy(off_file, off_new)
      file.copy(vid_file, vid_new)
      
 ######code to assign gqt index files to list of variables######
      gqt_files <-
        list(
          gz_new = gz_new,
          gz_file = gz_file,
          gqt_new =  gqt_new,
          bim_new = bim_new,
          csi_new = csi_new,
          off_new = off_new,
          vid_new = vid_new
        )
      gqt_files
    }
    ###code to assign the uploaded GQT index files to a list of variables####
    else if (input$dataset_rd == "Upload VCF")
    {
      req(input$uploadVCF$datapath)
      files <- input$uploadVCF$datapath
      gz_file  <- files[grep(".gz$", files)]
      gqt_file <- files[grep(".gqt", files)]
      bim_file <- files[grep(".bim", files)]
      csi_file <- files[grep(".csi", files)]
      off_file <- files[grep(".off", files)]
      vid_file <- files[grep(".vid", files)]
      
      gz_base <- tools::file_path_sans_ext(basename(gz_file))
      gz_new <-
        paste0(tools::file_path_sans_ext(gz_file), ".vcf.gz")
      gqt_new <-
        paste0(dirname(gqt_file), "/", gz_base, ".vcf.gz.gqt")
      bim_new <-
        paste0(dirname(bim_file), "/", gz_base, ".vcf.gz.bim")
      csi_new <-
        paste0(dirname(csi_file), "/", gz_base, ".vcf.gz.csi")
      off_new <-
        paste0(dirname(off_file), "/", gz_base, ".vcf.gz.off")
      vid_new <-
        paste0(dirname(vid_file), "/", gz_base, ".vcf.gz.vid")
      
      file.copy(gz_file, gz_new)
      file.copy(gqt_file, gqt_new)
      file.copy(bim_file, bim_new)
      file.copy(csi_file, csi_new)
      file.copy(off_file, off_new)
      file.copy(vid_file, vid_new)
      
      gqt_files <-
        list(
          gz_new = gz_new,
          gz_file = gz_file,
          gqt_new =  gqt_new,
          bim_new = bim_new,
          csi_new = csi_new,
          off_new = off_new,
          vid_new = vid_new
        )
      gqt_files
    }
  })
  
  # switch to upload ped file tab on clicking Next after VCF selection

  observeEvent(input$phase3_vcf_input_next, {
    newtab <- switch(input$tabs,
                     "dataset" = "pheno")
    updateTabItems(session, "tabs", newtab)
  })
  
  observeEvent(input$uploadvcf_input_next, {
    newtab <- switch(input$tabs,
                     "dataset" = "pheno")
    updateTabItems(session, "tabs", newtab)
  })
  
  # switch to pedigree analysis module on clicking Next after PED selection
  observeEvent(input$phase3_ped_input_next, {
    newtab <- switch(input$tabs,
                     "pheno" = "dominantModel")
    updateTabItems(session, "tabs", newtab)
  })
  
  
  #######################function to capture ped file name and ped path ##################
  
  ped_file <- reactive({
    if (input$ped_rd == "1000 Genomes") {
      ped_path = system.file('extdata', "webgqt/data/1K.phase3.ped", package = "webGQT")
      ped_base = basename(ped_path)
      ped_files <-
        list(ped_path = ped_path,
             ped_base = ped_base)
      ped_files
    }
    else if (input$ped_rd == "Upload PED")
    {
      req(input$uploadPED)
      ped_path = input$uploadPED$datapath
      ped_base = basename(ped_path)
      ped_files <-
        list(ped_path = ped_path,
             ped_base = ped_base)
      ped_files
    }
  })
  
  
  #####unction to validate default or uploaded ped file to check for required columns###
  ped_read <- reactive({
    file_ped <- ped_file()
    ped_input <-
      read.delim(file_ped$ped_path, sep = "\t", header = T)
    
    validate(need(
      colnames(ped_input) %in% c("IndividualID"),
      "Please provide PED file with IndividualID column"
    ))
    
    validate(need(
      colnames(ped_input) %in% c("Phenotype"),
      "Please provide PED file with Phenotype column"
    ))
    
    ####### function to extract the affected and unaffected sample names from the input ped file#######
    ped_cases_carriers <-
      ped_input$IndividualID[ped_input$Phenotype == 2 |
                               ped_input$Phenotype == 3]
    regex_cases_carriers_names <-
      paste0("^FORMAT$|",
             paste0(
               "^",
               ped_cases_carriers,
               collapse = "|",
               sep = "$"
             ))
  
    #### code to save the metainfo of affected , unafected samples and the input ped file to a list of variables######
    
    ncarriers <- nrow(ped_input[ped_input$Phenotype == 3, ])
    ped_metainfo <-
      list(
        regex_cases_carriers_names = regex_cases_carriers_names,
        ncarriers  = ncarriers,
        ped_input = ped_input
      )
    ped_metainfo
    
  })
  
  #####extract sample IDs given as input in ################
  
  sample_names <- reactive({
    sampleID <- input$samples
    regex_sampleID <-
      paste0("^FORMAT$|",
             paste0("^",
                    sampleID,
                    collapse = "|",
                    sep = "$"))
    sample_info <-
      list(sampleID = sampleID,
           regex_sampleID = regex_sampleID)
    sample_info
  })
  
  ###### Render input ped file as data table #######
  output$phase3_ped_table <-
    DT::renderDataTable(DT::datatable(
      ped_read()$ped_input,
      options = list(
        searching = TRUE,
        pageLength = 10,
        rownames(NULL),
        scrollX = T,
        filter = "top"
      )
    ))
  
  
  ped_content <- eventReactive(input$checkdb, {
    ped_table <- ped_read()$ped_input
    ped_table <-
      table(factor(ped_table$Phenotype, levels = c('0', '1', '2', '3')))
    names(ped_table) <-
      c('Excluded', 'Unaffected', 'Affected', 'Carriers')
    #colnames(ped_table) = c("Status","Count")
    return(ped_table[which(ped_table >= 1)])
  })
  
  # update tabsetpanel on checkdb button click
  observeEvent(input$checkdb, {
    updateTabsetPanel(session, "ped_tab", 'samples')
  })
  
  
  # Render input ped as sample summary data table
  output$ped_summary <- renderTable({
    req(ped_content())
    ped_content()
  })
  
  output$ped_full_table <-
    DT::renderDataTable(DT::datatable(
      ped_read()$ped_input,
      options = list(
        searching = TRUE,
        pageLength = 10,
        rownames(NULL),
        scrollX = T
      )
    ))
  
  
  
  ####dynamically populate Population names in the input ped file to the UI############
  choices_populations <- reactive({
    ped_pop <- ped_read()
    populations <-
      as.character(unique(ped_pop$ped_input$Population))
    populations
  })
  
  observe({
    updateSelectizeInput(session = session,
                         inputId = "pop1",
                         choices = choices_populations())
    updateSelectizeInput(session = session,
                         inputId = "pop2",
                         choices = choices_populations())
    updateSelectizeInput(session = session,
                         inputId = "pop1count",
                         choices = choices_populations())
    updateSelectizeInput(session = session,
                         inputId = "pop2count",
                         choices = choices_populations())
  })
  
  choices_samples <- reactive({
    ped_samp <- ped_read()
    samples <- as.character(unique(ped_samp$ped_input$IndividualID))
    samples
  })
  
  observe({
    updateSelectizeInput(session = session,
                         inputId = "samples",
                         choices = choices_samples())
  })
  
  observe({
    updateSelectizeInput(session = session,
                         inputId = "names",
                         choices = choices_samples())
  })
  
  choices_gender <- reactive({
    ped_gen <- ped_read()
    gender <- as.character(unique(ped_gen$ped_input$Gender))
    gender
  })
  
  observe({
    updateSelectizeInput(session = session,
                         inputId = "pop1_gender",
                         choices = choices_gender())
    updateSelectizeInput(session = session,
                         inputId = "pop2_gender",
                         choices = choices_gender())
  })
  
  
  ####create PED sample DB once per user session######
      peddb_default <- system.file("extdata", "webgqt/data/1K.phase3.ped.db", package = "webGQT")


  
  
  peddb_content <- reactiveVal()
  observeEvent(input$createdb, {
    prog <- Progress$new(session)
    prog$set(message = "Creating sample database",
             detail = "This may take a while...",
             value = NULL)
    pedfut <- NULL
    
    peddb_files <- gqt_list()
    peddb_input <- ped_file()
    
    pedfut <<- future({
      system(
        paste(
          gqt_path,
          "convert ped -i",
          peddb_files$gz_new,
          "-p",
          peddb_input$ped_path,
          "-D",
          paste0(tmp_dir, "/", peddb_input$ped_base, ".db")
        )
      )
      peddb <-
        file.path(paste0(tmp_dir, "/", peddb_input$ped_base, ".db"))
      return(peddb)
    })  %...>%
      peddb_content() %>%
      finally(~ prog$close())
    NULL
  })
  
  
  #####switch to pedigree analysis module after creating sample database####
  observeEvent(req(peddb_content()), {
    newtab <- switch(input$tabs,
                     "pheno" = "dominantModel")
    updateTabItems(session, "tabs", newtab)
  })
  
  ###############################Dominant module##############################################
  dom_content <- reactiveVal()
  
  observeEvent(input$dom_run, {
    shinyjs::disable(input$dom_run)
    prog <- Progress$new(session)
    prog$set(message = "Dominant Analysis in progress",
             detail = "This may take a while...",
             value = NULL)
    fut1 <- NULL
    
    dom_files <- gqt_list()
    dom_ped <- ped_file()
    if (input$ped_rd == "1000 Genomes") {
      dom_peddb <- peddb_default
    }
    else if (input$ped_rd == "Upload PED" && input$createdb) {
      dom_peddb <- peddb_content()
    }
    
    domCases <- reactive({
      return(input$domCases)
    })
    domMAF <- reactive({
      return(input$domMAF)
    })
    
    domCases_value <- domCases()
    domMAF_value <- domMAF()
    dom_ped_nCar <- ped_read()
    fut1 <<- future({
      system(
        paste(
          gqt_path,
          'query -i',
          dom_files$gz_new,
          '-v -d',
          dom_peddb,
          "-p \"Phenotype=2\"",
          paste0('-g \"count(UNKNOWN)<=', domCases_value, '\"'),
          "-p \"Phenotype=2\"",
          "-g \"HET HOM_ALT\"",
          # "-p \"Phenotype=3\"",
          # paste0('-g \"count(HET) ==', dom_ped_nCar$ncarriers, '\"'),
          "-p \"Phenotype=1\"",
          paste0('-g \"maf()<=', domMAF_value, '\"'),
          ">",
          paste0(tmp_dir, "/dom.vcf.gz", sep = "")
        )
      )
      dom_vcf <- vcfR::read.vcfR(paste0(tmp_dir, "/dom.vcf.gz"))
      dom_vcf
    }) %...>%
      dom_content() %>%
      finally(~ prog$close())
    
    # Return something other than the promise so shiny remains responsive
    NULL
  })
  
  #####Split VCF to table##########################
  observeEvent(req(dom_content()),
               {
                 #shinyjs::enable(id = dom_run)
                 updateTabsetPanel(session, "dom_tab", "dom_results")
                 dom_vcfinput <- dom_content()
                 dom_pedinfo <- ped_read()
                 dom_vcf_gt <-
                   dom_vcfinput@gt[, grep(dom_pedinfo$regex_cases_carriers_names,
                                          colnames(dom_vcfinput@gt))]
                 dom_vcf_info <- vcfR::vcfR2tidy(dom_vcfinput)$fix
                 dom_vcf_info <-
                   dom_vcf_info[, c(2:ncol(dom_vcf_info))]
                 dom_gt_info <- cbind(dom_vcf_gt, dom_vcf_info)
                 
                 output$dom_table <-
                   DT::renderDataTable(DT::datatable(
                     dom_gt_info,
                     options = list(
                       searching = TRUE,
                       pageLength = 10,
                       rownames(NULL),
                       scrollX = T
                     )
                   ))
                 
                 #####summarize by ANNOVAR and snpEff annotation flags if exists in input VCF#####
                 dom_gene_summary <- function() {
                   annot_levels = c(
                     "Annotation",
                     "Func.ensGene",
                     "Func.genericGene",
                     "Func.refGene",
                     "Func.FEELncGene"
                   )
                   if ((nrow(dom_gt_info) != 0) &&
                       length(annot_levels[which(annot_levels %in% names(dom_gt_info))]) >= 1)
                   {
                     annot_levels = annot_levels[which(annot_levels %in% names(dom_gt_info))]
                     # dom_out_gene<- as.data.frame(extract_info_tidy(dom_vcfinput,info_fields = annot_levels))
                     dom_out_gene_table <-
                       as.data.frame.matrix(xtabs(~ values + ind,
                                                  stack(dom_gt_info, select = annot_levels)))
                     dom_out_gene_table_fix <-
                       setDT(dom_out_gene_table, keep.rownames = TRUE)
                     colnames(dom_out_gene_table_fix)[1] <- "Region"
                     return(dom_out_gene_table_fix)
                   }
                   else if ((nrow(dom_gt_info) != 0) &
                            length(annot_levels[which(annot_levels %in% names(dom_gt_info))]) == 0)
                   {
                     dom_out_gene_table_fix <- data.frame()
                     return(dom_out_gene_table_fix)
                   }
                   else if ((nrow(dom_gt_info) == 0) |
                            length(annot_levels[which(annot_levels %in% names(dom_gt_info))]) ==
                            0)
                   {
                     dom_out_gene_table_fix <- data.frame()
                     return(dom_out_gene_table_fix)
                   }
                 }
                 
                 ######code to print count and summary table in 'Summary'pane###########
                 
                 output$dom_count <-
                   renderText(paste("Total filtered variants:", nrow(unique(dom_gt_info[c("CHROM", "POS")]))))
                 
                 
                 output$dom_summary <-
                   renderTable(
                     dom_gene_summary(),
                     rownames = TRUE,
                     hover = TRUE,
                     striped = TRUE
                   )
                 
                 ###### code to plot variant summary#############
                 dom_plot <- function() {
                   if (nrow(dom_gene_summary()) > 1) {
                     domGenePlot <- dom_gene_summary()
                     dfm <-
                       melt(domGenePlot,
                            varnames = c(colnames(domGenePlot)),
                            id.vars = 1)
                     ggplot(dfm, aes(x = Region, y = value)) + geom_bar(aes(fill = variable),
                                                                        stat = "identity",
                                                                        position = "dodge") +
                       theme_bw() + theme(
                         panel.border = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank()
                       ) +
                       xlab("Functional category") + ylab("Variant count") +
                       guides(fill = guide_legend(title = "Gene Database")) +
                       theme(
                         axis.text = element_text(size = 12, face = "bold"),
                         axis.title = element_text(size = 14, face = "bold"),
                         axis.text.x = element_text(angle = 90, hjust = 1)
                       )
                   }
                   else if (nrow(dom_gene_summary()) <= 1) {
                     Variants <- 0
                     hist(
                       Variants,
                       xlab = "Functional category",
                       ylab = ("Variant count"),
                       main = NULL,
                       labels = FALSE
                     )
                   }
                 }
                 
                 
                 #####code to render and download png,txt and VCF output files########
                 
                 output$dom_plot <- renderPlot({
                   dom_plot()
                 })
                 
                 output$dom_downPlot <- downloadHandler(
                   filename = function() {
                     paste("output", '.png', sep = '')
                   },
                   content = function(file) {
                     ggsave(file, dom_plot())
                   }
                 )
                 
                 output$dom_downList <- downloadHandler(
                   filename = function() {
                     paste0("output", ".txt")
                   },
                   content = function(file) {
                     write.table(
                       dom_gt_info,
                       file,
                       row.names = FALSE,
                       sep = "\t",
                       quote = FALSE
                     )
                   }
                 )
                 
                 output$dom_VCFdownList <- downloadHandler(
                   filename = function() {
                     paste0("output", ".vcf")
                   },
                   content = function(file) {
                     write.vcf(dom_vcfinput, file)
                   }
                 )
               })
  
  observeEvent(input$dom_cancel, {
    stopMulticoreFuture(fut1)
  })
  
  
  
  ############################Recessive module##############################################
  rec_content <- reactiveVal()
  
  observeEvent(input$rec_run, {
    shinyjs::disable(input$rec_run)
    prog <- Progress$new(session)
    prog$set(message = "Recessive analysis in progress",
             detail = "Please do not refresh,This may take a while...",
             value = NULL)
    fut2 <- NULL
    
    rec_files <- gqt_list()
    rec_ped <- ped_file()
    if (input$ped_rd == "1000 Genomes") {
      rec_peddb <- peddb_default
    }
    else if (input$ped_rd == "Upload PED" && input$rec_run) {
      rec_peddb <- peddb_content()
    }
    
    recCases <- reactive({
      return(input$recCases)
    })
    recMAF <- reactive({
      return(input$recMAF)
    })
    
    recCases_value <- recCases()
    recMAF_value <- recMAF()
    rec_ped_nCar <- ped_read()
    
    fut2 <<- future({
      system(
        paste(
          gqt_path,
          'query -i',
          rec_files$gz_new,
          '-v -d',
          rec_peddb,
          "-p \"Phenotype=2\"",
          paste0('-g \"count(UNKNOWN)<=', recCases_value, '\"'),
          "-p \"Phenotype=2\"",
          "-g \"HOM_ALT\"",
          "-p \"Phenotype=3\"",
          paste0('-g \"count(HET) ==', rec_ped_nCar$ncarriers, '\"'),
          "-p \"Phenotype=1\"",
          paste0('-g \"maf()<=', recMAF_value, '\"'),
          ">",
          paste0(tmp_dir, "/rec.vcf.gz", sep = "")
        )
      )
      rec_vcf <- vcfR::read.vcfR(paste0(tmp_dir, "/rec.vcf.gz"))
      rec_vcf
    }) %...>%
      rec_content() %>%
      finally( ~ prog$close())
    
    # Return something other than the promise so shiny remains responsive
    NULL
  })
  
  #####Split VCF to table##########################
  observeEvent(req(rec_content()),
               {
                 #shinyjs::enable(id = rec_run)
                 updateTabsetPanel(session, "rec_tab", "rec_results")
                 rec_vcfinput <- rec_content()
                 rec_pedinfo <- ped_read()
                 rec_vcf_gt <-
                   rec_vcfinput@gt[, grep(rec_pedinfo$regex_cases_carriers_names,
                                          colnames(rec_vcfinput@gt))]
                 rec_vcf_info <- vcfR::vcfR2tidy(rec_vcfinput)$fix
                 rec_vcf_info <-
                   rec_vcf_info[, c(2:ncol(rec_vcf_info))]
                 rec_gt_info <- cbind(rec_vcf_gt, rec_vcf_info)
                 
                 output$rec_table <-
                   DT::renderDataTable(DT::datatable(
                     rec_gt_info,
                     options = list(
                       searching = TRUE,
                       pageLength = 10,
                       rownames(NULL),
                       scrollX = T
                     )
                   ))
                 
                 #####summarize by ANNOVAR and snpEff annotation flags if exists in input VCF#####
                 rec_gene_summary <- function() {
                   annot_levels = c(
                     "Annotation",
                     "Func.ensGene",
                     "Func.genericGene",
                     "Func.refGene",
                     "Func.FEELncGene"
                   )
                   if ((nrow(rec_gt_info) != 0) &&
                       length(annot_levels[which(annot_levels %in% names(rec_gt_info))]) >= 1)
                   {
                     annot_levels = annot_levels[which(annot_levels %in% names(rec_gt_info))]
                     
                     rec_out_gene_table <-
                       as.data.frame.matrix(xtabs(~ values + ind,
                                                  stack(rec_gt_info, select = annot_levels)))
                     rec_out_gene_table_fix <-
                       setDT(rec_out_gene_table, keep.rownames = TRUE)
                     colnames(rec_out_gene_table_fix)[1] <- "Region"
                     return(rec_out_gene_table_fix)
                   }
                   else if ((nrow(rec_gt_info) != 0) &
                            length(annot_levels[which(annot_levels %in% names(rec_gt_info))]) == 0)
                   {
                     rec_out_gene_table_fix <- data.frame()
                     return(rec_out_gene_table_fix)
                   }
                   else if ((nrow(rec_gt_info) == 0) |
                            length(annot_levels[which(annot_levels %in% names(rec_gt_info))]) ==
                            0)
                   {
                     rec_out_gene_table_fix <- data.frame()
                     return(rec_out_gene_table_fix)
                   }
                 }
                 
                 ######code to print count and summary table in 'Summary'pane###########
                 
                 output$rec_count <-
                   renderText(paste("Total filtered variants:", nrow(unique(rec_gt_info[c("CHROM", "POS")]))))
                 
                 
                 output$rec_summary <-
                   renderTable(
                     rec_gene_summary(),
                     rownames = TRUE,
                     hover = TRUE,
                     striped = TRUE
                   )
                 
                 ###### code to plot variant summary#############
                 rec_plot <- function() {
                   if (nrow(rec_gene_summary()) > 1) {
                     recGenePlot <- rec_gene_summary()
                     dfm <-
                       melt(recGenePlot,
                            varnames = c(colnames(recGenePlot)),
                            id.vars = 1)
                     ggplot(dfm, aes(x = Region, y = value)) + geom_bar(aes(fill = variable),
                                                                        stat = "identity",
                                                                        position = "dodge") +
                       theme_bw() + theme(
                         panel.border = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank()
                       ) +
                       xlab("Functional category") + ylab("Variant count") +
                       guides(fill = guide_legend(title = "Gene Database")) +
                       theme(
                         axis.text = element_text(size = 12, face = "bold"),
                         axis.title = element_text(size = 14, face = "bold"),
                         axis.text.x = element_text(angle = 90, hjust = 1)
                       )
                   }
                   else if (nrow(rec_gene_summary()) <= 1) {
                     Variants <- 0
                     hist(
                       Variants,
                       xlab = "Functional category",
                       ylab = ("Variant count"),
                       main = NULL,
                       labels = FALSE
                     )
                   }
                 }
                 
                 
                 #####code to render and download png,txt and VCF output files########
                 
                 output$rec_plot <- renderPlot({
                   rec_plot()
                 })
                 
                 output$rec_downPlot <- downloadHandler(
                   filename = function() {
                     paste("output", '.png', sep = '')
                   },
                   content = function(file) {
                     ggsave(file, rec_plot())
                   }
                 )
                 
                 output$rec_downList <- downloadHandler(
                   filename = function() {
                     paste0("output", ".txt")
                   },
                   content = function(file) {
                     write.table(
                       rec_gt_info,
                       file,
                       row.names = FALSE,
                       sep = "\t",
                       quote = FALSE
                     )
                   }
                 )
                 
                 output$rec_VCFdownList <- downloadHandler(
                   filename = function() {
                     paste0("output", ".vcf")
                   },
                   content = function(file) {
                     write.vcf(rec_vcfinput, file)
                   }
                 )
               })
  
  observeEvent(input$rec_cancel, {
    stopMulticoreFuture(fut2)
  })
  
  ############################Compound Heterogyzous variant module##############################################
  compHet_content <- reactiveVal()
  
  observeEvent(input$comp_run, {
    shinyjs::disable(input$comp_run)
    prog <- Progress$new(session)
    prog$set(message = "Filtering compound heterozygous variants",
             detail = "Please do not refresh,This may take a while...",
             value = NULL)
    fut3 <- NULL
    
    compHet_files <- gqt_list()
    compHet_ped <- ped_file()
    if (input$ped_rd == "1000 Genomes") {
      compHet_peddb <- peddb_default
    }
    else if (input$ped_rd == "Upload PED" && input$compHet_run) {
      compHet_peddb <- peddb_content()
    }
    
    compHetCases <- reactive({
      return(input$compHetCases)
    })
    compHetMAF <- reactive({
      return(input$compHetMAF)
    })
    
    compHetCases_value <- compHetCases()
    compHetMAF_value <- compHetMAF()
    #compHet_ped_nCar <- ped_read()
    
    fut3 <<- future({
      system(
        paste(
          gqt_path,
          'query -i',
          compHet_files$gz_new,
          '-v -d',
          compHet_peddb,
          "-p \"Phenotype=2\"",
          paste0('-g \"count(UNKNOWN)<=', compHetCases_value, '\"'),
          "-p \"Phenotype=2\"",
          "-g \"HET\"",
          "-p \"Phenotype=3\"",
          paste0('-g \"count(HET) ==1\"'),
          "-p \"Phenotype=3\"",
          paste0('-g \"count(HOM_REF) ==1\"'),
          "-p \"Phenotype=1\"",
          paste0('-g \"maf()<=', compHetMAF_value, '\"'),
          ">",
          paste0(tmp_dir, "/compHet.vcf.gz", sep = "")
        )
      )
      compHet_vcf <- vcfR::read.vcfR(paste0(tmp_dir, "/compHet.vcf.gz"))
      compHet_vcf
    }) %...>%
      compHet_content() %>%
      finally( ~ prog$close())
    
    # Return something other than the promise so shiny remains responsive
    NULL
  })
  
  #####Split VCF to table##########################
  observeEvent(req(compHet_content()),
               {
                 #shinyjs::enable(id = compHet_run)
                 updateTabsetPanel(session, "compHet_tab", "compHet_results")
                 compHet_vcfinput <- compHet_content()
                 compHet_pedinfo <- ped_read()
                 compHet_vcf_gt <-
                   compHet_vcfinput@gt[, grep(compHet_pedinfo$regex_cases_carriers_names,
                                              colnames(compHet_vcfinput@gt))]
                 compHet_vcf_info <- vcfR::vcfR2tidy(compHet_vcfinput)$fix
                 compHet_vcf_info <-
                   compHet_vcf_info[, c(2:ncol(compHet_vcf_info))]
                 compHet_gt_info <- cbind(compHet_vcf_gt, compHet_vcf_info)
                 
                 output$compHet_table <-
                   DT::renderDataTable(DT::datatable(
                     compHet_gt_info,
                     options = list(
                       searching = TRUE,
                       pageLength = 10,
                       rownames(NULL),
                       scrollX = T
                     )
                   ))
                 
                 #####summarize by ANNOVAR and snpEff annotation flags if exists in input VCF#####
                 compHet_gene_summary <- function() {
                   annot_levels = c(
                     "Annotation",
                     "Func.ensGene",
                     "Func.genericGene",
                     "Func.refGene",
                     "Func.FEELncGene"
                   )
                   if ((nrow(compHet_gt_info) != 0) &&
                       length(annot_levels[which(annot_levels %in% names(compHet_gt_info))]) >= 1)
                   {
                     annot_levels = annot_levels[which(annot_levels %in% names(compHet_gt_info))]
                     
                     compHet_out_gene_table <-
                       as.data.frame.matrix(xtabs(~ values + ind,
                                                  stack(compHet_gt_info, select = annot_levels)))
                     compHet_out_gene_table_fix <-
                       setDT(compHet_out_gene_table, keep.rownames = TRUE)
                     colnames(compHet_out_gene_table_fix)[1] <- "Region"
                     return(compHet_out_gene_table_fix)
                   }
                   else if ((nrow(compHet_gt_info) != 0) &
                            length(annot_levels[which(annot_levels %in% names(compHet_gt_info))]) == 0)
                   {
                     compHet_out_gene_table_fix <- data.frame()
                     return(compHet_out_gene_table_fix)
                   }
                   else if ((nrow(compHet_gt_info) == 0) |
                            length(annot_levels[which(annot_levels %in% names(compHet_gt_info))]) ==
                            0)
                   {
                     compHet_out_gene_table_fix <- data.frame()
                     return(compHet_out_gene_table_fix)
                   }
                 }
                 
                 ######code to print count and summary table in 'Summary'pane###########
                 
                 output$compHet_count <-
                   renderText(paste("Total filtered variants:", nrow(unique(compHet_gt_info[c("CHROM", "POS")]))))
                 
                 
                 output$compHet_summary <-
                   renderTable(
                     compHet_gene_summary(),
                     rownames = TRUE,
                     hover = TRUE,
                     striped = TRUE
                   )
                 
                 ###### code to plot variant summary#############
                 compHet_plot <- function() {
                   if (nrow(compHet_gene_summary()) > 1) {
                     compHetGenePlot <- compHet_gene_summary()
                     dfm <-
                       melt(compHetGenePlot,
                            varnames = c(colnames(compHetGenePlot)),
                            id.vars = 1)
                     ggplot(dfm, aes(x = Region, y = value)) + geom_bar(aes(fill = variable),
                                                                        stat = "identity",
                                                                        position = "dodge") +
                       theme_bw() + theme(
                         panel.border = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank()
                       ) +
                       xlab("Functional category") + ylab("Variant count") +
                       guides(fill = guide_legend(title = "Gene Database")) +
                       theme(
                         axis.text = element_text(size = 12, face = "bold"),
                         axis.title = element_text(size = 14, face = "bold"),
                         axis.text.x = element_text(angle = 90, hjust = 1)
                       )
                   }
                   else if (nrow(compHet_gene_summary()) <= 1) {
                     Variants <- 0
                     hist(
                       Variants,
                       xlab = "Functional category",
                       ylab = ("Variant count"),
                       main = NULL,
                       labels = FALSE
                     )
                   }
                 }
                 
                 
                 #####code to render and download png,txt and VCF output files########
                 
                 output$compHet_plot <- renderPlot({
                   compHet_plot()
                 })
                 
                 output$compHet_downPlot <- downloadHandler(
                   filename = function() {
                     paste("output", '.png', sep = '')
                   },
                   content = function(file) {
                     ggsave(file, compHet_plot())
                   }
                 )
                 
                 output$compHet_downList <- downloadHandler(
                   filename = function() {
                     paste0("output", ".txt")
                   },
                   content = function(file) {
                     write.table(
                       compHet_gt_info,
                       file,
                       row.names = FALSE,
                       sep = "\t",
                       quote = FALSE
                     )
                   }
                 )
                 
                 output$compHet_VCFdownList <- downloadHandler(
                   filename = function() {
                     paste0("output", ".vcf")
                   },
                   content = function(file) {
                     write.vcf(compHet_vcfinput, file)
                   }
                 )
               })
  
  observeEvent(input$compHet_cancel, {
    stopMulticoreFuture(fut3)
  })
  
  
  
  
  ###############################Dominant de novo module##############################################
  dom_denovo_content <- reactiveVal()
  
  observeEvent(input$dom_denovo_run, {
    shinyjs::disable(input$dom_denovo_run)
    prog <- Progress$new(session)
    prog$set(message = "Dominant de novo Analysis in progress",
             detail = "Please do not refresh, This may take a while...",
             value = NULL)
    fut4 <- NULL
    
    domDenovo_files <- gqt_list()
    domDenovo_ped <- ped_file()
    if (input$ped_rd == "1000 Genomes") {
      domDenovo_peddb <- peddb_default
    }
    else if (input$ped_rd == "Upload PED" &&
             input$dom_denovo_run) {
      domDenovo_peddb <- peddb_content()
    }
    
    domDenovoCases <- reactive({
      return(input$domDenovoCases)
    })
    domDenovoMAF <- reactive({
      return(input$domDenovoMAF)
    })
    
    domDenovoCases_value <- domDenovoCases()
    domDenovoMAF_value <- domDenovoMAF()
    domDenovo_ped_nCar <- ped_read()
    
    fut4 <<- future({
      system(
        paste(
          gqt_path,
          'query -i',
          domDenovo_files$gz_new,
          '-v -d',
          domDenovo_peddb,
          "-p \"Phenotype=2\"",
          paste0('-g \"count(UNKNOWN)<=', domDenovoCases_value, '\"'),
          "-p \"Phenotype=2\"",
          "-g \"HET\"",
          # "-p \"Phenotype=3\"",
          # paste0(
          #   '-g \"count(HOM_REF) ==',
          #   domDenovo_ped_nCar$ncarriers,
          #   '\"'
          # ),
          "-p \"Phenotype=1\"",
          paste0('-g \"maf()<=', domDenovoMAF_value, '\"'),
          ">",
          paste0(tmp_dir, "/dom.denovo.vcf.gz", sep = "")
        )
      )
      dom_denovo_vcf <-
        vcfR::read.vcfR(paste0(tmp_dir, "/dom.denovo.vcf.gz"))
      dom_denovo_vcf
    }) %...>%
      dom_denovo_content() %>%
      finally(~ prog$close())
    
    # Return something other than the promise so shiny remains responsive
    NULL
  })
  
  #####Split VCF to table##########################
  observeEvent(req(dom_denovo_content()),
               {
                 updateTabsetPanel(session, "dom_denovo_tab", "dom_denovo_results")
                 dom_denovo_vcfinput <- dom_denovo_content()
                 dom_denovo_pedinfo <- ped_read()
                 dom_denovo_vcf_gt <-
                   dom_denovo_vcfinput@gt[, grep(
                     dom_denovo_pedinfo$regex_cases_carriers_names,
                     colnames(dom_denovo_vcfinput@gt)
                   )]
                 dom_denovo_vcf_info <-
                   vcfR::vcfR2tidy(dom_denovo_vcfinput)$fix
                 dom_denovo_vcf_info <-
                   dom_denovo_vcf_info[, c(2:ncol(dom_denovo_vcf_info))]
                 dom_denovo_gt_info <-
                   cbind(dom_denovo_vcf_gt, dom_denovo_vcf_info)
                 
                 output$dom_denovo_table <-
                   DT::renderDataTable(DT::datatable(
                     dom_denovo_gt_info,
                     options = list(
                       searching = TRUE,
                       pageLength = 10,
                       rownames(NULL),
                       scrollX = T
                     )
                   ))
                 
                 #######summarize by ANNOVAR and snpEff annotation flags if exists in input VCF#####
                 dom_denovo_gene_summary <- function() {
                   annot_levels = c(
                     "Annotation",
                     "Func.ensGene",
                     "Func.genericGene",
                     "Func.refGene",
                     "Func.FEELncGene"
                   )
                   if ((nrow(dom_denovo_gt_info) != 0) &&
                       length(annot_levels[which(annot_levels %in% names(dom_denovo_gt_info))]) >= 1)
                   {
                     annot_levels = annot_levels[which(annot_levels %in% names(dom_denovo_gt_info))]
                     
                     dom_denovo_out_gene_table <-
                       as.data.frame.matrix(xtabs(
                         ~ values + ind,
                         stack(dom_denovo_gt_info, select = annot_levels)
                       ))
                     dom_denovo_out_gene_table_fix <-
                       setDT(dom_denovo_out_gene_table, keep.rownames = TRUE)
                     colnames(dom_denovo_out_gene_table_fix)[1] <-
                       "Region"
                     return(dom_denovo_out_gene_table_fix)
                   }
                   else if ((nrow(dom_denovo_gt_info) != 0) &
                            length(annot_levels[which(annot_levels %in% names(dom_denovo_gt_info))]) == 0)
                   {
                     dom_denovo_out_gene_table_fix <- data.frame()
                     return(dom_denovo_out_gene_table_fix)
                   }
                   else if ((nrow(dom_denovo_gt_info) == 0) |
                            length(annot_levels[which(annot_levels %in% names(dom_denovo_gt_info))]) ==
                            0)
                   {
                     dom_denovo_out_gene_table_fix <- data.frame()
                     return(dom_denovo_out_gene_table_fix)
                   }
                 }
                 
                 ######code to print count and summary table in 'Summary'pane###########
                 
                 output$dom_denovo_count <-
                   renderText(paste("Total filtered variants:", nrow(unique(
                     dom_denovo_gt_info[c("CHROM", "POS")]
                   ))))
                 
                 
                 output$dom_denovo_summary <-
                   renderTable(
                     dom_denovo_gene_summary(),
                     rownames = TRUE,
                     hover = TRUE,
                     striped = TRUE
                   )
                 
                 ###### code to plot variant summary#############
                 dom_denovo_plot <- function() {
                   if (nrow(dom_denovo_gene_summary()) > 1) {
                     domGenePlot <- dom_denovo_gene_summary()
                     dfm <-
                       melt(domGenePlot,
                            varnames = c(colnames(domGenePlot)),
                            id.vars = 1)
                     ggplot(dfm, aes(x = Region, y = value)) + geom_bar(aes(fill = variable),
                                                                        stat = "identity",
                                                                        position = "dodge") +
                       theme_bw() + theme(
                         panel.border = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank()
                       ) +
                       xlab("Functional category") + ylab("Variant count") +
                       guides(fill = guide_legend(title = "Gene Database")) +
                       theme(
                         axis.text = element_text(size = 12, face = "bold"),
                         axis.title = element_text(size = 14, face = "bold"),
                         axis.text.x = element_text(angle = 90, hjust = 1)
                       )
                   }
                   else if (nrow(dom_denovo_gene_summary()) <= 1) {
                     Variants <- 0
                     hist(
                       Variants,
                       xlab = "Functional category",
                       ylab = ("Variant count"),
                       main = NULL,
                       labels = FALSE
                     )
                   }
                 }
                 
                 
                 #####code to render and download png,txt and VCF output files########
                 
                 output$dom_denovo_plot <- renderPlot({
                   dom_denovo_plot()
                 })
                 
                 output$dom_denovo_downPlot <- downloadHandler(
                   filename = function() {
                     paste("output", '.png', sep = '')
                   },
                   content = function(file) {
                     ggsave(file, dom_denovo_plot())
                   }
                 )
                 
                 output$dom_denovo_downList <- downloadHandler(
                   filename = function() {
                     paste0("output", ".txt")
                   },
                   content = function(file) {
                     write.table(
                       dom_denovo_gt_info,
                       file,
                       row.names = FALSE,
                       sep = "\t",
                       quote = FALSE
                     )
                   }
                 )
                 
                 output$dom_denovo_VCFdownList <- downloadHandler(
                   filename = function() {
                     paste0("output", ".vcf")
                   },
                   content = function(file) {
                     write.vcf(dom_denovo_vcfinput, file)
                   }
                 )
               })
  
  observeEvent(input$dom_denovo_cancel, {
    stopMulticoreFuture(fut4)
  })
  
  ###############################recessive de novo module##############################################
  rec_denovo_content <- reactiveVal()
  
  observeEvent(input$rec_denovo_run, {
    shinyjs::disable(input$rec_denovo_run)
    prog <- Progress$new(session)
    prog$set(message = "Recessive de novo Analysis in progress",
             detail = "Please do not refresh, This may take a while...",
             value = NULL)
    fut5 <- NULL
    
    recDenovo_files <- gqt_list()
    recDenovo_ped <- ped_file()
    if (input$ped_rd == "1000 Genomes") {
      recDenovo_peddb <- peddb_default
    }
    else if (input$ped_rd == "Upload PED" &&
             input$rec_denovo_run) {
      recDenovo_peddb <- peddb_content()
    }
    
    recDenovoCases <- reactive({
      return(input$recDenovoCases)
    })
    recDenovoMAF <- reactive({
      return(input$recDenovoMAF)
    })
    
    recDenovoCases_value <- recDenovoCases()
    recDenovoMAF_value <- recDenovoMAF()
    recDenovo_ped_nCar <- ped_read()
    
    fut5 <<- future({
      system(
        paste(
          gqt_path,
          'query -i',
          recDenovo_files$gz_new,
          '-v -d',
          recDenovo_peddb,
          "-p \"Phenotype=2\"",
          paste0('-g \"count(UNKNOWN)<=',
                 recDenovoCases_value,
                 '\"'),
          "-p \"Phenotype=2\"",
          "-g \"HOM_ALT\"",
          "-p \"Phenotype=3\"",
          paste0(
            '-g \"count(HOM_REF) ==',
            recDenovo_ped_nCar$ncarriers,
            '\"'
          ),
          "-p \"Phenotype=1\"",
          paste0('-g \"maf()<=', recDenovoMAF_value, '\"'),
          ">",
          paste0(tmp_dir, "/rec.denovo.vcf.gz", sep = "")
        )
      )
      rec_denovo_vcf <-
        vcfR::read.vcfR(paste0(tmp_dir, "/rec.denovo.vcf.gz"))
      rec_denovo_vcf
    }) %...>%
      rec_denovo_content() %>%
      finally(~ prog$close())
    
    # Return something other than the promise so shiny remains responsive
    NULL
  })
  
  #####Split VCF to table##########################
  observeEvent(req(rec_denovo_content()),
               {
                 updateTabsetPanel(session, "rec_denovo_tab", "rec_denovo_results")
                 rec_denovo_vcfinput <- rec_denovo_content()
                 rec_denovo_pedinfo <- ped_read()
                 rec_denovo_vcf_gt <-
                   rec_denovo_vcfinput@gt[, grep(
                     rec_denovo_pedinfo$regex_cases_carriers_names,
                     colnames(rec_denovo_vcfinput@gt)
                   )]
                 rec_denovo_vcf_info <-
                   vcfR::vcfR2tidy(rec_denovo_vcfinput)$fix
                 rec_denovo_vcf_info <-
                   rec_denovo_vcf_info[, c(2:ncol(rec_denovo_vcf_info))]
                 rec_denovo_gt_info <-
                   cbind(rec_denovo_vcf_gt, rec_denovo_vcf_info)
                 
                 output$rec_denovo_table <-
                   DT::renderDataTable(DT::datatable(
                     rec_denovo_gt_info,
                     options = list(
                       searching = TRUE,
                       pageLength = 10,
                       rownames(NULL),
                       scrollX = T
                     )
                   ))
                 
                 #######summarize by ANNOVAR and snpEff annotation flags if exists in input VCF#####
                 rec_denovo_gene_summary <- function() {
                   annot_levels = c(
                     "Annotation",
                     "Func.ensGene",
                     "Func.genericGene",
                     "Func.refGene",
                     "Func.FEELncGene"
                   )
                   if ((nrow(rec_denovo_gt_info) != 0) &&
                       length(annot_levels[which(annot_levels %in% names(rec_denovo_gt_info))]) >= 1)
                   {
                     annot_levels = annot_levels[which(annot_levels %in% names(rec_denovo_gt_info))]
                     
                     rec_denovo_out_gene_table <-
                       as.data.frame.matrix(xtabs(
                         ~ values + ind,
                         stack(rec_denovo_gt_info, select = annot_levels)
                       ))
                     rec_denovo_out_gene_table_fix <-
                       setDT(rec_denovo_out_gene_table, keep.rownames = TRUE)
                     colnames(rec_denovo_out_gene_table_fix)[1] <-
                       "Region"
                     return(rec_denovo_out_gene_table_fix)
                   }
                   else if ((nrow(rec_denovo_gt_info) != 0) &
                            length(annot_levels[which(annot_levels %in% names(rec_denovo_gt_info))]) == 0)
                   {
                     rec_denovo_out_gene_table_fix <- data.frame()
                     return(rec_denovo_out_gene_table_fix)
                   }
                   else if ((nrow(rec_denovo_gt_info) == 0) |
                            length(annot_levels[which(annot_levels %in% names(rec_denovo_gt_info))]) ==
                            0)
                   {
                     rec_denovo_out_gene_table_fix <- data.frame()
                     return(rec_denovo_out_gene_table_fix)
                   }
                 }
                 
                 ######code to print count and summary table in 'Summary'pane###########
                 
                 output$rec_denovo_count <-
                   renderText(paste("Total filtered variants:", nrow(unique(
                     rec_denovo_gt_info[c("CHROM", "POS")]
                   ))))
                 
                 
                 output$rec_denovo_summary <-
                   renderTable(
                     rec_denovo_gene_summary(),
                     rownames = TRUE,
                     hover = TRUE,
                     striped = TRUE
                   )
                 
                 ###### code to plot variant summary#############
                 rec_denovo_plot <- function() {
                   if (nrow(rec_denovo_gene_summary()) > 1) {
                     recGenePlot <- rec_denovo_gene_summary()
                     dfm <-
                       melt(recGenePlot,
                            varnames = c(colnames(recGenePlot)),
                            id.vars = 1)
                     ggplot(dfm, aes(x = Region, y = value)) + geom_bar(aes(fill = variable),
                                                                        stat = "identity",
                                                                        position = "dodge") +
                       theme_bw() + theme(
                         panel.border = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank()
                       ) +
                       xlab("Functional category") + ylab("Variant count") +
                       guides(fill = guide_legend(title = "Gene Database")) +
                       theme(
                         axis.text = element_text(size = 12, face = "bold"),
                         axis.title = element_text(size = 14, face = "bold"),
                         axis.text.x = element_text(angle = 90, hjust = 1)
                       )
                   }
                   else if (nrow(rec_denovo_gene_summary()) <= 1) {
                     Variants <- 0
                     hist(
                       Variants,
                       xlab = "Functional category",
                       ylab = ("Variant count"),
                       main = NULL,
                       labels = FALSE
                     )
                   }
                 }
                 
                 
                 #####code to render and download png,txt and VCF output files########
                 
                 output$rec_denovo_plot <- renderPlot({
                   rec_denovo_plot()
                 })
                 
                 output$rec_denovo_downPlot <- downloadHandler(
                   filename = function() {
                     paste("output", '.png', sep = '')
                   },
                   content = function(file) {
                     ggsave(file, rec_denovo_plot())
                   }
                 )
                 
                 output$rec_denovo_downList <- downloadHandler(
                   filename = function() {
                     paste0("output", ".txt")
                   },
                   content = function(file) {
                     write.table(
                       rec_denovo_gt_info,
                       file,
                       row.names = FALSE,
                       sep = "\t",
                       quote = FALSE
                     )
                   }
                 )
                 
                 output$rec_denovo_VCFdownList <- downloadHandler(
                   filename = function() {
                     paste0("output", ".vcf")
                   },
                   content = function(file) {
                     write.vcf(rec_denovo_vcfinput, file)
                   }
                 )
               })
  
  observeEvent(input$rec_denovo_cancel, {
    stopMulticoreFuture(fut5)
  })
  
  
  ####################Case specific module################
  
  case_specific_content <- reactiveVal()
  
  observeEvent(input$case_specific_run, {
    prog <- Progress$new(session)
    prog$set(message = "Case-specific analysis in progress",
             detail = "Please do not refresh,This may take a while...",
             value = NULL)
    fut6 <- NULL
    
    cs_files <- gqt_list()
    cs_ped <- ped_file()
    if (input$ped_rd == "1000 Genomes") {
      cs_peddb <- peddb_default
    }
    else if (input$ped_rd == "Upload PED" &&
             input$case_specific_run) {
      cs_peddb <- peddb_content()
    }
    cs_missing_cases <- reactive({
      return(input$cs_missing_cases)
    })
    case_specific_vartype <- reactive({
      return(input$case_specific_vartype)
    })
    
    cs_missing_cases_value <- cs_missing_cases()
    case_specific_vartype_value <- case_specific_vartype()
    
    fut6 <<- future({
      system(
        paste(
          gqt_path,
          'query -i',
          cs_files$gz_new,
          '-v -d',
          cs_peddb,
          "-p \"Phenotype=2\"",
          paste0('-g \"count(UNKNOWN)<=',
                 cs_missing_cases_value,
                 '\"'),
          "-p \"Phenotype=2\"",
          paste0('-g \"', case_specific_vartype_value, '\"'),
          "-p \"Phenotype=1\"",
          "-g \"maf()==0\"",
          ">",
          paste0(tmp_dir, "/case.specific.vcf.gz", sep = "")
        )
      )
      case_specific_vcf <-
        vcfR::read.vcfR(paste0(tmp_dir, "/case.specific.vcf.gz"))
      case_specific_vcf
    }) %...>%
      case_specific_content() %>%
      finally( ~ prog$close())
    NULL
  })
  
  
  
  #####Split VCF to table##########################
  observeEvent(req(case_specific_content()),
               {
                 updateTabsetPanel(session,
                                   "case_specific_tab",
                                   "case_specific_results")
                 case_specific_vcfinput <- case_specific_content()
                 case_specific_pedinfo <- ped_read()
                 case_specific_vcf_gt <-
                   case_specific_vcfinput@gt[, grep(
                     case_specific_pedinfo$regex_cases_carriers_names,
                     colnames(case_specific_vcfinput@gt)
                   )]
                 case_specific_vcf_info <-
                   vcfR::vcfR2tidy(case_specific_vcfinput)$fix
                 case_specific_vcf_info <-
                   case_specific_vcf_info[, c(2:ncol(case_specific_vcf_info))]
                 case_specific_gt_info <-
                   cbind(case_specific_vcf_gt, case_specific_vcf_info)
                 
                 output$case_specific_table <-
                   DT::renderDataTable(DT::datatable(
                     case_specific_gt_info,
                     options = list(
                       searching = TRUE,
                       pageLength = 10,
                       rownames(NULL),
                       scrollX = T
                     )
                   ))
                 
                 #######summarize by ANNOVAR and snpEff annotation flags if exists in input VCF#####
                 case_specific_gene_summary <- function() {
                   annot_levels = c(
                     "Annotation",
                     "Func.ensGene",
                     "Func.genericGene",
                     "Func.refGene",
                     "Func.FEELncGene"
                   )
                   if ((nrow(case_specific_gt_info) != 0) &&
                       length(annot_levels[which(annot_levels %in% names(case_specific_gt_info))]) >= 1)
                   {
                     annot_levels = annot_levels[which(annot_levels %in% names(case_specific_gt_info))]
                     
                     case_specific_out_gene_table <-
                       as.data.frame.matrix(xtabs(
                         ~ values + ind,
                         stack(case_specific_gt_info, select = annot_levels)
                       ))
                     case_specific_out_gene_table_fix <-
                       setDT(case_specific_out_gene_table, keep.rownames = TRUE)
                     colnames(case_specific_out_gene_table_fix)[1] <-
                       "Region"
                     return(case_specific_out_gene_table_fix)
                   }
                   else if ((nrow(case_specific_gt_info) != 0) &
                            length(annot_levels[which(annot_levels %in% names(case_specific_gt_info))]) == 0)
                   {
                     case_specific_out_gene_table_fix <- data.frame()
                     return(case_specific_out_gene_table_fix)
                   }
                   else if ((nrow(case_specific_gt_info) == 0) |
                            length(annot_levels[which(annot_levels %in% names(case_specific_gt_info))]) ==
                            0)
                   {
                     case_specific_out_gene_table_fix <- data.frame()
                     return(case_specific_out_gene_table_fix)
                   }
                 }
                 
                 ######code to print count and summary table in 'Summary'pane###########
                 
                 output$case_specific_count <-
                   renderText(paste("Total filtered variants:", nrow(unique(
                     case_specific_gt_info[c("CHROM", "POS")]
                   ))))
                 
                 
                 output$case_specific_summary <-
                   renderTable(
                     case_specific_gene_summary(),
                     rownames = TRUE,
                     hover = TRUE,
                     striped = TRUE
                   )
                 
                 ###### code to plot variant summary#############
                 case_specific_plot <- function() {
                   if (nrow(case_specific_gene_summary()) > 1) {
                     recGenePlot <- case_specific_gene_summary()
                     dfm <-
                       melt(recGenePlot,
                            varnames = c(colnames(recGenePlot)),
                            id.vars = 1)
                     ggplot(dfm, aes(x = Region, y = value)) + geom_bar(aes(fill = variable),
                                                                        stat = "identity",
                                                                        position = "dodge") +
                       theme_bw() + theme(
                         panel.border = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank()
                       ) +
                       xlab("Functional category") + ylab("Variant count") +
                       guides(fill = guide_legend(title = "Gene Database")) +
                       theme(
                         axis.text = element_text(size = 12, face = "bold"),
                         axis.title = element_text(size = 14, face = "bold"),
                         axis.text.x = element_text(angle = 90, hjust = 1)
                       )
                   }
                   else if (nrow(case_specific_gene_summary()) <= 1) {
                     Variants <- 0
                     hist(
                       Variants,
                       xlab = "Functional category",
                       ylab = ("Variant count"),
                       main = NULL,
                       labels = FALSE
                     )
                   }
                 }
                 
                 
                 #####code to render and download png,txt and VCF output files########
                 
                 output$case_specific_plot <- renderPlot({
                   case_specific_plot()
                 })
                 
                 output$case_specific_downPlot <- downloadHandler(
                   filename = function() {
                     paste("output", '.png', sep = '')
                   },
                   content = function(file) {
                     ggsave(file, case_specific_plot())
                   }
                 )
                 
                 output$case_specific_downList <- downloadHandler(
                   filename = function() {
                     paste0("output", ".txt")
                   },
                   content = function(file) {
                     write.table(
                       case_specific_gt_info,
                       file,
                       row.names = FALSE,
                       sep = "\t",
                       quote = FALSE
                     )
                   }
                 )
                 
                 output$case_specific_VCFdownList <-
                   downloadHandler(
                     filename = function() {
                       paste0("output", ".vcf")
                     },
                     content = function(file) {
                       write.vcf(case_specific_vcfinput, file)
                     }
                   )
               })
  
  observeEvent(input$case_specific_cancel, {
    stopMulticoreFuture(fut6)
  })
  
  
  ####################Cases shared module################
  
  case_shared_content <- reactiveVal()
  
  observeEvent(input$case_shared_run, {
    prog <- Progress$new(session)
    prog$set(message = "Cases shared analysis in progress",
             detail = "Please do not refresh,This may take a while...",
             value = NULL)
    fut7 <- NULL
    
    shared_files <- gqt_list()
    shared_ped <- ped_file()
    if (input$ped_rd == "1000 Genomes") {
      shared_peddb <- peddb_default
    }
    else if (input$ped_rd == "Upload PED" &&
             input$case_shared_run) {
      shared_peddb <- peddb_content()
    }
    case_shared_vartype <- reactive({
      return(input$case_shared_vartype)
    })
    case_shared_count <- reactive({
      return(input$case_shared_count)
    })
    sharedMAF <- reactive({
      return(input$sharedMAF)
    })
    
    case_shared_vartype_value <- case_shared_vartype()
    case_shared_count_value <- case_shared_count()
    sharedMAF_value <- sharedMAF()
    
    fut7 <<- future({
      system(
        paste(
          gqt_path,
          'query -i',
          shared_files$gz_new,
          '-v -d',
          shared_peddb,
          "-p \"Phenotype=2\"",
          paste0(
            '-g \"count(',
            case_shared_vartype_value,
            ")",
            ">=",
            case_shared_count_value,
            "\""
          ),
          "-p \"Phenotype=1\"",
          paste0('-g \"maf()<=', sharedMAF_value, '\"'),
          ">",
          paste0(tmp_dir, "/case.shared.vcf.gz", sep = "")
        )
      )
      case_shared_vcf <-
        vcfR::read.vcfR(paste0(tmp_dir, "/case.shared.vcf.gz"))
      case_shared_vcf
    }) %...>%
      case_shared_content() %>%
      finally( ~ prog$close())
    NULL
  })
  
  
  #####Split VCF to table##########################
  observeEvent(req(case_shared_content()),
               {
                 updateTabsetPanel(session, "case_shared_tab", "case_shared_results")
                 case_shared_vcfinput <- case_shared_content()
                 case_shared_pedinfo <- ped_read()
                 case_shared_vcf_gt <-
                   case_shared_vcfinput@gt[, grep(
                     case_shared_pedinfo$regex_cases_carriers_names,
                     colnames(case_shared_vcfinput@gt)
                   )]
                 case_shared_vcf_info <-
                   vcfR::vcfR2tidy(case_shared_vcfinput)$fix
                 case_shared_vcf_info <-
                   case_shared_vcf_info[, c(2:ncol(case_shared_vcf_info))]
                 case_shared_gt_info <-
                   cbind(case_shared_vcf_gt, case_shared_vcf_info)
                 
                 output$case_shared_table <-
                   DT::renderDataTable(DT::datatable(
                     case_shared_gt_info,
                     options = list(
                       searching = TRUE,
                       pageLength = 10,
                       rownames(NULL),
                       scrollX = T
                     )
                   ))
                 
                 #######summarize by ANNOVAR and snpEff annotation flags if exists in input VCF#####
                 case_shared_gene_summary <- function() {
                   annot_levels = c(
                     "Annotation",
                     "Func.ensGene",
                     "Func.genericGene",
                     "Func.refGene",
                     "Func.FEELncGene"
                   )
                   if ((nrow(case_shared_gt_info) != 0) &&
                       length(annot_levels[which(annot_levels %in% names(case_shared_gt_info))]) >= 1)
                   {
                     annot_levels = annot_levels[which(annot_levels %in% names(case_shared_gt_info))]
                     
                     case_shared_out_gene_table <-
                       as.data.frame.matrix(xtabs(
                         ~ values + ind,
                         stack(case_shared_gt_info, select = annot_levels)
                       ))
                     case_shared_out_gene_table_fix <-
                       setDT(case_shared_out_gene_table, keep.rownames = TRUE)
                     colnames(case_shared_out_gene_table_fix)[1] <-
                       "Region"
                     return(case_shared_out_gene_table_fix)
                   }
                   else if ((nrow(case_shared_gt_info) != 0) &
                            length(annot_levels[which(annot_levels %in% names(case_shared_gt_info))]) == 0)
                   {
                     case_shared_out_gene_table_fix <- data.frame()
                     return(case_shared_out_gene_table_fix)
                   }
                   else if ((nrow(case_shared_gt_info) == 0) |
                            length(annot_levels[which(annot_levels %in% names(case_shared_gt_info))]) ==
                            0)
                   {
                     case_shared_out_gene_table_fix <- data.frame()
                     return(case_shared_out_gene_table_fix)
                   }
                 }
                 
                 ######code to print count and summary table in 'Summary'pane###########
                 
                 output$case_shared_count <-
                   renderText(paste("Total filtered variants:", nrow(unique(
                     case_shared_gt_info[c("CHROM", "POS")]
                   ))))
                 
                 
                 output$case_shared_summary <-
                   renderTable(
                     case_shared_gene_summary(),
                     rownames = TRUE,
                     hover = TRUE,
                     striped = TRUE
                   )
                 
                 ###### code to plot variant summary#############
                 case_shared_plot <- function() {
                   if (nrow(case_shared_gene_summary()) > 1) {
                     recGenePlot <- case_shared_gene_summary()
                     dfm <-
                       melt(recGenePlot,
                            varnames = c(colnames(recGenePlot)),
                            id.vars = 1)
                     ggplot(dfm, aes(x = Region, y = value)) + geom_bar(aes(fill = variable),
                                                                        stat = "identity",
                                                                        position = "dodge") +
                       theme_bw() + theme(
                         panel.border = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank()
                       ) +
                       xlab("Functional category") + ylab("Variant count") +
                       guides(fill = guide_legend(title = "Gene Database")) +
                       theme(
                         axis.text = element_text(size = 12, face = "bold"),
                         axis.title = element_text(size = 14, face = "bold"),
                         axis.text.x = element_text(angle = 90, hjust = 1)
                       )
                   }
                   else if (nrow(case_shared_gene_summary()) <= 1) {
                     Variants <- 0
                     hist(
                       Variants,
                       xlab = "Functional category",
                       ylab = ("Variant count"),
                       main = NULL,
                       labels = FALSE
                     )
                   }
                 }
                 
                 
                 #####code to render and download png,txt and VCF output files########
                 
                 output$case_shared_plot <- renderPlot({
                   case_shared_plot()
                 })
                 
                 output$case_shared_downPlot <- downloadHandler(
                   filename = function() {
                     paste("output", '.png', sep = '')
                   },
                   content = function(file) {
                     ggsave(file, case_shared_plot())
                   }
                 )
                 
                 output$case_shared_downList <- downloadHandler(
                   filename = function() {
                     paste0("output", ".txt")
                   },
                   content = function(file) {
                     write.table(
                       case_shared_gt_info,
                       file,
                       row.names = FALSE,
                       sep = "\t",
                       quote = FALSE
                     )
                   }
                 )
                 
                 output$case_shared_VCFdownList <- downloadHandler(
                   filename = function() {
                     paste0("output", ".vcf")
                   },
                   content = function(file) {
                     write.vcf(case_shared_vcfinput, file)
                   }
                 )
               })
  
  observeEvent(input$case_shared_cancel, {
    stopMulticoreFuture(fut7)
  })
  
  ####################population filter by individual count module################
  popcount_content <- reactiveVal()
  
  observeEvent(input$popcount_run, {
    prog <- Progress$new(session)
    prog$set(message = "Population analysis by individual count in progress",
             detail = "Please do not refresh,This may take a while...",
             value = NULL)
    fut8 <- NULL
    
    popcount_files <- gqt_list()
    popcount_ped <- ped_file()
    if (input$ped_rd == "1000 Genomes") {
      popcount_peddb <- peddb_default
    }
    else if (input$ped_rd == "Upload PED" &&
             input$popcount_run) {
      popcount_peddb <- peddb_content()
    }
    pop1count <- reactive({
      return(input$pop1count)
    })
    pop2count <- reactive({
      return(input$pop2count)
    })
    
    pop1_vartype <- reactive({
      return(input$pop1_vartype)
    })
    pop2_vartype <- reactive({
      return(input$pop2_vartype)
    })
    
    pop1count_min <- reactive({
      return(input$pop1count_min)
    })
    pop2count_max <- reactive({
      return(input$pop2count_max)
    })
    
    pop1count_value <- pop1count()
    pop2count_value <- pop2count()
    pop1_vartype_value <- pop1_vartype()
    pop2_vartype_value <- pop2_vartype()
    pop1count_min_value <- pop1count_min()
    pop2count_max_value <- pop2count_max()
    
    fut8 <<- future({
      system(
        paste(
          gqt_path,
          'query -i',
          popcount_files$gz_new,
          '-v -d',
          popcount_peddb,
          paste0('-p \"Population in (',
                 paste0(
                   "'", paste(pop1count_value, collapse = "','"), "'"
                 ),
                 ")",
                 '\"'),
          
          paste0(
            '-g \"pct(',
            pop1_vartype_value,
            ")",
            ">=",
            pop1count_min_value,
            "\""
          ),
          
          paste0('-p \"Population in (',
                 paste0(
                   "'", paste(pop2count_value, collapse = "','"), "'"
                 ),
                 ")",
                 '\"'),
          
          paste0(
            '-g \"pct(',
            pop2_vartype_value,
            ")",
            "<=",
            pop2count_max_value,
            "\""
          ),
          ">",
          paste0(tmp_dir, "/popcount.vcf.gz", sep = "")
        )
      )
      popcount_vcf <-
        vcfR::read.vcfR(paste0(tmp_dir, "/popcount.vcf.gz"))
      #popcount_vcf
    }) %...>%
      popcount_content() %>%
      finally( ~ prog$close())
    NULL
  })
  
  #####Split vcf to table###########
  observeEvent(req(popcount_content()), {
    updateTabsetPanel(session, "popcount_tab", "popcount_results")
    #popcount_vcfinput <- vcfR::read.vcfR(paste0(tmp_dir, "/popcount.vcf.gz"))
    popcount_vcfinput <- popcount_content()
    #popcount_gt_info <- vcfR::vcfR2tidy(popcount_vcfinput)$fix
    my_INFOs <- grep("INFO", queryMETA(popcount_vcfinput), value = TRUE)
    my_INFOs <- sub("INFO=ID=", "", my_INFOs)

    my_INFOm <-
      matrix(unlist(lapply(my_INFOs, function(x) {
        extract.info(popcount_vcfinput, element = x)
      })),
      ncol = length(my_INFOs),
      byrow = FALSE)

    colnames(my_INFOm) = as.character(my_INFOs)
    popcount_gt_info <- cbind(getFIX(popcount_vcfinput), data.frame(my_INFOm,stringsAsFactors=FALSE))

    
    output$popcount_table <-
      DT::renderDataTable(DT::datatable(
        popcount_gt_info,
        options = list(
          searching = TRUE,
          pageLength = 10,
          rownames(NULL),
          scrollX = T
        )
      ))
    
    #######summarize by ANNOVAR and snpEff annotation flags if exists in input VCF#####
    popcount_gene_summary <- function() {
      annot_levels = c(
        "Annotation",
        "Func.ensGene",
        "Func.genericGene",
        "Func.refGene",
        "Func.knownGene",
        "Func.FEELncGene"
      )
      if ((nrow(popcount_gt_info) != 0) &&
          length(annot_levels[which(annot_levels %in% names(popcount_gt_info))]) >= 1)
      {
        annot_levels = annot_levels[which(annot_levels %in% names(popcount_gt_info))]
        
        popcount_out_gene_table <-
          as.data.frame.matrix(xtabs(
            ~ values + ind,
            stack(popcount_gt_info, select = annot_levels)
          ))
        popcount_out_gene_table_fix <-
          setDT(popcount_out_gene_table, keep.rownames = TRUE)
        colnames(popcount_out_gene_table_fix)[1] <- "Region"
        return(popcount_out_gene_table_fix)
      }
      else if ((nrow(popcount_gt_info) != 0) &
               length(annot_levels[which(annot_levels %in% names(popcount_gt_info))]) == 0)
      {
        popcount_out_gene_table_fix <- data.frame()
        return(popcount_out_gene_table_fix)
      }
      else if ((nrow(popcount_gt_info) == 0) |
               length(annot_levels[which(annot_levels %in% names(popcount_gt_info))]) ==
               0)
      {
        popcount_out_gene_table_fix <- data.frame()
        return(popcount_out_gene_table_fix)
      }
    }
    
    ######code to print count and summary table in 'Summary'pane###########
    
    output$popcount_count <-
      renderText(paste("Total filtered variants:", nrow(unique(
        popcount_gt_info[c("CHROM", "POS")]
      ))))
    
    
    output$popcount_summary <-
      renderTable(
        popcount_gene_summary(),
        rownames = TRUE,
        hover = TRUE,
        striped = TRUE
      )
    
    ###### code to plot variant summary#############
    popcount_plot <- function() {
      if (nrow(popcount_gene_summary()) > 1) {
        popcountGenePlot <- popcount_gene_summary()
        dfm <-
          melt(popcountGenePlot,
               varnames = c(colnames(popcountGenePlot)),
               id.vars = 1)
        ggplot(dfm, aes(x = Region, y = value)) + geom_bar(aes(fill = variable),
                                                           stat = "identity",
                                                           position = "dodge") +
          theme_bw() + theme(
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
          ) +
          xlab("Functional category") + ylab("Variant count") +
          guides(fill = guide_legend(title = "Gene Database")) +
          theme(
            axis.text = element_text(size = 12, face = "bold"),
            axis.title = element_text(size = 14, face = "bold"),
            axis.text.x = element_text(angle = 90, hjust = 1)
          )
      }
      else if (nrow(popcount_gene_summary()) <= 1) {
        Variants <- 0
        hist(
          Variants,
          xlab = "Functional category",
          ylab = ("Variant count"),
          main = NULL,
          labels = FALSE
        )
      }
    }
    
    
    #####code to render and download png,txt and VCF output files########
    
    output$popcount_plot <- renderPlot({
      popcount_plot()
    })
    
    output$popcount_downPlot <- downloadHandler(
      filename = function() {
        paste("output", '.png', sep = '')
      },
      content = function(file) {
        ggsave(file, popcount_plot())
      }
    )
    
    output$popcount_downList <- downloadHandler(
      filename = function() {
        paste0("output", ".txt")
      },
      content = function(file) {
        write.table(
          popcount_gt_info,
          file,
          row.names = FALSE,
          sep = "\t",
          quote = FALSE
        )
      }
    )
    
    output$popcount_VCFdownList <- downloadHandler(
      filename = function() {
        paste0("output", ".vcf")
      },
      content = function(file) {
        write.vcf(popcount_vcfinput, file)
      }
    )
  })
  
  observeEvent(input$popcount_cancel, {
    stopMulticoreFuture(fut8)
  })
  
  
  ####################population filter module by MAF ################
  
  pop_content <- reactiveVal()
  
  observeEvent(input$pop_run, {
    prog <- Progress$new(session)
    prog$set(message = "Population analysis by MAF filter in progress",
             detail = "Please do not refresh,This may take a while...",
             value = NULL)
    fut9 <- NULL
    
    pop_files <- gqt_list()
    pop_ped <- ped_file()
    if (input$ped_rd == "1000 Genomes") {
      pop_peddb <- peddb_default
    }
    else if (input$ped_rd == "Upload PED" && input$pop_run) {
      pop_peddb <- peddb_content()
    }
    pop1 <- reactive({
      return(input$pop1)
    })
    pop2 <- reactive({
      return(input$pop2)
    })
    
    pop1_gender <- reactive({
      return(input$pop1_gender)
    })
    pop2_gender <- reactive({
      return(input$pop2_gender)
    })
    
    pop1_min_MAF <- reactive({
      return(input$pop1_min_MAF)
    })
    pop2_max_MAF <- reactive({
      return(input$pop2_max_MAF)
    })
    
    pop1_value <- pop1()
    pop2_value <- pop2()
    pop1_gender_value <- pop1_gender()
    pop2_gender_value <- pop2_gender()
    pop1_min_MAF_value <- pop1_min_MAF()
    pop2_max_MAF_value <- pop2_max_MAF()
    
    fut9 <<- future({
      system(
        paste(
          gqt_path,
          'query -i',
          pop_files$gz_new,
          '-v -d',
          pop_peddb,
          paste0(
            '-p \"Population in (',
            paste0("'", paste(pop1_value, collapse = "','"), "'"),
            ")",
            "AND Gender=",
            paste0("'", pop1_gender_value, "'"),
            '\"'
          ),
          paste0('-g \"maf()>=', pop1_min_MAF_value, '\"'),
          paste0(
            '-p \"Population in (',
            paste0("'", paste(pop2_value, collapse = "','"), "'"),
            ")",
            "AND Gender=",
            paste0("'", pop2_gender_value, "'"),
            '\"'
          ),
          paste0('-g \"maf()<=', pop2_max_MAF_value, '\"'),
          ">",
          paste0(tmp_dir, "/pop.vcf.gz", sep = "")
        )
      )
      pop_vcf <-
        vcfR::read.vcfR(paste0(tmp_dir, "/pop.vcf.gz"))
      pop_vcf
    }) %...>%
      pop_content() %>%
      finally( ~ prog$close())
    NULL
  })
  
  
  #####Split vcf to table###########
  observeEvent(req(pop_content()), {
    updateTabsetPanel(session, "pop_tab", "pop_results")
    pop_vcfinput <- pop_content()
    pop_gt_info <- vcfR::vcfR2tidy(pop_vcfinput)$fix
    
    output$pop_table <-
      DT::renderDataTable(DT::datatable(
        pop_gt_info,
        options = list(
          searching = TRUE,
          pageLength = 10,
          rownames(NULL),
          scrollX = T
        )
      ))
    
    #######summarize by ANNOVAR and snpEff annotation flags if exists in input VCF#####
    pop_gene_summary <- function() {
      annot_levels = c(
        "Annotation",
        "Func.ensGene",
        "Func.genericGene",
        "Func.refGene",
        "Func.FEELncGene"
      )
      if ((nrow(pop_gt_info) != 0) &&
          length(annot_levels[which(annot_levels %in% names(pop_gt_info))]) >= 1)
      {
        annot_levels = annot_levels[which(annot_levels %in% names(pop_gt_info))]
        
        pop_out_gene_table <-
          as.data.frame.matrix(xtabs(~ values + ind,
                                     stack(pop_gt_info, select = annot_levels)))
        pop_out_gene_table_fix <-
          setDT(pop_out_gene_table, keep.rownames = TRUE)
        colnames(pop_out_gene_table_fix)[1] <- "Region"
        return(pop_out_gene_table_fix)
      }
      else if ((nrow(pop_gt_info) != 0) &
               length(annot_levels[which(annot_levels %in% names(pop_gt_info))]) == 0)
      {
        pop_out_gene_table_fix <- data.frame()
        return(pop_out_gene_table_fix)
      }
      else if ((nrow(pop_gt_info) == 0) |
               length(annot_levels[which(annot_levels %in% names(pop_gt_info))]) ==
               0)
      {
        pop_out_gene_table_fix <- data.frame()
        return(pop_out_gene_table_fix)
      }
    }
    
    ######code to print count and summary table in 'Summary'pane###########
    
    output$pop_count <-
      renderText(paste("Total filtered variants:", nrow(unique(pop_gt_info[c("CHROM", "POS")]))))
    
    
    output$pop_summary <-
      renderTable(
        pop_gene_summary(),
        rownames = TRUE,
        hover = TRUE,
        striped = TRUE
      )
    
    ###### code to plot variant summary#############
    pop_plot <- function() {
      if (nrow(pop_gene_summary()) > 1) {
        popGenePlot <- pop_gene_summary()
        dfm <-
          melt(popGenePlot,
               varnames = c(colnames(popGenePlot)),
               id.vars = 1)
        ggplot(dfm, aes(x = Region, y = value)) + geom_bar(aes(fill = variable),
                                                           stat = "identity",
                                                           position = "dodge") +
          theme_bw() + theme(
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
          ) +
          xlab("Functional category") + ylab("Variant count") +
          guides(fill = guide_legend(title = "Gene Database")) +
          theme(
            axis.text = element_text(size = 12, face = "bold"),
            axis.title = element_text(size = 14, face = "bold"),
            axis.text.x = element_text(angle = 90, hjust = 1)
          )
      }
      else if (nrow(pop_gene_summary()) <= 1) {
        Variants <- 0
        hist(
          Variants,
          xlab = "Functional category",
          ylab = ("Variant count"),
          main = NULL,
          labels = FALSE
        )
      }
    }
    
    
    #####code to render and download png,txt and VCF output files########
    
    output$pop_plot <- renderPlot({
      pop_plot()
    })
    
    output$pop_downPlot <- downloadHandler(
      filename = function() {
        paste("output", '.png', sep = '')
      },
      content = function(file) {
        ggsave(file, pop_plot())
      }
    )
    
    output$pop_downList <- downloadHandler(
      filename = function() {
        paste0("output", ".txt")
      },
      content = function(file) {
        write.table(
          pop_out,
          file,
          row.names = FALSE,
          sep = "\t",
          quote = FALSE
        )
      }
    )
    
    output$pop_VCFdownList <- downloadHandler(
      filename = function() {
        paste0("output", ".vcf")
      },
      content = function(file) {
        write.vcf(pop_vcfinput, file)
      }
    )
  })
  
  observeEvent(input$pop_cancel, {
    stopMulticoreFuture(fut9)
  })
  
  ####################sample filter module################
  
  sample_content <- reactiveVal()
  
  observeEvent(input$sample_run, {
    prog <- Progress$new(session)
    prog$set(message = "Sample based analysis in progress",
             detail = "Please do not refresh,This may take a while...",
             value = NULL)
    fut10 <- NULL
    
    sample_files <- gqt_list()
    sample_ped <- ped_file()
    if (input$ped_rd == "1000 Genomes") {
      sample_peddb <- peddb_default
    }
    else if (input$ped_rd == "Upload PED" &&
             input$sample_run) {
      sample_peddb <- peddb_content()
    }
    sample_vartype <- reactive({
      return(input$sample_vartype)
    })
    
    sample_min_count <- reactive({
      return(input$sample_min_count)
    })
    
    sample_vartype_value <- sample_vartype()
    sample_min_count_value <- sample_min_count()
    sample_ID <- sample_names()$sampleID
    fut10 <<- future({
      system(
        paste(
          gqt_path,
          'query -i',
          sample_files$gz_new,
          '-v -d',
          sample_peddb,
          paste0(
            '-p \"IndividualID in (',
            paste0("'", paste(sample_ID, collapse = "','"), "'"),
            ")",
            '\"'
          ),
          paste0(
            '-g \"count(',
            sample_vartype_value,
            ")",
            ">=",
            sample_min_count_value,
            "\""
          ),
          ">",
          paste0(tmp_dir, "/sample.vcf.gz", sep = "")
        )
      )
      sample_vcf <-
        vcfR::read.vcfR(paste0(tmp_dir, "/sample.vcf.gz"))
      sample_vcf
    }) %...>%
      sample_content() %>%
      finally( ~ prog$close())
    NULL
  })
  
  
  #########split vcf to table#########
  observeEvent(req(sample_content()),
               {
                 #shinyjs::enable(id = sample_run)
                 updateTabsetPanel(session, "sample_tab", "sample_results")
                 sample_vcfinput <- sample_content()
                 regex_sample_ID <- sample_names()$regex_sampleID
                 sample_vcf_gt <-
                   sample_vcfinput@gt[, grep(regex_sample_ID, colnames(sample_vcfinput@gt))]
                 sample_vcf_info <- vcfR::vcfR2tidy(sample_vcfinput)$fix
                 sample_vcf_info <-
                   sample_vcf_info[, c(2:ncol(sample_vcf_info))]
                 
                 sample_gt_info <- cbind(sample_vcf_gt, sample_vcf_info)
                 
                 output$sample_table <-
                   DT::renderDataTable(DT::datatable(
                     sample_gt_info,
                     options = list(
                       searching = TRUE,
                       pageLength = 10,
                       rownames(NULL),
                       scrollX = T
                     )
                   ))
                 
                 
                 
                 #######summarize by ANNOVAR and snpEff annotation flags if exists in input VCF#####
                 sample_gene_summary <- function() {
                   annot_levels = c(
                     "Annotation",
                     "Func.ensGene",
                     "Func.genericGene",
                     "Func.refGene",
                     "Func.FEELncGene"
                   )
                   if ((nrow(sample_gt_info) != 0) &&
                       length(annot_levels[which(annot_levels %in% names(sample_gt_info))]) >= 1)
                   {
                     annot_levels = annot_levels[which(annot_levels %in% names(sample_gt_info))]
                     
                     sample_out_gene_table <-
                       as.data.frame.matrix(xtabs(
                         ~ values + ind,
                         stack(sample_gt_info, select = annot_levels)
                       ))
                     sample_out_gene_table_fix <-
                       setDT(sample_out_gene_table, keep.rownames = TRUE)
                     colnames(sample_out_gene_table_fix)[1] <- "Region"
                     return(sample_out_gene_table_fix)
                   }
                   else if ((nrow(sample_gt_info) != 0) &
                            length(annot_levels[which(annot_levels %in% names(sample_gt_info))]) == 0)
                   {
                     sample_out_gene_table_fix <- data.frame()
                     return(sample_out_gene_table_fix)
                   }
                   else if ((nrow(sample_gt_info) == 0) |
                            length(annot_levels[which(annot_levels %in% names(sample_gt_info))]) ==
                            0)
                   {
                     sample_out_gene_table_fix <- data.frame()
                     return(sample_out_gene_table_fix)
                   }
                 }
                 
                 ######code to print count and summary table in 'Summary'pane###########
                 
                 output$sample_count <-
                   renderText(paste("Total filtered variants:", nrow(unique(
                     sample_gt_info[c("CHROM", "POS")]
                   ))))
                 
                 
                 output$sample_summary <-
                   renderTable(
                     sample_gene_summary(),
                     rownames = TRUE,
                     hover = TRUE,
                     striped = TRUE
                   )
                 
                 ###### code to plot variant summary#############
                 sample_plot <- function() {
                   if (nrow(sample_gene_summary()) > 1) {
                     recGenePlot <- sample_gene_summary()
                     dfm <-
                       melt(recGenePlot,
                            varnames = c(colnames(recGenePlot)),
                            id.vars = 1)
                     ggplot(dfm, aes(x = Region, y = value)) + geom_bar(aes(fill = variable),
                                                                        stat = "identity",
                                                                        position = "dodge") +
                       theme_bw() + theme(
                         panel.border = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank()
                       ) +
                       xlab("Functional category") + ylab("Variant count") +
                       guides(fill = guide_legend(title = "Gene Database")) +
                       theme(
                         axis.text = element_text(size = 12, face = "bold"),
                         axis.title = element_text(size = 14, face = "bold"),
                         axis.text.x = element_text(angle = 90, hjust = 1)
                       )
                   }
                   else if (nrow(sample_gene_summary()) <= 1) {
                     Variants <- 0
                     hist(
                       Variants,
                       xlab = "Functional category",
                       ylab = ("Variant count"),
                       main = NULL,
                       labels = FALSE
                     )
                   }
                 }
                 
                 
                 #####code to render and download png,txt and VCF output files########
                 
                 output$sample_plot <- renderPlot({
                   sample_plot()
                 })
                 
                 output$sample_downPlot <- downloadHandler(
                   filename = function() {
                     paste("output", '.png', sep = '')
                   },
                   content = function(file) {
                     ggsave(file, sample_plot())
                   }
                 )
                 
                 output$sample_downList <- downloadHandler(
                   filename = function() {
                     paste0("output", ".txt")
                   },
                   content = function(file) {
                     write.table(
                       sample_gt_info,
                       file,
                       row.names = FALSE,
                       sep = "\t",
                       quote = FALSE
                     )
                   }
                 )
                 
                 output$sample_VCFdownList <- downloadHandler(
                   filename = function() {
                     paste0("output", ".vcf")
                   },
                   content = function(file) {
                     write.vcf(sample_vcfinput, file)
                   }
                 )
               })
  
  observeEvent(input$sample_cancel, {
    stopMulticoreFuture(fut10)
  })

  #####variant count module############
  variant_content <- reactiveVal()

  observeEvent(input$variant_run,
               {
                 prog <- Progress$new(session)
                 prog$set(message = "Variant count analysis in progress",
                          detail = "Do not refresh the page,this may take a while...",
                          value = NULL)
                 fut11 <- NULL

                 variant_files <- gqt_list()
                 variant_ped <- ped_file()
                 if (input$ped_rd == "1000 Genomes") {
                   variant_peddb <- peddb_default
                 }
                 else if (input$ped_rd == "Upload PED" &&
                          input$variant_run) {
                   variant_peddb <- peddb_content()
                 }
                 count_vartype <- reactive({
                   return(input$count_vartype)
                 })


                 count_vartype_value <- count_vartype()
                 variant_ID <- input$names

                 fut11 <<- future({
                   system(
                     paste(
                       gqt_path,
                       'query -i',
                       variant_files$gz_new,
                       '-d',
                       variant_peddb,
                       paste0('-p \"BCF_Sample in (',
                              "'",
                              variant_ID,
                              "'",
                              ")",
                              '\"'),
                       paste0('-g \"', count_vartype_value, '\"'),
                       '-c',
                       ">",
                       paste0(tmp_dir, "/count.txt", sep = "")
                     )
                   )
                   count_vcf <-
                     read.delim(paste0(tmp_dir, "/count.txt"), header  =  F)
                   count_vcf
                 }) %...>%
                   variant_content() %>%
                   finally( ~ prog$close())
                 NULL
               })

  observeEvent(req(variant_content()),
               {
                 updateTabsetPanel(session, "count_tab", "variant_results")
                 output$variant_count <-
                   renderText(paste("Total variants:", variant_content()))
               })

}

shinyApp(ui = ui, server = server)
