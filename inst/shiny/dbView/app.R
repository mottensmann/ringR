library(shiny)
library(bslib)
library(shinyFiles)
library(dplyr)
library(plotly)

# Available filesystem roots for the file chooser
volumes <- c(Home = path.expand("~"), shinyFiles::getVolumes()())

# ── Persistence helpers ────────────────────────────────────────────────────────
.config_file <- file.path(tools::R_user_dir("ringR", which = "data"), "last_gpkg.txt")

save_last_path <- function(path) {
    dir.create(dirname(.config_file), recursive = TRUE, showWarnings = FALSE)
    writeLines(path, .config_file)
}

read_last_path <- function() {
    if (!file.exists(.config_file)) return(NULL)
    path <- readLines(.config_file, warn = FALSE)[1]
    if (length(path) == 1 && nzchar(path) && file.exists(path)) path else NULL
}

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- page_sidebar(
    title = "RING2.1 Datenbank",
    theme = bs_theme(version = 5),
    sidebar = sidebar(
        shinyFilesButton(
            "gpkg",
            label    = tagList(bsicons::bs_icon("database"), "Datenbank auswählen"),
            title    = "RING2.1.gpkg",
            multiple = FALSE
        ),

        textOutput("file_label"),
        hr(),
        uiOutput("ring_select"),
        uiOutput("map_toggle")
    ),
    card(
        card_header("Beringung"),
        tableOutput("tblRinging")
    ),
    uiOutput("card_open"),
    uiOutput("card_map")
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

    shinyFileChoose(input, "gpkg", roots = volumes, filetypes = c("gpkg"))

    # Holds the active .gpkg path; pre-seeded from the last session if available
    gpkg_path <- reactiveVal(read_last_path())

    # Update path and persist whenever the user picks a new file
    observe({
        req(input$gpkg)
        if (!is.integer(input$gpkg)) {
            path <- parseFilePaths(volumes, input$gpkg)$datapath
            if (length(path) == 1 && nzchar(path)) {
                gpkg_path(path)
                save_last_path(path)
            }
        }
    })

    output$file_label <- renderText({
        req(gpkg_path())
        basename(gpkg_path())
    })

    # Load data once a file is chosen
    ring <- reactive({
        req(gpkg_path())
        tryCatch(
            read_gpkg(gpkg_path()),
            error = function(e) {
                showNotification(
                    paste("Fehler beim Laden der Datenbank:", conditionMessage(e)),
                    type     = "error",
                    duration = NULL
                )
                NULL
            }
        )
    })

    # Render ring-number selector only after data is loaded
    output$ring_select <- renderUI({
        req(ring())
        selectInput("ring_number",
                    label   = tagList(bsicons::bs_icon("circle"), "Ring Nr."),
                    choices = ring()[["tblRinging"]][["strRingNr"]])
    })

    output$map_toggle <- renderUI({
        req(ring(), input$ring_number)
        df <- filter(ring()[["tblOpen"]], strRingNr == input$ring_number)
        if (nrow(df) > 0) {
            input_switch("show_map", "Recovery Map", value = FALSE)
        }
    })

    # ── Helper ----------------------------------------------------------------
    build_table <- function(df, ring_data) {
        df <- left_join(
            df,
            ring_data[["tblGeotab"]][, c("idGeoTab", "strPlace")],
            by = "idGeoTab"
        )
        data.frame(
            EURING     = df$strSpecies,
            Art        = df$strSpeciesDE,
            Ring       = df$strRingNr,
            CTR        = df$strRingScheme,
            Datum      = substr(as.character(df$dtmDate), 1, 19),
            Sex        = df$strSex,
            Alter      = df$strAge,
            Wing       = df$lngWingLength,
            P8         = df$lngLengthP8,
            Tarsus     = df$lngTarsus,
            Masse      = df$lngWeight,
            Ort        = df$strPlace,
            Lon        = df$lngLong,
            Lat        = df$lngLat,
            Bemerkungen = df$strRemarks
        )
    }

    # ── Tables ----------------------------------------------------------------
    output$tblRinging <- renderTable({
        req(ring(), input$ring_number)
        df <- filter(ring()[["tblRinging"]], strRingNr == input$ring_number)
        build_table(df, ring())
    }, na = "")

    output$card_open <- renderUI({
        req(ring(), input$ring_number)
        df <- filter(ring()[["tblOpen"]], strRingNr == input$ring_number)
        if (nrow(df) > 0) {
            card(
                card_header("Wiederfunde"),
                tableOutput("tblOpen")
            )
        }
    })

    output$tblOpen <- renderTable({
        req(ring(), input$ring_number)
        df <- filter(ring()[["tblOpen"]], strRingNr == input$ring_number)
        req(nrow(df) > 0)
        build_table(df, ring())
    }, na = "")

    # ── Map -------------------------------------------------------------------
    output$card_map <- renderUI({
        req(ring(), input$ring_number, input$show_map)
        card(
            full_screen = TRUE,
            card_header("Recovery Map"),
            plotlyOutput("Map")
        )
    })

    output$Map <- renderPlotly({
        req(ring(), input$ring_number, input$show_map)
        ggplotly(plot_resights(ring = input$ring_number, data = ring()))
    })
}

shinyApp(ui, server)
