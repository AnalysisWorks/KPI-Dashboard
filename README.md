# R Shiny Overview

 - Each Shiny app is composed of a server and a UI
 - A shiny app is run using 'shinyApp( ui, server)'
    * UI is a function containing the UI elements (which can also be functions)
    * server is also a function with the parameters 'input', 'output', and 'session'

## UI

 - Each Shiny UI elements are composed of multiple html elements
 - These UI elements all contain an ID attribute that can be used to reference the particular UI elsewhere in the app (usually the server)
 - HTML tags can also be used as html elements but the syntax is slightly different
    * div /div becomes div()
 - These UI are simplified further using the shinyDashboard package
    * This package separates the UI into header, sidebar, and body UIs
 - UI elements can also be generated dynamically from the server

## Server

 - This is where the content of the application comes from, additional UIs, data, chart creation, etc.
 - The Server contains three parameters input, output, and session
 - The Input acts as a data stream from the UI to the server.
    * You can access the values from any UI element using it's id on the input object. i.e. input$myUIid
    * Button UIs will return a boolean value, and NULL when not selected
    * Date sliders will return multiple values for start and end dates accessed by input$dt[1] and input$dt[2]
    * Some input objects are themselves objects i.e. the DT (datatable) from the package of the same name creates a interactive table where rows/cols/cells can be selected and attributes about them can be accessed with the same object. i.e. for input$DT (the table), the selected cell is input$DT_cell_clicked (the suffix is dynamically created with the package), and the column of that cell is input$DT_cell_clicked$col

 - Output acts as the data stream from the server to the UI
    * This is how dynamic UI elements are creaeted, i.e. output$myUI <- renderUI({'return UI here'})
    * These dynamic UIs referenced in the UI with uioutput("myUI"), where the prefix 'ui' matches the type of ui being generated.
    * The UI being returned within the server function does itself contain an ID. This ID is what will be referenced when retrieving information from the new dynamically created UI
    * Most of the time output will be used to spend text, plots, and tables to the UI

 - Session
    * This parameter stores the current state of the application.
    * i.e. the url can be parsed with parseQueryString(session$clientData$url_search) to pass through parameters
    * The state of any UI can be updated with the session. i.e. update'UINAMEHERE'Input(session, UIid,... other details of UI) without direct user input.


 - There are three main types of server functions 'reactive', 'render', and 'observe'
 - Render functions will always be started by referencing the output object followed by the desired name for the UI being created.
    * This name will be referenced in the UIoutput element where UI is replaced with the type of UI being generated.
    * The UI within an output function is defined the same as if it where defined in the UI ahead of time but variables can be passed in from the server
    * You can explicitly return the UI you created within the function or have the UI element be the last line of the function (or only line) since one of the properties of R is returning the final line of a function.

 - Reactive functions will be called whenever referenced. They are defined myfunction <- reactive({some function})
    * They can only be referenced on the server side of the application
    * These functions will be used whenever a server side function that uses them is activated.
    * i.e when the application is initially rendered these functions will be called if referenced by render functions.
    * These can also reactive to specific events i.e. button clicks/ other user actions

 - Observe functions will continually run within an application and can be set to only update when a certain event occurs
    * i.e. when the url contains arguments to be passed in
    * when an input variable is changed.


## Setup and layout

 - Once a shiny app is defined within the shinyApp() function with the UI and server correctly formatted it will be run on the machine it is being called on.
    * The network conditions can be set using options(shiny.attribute = "value") prior to the application call.
    * To have your application accessible from other users within the same network, use:
        1) options(shiny.host = "0.0.0.0")
        2) options(shiny.port = 5050) or whatever port you want
    * To access through a network the ip address of the host computer will need to be specified.
    * The app with run until the process is killed or a call to stopApp() is made within the application

 - Modularizing code
    * R libraries can be imported into the R module they are being used in with library( libName)
    * These can also be installed and imported with the require(libName) function
    * User defined modules and functions can be loade using the source('path to R module.R') these are best defined from the main calling functions as the relative paths to these functions is determined by the initial function called.
    * This allows functions for the server and UI to be defined in other modules and referenced within the main application. This shiny applications can quickly grow this modularization is crucial and allows functions to be used in other apps.
 - Debugging
    * Shiny unfortunately does not allow stepping through code on execution
    * You use print statements through the code to get around this or the log_event function
    * the package shinyEventLogger allows the exact flow through a shiny app to be determined.
    * Often elements depend on one another, when this is the case the function req(dependencies here) can be used. This will prevent the observe, reactive, and render functions from executing until all the dependencies (usually non-null variables) are met

## Package specific details

 - shinyDashboard
    * Pre-made shiny functions allows for rapid creation of apps with a dashboard layout
    * Separates apps into header, sidebar, dashboard, and server.

 - shinyEventLogger
    * Track events throughout application. Currently this is only being used as something similar to print statements throughout the code

 - dplyr
    * easy to use data manipulation library. Has options to do SQL joins on dataframes and filter data (similar to where clause)
    * Allows multiple manipulations to be strung together using %>% after each function

- RODBC
    * Setup connections to a database
    * Query the database with a sql string

 - plotly
    * Interactive plotting library, default plots allow for re-scaling, zooming, grids, and save as options
    * plot objects are created with the chart type and then features are added using the %>% syntax. This allows for layout features to be defined, legends, and other series to be uniquely defined and added to the plot

 - DT
    * The basic shiny package contains a datatable, however this package adds a lot to the interactivity of the table.
    * Data can be selected within the table and used within the server. This can be one or more of a cell, a row, or a column. This can then feed into other features within the app
    * Data can be dynmaically added, removed or updated
    * Data displayed can be dynamically changed based on user selection
    * Data can be easily sorted and searched (with options for regular expression searches)
    * Javascript can be inserted within the table, i.e. sparkline

 - sparkline
    * Create micro charts within table cells that show values on scroll over.
    * These are added to the DT (datatable) using JS callback functions. This pattern can be used to update most shiny components with javascript and htmlwidgets

 - tibbleTime
    * Used for statistical anomaly detections to separate a data series by different time intervals


 - anomalize
    * used in conjunction with tibbletime to detect anomalies within a series.
    * Statistics are calculated within a time frame and then a GESD algorithm is used to determine outliers

 - xgboost
    * Predictive modeling packages which used accelerated gradient descent to speed up algorithm training
    * Specializes in random forest models

 - e1071
    * Another decision tree package used for accuracy comparison

 - Matrix
    * Package to convert dataframes into matrices which are optimized for predictive algorithms
    * required for the predictive model packages used



# Helpful Links

 - General R help
    * updating R version and underlying packages: https://bootstrappers.umassmed.edu/bootstrappers-courses/courses/rCourse/Additional_Resources/Updating_R.html#updating-all-packages-after-r-update
