# the necessary packages
library(DT)
library(RSQLite)
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(tidyverse)
library(shinyauthr)
library(shinyFeedback)

# dataframe that holds usernames, passwords and other user data
user_base <- data.frame(
  user = c("user1", "user2"),
  password = c("pass1", "pass2"), 
  password_hash = sapply(c("pass1", "pass2"), sodium::password_store), 
  permissions = c("admin", "manager")
)

# sqlite keywords 
sqlite_kw <- 
  c('ABORT',
    'ACTION',
    'ADD',
    'AFTER',
    'ALL',
    'ALTER',
    'ALWAYS',
    'ANALYZE',
    'AND',
    'AS',
    'ASC',
    'ATTACH',
    'AUTOINCREMENT',
    'BEFORE',
    'BEGIN',
    'BETWEEN',
    'BY',
    'CASCADE',
    'CASE',
    'CAST',
    'CHECK',
    'COLLATE',
    'COLUMN',
    'COMMIT',
    'CONFLICT',
    'CONSTRAINT',
    'CREATE',
    'CROSS',
    'CURRENT',
    'CURRENT_DATE',
    'CURRENT_TIME',
    'CURRENT_TIMESTAMP',
    'DATABASE',
    'DEFAULT',
    'DEFERRABLE',
    'DEFERRED',
    'DELETE',
    'DESC',
    'DETACH',
    'DISTINCT',
    'DO',
    'DROP',
    'EACH',
    'ELSE',
    'END',
    'ESCAPE',
    'EXCEPT',
    'EXCLUDE',
    'EXCLUSIVE',
    'EXISTS',
    'EXPLAIN',
    'FAIL',
    'FILTER',
    'FIRST',
    'FOLLOWING',
    'FOR',
    'FOREIGN',
    'FROM',
    'FULL',
    'GENERATED',
    'GLOB',
    'GROUP',
    'GROUPS',
    'HAVING',
    'IF',
    'IGNORE',
    'IMMEDIATE',
    'IN',
    'INDEX',
    'INDEXED',
    'INITIALLY',
    'INNER',
    'INSERT',
    'INSTEAD',
    'INTERSECT',
    'INTO',
    'IS',
    'ISNULL',
    'JOIN',
    'KEY',
    'LAST',
    'LEFT',
    'LIKE',
    'LIMIT',
    'MATCH',
    'NATURAL',
    'NO',
    'NOT',
    'NOTHING',
    'NOTNULL',
    'NULL',
    'NULLS',
    'OF',
    'OFFSET',
    'ON',
    'OR',
    'ORDER',
    'OTHERS',
    'OUTER',
    'OVER',
    'PARTITION',
    'PLAN',
    'PRAGMA',
    'PRECEDING',
    'PRIMARY',
    'QUERY',
    'RAISE',
    'RANGE',
    'RECURSIVE',
    'REFERENCES',
    'REGEXP',
    'REINDEX',
    'RELEASE',
    'RENAME',
    'REPLACE',
    'RESTRICT',
    'RIGHT',
    'ROLLBACK',
    'ROW',
    'ROWS',
    'SAVEPOINT',
    'SELECT',
    'SET',
    'TABLE',
    'TEMP',
    'TEMPORARY',
    'THEN',
    'TIES',
    'TO',
    'TRANSACTION',
    'TRIGGER',
    'UNBOUNDED',
    'UNION',
    'UNIQUE',
    'UPDATE',
    'USING',
    'VACUUM',
    'VALUES',
    'VIEW',
    'VIRTUAL',
    'WHEN',
    'WHERE',
    'WINDOW',
    'WITH',
    'WITHOUT')
sqlite_kw_lo <- tolower(sqlite_kw)

# connect to the apple database
db <- dbConnect(SQLite(), 'apple2.db') 
prodtype <- dbGetQuery(db, 'SELECT distinct prod_type from prods_i')


###############################################
# define the ui function
###############################################
ui <- dashboardPage(
  # skin = "black",
  title="Apple | Database Management Platform",
  
  dashboardHeader(tags$li(class = "dropdown", style = "padding: 8px;",
                          tags$style(".main-header {max-height: 200px}")),
                  title = span("Database Management Platform", style = "font-size: 20px"),
                  titleWidth = 300,
                  tags$li(class = "dropdown",
                          tags$a(img(src = 'apple_logo.png', height = 18),
                                 href = "https://yujiexiang.shinyapps.io/apple_sales_dashboard/",
                                 title = "Check out the Apple Sales Dashboard"),
                  tags$li(class = "dropdown", 
                          tags$a(icon("github"), 
                                 href = "https://github.com/xeh1508/apple_DBMS",
                                 title = "See the code on github")),
                  tags$li(class = "dropdown", style = "padding: 8px;",
                          shinyauthr::logoutUI("logout"))
                )
  ),
  
  dashboardSidebar(collapsed = TRUE, 
                   div(textOutput("welcome"), style = "padding: 20px"),
                   sidebarMenu(
                     menuItem("Save Tables", tabName = "save_table", icon = icon("save")),
                     menuItem("Update Tables", tabName = "update_table", icon = icon("exchange-alt")),
                     menuItem("Create Tables", tabName = "create_table", icon = icon("plus-square")),
                     menuItem("Insert Entries", tabName = "insert_value", icon = icon("edit")),
                     menuItem("Delete Tables", tabName = "del_table", icon = icon("trash-alt")),
                     menuItem("About", tabName = "about", icon = icon("info-circle"))
                   )
  ),
  
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(tags$style(".table{margin: 0 auto;}"),
              tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                          type="text/javascript"),
              includeScript("returnClick.js")
    ),
    shinyauthr::loginUI("login"),
    uiOutput("user_table"),
    # tags$footer(tags$i("A Sequel to Apple Sales Dashboard."), 
    #             tags$i("© Yujie Xiang, '20SPS, Applied Analytics, Columbia University."),
    #           align = "center", style = "
    #           position: absolute;
    #           bottom:10px;
    #           right:0;
    #           left:0;
    #           color: #565051;
    #           background:white;
    #           padding:10px;
    #           box-sizing:border-box;"),
    #uiOutput("developer_info"),
    tabItems(
      # First Tab
      tabItem(
        tabName = 'save_table',
        uiOutput("tab1UI"),
        HTML('<div data-iframe-height></div>')
      ),
      # Second Tab
      tabItem(
        tabName = 'del_table',
        uiOutput("tab2UI")
      ),
      # Third Tab
      tabItem(
        tabName = 'update_table',
        uiOutput("tab3UI")
      ),
      # fourth Tab
      tabItem(
        tabName = 'create_table',
        uiOutput("tab4UI")
      ),
      # Fifth Tab
      tabItem(
        tabName = 'insert_value',
        uiOutput("tab5UI")
      ),
      # Sixth Tab
      tabItem(
        tabName = 'about',
        uiOutput("tab6UI")
      )
    )
  )
)

###############################################
# define the server function
###############################################

server <- function(input, output, session) {
  
  # call login module supplying data frame, user and password cols
  # and reactive trigger
  credentials <- callModule(shinyauthr::login, "login", 
                            data = user_base,
                            user_col = user,
                            pwd_col = password_hash,
                            sodium_hashed = TRUE,
                            log_out = reactive(logout_init()))
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- callModule(shinyauthr::logout, "logout", 
                            reactive(credentials()$user_auth))
  
  # Add or remove a CSS class from an HTML element
  # Here sidebar-collapse
  observe({
    if(credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })
  
  # Show the sample login info
  output$user_table <- renderUI({
    # only show pre-login
    if(credentials()$user_auth) return(NULL)
    tagList(
      tags$p("Below are the sample login information with different permissions.",class = "text-center"),
      tags$p("Test the app using these accounts as well as an invalid login attempt 
             to see different outputs.",class = "text-center"),
      br(),
      renderTable({user_base[, -3]})
    )
  })

  # pulls out the user information returned from login module
  user_info <- reactive({credentials()$info})
  
  # menu welcome info
  output$welcome <- renderText({
    req(credentials()$user_auth)
    paste0("Welcome ",{user_info()$permissions},"!")
  })
  
  ############# Tab 1: Save Table
  output$sel_store <- renderDataTable(
    dbGetQuery(
      conn = db,
      statement = 
        'SELECT
          city, d.store_name,
          sum(price*qty) as sales
          from orders a
          left join order_items b on a.ord_id = b.ord_id
          left join prods_i c on b.prod_id = c.prod_id
          left join stores d on a.store_id = d.store_id
          WHERE prod_type = ? and a.store_id != "00000"
          group by 1,2
          order by sales DESC',
      params = input$prod_type_i
    )
  )
  
  output$tab1UI <- renderUI({
    req(credentials()$user_auth)
    box(width = NULL, status = "primary",
        wellPanel(
          sidebarLayout(
            sidebarPanel(
              # style = 'background-color: #ffa700; color: #ffffff;',
              selectInput(
                inputId = 'prod_type_i',
                label = 'Product Type:',
                choices = prodtype$prod_type,
                selected = 'Macbook'
              ),
              # action button
              actionButton(inputId = "save", 
                           label = "Save the Table",
                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
              tags$style(type='text/css', "button#save {margin-left: 60%;}"),
            ),
            mainPanel(
              h4(strong("Top Stores with Highest Sales:")),
              br(),
              dataTableOutput(
                outputId = 'sel_store')
            )
          )
        )
    )
  })
  
  # in case save twice
  observeEvent(input$save, {
    if (tolower(paste0('top_store_',gsub(" ", "", input$prod_type_i))) %in% tolower(dbListTables(db))) { 
      showModal(modalDialog(
        title = "Failed to Save",
        paste0("The table ",paste0('top_store_',gsub(" ", "", input$prod_type_i))," already exists."),
        footer = modalButton("OK"), easyClose = TRUE ) )
    } else {
    dbGetQuery(
      conn = db,
      statement = paste0(
        'CREATE TABLE top_store_',gsub(" ", "", input$prod_type_i),' AS
               SELECT city, d.store_name,
                     sum(price*qty) as sales
               FROM orders a
                  left join order_items b on a.ord_id = b.ord_id
                  left join prods_i c on b.prod_id = c.prod_id
                  left join stores d on a.store_id = d.store_id
               WHERE prod_type = ? and a.store_id != "00000"
               GROUP BY 1,2
               ORDER BY sales DESC'),
      params = input$prod_type_i
    )
    # after save table, update the list of tables in tab 2,3,5 
      updateSelectInput(session, "sel_table_2", 
                        choices = setdiff(dbListTables(db),
                                          c("custs_0","order_items_0","orders_0",
                                            "prods_0","prods_i_0","stores_0")))
      updateSelectInput(session, "sel_table_3", 
                        choices = setdiff(dbListTables(db),
                                          c("custs_0","order_items_0","orders_0",
                                            "prods_0","prods_i_0","stores_0")))
      updateSelectInput(session, "sel_table_3_i", 
                        choices = setdiff(dbListTables(db),
                                          c("custs_0","order_items_0","orders_0",
                                            "prods_0","prods_i_0","stores_0")))
      updateSelectInput(session, "sel_table_3_ii", 
                        choices = setdiff(dbListTables(db),
                                          c("custs_0","order_items_0","orders_0",
                                            "prods_0","prods_i_0","stores_0")))
      updateSelectInput(session, "sel_table_5", 
                        choices = setdiff(dbListTables(db),
                                          c("custs_0","order_items_0","orders_0",
                                            "prods_0","prods_i_0","stores_0")))
      showModal(modalDialog(
        title = "Success",
        "Stores with top sales for the selected product type have been successfully saved to a new table.",
        footer = modalButton("OK"), easyClose = TRUE ) )
    }
  }
  )
  
  ############# Tab 2: Update Table (rename table, rename column, add column)
  # Unlike SQL-standard and other database systems, SQLite supports a very limited functionality of the ALTER TABLE statement.
  output$tab3UI <- renderUI({
    req(credentials()$user_auth)
    fluidPage(
      # rename table
      box(title = 'Rename Table', width = NULL, solidHeader = TRUE, status = "primary",
          # style = 'background-color: #ffa700; color: #ffffff;',
          selectInput(
            inputId = 'sel_table_3_ii',
            label = 'Select Table:',
            choices = setdiff(dbListTables(db),
                              c("custs_0","order_items_0","orders_0",
                                "prods_0","prods_i_0","stores_0")),
            selected = 'orders'
          ),
          wellPanel(
            textInput(inputId = "rnm_table_to",
                      label = "Rename To:"),
            actionButton(inputId = "rename_table",
                         label = "Rename Table",
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
            )
          )
      ),
      
      # rename colunm
      box(title = 'Rename Column', width = NULL, solidHeader = TRUE, status = "primary",
          # style = 'background-color: #ffa700; color: #ffffff;',
          selectInput(
            inputId = 'sel_table_3',
            label = 'Select Table:',
            choices = setdiff(dbListTables(db),
                              c("custs_0","order_items_0","orders_0",
                                "prods_0","prods_i_0","stores_0")),
            selected = 'orders'
          ),
          wellPanel(
            selectInput(
              inputId = 'sel_col_3',
              label = 'Select Column:',
              choices = NULL),
            textInput(inputId = "rnm_col_to",
                      label = "Rename the Column:"),
            actionButton(inputId = "rename_col",
                         label = "Rename Column",
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
            )
          )
      ),
      
      # add column
      box( title = 'Add Column', width = NULL, solidHeader = TRUE, status = "primary",
           selectInput(
             inputId = 'sel_table_3_i',
             label = 'Select Table:',
             choices = setdiff(dbListTables(db),
                               c("custs_0","order_items_0","orders_0",
                                 "prods_0","prods_i_0","stores_0")),
             selected = 'orders'
           ),
           wellPanel(
             textInput(inputId = 'add_col_name', label = "Add Column"),
             selectInput(inputId = "add_col_type", label = "Add Column Type", 
                         choices = c("NUMERIC","VARCHAR(255)","BOOLEAN")
             ),
             actionButton(inputId = "add_col",
                          label = "Add Column",
                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
           ) 
      )
    )
  })
  
  # show colnames 
  observeEvent(input$sel_table_3, {
    d <- dbGetQuery(
      conn = db,
      statement = paste0('SELECT * from ',input$sel_table_3)
    )
    updateSelectInput(session, "sel_col_3", choices = colnames(d))
  })

  
  # Rename Colunm!
  observeEvent(input$rename_col, {
                 d <- dbGetQuery(
                   conn = db,
                   statement = paste0('Select * from ',input$sel_table_3)
                 )
                 # in case the column name already exists
                 if (input$rnm_table_to %in% colnames(d) | 
                     !isTruthy(input$rnm_col_to)| 
                     grepl("^[a-zA-Z_][a-zA-Z0-9_]*$",input$rnm_col_to) == FALSE |
                     tolower(input$rnm_col_to) %in% sqlite_kw_lo )  {
                   showModal(modalDialog(
                     title = "Invalid column name",
                     "You get this message possibly because: 
                       1) the column name already exists;
                       2) the field is blank;
                       3) this is an invalid SQLite column name;
                       or 4) the field name conflicts with a SQLite keyword.",
                     footer = modalButton("OK"), easyClose = TRUE ) )
                 } else {
                   dbGetQuery(
                     conn = db,
                     statement = paste0('ALTER TABLE ',input$sel_table_3,
                                        ' RENAME COLUMN ',input$sel_col_3,
                                        ' TO ',input$rnm_col_to)
                   )
                   
                   # after rename column, clear the input box
                   updateTextInput(session, "rnm_col_to", value = '')
                   # after rename column, update colunm select range
                   updateSelectInput(session, "sel_col_3", choices = colnames(d))
                   showModal(modalDialog(
                     title = "Success",
                     "The column has been successfully renamed.",
                     footer = modalButton("OK"), easyClose = TRUE ) )
                 }
               }
  )
  
  # Add Column! 
  observeEvent(input$add_col, {
                d <- dbGetQuery(
                  conn = db,
                  statement = paste0('Select * from ',input$sel_table_3_i)
                )
                # in case the col name already exists
                if ( !isTruthy(input$add_col_name) | 
                     input$add_col_name %in% colnames(d) |
                     grepl("^[a-zA-Z_][a-zA-Z0-9_]*$",input$add_col_name) == FALSE |
                     tolower(input$add_col_name) %in% sqlite_kw_lo ) {
                  showModal(modalDialog(
                    title = "Invalid column name",
                    "You get this message possibly because: 
                       1) the column name already exists;
                       2) the field is blank;
                       3) this is an invalid SQLite column name;
                       or 4) the field name conflicts with a SQLite keyword.",
                    footer = modalButton("OK"), easyClose = TRUE
                  ) )
                } else {
                  dbGetQuery(
                    conn = db,
                    statement = paste0('ALTER TABLE ',input$sel_table_3_i,
                                       ' ADD COLUMN ',input$add_col_name,
                                       ' ',input$add_col_type)
                  )
                  # after add column, clear text input
                  updateTextInput(session, "add_col_name", value = '')
                  # after add column, update colunm select range
                  updateSelectInput(session, "sel_table_3", 
                                    choices = setdiff(dbListTables(db),
                                                      c("custs_0","order_items_0","orders_0",
                                                        "prods_0","prods_i_0","stores_0")))
                  updateSelectInput(session, "sel_col_3", choices = colnames(d))
                  showModal(modalDialog(
                    title = "Success",
                    "The column has been successfully added.",
                    footer = modalButton("OK"), easyClose = TRUE ) )
                }
  }
  )
  
  # Rename Table! 
  observeEvent(input$rename_table, 
               {
                 # in case table already exists/no input/invalid
                 if (tolower(input$rnm_table_to) %in% tolower(dbListTables(db)) | 
                     !isTruthy(input$rnm_table_to) | 
                     grepl("^[a-zA-Z_][a-zA-Z0-9_]*$",input$rnm_table_to) == FALSE) { 
                   showModal(modalDialog(
                     title = "Invalid table name",
                     "You get this message possibly because: 
                       1) the table already exists;
                       2) the table name is blank;
                       or 3) this is an invalid table name.",
                     footer = modalButton("OK"), easyClose = TRUE ) )
                 } else {
                   dbGetQuery(
                     conn = db,
                     statement = paste0('ALTER TABLE ',input$sel_table_3_ii,
                                        ' RENAME TO ',input$rnm_table_to)
                   )
                   
                   # after rename table, clear the text input
                   updateTextInput(session, "rnm_table_to", value = '')
                   # after rename table, update the list of tables in tab 2,3,5 
                   updateSelectInput(session, "sel_table_2", 
                                     choices = setdiff(dbListTables(db),
                                                       c("custs_0","order_items_0","orders_0",
                                                         "prods_0","prods_i_0","stores_0")))
                   updateSelectInput(session, "sel_table_3", 
                                     choices = setdiff(dbListTables(db),
                                                       c("custs_0","order_items_0","orders_0",
                                                         "prods_0","prods_i_0","stores_0")))
                   updateSelectInput(session, "sel_table_3_i", 
                                     choices = setdiff(dbListTables(db),
                                                       c("custs_0","order_items_0","orders_0",
                                                         "prods_0","prods_i_0","stores_0")))
                   updateSelectInput(session, "sel_table_3_ii", 
                                     choices = setdiff(dbListTables(db),
                                                       c("custs_0","order_items_0","orders_0",
                                                         "prods_0","prods_i_0","stores_0")))
                   updateSelectInput(session, "sel_table_5", 
                                     choices = setdiff(dbListTables(db),
                                                       c("custs_0","order_items_0","orders_0",
                                                         "prods_0","prods_i_0","stores_0")))
                   showModal(modalDialog(
                     title = "Success",
                     "The table has been successfully renamed.",
                     footer = modalButton("OK"), easyClose = TRUE ) )
                 }
               }
  )
  
  
  ############# Tab 3: Create Table 
  # Type in the table name and the column names
  output$tab4UI <- renderUI({
    req(credentials()$user_auth)
    box(width = NULL, status = "primary",
        textInput(inputId = "table_name", label = "Table name"),
        numericInput(inputId = "ncols", label = "Number of columns", 1, min = 1,max = 10),
        uiOutput(outputId = "cols"),
        actionButton(inputId = "create_table", label = "Create table", class = "btn-info",
                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    )
  })
  
  # Type in the column names and the column types
  output$cols <- renderUI({
    req(input$ncols>=1)
    cols <- vector("list", input$ncols)
    for (i in seq_len(input$ncols)) {
      cols[[i]] <- box(
        title = paste("Column", i), width = 6, solidHeader = TRUE, status = "primary",
        textInput(inputId = paste0("colName", i), label = "Column name"),
        selectInput(inputId = paste0("colType", i), label = "Column type", 
                    choices = c("NUMERIC", "VARCHAR(255)","BOOLEAN")
        )
      )
    }
    cols
  })

  # Create!
  observeEvent(input$create_table, {
    
    #1 in case the table name is null or has existed or invalid
    if (tolower(input$table_name) %in% tolower(dbListTables(db)) |
        !isTruthy(input$table_name) |
        grepl("^[a-zA-Z_][a-zA-Z0-9_]*$",input$table_name) == FALSE) {
      showModal(modalDialog(
        title = "Invalid table name",
        "You get this message possibly because:
         1) the table already exists;
         2) the table name is blank;
         or 3) this is an invalid table name.",
        footer = modalButton("OK"), easyClose = TRUE ) )
      return()
    }
    
    #2 in case the input ncols blank
    if (!isTruthy(input$ncols)) {
      showModal(modalDialog(
        title = "Invalid table name",
        "Please type in the right column number.",
        footer = modalButton("OK"), easyClose = TRUE ) )
    }
    
    #3 in case the input ncols is 0
    else if (input$ncols < 1 ) {
      showModal(modalDialog(
        title = "No columns",
        "Each table must have one or more columns.",
        footer = modalButton("OK"), easyClose = TRUE
      )) 
    }  
    
    else {
      # gather all the colnames into a list
      col_names_list = list()
        for (i in seq_len(input$ncols)) {
          col_names_list <- c(col_names_list,input[[paste0("colName", i)]])
        }
  
      # in case there are column with no names/duplicate names/informal signs/in sqlite keywords
      if ( any(col_names_list == '') | 
           sum(duplicated(col_names_list)) > 0 |
           any(grepl("^[a-zA-Z_][a-zA-Z0-9_]*$",col_names_list) == FALSE) |
           any(tolower(col_names_list) %in% sqlite_kw_lo) ) {
        showModal(modalDialog(
          title = "Invalid column name",
          "You get this message possibly because: 
           1) one or more fields are blank;
           2) one or more fields contain invalid SQLite column name(s); 
           3) there are duplicate column names;
           or 4) one or more fields conflict with a SQLite keyword.",
          footer = modalButton("OK"), easyClose = TRUE
        ) )
        return()
      }
      
      # compile query
      query <- paste0('CREATE TABLE ',input$table_name,' (')
      for (i in seq_len(input$ncols)) { 
        query <- paste0(query,input[[paste0("colName", i)]],' ',input[[paste0("colType", i)]],',')
      }
      query <- paste0(str_sub(query,1,-2),')')
      dbGetQuery(
        conn = db,
        statement = query )
      
      # if succuess, after create table, update the list of tables in tab2,3,5 and clear the input box
      updateNumericInput(session, "ncols", value = '1')
      updateTextInput(session, "table_name", value = '')
      updateSelectInput(session, "sel_table_2", 
                        choices = setdiff(dbListTables(db),
                                          c("custs","iphones","order_items",
                                            "orders","prods","prods_i",
                                            "store_sales","stores")))
      updateSelectInput(session, "sel_table_3", 
                        choices = setdiff(dbListTables(db),
                                          c("custs","iphones","order_items",
                                            "orders","prods","prods_i",
                                            "store_sales","stores")))
      updateSelectInput(session, "sel_table_3_i", 
                        choices = setdiff(dbListTables(db),
                                          c("custs","iphones","order_items",
                                            "orders","prods","prods_i",
                                            "store_sales","stores")))
      updateSelectInput(session, "sel_table_3_ii", 
                        choices = setdiff(dbListTables(db),
                                          c("custs","iphones","order_items",
                                            "orders","prods","prods_i",
                                            "store_sales","stores")))
      updateSelectInput(session, "sel_table_5", 
                        choices = setdiff(dbListTables(db),
                                          c("custs","iphones","order_items",
                                            "orders","prods","prods_i",
                                            "store_sales","stores")))
      showModal(modalDialog(
        title = "Success",
        "The table has been successfully created.",
        footer = modalButton("OK"), easyClose = TRUE ) )
    }
  }
  )

  ############# Tab 4: Insert Values
  # select the table to insert values
  output$tab5UI <- renderUI({
    req(credentials()$user_auth)
    box(width = NULL, status = "primary",
        selectInput(
          inputId = 'sel_table_5',
          label = 'Select Table:',
          choices = setdiff(dbListTables(db),
                            c("custs_0","order_items_0","orders_0",
                              "prods_0","prods_i_0","stores_0",
                              "custs","order_items",
                              "prods","prods_i","stores")),
          selected = 'orders'
        ),
        # show each colnames names
        uiOutput("values"),
        actionButton(inputId = "insert_value", label = "Insert Value", class = "pull-right btn-info",
                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    )
  })
  
  # Type in the corresponding values names 
  output$values <- renderUI({
    values <- list()
    d <- dbGetQuery(
      conn = db,
      statement = paste0('SELECT * from ',input$sel_table_5)
    )
    typ <- dbGetQuery(
      conn = db, statement = paste0('PRAGMA table_info(',input$sel_table_5,')')
    )
    # show name and coltype
    for (col in colnames(d)) {
      typ_i = typ$type[typ$name==col]
      values[[col]] <- box(
          title = paste0(as.character(col),' (',typ_i,')'), 
          width = 6, solidHeader = TRUE, status = "primary",
          
          if (typ_i == 'BOOLEAN') {radioButtons(inputId = paste0("value_", col), label = 'Value',
                                                c("TRUE","FALSE") )}
          else if (typ_i == 'NUMERIC' | typ_i == 'FLOAT' |
                   typ_i == 'INTEGER' | typ_i == 'NUM' ) 
          {numericInput(inputId = paste0("value_", col), label = 'Value',value = 0)} 
          else if (typ_i == 'DATE') {dateInput(inputId = paste0("value_", col),
                                               label = "Value",
                                               value = "2020-12-01") }
          else {tagList(useShinyFeedback(),
                        textInput(inputId = paste0("value_", col), label = 'Value'))}
       )
    }
    values
  })
  
  # check store_id num of char
  observeEvent(input$value_store_id, {
    if (nchar(input$value_store_id) > 5) {
      showFeedbackWarning(
        inputId = "value_store_id",
        text = "Maximum length is 5."
      )  
    } else {
      hideFeedback("value_store_id")
    }
  }
  )

  # Insert!
  observeEvent(input$insert_value, {
    d <- dbGetQuery(
      conn = db,
      statement = paste0('SELECT * from ',input$sel_table_5)
    )
    
    req(nchar(input$value_store_id) <= 5)
    # compile and execute query 
    query <- paste0('INSERT INTO ',input$sel_table_5,' VALUES (')
    for (col in colnames(d)) {
      query <- paste0(query,'"',input[[paste0("value_", col)]],'",') }
    query <- paste0(str_sub(query,1,-2),')')
    dbGetQuery(
      conn = db,
      statement = query )
    
    # if success update the input box and selected input
    for (col in colnames(d)) {
      updateTextInput(session, paste0("value_",col), value = '')
    }
    updateSelectInput(session, "sel_table_2", 
                      choices = setdiff(dbListTables(db),
                                        c("custs_0","order_items_0","orders_0",
                                          "prods_0","prods_i_0","stores_0")))
    updateSelectInput(session, "sel_table_3", 
                      choices = setdiff(dbListTables(db),
                                        c("custs_0","order_items_0","orders_0",
                                          "prods_0","prods_i_0","stores_0")))
    updateSelectInput(session, "sel_table_3_i", 
                      choices = setdiff(dbListTables(db),
                                        c("custs_0","order_items_0","orders_0",
                                          "prods_0","prods_i_0","stores_0")))
    updateSelectInput(session, "sel_table_3_ii", 
                      choices = setdiff(dbListTables(db),
                                        c("custs_0","order_items_0","orders_0",
                                          "prods_0","prods_i_0","stores_0")))
    updateSelectInput(session, "sel_table_5", 
                      choices = setdiff(dbListTables(db),
                                        c("custs_0","order_items_0","orders_0",
                                          "prods_0","prods_i_0","stores_0")))
    showModal(modalDialog(
      title = "Success",
      "The values have been successfully inserted.",
      footer = modalButton("OK"), easyClose = TRUE ) )
  }  
  )

  
  ############# Tab 5: Delete Table
  output$tab2UI <- renderUI({
    req(credentials()$user_auth)
    
    box(width = NULL, status = "primary",
        wellPanel(
          sidebarLayout(
            sidebarPanel(
              # style = 'background-color: #ffa700; color: #ffffff;',
              selectInput(
                inputId = 'sel_table_2',
                label = 'Tables in Database',
                choices = setdiff(dbListTables(db),
                                  c("custs_0","order_items_0","orders_0",
                                    "prods_0","prods_i_0","stores_0",
                                    "custs","order_items","orders",
                                    "prods","prods_i","stores"))
              ),
              actionButton(inputId = "del_tab", 
                           label = "Delete the Table",
                           style="color: #fff; background-color: #C11B17"
              ),
              hr(),
              tags$i("Note: You are only allowed to delete the newly created tables."),
              tags$style(type='text/css', "button#del_tab {margin-left: 60%;}")
              
            ),
            mainPanel(
              h4(strong("Table Preview")),
              br(),
              dataTableOutput(
                outputId = 'sel_table')
              
            )
          )
        )
    )
  })
  
  # show table
  output$sel_table <- renderDataTable(
    dbGetQuery(
      conn = db,
      statement = paste0('SELECT * from ',input$sel_table_2)
    )
  )
  
  # If admin then no access to delete, if manager ok.
  observeEvent(input$del_tab, 
    if (user_info()$permissions == "admin") {
      showModal(
        modalDialog(
          title = "Access Denied",
          "You are not authorized to perform this operation.",
          footer = modalButton("OK"), easyClose = TRUE
        )
      )
    }
    else {
    confirmSweetAlert(
      session = session, inputId = "confirmation", type = "warning",
      text = "This action cannot be undone.", 
      title = "Are you sure you want to delete the table?", danger_mode = TRUE
    )
  })
  
  # for manager, if double check is passed then delete the table
  observeEvent(input$confirmation, {
    if (isTRUE(input$confirmation)) {
      dbGetQuery(
        conn = db,
        statement = 
          paste0('DROP TABLE ',input$sel_table_2)  
      )
      updateSelectInput(session, "sel_table_2", 
                        choices = setdiff(dbListTables(db),
                                          c("custs_0","order_items_0","orders_0",
                                            "prods_0","prods_i_0","stores_0")))
      updateSelectInput(session, "sel_table_3", 
                        choices = setdiff(dbListTables(db),
                                          c("custs_0","order_items_0","orders_0",
                                            "prods_0","prods_i_0","stores_0")))
      updateSelectInput(session, "sel_table_3_i", 
                        choices = setdiff(dbListTables(db),
                                          c("custs_0","order_items_0","orders_0",
                                            "prods_0","prods_i_0","stores_0")))
      updateSelectInput(session, "sel_table_3_ii", 
                        choices = setdiff(dbListTables(db),
                                          c("custs_0","order_items_0","orders_0",
                                            "prods_0","prods_i_0","stores_0")))
      updateSelectInput(session, "sel_table_5", 
                        choices = setdiff(dbListTables(db),
                                          c("custs_0","order_items_0","orders_0",
                                            "prods_0","prods_i_0","stores_0")))
    showModal(
        modalDialog(
          title = "Success",
          "The table has been successfully deleted.",
          footer = modalButton("OK"), easyClose = TRUE
        )
      )
    }
  } 
  )
  
  ############# Tab 6: About
  output$tab6UI <- renderUI({
    req(credentials()$user_auth)
    box(title = 'About this app',width = NULL, status = "primary",solidHeader = TRUE,
        "As an extension to Apple Sales Dashboard, this Shiny app is a prototype of a database management system, featuring a variety of functions. 
          While Apple Sales Dashboard focuses on the front-end user interaction, this app addresses the needs in back-end database management.",
        br(),
        br(),
        h4(strong("Apple Sales Database")),
        "A SQLite database that stores the Apple sales information in Jan 2020.", 
        br(),
        "Consists of 5 tables - order items (order_items), order details (orders), products information (prods_i), customer information (custs), and store information (stores). 
          ",
        br(),
        br(),
       
        h4(strong("Links")),
        tags$head(tags$style(HTML("a {color: blue}"))),
        tags$a(href="https://github.com/xeh1508/apple_DBMS", 
               "Code on Github"), 
        tags$br(),
        img(src = 'me.jpg', height = 180, align = "right"),
        tags$a(href="https://yujiexiang.shinyapps.io/apple_sales_dashboard/", 
               "Prequel: Apple Sales Dashboard"), 
        br(),
        br(),
        h4(strong("Developer")),
        p("Yujie Xiang, '20SPS, Applied Analytics, Columbia University."),
        tags$head(tags$style(HTML("a {color: blue}"))),
        tags$a(href="http://www.linkedin.com/in/yujie-xiang", 
               "LinkedIn Profile"),
        br()
        
    )
  })
  
  
}

# when exiting app, disconnect from the apple database
onStop(
  function()
  {
    dbDisconnect(db)
  }
)

# execute the Shiny app
shinyApp(ui, server)



