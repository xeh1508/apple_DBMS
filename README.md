# Apple Database Management System

About this Shiny app:

As an extension to [Apple Sales Dashboard](https://yujiexiang.shinyapps.io/apple_sales_dashboard/), this Shiny app simulates a database management system featuring functions like login/logout, save/create/delete tables, add/rename columns, etc. These two apps compare the roles between data analysts and database administrators, and both showcase the rich and varied functionality of Shiny app.

Features of this app:

1. Back-end database: A SQLite database that stores the Apple sales information in Jan 2020.  The data base consists of 5 tables - order items (*order_items*), order details (*orders*), products information (*prods_i*), customer information (*custs*), and store information (*stores*). 
2. Authorization: Credit to the package `shinyauthr` which provides module functions that can be used to add an authentication layer to shiny apps. Github Link: https://github.com/paulc91/shinyauthr
3. Highlights of the functions:
    - Save tables: save a store sales summary table to the database.
    - Update existing tables: rename tables, rename/add columns of tables.
    - Create new tables: table and column names can be customized. Provide 4 types of column to be added - integer, float, varchar(255), and boolean. 
    - Create entries to tables:  prompt columns contained in the selected table together with their types. 
    - Delete tables: the action is only accessible to specific authorization.
4. Robustness: Defense mechanism that prevents duplicates, invalid expressions, and conflicts with SQL keywords are set for all the input table and colunm names to ensure the smooth execution of SQL queries. Once errors are detected, prompt messages will show up suggesting possible failure reasons. 
