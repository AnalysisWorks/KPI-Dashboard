require(shiny)
require(shinydashboard)
require(dplyr)

getHeader <- function(){
    header <- dashboardHeader( 
        title = "Database Anomalies",              
        getDropDownMessage(),
        getDropDownNotification(),
        getDropDownTasks()
    )
    header
}


getDropDownMessage <- function(){
    dropDownMessages <- dropdownMenu(type = "messages",
        messageItem(
            from = "Sales Dept",
            message = "Sales are steady this month."
        ),
        messageItem(
            from = "New User",
            message = "How do I register?",
            icon = icon("question"),
            time = "13:45"
        )
    )
    dropDownMessages
}

getDropDownNotification <- function(){

    dropDownNotifications <- dropdownMenu(type = "notifications",
        notificationItem(
            text = "5 new users today",
            icon("users")
        ),
        notificationItem(
            text = "12 items delivered",
            icon("truck"),
            status = "success"
        )
    )
    dropDownNotifications
}

getDropDownTasks <- function(){
    dropDownTasks <- dropdownMenu(type = "tasks", badgeStatus = "success",
        taskItem(value = 90, color = "green",
            "Documentation"
        ),
        taskItem(value = 17, color = "aqua",
            "Project X"
        )
    )
    dropDownTasks
}