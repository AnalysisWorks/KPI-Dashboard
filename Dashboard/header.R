require(shiny)
require(shinydashboard)
require(dplyr)

getHeader <- function(){
    header <- dashboardHeader( 
                    title = "Dashboard",
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
        ),
        messageItem(
            from = "Support",
            message = "The new server is ready.",
            icon = icon("life-ring"),
            time = "2014-12-01"
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
        ),
        notificationItem(
            text = "Server load at 86%",
            icon = icon("exclamation-triangle"),
            status = "warning"
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
        ),
        taskItem(value = 75, color = "yellow",
            "Server deployment"
        ),
        taskItem(value = 80, color = "red",
            "Overall project"
        )
    )
    dropDownTasks
}