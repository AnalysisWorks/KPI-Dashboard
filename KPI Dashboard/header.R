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
            from = "Zachary",
            message = "Please review the README.md file."
        )
    )
    dropDownMessages
}

getDropDownNotification <- function(){

    dropDownNotifications <- dropdownMenu(type = "notifications",
        notificationItem(
            text = "Version 1.0.0",
            icon("users"),
            status = "success"
        ),
        notificationItem(
            text = "Please contact NW on error.",
            icon("truck")
        )
    )
    dropDownNotifications
}

getDropDownTasks <- function(){
    dropDownTasks <- dropdownMenu(type = "tasks", badgeStatus = "success",
        taskItem(value = 17, color = "aqua",
            "Progress Placeholder"
        ),
        taskItem(value = 90, color = "green",
            "Progress Placeholder"
        )
    )
    dropDownTasks
}
