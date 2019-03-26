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
            message = "This is my QA system."
        ),
        messageItem(
            from = "Zachary",
            message = "Improvement suggestions welcome.",
            time = "00:00"
        )
    )
    dropDownMessages
}

getDropDownNotification <- function(){

    dropDownNotifications <- dropdownMenu(type = "notifications",
        notificationItem(
            text = "Currently in Beta.",
            icon("users"),
            status = "success"
        ),
        notificationItem(
            text = "Please contact on error.",
            icon("truck")
        )
    )
    dropDownNotifications
}

getDropDownTasks <- function(){
    dropDownTasks <- dropdownMenu(type = "tasks", badgeStatus = "success",
        taskItem(value = 17, color = "aqua",
            "QA Project"
        ),
        taskItem(value = 90, color = "green",
            "QA Progress placeholder"
        )
    )
    dropDownTasks
}