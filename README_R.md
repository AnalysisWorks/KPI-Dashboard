# R Overview

 - Installation and Setup
 - Data Types
 - Helpful Packages

## Installation and Setup

 - R is separated into different versions like any other software but allows for multiple versions to exist simultaneously on your computer. The easiest way to update your version is to run the following commands in the R gui and follow the prompts.
    * install.packages("installr"); library(installr) # install+load installr
    * updateR()

 - Each R distribution is downloaded from a mirror. You'll first run into this when install new packages (with the install.package() command). This will prompt you to select a mirror. SFU currently runs the closest mirror to AW so it is generally the source for all your packages and updates.

 - When installing packages, the dependencies may not always install as part of the package. To get around this you can add the dependencies = TRUE to the installation function. (install.packages("packageName", dependencies = TRUE)).

 - These packages will by default be installed to your user account and not the computer. This is not generally an issue but it can be when running R scripts that require admin privledges. To avoid this you can specifiy the 'lib' parameter of the installation function to install packages into the program files of your computer. This will reuire the R gui to be run as an admin as the program files will be changing on your compputer.

 - As a side, multiple packages can be installed in one go. If you place the packages in a list c("packageOne", "packageTwo) and place this into the installation call you can install multiple packages at once.

## Data types

 - Outside of the basic data type (numerics, strings, booleans) R has some other data types to support advanced calcuations and functions. The main types are vectors, dataframes, matrices, and lists
 - vectors can generally be thought of as arrays and are created with the c() syntax
 - dataframes are tables with an optional header. These are one of the most robust data types.
 - Matrices are a simplified version of dataframes without explicit column names. Several packages require this format for computation as it can be handled easier than dataframes.
 - Lists are the closest thing to a hash table (or dictionary) in R. They can be simple arrays or a collection of key -> value paris


## Helpful Pacakges

 - Readr a very robust file reading package. R has it's own built in read functions but this package can more accurately handle the types of delimited files that we most commonly deal with.

 - Dplyr handles data manipulation using a sql type pattern. It has a unique syntax %>% that allows multiple join/filters/grouping to be strung together and acts similar to the pipe operation in windows powershell.

 - RODBC handles all database connections and queries whenever you want to read, write and update a database.
