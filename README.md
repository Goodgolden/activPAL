# activPAL R Tools

## Randy's Notes on the activPAL R Tools

### Clarification

This is a package that was developed by:

  - [Speirs Craig](mailto:craig@palt.com)
  
  - [Maxwell Douglas](mailto:douglas@palt.com)
  
  - [Loudon David](mailto:david@palt.com)

The original package was developed for use with the [activPAL device](https://github.com/PALkitchen/activPAL)
    
Randy simply added extra functions and files to make it work on 
both Macbook and Windows systems for Dr. Christiansen, Cory's Lab

### Files explanation

There are several files Randy created for Kat's work:

  - `00_development.Rmd` is Randy's package modification files

  - `00_randy_working_log` is Randy's working plan and list
  
Here are the files that Kat should use:

  - `01_setup.Rmd` is the setup for the **first time user** to run the codes

  - `02_macbook_work.Rmd` is working file for Macbook, Windows users can ignore this file

  - `03_windows_work.Rmd` is working file for Windows, Macbook users can ignore this file

## Original Notes from the Author

A set of functions that allow you to generate a range of pre-defined summary 
statistics from activPAL events files, including a breakdown of time spent 
in different classes of activity by custom time period and the distribution of 
stepping and cadence for stepping bouts of different durations.
The package also supports the generation of a visual summary of 
different physical behaviour outcomes across multiple individuals.  

Documentation on how to use these functions are provided within the package.

A data dictionary with an explanation of the column names used in the function 
outputs can be found at https://kb.palt.com/categories/data_dictionary/
