# Transpose

R Shiny app to translate text across languages, and show alignment. The project was funded by the AHRC Creative Multilingualism group ![](https://www.eurolitnetwork.com/wp-content/uploads/2018/07/creative-multilingualism-logo.jpg)

Full instructions are provided on the front page. The app may be accessed [here](https://nickriches.shinyapps.io/transpose_shiny/), but please bear in mind that as this is a free account there may be usage restrictions.

The code is published under creative commons, and you may set up the app on your own server. To do this you will require a certain amount of technical knowledge, in particular knowledge of the R programming language. You can host the app for free on [shinyapps](https://rstudio.com/products/shinyapps/). To do this, you need to

(a) Set up a [shiny app account](https://rstudio.com/products/shinyapps/)

(b) [Install R](https://www.r-project.org/) and [RStudio](https://rstudio.com/products/rstudio/download/) on your computer

(c) [Create a "Shiny app" project](https://shiny.rstudio.com/tutorial/) in Rstudio

(d) Clone all of the files in this github repository into your Shiny app folder.

(e) [Upload](https://shiny.rstudio.com/deploy/) these to your shiny app account (within RStudio)

Please also note that to run properly you need to obtain an API key from Google to access its translation services. See [this site](https://developers.google.com/maps/documentation/embed/get-api-key), and [this site](https://blog.weglot.com/google-translate-api-key/). The name of the key needs to entered into the `app.R` code (look for the location in the code which refers to a ".json" file) and it needs to be stored in the same directory as this file.
