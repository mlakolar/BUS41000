
packageNames = c(
	"shiny",
	"ggplot2",
	"knitr",
	"RJSONIO",
	"plyr",
	"lubridate",
	"stringr",
	"maptools",
	"ellipse",
	"openintro",
	"tigerstats"
)

for (pkgName in packageNames) {
  if (! (pkgName %in% rownames(installed.packages()))) { 
    install.packages(pkgName,
                     dependencies=TRUE,
                     repos='http://cran.rstudio.com') 
  }  
}

update.packages(ask=FALSE,	
                repos='http://cran.rstudio.com')


