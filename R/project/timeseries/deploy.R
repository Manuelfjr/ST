library(rsconnect)
addServer('https://mfjr.shinyapps.io/','timeseries')
rsconnect::setAccountInfo(name='mfjr',
                          token='5B156E304307492E983E20A57BC37AF3',
                          secret='Iq8zqlCejm+4F9aIHK8XXh8QE1aG4M0pFrIbwCvh')
rsconnect::deployApp('/home/manuel/Área de Trabalho/git/UFPB/ST/R/project/timeseries',
                     appName = 'timeseries',
                     account='mfjr',
                     server='shinyapps.io',
                     forceUpdate = getOption("rsconnect.force.update.apps", TRUE))
