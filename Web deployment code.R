# Set up your account (get token from shinyapps.io)
library(rsconnect)
library(here)

rsconnect::setAccountInfo(name='fins-app',
                          token='23726099B197FA2EA001CD541320F18E',
                          secret='e3Gr9jYlVsPCfSbG/43TZDem0ucL+Hxs66e2pRdQ')

# Check if data and branding exist in project root
list.files(here::here())

# Move data folder into Code/
file.copy(from = here::here("data"), 
          to = here::here("Code"), 
          recursive = TRUE)

# Move branding folder into Code/
file.copy(from = here::here("branding"), 
          to = here::here("Code"), 
          recursive = TRUE)

# Verify they're there now
list.files(here::here("Code"), recursive = TRUE)

# Deploy the app from the Code folder
rsconnect::deployApp(
  appDir = here::here("Code"),
  appName = "FINS_App",
  account = "fins-app",  # Add this line to specify the account
  forceUpdate = TRUE
)

