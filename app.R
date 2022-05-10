# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

Sys.setenv(R_CONFIG_ACTIVE = "1kD", # Replace "default" with your config
           R_CONFIG_FILE = "inst/config.yml")
pkgload::load_all()
options("golem.app.prod" = TRUE)
dccmonitor::run_app() # add parameters here (if any)
