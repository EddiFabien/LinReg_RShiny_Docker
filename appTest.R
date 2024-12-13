library(shinytest2)

# Change directory
setwd("/home/eddi/Desktop/Personnal_project/projet_Ricco_Rakotomalala/my-app/app")

# Record a test
record_test()

# Run the test
shinytest2::test_app()


