# Base R Shiny image
FROM rocker/shiny

# Make a directory in the container
RUN mkdir /home/shiny-app

# Install R dependencies
RUN R -e "install.packages(c('shiny', 'bslib', 'shinythemes', 'readxl', 'DT', 'dplyr', 'ggplot2', 'stats', 'MASS'))"

# Copy the Shiny app code
COPY app.R /home/shiny-app/app.R

# Expose the application port
EXPOSE 1032

# Run the R Shiny app
CMD ["Rscript","/home/shiny-app/app.R"]
