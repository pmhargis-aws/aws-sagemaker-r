FROM rocker/tidyverse

MAINTAINER AWS

RUN R -e "install.packages('remotes', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('arules', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('bit64', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('caTools', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('combinat', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('data.table', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('e1071', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('doParallel', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('foreach', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('fBasics', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('foreach', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('forecast', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('fpp', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('ggplot2', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('gridExtra', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('here', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('itertools', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('lime', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('lubridate', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('Matrix', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('MLmetrics', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('monreg', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('nortest', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('RColorBrewer', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('recommenderlab', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('ROCR', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('pROC', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('Rcpp', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('scatterplot3d', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('stringr', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('sde', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('timeDate', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('tsoutliers', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('wordcloud', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('xgboost', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('RCurl', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('jsonlite', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('pkg', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('h2o', repos='http://h2o-release.s3.amazonaws.com/h2o/latest_stable_R')"
RUN R -e "remotes::install_github('catboost/catboost', subdir='catboost/R-package')" 
RUN R -e "remotes::install_github('AdrianAntico/RemixAutoML', dependencies=FALSE)"
RUN R -e "install.packages('reticulate', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('python3-dev', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('plumber', repos='http://cran.us.r-project.org')"

RUN apt-get update && apt-get upgrade -y
#RUN apt-get install -y libpython3-dev
RUN apt-get install -y python3 python3-dev python3-pip python3-venv
RUN pip3 install sagemaker==1.0.0

COPY myscript.R /opt/ml/plumber.R
COPY myscript.R /opt/ml/myscript.R

ENTRYPOINT ["Rscript", "/opt/ml/myscript.R", "--no-save"]