#' Ping to show server is there
#' @get /ping
function() {
    return('')}


#' Parse input and return prediction from model
#' @param req The http request sent
#' @post /invocations
function(req) {

    # Setup locations
    prefix <- '/opt/ml'
    model_path <- paste(prefix, 'model', sep='/')

    # Bring in model file and factor levels
    load(paste(model_path, 'model', sep='/'))

    # Read in data
    conn <- textConnection(gsub('\\\\n', '\n', req$postBody))
    data <- read.csv(conn)
    close(conn)

    # Convert input to model matrix
    scoring_X <- model.matrix(~., data)

    # Return prediction
    return(paste(predict(model, scoring_X, row.names=FALSE), collapse=','))}