
rsplit <- rsample::initial_split(iris)
train <- rsample::training(rsplit)
test <- rsample::testing(rsplit)
