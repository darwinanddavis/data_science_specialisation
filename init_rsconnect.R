# install.packages('rsconnect')
require(rsconnect)
require(here)
# rsconnect::setAccountInfo(name='darwinanddavis', token='BD52C7D2B5250ECAA968DB1926B6C78B', secret='5bOp4tMxCXMS0X+mrX7y/bGEs97GyIDvy2ZnhZ+2')
rsconnect::deployApp(here::here("data_products","week4","airbnb")) 
Y