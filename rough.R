car_price[2,5]
car_price[2,5]/2
car_price[2,7]
car_price[2,7]/2
car_price[2,13]
car_price[2,13]/2

#write column into datset where age of car is shown (current year-reg year)

car_price$age <- with(car_price, 2018-year)