dd <- read.csv("C:/Users/Gerard/Downloads/dd.csv",header=T)
num_rows <- nrow(dd)

for (i in 1:num_rows) {
  if (is.na(dd[i, "arrival_date_week_number"])) {
	y = dd[i, "arrival_date_year"]
	month = dd[i, "arrival_date_month"]
	m = match(month, month.name)
	d = dd[i, "arrival_date_day_of_month"]
	date = sprintf("%s-%s-%s", y, m, d)
	week_num = strftime(date, format="%V")
	dd[i, "arrival_date_week_number"] <- week_num
  }
}

for (i in 1:num_rows) {
  if (is.na(dd[i, "country"])) {
	dd[i, "country"] <- "Unknown"
  }
}

for (i in 1:num_rows) {
  if (is.na(dd[i, "meal"])) {
	dd[i, "meal"] <- "Unknown"
  }
}
	 
write.csv(dd, "C:/Users/Gerard/Downloads/dd_filled_quali.csv", row.names = FALSE)print(missing_values)
