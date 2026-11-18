library(dplyr)
library(tidyr)
library(quantmod)
library(ggplot2)

rm(df)
df = read.csv('data/OUT/Pos+quotes+volume+spread.csv')
df = df[!(duplicated(df$isin) & duplicated(df$close)), ]
rm(data)
rm(df_f)

unique(df$isin)

ISIN = 'ROSN'
df_f = subset(df, isin == ISIN)

ruonia = readxl::read_xlsx('data/RC_F01_01_2023_T16_02_2024.xlsx')
ruonia = ruonia[,1:2]
names(ruonia)[1] = 'moment'
ruonia$moment <- as.character(ruonia$moment)

df_joined <- left_join(df_f, ruonia, by = "moment")
df_joined = fill(df_joined,ruo, .direction = 'up')


# expirations dates num rows = 

find_changed_rows <- function(dataframe, column_name) {
  changed_rows <- which(dataframe[[column_name]] != dplyr::lag(dataframe[[column_name]], default = NA))
  return(changed_rows)
}
changed_rows <- find_changed_rows(df_joined, "instrument")
print(changed_rows)

changed_rows = c(0,changed_rows,nrow(df_joined))
i = 0
k=0
for (k in 2:5){
  for (i in changed_rows[k-1]:(changed_rows[k])) {
    df_joined$yield[i] = ((df_joined$ruo[i]/100/365)+1)^(changed_rows[k]-i)
  }
}

quotes = read.csv('data/OUT/shares_quotes.csv')
names(quotes)[4] = 'moment'
names(quotes)[2] = 'isin'

df_joined <- left_join(df_joined, quotes, by = c('isin',"moment"))
df_joined <- subset(df_joined, select = -c(X.y,X.x))

df_joined$estim_price = df_joined$CLOSE * 100 * df_joined$yield
df_joined$contrango = df_joined$close - df_joined$estim_price

df_for_graph = subset(df_joined, select = c(moment,isin,long_position,long_position_jur,close,estim_price,contrango))



# Graph
{
  par(mfrow = c(2, 1), mar = c(0, 4, 4, 5))
  plot(as.Date(df_for_graph$moment), df_for_graph$close, type = "l", xlab = "Date", ylab = "price", col = "blue")
  
  
  par(new = TRUE)
  plot(as.Date(df_for_graph$moment), df_for_graph$long_position, type = "l", xlab = "", ylab = "", col = "red", axes = FALSE,ylim = c(0, max(df_for_graph$long_position)))
  
  axis(side = 4)
  mtext("open position",side = 4, line = 3)
  
  # Добавляем третью линию на правую ось Y
  lines(as.Date(df_for_graph$moment), df_for_graph$long_position_jur, col = "green")
  
  # Создаем легенду
  legend("topleft", legend = c("close price", "long pos", "long pos legal entities"), col = c("blue", "red", "green"), lty = 1,cex=0.5)

  par(mar = c(7, 4, 4, 5))  # Установим другие размеры полей для второго графика
  
  plot(as.Date(df_for_graph$moment), df_for_graph$contrango, type = "l", col = "red", xlab = "", ylab = "contrango")
  abline(h = 0, col = "blue")
}


