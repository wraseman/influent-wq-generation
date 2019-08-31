The figures in this directory were created to illustrate systematic data collection errors identified during the data quality control process. 

To generate the figures, run the following lines of code with 'show_plots == TRUE' from 01_import_clean.R: 

if (show_plots == TRUE) {
## plot Q-Q plots of the data (warning: takes quite a bit of time to plot)
ggplot(wq2_df, aes(sample=pH_intake)) +
  stat_qq()  ## suspicious values at 14.0 and below 6.0

ggplot(wq2_df, aes(sample=pH_plant)) +
  stat_qq()  ## suspicious values at 14.0 and below 6.0 

ggplot(wq2_df, aes(sample=temp_intake)) +
  stat_qq()  ## no suspicious values

ggplot(wq2_df, aes(sample=temp_plant)) +
  stat_qq()  # suspicious repeated values at 30 deg C (try eliminating values > 28)

# view time series of suspicious values 
## plant intake: 2014-09-30
ggplot(data=filter(wq2_df, date==as.Date("2014-09-30")),  
			  aes(x=datetime, y=pH_intake)) +
  geom_point()

## plant intake: 2013-12-15
ggplot(data=filter(wq2_df, date==as.Date("2013-12-16")),  
	   aes(x=datetime, y=pH_plant)) +
  geom_point()

## plant intake: 2013-12-18
ggplot(data=filter(wq2_df, date==as.Date("2013-12-18")),  
	   aes(x=datetime, y=pH_plant)) +
  geom_point()
}