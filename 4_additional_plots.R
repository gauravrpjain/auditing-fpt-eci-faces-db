# Loading libraries
library(tidyverse)
library(tikzDevice)

# Load data for plots 
load('./combined_df.Rda') #Candidate and API response data
load('./image_df.Rda') #Image data - for plotting resolution

# Create directory for plots
dir.create('./Output/Plots')

# ---- Doing Age and Gender Plots ----

# Filtering case for successful face detection
temp <- combined_df[which(combined_df$number_faces==1),]

# Doing Age Error plot 
# Aggregating data along the Gender and Age axes 
test <- aggregate(temp$age_error,by=list(temp$Age,temp$Gender),mean)

#Saving as Latex
tikz(file = "./Output/Plots/age_plot_gender_error.tex", width = 4.5, height = 3)

age_plot <- ggplot(test,aes(Group.1,x,colour=Group.2))+
  geom_point(size=1)+theme_bw() + xlab("Age (years)") + ylab("Difference between predicted and actual age (years)")+
  labs(colour="Gender") + geom_hline(yintercept = 0)

print(age_plot)

dev.off()

# Saving a png
ggsave(age_plot,filename = "./Output/Plots/age_plot_gender_error.png",
       width = 207, height = 101, units = "mm")


rm(test)

## Doing Gender error plot
# Dropping Third Gender for Gender plot 
temp <- droplevels(temp[(temp$Gender!='T'),])
# Aggregating data along the Gender and Age axes 
test <- aggregate(temp$gender_error,by=list(temp$Age,temp$Gender),mean)

#Converting error rate into percentage for ease of reading
test$x <- 100*test$x

#Saving as Latex
tikz(file = "./Output/Plots/gender_plot_age_error.tex", width = 4.5, height = 3)

gender_plot <- ggplot(test,aes(Group.1,x,colour=Group.2))+
  geom_point(size=1)+theme_bw() + xlab("Age (years)") + ylab("Error (Percentage)")+
   scale_color_manual(name="Gender",values = c("F"="#f04546","M"="#62c76b"))#ensuring colour consistency with previous plot

print(gender_plot)

dev.off()
ggsave(gender_plot,filename = "./Output/Plots/gender_plot_age_error.png",
       width = 207, height = 101, units = "mm")

# ---- Now making Image Resolution Plot --------

#Saving as Latex
tikz(file = "./Output/Plots/resolution_plot.tex", width = 5, height = 3)

hist(image_df$ln_res,freq = F,
     xlab = "Resolution (Log Scale)",ylab = "Density",main = "")

res_plot

dev.off()

#Saving as PNG
png(filename =  "./Output/Plots/resolution_plot.png",width = 415,height = 203,units = "px")

hist(image_df$ln_res,freq = F,
     xlab = "Resolution (Log Scale)",ylab = "Density",main = "")

dev.off()
