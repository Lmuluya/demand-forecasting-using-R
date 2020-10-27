# Demand forecasting and trends using
# Iam using the whole sale customer from https://archive.ics.uci.edu/ml/datasets/wholesale+customers

#Attribute Information:
  
#1) FRESH: annual spending (m.u.) on fresh products (Continuous);
#2) MILK: annual spending (m.u.) on milk products (Continuous);
#3) GROCERY: annual spending (m.u.)on grocery products (Continuous);
#4) FROZEN: annual spending (m.u.)on frozen products (Continuous)
#5) DETERGENTS_PAPER: annual spending (m.u.) on detergents and paper products (Continuous)
#6) DELICATESSEN: annual spending (m.u.)on and delicatessen products (Continuous);
#7) CHANNEL: customers’ Channel - Horeca (Hotel/Restaurant/Café) or Retail channel (Nominal)
#8) REGION: customers’ Region – Lisnon, Oporto or Other (Nominal)

#Descriptive Statistics:
  
#(Minimum, Maximum, Mean, Std. Deviation)
#FRESH ( 3, 112151, 12000.30, 12647.329)
#MILK (55, 73498, 5796.27, 7380.377)
#GROCERY (3, 92780, 7951.28, 9503.163)
#FROZEN (25, 60869, 3071.93, 4854.673)
#DETERGENTS_PAPER (3, 40827, 2881.49, 4767.854)
#DELICATESSEN (3, 47943, 1524.87, 2820.106)


# importing the dataset
RawDataset <- read.csv("Wholesale customers data.csv")
RawDataset

# Data Exploration
str(RawDataset)

# Summary distribution of attributes
summary(RawDataset)

# Getting the spending distributions
par(mfrow = c(2,3))
for(i in c(3:8))
  hist(RawDataset[,c(i)],breaks = 200, main = colnames(RawDataset)[i],xlab = "Annual Spendings",ylab = "Number of customers")

# Normalisation of the dataset and discritisation 

normalisedData <- cbind(RawDataset[,c(2,1)],scale(RawDataset[,c(3:8)]))
str(normalisedData)
summary(normalisedData)

# Visualising the normalised dataset
par(mfrow = c(2,3))
for (i in c(3:8))
  hist(normalisedData[,c(i)],breaks = 200, main = colnames(RawDataset)[i],xlab = "Normalised Annual Spending",ylab = "Number of Customers")
  
    RawDataset <- within(RawDataset,FreshQunitile <- as.integer(cut(Fresh,quantile(Fresh,probs =
                                                                                      0:5/5),include.lowest = TRUE)))
  


