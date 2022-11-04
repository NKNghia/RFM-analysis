library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(knitr)
library(rmarkdown)
library(kableExtra)
library(rfm)
library(factoextra)
library(Hmisc)



#Doc du lieu
ecommerce <- read.csv('E:/R/data.csv')
glimpse(ecommerce)

#Tinh Revenue cua tung khach hang
ecom <- mutate(ecommerce[,c(-2:-3, -8)], # gets only columns we need
               Revenue = UnitPrice*Quantity, # creates revenue
               InvoiceDate =as.Date(InvoiceDate, # reformats data format
                                    format='%m/%d/%Y'))

ecom %>% 
  head() %>% 
  kable() %>% 
  kable_minimal()

#Tinh  tu phan vi
summary(ecom) %>% 
  kable() %>% 
  kable_minimal()

#Xu ly du Null
ecom %>% 
  filter(UnitPrice <= 0 | Quantity <= 0 ) %>% 
  arrange(UnitPrice)  %>% 
  head(10) %>% 
  kable() %>% 
  kable_classic_2()

ecom_clean <- ecom %>%  
  drop_na(CustomerID) %>% # Removes entries with out customer information
  filter(Quantity > 0,
         UnitPrice > 0 )  #Removes entries with negative quantity or Unit Price


#Tinh chi so RFM
rfm_result <- rfm_table_order(
  data = ecom_clean,
  customer_id = CustomerID,
  revenue = Revenue,
  order_date = InvoiceDate, 
  analysis_date = as.Date("2022/01/01") 
)

df_RFM <- ecom_clean %>% 
  group_by(CustomerID) %>% 
  summarise(recency=as.numeric(as.Date("2022-01-01")-max(InvoiceDate)),
            frequenci=n_distinct(InvoiceNo), monitery= sum(Revenue)/n_distinct(InvoiceNo)) 

df_RFM

#Truc quan hoa du lieu
#Bieu do tuong quan cua RM va FM
rfm_heatmap(rfm_result) 

rfm_rm_plot(rfm_result) 

rfm_fm_plot(rfm_result)

hist(df_RFM$monitery)

hist(df_RFM$recency)

segment_names <- c("Champions", "Loyal Customers", "Potential",
                   "New Customers", "Promising", "Need Attention",
                   "At Risk", "Lost")

# We set the upper and lower bounds for recency, frequency, and monetary for the above segments
recency_lower <- c(4, 2, 3, 4, 3, 2, 1, 1)
recency_upper <- c(5, 5, 5, 5, 4, 3, 2, 2)
frequency_lower <- c(4, 3, 1, 1, 1, 2, 2, 1)
frequency_upper <- c(5, 5, 3, 1, 1, 3, 5, 2)
monetary_lower <- c(4, 3, 1, 1, 1, 2, 2, 1)
monetary_upper <- c(5, 5, 3, 1, 1, 3, 5, 2)

# We use the segments and the bounds we previously established to group our users into different segments
segment <- rfm_segment(rfm_result,
                       segment_names,
                       recency_lower,
                       recency_upper,
                       frequency_lower, 
                       frequency_upper, 
                       monetary_lower,
                       monetary_upper)


head(segment) %>% 
  kable() %>% 
  kable_classic_2()
rfm_plot_median_monetary(segment)

rfm_plot_median_frequency(segment)

#Xet ngoai lai
df_RFM %>% 
  ggplot(aes(CustomerID, monitery)) + 
  geom_col() + 
  theme(panel.grid.minor.x = element_blank()) + 
  theme(panel.grid.major.x = element_blank()) + 
  theme(panel.grid.minor.y = element_blank()) + 
  theme(axis.text.x = element_blank()) + 
  labs(title = " Spending by Customer", y = "")

df_RFM %>% 
  mutate(frequenci = frequenci / 1000) %>% 
  ggplot(aes(CustomerID, frequenci)) + 
  geom_col() + 
  theme(panel.grid.minor.x = element_blank()) + 
  theme(panel.grid.major.x = element_blank()) + 
  theme(panel.grid.minor.y = element_blank()) + 
  theme(axis.text.x = element_blank()) + 
  labs(title = "Frequency by Customer", y = "")

# Hàm xác minh Outlier: 
outlier_label <- function(x) {
  a <- mean(x)
  b <- sd(x)
  th1 <- a - 3*b
  th2 <- a + 3*b
  y <- case_when(x >= th1 & x <= th2 ~ "Normal", TRUE ~ "Outlier")
  return(y)
  
}

# Ch??? s??? d???ng Normal Observation cho thu???t toán K-means Clustering: 

df_RFM %>% 
  mutate(nor_money = outlier_label(monitery), nor_freq = outlier_label(frequenci)) %>% 
  filter(nor_money == "Normal", nor_freq == "Normal") %>% 
  select(1:4) -> final_df_normal

final_df_normal %>% 
  mutate_if(is.numeric, function(x) {(x - min(x)) / (max(x) - min(x))}) -> final_df_normal_scaled


#Xac dinh so cum K
set.seed(29)
wss <- sapply(1:10, 
              function(k){kmeans(final_df_normal_scaled %>% select(-CustomerID) %>% sample_frac(0.2), 
                                 k, nstart = 30)$tot.withinss})


u <- data.frame(k = 1:10, WSS = wss)

u %>% 
  ggplot(aes(k, WSS)) + 
  geom_line() + 
  geom_point() + 
  geom_point(data = u %>% filter(k == 4), color = "red", size = 3) + 
  scale_x_continuous(breaks = seq(1, 10, by = 1)) + 
  labs(title = " The Optimal Number of Clusters, Elbow Method", 
       subtitle = "Outliers are are removed from sample.", 
       x = "Number of Clusters (K)") + 
  theme(panel.grid.minor = element_blank())

set.seed(123)
km.res4 <- kmeans(final_df_normal_scaled %>% select(-CustomerID), 4, nstart = 30)

# S??? d???ng k???t qu??? phân c???m: 
final_df_normal %>% 
  mutate(Group = km.res4$cluster) %>% 
  mutate(Group = paste("Group", Group)) -> final_df_clustered

final_df_clustered %>% 
  group_by(Group) %>% 
  summarise_each(funs(mean), recency, frequenci, monitery) %>% 
  ungroup() %>% 
  mutate_if(is.numeric, function(x) {round(x, 0)}) %>% 
  arrange(-monitery) %>% 
  kable()

final_df_clustered %>% 
  group_by(Group) %>% 
  summarise_each(funs(sum, mean, median, min, max, sd, n()), monitery) %>% 
  ungroup() %>% 
  mutate(per_sale = round(100*sum / sum(sum), 2)) -> sale_group

dd <- cbind(df_RFM, cluster = km.res$cluster)
View(dd)

library(ggthemes)

sale_group %>% 
  ggplot(aes(reorder(Group, per_sale), per_sale, fill = Group, color = Group)) + 
  geom_col(width = 0.5, show.legend = FALSE) + 
  coord_flip() + 
  geom_text(aes(label = paste(per_sale, paste0(paste0("(", "%")), ")")), 
            hjust = -0.05, color = "white", size = 5) + 
  scale_y_continuous(limits = c(0, 90), expand = c(0.01, 0)) + 
  scale_fill_tableau() + 
  scale_color_tableau() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) + 
  theme(panel.grid.major = element_blank()) + 
  theme(panel.grid.minor = element_blank()) + 
  labs(x = NULL, title = "Share of Sales by Customer Group")

sale_group %>% 
  select(Group, n) %>% 
  mutate(total = sum(n)) %>% 
  mutate(label = 100*n / total) %>% 
  mutate(label = paste(round(label, 1), "%")) %>% 
  ggplot(aes(Group, n, fill = Group, color = Group)) + 
  geom_col(width = 0.5, show.legend = FALSE) + 
  geom_text(aes(label = label), color = "white", vjust = 1.4, size = 5) + 
  scale_fill_tableau() + 
  scale_color_tableau() + 
  theme(panel.grid.minor = element_blank()) + 
  theme(panel.grid.major.x = element_blank()) + 
  labs(x = NULL, y = NULL, title = "Number of Customers by Group")