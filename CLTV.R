#lifespan
SalesData <- read.csv("storeData.csv")
SalesData |> 
  mutate(subtotal = Quantity*UnitPrice,
         invoicedate = strptime(InvoiceDate, format = "%m/%d/%Y %H:%M"))|>
  group_by(CustomerID) |>
  summarise(total_revenue = sum(subtotal),
            lifespan = difftime(max(invoicedate), min(invoicedate), units = "days")) |> 
  mutate(lifespan = round(as.integer(lifespan)/30)) |>
  filter(total_revenue> 0) |> 
  summarise(avg_lifespan= round(mean(lifespan)))  -> avg_lifespan
  


#average purchase frequency
SalesData |> 
  mutate(invoicedate = strptime(InvoiceDate, format = "%m/%d/%Y %H:%M"),
         month = lubridate::month(invoicedate,label = TRUE,abbr = TRUE), 
         year = lubridate::year(invoicedate)) |> 
  drop_na() |>
  group_by(CustomerID,month,year) |> 
  summarise(n_transactions = n_distinct(InvoiceNo)) |>
  ungroup() |>
  summarise(avg_purchase_frequency = mean(n_transactions)) -> avg_purchase_frequency
  

#avg purchase value 
SalesData |> 
  mutate(subtotal= Quantity*UnitPrice) |> 
  group_by(InvoiceNo) |> 
  summarise(total= sum(subtotal)) |>
  summarise(avg_purchase_value= mean(total)) -> avg_purchase_value
              

CLV= (avg_purchase_value$avg_purchase_value) * (0.3) * (avg_purchase_frequency$avg_purchase_frequency) * (avg_lifespan$avg_lifespan)

