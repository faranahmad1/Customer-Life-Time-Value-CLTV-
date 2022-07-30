# Customer-Life-Time-Value-CLTV-
Customer lifetime value (CLV) is a business metric that measures how much a business can plan to earn from the average customer over the course of the relationship.
Why it is so important?

It tells that how valuable a customer is to your business. It directly affects your revenue. It increases customer loyalty and retention rate. It reduces customer acquisition costs. Every business wants a higher number of customers in order to make a high profit. We always have to spend money on acquiring new customers and retaining existing ones. So when we know our customer lifetime value, we can improve it. We work on retaining our customers and acquiring new ones through different campaigns like email, SMS, and social media marketing. At this point, CLV tells us that the money you spent on the customers is really worth it and how much can we comfortably spend on marketing and customer acquisition costs (Cost required in convincing people to buy your products or service). If a business is spending too much money on customer acquisition costs (CAC), but its customers are not profitable for the business, then it leads to losses which definitely affect the business. Once we calculate CLV, we can focus on optimizing this ratio which ensures that our business continues to grow at a healthy rate.

How to Calculate Customer Lifetime Value?

Several distinct strategies exist to compute CLV. The way in which we calculate customer lifetime value can likewise fluctuate in view of our business model. For example, it's simpler to compute CLV assuming we have a membership model than we you're in a web-based business. That is on the grounds that deals become more prescient. Before we start calculating CLV, there are a few things we need to know:

Average Purchase Value
It is the average value of the customers' transactions.

For example, I have the dataset which contains the variable Unit Price and Quantity of products a customer bought. So, I calculated the total transaction value or purchase values of each customer. I used the R Studio to perform these calculations. Firstly, I multiplied the Unit Price with Quantity to find the total amount of each transaction. Secondly, I used the group by and summarize function to find purchase values for each customer. And lastly, I found the mean of all customers' purchase values.

Average Purchase Frequency
This is the average number of transactions a customer makes over a given time period.

Purchase frequency can be calculated by dividing the average number of purchases by the average number of customers.

For example, I used invoice date and invoice no of each customer in this example dataset to find the average purchase frequency. For this, I used the strptime and lubridate functions because R by default treat every character as string , strptime function is used to tell R to treat invoice date column as numeric also by specifying format and lubridate function makes data and time easy read. Furthermore, I did some cleaning of dataset by using drop_na function which removes empty observations. Finally, I calculated the average number of purchases by using InvoiceNo variable (n_distinct for counting unique InvoiceNo and summarise for aggregating the mean of total transactions of all customers).

Average Customer Lifespan
This is the average time of customer relationship. In other words, it is the average number of days between first order date and last order date of all of our customers.

A business that retains its customers well may have a customer lifespan of 5 years, while one that isn’t good at retaining its customers may have a lifespan of 6 months, or 0.5 years.

For example, I again used the invoice data variable, which refers to the transaction date, to calculate the first transaction and last transaction date of each customers by using max, min, and difftime functions in R Studio. The unit of lifespan I used here is days which then I converted into months. Finally, I calculated the mean of all customer's lifespan.

Average Gross Margin
Gross Margin tells you what part of each customer purchase is profit and what part is cost. Average gross margin can be calculated with the following formula:

Average Gross Margin = (Total Revenue – Cost of Sales) ÷ (Total Revenue)

For example, the average gross margin for the previous example dataset, which we calculated by the above formula, is 0.3

Customer Acquisition Cost(CAC)
We have already discussed it that it is the average amount you spend on acquiring a customer, and includes everything from marketing and advertising.

###General Formula to calculate CLV###

Once we have the above information, calculating CLV is easy. Just multiply our average purchase value with average gross margin, purchase frequency, and customer lifespan. Finally, subtract with cost of acquisition(CAC).

CLV = (Average Purchase Value × Gross Margin × Average Purchase Frequency × Average Customer Lifespan) – CAC 
if our CLV is negative then customers are costing you more to serve than customers are making you in revenue, so we can actively work to stop selling to those customers.
