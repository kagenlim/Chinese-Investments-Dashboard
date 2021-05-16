library(plotly)
library(tidyr)
library(dplyr)
library(ggmap)
library(countrycode)
library(tmap)
library(geojsonio)

china_africa_stock = readxl::read_xlsx("./data/FDIData_08Jan2021.xlsx", skip=1, n_max=30)
china_africa_stock = china_africa_stock %>% select(where(function(x) !all(is.na(x)))) %>% rename("year"="US$ mn, unadjusted")
china_africa_flow = readxl::read_xlsx("./data/FDIData_08Jan2021.xlsx", skip=1, n_max=30, sheet=2)
china_africa_flow = china_africa_flow %>% select(where(function(x) !all(is.na(x)))) %>% rename("year"="US$ mn, unadjusted")

china_africa_sector = readxl::read_xlsx("./data/FDIData_08Jan2021.xlsx", skip=1, n_max=31, sheet=3)
china_africa_sector %>% select(where(function(x) x[1]!="%"))
china_africa = cbind(china_africa_stock["year"], china_africa_stock["Total, US$ bn"], china_africa_flow["Total, US$ bn"])
names(china_africa) = c("year", "stock", "flow")

us_africa = readxl::read_xlsx("./data/FDIData_08Jan2021.xlsx", skip=1, n_max=30, sheet=6)
us_africa = us_africa %>% rename("year"="US$ bn, unadjusted", "flow"="Flows*", "stock"="Stock")


china_africa_long = pivot_longer(china_africa, 2:3, names_to="fdi_type", values_to="investment_amount")
china_africa_long$country="China"

us_africa_long = pivot_longer(us_africa, 2:3, names_to="fdi_type", values_to="investment_amount")
us_africa_long$country="US"

totals = rbind(china_africa_long, us_africa_long)

stock_plot = ggplot(filter(totals, fdi_type=="stock", year >=2003), aes(x=year, y=investment_amount, fill=country)) +
  geom_bar(aes(fill=country), stat="identity") +
  ggthemes::theme_tufte() +
  labs(x= "Year", y="Investment amount in billions of USD, unadjusted",
       title="FDI in Africa by Year: Stock") + 
  scale_x_continuous(breaks=2003:2019) + 
  scale_y_continuous(labels=scales::unit_format(unit="b"))

ggplotly(stock_plot)

flow_plot = ggplot(filter(totals, fdi_type=="flow", year >= 2003), aes(x=year, y=investment_amount, fill=country)) +
  geom_bar(aes(fill=country), stat="identity") +
  ggthemes::theme_tufte() +
  labs(x= "Year", y="Investment amount in billions of USD, unadjusted",
       title="FDI in Africa by Year: Flow") + 
  scale_x_continuous(breaks=2003:2019) +
  scale_y_continuous(labels=scales::unit_format(unit="b"))

ggplotly(flow_plot)



# ggplot(nations, aes(x=factor(decade), fill=nationality)) +
#   geom_bar(aes(fill=nationality), stat="count") + 
#   geom_text(aes(label=..count..), stat="count", position=position_stack(1)) + 
#   theme_minimal() +
#   labs(x = "Decade", y = "Number of Astronauts", 
#        title = "Composition of Astronaut Nationalities by Decade",
#        caption = "Source: Astronaut Database (Stavnichuk & Corlett 2020)") + 
#   guides(fill=guide_legend(title="Nationality:")) + 
#   theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5), 
#         legend.position = "top", 
#         legend.title = element_text(size = 9))

# x_intercept = as.Date("2020-03-14")
# covid_effect_pltly = plot_ly(count_by_date, x=~date, y=~cnt, type="scatter", mode="lines") %>%
#   add_trace(x=x_intercept, type="scatter", mode="lines",
#             line=list(color="red", dash="dash"), showlegend=FALSE) %>%
#   add_annotations(x=x_intercept, y=.7, yref="paper",
#                   text="Statewide stay-at-home order issued") %>%
#   layout(
#     title="Parking Violations in NY over 2020",
#     xaxis=list(title="Date", range=c("2020-01-01", "2021-2-28"),
#                rangeslider=list(type="date")),
#     yaxis=list(title="Count")
#   )
# 
# covid_effect_pltly


# tickets_plt =  tm_shape(nypp_man) + 
#   tm_borders() +
#   tm_fill("total.tickets", title="Ticket Count", convert2density=FALSE) +
#   tm_text("Precinct", size=.5) +
#   tm_layout(title="Number of tickets by precinct",
#             legend.title.size = 1,
#             legend.text.size = 0.5,
#             legend.bg.color = "white",
#             legend.bg.alpha = 1,
#             legend.width = 2,
#             legend.position = c(.7, .1),
#             bg.color="white",
#             frame=FALSE)
# 
# tickets_plt


china_flow_long = china_africa_flow %>% pivot_longer(2:54, names_to="country", values_to="flow_investment") %>%
  filter(!is.na(flow_investment), country!="Regional", country !="Total, US$ mn") %>% select(year, country, flow_investment)

china_stock_long = china_africa_stock %>% pivot_longer(2:54, names_to="country", values_to="stock_investment") %>%
  filter(!is.na(stock_investment), country!="Regional") %>% select(year, country, stock_investment)
china_stock_long$iso = countrycode(china_stock_long$country, origin="country.name", destination="iso3c")

china_stock_wide = china_stock_long %>% pivot_wider(id_cols=c(country, iso), names_from=year, values_from="stock_investment")
test = africa_geo@data %>% left_join(china_stock_wide, by=c("gu_a3"="iso"))

africa_geo = geojson_read("./data/africa.geo.json", what="sp")
africa_geo@data = africa_geo@data %>% left_join(china_stock_wide, by=c("gu_a3"="iso"))


m = tm_shape(africa_geo) +
  tm_borders() +
  tm_text("country", size = .5) +
  tm_fill("2019", title="Millions USD", convert2density = FALSE) +
  tm_layout(
    main.title="2019 China FDI Heatmap: Stock",
    frame=FALSE
  )
m



### Global pattern holds in Africa as well

```{r}
# libraries
library(plotly)
library(tidyr)
library(dplyr)
library(ggmap)
library(countrycode)
library(tmap)
library(geojsonio)

# data manipulation
china_africa_stock = readxl::read_xlsx("./data/FDIData_08Jan2021.xlsx", skip=1, n_max=30)
china_africa_stock = china_africa_stock %>% select(where(function(x) !all(is.na(x)))) %>% rename("year"="US$ mn, unadjusted")
china_africa_flow = readxl::read_xlsx("./data/FDIData_08Jan2021.xlsx", skip=1, n_max=30, sheet=2)
china_africa_flow = china_africa_flow %>% select(where(function(x) !all(is.na(x)))) %>% rename("year"="US$ mn, unadjusted")

china_africa_sector = readxl::read_xlsx("./data/FDIData_08Jan2021.xlsx", skip=1, n_max=31, sheet=3)
china_africa_sector %>% select(where(function(x) x[1]!="%"))
china_africa = cbind(china_africa_stock["year"], china_africa_stock["Total, US$ bn"], china_africa_flow["Total, US$ bn"])
names(china_africa) = c("year", "stock", "flow")

us_africa = readxl::read_xlsx("./data/FDIData_08Jan2021.xlsx", skip=1, n_max=30, sheet=6)
us_africa = us_africa %>% rename("year"="US$ bn, unadjusted", "flow"="Flows*", "stock"="Stock")


china_africa_long = pivot_longer(china_africa, 2:3, names_to="fdi_type", values_to="investment_amount")
china_africa_long$country="China"

us_africa_long = pivot_longer(us_africa, 2:3, names_to="fdi_type", values_to="investment_amount")
us_africa_long$country="US"

totals = rbind(china_africa_long, us_africa_long)
```


```{r}
stock_plot = ggplot(filter(totals, fdi_type=="stock", year >=2003), aes(x=year, y=investment_amount, fill=country)) +
  geom_bar(aes(fill=country), stat="identity") +
  ggthemes::theme_tufte() +
  labs(x= "Year", y="Investment amount in billions of USD, unadjusted",
       title="FDI in Africa by Year: Stock") + 
  scale_x_continuous(breaks=2003:2019) + 
  scale_y_continuous(labels=scales::unit_format(unit="b"))

ggplotly(stock_plot)
```

```{r}
flow_plot = ggplot(filter(totals, fdi_type=="flow", year >= 2003), aes(x=year, y=investment_amount, fill=country)) +
  geom_bar(aes(fill=country), stat="identity") +
  ggthemes::theme_tufte() +
  labs(x= "Year", y="Investment amount in billions of USD, unadjusted",
       title="FDI in Africa by Year: Flow") + 
  scale_x_continuous(breaks=2003:2019) +
  scale_y_continuous(labels=scales::unit_format(unit="b"))

ggplotly(flow_plot)
```

```{r}
top_10_stock = china_stock_long %>% group_by(country) %>% summarize(max=max(stock_investment)) %>% arrange(desc(max)) %>% head(10) %>% select(country) %>% unlist()
by_country_stock = china_stock_long %>% filter(country %in% top_10_stock) %>%
  plot_ly(x=~year, y=~stock_investment, split=~country, type = "scatter", mode='lines') %>%
  # add_annotations(x=0, y=-.1, xref="paper", yref="paper", text=caption, showarrow=FALSE) %>%
  layout(
    title="Top 10 most invested African countries by year: Stock",
    yaxis=list(title="Investment Amount (US$ million, unadjusted)")
  )
by_country_stock
```



top_10_stock = china_stock_long %>% group_by(country) %>% summarize(max=max(stock_investment)) %>% arrange(desc(max)) %>% head(10) %>% select(country) %>% unlist()
by_country_stock = china_stock_long %>% filter(country %in% top_10_stock) %>%
  plot_ly(x=~year, y=~stock_investment, split=~country, type = "scatter", mode='lines') %>%
  # add_annotations(x=0, y=-.1, xref="paper", yref="paper", text=caption, showarrow=FALSE) %>%
  layout(
    title="Top 10 most invested African countries by year: Stock",
    yaxis=list(title="Investment Amount (US$ million, unadjusted)")
  )
by_country_stock

top_10_flow = china_flow_long %>% group_by(country) %>% summarize(max=max(flow_investment)) %>% arrange(desc(max)) %>% head(10) %>% select(country) %>% unlist()
by_country_flow = china_flow_long %>% filter(country %in% top_10_flow) %>%
  plot_ly(x=~year, y=~flow_investment, split~country, type="scatter", mode="lines") %>%
  layout(
    title="Top 10 most invested African countries by year: Flow",
    yaxis=list(title="Investment Amount (US$ million, unadjusted)")
  )
by_country_flow

countries = unique(stock_long$country)
geocoded = geocode(countries)

basic
register_google(Sys.getenv("GOOGLE_KEY"))
af_coord = geocode("africa")

basic = china_stock_long %>% group_by(year) %>%
  plot_ly(x=stock$year, type="scatter", mode="lines") %>%
  add_lines(y=~stock_investment, split=~=country) %>%
  # layout(
  #   title="Chinese FDI Stock in African Countries by year"
  # )

basic

stock_long %>%
  plot_ly(x=~year, y=~stock_investment, split=~country, type = "scatter", mode='lines', 
          transforms = list(list(type = 'filter',
                                 target = ~country,
                                 operation = 'in',
                                 value = stock_long$country[1]
    )))
  
ggplot()
