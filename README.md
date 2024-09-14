> activity of new antibacterial products on priority
> pathogens![](vertopal_2604346bc6fb4e008a0cbe2012e3cc69/media/image3.png){width="10.647222222222222in"
> height="6.016525590551181in"}

**Authors**

> **AuthorsClaudia Agyeere(@Agyeere9), Gideon Danso(@Gideon_0703),
> Akeemat Ayinla(@That_Hakeemah) , Frederick Allou(@Freddie), Duong Gia
> Khanh(@Khanh)**
>
> This presentation focuses on the WHO AMR pipeline analysis. It
> includes information on various non-traditional and antibiotic
> products targeting priority pathogens

+-----------------------------------------------------------------------+
| > ![](vertopal_2604346bc6                                             |
| fb4e008a0cbe2012e3cc69/media/image1.png){width="27.363888888888887in" |
| > height="13.6875in"}                                                 |
+=======================================================================+
+-----------------------------------------------------------------------+

> **F1: Distribution of Product Type**\
> This shows 383 traditional antibiotics and 283 non-traditional
> products in the pipeline. While many efforts focus on improving
> conventional antibiotics, nearly half explore innovative approaches
> like bacteriophages,

  -----------------------------------------------------------------------
  probiotics, and immune modulators, balancing existing treatments with
  new strategies.
  -----------------------------------------------------------------------

  -----------------------------------------------------------------------

+-----------------------------------------------------------------------+
| > ![](vertopal_2604346bc6                                             |
| fb4e008a0cbe2012e3cc69/media/image2.png){width="22.854166666666668in" |
| > height="17.031944444444445in"}                                      |
+=======================================================================+
+-----------------------------------------------------------------------+

> **Figure 2: Distribution of AMR Products by R&D Phase**\
> Considering the research and development phases, the majority of the
> tested products (46.3%) are in Phase 1 while Preregistration happened
> to be the phase with the least number of AMR Products (4.03%).

**Out of the 669 antibacterial entries tested, a total of 146 (54.6%)
showed activity against priority pathogens.**
![](vertopal_2604346bc6fb4e008a0cbe2012e3cc69/media/image5.png){width="26.990277777777777in"
height="13.495138888888889in"}

> Out of the 669 antibacterial entries tested, a total of 146 (54.6%)
> showed activity against priority pathogens.
>
> ![](vertopal_2604346bc6fb4e008a0cbe2012e3cc69/media/image4.png){width="26.988888888888887in"
> height="16.1875in"}
>
> **Figure 4: Pathogen Activity against Active Priority Pathogens**
>
> Critical priority pathogen have the highest number of active products,
> indicating strong research focus. Significant activity targets
> Gram-positive infections like Staphylococcus aureus. However, lowe r
> product counts for pathogens such a*s Helicobacter pylor*i an*d
> Campylobacter s*pp. highlight the ne ed for increased research and
> development efforts in these challenging areas.
>
> ![](vertopal_2604346bc6fb4e008a0cbe2012e3cc69/media/image6.png){width="26.988888888888887in"
> height="24.291666666666668in"}
>
> **Figure 5: Shows a graph of the "Number of Yes for Active pathogens
> by Product name"** The product named Rhu-pGSN showed greatest
> antibacterial activity
>
> ![](vertopal_2604346bc6fb4e008a0cbe2012e3cc69/media/image7.png){width="26.9374989063867in"
> height="28.16666557305337in"}
>
> **Figure 6: Product name against active priority pathogens**
>
> We evaluated how well the products work against critical pathogens.
> "Yes" means the pathogen is susceptible, "No" means it's resistant,
> and "Possibly" indicates the need for more research t o improve
> effectiveness.
>
> ![](vertopal_2604346bc6fb4e008a0cbe2012e3cc69/media/image8.png){width="26.988888888888887in"
> height="26.990277777777777in"}
>
> **Figure 7: Product distribution across R&D phases**
>
> This highlights 27 products that made it to the Preregistration phase.
> Indicating that "BB128",
>
> "AB103", and "Solithromycin" might be the only products to hit the
> market soon
>
> #Below are the codes we used in generating our graphs

> #Below is the R programming codes for analysing
>
> ---
title: "Untitled"
authors: "Claudia Agyeere", "Gideon Danso", "Frederick Allou", "Akeemat Ayinla", "Duong Gia Khanh"
date: "2024-09-14"
---


```{r setup, include=FALSE}
# Load necessary libraries
library(writexl)
library(ggplot2)
library(readxl)
library(DT)
library(plotly)
library(dplyr)
library(wordcloud)
library(RColorBrewer)
library(orca)
library(tidyr)
library(reshape2)
library(forcats)
library(htmlwidgets)
library(htmltools)
library(prettydoc)
library(ggridges)
library(hrbrthemes)
library(extrafont)
library(stringr)
library(lattice)
library(babynames)
library(reticulate)

# Set working directory
setwd("C:/Users/musah yussif/Downloads/HackBio/stage 2/AMR_APP")


# Load the data
amr_data <- read_excel("AMR_PRODUCTS_1.xlsx")
```

```{r setup-chunk, include=FALSE, echo=FALSE}
#knitr::opts_chunk$set(dev = "png",
                     # dpi = 300,
                     # echo = FALSE,
                     # cache = TRUE)
```




```{r 1, echo=FALSE}

# Loading the dataset
url <- "https://raw.githubusercontent.com/HackBio-Internship/public_datasets/main/R/WHO_AMR_PRODUCTS_DATA.tsv"
amr_data <- read.table(url, header = TRUE, sep = "\t")

# View the first few rows
head(amr_data)

#saving file

write_xlsx(amr_data, "AMR_PRODUCTS.xlsx")

#summary
summary(amr_data)

#checking for missing data
colSums(is.na(amr_data))

# Summary of the data
summary(amr_data)

# Checking for missing data
colSums(is.na(amr_data))


# Converting 'y' to 'Yes' in the 'Mycobacterium.tuberculosis' and 'Clostridioides.difficile' columns
#amr_data$Mycobacterium.tuberculosis <- ifelse(amr_data$Mycobacterium.tuberculosis == "y", "Yes", amr_data$Mycobacterium.tuberculosis)
#amr_data$Clostridioides.difficile <- ifelse(amr_data$Clostridioides.difficile == "y", "Yes", amr_data$Clostridioides.difficile)
```



```{r 2, echo=FALSE, fig.width=10, fig.cap="Figure 1. Showing the Distribution of Product Type"}

# Creating a bar graph for Product Type
ggplot(amr_data, aes(x = Product.type)) +
  geom_bar(fill = "steelblue", color = "black", width = 0.7) +
  theme_minimal(base_size = 15) +
  labs(
    title = "Distribution of Product Type",
    x = "Product Type",
    y = "Count"
  ) +
  coord_flip() +
  theme(
    plot.title = element_text(hjust = 0.3, face = "bold", color = "darkblue"),
    axis.title.x = element_text(face = "bold", color = "darkred"),
    axis.title.y = element_text(face = "bold", color = "darkred"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

```


```{r 1i, echo=FALSE, fig.width=10, fig.cap="Figure 2. Showing the Distribution of R & D Phase of Products"}

# assigning data to amr_data
data <- amr_data %>%
  count(R.D.phase) %>%   
  summarize(n = sum(n)) %>%                                 
  ungroup() %>%
  arrange(n) %>%                                            
  mutate(R.D.phase = factor(R.D.phase, levels = c("Phase I", "Phase II", "Phase III", "Preregistration"))) 

# Defining colors for each R.D.phase
colors <- c("Phase I" = "blue", 
            "Phase II" = "green", 
            "Phase III" = "red", 
            "Preregistration" = "purple")

# Creating the plot with annotations for each bar
ggplot(data, aes(x = R.D.phase, y = n, color = R.D.phase)) +
  geom_segment(aes(xend = R.D.phase, yend = 0)) +
  geom_point(size = 4) +
  geom_text(aes(label = n), vjust = -0.5, size = 5) +  # Add text annotations
  scale_color_manual(values = colors) + # Apply custom colors
  coord_flip() +
  theme_bw() +
  xlab("R.D. Phase") +  # Label x-axis
  ylab("Count") +# Label y-axis
  ggtitle("Distribution of R&D Phases")

 




```

```{r comparing the r.d phase to the product type, echo=FALSE, fig.height=12, fig.width=12, fig.cap="Figure 3. Showing the Distribution of Products and their R & D Phases"}

# Preparing data
filtered_data <- amr_data %>%
  count(Product.name, R.D.phase) %>%
  arrange(Product.name, R.D.phase)

# Plot
p <- ggplot(filtered_data, aes(x = reorder(Product.name, n), y = n)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(aes(label = n), vjust = -0.5, hjust = 1.2, size = 2, color = "black") +  
  facet_wrap(~ R.D.phase, scales = "free_y") +
  theme_minimal(base_family = "sans") +  
  coord_flip() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    axis.line = element_line(color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  xlab("Product Name") +
  ylab("Count") +
  ggtitle("Product Distribution Across R&D Phases")

print(p)


```


```{r Administration, echo=FALSE, fig.height=7, fig.width=10, fig.cap="Figure 6. Showing the Route of Administration"}

# Creating dataset
route_counts <- data.frame(
  Route.of.administration = factor(
    c("Colonoscopy", "Enema", "Inhalation", "IV", "IV & oral", "IV, Oral", "Oral", "Oral, Not absorbed"),
    levels = c("Colonoscopy", "Enema", "Inhalation", "IV", "IV & oral", "IV, Oral", "Oral", "Oral, Not absorbed")
  ),
  Count = c(1, 1, 65, 326, 13, 65, 195, 3)
)

# Ploting with counts displayed
ggplot(route_counts, aes(x = Route.of.administration, y = Count, color = Route.of.administration)) +
  geom_line(aes(group = 1), linewidth = 1) +  `
  geom_point(size = 3) +
  geom_text(aes(label = Count), vjust = -0.8, size = 4) +  
  theme_minimal() +
  theme(
    legend.position = "right", 
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10), 
    axis.title = element_text(size = 12),
    axis.line = element_line(color = "black"),
    panel.grid = element_blank()
  ) +
  xlab("Route of Administration") +
  ylab("Count") +
  ggtitle("Distribution of Routes of Administration")

```


```{r Administration, echo=FALSE, fig.height=7, fig.width=10, fig.cap="Figure 6. Showing the Route of Administration"}

# Creating dataset
route_counts <- data.frame(
  Route.of.administration = factor(
    c("Colonoscopy", "Enema", "Inhalation", "IV", "IV & oral", "IV, Oral", "Oral", "Oral, Not absorbed"),
    levels = c("Colonoscopy", "Enema", "Inhalation", "IV", "IV & oral", "IV, Oral", "Oral", "Oral, Not absorbed")
  ),
  Count = c(1, 1, 65, 326, 13, 65, 195, 3)
)

# Plotting with counts displayed
ggplot(route_counts, aes(x = Route.of.administration, y = Count, color = Route.of.administration)) +
  geom_line(aes(group = 1), linewidth = 1) +  `
  geom_point(size = 3) +
  geom_text(aes(label = Count), vjust = -0.8, size = 4) +  
  theme_minimal() +
  theme(
    legend.position = "right", 
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10), 
    axis.title = element_text(size = 12),
    axis.line = element_line(color = "black"),
    panel.grid = element_blank()
  ) +
  xlab("Route of Administration") +
  ylab("Count") +
  ggtitle("Distribution of Routes of Administration")

```



```{r 7, echo=FALSE, fig.height=5, fig.width=10, fig.cap="Figure 6. Showing the Distribution Pathogen Activity against Active against Priority pathogens" }

# Filtering data for only 'Yes' in Active.against.priority.pathogens.
filtered_data <- amr_data %>%
  filter(Active.against.priority.pathogens. == "Yes")  # Filter for 'Yes' only

# Creating the horizontal bar plot
ggplot(filtered_data, aes(x = Pathogen.activity, fill = Pathogen.activity)) +
  geom_bar() +  
  coord_flip() +  
  geom_text(stat = 'count', aes(label = after_stat(count)), hjust = -0.2, size = 4, fontface = "bold") +  
  theme_minimal() +  
  labs(
    x = "Pathogen Activity",  
    y = "Count",  
    title = "Pathogen Activity for 'Yes' Active Priority Pathogens"  
  ) +
  theme(
    axis.text.x = element_text(size = 12, face = "bold"),  # Bold x-axis labels
    axis.text.y = element_text(size = 12, face = "bold"),  # Bold y-axis labels
    axis.title.x = element_text(size = 14, face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(size = 14, face = "bold"),  # Bold y-axis title
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),  
    legend.position = "none",  
    axis.line = element_line(color = "black", linewidth = 0.8)  
  ) +
  scale_fill_manual(values = c("skyblue", "orange", "green", "red"))  

```


```{r 8, echo=FALSE, fig.height=6, fig.width=10, fig.cap="Figure 7. Showing the Distribution of Pathogen name against Active Priority pathogen" }

# Filtering data for only 'Yes' in Active.against.priority.pathogens.
filtered_data <- amr_data %>%
  filter(Active.against.priority.pathogens. == "Yes") %>%
  count(Pathogen.name)  # Count occurrences of each pathogen for 'Yes'

# Reordering the data based on count
filtered_data <- filtered_data %>%
  arrange(n) %>%
  mutate(Pathogen.name = factor(Pathogen.name, levels = Pathogen.name))

# Plotting with counts displayed
p <- ggplot(filtered_data, aes(x = Pathogen.name, y = n)) +
  geom_segment(
    aes(x = Pathogen.name, xend = Pathogen.name, y = 0, yend = n), 
    color = "black", 
    linewidth = 0.7
  ) +
  geom_point(
    color = "red", 
    size = 2
  ) +
  geom_text(aes(label = n), vjust = -0.5, size = 3.5) +  
  coord_flip() +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.5),
    text = element_text(family = "sans"),  
    panel.grid = element_blank(),  
    plot.title = element_text(size = 12, hjust = 0.5)  
  ) +
  xlab("") +
  ylab("Number of 'Yes' for Active Against Priority Pathogens") +
  ggtitle("Number of 'Yes' for Active Pathogens by Pathogen Name")

# Display the plot
p

```


```{r 9, echo=FALSE, fig.height=9, fig.width=10, fig.cap="Figure 8. Showing the Distribution of Product Name by Active Pathogens"}

# Filtering data for only 'Yes' in Active.against.priority.pathogens
filtered_data <- amr_data %>%
  filter(Active.against.priority.pathogens. == "Yes") %>%
  count(Product.name)  # Count occurrences of each product for 'Yes'

# Reordering the data based on count
filtered_data <- filtered_data %>%
  arrange(n) %>%
  mutate(Product.name = factor(Product.name, levels = Product.name))

# Plotting with counts displayed below the points
p <- ggplot(filtered_data, aes(x = Product.name, y = n)) +
  geom_segment(
    aes(x = Product.name, xend = Product.name, y = 0, yend = n), 
    color = "black", 
    linewidth = 0.7
  ) +
  geom_point(
    color = "purple", 
    size = 2
  ) +
  coord_flip() +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.5),
    text = element_text(family = "sans"),  
    panel.grid = element_blank(),  
    plot.title = element_text(size = 12, hjust = 0.5),
    plot.margin = margin(10, 10, 30, 10),  
    axis.text.y = element_text(size = 7, face = "bold", color = "black") 
  ) +
  xlab("") +
  ylab("Number of 'Yes' for Active Against Priority Pathogens") +
  ggtitle("Number of 'Yes' for Active Pathogens by Product Name")

# Display the plot
p

```


```{r 10, echo=FALSE, fig.height=6, fig.width=10, fig.cap="Figure 9. Showing the Distribution of Alternative name by Active priority pathogen"}

# Filtering data for only 'Yes' in Active.against.priority.pathogens
filtered_data <- amr_data %>%
  filter(Active.against.priority.pathogens. == "Yes") %>%
  count(Alternative.name)  # Count occurrences of each alternative for 'Yes'

# Reordering the data based on count
filtered_data <- filtered_data %>%
  arrange(n) %>%
  mutate(Alternative.name = factor(Alternative.name, levels = Alternative.name))

# Plot
p <- ggplot(filtered_data, aes(x = Alternative.name, y = n)) +
  geom_segment(
    aes(x = Alternative.name, xend = Alternative.name, y = 0, yend = n), 
    color = "black", 
    linewidth = 0.7
  ) +
  geom_point(
    color = "green", 
    size = 2
  ) +
  coord_flip() +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.5),
    text = element_text(family = "sans"),  
    panel.grid = element_blank(),  
    plot.title = element_text(size = 12, hjust = 0.5),
    plot.margin = margin(10, 10, 30, 10),  
    axis.text.y = element_text(size = 7, face = "bold", color = "black")  
  ) +
  xlab("") +
  ylab("Number of 'Yes' for Active Against Priority Pathogens") +
  ggtitle("Number of 'Yes' for Active Pathogens by Alternative Name")

# Display the plot
p


```


```{r grouped barplot, echo=FALSE, fig.height=11.5, fig.width=11, fig.cap="Figure 10. Showing the Distribution of Product Name by Active Pathogens"}


# Converting 'NA' values to 'Unknown' and filter out 'Unknown' and 'N/A'
amr_data$Active.against.priority.pathogens. <- ifelse(is.na(amr_data$Active.against.priority.pathogens.) | amr_data$Active.against.priority.pathogens. == "N/A", "Unknown", amr_data$Active.against.priority.pathogens.)

# Filtering out 'Unknown'
amr_data_filtered <- amr_data[amr_data$Active.against.priority.pathogens. != "Unknown", ]

# Creating a horizontal stacked bar plot 
ggplot(amr_data_filtered, aes(x = reorder(Product.name, Product.name, length), fill = Active.against.priority.pathogens.)) +
  geom_bar(position = "stack") +
  theme_minimal() +
  labs(title = "Stacked Bar Chart of Product Names Against Active Priority Pathogens",
       x = "Product Name",
       y = "Count",
       fill = "Active Against Priority Pathogens") +
  theme(axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 8, margin = margin(r = 10)), 
        axis.line = element_line(color = "black", linewidth = 0.5),
        panel.grid = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(10, 10, 10, 10),
        plot.title = element_text(size = 15, hjust = 0.5)
        ) + 
  coord_flip() 

```
