library(ggplot2)
library(dplyr)
library(readxl)
library(countrycode)
library(RColorBrewer)
library(forcats)
library(stringr)
library(scales)



data <- read_excel("C:/Users/ttruo/OneDrive - The University of Melbourne/Epidemiology 2025/Major research project/Scoping review/data_extraction_final.xlsx", sheet = "otherstudyvariables")

# bar graph of methods documentation
detailc <- data %>%
  count(detail, name = "count") %>%
  mutate(percentage = (count/154) * 100) %>%
  arrange(desc(count))

detail_total = sum(detailc$count)

var1 <- data.frame(
  Category = c("Detailed in paper", "Detailed in paper \nand code repository", "Insufficient information"),
  Count = c(116, 36, 2) # EDIT actual counts lol
)

ggplot(data = var1, aes(x = Category, y = Count, fill = Category)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(y = "Number of studies") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 140, by = 20)) +
  scale_fill_manual(values = c("Detailed in paper" = "#619CFF", "Detailed in paper \nand code repository" = "#785EF0",  "Insufficient information" = "gray")) +
  theme(
    axis.text.x = element_text(size = 14),
    title = element_text(size = 18),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 14),
    legend.position = "None",
  )

# bar graph of estimation method

estc <- data %>% 
  count(est, name = "count") %>%
  mutate(percentage = (count/154) * 100) %>%
  arrange(desc(count))

est_total = sum(estc$count)

var2 <- data.frame(
  Category = c("Descriptive statistics", "Model-based methods", "Not specified"),
  Count = c(24, 128, 2) 
)


ggplot(data = var2, aes(x = Category, y = Count, fill = Category)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(y = "Number of studies") +
  theme_minimal() +
  scale_fill_manual(values = c("Descriptive statistics" = "#619CFF", "Model-based methods" = "#785EF0",  "Not specified" = "gray")) +
  theme(
    axis.text.x = element_text(size = 14),
    title = element_text(size = 18),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 14),
    legend.position = "None"
  )

# bar graph of sample size type
# serial interval
data_sp <- read_excel("C:/Users/ttruo/OneDrive - The University of Melbourne/Epidemiology 2025/Major research project/Scoping review/data_extraction_final.xlsx", sheet = "data_extraction_main")

datatype <- data_sp %>%
  filter(delay_estimated == "Serial interval") %>%
  count(`Sample size type`, name = "count")

var3_SI <- data.frame(
  category = c("Transmission pairs", "Daily case incidence", "Genomic data (virus sequences)", "Hospital-associated cluster", "Household cluster", "Social media (Twitter tweets)"),
  count = c(127, 2, 1, 1, 2, 1) 
)

pastels <- c(
  "#AEC6CF", # pastel blue
  "#FFB7C5", # pastel pink
  "#C1E1C1", # pastel green
  "#FAC898", # pastel yellow
  "#FFD1DC", # pastel rose
  "#E3E4FA"  # lavender
)

var3_SI$fraction <- var3_SI$count / sum(var3_SI$count)
var3_SI$ymax <- cumsum(var3_SI$fraction)
var3_SI$ymin <- c(0, head(var3_SI$ymax, n=-1))
var3_SI$labelPosition <- (var3_SI$ymax + var3_SI$ymin) / 2
var3_SI$label <- paste0(var3_SI$category, ": ", var3_SI$count)

dSI <- ggplot(var3_SI,
       aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
  geom_rect() +
  geom_text(
    aes(
      x = 3.5,
      y = labelPosition,
      label = count
    ),
    colour = "black",
    size = 4
  ) +
  coord_polar(theta = "y") +
  xlim(c(0, 4)) +
  scale_fill_manual(values = pastels, name = "Data source") +
  theme_void() +
  labs(title = "Serial interval") + 
  theme(
    legend.position = "right",
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    title = element_text(size = 16)
  )

# generation interval
datatype <- data_sp %>%
  filter(delay_estimated == "Generation interval") %>%
  count(`Sample size type`, name = "count")


var3_GI <- data.frame(
  category = c("Transmission pairs", "Daily case incidence", "Household cluster", "Local transmission clusters"),
  count = c(35, 6, 3, 2)
)

var3_GI$fraction <- var3_GI$count / sum(var3_GI$count)
var3_GI$ymax <- cumsum(var3_GI$fraction)
var3_GI$ymin <- c(0, head(var3_GI$ymax, n=-1))
var3_GI$labelPosition <- (var3_GI$ymax + var3_GI$ymin) / 2
var3_GI$label <- paste0(var3_GI$category, ": ", var3_GI$count)

dGI <- ggplot(var3_GI,
       aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
  geom_rect() +
  geom_text(
    aes(
      x = 3.5,
      y = labelPosition,
      label = count
    ),
    colour = "black",
    size = 6
  ) +
  coord_polar(theta = "y") +
  xlim(c(0, 4)) +
  scale_fill_manual(values = pastels, name = "Data source") +
  theme_void() +
  labs(title = "Generation interval") + 
  theme(
    legend.position = "right",
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    title = element_text(size = 16)
  )


splot <- (dGI / dSI) + plot_annotation(tag_levels = 'A')
ggsave("data_sourcev3.png", splot, width = 8, height = 10, units = "in")


# sample size: transmission pairs used in GI vs SI

GI_sample <- data_sp %>%
  filter(delay_estimated == "Generation interval",
         !is.na(as.numeric(sample_size))) %>%
  select(`Study ID`, delay_estimated, sample_size) 

SI_sample <- data_sp %>%
  filter(delay_estimated == "Serial interval",
         !is.na(as.numeric(sample_size))) %>%
  select(`Study ID`, delay_estimated, sample_size) 

samples <- rbind(GI_sample, SI_sample)
samples <- samples %>%
  mutate(sample_size = as.numeric(sample_size),
         delay_estimated = factor(delay_estimated, levels = c("Generation interval", "Serial interval")))
     

log_scale <- ggplot(samples, aes(x = delay_estimated, y = sample_size)) +
  geom_jitter(width = 0.1, height = 0.5, aes(colour = delay_estimated)) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  labs(y = "Transmission pairs (log10 scale)", x = NULL) +
  scale_colour_manual(
    values = c("Generation interval" = "#F6C7B3",
               "Serial interval" = "#82B2C0")
  ) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        ) +
  theme_minimal(base_size = 14) 





# country
countrydata <- read_excel("C:/Users/ttruo/OneDrive - The University of Melbourne/Epidemiology 2025/Major research project/Scoping review/data_extraction_final.xlsx", sheet = "country")
countrydata$subregion <- countrycode(sourcevar = countrydata$country,
                                     origin = "country.name",
                                     destination = "un.regionsub.name")
countrydata$subregion[countrydata$country == "Taiwan"] <- "Eastern Asia"

countrydf <- countrydata %>% 
  select(country, subregion) %>%
  count(subregion, country, name = "count") %>%
  arrange(desc(count))

subregiondf <- countrydf %>%
  select(subregion, count) %>%
  group_by(subregion) %>%
  summarise(sum(count)) %>%
  mutate(percentage = (`sum(count)`/145) * 100) %>%
  arrange(desc(`sum(count)`))

total_count = sum(subregiondf$count)

countrydf$subregion <- countrycode(sourcevar = countrydf$country,
                                     origin = "country.name",
                                     destination = "un.regionsub.name")
countrydf$subregion[countrydf$country == "Taiwan"] <- "Eastern Asia"
countrydf <- countrydf %>% 
  country = factor(country, levels = country)
countrydf$country <- factor(countrydf$country, 
                            levels = countrydf$country[order(countrydf$count)])


plt <- ggplot(countrydf) +
  geom_col(aes(x = count, y = country, fill = subregion), width = 0.6) +
  labs(x = "Country", y = "Number", fill = "Subregion") +
  theme(
    legend.position = "right",
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
  ) + 
  theme_minimal()

countrydf <- countrydf %>%
  arrange(subregion, desc(count)) %>%
  mutate(country = factor(country, levels = rev(unique(country))))


# parametric form
data_GI <- read_excel("C:/Users/ttruo/OneDrive - The University of Melbourne/Epidemiology 2025/Major research project/Scoping review/data_extraction_final.xlsx", sheet = "data_extraction_main")

pform_GI <- data_GI %>%
  filter(delay_estimated == "Generation interval") %>%
  count(parametric_form_edit, name = "count")

pform_GI$parametric_form_edit <- str_wrap(pform_GI$parametric_form_edit, width = 50)

pform_GI$parametric_form_edit <- factor(pform_GI$parametric_form_edit, 
                            levels = pform_GI$parametric_form_edit[order(pform_GI$count)])


pltp <- ggplot(pform_GI) +
  geom_col(aes(x = count, y = parametric_form_edit), width = 0.6, fill = "#F6C7B3") +
  labs(title = "Generation interval", x = "Number of estimates", y = "Assumed parametric form") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 16))


pform_SI <- data_GI %>%
  filter(delay_estimated == "Serial interval") %>%
  count(parametric_form_edit, name = "count")

pform_SI$parametric_form_edit <- str_wrap(pform_SI$parametric_form_edit, width = 50)

pform_SI$parametric_form_edit <- factor(pform_SI$parametric_form_edit, 
                                        levels = pform_SI$parametric_form_edit[order(pform_SI$count)])


pltp <- ggplot(pform_SI) +
  geom_col(aes(x = count, y = parametric_form_edit), width = 0.6, fill = "#82B2C0") +
  labs(title = "Serial interval", x = "Number of estimates", y = "Assumed parametric form") +
  scale_x_continuous(
    breaks = seq(0, 50, by = 5)
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 16))
  

# downstream estimates
downest_SI <- data_sp %>%
  filter(delay_estimated == "Serial interval") %>%
  count(downstream_estimates, name = "count")


downest_SI$downstream_estimates <- str_wrap(downest_SI$downstream_estimates, width = 50)

downest_SI$downstream_estimates <- factor(downest_SI$downstream_estimates, 
                                          levels = downest_SI$downstream_estimates[order(downest_SI$count)])


ggplot(downest_SI) +
  geom_col(aes(x = count, y = downstream_estimates), width = 0.6, fill = "#82B2C0") +
  labs(title = "Serial interval", x = "Number of estimates", y = "Downstream epidemiological estimate that used serial intervals") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 16))


downest_GI <- data_sp %>%
  filter(delay_estimated == "Generation interval") %>%
  count(downstream_estimates, name = "count")

downest_GI$downstream_estimates <- str_wrap(downest_GI$downstream_estimates, width = 50)

downest_GI$downstream_estimates <- factor(downest_GI$downstream_estimates, 
                                          levels = downest_GI$downstream_estimates[order(downest_GI$count)])
ggplot(downest_GI) +
  geom_col(aes(x = count, y = downstream_estimates), width = 0.6, fill = "#F6C7B3") +
  labs(title = "Generation interval", x = "Number of estimates", y = "Downstream epidemiological estimate that used generation intervals") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 16))

# bias
# right truncation
rtrunc <- data.frame(
  correction = c("No", "Aware", "Survival analysis with right-truncated data", "Right-truncated likelihood", "Study design"),
  count  = c(NA, 5, 5, 1, 1)
)
# dynamical bias 



# interval censoring 
cens <- data.frame(
  correction = c("No", "Aware", "Interval-censored likelihood", "Indirectly accounted for", "Midpoint imputation", "Minimal detail"),
  count = c(NA, 5, 19, 4, 1, 1)
)

dynamical <- data.frame(
  correction = c("No", "Aware", "Aware and visualised", "Weigh likelihood by growth rate", "Forward/backward sampling correction", "Susceptible depletion modelling"),
  count = c(NA, 10, 5)
)



