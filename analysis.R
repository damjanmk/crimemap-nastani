#####
# Visualise data stored in crimemap.finki.ukim.mk
# Author: Damjan Temelkovski
# 27 Dec 2018
#####
# load libraries
library(XML)
library(ggplot2)
library("RColorBrewer")
library(Cairo)
library(dplyr)
# general options
options(stringsAsFactors = F)
theme_clear <- function(base_size = 12, base_family = "sans"){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border=element_blank(),
      axis.text=element_text(size=15),
      axis.title=element_text(size=13)
    )
}
#####
# load xml, firstly download from http://crimemap.finki.ukim.mk/model/xmldump.php
result_dir = "~/Documents/crimemap-nastani/" # CHANGE accordingly
path_to_xml = paste0(result_dir, "crimemap_nastani.xml")
nastani = xmlToDataFrame(path_to_xml)
# some events have "" as city - replace with NA
nastani = nastani %>% mutate(grad = ifelse(grad == "", NA, grad))
# correct format of some columns
nastani$lat = as.numeric(nastani$lat)
nastani$lng = as.numeric(nastani$lng)
nastani$datum_format = as.Date(nastani$datum, "%d.%m.%Y")
nastani$datum_d = as.numeric(substr(nastani$datum, start = 1, stop = 2))
nastani$datum_m = as.numeric(substr(nastani$datum, start = 4, stop = 5))
nastani$datum_y = as.numeric(substr(nastani$datum, start = 7, stop = 10))

#####
# group events by city and by city + by type
n_crimes_by_city = nastani %>% group_by(grad) %>% summarise(n_crimes_total = n()) %>% arrange(desc(n_crimes_total))
n_crimes_by_type_city = nastani %>% group_by(grad, shto) %>% summarise(n_crimes = n())
ordered_cities = n_crimes_by_city$grad # store order of cities by number of total crimes (desc)
# re-order the cities by number of total crimes - to be used in the x-axis of the barplot
n_crimes_by_type_city$grad = factor(n_crimes_by_type_city$grad, levels = ordered_cities, ordered = T)

#####
# plot a stacked barplot of crimes by city and by type
by_type_city_stacked_barplot = 
  ggplot(data = n_crimes_by_type_city, aes(x = grad, y = n_crimes, fill = shto)) +
  geom_bar(stat = "identity") + theme_clear() +
  theme(axis.text.x = element_text(angle = 66, hjust = 1),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position = c(0.95, 0.6)) + 
  scale_fill_manual(values = brewer.pal(n = 7, name = "Set1")) + 
  xlab("Град") + ylab(paste0("Број на настани групирани по вид\nвкупно ", sum(n_crimes_by_type_city$n_crimes), " од 06.04.11 до 24.12.18 (извор: crimemap.finki.ukim.mk)")) + 
  labs(fill = "Вид на настан")

ggsave(by_type_city_stacked_barplot, filename = paste0(result_dir, "by_type_city_stacked_barplot.pdf"), device=cairo_pdf, width = 15, height = 9, units = "in", dpi = 300)
ggsave(by_type_city_stacked_barplot, filename = paste0(result_dir, "by_type_city_stacked_barplot.png"), device="png", width = 15, height = 9, units = "in", dpi = 300)

# plot a barplot of crimes by city and by type using position_dodge
by_type_city_barplot = 
  ggplot(data = n_crimes_by_type_city, aes(x = grad, y = n_crimes, fill = shto)) +
  geom_bar(stat = "identity", position = position_dodge()) + theme_clear() +
  theme(axis.text.x = element_text(angle = 66, hjust = 1),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position = c(0.95, 0.6)) + 
  scale_fill_manual(values = brewer.pal(n = 7, name = "Set1")) + 
  xlab("Град") + ylab(paste0("Број на настани групирани по вид\nвкупно ", sum(n_crimes_by_type_city$n_crimes), " од 06.04.11 до 24.12.18 (извор: crimemap.finki.ukim.mk)")) + 
  labs(fill = "Вид на настан")

ggsave(by_type_city_barplot, filename = paste0(result_dir, "by_type_city_barplot.pdf"), device=cairo_pdf, width = 15, height = 9, units = "in", dpi = 300)
ggsave(by_type_city_barplot, filename = paste0(result_dir, "by_type_city_barplot.png"), device="png", width = 15, height = 9, units = "in", dpi = 300)



# group events by date and by city + by date
n_crimes_by_date = nastani %>% group_by(datum_format) %>% summarise(n_crimes_total = n()) %>% arrange(desc(n_crimes_total))
n_crimes_by_date_city = nastani %>% group_by(grad, datum_format) %>% summarise(n_crimes = n())
n_crimes_by_date = n_crimes_by_date %>% subset(datum_format > as.Date("2011-04-06"))
n_crimes_by_date_city = n_crimes_by_date_city %>% subset(datum_format > as.Date("2011-04-06"))
#####
# plot a stacked barplot of crimes by city and by type
by_date_barplot = 
  ggplot(data = n_crimes_by_date, aes(x = datum_format, y = n_crimes_total)) +
  geom_bar(stat = "identity") + theme_clear() +
  theme(axis.text.x = element_text(angle = 66, hjust = 1),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position = "top") + 
  xlab("Датум") + ylab(paste0("Број на настани\nвкупно ", sum(n_crimes_by_type_city$n_crimes), " од 06.04.11 до 24.12.18 (извор: crimemap.finki.ukim.mk)")) + 
  labs(fill = "Град") 

ggsave(by_date_barplot, filename = paste0(result_dir, "by_date_barplot.pdf"), device=cairo_pdf, width = 15, height = 9, units = "in", dpi = 300)
ggsave(by_date_barplot, filename = paste0(result_dir, "by_date_barplot.png"), device="png", width = 15, height = 9, units = "in", dpi = 300)

by_date_city_stacked_barplot = 
  ggplot(data = n_crimes_by_date_city, aes(x = datum_format, y = n_crimes, fill = grad)) +
  geom_bar(stat = "identity") + theme_clear() +
  theme(axis.text.x = element_text(angle = 66, hjust = 1),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position = "top") + 
   xlab("Датум") + ylab(paste0("Број на настани групирани по град\nвкупно ", sum(n_crimes_by_type_city$n_crimes), " од 06.04.11 до 24.12.18 (извор: crimemap.finki.ukim.mk)")) + 
   labs(fill = "Град")

ggsave(by_date_city_stacked_barplot, filename = paste0(result_dir, "by_date_city_stacked_barplot.pdf"), device=cairo_pdf, width = 15, height = 9, units = "in", dpi = 300)
ggsave(by_date_city_stacked_barplot, filename = paste0(result_dir, "by_date_city_stacked_barplot.png"), device="png", width = 15, height = 9, units = "in", dpi = 300)






