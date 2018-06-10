# Exploratory analysis of COSMIC samples

library(dplyr)

samples <- read.csv('~/HackCancer/data/COSMIC/Cosmicsample.tsv',
              sep = "\t", na.strings = c("NS", ""))

samples_summary <- summary(samples)

# SITE VS HISTOLOGY ----------------------------------------------------------

samples <- samples %>%
  dplyr::mutate(Histology = ifelse(Histology.subtype.1 == "NS", 
                                   as.character(Primary.histology), 
                                   as.character(Histology.subtype.1)))
within(samples, 
       Primary.site <- factor(Primary.site, 
                          levels = names(sort(table(Primary.site), 
                                            decreasing = TRUE)))) %>% 
  dplyr::filter(!is.na(Primary.site)) %>% 
  ggplot2::ggplot(ggplot2::aes(x = Primary.site, fill = Site.subtype.1)) +
  ggplot2::geom_bar() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                 axis.title.x = ggplot2::element_blank(),
                 legend.position = "none") +
  ggplot2::ggsave(file = "SiteVSSiteSuv1.png", width = 8, heigh = 5)

within(samples, 
       Primary.site <- factor(Primary.site, 
                              levels = names(sort(table(Primary.site), 
                                                  decreasing = TRUE)))) %>% 
  dplyr::filter(!is.na(Primary.site)) %>% 
  ggplot2::ggplot(ggplot2::aes(x = Primary.site, fill = Histology)) +
  ggplot2::geom_bar() +
  ggplot2::ggtitle("Histologies in sites") +
  # ggplot2::theme_minimal() +
  ggplot2::theme(plot.margin = ggplot2::unit(c(0.2, 0.2, 0.2, 2.2), "cm"),
                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                 axis.title.x = ggplot2::element_blank(),
                 legend.position = "none") +
  ggplot2::ggsave(file = "SiteVSHistology.png", width = 8, heigh = 5)



# summary(samples$Histology)
# 
# s <- samples %>%
#   dplyr::group_by(Histology) %>% 
#   dplyr::summarise(count = n()) %>% 
#   dplyr::arrange(desc(count))
# 
# s <- s %>% 
#   dplyr::mutate(perc = count / sum(s$count))
# 
# s %>% 
#   dplyr::filter(perc < 0.01) %>% 
#   dplyr::mutate(index = as.integer(row.names(.))) %>%
#   ggplot2::ggplot(ggplot2::aes(x = index, y = perc)) +
#   ggplot2::geom_line()
# 
# GENDER ---------------------------------------------------------------------

samples %>%
  dplyr::group_by(gender) %>% 
  dplyr::summarise(count = n()) %>% 
  dplyr::mutate(perc = count / sum(count) * 100)

samples %>% 
  dplyr::select(gender, age, Primary.site) %>%
  dplyr::group_by(gender, Primary.site) %>% 
  dplyr::summarise(count = n()) %>% 
  dplyr::group_by(Primary.site) %>% 
  dplyr::mutate(freq = count/sum(count)) %>% 
  dplyr::select(-count) %>% 
  dplyr::ungroup() %>% 
  tidyr::spread(gender, freq) %>%
  dplyr::mutate(f = ifelse(is.na(f), 0, f),
                m = ifelse(is.na(m), 0, m),
                u = ifelse(is.na(u), 0, u)) %>% 
  ggtern::ggtern(ggtern::aes(x = f, y = m, z = u)) +
  ggplot2::geom_point() +
  ggplot2::ggtitle("Gender in sites") +
  ggplot2::ggsave(file = "GenderBySite.png", width = 5, heigh = 5)

samples %>% 
  dplyr::select(gender, Histology) %>%
  dplyr::filter(!is.na(Histology)) %>% 
  dplyr::group_by(gender, Histology) %>% 
  dplyr::summarise(count = n()) %>% 
  dplyr::group_by(Histology) %>% 
  dplyr::mutate(freq = count/sum(count)) %>%
  dplyr::select(-count) %>%
  dplyr::ungroup() %>% 
  tidyr::spread(gender, freq) %>%
  dplyr::mutate(f = ifelse(is.na(f), 0, f),
                m = ifelse(is.na(m), 0, m),
                u = ifelse(is.na(u), 0, u)) %>% 
  ggtern::ggtern(ggtern::aes(x = f, y = m, z = u)) +
  ggplot2::geom_point() +
  ggplot2::ggtitle("Gender in sites") +
  ggplot2::ggsave(file = "GenderByHistology.png", width = 5, heigh = 5)

# AGE -------------------------------------------------------------------------

nrow(samples)
sum(!is.na(samples$age))
sum(samples$age != 136, na.rm = TRUE)

samples <- samples %>%
  dplyr::filter(age != 136)

s <- samples %>% 
  dplyr::filter(!is.na(age)) %>%
  dplyr::group_by(Primary.site) %>% 
  dplyr::summarise(sum_count = n())

pat_samples <- samples %>%
  dplyr::select(Primary.site, age) %>% 
  dplyr::left_join(s, by = "Primary.site") %>% 
  dplyr::filter(sum_count > 10)

pat_samples %>%
  ggplot2::ggplot(ggplot2::aes(x = age, color = Primary.site)) +
  ggplot2::geom_density() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::coord_cartesian(xlim = c(0, 100)) +
  ggplot2::ggtitle("Age by site") +
  ggplot2::ggsave(file = "AgeBySite.png", width = 5, heigh = 5)


get.density.df <- function(pat_samples){
  d <- density(pat_samples$age)
  data.frame(age = d$x, density = d$y)
}

pathologies <- pat_samples %>%
  dplyr::select(Primary.site) %>% 
  na.omit() %>% 
  unique() 


densities <- lapply(pathologies$Primary.site,
                    function(p, x){dplyr::filter(x, Primary.site == p) %>% 
                        get.density.df() %>% 
                        dplyr::mutate(Primary.site = p)},
                    dplyr::select(samples, Primary.site, age)) %>% 
  dplyr::bind_rows()

densities %>%
  ggplot2::ggplot(ggplot2::aes(x = age, y = density, color = Primary.site)) +
  ggplot2::geom_line() +
  ggplot2::coord_cartesian(xlim = c(0, 100)) +
  ggplot2::facet_grid(Primary.site ~ .) +
  ggplot2::ggtitle("Age by site") +
  ggplot2::theme(legend.position = "none",
                 strip.text.y = ggplot2::element_text(angle = 0),
                 axis.text.y = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank()) +
  ggplot2::ggsave(file = "age_in_facets.png", height = 20, width = 5)

d_order <- densities %>%
  dplyr::group_by(Primary.site) %>%
  dplyr::summarise(q2_age = median(age))

densities <- densities %>%
  dplyr::left_join(d_order, by = "Primary.site") %>% 
  dplyr::arrange(q2_age)

k <- 0.01

joy.division.plot <- function(k, densities){
  p <- ggplot2::ggplot()
  pathology_loop <- unique(densities$Primary.site)
  i <- length(pathology_loop)*k + k
  for (site in pathology_loop) {
    site_density <- densities %>%
      dplyr::filter(Primary.site == site) %>% 
      dplyr::mutate(Primary.site = as.character(Primary.site),
                    density = i + density)
    p <- p + 
      ggplot2::geom_ribbon(data = site_density, alpha = 0.2,
                           ggplot2::aes(x = age, ymax = density), ymin = i,
                           colour = "white", fill = "black", size = 0.5)
    i <- i - k
  }
  
  p + 
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "#000000"),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank()) +
    ggplot2::geom_rect(ggplot2::aes(ymin = 0, ymax = k, xmin = -Inf, xmax = Inf),
                       color = "black", fill = "black") 
  
}

joy.division.plot(0.01, densities) +
  ggplot2::ggtitle("Age by site") +
  ggplot2::ggsave(file = "AgeBySiteJoyDivision.png", width = 5, heigh = 8)

