# working color palettes for isotope modeling plots
family_colors <- function(){
  cmap <- scale_color_manual(
    name = "Family",
    values = c("Bovidae" = "#FF0066FF", 
               "Canidae" = "#328C97FF", 
               "Cercopithecidae" = "#D1AAC2FF", 
               "Elephantidae" = "#A5506DFF",
               "Equidae" = "gray40",
               "Felidae" = "#7ac98f", # changed from '#B3E0BFFF'
               "Giraffidae" = "#2A9D3DFF", 
               "Hippopotamidae" = "#EDE15A", 
               "Hominidae" = "#DB7003FF", 
               "Hyaenidae" = "#F8C1A6FF", 
               "Hystricidae" = "#FBA600FF", 
               "Rhinocerotidae" = "#A30000FF", 
               "Suidae" = "#011A51FF", 
               "Tragulidae" = "#97D1D9FF",
               "Viverridae" = "#916C37FF"),
    breaks = c("Bovidae",
               "Canidae",
               "Cercopithecidae",
               "Elephantidae",
               "Equidae",
               "Felidae",
               "Giraffidae",
               "Hippopotamidae",
               "Hominidae",
               "Hyaenidae",
               "Hystricidae",
               "Rhinocerotidae",
               "Suidae",
               "Tragulidae",
               "Viverridae")
  )
  return(cmap)
}
family_fills <- function(){
  cmap <- scale_fill_manual(
    name = "Family",
    values = c("Bovidae" = "#FF0066FF", 
               "Canidae" = "#328C97FF", 
               "Cercopithecidae" = "#D1AAC2FF", 
               "Elephantidae" = "#A5506DFF",
               "Equidae" = "gray40",
               "Felidae" = "#7ac98f", # changed from '#B3E0BFFF'
               "Giraffidae" = "#2A9D3DFF", 
               "Hippopotamidae" = "#EDE15A", 
               "Hominidae" = "#DB7003FF", 
               "Hyaenidae" = "#F8C1A6FF", 
               "Hystricidae" = "#FBA600FF", 
               "Rhinocerotidae" = "#A30000FF", 
               "Suidae" = "#011A51FF", 
               "Tragulidae" = "#97D1D9FF",
               "Viverridae" = "#916C37FF"),
    breaks = c("Bovidae",
               "Canidae",
               "Cercopithecidae",
               "Elephantidae",
               "Equidae",
               "Felidae",
               "Giraffidae",
               "Hippopotamidae",
               "Hominidae",
               "Hyaenidae",
               "Hystricidae",
               "Rhinocerotidae",
               "Suidae",
               "Tragulidae",
               "Viverridae")
  )
  return(cmap)
}

bovid_colors <- function(){
  cmap <- scale_color_manual(
    name = "Tribe",
    values = c("Aepycerotini" = "#1F77B4FF",
               "Alcelaphini" = "#FF7F0EFF",
               "Antilopini" = "#2CA02CFF",
               "Bovini" = "#D62728FF",
               "Caprini" =  "#8C564BFF",
               "Cephalophini" = "#9467BDFF",
               "Hippotragini" = "#E377C2FF",
               "Neotragini" = "#7F7F7FFF",
               "Reduncini" = "#BCBD22FF",
               "Tragelaphini" =  "#17BECFFF"),
    breaks = c("Aepycerotini",
               "Alcelaphini",
               "Antilopini",
               "Bovini",
               "Caprini",
               "Cephalophini",
               "Hippotragini",
               "Neotragini",
               "Reduncini",
               "Tragelaphini")
  )
  return(cmap)
}

bovid_fills <- function(){
  cmap <- scale_fill_manual(
    name = "Tribe",
    values = c("Aepycerotini" = "#1F77B4FF",
               "Alcelaphini" = "#FF7F0EFF",
               "Antilopini" = "#2CA02CFF",
               "Bovini" = "#D62728FF",
               "Caprini" =  "#8C564BFF",
               "Cephalophini" = "#9467BDFF",
               "Hippotragini" = "#E377C2FF",
               "Neotragini" = "#7F7F7FFF",
               "Reduncini" = "#BCBD22FF",
               "Tragelaphini" =  "#17BECFFF"),
    breaks = c("Aepycerotini",
               "Alcelaphini",
               "Antilopini",
               "Bovini",
               "Caprini",
               "Cephalophini",
               "Hippotragini",
               "Neotragini",
               "Reduncini",
               "Tragelaphini")
  )
  return(cmap)
}

# plot posterior predictions, divided by country, and compare to the woody cover value the model is trying to estimate
country_plot <- function(posterior_preds, country, real_data){
  fig <- posterior_preds %>% filter(COUNTRY == country) %>%
    ggplot()+
    stat_halfeye(aes(x = value, y = locality, shape = "predicted value"), color = "lightsalmon4", fill = alpha("lightsalmon3", alpha = 0.7), point_size = 3, point_interval = "median_qi", .width = 0.89)+
    new_scale(new_aes = "shape")+ 
    geom_star(data = real_data[real_data$COUNTRY == country,], mapping = aes(x = pct_wc_median, y = locality, starshape = "actual value"), size = 4, fill = "white", starstroke = 1.5)+
    scale_starshape_manual(values = c("actual value" = 1), name = element_blank())+
    xlab("proportion woody cover") +
    xlim(0, 1)+
    ylab(NULL)+
    ggtitle(country)+
    theme_classic(base_size = 16)+
    theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5), legend.title = element_blank())
  return(fig)
}

# plot posterior predictions for each country, but with distributions divided by taxon
country_taxon_plot <- function(posterior_preds, country, real_data){
  fig <- posterior_preds %>% filter(COUNTRY == country) %>%
    ggplot()+
    stat_halfeye(aes(x = value, y = locality, color = Family, fill = Family), shape = 16, point_interval = "median_qi", .width = 0.89, slab_alpha = 0.7, point_size = 3)+
    family_colors() +
    family_fills() +
    new_scale(new_aes = "shape")+
    geom_star(data = real_data[real_data$COUNTRY == country,], aes(x = pct_wc_median, y = locality, starshape = "actual value"), size = 4, fill = "white", starstroke = 1.5) +
    scale_starshape_manual(values = c("actual value" = 1), name = "") +
    xlim(0,1)+
    xlab("proportion woody cover") + 
    ylab(NULL)+
    ggtitle(country)+
    theme_classic(base_size = 16) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
  return(fig)
}

# plot bovid tribes only, by country
country_bovid_plot <- function(posterior_preds, country, real_data){
  fig <- posterior_preds %>% filter(COUNTRY == country & Family == "Bovidae") %>%
    ggplot()+
    stat_halfeye(aes(x = value, y = locality, color = taxon_model, fill = taxon_model), shape = 16, point_size = 3, point_interval = "median_qi", .width = 0.89, slab_alpha = 0.6)+
    bovid_colors() +
    bovid_fills()+
    new_scale(new_aes = "shape")+
    geom_star(data = real_data[real_data$COUNTRY == country & real_data$Family == "Bovidae",], aes(x = pct_wc_median, y = locality, starshape = "actual value"), size = 4, fill = "white", starstroke = 1.5) +
    scale_starshape_manual(values = c("actual value" = 1), name = "") +
    xlim(0,1)+
    xlab("proportion woody cover") + 
    ylab(NULL)+
    ggtitle(country)+
    theme_classic(base_size = 16) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
  return(fig)
}
