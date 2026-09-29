library(tidyverse)
library(httr)
library(jsonlite)
library(data.table)
library(hrbrthemes)
library(scales)
library(cowplot)

data <- data.frame()

while(T){
  
br = "https://resultados-sim.tse.jus.br/teste/ele2022/9240/dados-simplificados/br/br-c0001-e009240-r.json"
urnas = "https://resultados-sim.tse.jus.br/teste/ele2022/9240/dados/br/br-e009240-ab.json"
  
  ab <- fromJSON(urnas, simplifyDataFrame = TRUE) %>% .[["abr"]] %>% 
    filter(cdabr=="BR") %>% pull(pst)
  
  add <- fromJSON(br, simplifyDataFrame = TRUE) %>% .[["cand"]] %>% 
    mutate(voto=as.numeric(vap),
           pct=voto/sum(voto)) %>% 
    arrange(desc(pct)) %>% slice(1) %>% 
    mutate(time=Sys.time(),
           group=1) %>% select(time, pct, group)
  data <- rbind(data,add)
  
  line <- data %>% ggplot(aes(x=time,y=pct)) +
    geom_line(aes(group=group), size=1) +
    scale_y_continuous(limits = c(.45,.55),
                       breaks = seq(.46,.54, by = .02),
                       labels = percent_format(accuracy = 1)) +
    theme_ipsum() +
    theme(plot.title = element_text(size=10,hjust=.5)) +
    labs(x="",y="", title="evolução % Lula")
  
  plot <- fromJSON(br, simplifyDataFrame = TRUE) %>% .[["cand"]] %>% 
          mutate(voto=as.numeric(vap),
                 pct=voto/sum(voto)) %>% 
          arrange(desc(pct)) %>% slice(1:4) %>% 
          mutate(ordem=1:4,
                 nm=fct_reorder(nm,-ordem),
                 cor=case_when(n==13 ~ "#FF0000",
                               n==22 ~ "#000000",
                               n==12 ~ "#F7970A",
                               n==15 ~ "#416529")) %>% 
          ggplot(aes(x=nm, y=pct)) +
          geom_col(aes(fill=I(cor)), width = 1, alpha=.9) +
          coord_flip() +
          geom_hline(yintercept=.5, linetype=2) +
          geom_text(aes(y=pct,label=paste0(round(pct*100, digits=2))), hjust=1.1, size=4, color="white") +
          theme_ipsum() +
          theme(plot.title = element_text(size=10,hjust=.5)) +
          scale_y_continuous(limits = c(0,.55),
                             breaks = seq(0,.5, by = .1),
                             labels = percent_format(accuracy = 1)) +
          labs(x="",y="", title=paste0(ab,"% das seções totalizadas (",Sys.time(),")"))

  print(plot_grid(plot,line, rel_widths = c(1,.7)))
  
  Sys.sleep(3)
}

