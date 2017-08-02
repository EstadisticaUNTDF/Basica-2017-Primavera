library(tabulizer)
library(pdftools)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)

files <- list.files(path = "Labs/lab-1/data/", pattern = "pdf", 
                    full.names = TRUE)
pdfs <- tibble(path = files, 
       file = list.files(path = "Labs/lab-1/data/", pattern = "pdf"))
pdfs <- pdfs %>% 
  mutate(txt = list(pdf_text(path)))

pdfs <- pdfs %>% 
  mutate(table_position = grep("TABLA 13", txt) %>%  
           tail(1) %>% 
           ifelse(is.null(.), NA, .),
         table = ifelse(is.na(table_position), NA, 
                              extract_tables(path, table_position, 
                                             encoding = "UTF-8" )))
pdfs_11_14 <- pdfs %>% 
  slice(11:14) %>% 
  rowwise() %>% 
  mutate(nacidos = table[4,3], "\\.", "") %>% str_split(" ")) %>% 
  unnest()

fix_table <- function(out){
  out[2, 1] <- out[3, 1]
  out[2, 2] <- out[3, 2]
  out[1:2,3:11] <- out[2:3,3:11]
  out <- out[-3,]
  out <- as_tibble(out)
  nombres <- str_replace(out[1,], "\\r", " ")
  out <- out[2,]
  out<- str_replace_all(out, "([0-9]*)\\.([0-9]*)", "\\1\\2") %>% 
    matrix(ncol = length(out)) %>% 
    as_tibble()
  final <- separate_rows(out, -V1)
  final[[1]] <- str_split(out[[1]], "\\r")[[1]]
  names(final) <- nombres
  final <- final %>% 
    mutate_at(2:11, .funs = as.numeric)
  final
}


names(final)[1:2] <- c("Residencia", "Total")
write.csv(final, "Labs/lab-1/cache/nacidos_lugar_peso.csv", row.names = FALSE, 
          fileEncoding = "UTF-8")

final <- final %>% 
  select(-Total) %>% 
  gather(Peso, Frecuencia, -Residencia)

nacimientos <- final %>% 
  filter(Residencia %in% c("Tierra del Fuego", "Santa Cruz")) %>% 
  arrange(Residencia) %>% 
  filter(Peso != "Sin.especificar") %>% 
  mutate(Peso = str_replace_all(Peso, "De\\.|\\.", "") %>% 
           str_replace_all("a", "_"))

write.csv(nacimientos, "Labs/lab-1/cache/nacidos_seleccion_peso.csv", row.names = FALSE, 
          fileEncoding = "UTF-8")
