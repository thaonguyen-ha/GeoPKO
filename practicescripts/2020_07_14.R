library(tidyverse)

####Extracting dataframe; choosing mission ####

UNMISS <- GeoPKO %>% filter(Mission=="UNMISS")

example <- UNMISS %>% filter(Source=="Map no. 4456 Rev. 09")
#### concat####

example <- example %>% select(Source, Mission, year, month, No.troops, 16:44)
example1 <- example %>% gather(TCC.no, TCC.name, name.of.TCC1:name.of.TCC14)


####Tilo's solution####

df <- tibble(somecolumn = 10:12, foo.1 = as.character(1:3), bar.1 = letters[1:3], foo.2 = as.character(4:6), bar.2 = letters[4:6])
df %>%
  pivot_longer(-somecolumn, names_to="name", values_to="value") %>%
  separate(col="name", into=c("var", "unit"), sep="\\.") %>%
  pivot_wider(names_from="var", values_from="value")

example1 <- example1 %>% select()


#### This one works.

example2 <- example %>%
  pivot_longer(c(7:34),
               names_to = c(".value", "TCC_No"),
               names_sep="_")

UNMISS2 <- UNMISS %>% select(Source, Mission, year, month, 16:44)
colnames(UNMISS2) <- sub("name.of.TCC", "nameofTCC_", colnames(UNMISS2))
colnames(UNMISS2) <- sub("No.troops.per.TCC", "notroopsperTCC_", colnames(UNMISS2))


UNMISS2 <- UNMISS2 %>% mutate_at(vars(starts_with("notroopsperTCC")), as.character)
UNMISS3 <- UNMISS2 %>% pivot_longer(c(6:33), names_to=c(".value", "TCC_id"),
                                    names_sep="_")
UNMISS3$notroopsperTCC <- as.numeric(UNMISS3$notroopsperTCC)

UNMISS3 <- UNMISS3 %>% select(Source, Mission, year, month, No.TCC, nameofTCC, notroopsperTCC)
UNMISS3 <- UNMISS3 %>% filter(!is.na(notroopsperTCC))
UNMISS3 <- UNMISS3 %>% group_by(Source, Mission, year, month, nameofTCC) %>% summarise(total=sum(notroopsperTCC))

UNMISS4 <- UNMISS3 %>% add_count(Source)

UNMISS_final <- UNMISS4 %>% mutate(TCC_sum = str_c(nameofTCC, total, sep="-")) %>%
  select(-nameofTCC, -total)

UNMISS_final <- UNMISS_final %>%  group_by(Source, Mission, year, month, n) %>%
  summarise_each(funs(paste(., collapse=",")))


