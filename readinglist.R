#------------------Load packages--------------
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("hablar")

library(ggplot2)
library(dplyr)
library(hablar)
library(magrittr)
library(tidyverse)

#------------------Read in data--------------
books <- read.csv("Books - Sheet2.csv")

#------------------Change column names--------------
colnames(books) <- c("book", "author", "year", "pages", "genre", "sub.genre")

#------------------Explore dataset--------------
nrow(books)
ncol(books)
head(books)
str(books)
View(books)

#------------------Facet years--------------
books.2018 <- books[books$year == 2018,]
books.2019 <- books[books$year == 2019,]
books.2020 <- books[books$year == 2020,]

sum(books.2018$pages)

#------------------Create visualizations--------------
#NUMBER OF PAGES

books.1 <- ggplot(data=books, aes(x=year, y=pages, fill=sub.genre))
books.1 + geom_col(color="Black", linewidth=.7)

#This one is ok
books.1 <- ggplot(data=books, aes(x=year, y=pages, fill=sub.genre))
books.1 + geom_bar(stat="identity") 

#This one is ok 
books.1 <- ggplot(data=books, aes(x=year, y=pages, fill=sub.genre))
books.1 + geom_bar(stat="identity", position=position_dodge()) +
  theme_minimal()

books.1 <- ggplot(data=books, aes(x=year, y=pages, fill=sub.genre))
books.1 + geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=pages), vjust=1.5, color="White", position=position_dodge(0.9))

books.1 <- ggplot(data=books, aes(x=year, y=pages, fill=sub.genre))
books.1 + geom_bar(stat="identity", position=position_dodge(), color="Black") +
  geom_text(aes(label=pages), vjust=1.5, color="White", position=position_dodge(0.9))

books.1 <- ggplot(data=books, aes(x=year, y=pages, fill=sub.genre))
books.1 + geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=book), vjust=1.5, color="White", position=position_dodge(0.9))

#Bar chart of genre % over all years (https://www.cedricscherer.com/2021/07/05/a-quick-how-to-on-labelling-bar-graphs-in-ggplot2/)

ordered <- books %>% 
  dplyr::count(sub.genre, sort=TRUE) %>% 
  dplyr::mutate(
    sub.genre=forcats::fct_rev(forcats::fct_inorder(sub.genre))
  )

ordered <- ordered %>% 
  dplyr::mutate(perc = paste0(sprintf("%4.1f", n/sum(n)*100), "%"))

ordered <-
  ordered %>% 
  mutate(
    color = case_when(
      row_number() == 1 ~ "goldenrod1",
      row_number() == 2 ~ "mediumpurple1",
      row_number() == 3 ~ "coral2",
      sub.genre == "Other" ~ "gray85",
      TRUE ~ "gray80"
    )
  )

ggplot(ordered, aes(x=n, y=sub.genre, fill=color)) +
  geom_col() +
  geom_text(aes(label=perc),
            hjust=1, nudge_x=1) +
  scale_fill_identity(guide="none") +
  theme_minimal()

ggplot(ordered, aes(x=n, y=sub.genre, fill=color)) +
  geom_col() +
  geom_text(aes(label=perc),
            hjust=1, nudge_x=-.1, size=4, fontface="bold", fill="white", label.size=0) +
  scale_x_continuous(expand=c(.01,.01)) +
  scale_fill_identity(guide="none") +
  theme_void() +
  theme(axis.text.y=element_text(size=12, hjust=1),
        plot.margin=margin(rep(15, 4)))

ggplot(ordered, aes(x=n, y=sub.genre, fill=color)) +
  geom_col() +
  geom_label(
    aes(label=perc),
    hjust=1, nudge_x=-.1, 
    size=4, fontface="bold", 
    fill="white", label.size=0
  ) +
  scale_x_continuous(expand=c(.01,.01)) +
  scale_fill_identity(guide="none") +
  theme_void() +
  theme(axis.text.y=element_text(size=12, hjust=1),
        plot.margin=margin(rep(15, 4)))
