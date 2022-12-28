#------------------Load packages--------------
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("hablar")
#install.packages("tidyr")
#install.packages("reshape2")

#library(ggplot2)
#library(dplyr)
#library(hablar)
#library(magrittr)
#library(tidyverse)
#library(tidyr)
#library(knitr)
#library(reshape2)

#------------------read in data--------------
# read in 
books <- read.csv("books12:27.csv")
# convert to local data frame
books_l <- tibble::as_tibble(books)
# change column names
colnames(books_l) <- c("book", "author", "year", "pages", "genre", "sub.genre")

#------------------explore data--------------
# for each year, # of books read 
book.count <- books_l %>%
  group_by(year) %>%
  summarise(book_count=n()) 

ggplot(book.count, aes(x=year, y=book_count)) +
  geom_col()

# for each year, # of pages read
page.count <- books_l %>%
  group_by(year) %>%
  summarise(pages_sum = sum(pages))

#...hacky way to add labels 
count <- c(10, 15, 16, 6, 11)
page.count.1 <- cbind(page.count, count)

ggplot(page.count, aes(x=year, y=pages_sum)) +
  geom_col() +
  geom_text(data=page.count.1, aes(x=year, y=pages_sum + 200, label=count))

ggplot(ordered, aes(x=n, y=sub.genre, fill=color)) +
  geom_col() 


page.count.1 <-
  page.count.1 %>% 
  mutate(
    col.colors = case_when(
      row_number() == 1 ~ "gray80",
      row_number() == 2 ~ "gray80",
      row_number() == 3 ~ "gray80",
      row_number() == 4 ~ "gray80",
      row_number() == 5 ~ "seagreen3"
    ))

ggplot(page.count.1, aes(x=year, y=pages_sum, fill=col.colors)) +
  geom_col() +
  coord_flip() +
  geom_text(data=page.count.1, aes(x=year, y=pages_sum + 200, label=count)) +
  theme_light() +
  theme(
    panel.grid.major=element_blank(), 
    panel.border=element_blank(),
    axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank()) +
  scale_fill_identity(guide="none") +
  theme(
    axis.text.y=element_text(size=12, hjust=1, face="bold"),
    plot.margin=margin(rep(15, 4))) +
  theme(
    axis.text.x=element_text(size=12, hjust=.5),
    plot.margin=margin(rep(15, 4))) +
  theme(
    axis.title.x=element_text(vjust=-3)) + 
  theme(
    legend.position="none"
  ) +
  xlab("") +
  ylab("# of Pages") 


df <- data.frame(ID = c(1,2,3,4), Type = c("A","B","A","B"), Score1 = c(10,20,30,40), Score2 = c(20,40,60,80))


rawscore <- df[, c("Type","Score1", "Score2")]
rawscore <- melt(rawscore, id = c("Type"))

dfmean <- rawscore %>% 
  group_by(interaction(variable, Type)) %>% 
  summarise(m = mean(value), count = n())
names(dfmean)[1] <- "Inter"

ggplot(rawscore, aes(x = interaction(variable, Type), y = value)) + 
  geom_bar(aes(fill = variable), stat="summary", fun.y="mean", position="dodge") +
  geom_text(data = dfmean, aes(x = Inter, y = m + 2, label = count))






# across all years, average # of books read per year
books_l %>%
  summarise(book_count=n()/n_distinct(year)) 

# across all years, # of fiction and non-fiction read
genre.count <- books_l %>% 
  group_by(genre) %>%
  summarise(book_count=n()) 

ggplot(genre.count, aes(x=genre, y=book_count)) +
  geom_col() 

# across all years, # of books in each sub genre and ordered from most to least
books_l %>% 
  group_by(sub.genre) %>%
  summarise(book_count=n()) %>%
  arrange(desc(book_count))

# faster way of across all years, # of books in each sub genre and ordered from most to least
books_l %>%
  count(sub.genre, sort=TRUE)

# across all years, % of books in each genre
ordered <- books_l %>% 
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
      row_number() == 4 ~ "seagreen3",
      TRUE ~ "gray80"
    ))

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



