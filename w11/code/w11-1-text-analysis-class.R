# ============================================================================
# MKT 566, Week 10: Text analysis, turning words into numbers
# ============================================================================
# This script reproduces every chart and table from the text-analysis slides,
# in the order they appear:
#   PART 1  the data: 5,000 labeled IMDB movie reviews (no download needed)
#   PART 2  cleaning and tokenizing one review
#   PART 3  the most common words (before removing stopwords)
#   PART 4  removing stopwords, and the top words after
#   PART 5  bag-of-words on a tiny toy corpus (three phone reviews)
#   PART 6  TF-IDF by hand, on the same toy corpus
#   PART 7  which words separate positive from negative reviews
#   PART 8  sentiment with a dictionary (sentimentr), and how well it does
#
# HOW TO RUN THIS SCRIPT (same workflow as before):
#   - Run it from the top, one step at a time: click on a line and press
#     Cmd+Enter (Mac) or Ctrl+Enter (Windows).
#   - Tables print in the terminal (the R console). Charts open in a VS Code
#     tab and are also saved as PDFs in the "figures" folder.
#   - Lines starting with # are comments: notes for humans, ignored by R.
#
# You are NOT expected to memorize this code. Read the comment above each
# block, run it, and look at the result. If you want to understand a
# specific line, select it and ask your AI assistant to explain it.
# ============================================================================

#### install libraries ####
pkgs <- c("ggplot2", "data.table", "scales", "text2vec", "sentimentr",
          "stopwords")
install.packages(setdiff(pkgs, rownames(installed.packages())),
                 repos = "https://cloud.r-project.org")
#####

# load libraries
library(ggplot2)      # charts
library(data.table)   # fast tables
library(scales)       # nicer axis labels
library(text2vec)     # text tools + the movie-review data: new this week
library(sentimentr)   # dictionary-based sentiment: new this week
library(stopwords)    # standard stopword lists: new this week

# ---- housekeeping: safe to run without understanding -----------------------
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
dir.create("figures", showWarnings = FALSE)
usc_red <- "#990000"; blue <- "#0072B2"
# ---------------------------------------------------------------------------

# ============================================================================
# PART 1: The data
# ============================================================================
# 5,000 real IMDB movie reviews, each labeled positive (1) or negative (0).
# The dataset ships WITH the text2vec package, so there is nothing to
# download: the line below loads it into memory.

data("movie_review", package = "text2vec")
rv <- as.data.table(movie_review)
rv[, label := ifelse(sentiment == 1, "positive", "negative")]

nrow(rv)                 # 5,000 reviews
rv[, .N, by = label]     # roughly half positive, half negative

# read one full review of each kind (they are long!)
rv[label == "positive", review[3]]
rv[label == "negative", review[3]]

# ============================================================================
# PART 2: Cleaning and tokenizing
# ============================================================================
# Step 1 (clean): lowercase everything and strip the HTML tags that litter
# scraped text (these reviews are full of "<br />" line breaks).
# Step 2 (tokenize): split each review into individual words (tokens).

rv[, clean := gsub("<[^>]+>", " ", tolower(review))]   # lowercase + de-HTML
rv[, clean := gsub("\\\\", "", clean)]                 # stray backslashes

# see what cleaning does to the first 200 characters of one review:
substr(rv$review[1], 1, 200)   # before
substr(rv$clean[1], 1, 200)    # after

# tokenize all 5,000 reviews at once: a list with one vector of words per
# review ("word_tokenizer" also drops punctuation and numbers)
toks <- word_tokenizer(rv$clean)
toks[[1]][1:15]                # the first 15 tokens of review 1

# ============================================================================
# PART 3: The most common words
# ============================================================================
# Count how often every distinct word appears across all 5,000 reviews.
# create_vocabulary() does the counting; the rest is sorting and plotting.

vocab <- as.data.table(create_vocabulary(itoken(toks, progressbar = FALSE)))
setorder(vocab, -term_count)

sum(vocab$term_count)    # about 1.2 million words in total...
nrow(vocab)              # ...drawn from ~42,000 distinct terms

top_all <- vocab[1:15]
top_all                  # look at the top 15: all grammar glue

p1 <- ggplot(top_all, aes(x = term_count, y = reorder(term, term_count))) +
  geom_col(fill = usc_red, width = 0.7) +
  scale_x_continuous(labels = comma) +
  labs(title = "The 15 Most Common Words in 5,000 Reviews",
       x = "Times the word appears", y = NULL) +
  theme_minimal(base_size = 14)
p1
ggsave("figures/01_top_words_raw.pdf", p1, width = 8, height = 4.5)

# ============================================================================
# PART 4: Removing stopwords
# ============================================================================
# "the", "a", "and" appear in every document, so they cannot separate
# anything from anything. We delete them using a standard list. The list is
# a CHOICE: print it, and notice it contains "not": in sentiment problems
# you may want to keep that word!

sw <- stopwords::stopwords("en")
length(sw)        # about 175 words
head(sw, 20)      # the start of the list

top_nostop <- vocab[!term %in% sw][1:10]
top_nostop        # now the top words mean something: movie, film, story...

p2 <- ggplot(top_nostop, aes(x = term_count, y = reorder(term, term_count))) +
  geom_col(fill = blue, width = 0.7) +
  scale_x_continuous(labels = comma) +
  labs(title = "The Top 10 After Removing Stopwords",
       x = "Times the word appears", y = NULL) +
  theme_minimal(base_size = 14)
p2
ggsave("figures/02_top_words_nostop.pdf", p2, width = 8, height = 4.5)

# ============================================================================
# PART 5: Bag-of-words on a toy corpus
# ============================================================================
# Three made-up phone reviews, small enough to count by eye. The
# bag-of-words matrix has one row per document, one column per word, and
# counts in the cells. Word ORDER is thrown away: that is the "bag".

toy <- data.table(
  doc  = c("A", "B", "C"),
  text = c("great phone great camera",
           "terrible battery terrible screen",
           "great battery"))
toy

# split into (doc, word) pairs, count, and reshape into the matrix
toy_long <- toy[, .(word = unlist(strsplit(text, " "))), by = doc]
dtm_toy  <- dcast(toy_long[, .N, by = .(doc, word)], doc ~ word,
                  value.var = "N", fill = 0)
dtm_toy   # compare with the slide: review A is the vector (0, 1, 2, 1, 0, 0)

# ============================================================================
# PART 6: TF-IDF by hand
# ============================================================================
# TF-IDF = TF x IDF, where
#   TF(word, doc)  = count of the word in the doc / words in the doc
#   IDF(word)      = log( number of docs / docs containing the word )
# A word scores high only if it is common HERE and rare EVERYWHERE ELSE.

tf_tab <- toy_long[, .N, by = .(doc, word)]
tf_tab[, tf := N / sum(N), by = doc]                 # the TF part
df_tab <- toy_long[, .(df = uniqueN(doc)), by = word]
tf_tab <- merge(tf_tab, df_tab, by = "word")
tf_tab[, idf := log(3 / df)]                         # the IDF part (N = 3)
tf_tab[, tf_idf := round(tf * idf, 3)]

# document A, sorted by TF-IDF: "phone" (once, but only in A) beats
# "great" (twice, but in 2 of the 3 documents)
tf_tab[doc == "A"][order(-tf_idf)]

# ============================================================================
# PART 7: Which words separate positive from negative reviews
# ============================================================================
# For every word: how often do positive reviews use it, and how often do
# negative reviews use it (per 10,000 words)? Then take the ratio. "worst"
# at 8x means negative reviews use it 8 times more often than positive ones.

tok_dt <- data.table(doc  = rep(seq_len(nrow(rv)), lengths(toks)),
                     word = unlist(toks))
tok_dt[, label := rv$label[doc]]

freq <- tok_dt[!word %in% sw & nchar(word) > 2, .N, by = .(label, word)]
freq[, per10k := N / sum(N) * 10000, by = label]     # usage rate per 10k words

wide <- dcast(freq, word ~ label, value.var = "per10k", fill = 0)
wide <- wide[positive + negative > 8]                # keep common-ish words
wide[, times_neg := (negative + 0.5) / (positive + 0.5)]
wide[, times_pos := (positive + 0.5) / (negative + 0.5)]

wide[order(-times_neg)][1:6]   # the most negative-leaning words
wide[order(-times_pos)][1:6]   # the most positive-leaning words

sep_words <- rbind(
  wide[order(-times_neg)][1:6][, .(word, times = -times_neg,
                                   side = "used more in negative reviews")],
  wide[order(-times_pos)][1:6][, .(word, times = times_pos,
                                   side = "used more in positive reviews")])
sep_words <- sep_words[order(times)]
sep_words[, word := factor(word, levels = word)]

p3 <- ggplot(sep_words, aes(x = times, y = word, fill = side)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0) +
  scale_fill_manual(values = setNames(c(usc_red, blue),
                    c("used more in negative reviews",
                      "used more in positive reviews")), name = NULL) +
  scale_x_continuous(breaks = seq(-8, 4, 2),
                     labels = function(x) paste0(abs(x), "x")) +
  labs(title = "Which Words Tilt Negative, Which Tilt Positive",
       x = "How many times more often the word appears on that side",
       y = NULL) +
  theme_minimal(base_size = 14) + theme(legend.position = "top")
p3
ggsave("figures/03_separating_words.pdf", p3, width = 8, height = 4.5)

# ============================================================================
# PART 8: Sentiment with a dictionary (sentimentr)
# ============================================================================
# sentimentr scores text with a human-built dictionary of charged words
# (great = +0.75, terrible = -0.75) and handles VALENCE SHIFTERS: "not
# great" flips the sign, "absolutely terrible" amplifies it.

demo_sent <- c("This movie is great.",
               "This movie is not great.",
               "Absolutely terrible, I want my money back.",
               "The plot is slow at first, but the ending is fantastic.")
sentiment_by(demo_sent)   # one average score per sentence; compare 1 vs. 2!

# --- how good is the dictionary, really? -----------------------------------
# Score 500 random reviews, call a review "positive" when its score is
# above 0, and compare with the true labels. No training data used at all.

set.seed(123)
samp <- rv[sample(.N, 500)]
scores <- sentiment_by(get_sentences(samp$clean))
samp[, dict_label := ifelse(scores$ave_sentiment > 0, "positive", "negative")]

table(predicted = samp$dict_label, actual = samp$label)
mean(samp$dict_label == samp$label)   # about 76% correct, with zero training

# 76% beats coin-flipping (50%) by a lot, but notice WHERE it fails: it
# calls far too many negative reviews positive (sarcasm, faint praise,
# "great idea, terrible execution"). On Thursday we TRAIN a model on the
# labeled reviews instead, and it will beat the dictionary. That model
# needs the TF-IDF matrix from today, plus one new tool: regularization.
