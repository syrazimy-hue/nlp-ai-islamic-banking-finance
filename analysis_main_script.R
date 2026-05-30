#==============================================================================#
#  A NLP-Based Analysis of the AI Landscape in Islamic Banking and Finance  #
#  Lexicon-Based Approach using R                                              #
#  Dataset: Data_CleanedFinal.xlsx (PaperID, Abstract)                         #
#==============================================================================#

# Set working directory
setwd("C:/Users/YourName/Desktop/Paper/Final")

# Install required packages
required_packages <- c(
  "readxl",        # Read Excel files
  "tidyverse",     # Data wrangling & visualization (dplyr, ggplot2, tidyr, stringr, etc.)
  "tidytext",      # Text mining in tidy format
  "textdata",      # Access to sentiment lexicons (AFINN, NRC, Bing, Loughran)
  "syuzhet",       # Syuzhet sentiment & emotion detection
  "wordcloud",     # Word clouds
  "RColorBrewer",  # Color palettes
  "scales",        # Axis formatting
  "gridExtra",     # Arrange multiple plots
  "ggrepel",       # Non-overlapping text labels
  "openxlsx",      # Export tables to Excel
  "knitr",         # Nice tables
  "reshape2",      # Data reshaping
  "topicmodels",   # LDA topic modeling
  "tm",            # Text mining infrastructure
  "SnowballC",     # Word stemming
  "ldatuning",     # Optimal number of topics
  "corrplot"       # Correlation plots
)

# Install missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, dependencies = TRUE)

# Load all libraries
invisible(lapply(required_packages, library, character.only = TRUE))

# NOTE: On first run, textdata may prompt you to download lexicons.
# Accept the prompts for AFINN, Bing, NRC, and Loughran lexicons.

# ---- 1. LOAD & PREPARE DATA ----

# Read dataset
df <- read_excel("data_cleaned_final")

# Inspect
cat("Dataset dimensions:", nrow(df), "papers x", ncol(df), "columns\n")
cat("Column names:", paste(colnames(df), collapse = ", "), "\n")
head(df, 3)

# Ensure column names are correct
colnames(df) <- c("PaperID", "Abstract")

# Remove any rows with missing abstracts
df <- df %>% filter(!is.na(Abstract) & Abstract != "")
cat("Papers after cleaning:", nrow(df), "\n")

# ---- 2. TEXT TOKENIZATION ----

# Tokenize abstracts into individual words (unnest_tokens)
tokens <- df %>%
  unnest_tokens(word, Abstract) %>%
  anti_join(stop_words, by = "word") %>%       # Remove stopwords
  filter(!str_detect(word, "^[0-9]+$"))         # Remove pure numbers

cat("Total tokens (after stopword removal):", nrow(tokens), "\n")
cat("Unique words:", n_distinct(tokens$word), "\n")

# Save token summary
token_summary <- tokens %>% count(word, sort = TRUE)

#==============================================================================#
#  RESEARCH OBJECTIVE 1 (RO1):                                                 #
#  Overall Sentiment Orientation (Positive / Negative / Neutral)               #
#==============================================================================#

# ---- RO1: Bing Sentiment (Primary) ----

bing_lex <- get_sentiments("bing")

bing_sentiment <- tokens %>%
  inner_join(bing_lex, by = "word") %>%
  count(PaperID, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(
    net_sentiment = positive - negative,
    orientation = case_when(
      net_sentiment > 0 ~ "Positive",
      net_sentiment < 0 ~ "Negative",
      TRUE ~ "Neutral"
    )
  )

# Merge back to get full coverage (papers with no sentiment words = Neutral)
bing_full <- df %>%
  select(PaperID) %>%
  left_join(bing_sentiment, by = "PaperID") %>%
  mutate(
    positive = replace_na(positive, 0),
    negative = replace_na(negative, 0),
    net_sentiment = replace_na(net_sentiment, 0),
    orientation = replace_na(orientation, "Neutral")
  )

# --- RO1 Table 1: Overall Sentiment Distribution ---
RO1_Table1 <- bing_full %>%
  count(orientation) %>%
  mutate(percentage = round(n / sum(n) * 100, 2)) %>%
  arrange(desc(n))

cat("\n===== RO1 Table 1: Overall Sentiment Distribution (Bing) =====\n")
print(RO1_Table1)

write.xlsx(RO1_Table1, "RO1 Table 1 Overall Sentiment Distribution Bing.xlsx", rowNames = FALSE)

# --- RO1 Table 2: Descriptive Statistics of Sentiment Scores ---
RO1_Table2 <- bing_full %>%
  summarise(
    N = n(),
    Mean = round(mean(net_sentiment), 3),
    Median = median(net_sentiment),
    SD = round(sd(net_sentiment), 3),
    Min = min(net_sentiment),
    Max = max(net_sentiment),
    Positive_Papers = sum(orientation == "Positive"),
    Negative_Papers = sum(orientation == "Negative"),
    Neutral_Papers = sum(orientation == "Neutral")
  )

cat("\n===== RO1 Table 2: Descriptive Statistics of Net Sentiment =====\n")
print(RO1_Table2)

write.xlsx(RO1_Table2, "RO1 Table 2 Descriptive Statistics Sentiment Scores.xlsx", rowNames = FALSE)

# --- RO1 Figure 1: Sentiment Distribution Bar Chart ---
p_ro1_fig1 <- ggplot(RO1_Table1, aes(x = reorder(orientation, -n), y = n, fill = orientation)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(n, " (", percentage, "%)")), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("Positive" = "#2E86C1", "Negative" = "#E74C3C", "Neutral" = "#95A5A6")) +
  labs(
    title = "Overall Sentiment Orientation of Academic Publications",
    subtitle = "AI in Islamic Banking and Finance (Bing Lexicon)",
    x = "Sentiment Orientation", y = "Number of Papers"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  ylim(0, max(RO1_Table1$n) * 1.15)

ggsave("RO1 Figure 1 Overall Sentiment Distribution Bing.png", p_ro1_fig1, width = 8, height = 6, dpi = 300)

# --- RO1 Figure 2: Distribution of Net Sentiment Scores (Histogram) ---
p_ro1_fig2 <- ggplot(bing_full, aes(x = net_sentiment)) +
  geom_histogram(binwidth = 1, fill = "#3498DB", color = "white", alpha = 0.85) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) +
  labs(
    title = "Distribution of Net Sentiment Scores Across Papers",
    subtitle = "AI in Islamic Banking and Finance (Bing Lexicon)",
    x = "Net Sentiment Score (Positive - Negative)", y = "Frequency"
  ) +
  theme_minimal(base_size = 13)

ggsave("RO1 Figure 2 Net Sentiment Score Distribution.png", p_ro1_fig2, width = 8, height = 6, dpi = 300)

# --- RO1 Figure 3: Top Positive & Negative Words (Bing) ---
bing_word_counts <- tokens %>%
  inner_join(bing_lex, by = "word") %>%
  count(word, sentiment, sort = TRUE)

top_bing_words <- bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 15) %>%
  ungroup() %>%
  mutate(
    n = ifelse(sentiment == "negative", -n, n),
    word = reorder(word, n)
  )

p_ro1_fig3 <- ggplot(top_bing_words, aes(x = word, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = c("positive" = "#2E86C1", "negative" = "#E74C3C")) +
  labs(
    title = "Top 15 Positive and Negative Words (Bing Lexicon)",
    subtitle = "AI in Islamic Banking and Finance",
    x = NULL, y = "Frequency"
  ) +
  theme_minimal(base_size = 12)

ggsave("RO1 Figure 3 Top Positive Negative Words Bing.png", p_ro1_fig3, width = 9, height = 7, dpi = 300)

# --- RO1 Table 3: Top 20 Positive and Negative Words ---
RO1_Table3_pos <- bing_word_counts %>% filter(sentiment == "positive") %>% head(20)
RO1_Table3_neg <- bing_word_counts %>% filter(sentiment == "negative") %>% head(20)
RO1_Table3 <- bind_rows(
  RO1_Table3_pos %>% mutate(rank = row_number()),
  RO1_Table3_neg %>% mutate(rank = row_number())
) %>% select(rank, word, sentiment, n)

write.xlsx(RO1_Table3, "RO1 Table 3 Top 20 Positive Negative Words Bing.xlsx", rowNames = FALSE)

#==============================================================================#
#  RESEARCH OBJECTIVE 2 (RO2):                                                 #
#  Dominant Emotions in Academic Discourse                                      #
#==============================================================================#

# --- RO2: NRC Emotion Lexicon ---

nrc_lex <- get_sentiments("nrc")

# Get emotion categories only (exclude positive/negative)
nrc_emotions <- nrc_lex %>% filter(!sentiment %in% c("positive", "negative"))

emotion_counts <- tokens %>%
  inner_join(nrc_emotions, by = "word") %>%
  count(sentiment, sort = TRUE) %>%
  mutate(percentage = round(n / sum(n) * 100, 2))

# --- RO2 Table 1: Emotion Frequency Distribution (NRC) ---
RO2_Table1 <- emotion_counts %>% arrange(desc(n))

cat("\n===== RO2 Table 1: Emotion Distribution (NRC) =====\n")
print(RO2_Table1)

write.xlsx(RO2_Table1, "RO2 Table 1 Emotion Distribution NRC.xlsx", rowNames = FALSE)

# --- RO2 Figure 1: Emotion Bar Chart (NRC) ---
p_ro2_fig1 <- ggplot(RO2_Table1, aes(x = reorder(sentiment, n), y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(percentage, "%")), hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Dominant Emotions in Academic Discourse on AI in IBF",
    subtitle = "NRC Emotion Lexicon",
    x = "Emotion", y = "Word Count"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  xlim(NA, max(RO2_Table1$n) * 1.12)

ggsave("RO2 Figure 1 Emotion Distribution NRC.png", p_ro2_fig1, width = 9, height = 6, dpi = 300)

# --- RO2: Paper-Level Emotion Scores (NRC) ---
emotion_by_paper <- tokens %>%
  inner_join(nrc_emotions, by = "word") %>%
  count(PaperID, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0)

# --- RO2 Table 2: Descriptive Statistics of Emotions Across Papers ---
emotion_cols <- setdiff(colnames(emotion_by_paper), "PaperID")
RO2_Table2 <- data.frame(
  Emotion = emotion_cols,
  Mean = sapply(emotion_by_paper[emotion_cols], mean, na.rm = TRUE) %>% round(3),
  Median = sapply(emotion_by_paper[emotion_cols], median, na.rm = TRUE),
  SD = sapply(emotion_by_paper[emotion_cols], sd, na.rm = TRUE) %>% round(3),
  Min = sapply(emotion_by_paper[emotion_cols], min, na.rm = TRUE),
  Max = sapply(emotion_by_paper[emotion_cols], max, na.rm = TRUE)
) %>% arrange(desc(Mean))
rownames(RO2_Table2) <- NULL

cat("\n===== RO2 Table 2: Descriptive Statistics of Emotions =====\n")
print(RO2_Table2)

write.xlsx(RO2_Table2, "RO2 Table 2 Descriptive Statistics Emotions NRC.xlsx", rowNames = FALSE)

# --- RO2: Syuzhet Emotion Detection ---

syuzhet_emotions <- get_nrc_sentiment(df$Abstract)
syuzhet_emotions$PaperID <- df$PaperID

# Emotion columns from syuzhet (first 8 are emotions, last 2 are positive/negative)
emo_cols <- c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")

# --- RO2 Table 3: Syuzhet Emotion Summary ---
RO2_Table3 <- data.frame(
  Emotion = emo_cols,
  Total = colSums(syuzhet_emotions[emo_cols]),
  Mean = sapply(syuzhet_emotions[emo_cols], mean) %>% round(3),
  SD = sapply(syuzhet_emotions[emo_cols], sd) %>% round(3)
) %>% arrange(desc(Total))
rownames(RO2_Table3) <- NULL

cat("\n===== RO2 Table 3: Syuzhet Emotion Summary =====\n")
print(RO2_Table3)

write.xlsx(RO2_Table3, "RO2 Table 3 Syuzhet Emotion Summary.xlsx", rowNames = FALSE)

# --- RO2 Figure 2: Syuzhet Emotion Radar/Bar Chart ---
syuzhet_long <- RO2_Table3 %>%
  select(Emotion, Total) %>%
  mutate(Emotion = str_to_title(Emotion))

p_ro2_fig2 <- ggplot(syuzhet_long, aes(x = reorder(Emotion, Total), y = Total, fill = Emotion)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_brewer(palette = "Paired") +
  labs(
    title = "Emotion Detection Across Abstracts (Syuzhet Package)",
    subtitle = "AI in Islamic Banking and Finance",
    x = "Emotion", y = "Total Score"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

ggsave("RO2 Figure 2 Syuzhet Emotion Detection.png", p_ro2_fig2, width = 9, height = 6, dpi = 300)

# --- RO2 Figure 3: Top Words per Emotion (NRC) ---
top_emotion_words <- tokens %>%
  inner_join(nrc_emotions, by = "word") %>%
  count(sentiment, word, sort = TRUE) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 8) %>%
  ungroup()

p_ro2_fig3 <- ggplot(top_emotion_words, aes(x = reorder_within(word, n, sentiment), y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y", ncol = 4) +
  coord_flip() +
  scale_x_reordered() +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Top Words Associated with Each Emotion (NRC Lexicon)",
    subtitle = "AI in Islamic Banking and Finance",
    x = NULL, y = "Frequency"
  ) +
  theme_minimal(base_size = 11)

ggsave("RO2 Figure 3 Top Words Per Emotion NRC.png", p_ro2_fig3, width = 14, height = 8, dpi = 300)

# --- RO2 Table 4: Top 10 Words Per Emotion ---
RO2_Table4 <- tokens %>%
  inner_join(nrc_emotions, by = "word") %>%
  count(sentiment, word, sort = TRUE) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  mutate(rank = row_number()) %>%
  ungroup()

write.xlsx(RO2_Table4, "RO2 Table 4 Top 10 Words Per Emotion NRC.xlsx", rowNames = FALSE)

#==============================================================================#
#  RESEARCH OBJECTIVE 3 (RO3):                                                 #
#  Cross-Lexicon Comparison (Bing, AFINN, NRC, Syuzhet, Loughran-McDonald)     #
#==============================================================================#

# ---- RO3a: AFINN Lexicon ----
afinn_lex <- get_sentiments("afinn")

afinn_sentiment <- tokens %>%
  inner_join(afinn_lex, by = "word") %>%
  group_by(PaperID) %>%
  summarise(afinn_score = sum(value), .groups = "drop") %>%
  mutate(afinn_orientation = case_when(
    afinn_score > 0 ~ "Positive",
    afinn_score < 0 ~ "Negative",
    TRUE ~ "Neutral"
  ))

afinn_full <- df %>% select(PaperID) %>%
  left_join(afinn_sentiment, by = "PaperID") %>%
  mutate(afinn_score = replace_na(afinn_score, 0),
         afinn_orientation = replace_na(afinn_orientation, "Neutral"))

# ---- RO3b: NRC Sentiment (positive/negative only) ----
nrc_posneg <- get_sentiments("nrc") %>% filter(sentiment %in% c("positive", "negative"))

nrc_sentiment <- tokens %>%
  inner_join(nrc_posneg, by = "word") %>%
  count(PaperID, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(nrc_score = positive - negative,
         nrc_orientation = case_when(
           nrc_score > 0 ~ "Positive",
           nrc_score < 0 ~ "Negative",
           TRUE ~ "Neutral"
         ))

nrc_full <- df %>% select(PaperID) %>%
  left_join(nrc_sentiment %>% select(PaperID, nrc_score, nrc_orientation), by = "PaperID") %>%
  mutate(nrc_score = replace_na(nrc_score, 0),
         nrc_orientation = replace_na(nrc_orientation, "Neutral"))

# ---- RO3c: Syuzhet Lexicon ----
syuzhet_scores <- get_sentiment(df$Abstract, method = "syuzhet")
syuzhet_full <- df %>% select(PaperID) %>%
  mutate(syuzhet_score = syuzhet_scores,
         syuzhet_orientation = case_when(
           syuzhet_score > 0 ~ "Positive",
           syuzhet_score < 0 ~ "Negative",
           TRUE ~ "Neutral"
         ))

# ---- RO3d: Loughran-McDonald Lexicon ----
loughran_lex <- get_sentiments("loughran") %>%
  filter(sentiment %in% c("positive", "negative"))

loughran_sentiment <- tokens %>%
  inner_join(loughran_lex, by = "word") %>%
  count(PaperID, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(loughran_score = positive - negative,
         loughran_orientation = case_when(
           loughran_score > 0 ~ "Positive",
           loughran_score < 0 ~ "Negative",
           TRUE ~ "Neutral"
         ))

loughran_full <- df %>% select(PaperID) %>%
  left_join(loughran_sentiment %>% select(PaperID, loughran_score, loughran_orientation), by = "PaperID") %>%
  mutate(loughran_score = replace_na(loughran_score, 0),
         loughran_orientation = replace_na(loughran_orientation, "Neutral"))

# ---- RO3: Merge All Lexicons ----
cross_lexicon <- bing_full %>%
  select(PaperID, bing_score = net_sentiment, bing_orientation = orientation) %>%
  left_join(afinn_full %>% select(PaperID, afinn_score, afinn_orientation), by = "PaperID") %>%
  left_join(nrc_full %>% select(PaperID, nrc_score, nrc_orientation), by = "PaperID") %>%
  left_join(syuzhet_full %>% select(PaperID, syuzhet_score, syuzhet_orientation), by = "PaperID") %>%
  left_join(loughran_full %>% select(PaperID, loughran_score, loughran_orientation), by = "PaperID")

# --- RO3 Table 1: Cross-Lexicon Orientation Summary ---
orientation_summary <- data.frame(
  Lexicon = c("Bing", "AFINN", "NRC", "Syuzhet", "Loughran-McDonald"),
  Positive = c(
    sum(cross_lexicon$bing_orientation == "Positive"),
    sum(cross_lexicon$afinn_orientation == "Positive"),
    sum(cross_lexicon$nrc_orientation == "Positive"),
    sum(cross_lexicon$syuzhet_orientation == "Positive"),
    sum(cross_lexicon$loughran_orientation == "Positive")
  ),
  Negative = c(
    sum(cross_lexicon$bing_orientation == "Negative"),
    sum(cross_lexicon$afinn_orientation == "Negative"),
    sum(cross_lexicon$nrc_orientation == "Negative"),
    sum(cross_lexicon$syuzhet_orientation == "Negative"),
    sum(cross_lexicon$loughran_orientation == "Negative")
  ),
  Neutral = c(
    sum(cross_lexicon$bing_orientation == "Neutral"),
    sum(cross_lexicon$afinn_orientation == "Neutral"),
    sum(cross_lexicon$nrc_orientation == "Neutral"),
    sum(cross_lexicon$syuzhet_orientation == "Neutral"),
    sum(cross_lexicon$loughran_orientation == "Neutral")
  )
)

orientation_summary <- orientation_summary %>%
  mutate(Total = Positive + Negative + Neutral,
         Pct_Positive = round(Positive / Total * 100, 2),
         Pct_Negative = round(Negative / Total * 100, 2),
         Pct_Neutral = round(Neutral / Total * 100, 2))

RO3_Table1 <- orientation_summary
cat("\n===== RO3 Table 1: Cross-Lexicon Sentiment Orientation =====\n")
print(RO3_Table1)

write.xlsx(RO3_Table1, "RO3 Table 1 Cross-Lexicon Sentiment Orientation.xlsx", rowNames = FALSE)

# --- RO3 Table 2: Descriptive Statistics Across Lexicons ---
score_cols <- cross_lexicon %>% select(bing_score, afinn_score, nrc_score, syuzhet_score, loughran_score)
RO3_Table2 <- data.frame(
  Lexicon = c("Bing", "AFINN", "NRC", "Syuzhet", "Loughran-McDonald"),
  Mean = sapply(score_cols, mean) %>% round(3),
  Median = sapply(score_cols, median) %>% round(3),
  SD = sapply(score_cols, sd) %>% round(3),
  Min = sapply(score_cols, min),
  Max = sapply(score_cols, max)
)
rownames(RO3_Table2) <- NULL

cat("\n===== RO3 Table 2: Descriptive Statistics Across Lexicons =====\n")
print(RO3_Table2)

write.xlsx(RO3_Table2, "RO3 Table 2 Descriptive Statistics Cross-Lexicon.xlsx", rowNames = FALSE)

# --- RO3 Figure 1: Grouped Bar Chart - Sentiment Orientation by Lexicon ---
orient_long <- orientation_summary %>%
  select(Lexicon, Positive, Negative, Neutral) %>%
  pivot_longer(-Lexicon, names_to = "Orientation", values_to = "Count") %>%
  mutate(Orientation = factor(Orientation, levels = c("Positive", "Neutral", "Negative")))

p_ro3_fig1 <- ggplot(orient_long, aes(x = Lexicon, y = Count, fill = Orientation)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = Count), position = position_dodge(width = 0.7), vjust = -0.3, size = 3.2) +
  scale_fill_manual(values = c("Positive" = "#2E86C1", "Neutral" = "#95A5A6", "Negative" = "#E74C3C")) +
  labs(
    title = "Cross-Lexicon Comparison of Sentiment Orientation",
    subtitle = "AI in Islamic Banking and Finance",
    x = "Lexicon", y = "Number of Papers", fill = "Orientation"
  ) +
  theme_minimal(base_size = 13)

ggsave("RO3 Figure 1 Cross-Lexicon Sentiment Orientation.png", p_ro3_fig1, width = 10, height = 6, dpi = 300)

# --- RO3 Figure 2: Boxplot of Sentiment Scores Across Lexicons ---
scores_long <- cross_lexicon %>%
  select(PaperID, Bing = bing_score, AFINN = afinn_score, NRC = nrc_score,
         Syuzhet = syuzhet_score, `Loughran-McDonald` = loughran_score) %>%
  pivot_longer(-PaperID, names_to = "Lexicon", values_to = "Score")

p_ro3_fig2 <- ggplot(scores_long, aes(x = Lexicon, y = Score, fill = Lexicon)) +
  geom_boxplot(alpha = 0.7, outlier.size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Distribution of Sentiment Scores Across Five Lexicons",
    subtitle = "AI in Islamic Banking and Finance",
    x = "Lexicon", y = "Sentiment Score"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

ggsave("RO3 Figure 2 Boxplot Cross-Lexicon Scores.png", p_ro3_fig2, width = 10, height = 6, dpi = 300)

# --- RO3 Figure 3: Correlation Heatmap Across Lexicons ---
cor_matrix <- cor(score_cols, use = "complete.obs")

png("RO3 Figure 3 Correlation Heatmap Cross-Lexicon.png", width = 800, height = 700, res = 150)
corrplot(cor_matrix, method = "color", type = "upper", addCoef.col = "black",
         tl.col = "black", tl.srt = 45, number.cex = 0.9,
         title = "Correlation Between Lexicon Sentiment Scores",
         mar = c(0, 0, 2, 0),
         col = colorRampPalette(c("#E74C3C", "white", "#2E86C1"))(200))
dev.off()

# --- RO3 Table 3: Pairwise Correlation Matrix ---
RO3_Table3 <- as.data.frame(round(cor_matrix, 3))
RO3_Table3$Lexicon <- rownames(RO3_Table3)
RO3_Table3 <- RO3_Table3 %>% select(Lexicon, everything())

write.xlsx(RO3_Table3, "RO3 Table 3 Pairwise Correlation Matrix.xlsx", rowNames = FALSE)

# --- RO3 Table 4: Agreement Rate Across Lexicons ---
agreement <- cross_lexicon %>%
  mutate(all_agree = (bing_orientation == afinn_orientation &
                        afinn_orientation == nrc_orientation &
                        nrc_orientation == syuzhet_orientation &
                        syuzhet_orientation == loughran_orientation))

RO3_Table4 <- data.frame(
  Metric = c("Full Agreement (all 5 lexicons)", "Majority Agreement (>=4 lexicons)"),
  Count = c(
    sum(agreement$all_agree),
    {
      orient_matrix <- cross_lexicon %>%
        select(ends_with("_orientation"))
      majority <- apply(orient_matrix, 1, function(x) {
        tbl <- table(x)
        max(tbl) >= 4
      })
      sum(majority)
    }
  ),
  Percentage = NA
)
RO3_Table4$Percentage <- round(RO3_Table4$Count / nrow(cross_lexicon) * 100, 2)

cat("\n===== RO3 Table 4: Agreement Rate =====\n")
print(RO3_Table4)

write.xlsx(RO3_Table4, "RO3 Table 4 Agreement Rate Across Lexicons.xlsx", rowNames = FALSE)

#==============================================================================#
#  RESEARCH OBJECTIVE 4 (RO4):                                                 #
#  Dominant Themes Associated with Positive and Negative Sentiment             #
#==============================================================================#

# ---- RO4: Split Papers by Sentiment, Then Topic Model ----

# Classify papers using Bing orientation (primary lexicon)
positive_papers <- bing_full %>% filter(orientation == "Positive") %>% pull(PaperID)
negative_papers <- bing_full %>% filter(orientation == "Negative") %>% pull(PaperID)

df_pos <- df %>% filter(PaperID %in% positive_papers)
df_neg <- df %>% filter(PaperID %in% negative_papers)

cat("\nPositive papers:", nrow(df_pos), "| Negative papers:", nrow(df_neg), "\n")

# Function: Build DTM from abstracts
build_dtm <- function(abstracts) {
  corpus <- Corpus(VectorSource(abstracts))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  # Remove domain-specific generic words
  corpus <- tm_map(corpus, removeWords, c("study", "paper", "research", "results",
                                          "findings", "analysis", "data", "based",
                                          "using", "can", "also", "however", "one",
                                          "may", "will", "use", "used", "article"))
  corpus <- tm_map(corpus, stripWhitespace)
  dtm <- DocumentTermMatrix(corpus, control = list(minWordLength = 3))
  # Remove sparse terms (keep terms appearing in at least 5% of documents)
  dtm <- removeSparseTerms(dtm, 0.95)
  # Remove empty rows
  row_totals <- apply(dtm, 1, sum)
  dtm <- dtm[row_totals > 0, ]
  return(dtm)
}

# Build DTMs
dtm_pos <- build_dtm(df_pos$Abstract)
dtm_neg <- build_dtm(df_neg$Abstract)

cat("Positive DTM:", nrow(dtm_pos), "docs x", ncol(dtm_pos), "terms\n")
cat("Negative DTM:", nrow(dtm_neg), "docs x", ncol(dtm_neg), "terms\n")

# ---- RO4: LDA Topic Modeling ----

# Set number of topics (k)
k_topics <- 5  # Adjust as needed

set.seed(42)
lda_pos <- LDA(dtm_pos, k = k_topics, method = "Gibbs",
               control = list(seed = 42, burnin = 1000, iter = 2000, thin = 100))

set.seed(42)
lda_neg <- LDA(dtm_neg, k = k_topics, method = "Gibbs",
               control = list(seed = 42, burnin = 1000, iter = 2000, thin = 100))

# Extract top terms per topic
extract_topics <- function(lda_model, n_terms = 10) {
  topics <- tidy(lda_model, matrix = "beta")
  top_terms <- topics %>%
    group_by(topic) %>%
    slice_max(beta, n = n_terms) %>%
    ungroup() %>%
    arrange(topic, desc(beta)) %>%
    mutate(beta = round(beta, 5))
  return(top_terms)
}

pos_topics <- extract_topics(lda_pos, 10)
neg_topics <- extract_topics(lda_neg, 10)

# --- RO4 Table 1: Top Terms in Positive Sentiment Topics ---
RO4_Table1 <- pos_topics %>%
  group_by(topic) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic_", topic))

cat("\n===== RO4 Table 1: Themes in Positive Sentiment Papers =====\n")
print(RO4_Table1 %>% pivot_wider(id_cols = rank, names_from = topic, values_from = term))

write.xlsx(RO4_Table1, "RO4 Table 1 Positive Sentiment Topics LDA.xlsx", rowNames = FALSE)

# --- RO4 Table 2: Top Terms in Negative Sentiment Topics ---
RO4_Table2 <- neg_topics %>%
  group_by(topic) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic_", topic))

cat("\n===== RO4 Table 2: Themes in Negative Sentiment Papers =====\n")
print(RO4_Table2 %>% pivot_wider(id_cols = rank, names_from = topic, values_from = term))

write.xlsx(RO4_Table2, "RO4 Table 2 Negative Sentiment Topics LDA.xlsx", rowNames = FALSE)

# --- RO4 Figure 1: Top Terms per Topic (Positive Papers) ---
p_ro4_fig1 <- ggplot(pos_topics, aes(x = reorder_within(term, beta, topic), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~paste("Topic", topic), scales = "free_y", ncol = 3) +
  coord_flip() +
  scale_x_reordered() +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Dominant Themes in Positive Sentiment Papers (LDA)",
    subtitle = "AI in Islamic Banking and Finance",
    x = NULL, y = "Beta (Term Probability)"
  ) +
  theme_minimal(base_size = 11)

ggsave("RO4 Figure 1 Positive Sentiment Topics LDA.png", p_ro4_fig1, width = 14, height = 8, dpi = 300)

# --- RO4 Figure 2: Top Terms per Topic (Negative Papers) ---
p_ro4_fig2 <- ggplot(neg_topics, aes(x = reorder_within(term, beta, topic), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~paste("Topic", topic), scales = "free_y", ncol = 3) +
  coord_flip() +
  scale_x_reordered() +
  scale_fill_brewer(palette = "Reds") +
  labs(
    title = "Dominant Themes in Negative Sentiment Papers (LDA)",
    subtitle = "AI in Islamic Banking and Finance",
    x = NULL, y = "Beta (Term Probability)"
  ) +
  theme_minimal(base_size = 11)

ggsave("RO4 Figure 2 Negative Sentiment Topics LDA.png", p_ro4_fig2, width = 14, height = 8, dpi = 300)

# --- RO4: Word Clouds by Sentiment ---

# Positive sentiment word cloud
pos_word_freq <- tokens %>%
  filter(PaperID %in% positive_papers) %>%
  count(word, sort = TRUE) %>%
  head(100)

png("RO4 Figure 3 Word Cloud Positive Sentiment.png", width = 800, height = 600, res = 150)
wordcloud(words = pos_word_freq$word, freq = pos_word_freq$n,
          min.freq = 2, max.words = 80, random.order = FALSE,
          colors = brewer.pal(8, "Blues"), scale = c(3, 0.5))
title(main = "Word Cloud: Positive Sentiment Papers")
dev.off()

# Negative sentiment word cloud
neg_word_freq <- tokens %>%
  filter(PaperID %in% negative_papers) %>%
  count(word, sort = TRUE) %>%
  head(100)

png("RO4 Figure 4 Word Cloud Negative Sentiment.png", width = 800, height = 600, res = 150)
wordcloud(words = neg_word_freq$word, freq = neg_word_freq$n,
          min.freq = 2, max.words = 80, random.order = FALSE,
          colors = brewer.pal(8, "Reds"), scale = c(3, 0.5))
title(main = "Word Cloud: Negative Sentiment Papers")
dev.off()

# --- RO4 Table 3: Comparative Top Terms (Positive vs Negative) ---
pos_top30 <- tokens %>%
  filter(PaperID %in% positive_papers) %>%
  inner_join(bing_lex, by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  head(30) %>%
  mutate(source = "Positive Papers", rank = row_number())

neg_top30 <- tokens %>%
  filter(PaperID %in% negative_papers) %>%
  inner_join(bing_lex, by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  head(30) %>%
  mutate(source = "Negative Papers", rank = row_number())

RO4_Table3 <- bind_rows(pos_top30, neg_top30)
write.xlsx(RO4_Table3, "RO4 Table 3 Comparative Top Terms by Sentiment Group.xlsx", rowNames = FALSE)

# --- Save Master Cross-Lexicon Dataset ---
write.xlsx(cross_lexicon, "Master Cross-Lexicon Sentiment Scores.xlsx", rowNames = FALSE)

#==============================================================================#
#  END OF SCRIPT                                                               #
#==============================================================================#
