library(stm)

data <- read.csv("Embedded_lies_manprep.csv", sep = ";")

# General EDA

## Check general count
table(data$Sex)
table(data$Valence_event)

## Remove CONSENT_REVOKED, DATA_EXPIRED, and Prefer Not To Say
data <- data[!(data$Sex %in% c("CONSENT_REVOKED", "DATA_EXPIRED", "Prefer not to say")),]

## Check removal
table(data$Sex)

## Check count per event type
table(data$Event, data$Sex)

## Create dataframes per event_type being considered
data_noticket <- data[data$Event == "Taking the bus/train without the ticket",]
data_missdeadline <- data[data$Event == "Missing a deadline at work because of bad organisation",]
data_caraccidentinvolve <- data[data$Event == "Being involved in a car accident",]
data_cheatexam <- data[data$Event == "Cheating on an exam",]
data_getfired <- data[data$Event == "Getting fired",]

# STM Parameter Testing

## Processing text data for each data frame
processed_noticket <- textProcessor(data_noticket$False_event_clean, metadata = data_noticket, stem = FALSE)
processed_missdeadline <- textProcessor(data_missdeadline$False_event_clean, metadata = data_missdeadline, stem = FALSE)
processed_caraccidentinvolve <- textProcessor(data_caraccidentinvolve$False_event_clean, metadata = data_caraccidentinvolve, stem = FALSE)
processed_cheatexam <- textProcessor(data_cheatexam$False_event_clean, metadata = data_cheatexam, stem = FALSE)
processed_getfired <- textProcessor(data_getfired$False_event_clean, metadata = data_getfired, stem = FALSE)

## Checking word removal thresholds
plotRemoved(processed_noticket$documents, lower.thresh = seq(1, 123, by = 1))
plotRemoved(processed_missdeadline$documents, lower.thresh = seq(1, 96, by = 1))
plotRemoved(processed_caraccidentinvolve$documents, lower.thresh = seq(1, 47, by = 1))
plotRemoved(processed_cheatexam$documents, lower.thresh = seq(1, 48, by = 1))
plotRemoved(processed_getfired$documents, lower.thresh = seq(1, 34, by = 1))

## Preparing documents
out_noticket <- prepDocuments(processed_noticket$documents, processed_noticket$vocab, processed_noticket$meta, lower.thresh = 3)
docs_noticket <- out_noticket$documents
vocab_noticket <- out_noticket$vocab
meta_noticket <- out_noticket$meta

out_missdeadline <- prepDocuments(processed_missdeadline$documents, processed_missdeadline$vocab, processed_missdeadline$meta, lower.thresh = 3)
docs_missdeadline <- out_missdeadline$documents
vocab_missdeadline <- out_missdeadline$vocab
meta_missdeadline <- out_missdeadline$meta

out_caraccidentinvolve <- prepDocuments(processed_caraccidentinvolve$documents, processed_caraccidentinvolve$vocab, processed_caraccidentinvolve$meta, lower.thresh = 2)
docs_caraccidentinvolve <- out_caraccidentinvolve$documents
vocab_caraccidentinvolve <- out_caraccidentinvolve$vocab
meta_caraccidentinvolve <- out_caraccidentinvolve$meta

out_cheatexam <- prepDocuments(processed_cheatexam$documents, processed_cheatexam$vocab, processed_cheatexam$meta, lower.thresh = 2)
docs_cheatexam <- out_cheatexam$documents
vocab_cheatexam <- out_cheatexam$vocab
meta_cheatexam <- out_cheatexam$meta

out_getfired <- prepDocuments(processed_getfired$documents, processed_getfired$vocab, processed_getfired$meta, lower.thresh = 2)
docs_getfired <- out_getfired$documents
vocab_getfired <- out_getfired$vocab
meta_getfired <- out_getfired$meta

## Checking topic count
set.seed(42)
storage_noticketwide <- searchK(docs_noticket, vocab_noticket, K = c(4, 6, 8, 10, 12, 14), prevalence = ~Sex, content = ~Sex, data = meta_noticket)
plot(storage_noticketwide)
set.seed(42)
storage_noticketnarrow <- searchK(docs_noticket, vocab_noticket, K = c(8, 9, 10, 11, 12), prevalence = ~Sex, content = ~Sex, data = meta_noticket)
plot(storage_noticketnarrow)

set.seed(42)
storage_missdeadlinewide <- searchK(docs_missdeadline, vocab_missdeadline, K = c(4, 6, 8, 10, 12, 14), prevalence = ~Sex, content = ~Sex, data = meta_missdeadline)
plot(storage_missdeadlinewide)
set.seed(42)
storage_missdeadlinenarrow <- searchK(docs_missdeadline, vocab_missdeadline, K = c(3, 4, 5, 6, 7, 8, 9), prevalence = ~Sex, content = ~Sex, data = meta_missdeadline)
plot(storage_missdeadlinenarrow)

set.seed(42)
storage_caraccidentinvolvewide <- searchK(docs_caraccidentinvolve, vocab_caraccidentinvolve, K = c(4, 6, 8, 10, 12, 14), prevalence = ~Sex, content = ~Sex, data = meta_caraccidentinvolve)
plot(storage_caraccidentinvolvewide)
set.seed(42)
storage_caraccidentinvolvenarrow <- searchK(docs_caraccidentinvolve, vocab_caraccidentinvolve, K = c(3, 4, 5, 6, 7, 8), prevalence = ~Sex, content = ~Sex, data = meta_caraccidentinvolve)
plot(storage_caraccidentinvolvenarrow)

set.seed(42)
storage_cheatexamwide <- searchK(docs_cheatexam, vocab_cheatexam, K = c(4, 6, 8, 10, 12, 14), prevalence = ~Sex, content = ~Sex, data = meta_cheatexam)
plot(storage_cheatexamwide)
set.seed(42)
storage_cheatexamnarrow <- searchK(docs_cheatexam, vocab_cheatexam, K = c(4, 5, 6, 7, 8, 9, 10), prevalence = ~Sex, content = ~Sex, data = meta_cheatexam)
plot(storage_cheatexamnarrow)

set.seed(42)
storage_getfiredwide <- searchK(docs_getfired, vocab_getfired, K = c(4, 6, 8, 10, 12, 14), prevalence = ~Sex, content = ~Sex, data = meta_getfired)
plot(storage_getfiredwide)
set.seed(42)
storage_getfirednarrow <- searchK(docs_getfired, vocab_getfired, K = c(3, 4, 5, 6, 7, 8, 9, 10), prevalence = ~Sex, content = ~Sex, data = meta_getfired)
plot(storage_getfirednarrow)